# This file
# Date
# Author
# Last modfied


# Load data
stockData <- read.csv("C:/Users/A1410346/Dropbox/option metrics data/SECURITY PRICE DATA.csv")
optData <- read.csv("C:/Users/A1410346/Dropbox/option metrics data/STANDARDISED OPTIONS INDEX.csv")
# Add Fama French 5 factors daily 
FFdaily <- read.csv("C:/Users/A1410346/Dropbox/option metrics data/F-F_Research_Data_5_Factors_2x3_daily.CSV")
FFdaily[,2:6] <- FFdaily[,2:6]/100
FFdaily$Mkt <- FFdaily$Mkt.RF + FFdaily$RF
# Restrict data set
setwd("C:/Users/A1410346/Dropbox/option metrics data")
cusip <- readLines("list_cusip_ETFs.txt") # strings
numberOfUniqueStocks <- length(cusip)

# Subset stock data
stocks <- subset(stockData, select = c("cusip", "date", "return")) # select relevant variables
stocks <- stocks[stocks$cusip %in% cusip,] # based on filter 
stocks$date_ym <- floor(stocks$date/100) # add monthly dates



# Merge stock returns with Fama French factors
dates <- sort(unique(stocks$date))
numberDaysOfStockReturn <- length(dates)
numberDaysOfStockReturn
burnInDays <- 29 # Number of observartions used in the estimation

# Generate data_set




####################### OPTION PRICES ########################################
# link to option premia
options <- subset(optData, select= c("cusip", "date", "days", "impl_volatility", "premium", "strike_price", "cp_flag"))
options <- options[optData$cusip %in% cusip,] # the relevant cusips
options <- options[order(options$cusip, options$date, decreasing = T),] # Order data 
# Align time with stock data
options <- options[options$date %in% options$date[match(dates, options$dat)],] 
head(options)

# limit to month end values
options  <- options[match(unique(paste(options$cusip, options$date, options$cp_flag)),paste(options$cusip, options$date, options$cp_flag)),]

# compute premias
for (h in unique(options$days)){
  temp_C <- options[options$days == h & options$cp_flag =="C",]
  d.set[match(paste(temp_C$cusip, temp_C$date),paste(d.set$cusip, d.set$dates)),paste("Rel_Premium_Call_", h, sep="")] <- temp_C$premium / temp_C$strike_price 
  d.set[match(paste(temp_C$cusip, temp_C$date),paste(d.set$cusip, d.set$dates)),paste("ImplVol_C_", h, sep="")] <- temp_C$impl_volatility
  
  temp_P <- options[options$days == h & options$cp_flag =="P",]
  d.set[match(paste(temp_P$cusip, temp_P$date),paste(d.set$cusip, d.set$dates)),paste("Rel_Premium_Put_", h, sep="")] <- temp_P$premium / temp_P$strike_price 
  d.set[match(paste(temp_C$cusip, temp_C$date),paste(d.set$cusip, d.set$dates)),paste("ImplVol_P_", h, sep="")] <- temp_P$impl_volatility
}  

# Run regressions
data <- na.omit(d.set)
names(data)
m1 <- felm(ImplVol_P_30  ~ cor + sdStock |dates|0|cusip + dates, data=data)
m2 <- felm(ImplVol_C_30  ~ cor + sdStock |dates|0|cusip + dates, data=data)

m3 <- felm(ImplVol_P_30  ~ beta |dates|0|cusip + dates, data=data)
m4 <- felm(ImplVol_C_30  ~ beta |dates|0|cusip + dates, data=data)

m5 <- felm(ImplVol_P_30  ~ cor |dates|0|cusip + dates, data=data)
m6 <- felm(ImplVol_C_30  ~ cor |dates|0|cusip + dates, data=data)

m7 <- felm(log(ImplVol_P_30)  ~ log(cor+1) |cusip + dates |0|cusip + dates, data=data)
m8 <- felm(ImplVol_C_30  ~ cor |cusip + dates |0|cusip + dates, data=data)

hist(data$cor)
summary(m5)
summary(m6)
summary(m7)
summary(m8)

summary(m1)
summary(m2)
summary(m3)
summary(m4) 
