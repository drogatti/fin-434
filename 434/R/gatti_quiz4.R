#set a working directory
setwd("C:/Users/aless/Documents/434/R/Data")

#read in csv file
wmt_msft_amzn <- read.csv("gatti_WMT_MSFT_AMZN.csv")

#split into three dataframes by ticker
wmt_msft_amzn_split <- split(wmt_msft_amzn, wmt_msft_amzn$TICKER)
wmt <- wmt_msft_amzn_split$WMT
msft <- wmt_msft_amzn_split$MSFT
amzn <- wmt_msft_amzn_split$AMZN

#print median return for each three dfs
wmt_median_ret <- median(wmt$RET, na.rm=TRUE)
print(paste("Walmart's 5-year median daily return: ", wmt_median_ret))

msft_median_ret <- median(msft$RET, na.rm=TRUE)
print(paste("Microsoft's 5-year median daily return: ", msft_median_ret))

amzn_median_ret <- median(amzn$RET, na.rm=TRUE)
print(paste("Amazon's 5-year median daily return: ", amzn_median_ret))