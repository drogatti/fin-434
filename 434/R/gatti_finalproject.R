#import libraries
library(tidyquant)
library(ggplot2)
library(gridExtra)
library(quantmod)

#set a working directory
setwd("C:/Users/aless/Documents/434/R/Data")

#Read in the DPZ data from CRSP
dominos <- read.csv("gatti_DPZ.csv")

#Get Yahoo data remotely from QQQ ticker
market_etf <- tq_get("QQQ", from="2017-01-01", to="2023-12-31", get="stock.prices")

#calculate returns for dominos and market ETF using Quantmod
dominos_returns <- Delt(dominos$PRC)
dominos_returns <- dominos_returns[-1,]
market_etf_returns <- Delt(market_etf$close)
market_etf_returns <- market_etf_returns[-1,]

#calculate and report annualized stdevs (sigmas) for company, the index, and ETF
dominos_sigma <- sd(dominos_returns) * sqrt(252)
index_sigma <- sd(dominos$vwretd) * sqrt(252)
market_etf_sigma <- sd(market_etf_returns) * sqrt(252)
print(paste("The sigma for dominos is: ", dominos_sigma))
print(paste("The sigma for the index is: ", index_sigma))
print(paste("The sigma for the market etf is: ", market_etf_sigma))

#calculate and report avg daily volume for dominos and ETF
dominos_avg_daily_vol <- mean(dominos$VOL)
market_etf_avg_daily_vol <- mean(market_etf$volume)
print(paste("The avg daily vol. for dominos is: ", dominos_avg_daily_vol))
print(paste("The avg daily vol. for the market etf is: ", market_etf_avg_daily_vol))

#plot histogram of daily returns for each of three series
df <- cbind(dominos_returns, market_etf_returns)

dominos_ret_hist <- ggplot(df, aes(dominos_returns))+
  geom_histogram(color="orange", fill="orange", bins=20)+
  labs(y="Number of Obs", x="Ret Bin")+
  ggtitle("Histogram of Daily DPZ Rets")+
  theme_classic()+
  theme(plot.title=element_text (hjust=0.5))

vwretd_hist <- ggplot(dominos, aes(vwretd))+
  geom_histogram(color="orange", fill="orange", bins=20)+
  labs(y="Number of Obs", x="Ret Bin")+
  ggtitle("Histogram of Index VW Daily Rets")+
  theme_classic()+
  theme(plot.title=element_text (hjust=0.5))

etf_ret_hist <- ggplot(df, aes(market_etf_returns))+
  geom_histogram(color="orange", fill="orange", bins=20)+
  labs(y="Number of Obs", x="Ret Bin")+
  ggtitle("Histogram of Daily QQQ ETF Rets")+
  theme_classic()+
  theme(plot.title=element_text (hjust=0.5))

#save the plots to a jpeg on computer
#open blank file
jpeg("gatti_Histograms_returns.jpg", width=1000, height=600)
#create the plot
grid.arrange(dominos_ret_hist, vwretd_hist, etf_ret_hist, nrow=1, ncol=3, top="Daily Returns Several Histograms")
#close the file
dev.off()

#merge data and WRDS data and run two regressions and report to user
dominos$returns <- Delt(dominos$PRC)
dominos$etf_returns <- NA
dominos$etf_returns[2:length(dominos$etf_returns)] <- market_etf_returns

model_dpz_index <- lm(vwretd ~ returns, data = dominos)
beta_dpz_index <- coef(model_dpz_index)[2]
print(paste("The beta for dominos returns on index returns is: ", beta_dpz_index))

model_dpzvol_etf <- lm(etf_returns ~ VOL, data = dominos)
beta_dpzvol_etf <- coef(model_dpzvol_etf)[2]
print(paste("The beta for dominos volume on market ETF returns is: ", beta_dpzvol_etf))
