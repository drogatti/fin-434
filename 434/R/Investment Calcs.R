#how to clear the environment
rm(list = ls())

df <- read.csv("bank.csv")
library(tidyquant)
library(ggplot2)
library(ggcorrplot)
library(quantmod)
library(readr)
library(zoo)
library(ggpubr)
library(scales)
library(Hmisc)
library(corrplot)
library(reshape2)

#set a working directory
setwd("C:/Users/aless/Documents/434/R/Data")

#set user inputs
tic1 <- readline("First Ticker? in Caps")
tic2 <- readline("Second Ticker? in Caps")

tickers = c(tic1, tic2)

#Get Yahoo data remotely from user selected tickers
prices <- tq_get(tickers, from="2010-01-01", to="2023-12-31", get="stock.prices")
#create separate dfs for each of the user selected tickers
stock_split <- split(prices, prices$symbol)
AAPL <- stock_split$AAPL
TSLA <- stock_split$TSLA

#Calculate returns using Quantmod function
AAPL$returns <- Delt(AAPL$close)

#drop the top row
AAPL <- AAPL[-1,]

#calculate the value of $1 over time in AAPL
AAPL$multiplier <- AAPL$returns + 1
AAPL$cum_value <- cumprod(AAPL$multiplier)

#calculate moving averages for n number of days
AAPL$mov_av_50 <- rollmean(AAPL$close, 50, fill=NA, align="right")
AAPL$mov_av_200 <- rollmean(AAPL$close, 200, fill=NA, align="right")
AAPL$mov_av_75 <- rollmean(AAPL$close, 75, fill=NA, align="right")
summary(AAPL)

#PART 2 Starts Here----------------------------
#plot moving averages
ggplot(AAPL, aes(x=date))+
  geom_line(aes(y=mov_av_50, color="50-day Moving Average"), size=1.1)+
  geom_line(aes(y=mov_av_200, color="200-day Moving Average"), size=1.1)+
  ggtitle(paste("Moving Averages of", tic1))+
  labs(x="Date", y="Dollars", color="Legend")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_y_continuous(labels=dollar_format())

#plot the cumulative value of $1
ggplot(AAPL, aes(x=date))+
  geom_line(aes(y=cum_value), size=1.1)+
  ggtitle(paste("Cumulative Value of $1 Invested"))+
  labs(x="Date", y="Dollars", color="Legend")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_y_continuous(labels=dollar_format())

#Read in the bank data from UCI data repository
df <- read.csv("bank.csv")
names(df)
#Trim DF to keep certain variables
keep <- c("age", "balance", "day", "pdays", "previous")
df=df[keep]

#correlation with p-values
corrwp <- rcorr(as.matrix(df))
corrwp
corrmatrix <- cor(df, use="pairwise.complete.obs")
corrmatrix

#create a heatmap
cormat <- round(cor(df), 2)
melted_cormat <- melt(cormat)

ggplot(data=melted_cormat, aes(x=Var1, y=Var2, fill=value))+
  labs(y="Variable", "Variable")+
  ggtitle("HeatMap")+
  geom_tile()

#Generate random samples
n <- readline("What is the sample size?")
t <- readline("What is the number of Trials?")
m <- readline("What is the population mean?")
std <- readline("What is the population standard deviation?")

#convert to appropriate data type
n <- as.integer(n)
t <- as.integer(t)
m <- as.numeric(m)
std <- as.numeric(std)
#create some blank arrays to fill in with stats
x <- array(data=NA, dim=n, dimname=NULL)
s <- array(data=NA, dim=t, dimname=NULL)
mn <- array(data=NA, dim=t, dimname=NULL)
md <- array(data=NA, dim=t, dimname=NULL)
v <- array(data=NA, dim=t, dimname=NULL)

#generate random samples and then fill in the blank arrays with stats
for (i in 1:t){
  x <- rnorm(n, mean=m, sd=std)
  s[i] <- sd(x)
  mn[i] <- mean(x)
  md[i] <- median(x)
  v[i] <- var(x)
}
#create a df and get some summary stats
df=data.frame(mn, md, s, v)
summary(df)
#export summary stats to CSV
varsum <- summary(df)
write.csv(varsum, file="gatti_varsum.csv")