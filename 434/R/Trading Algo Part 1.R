library(tidyquant)
library(quantmod)
library(ggplot2)
library(scales)

#set a working directory
setwd("C:/Users/aless/Documents/434/R/Data")

#enter our Tiingo API Key
tiingo_api_key('5364c06f2b84bbcb0f0338c4829a7465be0a3fc9')

#get stock price data from Tiingo
prices <- tq_get(c("AAPL", "TSLA"), get="tiingo", from="2010-01-01")

#get sub-daily data from Tiingo
intra_stocks <- tq_get(c("AAPL", "TSLA"),
                       get="tiingo.iex",
                       from="2022-01-01",
                       to="2022-01-15",
                       resample_frequency="30min")

intra_crypto <- tq_get(c("btcusd", "btceur"),
                       get="tiingo.crypto",
                       from="2022-01-01",
                       to="2022-01-15",
                       resample_frequency="30min")

intra_spy <- tq_get(c("SPY"),
                       get="tiingo.iex",
                       from="2023-01-01",
                       to="2024-04-10",
                       resample_frequency="30min")

#calculate returns using quantmod
intra_spy$PercentIncrease <- Delt(intra_spy$close)*100
intra_spy <- intra_spy[-1,]

MoNoGo <- function(Data, Input){
  Data$Signal <- ""
  Data$`Buy Or Sell Price` <- 0
  Data$`Real Cumulative Profit` <- 0
  Data$`Possible Transaction Profit` <- 0
  
  nrows <- nrow(Data)
  Position <- 0
  
  for (i in 1:nrows){
    if (Data$PercentIncrease[i] > Input & Position < 1){
      Data$Signal[i] <- "buy"
      Data$`Buy Or Sell Price`[i] <- -Data$close[i]
      MostRecentBuy <- Data$close[i]
      Position <- 1
    }
    else if(abs(Data$PercentIncrease[i]) > Input & Position == 1){
      Data$Signal[i] <- "sell"
      Data$`Buy Or Sell Price`[i] <- Data$close[i]
      Position <- -1
    }
    else {Data$Signal[i] <- "no change"}
    
    #Now we will calculate the transaction profit at any point in time
    if (Position != 0){Data$`Possible Transaction Profit`[i] <- Data$close[i] - MostRecentBuy}
    #calculate our cumulative profit
    Data$`Real Cumulative Profit`[i] <- ifelse(Position==1, sum(Data$`Buy Or Sell Price`[1:i]) + Data$close[i],
                                               sum(Data$`Buy Or Sell Price`[1:i]))
    if (Position > 0 & i == nrows){Data$`Real Cumulative Profit`[i] <- sum(Data$`Buy Or Sell Price`[1:i]) + Data$close[nrows]}
  }
  return(Data)
}

spy_strat <- MoNoGo(intra_spy, .2)

#plot the profitability of the MoNoGo
ggplot(spy_strat, aes(x=date))+
  geom_line(aes(y=`Real Cumulative Profit`, color="Cumulative Profit"), size=1)+
  scale_color_manual(values=c("Cumulative Profit"="blue"))+
  labs(color="Legend")+
  ggtitle(paste("Cumulative Profit for MoNoGo"))+
  labs(x="Date", y="Profit")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_y_continuous(labels=dollar_format())

#get more price data from Tiingo
tsla <- tq_get(c("TSLA"),
               get="tiingo",
               from="2019-01-01",
               to="2021-11-30")

tsla2 <- tq_get(c("TSLA"),
               get="tiingo",
               from="2021-11-30",
               to="2023-11-30")

tsla = subset(tsla, select=-c(close))
names(tsla)[names(tsla) == "adjusted"] <- "close"

tsla2 = subset(tsla2, select=-c(close))
names(tsla2)[names(tsla2) == "adjusted"] <- "close"

#calculate returns from Quantmod
tsla$PercentIncrease <- Delt(tsla$close)*100
tsla <- tsla[-1,]

tsla2$PercentIncrease <- Delt(tsla2$close)*100
tsla2 <- tsla2[-1,]

MoNoGo2 <- function(Data, Input){
  Data$Signal <- ""
  Data$`Buy Or Sell Price` <- 0
  Data$`Real Cumulative Profit` <- 0
  Data$`Possible Transaction Profit` <- 0
  
  nrows <- nrow(Data)
  Position <- 0
  
  for (i in 1:nrows){
    if (Data$PercentIncrease[i] < -Input & Position < 1){
      Data$Signal[i] <- "buy"
      Data$`Buy Or Sell Price`[i] <- -Data$close[i]
      MostRecentBuy <- Data$close[i]
      Position <- 1
    }
    else if(abs(Data$PercentIncrease[i]) > Input & Position == 1){
      Data$Signal[i] <- "sell"
      Data$`Buy Or Sell Price`[i] <- Data$close[i]
      Position <- -1
    }
    else {Data$Signal[i] <- "no change"}
    
    #Now we will calculate the transaction profit at any point in time
    if (Position != 0){Data$`Possible Transaction Profit`[i] <- Data$close[i] - MostRecentBuy}
    #calculate our cumulative profit
    Data$`Real Cumulative Profit`[i] <- ifelse(Position==1, sum(Data$`Buy Or Sell Price`[1:i]) + Data$close[i],
                                               sum(Data$`Buy Or Sell Price`[1:i]))
    if (Position > 0 & i == nrows){Data$`Real Cumulative Profit`[i] <- sum(Data$`Buy Or Sell Price`[1:i]) + Data$close[nrows]}
  }
  return(Data)
}
#hypothetical training period
tsla_strat <- MoNoGo2(tsla, .2)
ggplot(tsla_strat, aes(x=date))+
  geom_line(aes(y=`Real Cumulative Profit`, color="Cumulative Profit"), size=1)+
  scale_color_manual(values=c("Cumulative Profit"="blue"))+
  labs(color="Legend")+
  ggtitle(paste("Cumulative Profit for MoNoGo"))+
  labs(x="Date", y="Profit")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_y_continuous(labels=dollar_format())

#hypothetical test period
tsla_strat2 <- MoNoGo2(tsla2, .2)
ggplot(tsla_strat2, aes(x=date))+
  geom_line(aes(y=`Real Cumulative Profit`, color="Cumulative Profit"), size=1)+
  scale_color_manual(values=c("Cumulative Profit"="blue"))+
  labs(color="Legend")+
  ggtitle(paste("Cumulative Profit for MoNoGo"))+
  labs(x="Date", y="Profit")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_y_continuous(labels=dollar_format())