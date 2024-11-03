#import libraries
library(tidyquant)
library(ggplot2)
library(gridExtra)
library(quantmod)
library(scales)
library(stats)

#set a working directory
setwd("C:/Users/aless/Documents/434/R/Data")

#Get Yahoo data remotely from MA ticker
prices <- tq_get("MA", from="2019-04-23", to="2024-04-24", get="stock.prices")

#Calculate returns using Quantmod function
returns <- Delt(prices$close)
returns <- returns[-1,]

#Get annualized standard deviation (sigma)
daily_stdev <- sd(returns)
sigma <- daily_stdev * sqrt(252)

#Create the BS Function
BlackScholes <- function(S, K, r, T, sig, type){
  if(type=="C"){
    d1 <- (log(S/K)+(r + sig^2/2)*T)/(sig*sqrt(T))
    d2 <- d1 - sig*sqrt(T)
    
    value <- S * pnorm(d1) - K*exp(-r*T)*pnorm(d2)
    return(value)}
  if(type=="P"){
    d1 <- (log(S/K)+(r + sig^2/2)*T)/(sig*sqrt(T))
    d2 <- d1 - sig*sqrt(T)
    
    value <- K*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1)
    return(value)}
}

#generate uniformly distributed price outcomes
#blank array creation
current_price <- as.numeric(prices[nrow(prices), "close"])
lower_bound <- current_price * 0.79
upper_bound <- current_price * 1.21
t <- 250
prem_call <- array(data=NA, dim=t, dimname=NULL)
prem_put <- array(data=NA, dim=t, dimname=NULL)
int_val_call <- array(data=NA, dim=t, dimname=NULL)
int_val_put <- array(data=NA, dim=t, dimname=NULL)

#generate price outcomes
price_outcomes <- runif(t, min=lower_bound, max=upper_bound)

#allow user to set risk-free rate
rfr <- as.numeric(readline("Please enter the risk-free rate (in decimals): "))

#calculate the value of a SHORT strangle
#Sell a Call and a Put with different strike prices
for (i in 1:t) {
  price <- price_outcomes[i]
  
  if (price < current_price) {
    #in the money
    strike_c <- current_price * 1.08
    strike_p <- current_price * 0.92
  } else if (price > current_price) {
    #out of the money
    strike_c <- current_price * 1.08
    strike_p <- current_price * 0.92
  } else {
    #at the money
    strike_c <- current_price
    strike_p <- current_price
  }
  prem_call[i] <- BlackScholes(current_price, strike_c, rfr, 1, sigma, "C")
  prem_put[i] <- BlackScholes(current_price, strike_p, rfr, 1, sigma, "P")
  
  #value of the call at exp
  int_val_call[i] <- price - strike_c - prem_call[i]
  
  #value of the put at exp
  int_val_put[i] <- strike_p - price - prem_put[i]
}

payoffShortCall <- pmax(-prem_call, int_val_call) * -1

payoffShortPut <- pmax(-prem_put, int_val_put) * -1

#create an array of payoffs for the entire strategy
payoff <- rowSums(cbind(payoffShortCall, payoffShortPut))

#create a df with all of the Short Strangle info
results <- data.frame(cbind(price_outcomes, payoffShortCall, payoffShortPut, payoff))

#now plot the total payoff
plot1 <- ggplot(results, aes(x=price_outcomes))+
  geom_line(aes(y=payoff, color="SSPayoff"), size=1)+
  scale_color_manual(values=c('ShortCall' = "blue", "ShortPut" = "red", "SSPayoff" = "darkgreen"))+
  labs(color="Legend")+
  ggtitle(paste("Short Strangle Payoff"))+
  labs(x="Stock Price", y="Payoff")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_y_continuous(labels=dollar_format(), breaks=seq(-20,100,10))+
  scale_x_continuous(labels=dollar_format(), breaks=seq(lower_bound,upper_bound,30))

#now plot the strategy with separate legs
plot2 <- ggplot(results, aes(x=price_outcomes))+
  geom_line(aes(y=payoffShortCall, color="ShortCall"), size=1)+
  geom_line(aes(y=payoffShortPut, color="ShortPut"), size=1)+
  geom_line(aes(y=payoff, color="SSPayoff"), size=1)+
  scale_color_manual(values=c('ShortCall' = "blue", "ShortPut" = "red", "SSPayoff" = "darkgreen"))+
  labs(color="Legend")+
  ggtitle(paste("Short Strangle Payoff w/ Sep. Legs"))+
  labs(x="Stock Price", y="Payoff")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_y_continuous(labels=dollar_format(), breaks=seq(-20,100,10))+
  scale_x_continuous(labels=dollar_format(), breaks=seq(lower_bound,upper_bound,30))

#save the plots to a jpeg on computer
ggsave(file="gatti_SSPAYOFFS.jpg", grid.arrange(plot1, plot2, nrow=1, ncol=2, top="Short Strangle Payoff Charts"))

#calculate the payoff of the strat if implemented one year ago
current_price_1_year_ago <- prices$close[which.max(prices$date == "2023-04-24")]
call_strike <- current_price_1_year_ago * 1.08
put_strike <- current_price_1_year_ago * 0.92
##determine which option is in the money (booleans)
call_option_in_money <- current_price > call_strike
put_option_in_money <- current_price < put_strike
premium_call <- BlackScholes(current_price_1_year_ago, call_strike, rfr, 1, sigma, "C")
premium_put <- BlackScholes(current_price_1_year_ago, put_strike, rfr, 1, sigma, "P")
int_val_call_calc <- ifelse(call_option_in_money, current_price - call_strike, 0)
int_val_put_calc <- ifelse(put_option_in_money, put_strike - current_price, 0)
payoff_call <- premium_call - int_val_call_calc
payoff_put <- premium_put - int_val_put_calc
total_payoff <- payoff_call + payoff_put
print(paste("If the strat was implemented 1 year ago, the total payoff today would be: ", total_payoff))

#now plot time series chart of payoffs over 5 years of stock data
five_year_payoff <- function(stock_price){
  call_intrinsic <- pmax(stock_price - (stock_price*1.08), 0)
  put_intrinsic <- pmax((stock_price*0.92) - stock_price, 0)
  payoff_total <- -call_intrinsic - put_intrinsic
  return(payoff_total)
}