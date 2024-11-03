library(ggplot2)
library(scales)

#get the percentile probability from the standard normal distribution
pnorm(-1)

#demo the log and exp form
exp(0.05)
log(1.051271)
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
call <- BlackScholes(130,125,.03,0.02739726,.35,"C")
put <- BlackScholes(45,50,.03,0.5,.35,"P")

#generate random strike prices, feed into BS model and graph
#blank array creation

t <- 100
p <- array(data=NA, dim=t, dimname=NULL)
c <- array(data=NA, dim=t, dimname=NULL)

#generate random strike prices
x <- rnorm(t, mean=100, sd=30)
hist(x)

#loop through the blank arrays we created and fill in option values
for (i in 1:t){
  c[i] <- BlackScholes(100, x[i], 0.04, 2, .2, "C")
  p[i] <- BlackScholes(100, x[i], 0.04, 2, .2, "P")
}

df=data.frame(x,c,p)

#plot the value of the call and the put at the different strikes
ggplot(df, aes(x=x))+
  geom_line(aes(y=c, color="Call"), size=1)+
  geom_line(aes(y=p, color="Put"), size=1)+
  scale_color_manual(values=c('Call' = "blue", "Put" = "red"))+
  labs(color="Legend")+
  ggtitle(paste("Option Values for Various Strike Prices"))+
  labs(x="Strike Price", y="Value of the Option")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_y_continuous(labels=dollar_format())+
  scale_x_continuous(labels=dollar_format())

#calculate the value of a LONG straddle
#Buy a Call and a Put with same strike
prices <- seq(40,160,1)
strike <- 95
prem_call <- BlackScholes(100,strike,0.04, 1, .2, "C")
prem_put <- BlackScholes(100,strike,0.04, 1, .2, "P")

#value of the call at exp
int_val_call <- prices - strike - prem_call
payoffLongCall <- pmax(-prem_call, int_val_call)

#value of the put at exp
int_val_put <- strike - prices - prem_put
payoffLongPut <- pmax(-prem_put, int_val_put)

#create an array of payoffs for the entire strategy
payoff <- rowSums(cbind(payoffLongCall, payoffLongPut))

#create a df with all of the Long Straddle info
results <- data.frame(cbind(prices, payoffLongCall, payoffLongPut, payoff))

#now we plot the strategy
ggplot(results, aes(x=prices))+
  geom_line(aes(y=payoffLongCall, color="LongCall"), size=1)+
  geom_line(aes(y=payoffLongPut, color="LongPut"), size=1)+
  geom_line(aes(y=payoff, color="LSPayoff"), size=1)+
  scale_color_manual(values=c('LongCall' = "blue", "LongPut" = "red", "LSPayoff" = "darkgreen"))+
  labs(color="Legend")+
  ggtitle(paste("Long Straddle Payoff"))+
  labs(x="Stock Price", y="Payoff")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_y_continuous(labels=dollar_format(), breaks=seq(-20,60,10))+
  scale_x_continuous(labels=dollar_format(), breaks=seq(40,160,10))