library(ggplot2)
library(gridExtra)
library(lubridate)
#how to clear the environment
rm(list = ls())
#set a working directory
setwd("C:/Users/aless/Documents/434/R/Data")

#Bring in tsla trend data
tslatrend <- read.csv("tslatrends.csv", skip=1)
names(tslatrend)
#change a variable name
names(tslatrend)[names(tslatrend)== "NASDAQ.TSLA...United.States."] <- "GSVI"
names(tslatrend)
#summary stats for trends data
summary(tslatrend)

#read in TAMRETS dataset
tamrets <- read.csv("TAMRETS.csv")

#demonstrate how to split a df on a column
names(tamrets)
tamrets_split <- split(tamrets, tamrets$TICKER)
tsla_rets <- tamrets_split$TSLA
amzn_rets <- tamrets_split$AMZN
msft_rets <- tamrets_split$MSFT

#import trex csv for stock data and calculate returns
trex <- read.csv("TREX.csv")
names(trex)

#convert variables to numeric format
trex$Close <- as.numeric(trex$Close)
trex$Open <- as.numeric(trex$Open)

#Drop missing values (nan)
trex <- na.omit(trex)

#get a look at the summary stats
summary(trex)

#bring in trex and GSVI lumber data
trextrend <- read.csv("TREXTREND.csv", skip=1)
lumbertrend <- read.csv("LUMBERTREND.csv", skip=1)

names(trextrend)
#change a variable name
names(trextrend)[names(trextrend)== "NYSE.TREX...United.States."] <- "GSVI_trex"
names(trextrend)

names(lumbertrend)
#change a variable name
names(lumbertrend)[names(lumbertrend)== "lumber...United.States."] <- "GSVI_lumber"
names(lumbertrend)

#merge the two dfs into one
tlmerged <- merge(trextrend, lumbertrend, by.x="Week", by.y="Week")
names(tlmerged)
head(tlmerged)

#Bring in WOOD ETF prices from yahoo
wood <- read.csv("WOOD.csv")
head(wood)

#Now we will merge WOOD prices with GSVI data
class(wood$Date)

#change chr date type to "date" type
#an aside example
date_example <- tlmerged$Week
date_example1 <- as.Date.character(date_example)

#back on track
tlmerged$date1 <- as.Date.character(tlmerged$Week) + 1
wood$Date_new <- as.Date.character(wood$Date)

lumber_merged <- merge(tlmerged, wood, by.x="date1", by.y="Date_new")
summary(lumber_merged)

#plot the time series of the above dataset
p1 <- ggplot(lumber_merged, aes(x=date1, y=Adj.Close))+
  geom_line(color="green", linewidth=0.5)+
  ggtitle("WOOD Close in $")+
  labs(x="Date", y="Close in $")+
  theme_gray()+
  theme(plot.title = element_text(hjust=0.5))

p2 <- ggplot(tlmerged, aes(x=date1, y=GSVI_trex))+
  geom_line(color="blue", linewidth=0.5)+
  ggtitle("GSVI TREX")+
  labs(x="Date", y="GSVI out of 100")+
  theme_gray()+
  theme(plot.title = element_text(hjust=0.5))
p2
p3 <- ggplot(tlmerged, aes(x=date1, y=GSVI_lumber))+
  geom_line(color="red", linewidth=0.5)+
  ggtitle("GSVI LUMBER")+
  labs(x="Date", y="GSVI out of 100")+
  theme_gray()+
  theme(plot.title = element_text(hjust=0.5))
p3

#plot all of the graphs on one grid
grid.arrange(p1, p2, p3, ncol=3)

#export to a jpeg
#step 1 open a blank jpeg file
jpeg("gatti_lumberplot.jpg", width=1800, height=350)
#step 2 create the plot
grid.arrange(p1, p2, p3, ncol=3)
#step 3 close the file
dev.off()


#create a new jpeg file and set dimensions
jpeg("gatti_lumber2axis.jpg", width=900, height=350)
par(mar=c(5,5,3,5)+1)
plot(lumber_merged$date1, lumber_merged$GSVI_lumber, type="l", col="green", ylab="GSVI", xlab="Date", ylim=range(1,100))
lines(lumber_merged$date1, lumber_merged$GSVI_trex, col="blue")
par(new=TRUE)
#draw on our secondary y axis
plot(lumber_merged$date1, lumber_merged$Close, type="l", col="orange", axes=FALSE, xlab="", ylab="")
axis(side=4, at=pretty(range(lumber_merged$Close)))
mtext("Close Price in $", side=4, line=2)
mtext("GSVI Trends and Price Over Time", side=3, line=2)

#add a legend
legend("topright", legend=c("GSVI_Lumber", "GSVI_Trex", "Wood_Close"), col=c("green", "blue", "orange"), cex=1, lty=1:1)
dev.off()