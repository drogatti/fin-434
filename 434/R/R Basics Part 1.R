#Print Hello World

#set the directory
setwd("C:/Users/aless/Documents/434/R/Data")

print("Hello World")

#install.packages("FinCal")
library(FinCal)

r <- .08
pv <- 100
pmt <- 0
type <- 0

n <- 1:40
  
x <- fv(r,n,-pv,pmt,type)
print(x)

#read in CSV

#bring in tsla trend data

names(tslatrend)

df <- read.csv("tslatrends.csv", skip=1)
names(df)
head(df)

plot(x)

fv()