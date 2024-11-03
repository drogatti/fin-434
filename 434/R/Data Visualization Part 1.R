library(ggplot2)
library(gridExtra)
library(lubridate)
library(aplpack)
library(vioplot)
library(ggthemes)
library(tidyr)
library(ggpubr)

#set a working directory
setwd("C:/Users/aless/Documents/434/R/Data")

#Read in the TSLA data from CRSP
df <- read.csv("tslarets_new.csv")

df$date <- ymd(df$date)

#^^^^^^^^^^^^^^^^LINE PLOTS^^^^^^^^^^^^^^^^^^^^^^^^^^
#use the base r function
plot(df$date, df$RET)

#use ggplot
gl1 <- ggplot(df, aes(x=date))+
  geom_line(aes(y=sprtrn), color="orange")+
  ylim(-.25, .25)+ labs(y="S&P 500 Rets", x="Date")+
  ggtitle("Line Plot of S&P 500 Rets")+
  theme(plot.title=element_text (hjust=0.5))
gl1

gl2 <- ggplot(df, aes(x=date))+
  geom_line(aes(y=ewretd), color="purple")+
  ylim(-.25, .25)+ labs(y="TSLA EW Rets", x="Date")
gl2

gl3 <- ggplot(df, aes(x=date))+
  geom_line(aes(y=vwretd), color="blue")+
  ylim(-.25, .25)+ labs(y="TSLA VW Rets", x="Date")
gl3

gl4 <- ggplot(df, aes(x=date))+
  geom_line(aes(y=RET), color="red")+
  ylim(-.25, .25)+ labs(y="TSLA Rets", x="Date")
gl4

grid.arrange(gl1, gl2, gl3, gl4, nrow=2, top="TSLA and MKT Rets")

#^^^^^^^^^^^^^^^^^^HISTOGRAMS^^^^^^^^^^^^^^^^^^^^^^^^
#base r
hist(df$sprtrn, main="Histogram -- Base R")

#in GGPLOT
gh1 <- ggplot(df, aes(sprtrn))+
  geom_histogram(color="orange", fill="orange", bins=30)+
  labs(y="Number of Obs", x="Ret Bin")+
  ggtitle("Histogram of S&P 500 Rets")+
  theme_classic()+
  theme(plot.title=element_text (hjust=0.5))

gh2 <- ggplot(df, aes(ewretd))+
  geom_histogram(color="orange", fill="orange", bins=10)+
  labs(y="Number of Obs", x="Ret Bin")+
  ggtitle("Histogram of TSLA EW Rets")+
  theme_classic()

gh3 <- ggplot(df, aes(vwretd))+
  geom_histogram(color="orange", fill="orange", bins=10)+
  labs(y="Number of Obs", x="Ret Bin")+
  ggtitle("Histogram of TSLA VW Rets")+
  theme_classic()

gh4 <- ggplot(df, aes(RET))+
  geom_histogram(color="orange", fill="orange", bins=10)+
  labs(y="Number of Obs", x="Ret Bin")+
  ggtitle("Histogram of TSLA Rets")+
  theme_classic()

grid.arrange(gh1, gh2, gh3, gh4, nrow=2, top="TSLA and MKT Hist Rets")

#^^^^^^^^^^^^^^^^BOXPLOTS^^^^^^^^^^^^^^^^^^^^^^^^^^
#in base r
par(mfrow=c(1,4)) # form a blank grid

boxplot(df$sprtrn, col="orange", main="S&P 500 Rets", ylab="Ret Level")
boxplot(df$ewretd, col="green", main="EW Rets", ylab="Ret Level")
boxplot(df$vwretd, col="blue", main="VW Rets", ylab="Ret Level")
boxplot(df$RET, col="red", main="TSLA Rets", ylab="Ret Level")

par(mfrow=c(1,1))

#using ggplot
gb1 <- ggplot(df, aes(x="", y=sprtrn))+
  geom_boxplot(color="orange", fill="orange", outlier.color="black")+
  labs(y="S&P 500 Rets", x="")+
  ggtitle("S&P Ret")+
  stat_boxplot(geom="errorbar")+
  theme_gray()+
  theme(plot.title=element_text (hjust=0.5))
gb2 <- ggplot(df, aes(x="", y=ewretd))+
  geom_boxplot(color="green", fill="green", outlier.color="black")+
  labs(y="TSLA EW Rets", x="")+
  ggtitle("EW Ret")+
  stat_boxplot(geom="errorbar")+
  theme_gray()
gb3 <- ggplot(df, aes(x="", y=vwretd))+
  geom_boxplot(color="blue", fill="blue", outlier.color="black")+
  labs(y="TSLA VW Rets", x="")+
  ggtitle("VW Ret")+
  stat_boxplot(geom="errorbar")+
  theme_gray()
gb4 <- ggplot(df, aes(x="", y=RET))+
  geom_boxplot(color="red", fill="red", outlier.color="black")+
  labs(y="TSLA Rets", x="")+
  ggtitle("TSLA Ret")+
  stat_boxplot(geom="errorbar")+
  theme_gray()
grid.arrange(gb1,gb2,gb3,gb4, nrow=1, top="TSLA and MKT Ret BoxPlots")



#^^^^^^^^^^^^^^^^^^Density Plot^^^^^^^^^^^^^^^^^^^^^^^^
#in ggplot
ggplot(df)+geom_density(aes(x=sprtrn), color="orange", fill="orange", alpha=.1)+
  geom_density(aes(sprtrn), color="red", fill="red", alpha=.1)+
  theme_classic()
#add other variables and a legend to density plot
dfden <- data.frame(sp=df$sprtrn, vwret=df$vwretd, tsla=df$RET, ewret=df$ewretd)

den <- gather(dfden)
ggplot(den, aes(x=value, fill=key))+
  geom_density(alpha=0.3)+theme_gray()
#^^^^^^^^^^^^^^^^^^Violin Plot^^^^^^^^^^^^^^^^^^^^^^^^
#use vioplot library
par(mar=c(2,2,2,2)+1)
vioplot(df$sprtrn, df$vwretd, df$ewretd, df$RET, col=c("orange", "green", "blue", "red"),
        names=c("S&P 500", "VW", "EW", "TSLA"))
#use ggplot
gv1 <- ggplot(df, aes(x="", y=sprtrn))+
  geom_violin(color="orange", fill="orange")+
  geom_boxplot(width=.25)+
  stat_summary(fun=mean, geom="point", shape=23, size=3, fill="gold")+
  labs(y="S&P 500 Rets", x="")+
  ggtitle("Violin Plot")+
  theme(plot.title=element_text (hjust=0.5))
gv1
gv2 <- ggplot(df, aes(x="", y=vwretd))+
  geom_violin(color="blue", fill="blue")+
  geom_boxplot(width=.25)+
  stat_summary(fun=mean, geom="point", shape=23, size=3, fill="gold")+
  labs(y="VW Rets", x="")+
  ggtitle("Violin Plot")+
  theme(plot.title=element_text (hjust=0.5))
gv2
gv3 <- ggplot(df, aes(x="", y=ewretd))+
  geom_violin(color="green", fill="green")+
  geom_boxplot(width=.25)+
  stat_summary(fun=mean, geom="point", shape=23, size=3, fill="gold")+
  labs(y="EW Rets", x="")+
  ggtitle("Violin Plot")+
  theme(plot.title=element_text (hjust=0.5))
gv3
gv4 <- ggplot(df, aes(x="", y=RET))+
  geom_violin(color="red", fill="red")+
  geom_boxplot(width=.25)+
  stat_summary(fun=mean, geom="point", shape=23, size=3, fill="gold")+
  labs(y="TSLA Rets", x="")+
  ggtitle("Violin Plot")+
  theme(plot.title=element_text (hjust=0.5))
gv4

grid.arrange(gv1, gv2, gv3, gv4, nrow=1, top="Violin Plots of Rets")
#^^^^^^^^^^^^^^^^^^Scatter Plot^^^^^^^^^^^^^^^^^^^^^^^^
#use base r
plot(df$sprtrn, df$RET)

#use ggplot
ggplot(df, aes(vwretd, RET))+
  geom_point(color="purple")+
  labs(y="TSLA Ret", x="MKT Ret")+
  ggtitle("Scatter Plot of S&P 500 and TSLA")+
  theme(plot.title=element_text (hjust=0.5))+
  stat_regline_equation(label.x=-.1, label.y=.18, size=5)

#^^^^^^^^^^^^^^^^^^Plotting a grid of different Plots^^^^^^^^^^^^^^^^^^^^^^^^
g <- grid.arrange(gh1, gb1, gv1, gl1, nrow=2, ncol=2, top="S&P 500 Several Plot Types")
g

#save the plot to a jpeg on computer
ggsave(file="Gatti_dataVizplots.jpg", grid.arrange(gh1, gb1, gv1, gl1, nrow=2, ncol=2, top="S&P 500 Several Plot Types"))

#alternative way
#open blank file
jpeg("gatti_datavizplots2.jpg", width=800, height=600)
#create the plot
grid.arrange(gh1, gb1, gv1, gl1, nrow=2, ncol=2, top="S&P 500 Several Plot Types")
#close the file
dev.off()