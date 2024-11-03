#import libraries
library(ggplot2)
library(gridExtra)

#set a working directory
setwd("C:/Users/aless/Documents/434/R/Data")

#read in AVGO data retrieved from CRSP
avgo <- read.csv("gatti_AVGO.csv")

#calculate the mean and standard deviation of returns
mean_avgo <- mean(avgo$RET)
std_avgo <- sd(avgo$RET)

#give user selection for sample size, target, # of trials
sample_size <- as.integer(readline("Please enter sample size: "))
target <- as.numeric(readline("Please enter a target(in decimals): "))
num_trials <- as.integer(readline("Please enter the number of trials: "))

#initialize vector to store semidevs, target semidevs, and harmonic means
semidevs <- numeric(num_trials)
target_semidevs <- numeric(num_trials)
harmonic_means <- numeric(num_trials)

#generate random samples
for (i in 1:num_trials) {
  rand_samples <- rnorm(sample_size, mean=mean_avgo, sd=std_avgo)
  
  #calculate semi-deviation and store
  under_mean <- rand_samples[rand_samples < mean_avgo]
  semidev <- sd(under_mean)
  semidevs[i] <- semidev
  
  #and for target semi-deviation
  under_target <- rand_samples[rand_samples < target]
  target_semidev <- sd(under_target)
  target_semidevs[i] <- target_semidev
  
  #and for harmonic mean
  harmonic_means[i] <- 2 / (1 / mean(rand_samples) + 1 / target)}

#generate descriptive stats on deviations and write to csv file
semidevs_summary <- summary(semidevs)
semidevs_summary_statistics <- c("Min" = semidevs_summary[1], "1st Qu." = semidevs_summary[2], "Median" = semidevs_summary[3], "Mean" = semidevs_summary[4], "3rd Qu." = semidevs_summary[5], "Max" = semidevs_summary[6])
semidevs_summary_df <- data.frame(Summary = names(semidevs_summary_statistics), Value = unname(semidevs_summary_statistics))
write.csv(semidevs_summary_df, file="gatti_semidev_stats.csv", row.names=FALSE)
target_semidevs_summary <- summary(target_semidevs)
target_semidevs_summary_statistics <- c("Min" = target_semidevs_summary[1], "1st Qu." = target_semidevs_summary[2], "Median" = target_semidevs_summary[3], "Mean" = target_semidevs_summary[4], "3rd Qu." = target_semidevs_summary[5], "Max" = target_semidevs_summary[6])
target_semidevs_summary_df <- data.frame(Summary = names(target_semidevs_summary_statistics), Value = unname(target_semidevs_summary_statistics))
write.csv(target_semidevs_summary_df, file="gatti_targetsemidev_stats.csv", row.names=FALSE)

#create a histogram of harmonic mean stats
hm_df <- data.frame(Harmonic_Means = harmonic_means)
hm_hist <- ggplot(hm_df, aes(Harmonic_Means))+
  geom_histogram(color="orange", fill="orange", bins=10)+
  labs(y="Number of Obs", x="Harmonic Mean Bin")+
  ggtitle("Histogram of Harmonic Means")+
  theme_classic()+
  theme(plot.title=element_text (hjust=0.5))

#create a histogram of the deviation stats
dev_df <- data.frame(Semi_Deviations = semidevs, Target_Semi_Deviations = target_semidevs)
semidev_hist <- ggplot(dev_df, aes(Semi_Deviations))+
  geom_histogram(color="blue", fill="blue", bins=10)+
  labs(y="Number of Obs", x="Semi-Deviation Bin")+
  ggtitle("Histogram of Semi-Deviations")+
  theme_classic()+
  theme(plot.title=element_text (hjust=0.5))

target_semidev_hist <- ggplot(dev_df, aes(Target_Semi_Deviations))+
  geom_histogram(color="red", fill="red", bins=10)+
  labs(y="Number of Obs", x="Target Semi-Deviation Bin")+
  ggtitle("Histogram of Target Semi-Deviations")+
  theme_classic()+
  theme(plot.title=element_text (hjust=0.5))

#write histograms to a jpeg file
ggsave(file="gatti_histogram_stats.jpg", grid.arrange(hm_hist, semidev_hist, target_semidev_hist, nrow=1, ncol=3, top="Harmonic Mean and Deviation Histograms"))
