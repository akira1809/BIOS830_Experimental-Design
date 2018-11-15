#HW1 Question 2

library(mosaic)
library(ggplot2)

# set the working directory to location of Hyptertension.csv
setwd("C:/Akira/Important_Data/articles/academics/KUMC/2017 Spring/BIOS830(Experimental Design)/HW/HW01")

#read in the patient data, store it in a data frame called hw1q2
hw1q2 <- read.csv("Hypertension.csv")

#check the number of observations in each treatment
#store the result in a table called a
a <- table(hw1q2$Treatment)

#print table a to see the number of observations
#Treatment A has 90 observations
#Treatment B has 95 observations
print(a)

#plot a histogram of blood measure to roughly check normality
hist(hw1q2$Blood.pressure..mg.Hg.)

# Create a side-by-side boxplot of the data
boxplot(hw1q2$Blood.pressure..mg.Hg. ~ hw1q2$Treatment)

# Sort the data by treatment before doing ANOVA
hw1q2a <-hw1q2[order(hw1q2[, 2]),]

# Perform ANOVA
hw1q2aanova <- aov(Blood.pressure..mg.Hg.~Treatment, data = hw1q2a)
anova(hw1q2aanova)

#get the data for part (b)
hw1q2b <-hw1q2a

#Calculate the observed difference in means
mean(Blood.pressure..mg.Hg. ~ Treatment, data=hw1q2b)
observed <- diff(mean(Blood.pressure..mg.Hg. ~ Treatment, data=hw1q2b))

# To simulate a single trial, we need to shuffle the treatment labels
# we will use this in the next block when we create the randomization distribution
#diff(mean(Blood.pressure..mg.Hg.~shuffle(Treatment), data = hw1q2b))

#Create the randomization distribution
# we simulate 1000 trials here.
hw1q2br <- do(1000)*diff(mean(Blood.pressure..mg.Hg.~shuffle(Treatment), data = hw1q2b))

#we can check what the data look like here
#head(hw1q2br)

#Plotting the randomization distribution
ggplot(data <-hw1q2br) + geom_histogram(mapping=aes(x = B)) + xlab("mean difference")

#Superimpose a line indicating the observed mean difference
ggplot(data=hw1q2br)+ geom_histogram(mapping=aes(x = B)) + xlab("mean difference") + geom_vline(xintercept = observed, linetype = 2, colour = "red")

#Calculate the proportion of simulated diff in means as or more extreme 
#than the observed, which is our p value
p <- prop(~B >= observed, data = hw1q2br)

