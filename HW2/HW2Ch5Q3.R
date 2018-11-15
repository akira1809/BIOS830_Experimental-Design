#HW2 Ch5 Question 3

#Margarine experiment model diagnostic

#part (a) check equail-variance

#input data

magarine1 <- c(167, 171, 178, 175, 184, 176, 185, 172, 178, 178)
magarine2 <- c(231, 233, 236, 252, 233, 225, 241, 248, 239, 248)
magarine3 <- c(176, 168, 171, 172, 178, 176, 169, 164, 169, 171)
butter <- c(201, 199, 196, 211, 209, 223, 209, 219, 212, 210)
times <- c(magarine1, magarine2, magarine3, butter)

#make labels
n <- rep(10, 4)
group <- rep(c("magarine1", "magarine2", "magarine3", "butter"), n)

#Calculate within-run stats:
#the book actually gave us the stats here
#but this is for the purpose of general use.
withinRunStats = function(x) {c(sum = sum(x), mean= mean(x), var = var(x),n = length(x))
}
withinStats<-tapply(times, group, withinRunStats)

#ANOVA analysis
data = data.frame(times, group = factor(group))
fit.aov <- aov(times ~ group, data= data)
anova(fit.aov)

#plot to do model dianostic
#this is really what we need for plotting in part (a)
plot(fit.aov)

#another plot we need in part (a)
group_var <- sapply(withinStats, '[[', "var")
group_mean <- sapply(withinStats, '[[', "mean")
reg1 <-lm(log(group_var)~log(group_mean))
plot(log(group_mean), log(group_var), main="check for possibility of transform")
abline(reg1)

#transform the data based on reg1:
htimes <- 1/times
#redo everything for new response as above:

#Calculate within-run stats:
hwithinStats <- tapply(htimes, group, withinRunStats)
#ANOVA analysis
hdata <- data.frame(htimes, group = factor(group))
hfit.aov <- aov(htimes ~ group, data= hdata)
hanova <-anova(hfit.aov)
#compute the MSE for transformed data ANOVA model
hmsE <- hanova$`Mean Sq`[2]

#plot for model diagnostic under transformed data
plot(hfit.aov)

#plot for standardized residuals against order of observation
order <- c(1:40)
htimes.lm <- lm(htimes ~ order)
htimes.stdres <- rstandard(htimes.lm)
plot(order, htimes.stdres, ylab ="Standardized residuals"
     , xlab = "order of observation")
abline(htimes.lm)

#part (b) compute confidence intervals
library(MBESS)

#extract treat means
hgroup_mean <- sapply(hwithinStats, '[[', "mean")

#---
#if we do ont use the package
#the code within --- give the same result
#contrast <- c(-1, 1/3, 1/3, 1/3)
#hlse <- sum(contrast*hgroup_mean)
#hlse_var <- hmsE*sum(contrast^2)/10
#w1 <-qt(0.975, 40-4)
#msd1 <- w1*sqrt(hlse_var)
#ci1_lower<-hlse-msd1
#ci1_upper<-hlse+msd1

#---


#in hgroup_mean, the groups are re-arranged so butter is the first treatment now
#the following function gives us confidence interval at the level of 95%
ci.c(means = c(hgroup_mean), s.anova=sqrt(hmsE), c.weights=c(-1, 1/3, 1/3, 1/3), n=c(10, 10, 10, 10), N=40, conf.level=.95)

#part (c) computer confidence intervals 
#analysis with unequal error variance.

#compute least square estimate of the contrast
contrast <- c(-1, 1/3, 1/3, 1/3)
lse <- sum(contrast*group_mean)

#compute estimate for the variance of the contrast
lse_var <- sum(contrast^2 * group_var)/10

#compute degree of freedom for the t-distribution
df <- 9*lse_var^2/sum((contrast^2 * group_var/10)^2)

#compute critical coefficient
w<- qt(0.975, df)

#compute msd
msd <- w*sqrt(lse_var)

#compute confidence interval at confidence level 95%
ci_lower<- lse-msd
ci_upper<- lse+msd



