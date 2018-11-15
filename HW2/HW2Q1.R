#HW2 Question 1

#for part (a): compute the power

#load the pwr library
#install.packages("pwr")
library(pwr)

# set the working directory to location of Hyptertension.csv
setwd("C:/Akira/Important_Data/articles/academics/KUMC/2017 Spring/BIOS830(Experimental Design)/HW/HW02")

#read in the data.
Chole <- read.csv("Cholesterol.csv")

#compute effective size

N = length(Chole$Subject)#total sample size
v = 5 #number of treatments
r = 20#group sample size
#get within group information
withinRunStats = function(x) {c(sum = sum(x), mean= mean(x), var = var(x),n = length(x))
}
withinStats<-tapply(Chole$Response, Chole$Treatment, withinRunStats)
group_var <- sapply(withinStats, '[[', "var")
group_mean <- sapply(withinStats, '[[', "mean")
#get MSE as an estimate of the population variance
fit.aov <- aov(Chole$Response ~ factor(Chole$Treatment))
#we can also print out the reuslt here to get the anova table
#so part (b) is also done here
Chole_anova <-anova(fit.aov)
msE <- Chole_anova$`Mean Sq`[2]
ES <- sqrt(r*sum((group_mean- mean(Chole$Response))^2)/N/msE)

#compute the power
power <- pwr.anova.test(k = v, n= r, f = ES, sig.level=0.05, power = NULL)
# we found power = 0.85 here

#part (c)

#input contrast
a <- c(1, -1/4, -1/4, -1/4, -1/4)
b <- c(0, 1/2, -1/2, 1/2, -1/2)
c <- c(0, 1/2, 1/2, -1/2, -1/2)
contrast <-list(a, b, c)
n <-length(contrast)

#since the group number is small, we use Bonferonni method
m<-5
df <- N-v

#compute critical coefficient
w <- qt(1 - 0.05/(2*m), df)
#the following critical coefficient is for part (e) of this question
#w_e<- qt(1 - 0.025/(2*m), df)
#we do a loop here to compute CI for all three contrast at once
for (i in 1:n){
  #compute least square estimate
  lse <- sum(contrast[[i]]*group_mean)
  #compute estimate for contrast variance
  lse_var <- msE*sum(contrast[[i]]^2)/r
  #compute minimal significat difference
  msd <- w*sqrt(lse_var)
  # for part e, we compute msd as:
  #msd <- w_e*sqrt(lse_var)
  #compute lower and upper bound for CI
  ci_lower<- lse-msd
  ci_upper<- lse+msd
  #print the result of CI
  cat("The CI for (", contrast[[i]], ") is (", ci_lower<- lse-msd, ", ", ci_upper<- lse+msd, ")", "\n")
}

#part (d)
w_T <- qtukey(0.95, 5, df = 95)/sqrt(2)

#part (e)
# see the two lines of code I added in part(c)
# simply changed how to compute critical coefficient w
