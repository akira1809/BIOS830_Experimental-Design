#for part (a)

#install the MBESS package that contains the 
#"ci.c()" function for computing confidence intervals
#in a fixed effects ANOVA model
#install.packages("MBESS")

#add the package in library
library(MBESS)

#now compute the confidence interval for our
#soap experiment.

#our contrast is (1, -1/2, -1/2)
#significant level is alpha = 0.05
#3 treatments with equal size r = 4
#mean square error is 0.0772
#treatment mean is given in the solution
ci.c(means = c(-0.0350, 2.7000, 1.9925), s.anova=sqrt(0.0772), c.weights=c(1, -1/2, -1/2), n=c(4, 4, 4), N=12, conf.level=.95)

#for part (b)
msE <- 0.0772
c <- c(1, -0.5, -0.5)
ymean<- c(-0.0350, 2.7000, 1.9925)
r<-4
T <- (sum(ymean*c))^2/(msE*(sum(c^2))/4)

#for part (d)

#we actually input the full data here
#3 different soaps
soap.regular <- c(-0.30, -0.10, -0.14, 0.40)
soap.deodorant <- c(2.63, 2.61, 2.41, 3.15)
soap.moisture <- c(1.86, 2.03, 2.26, 1.82)
#Combine as single variable "soap.soaps"
soap.soaps <- c(soap.regular, soap.deodorant, soap.moisture)
#Create factor variable(soap type)
soap.type <-rep(c("reg", "deo", "mois"), c(4, 4, 4))
soap.type <-factor(soap.type)

#Analysis of Variance:
#select alpha = 0.01
soap.aov <-aov(soap.soaps ~ soap.type)
anova(soap.aov)

#Tukey multiple comparisons test
#99% family-wise confidence level
TukeyHSD(soap.aov, conf.level = 0.99)
