#Chapter 5 Question 8.

#part (a) check for constant variance

#input data
#germ1: code for spring/stratified
#germ2: code for spring/unstratified
#germ3: code for summer/stratified
#germ4: code for summmer/unstratified
germ1 <- c(12, 13, 2, 7, 19, 0, 0, 3, 17, 11)
germ2 <- c(6, 2, 0, 2, 4, 1, 0, 10, 0, 0)
germ3 <- c(6, 4, 5, 7, 6, 5, 7, 5, 2, 3)
germ4 <- c(0, 6, 2, 5, 1, 5, 2, 3, 6, 6)
germs <- c(germ1, germ2, germ3, germ4)

#generate some label
n = rep(10, 4)
group <- rep(c("germ1", "germ2", "germ3", "germ4"), n)

#calculate stats within groups
withinRunStats = function(x) {c(sum = sum(x), mean= mean(x), var = var(x),n = length(x))
}
withinStats<-tapply(germs, group, withinRunStats)

#ANOVA
data = data.frame(germs, group = factor(group))
fit.aov <- aov(germs ~ group, data= data)
germ_anova <-anova(fit.aov)

#plot to check constant varaicne.
#check the one with square root of standardized
#residuals against fit values
plot(fit.aov)

#part (b)
#transform the data based on the binomial distribution
#of the sample

m <-20
hgerms <- asin(sqrt(germs/m))
hwithinStats<-tapply(hgerms, group, withinRunStats)

#part (c)
#common transformation
group_var <- sapply(withinStats, '[[', "var")
group_mean <- sapply(withinStats, '[[', "mean")
reg1 <-lm(log(group_var)~log(group_mean))
plot(log(group_mean), log(group_var), main="check for possibility of transform")
abline(reg1)

#transform the data based on reg1:
hgerms1 <- germs^(2/5)

#Check constance for variance again

#Calculate within-run stats:
hwithinStats1 <- tapply(hgerms1, group, withinRunStats)
#ANOVA analysis
hdata1 <- data.frame(hgerms1, group = factor(group))
hfit1.aov <- aov(hgerms1 ~ group, data= hdata1)
hanova1 <-anova(hfit1.aov)
#compute the MSE for transformed data ANOVA model
hmsE1 <- hanova1$`Mean Sq`[2]

#plot for model diagnostic under transformed data
plot(hfit1.aov)

#part (d)
#compute 95% confidence interval for the
#following contrasts

#creating the list of contrasts
a<- c(1, -1, 0, 0)
b<- c(1, 0, -1, 0)
c<- c(1, 0, 0, -1)
d<- c(0, 1, -1, 0)
e<- c(0, 1, 0, -1)
f<- c(0, 0, 1, -1)
g<- c(1/2, 1/2, -1/2, -1/2)
h<- c(1/2, -1/2, 1/2, -1/2)
contrast <- list(a, b, c, d, e, f, g, h)
n <- length(contrast)

#loop here to compute all CI at once
for (i in 1:n){
#compute least square estimate
lse <- sum(contrast[[i]]*group_mean)
#compute estimate for variance
lse_var <- sum(contrast[[i]]^2 * group_var)/10
#compute degree of freedom for Satterthwaites approximation
df <- 9*lse_var^2/sum((contrast[[i]]^2 * group_var/10)^2)
#compute critical efficient
w<- sqrt(3*qf(0.95, 3, df))
#compute minimal significat difference
msd <- w*sqrt(lse_var)
#compute lower and upper bound for CI
ci_lower<- lse-msd
ci_upper<- lse+msd
#print the result of CI
cat("The CI for (", contrast[[i]], ") is (", ci_lower<- lse-msd, ", ", ci_upper<- lse+msd, ")", "\n")
}