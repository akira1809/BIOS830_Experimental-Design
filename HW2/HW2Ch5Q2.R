#HW2 Ch5 Question 2
#soap experiment model diagnostic

#input the data for weightloss on each soap
soap1 <- c(-0.30, -0.10, -0.14, 0.40)
soap2 <- c(2.63, 2.61, 2.41, 3.15)
soap3 <- c(1.72, 2.07, 2.17, 2.01)
soaps <- c(soap1, soap2, soap3)

#Make some labels:
n = rep(4, 3)
group <- rep(c("regular", "deodorant", "moisture"), n)

#Calculate within-run stats:
withinRunStats = function(x) {c(sum = sum(x), mean= mean(x), var = var(x),n = length(x))
}
tapply(soaps, group, withinRunStats)

#ANOVA analysis
data = data.frame(soaps, group = factor(group))
fit.aov <- aov(soaps ~ group, data= data)
anova(fit.aov)

#plot to do model dianostic
plot(fit.aov)