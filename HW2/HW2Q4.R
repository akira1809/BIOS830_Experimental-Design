#HW2 Question 4
#check that for CRD with two groups, balanced study gives largest power

#specify the total number of experimental units
N <- 40
#specify upper bound of the loop later
n <- N-2
#create variables for group sample sizes
n_1 <- c()
n_2 <- c()

#specify the mean and standard deviation for
#the to be simulated samples
#notice: we are not going to use these in pwr.t2n.test
#we are only going to use these to generate sample
mu_1 <- 3
mu_2 <- 6
stdev <-2

#create variable to hold power output
pw <- c()

#computer power as the group sample size changes
for (i in 2:n){
  n_1 <- i
  n_2 <- N-i
  #generate samples
  group1 <- rnorm(n_1, mean = mu_1, sd = stdev)
  group2 <- rnorm(n_2, mean = mu_2, sd = stdev)
  #generate parameters to pass onto power function
  #mean1 <- mean(group1)#sample mean of group1
  #mean2 <- mean(group2)#sample mean of group2
  #this line compute msE to estimate variance
  #msE <- (sum((group1 - mean1)^2) + sum((group2 - mean2)^2))/(N - 2)
  #this line compute effective size
  #d <- abs(mean1 - mean2)/sqrt(msE)
  d <- abs(mu_1 - mu_2)/stdev
  #now we compute the power
  pw[i - 1] <- pwr.t2n.test(n1 = n_1, n2 = n_2, d = d, sig.level= 0.05, power = NULL, 
                        alternative = "two.sided")$power
}
#record where we get max power
n1.max <- which.max(pw) + 1
#print the result
cat("the largest power is achieved at group size (", c(n1.max, N - n1.max), ") for power = ", max(pw))

#plot the trend of power versus group 1 sample size
size1<- c(2:38)
plot(size1, pw, xlab = "group 1 sample size", ylab = "power of the study")