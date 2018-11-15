#HW2 Question 3
#develop a script that functions similarly as pwr.anova.test
#Verify this function give consistent result as pwr.anova.test

library(pwr)

#to test consistency, we need to specify necessary parameter values
#for pwr.anova.test function

#number of groups, significance level, delta, empirical standard deviation, 
#power. The value can be flexibly chosen for further check of consistency.
v <- 3
alpha <- 0.05
delta <- 5
stdev <- 3
power <- 0.9

#compute the effective size ES, consider the worst scenario of the study
#two groups differ by delta, and the rest stay in the middle
ES <- sqrt(((delta/2)^2 + (delta/2)^2)/(v*stdev^2))

#generate the sample size computed from pwr.anova.test
#this is the size for each group
size1 <- ceiling(pwr.anova.test(k = v, n = NULL, f = ES, sig.level=alpha, power = power)$n)

#now write our function

#this is our main function. assume we already know
#phi_sqr, then we do the iteration to find the sample size

#function to generate the next lambda_sqr
flambda_sqr = function(power, df1, df2, sig.level){
  lambda_sqr = 1
  while(pf(1-power, df1, df2, lambda_sqr)< pf(1-sig.level, df1, df2)){
    lambda_sqr <- lambda_sqr+0.5
  }
  return(lambda_sqr)
}


#sample.anova
#our own function to compute sample size

sample.anova = function(k, power, sig.level, delta, sigma){
  #initialize the first df2
  v2 <-1000
  repeat{
    #get the noncentral parameter lambda_sqr
    lambda_sqr <- flambda_sqr(power, k-1, v2, sig.level)
    #compute phi_sqr
    phi_sqr <- lambda_sqr/k
    #compute the group sample size r
    r <- ceiling(2*k*sigma^2*phi_sqr/delta^2)
    #check thresh hold for interpolation stop point
    temp <-k*(r-1)
    if (abs(v2 -temp) >=1){
      v2<- temp
    } 
    else break
  }
  cat("sample size per group is", r)
}

#check consistency
sample.anova(k = v, power = power, sig.level= alpha, delta=delta, sigma=stdev)

