for (j in 1:100){
  #two levels for A and three levels for B
  #generate randomly assigned objects
  a = sample(1:2, 100, replace = T)
  b = sample(1:3, 100, replace = T)
  ab = as.data.frame(cbind(a, b))
  
  #initilize pvalues
  a2waypval <- c()
  b2waypval <- c()
  a1waypval <- c()
  b1waypval <- c()
  diffa <- c()
  diffb <- c()
  #simulate 1000 times
  for (i in 1:1000){
    #add normal noise to the two way model
    ab$Y = ab$a + ab$b + rnorm(100, mean= 0, sd= 25)
    #record pvalues
    a2waypval[i] <- anova(lm(ab$Y~factor(ab$a)*factor(ab$b)))$"Pr(>F)"[1]
    b2waypval[i] <- anova(lm(ab$Y~factor(ab$a)*factor(ab$b)))$"Pr(>F)"[2]
    a1waypval[i] <- anova(lm(ab$Y~factor(ab$a)))$"Pr(>F)"[1]
    b1waypval[i] <- anova(lm(ab$Y~factor(ab$b)))$"Pr(>F)"[1]
  }
  #compute power difference between two way and separate studies
  diffa[j]<- (1-(length(a2waypval[a2waypval >=.05])/length(a2waypval)))- 
    (1-(length(a1waypval[a1waypval>=.05])/length(a1waypval)))
  diffb[j]<- (1-(length(b2waypval[b2waypval >=.05])/length(b2waypval)))- 
    (1-(length(b1waypval[b1waypval>=.05])/length(b1waypval)))
}
#print the percentage of the simulations where two way model is more powerful
length(diffa[diffa>0])/length(diffa)
length(diffb[diffb>0])/length(diffb)
