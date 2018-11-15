#HW1 Question 9

#Write our own one sample t-test
#Instead of plug in data, we plug in mean and sd of the data
#So we can bring in information of rho
#our ttest return p value

ttest <- function(samplemean, samplesd, n)
{
    obs <-samplemean/samplesd
    
    #pt gives the cdf of t-distribution
    pvalue <- 2*min(pt(obs, n-1), 1 - pt(obs, n - 1))
    pvalue
}

# Read in our rho value
rho <- c(-0.50, -0.25, -0.10, 0.00, 0.10, 0.25, 0.50)
rho_coefficient <- c(0.58, 0.77, 0.90, 1.00, 1.11, 1.29, 1.73)

# Specify Sample Size for each simulation
N <-10000

# Specify the number of simulations for each rho value
M <-1000

#Create vecotr for storing results
result <- vector(mode = "numeric", length = M)

#Create vector for storing type one error rate
#from our simulation
typeIerror <- vector(mode = "numeric", length = length(rho))

# Simulate data and run the t test
for (i in 1:length(rho)){
  for(j in 1: M){
    #simulate the AR(1) process with lag rho
    sample<- arima.sim(n = N, list(ar=c(rho[i])),innov= rnorm(N, 0, 5))
    
    #compute sample mean
    samplemean <-mean(sample)
    
    #compute sample sd, and also modify according to Question 8
    samplesd <- sd(sample)/ sqrt(N)* rho_coefficient[i]
    
    #run the one sample ttest using the function we
    #defined above
    result[j] <- ttest(samplemean, samplesd, N)
  }
  
  #compute the type I error rate among M simulations
  #for a single rho value
  #we use 0.05 as the threshhold for p value
  typeIerror[i] <-sum(ifelse(result<0.05, 1, 0))/M
  
}

#Print out TypeIerror rate here:
typeIerror
