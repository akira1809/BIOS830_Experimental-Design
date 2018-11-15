#HW1 Question 3

#create our data
Treatment1 <-c(3.52, 3.36, 3.57, 4.19, 3.88, 3.76, 3.94)
Treatment2 <-c(3.47, 3.73, 3.38, 3.87, 3.69, 3.51, 3.35, 3.64)
Treatment3 <-c(3.54, 3.52, 3.61, 3.76, 3.65, 3.51)
Treatment4 <-c(3.74, 3.83, 3.87, 4.08, 4.31, 3.98, 3.86, 3.71)

#Comibne and Stack the data into standard format

#cbind only works on vecotrs of the same length
n <- max(length(Treatment1), length(Treatment2),length(Treatment3),length(Treatment4))
length(Treatment1) <- n
length(Treatment2) <- n
length(Treatment3) <- n
length(Treatment4) <- n

#combine our vectors(treatments)
Combined_Treatments <-data.frame(cbind(Treatment1, Treatment2, Treatment3, Treatment4))
#stack our observations across treatments
Stacked_Treatments <- stack(Combined_Treatments)

#delete rows with NA to get cleaned data hw1q3
hw1q3<- Stacked_Treatments[complete.cases(Stacked_Treatments),]

#obtain overall mean and treatment effects

#the over all mean is 3.718276
m <- mean(hw1q3$values)

#we can get mean by treatment
a <- aggregate(values~ind, hw1q3, mean)

#obtain treatment effects by taking the difference
treatment_effects <- a$values - m

# Perform ANOVA
hw1q3anova <- aov(values~ind, data = hw1q3)
anova(hw1q3anova)




