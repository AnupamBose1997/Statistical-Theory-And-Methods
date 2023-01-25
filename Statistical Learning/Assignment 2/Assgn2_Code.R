library(Metrics)
library(MASS)
library(tidyverse)
library(sf)
library(corrgram)
#install.packages("corrgram")

#import
df = read.csv("brexit.csv")
#exploration of data
head(df)
view(df)
summary(df)
str(df)
#list of all possible models

log_reg_model <- glm(voteBrexit ~., family = binomial, data = df)
print(summary(log_reg_model))

#correlation matrix
cor_mat <- df[, c(1,2,3,4,5,6)]
round(cor(cor_mat),2)

 
#Random samples, 50-50 test&train data
idx = sample(1:344, 68)
train_data = df[idx, ]
test_data = df[-idx, ]

formulas = c("voteBrexit ~ abc1", "voteBrexit ~ notBornUK", "voteBrexit ~ medianIncome","voteBrexit ~ medianAge","voteBrexit ~ withHigherEd",
             "voteBrexit ~ abc1 + notBornUK", "voteBrexit ~ abc1 + medianIncome", "voteBrexit ~ abc1 + medianAge","voteBrexit ~ abc1 + withHigherEd",
             "voteBrexit ~ notBornUK + medianIncome","voteBrexit ~ notBornUK + medianAge","voteBrexit ~ notBornUK + withHigherEd","voteBrexit ~ medianIncome + medianAge",
             "voteBrexit ~ medianIncome + withHigherEd","voteBrexit ~ medianAge + withHigherEd",
             "voteBrexit ~ abc1 + notBornUK + medianIncome","voteBrexit ~ notBornUK + medianIncome + medianAge","voteBrexit ~ medianIncome + medianAge + withHigherEd",
             "voteBrexit ~ abc1 + medianIncome + medianAge","voteBrexit ~ abc1 + medianAge + withHigherEd","voteBrexit ~ abc1 + notBornUK + medianAge",
             "voteBrexit ~ abc1 + notBornUK + withHigherEd","voteBrexit ~ notBornUK + medianIncome + withHigherEd","voteBrexit ~ notBornUK + medianAge + withHigherEd",
             "voteBrexit ~ abc1 + medianIncome + withHigherEd",
             "voteBrexit ~ abc1 + notBornUK + medianIncome + medianAge","voteBrexit ~ notBornUK + medianIncome + medianAge + withHigherEd",
             "voteBrexit ~ abc1 + medianIncome + medianAge + withHigherEd","voteBrexit ~ abc1 + notBornUK + medianAge + withHigherEd",
             "voteBrexit ~ abc1 + notBornUK + medianIncome + withHigherEd",
             "voteBrexit ~ abc1 + notBornUK + medianIncome + medianAge + withHigherEd")


predictive_log_likelihood = rep(NA, length(formulas))
for (i in 1:length(formulas)){
  #First fit a linear regression model with the training data
  current_model = glm(formula = formulas[i], data = train_data, family = binomial(link = 'logit'))
  #Extract the 'dispersion parameter' from the model - recall this is the unbiased estimate for the rsigma = sqrt(summary(current_model)$dispersion)
  #Now use this model to evaluate the probability of the test outputs
  #Get the predicted mean for each new data point
  ypredict_mean = predict(current_model, test_data,type='response')
  #Now calculate the predictive log probability by summing the
  #log probability of each output value in the test data
  predictive_log_likelihood[i] = sum(dbinom(test_data$voteBrexit, ypredict_mean, size=344-68))
}
plot(1:length(formulas), predictive_log_likelihood,
     xlab="Model Number", ylab="Log Probability")



winner = rep(NA, 100)
for (iteration in 1:100){
  #Make a new random training data - test data split
  idx = sample(1:344, 172)
  train_data = df[idx, ]
  test_data = df[-idx, ]
  predictive_log_likelihood = rep(NA, length(formulas))
  for (i in 1:length(formulas)){
    #First fit a linear regression model with the training data
    current_model = glm(formula = formulas[i], data = train_data, family = binomial(link = 'logit'))
    #Extract the 'dispersion parameter' from the model - recall this is the unbiased estimate for the rsigma = sqrt(summary(current_model)$dispersion)
    #Now use this model to evaluate the probability of the test outputs
    #Get the predicted mean for each new data point
    ypredict_mean = predict(current_model,test_data,type='response')
    #Now calculate the predictive log probability by summing the
    #log probability of each output value in the test data
    predictive_log_likelihood[i] = sum(dbinom(test_data$voteBrexit, ypredict_mean, size=344-172))
  }
  #Find the winning model for this iteration
  winner[iteration] = which.max(predictive_log_likelihood)
}
#Plot a histogram of how often each model wins
hist(winner, breaks = seq(1:31), xlab='Model', ylab='Frequency', main='')
ggplot(data=as.data.frame(winner), aes(winner)) + 
  geom_histogram(bins = 30,binwidth = 0.09)

formulas[28]

#final_model

final_model = glm("voteBrexit ~ abc1 + medianIncome + medianAge + withHigherEd", data = df, family = binomial(link = 'logit'))
final_model_pred = predict(current_model,test_data,type='response')
summary(final_model)

#For_abc1

#Get critical value of z_2.5% (should be 1.96)
zc = qnorm(0.975)

#C.I of abc1
CI_abc1 = summary(final_model)$coefficients[2,1] + c(-1,1)*zc*summary(final_model)$coefficients[2,2]
print(CI_abc1)

#C.I of medianIncome
CI_medianIncome = summary(final_model)$coefficients[3,1] + c(-1,1)*zc*summary(final_model)$coefficients[3,2]
print(CI_medianIncome)

#C.I of medianAge
CI_medianAge = summary(final_model)$coefficients[4,1] + c(-1,1)*zc*summary(final_model)$coefficients[4,2]
print(CI_medianAge)

#C.I of withHigherEd
CI_HighEd = summary(final_model)$coefficients[5,1] + c(-1,1)*zc*summary(final_model)$coefficients[5,2]
print(CI_HighEd)