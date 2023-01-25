

MikTex::install_MikTex()
install.packages("MikTex")
#libraries
library(Metrics)
library(MASS)
library(tidyverse)
library(sf)

#Reading Data File
df = read.csv("medal_pop_gdp_data_statlearn.csv")
df
length(df)
#Question 1

model_train = glm(Medal2012 ~ Population + GDP , data= df)
summary(model_train)

#t-statistic Value 
tc = qt(p=0.975, df=68)

#Confidence Interval of poupulation
pop_ci = summary(model_train)$coefficients[2, 1] +
  c(-1,1)*tc*summary(model_train)$coefficients[2, 2]

print(pop_ci)

#Confidence Interval of gdp

gdp_ci = summary(model_train)$coefficients[3, 1] +
  c(-1,1)*tc*summary(model_train)$coefficients[3, 2]

print(gdp_ci)

#Testing model with prediction
model_test=df[,c(2,3)]
pred= predict(model_train, newdata = model_test)

df %>%
  mutate(Pred_Medal2016=round(pred)) %>%
  select(Medal2016,Pred_Medal2016)%>%
  head(10)

#Plot with log transformed axes
ggplot(data=df, aes(x=Medal2016, y=pred,label = Country)) +
  geom_point() +
  ggtitle("Observed VS Predicted Medal Count for 2016")+
  scale_y_continuous(trans='log10')+
  scale_x_continuous(trans='log10')+
  ylab("Predicted Medal Count for 2016")+
  xlab("Observed Medal Count for 2016")+
  geom_abline(slope = 1, intercept = 0, col = 'red')+
  geom_text(size=1.5,nudge_y = 0.05,  check_overlap = TRUE)

ggplot(data=df, aes(x=pred, y=Medal2016,label = Country)) +
  geom_point() +
  ggtitle("Observed VS Predicted Medal Count for 2016")+
  scale_y_continuous(trans='log10')+
  scale_x_continuous(trans='log10')+
  xlab("Predicted Medal Count for 2016")+
  ylab("Observed Medal Count for 2016")+
  geom_abline(slope = 1, intercept = 0, col = 'red')+
  geom_text(size=1.5,nudge_y = 0.05,  check_overlap = TRUE)


plot(df$Medal2016, round(pred), log= "xy",pch=20, xlab = "Number of medals obtained in 2016", ylab = "Number of medals predicted in 2016")
abline(a=0,b=1,untf= FALSE)

plot(round(pred),df$Medal2016, log= "xy",pch=20, xlab = "Number of medals obtained in 2016", ylab = "Number of medals predicted in 2016")
abline(a=0,b=1,untf= FALSE)

#Boxplot of Absolute error 
boxplot(abs(pred - df$Medal2016))

#Quantile
quantile(abs(pred - df$Medal2016))

#Outliers of Absolute errors: 
#Values Beyond [75th Quantile +(1.5*IQR)]

6.61 + 1.5*IQR(abs(pred - df$Medal2016))  


#Countries with Absolute Error Outliers

df %>% 
  select(Country,Medal2016) %>%
  mutate(Absolute_Error = abs(pred - Medal2016)) %>%
  mutate(pred=round(pred)) %>%
  select(Country,Medal2016,pred,Absolute_Error) %>%
  mutate(Absolute_Error=round(Absolute_Error)) %>%
  filter(Absolute_Error>12.786)%>%
  arrange(desc(Absolute_Error))

#Ploting using ggplot
ggplot(data=df, aes(x=round(pred), y=Medal2016,label = Country))+
  ggplot(data=df,aes(x = log(Observed), y = log(Pred),label = Country))+ geom_point(size = 1, alpha = 0.5)+
  #geom_smooth(method = "loess", col = "red")+
  #geom_smooth(method = "lm") + 
  geom_abline(intercept = 0,slope = 1)+
xlab("Predicted Medal Count for 2016")+
  ylab("Observed Medal Count for 2016")+
  geom_abline(slope = 1, intercept = 0, col = 'red')+
  geom_text(size=1.5,nudge_y = 0.05,  check_overlap = TRUE)


#Mean Absolute Error and Root Mean Squared Error
mae(df$Medal2016,pred)
rmse(df$Medal2016,pred)


#Question 2
log_of_Medal2012=log(df$Medal2012)
log_model_train = glm(log_of_Medal2012 ~ Population + GDP , data= df)
summary(log_model_train)
tc <- qt(p=0.975, df=68)
#Confidence Interval of population
pop_ci_log <- summary(log_model_train)$coefficients[2, 1] +
  c(-1,1)*tc*summary(log_model_train)$coefficients[2, 2]
print(pop_ci_log)
#Confidence Interval of gdp
gdp_ci_log = summary(log_model_train)$coefficients[3, 1] +
  c(-1,1)*tc*summary(log_model_train)$coefficients[3, 2]
print(gdp_ci_log)

#Testing model with prediction
log_df <- df[,c(2,3)]
log_predictions <- exp(predict(log_model_train, newdata = log_df))


df %>%
  mutate(Pred_Medal2016=round(log_predictions)) %>%
  select(Medal2016,Pred_Medal2016)%>%
  head(10)

#Plot with log transformed axes
ggplot(data=df, aes(x=log_predictions, y=Medal2016,label = Country)) +
  geom_point() +
  ggtitle("Observed vs Predicted Medal Count for 2016")+
  scale_y_continuous(trans='log10')+
  scale_x_continuous(trans='log10')+
  xlab("Predicted Medal Count for 2016")+
  ylab("Observed Medal Count for 2016")+
  geom_abline(slope = 1, intercept = 0, col = 'red')+
  geom_text(size=1.5,nudge_y = 0.05,  check_overlap = TRUE)

#Boxplot of Absolute error 
boxplot(abs(log_predictions - df$Medal2016))

#Quantile
quantile(abs(log_predictions - df$Medal2016))

#Outliers of Absolute errors: 
#Values Beyond [75th Quantile +(1.5*IQR)]

8.76 + 1.5*IQR(abs(log_predictions - df$Medal2016))  


#Countries with Absolute Error Outliers

df %>% 
  select(Country,Medal2016) %>%
  mutate(Absolute_Error = abs(log_predictions - Medal2016)) %>%
  mutate(log_predictions=round(log_predictions)) %>%
  select(Country,Medal2016,log_predictions,Absolute_Error) %>%
  mutate(Absolute_Error=round(Absolute_Error)) %>%
  filter(Absolute_Error>18.619)%>%
  arrange(desc(Absolute_Error))

#Mean Absolute Error and Root Mean Squared Error
mae(df$Medal2016,log_predictions)
rmse(df$Medal2016,log_predictions)

#Q.3
Poisson_Model = glm(Medal2012 ~ Population + GDP , data= df,family = poisson(link='log'))
summary(Poisson_Model)
tc = qt(p=0.975, df=68)
pop_ci_poi = summary(Poisson_Model)$coefficients[2, 1] +
  c(-1,1)*tc*summary(Poisson_Model)$coefficients[2, 2]
print(pop_ci_poi)

gdp_ci_poi = summary(Poisson_Model)$coefficients[3, 1] +
  c(-1,1)*tc*summary(Poisson_Model)$coefficients[3, 2]
print(gdp_ci_poi)
#Testing model with prediction
poi_df <- df[,c(2,3)]
poi_predictions= predict(Poisson_Model, newdata = poi_df, type = "response")
df %>%
  mutate(Pred_Medal2016=round(poi_predictions)) %>%
  select(Medal2016,Pred_Medal2016)%>%
  head(10)

#Plot with log transformed axes
ggplot(data=df, aes(x=poi_predictions, y=Medal2016,label = Country)) +
  geom_point() +
  ggtitle("Observed VS Predicted Medal Count for 2016")+
  scale_y_continuous(trans='log10')+
  scale_x_continuous(trans='log10')+
  xlab("Predicted Medal Count for 2016")+
  ylab("Observed Medal Count for 2016")+
  geom_abline(slope = 1, intercept = 0, col = 'red')+
  geom_text(size=1.5,nudge_y = 0.05,  check_overlap = TRUE)

#Boxplot of Absolute error 
boxplot(abs(poi_predictions - df$Medal2016))

#Quantile
quantile(abs(poi_predictions - df$Medal2016))

#Outliers of Absolute errors: 
#Values Beyond [75th Quantile +(1.5*IQR)]

8.33 + 1.5*IQR(abs(poi_predictions - df$Medal2016))  


#Countries with Absolute Error Outliers

df %>% 
  select(Country,Medal2016) %>%
  mutate(Absolute_Error = abs(poi_predictions - Medal2016)) %>%
  mutate(poi_predictions=round(poi_predictions)) %>%
  select(Country,Medal2016,poi_predictions,Absolute_Error) %>%
  filter(Absolute_Error>15.93)%>%
  arrange(desc(Absolute_Error))

#Mean Absolute Error and Root Mean Squared Error
mae(df$Medal2016,poi_predictions)
rmse(df$Medal2016,poi_predictions)


#Q.4

theta = seq(0.01,1000,0.5)
logLikelihood <- function(b1){
  #logLikelihood for faithful data, with slope of b1 and intercept of 33
  Olympics_model3 = glm(Medal2012 ~ Population + GDP , data= df,family = negative.binomial(theta = b1))
  LL = logLik(Olympics_model3)
  return(LL)
}

length(theta)
print(logLikelihood(2))

LL = rep(NA, 2000) #defining the LL variable
for (i in 1:2000){ #looping over values to get log likelihood
  LL[i] = logLikelihood(theta[i])
}

LL[1:5]

plot(theta, LL, xlab='Slope', ylab='Log Likelihood')

nLL <- function(b1){-logLikelihood(b1)}
optimise_output = optim(par=10, fn = nLL)
print(optimise_output$par) #print the optimised slope value

negbin_model = glm(Medal2012 ~ Population + GDP , data= df,family = negative.binomial(theta = 1.54))
summary(negbin_model)

tc = qt(p=0.975, df=68)
#Confidence Interval of population
pop_negbin_ci = summary(negbin_model)$coefficients[2, 1] +
  c(-1,1)*tc*summary(negbin_model)$coefficients[2, 2]
print(pop_negbin_ci)
#Confidence Interval of gdp
gdp_negbin_ci = summary(negbin_model)$coefficients[3, 1] +
  c(-1,1)*tc*summary(negbin_model)$coefficients[3, 2]
print(gdp_negbin_ci)

#Testing model with prediction
negbin_df <- df[,c(2,3)]
negbin_predictions= predict(negbin_model, newdata = negbin_df, type = "response")


df %>%
  mutate(Pred_Medal2016=round(negbin_predictions)) %>%
  select(Medal2016,Pred_Medal2016)%>%
  head(10)


#Plot with log transformed axes
ggplot(data=df, aes(y=negbin_predictions, x=Medal2016,label = Country)) +
  geom_point() +
  ggtitle("Observed vs Predicted Medal Count for 2016")+
  scale_y_continuous(trans='log10')+
  scale_x_continuous(trans='log10')+
  ylab("Predicted Medal Count for 2016")+
  xlab("Observed Medal Count for 2016")+
  geom_abline(slope = 1, intercept = 0, col = 'red')+
  geom_text(size=1.5,nudge_y = 0.05,  check_overlap = TRUE)


#Quantile
quantile(abs(negbin_predictions - df$Medal2016))

#Outliers of Absolute errors: 
#Values Beyond [75th Quantile +(1.5*IQR)]

6.61 + 1.5*IQR(abs(negbin_predictions - df$Medal2016))  


#Countries with Absolute Error Outliers

df %>% 
  select(Country,Medal2016) %>%
  mutate(Absolute_Error = abs(negbin_predictions - Medal2016)) %>%
  mutate(negbin_predictions=round(negbin_predictions)) %>%
  select(Country,Medal2016,negbin_predictions,Absolute_Error) %>%
  mutate(Absolute_Error=round(Absolute_Error)) %>%
  filter(Absolute_Error>12.62)%>%
  arrange(desc(Absolute_Error))

#Mean Absolute Error and Root Mean Squared Error
mae(df$Medal2016,negbin_predictions)
rmse(df$Medal2016,negbin_predictions)