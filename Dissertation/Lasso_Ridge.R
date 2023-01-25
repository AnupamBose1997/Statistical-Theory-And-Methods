#Ridge Regression

library(glmnet)

x_ridge = model.matrix(LogS~.-1, data=data.train.z)
y_train_ridge = data.train$LogS

x_test_ridge = model.matrix(LogS~.-1, data=data.test.z)
y_test_ridge = data.test.z$LogS
set.seed(1234)
lambdas <- 10^seq(2, -3, by = -.01)
ridge_reg = glmnet(x_ridge, y_train_ridge, alpha = 0, family = 'gaussian', lambda = lambdas)

summary(ridge_reg)

max(ridge_reg$lambda)
min(ridge_reg$lambda)

model.matrix(LogS~.-1, data=data.train.z)

set.seed(1234)
cv_ridge <- cv.glmnet(x_ridge, y_train_ridge, alpha = 0, lambda = lambdas, type.measure = "mse")
optimal_lambda <- cv_ridge$lambda.min
log(optimal_lambda)

cv_ridge$cvm[cv_ridge$lambda==cv_ridge$lambda.1se]

log(min(cv_ridge$lambda.1se))

plot(cv_ridge)
abline(v=log(cv_ridge$lambda.min), col = "red", lty=2)
abline(v=log(cv_ridge$lambda.1se), col="blue", lty=2)

# Cross validation to find the optimal lambda penalization
cv.ridge <- cv.glmnet(x_ridge, y_train_ridge, alpha=0)

lbs_fun <- function(fit, offset_x=1, ...) {
  L <- length(fit$lambda)
  x <- log(fit$lambda[L])+ offset_x
  y <- fit$beta[, L]
  labs <- names(y)
  text(x, y, labels=labs, ...)
}
plot(ridge_reg, xvar = "lambda", label=T)
lbs_fun(ridge_reg)
abline(v=cv_ridge$lambda.min, col = "red", lty=2)
abline(v=cv_ridge$lambda.1se, col="blue", lty=2)




# Compute R^2 from true and predicted values
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}

# Prediction and evaluation on train data
predictions_train_ridge <- predict(ridge_reg, s = optimal_lambda, newx = x_ridge)
eval_results(y_train_ridge, predictions_train_ridge, data.train.z)




ridge_Logrel= sum((predictions_test_ridge-y_test_ridge)>-0.7 & (predictions_test_ridge-y_test_ridge)<0.7)/268
# Prediction and evaluation on test data
predictions_test_ridge <- predict(cv_ridge, s = optimal_lambda, newx = x_test_ridge)
eval_results(y_test_ridge, predictions_test_ridge, data.test.z)

varImp(ridge_reg, scale = FALSE, lambda=optimal_lambda)







#LASSO

x_train_lasso = model.matrix(LogS~.-1, data=data.train.z)
y_train_lasso = data.train$LogS

x_test_lasso = model.matrix(LogS~.-1, data=data.test.z)
y_test_lasso = data.test.z$LogS



lambdas <- 10^seq(2, -3, by = -.1)


set.seed(1234)
# Setting alpha = 1 implements lasso regression
lasso_reg <- cv.glmnet(x_train_lasso, y_train_lasso, alpha = 1, lambda = lambdas, nfolds = 10)

# Best 
lambda_best <- lasso_reg$lambda.min 
lambda_best

set.seed(1234)
# Cross validation to find the optimal lambda penalization
cv.lasso <- cv.glmnet(x_train_lasso, y_train_lasso, alpha=1)

min(cv.lasso$cvm)
#plot(lasso_reg, xvar = "lambda", label=T)
min(lasso_reg$cvm)
lasso_reg$cvm[lasso_reg$lambda==lasso_reg$lambda.1se]
plot(lasso_reg)
lbs_fun(lasso_reg, offset_x = -2)
abline(v=log(lasso_reg$lambda.min), col = "red", lty=2)
abline(v=log(lasso_reg$lambda.1se), col="blue", lty=2)

plot(lasso_model)
log(1.43)
set.seed(1234)
lasso_model <- glmnet(x_train_lasso, y_train_lasso, alpha = 1, lambda = lambda_best)

predictions_train <- predict(lasso_model, s = lambda_best, newx = x_train_lasso)
eval_results(y_train_lasso, predictions_train, data.train.z)

predictions_test <- predict(lasso_model, s = lambda_best, newx = x_test_lasso)
eval_results(y_test_lasso, predictions_test, data.test.z)

summary(lasso_model$df)
coef(lasso_model)
#use fitted best model to make predictions
Lasso_prediction <- predict(lasso_model, s = lambda_best, newx = x_test_lasso)

lasso_Logrel= sum((predictions_test-y_test_lasso)>-0.7 & (predictions_test-y_test_lasso)<0.7)/268
# plot these against observed
data.frame(Predicted = predictions_test, Observed = y_test_lasso) %>%
  ggplot(aes(x = Observed, y = s1))+ geom_point(size = 1, alpha = 0.5)+
  geom_smooth(method = "loess", col = "red")+
  geom_smooth(method = "lm")


varImp(lasso_model, scale = FALSE, lambda=lambda_best)
