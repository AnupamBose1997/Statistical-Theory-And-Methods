theta = seq(0, pi/2, length.out=100)
x = (1/9.8)*sin(2*theta)

plot(theta, x, xlab="Angle / radians", ylab="Distance / m")
idx = which.max(x)
print(theta[idx])

#Gradient-Ascent/Hill-Climbing Algorithm


#define an objective function to MINIMISE (notice the minus sign)
obj_fn <- function(param){-(1/9.8)*sin(2*param)}
#Start randomly between 0 and pi/2
start_guess = (pi/2)*runif(1)
#Run the optimiser and store the result
opt_result = optim(par=start_guess, fn=obj_fn)
#Print the optimised parameter value
print(paste("Best parameter value is: ", opt_result$par, collapse=""))



y = rnorm(100, m = 4, sd = 2)
hist(y)



lognorm <- function(param, Y){100*log(sqrt(2*pi)*param[2]) +sum((Y-param[1])^2/(2*param[2]^2))}
neg_LL <- function(param) -lognorm(param, Y=y)
start_guess = c(0, 1) #first guess of mean of 0, stand.dev. of 1
mle_param = optim(par=start_guess, fn=neg_LL)
print(paste("MLE mean = ", mle_param$par[1], collapse=""))

print(paste("MLE variance = ", mle_param$par[2]^2, collapse=""))


#Linear Regression
#Define the number of data points
N = 100
X = runif(n=N, min=0, max=10)
Y = 2*X + rnorm(n=N, mean=0, sd= 3)
plot(X, Y)
lines(X, 1*X, col="blue")
lines(X, 2*X, col="red")
lines(X, 3*X, col="green")

set.seed(55)
sunlight_hours = rnorm(20, 12, 3)
temperature = 10 + sunlight_hours + rnorm(20, 0, 1)
sales = 10 + temperature + rnorm(20, 0, 5)
icecream_data = data.frame(sunlight_hours, temperature, sales)

icecream_model = glm(sales ~ sunlight_hours + temperature, data=icecream_data)
summary(icecream_model)

plot(sunlight_hours,temperature)


