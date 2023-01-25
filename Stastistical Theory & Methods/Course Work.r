setwd("E:/Leeds University/Stastistical Theory & Methods/Course work")
createsample(85) 
load(file = "killersandmotives.Rdata")

names(mysample)
which(mysample$AgeFirstKill == '99999')
mysample$KillerID
mysample[mysample$AgeFirstKill=='99999',c('KillerID','AgeFirstKill')]


#galaxies <- galaxies[!is.na(galaxies$Distance) , ]

#mysample <- mysample[mysample$AgeFirstKill!='99999',] --- Data Cleaning

#mysample[mysample$AgeFirstKill=='99999',c("AgeFirstKill",'KillerID')]

#mysample <- mysample[!is.na(mysample$Motive),] -- Data Cleaning


mysample[mysample$Yearborn + mysample$AgeFirstKill < 2000,]
mysample[1:10,c("AgeFirstKill","AgeLastKill")]
x <-mysample
#mysample <- mysample[rowSums(mysample[ , c("AgeFirstKill","YearBorn")], na.rm=TRUE)>=1900,] -- Data cleaning (above 1900)

time_length(difftime(as.Date("2003-04-05"), as.Date("2001-01-01")), "years")


start = as.Date("1948-12-07")
end = as.Date("2018-12-07")

rowSums(mysample[ , c("AgeLastKill","YearBorn")]) - rowSums(mysample[ , c("AgeFirstKill","YearBorn")])

#mysample["CareerDuration"]<- mysample$AgeLastKill - mysample$AgeFirstKill
#lubridate::time_length(difftime(end,start), "years") -- experiment

var(x$AgeLastKill)

mean(x$AgeFirstKill)
sd(x$AgeFirstKill)
max(x$AgeFirstKill)
min(x$AgeFirstKill)
par(mfrow = c(1, 1))
boxplot(x$AgeFirstKill)
plot(x$AgeFirstKill,x$AgeLastKill)
cor(x$AgeFirstKill,x$AgeLastKill)
install.packages("moments")
library(moments)
skewness(x$AgeFirstKill)
skewness(x$AgeLastKill)


mean(x$AgeLastKill)
sd(x$AgeLastKill)
max(x$AgeLastKill)
min(x$AgeLastKill)
quantile(x$AgeLastKill,type=1)
boxplot(x$AgeLastKill)

mean(x$CareerDuration)
sd(x$CareerDuration)
max(x$CareerDuration)
min(x$CareerDuration)
quantile(x$CareerDuration,type=1)
boxplot(x$CareerDuration)
skewness(x$CareerDuration)

plot(x$AgeFirstKill,x$CareerDuration)
plot(x$AgeLastKill,x$CareerDuration)
plot(x$AgeFirstKill,x$AgeLastKill)

hist(x$AgeFirstKill)

pnorm(22, mean = 29.64, sd = 9.14)#20% chance that killer starts before the age 22

?pnorm()

hist(x$CareerDuration, freq = TRUE)

x[1:5,"Motive"]

hist(x$AgeLastKill)

mean(x$CareerDuration)

x1 <-sample(x$CareerDuration,250)
#x1 <- x$CareerDuration
length(x$CareerDuration)
x1bar <- mean(x1)
loglik <- function(lambda){
  
  L <- (lambda^250)*exp(-lambda*250*x1bar)
  return(log(L))
  
}

lambda <- (1:4000)/5500
plot(lambda, loglik(lambda), type = "l",
     xlab = "lambda", ylab = "log likelihood")

x2 <-sample(x$AgeFirstKill,250)

unique(x$Motive)

m1 <- x[x$Motive=='Unknown',c('AgeFirstKill')]
m2 <- x[x$Motive=='Robbery or financial gain',c('AgeFirstKill')]
m3 <- x[x$Motive=="Convenience (didn't want children/spouse)",c('AgeFirstKill')]

par(mfrow = c(1, 1))
mean(m1)
sd(m1)
mean(m2)
sd(m2)
mean(m3)
sd(m3)

boxplot(m1)
boxplot(m2)
boxplot(m3)
hist(m1)
hist(m2)
hist(m3)
fn <- ecdf(m3)
mu <- mean(m1)
sigma <- sd(m1)

G <- function(x){
  
  return(pnorm(x, mean = mu, sd = sigma))
  
}

plot(fn, verticals = TRUE, pch = NA)
z <- 1:500

lines(z, G(z), col = "red")

hist(m2)
hist(m3,breaks = seq(from = 10, to = 65 , by = 7))

#ks tesing of m3:
ks.test(x =  m1, y = "pnorm", mean = mu, sd = sigma)

n <- 77

sigma <- sqrt(97.86)
sd(m1)
var(m1)
# Simulate a sample of size n:
#x <- runif(n = n, 1.96, 4.04)                

# Calculate the estimate of mu:
xbar <- mean(m1)                              

# Calculate the confidence interval:
CI =  xbar + c(-1, 1)*1.96*sqrt(sigma^2/n)   

# View the confidence interval:
CI     







length(m3)

n <- 510

sigma <- sd(m2)
sd(m2)
var(m2)
# Simulate a sample of size n:
#x <- runif(n = n, 1.96, 4.04)                

# Calculate the estimate of mu:
xbar <- mean(m2)                              

# Calculate the confidence interval:
CI =  xbar + c(-1, 1)*1.96*sqrt(sigma^2/n)   

# View the confidence interval:
CI  

v <- seq(from = 0, to = 40, by = 4)

hist(x$CareerDuration ,
     xlab = "Career Duration",
     ylab = "Density",
     main = "Distribution of Career Duration",
     breaks = v,
     freq = FALSE)

alk <- x$AgeLastKill
alkm <- mean(x$AgeLastKill)
alksd <- sd(x$AgeLastKill)
alkdnorm <- dnorm(sort(alk), mean= alkm , sd = alksd)

lines( sort(alk), alkdnorm , type = "l" , col = "blue" )
##career duration
v_cd <- seq(from = 0, to = 50, by = 5)



cd_data <-ms1[ms1$careerDuration >=0,]



hist(cd_data$careerDuration,
     xlab = "Career Duration",
     ylab = "Density",
     main = "Serial Killer:Career Duration distribution",
     breaks = v_cd,
     freq = FALSE,
     ylim = c( 0 , 0.15),
     right = FALSE)



cd <- x$CareerDuration
cdm <- mean(x$CareerDuration)
cd_l = 1/cdm
cdsd <- sd(x$CareerDuration)
cddnorm <- dexp(sort(cd) , rate = cd_l)
points( sort(cd), cddnorm , col = "blue" , type = "l" )



xbar <-cdm

plot(x$AgeFirstKill,x$AgeLastKill,
     pch = 16, cex = 1, 
     col = "red", 
     main = "Relationship between Age at first kill \n and Age at Last Kill",
     xlab = "Age at first kill", 
     ylab = "Age at Last Kill")

cor(x$AgeFirstKill, x$AgeLastKill)


#fitting age at first kill
v <- seq(from = 0, to = 80, by = 4)

hist(x$AgeFirstKill ,
     xlab = "Age at First Kill",
     ylab = "Density",
     main = "Fitting Distribution of Age at First Kill ",
     breaks = v,
     freq = FALSE)


afk <- x$AgeFirstKill
afkm <- mean(x$AgeFirstKill)
afksd <- sd(x$AgeFirstKill)
afkdnorm <- dnorm(sort(afk), mean= afkm , sd = afksd)



lines( sort(afk), afkdnorm , type = "l" , col = "red" )



#fitting age at last kill

v <- seq(from = 0, to = 80, by = 4)

hist(x$AgeLastKill ,
     xlab = "Age at Last Kill",
     ylab = "Density",
     main = "Fitting Distribution of Age at Last Kill ",
     breaks = v,
     freq = FALSE)


afk <- x$AgeLastKill
afkm <- mean(x$AgeLastKill)
afksd <- sd(x$AgeLastKill)
afkdnorm <- dnorm(sort(afk), mean= afkm , sd = afksd)



lines( sort(afk), afkdnorm , type = "l" , col = "blue" )

#fitting career duration


v <- seq(from = 0, to = 40, by = 4)

hist(x$CareerDuration ,
     xlab = "Career Duration",
     ylab = "Density",
     main = "Fitting Distribution of Career Duration",
     breaks = v,
     freq = FALSE)
cd <- cd_data$careerDuration
cdm <- mean(cd_data$careerDuration)
cd_l = 1/cdm
cdsd <- sd(cd_data$careerDuration)
cddnorm <- dexp(sort(cd) , rate = cd_l)
points( sort(cd), cddnorm , col = "Red" , type = "l" )

#maximum liklihood








#checking normal distribution for different motives

qqnorm(m1,pch = 16, cex = 1,col="pink",
       main = "Normal Q-Q Plot for Motive: Unknown")
abline(mean(m1),sd(m1),col="blue")


qqnorm(m2,pch = 16, cex = 1,col="pink",
       main = "Normal Q-Q Plot for Motive: Robbery or Financial gain")
abline(mean(m2),sd(m2),col="blue")


qqnorm(m3,pch = 16, cex = 1,col="pink",
       main = "Normal Q-Q Plot for Motive: Convenience \n (didn’t want children/spouse)")
abline(mean(m3),sd(m3),col="blue")



z.test(m1,alternative = "two.sided",mu = 27,sd(m1),conf.level = 0.95)
z.test(m2,alternative = "two.sided",mu = 27,sd(m2),conf.level = 0.95)

t.test(m3, alternative = "two.sided", mu = 27, conf.level = 0.95)

t.test(x = m1, y = m3, mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)



boxplot(m1)







z <- c(5445087,2870325,17424978,1319133,1921800,2070050,4105493,2969200,2795674)
boxplot(z)


m1 <- c(0.45,0.50,0.52,0.45,0.46,0.55,0.60,0.49,0.35,0.55,0.52,0.53,0.57,0.53,0.59,0.64,0.50,0.57,0.64)
m2 <- c(0.42,0.50,0.52,0.45,0.43,0.55,0.45,0.34,0.45,0.54,0.42,0.51,0.49,0.54,0.50,0.58,0.49,0.56,0.63)
t.test(x = m1, y = m2, mu = 0, paired = TRUE, var.equal = FALSE, conf.level = 0.95)



































  
  
  
  
  
  
  
  
  
  
  




















































































