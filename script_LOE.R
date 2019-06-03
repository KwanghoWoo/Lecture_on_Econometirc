#Lectures on Econometric

datadir <- "http://econ.korea.ac.kr/~chirokhan/book/data"
ekc <- read.csv(file.path(datadir, "co2gdp2005.csv"))
names(ekc)
nrow(ekc)
head(ekc)

summary(ekc)

plot(co2pc ~ gdppcppp, data = ekc)

#p.41 우리나라 군별 공무원수와 재정자립도

serv <- read.csv(file.path(datadir, "serv.csv"))
nrow(serv)
summary(serv)
plot(finind ~ servpc, data = serv, subset = servpc < 28)

if (!requireNamespace("quantreg"))
  install.packages("quantreg")
library(quantreg)
serv1 <- serv[serv$servpc < 28, ]
plot(finind ~ servpc, data = serv1, pch = 19)
abline(lm(finind ~ servpc, data = serv1)$coef, lty = 1)
abline(rq(finind ~ servpc, data = serv1)$coef, lty = 2)


# IV

datadir <- "http://econ.korea.ac.kr/~chirokhan/book/data"
ivdata <- read.csv(file.path(datadir, "ivdata.csv"))
names(ivdata)
nrow(ivdata)

if (!requireNamespace("lmtest"))
  install.packages("lmtest")
library(lmtest)
coeftest(lm(y ~ x1 + x2, data = ivdata))

# First-stage regression
stage1 <- lm(x2 ~ x1 + z2a, data = ivdata)
coeftest(stage1)
ivdata$x2hat <- fitted(stage1)
stage2 <- lm(y ~ x1 + x2hat, data = ivdata)
coeftest(stage2)

# include z2a and z2b in first stage
stage1_1 <- lm(x2 ~ x1 + z2a + z2b, data = ivdata)
coeftest(stage1_1)
ivdata$x2_1hat <- fitted(stage1_1)
stage2_1 <- lm(y ~ x1 + x2_1hat, data = ivdata)
coeftest(stage2_1)

# simulation(p. 101)
n1 <- 13
n2 <- 25
n3 <- 16
n <- n1 + n2 + n3
educ <- c(rep(12, n1), rep(25, n2), rep(16, n3))
stdevs <- c(rep(0.8, n1), rep(1.0, n2), rep(1.4, n3))
u <- stdevs * rnorm(n)
lnwage <- 8.3 + 0.08 * educ + u

cbind(educ, lnwage)
plot(lnwage ~ educ)

## extrapolation ##
if (!requireNamespace("Ecdat"))
  install.packages("Ecdat")
library(Ecdat)

data(Housing, package = "Ecdat")
H3 <- Housing[Housing$bedrooms == 3, ]
ols2 <- lm(log(price) ~ log(lotsize) + I(log(lotsize) ^ 2), data = H3)
summary(ols2)

# check the variable range 
-3.29052 / (0.16510 * 2)
summary(log(H3$lotsize))

ols1 <- lm(log(price) ~ log(lotsize), data = H3)
summary(ols1)

plot(log(price) ~ log(lotsize), data = H3)
x <- sort(log(H3$lotsize))
b1 <- ols1$coefficients
b2 <- ols2$coefficients
lines(x, b2[1] + b2[2] * x + b2[3] * x ^ 2)
lines(x, b1[1] + b1[2] * x, lty = 2)
legend("topleft", c("quadratic", "linear"), lty = c(1, 2))

### please~~~~~!!!!!###
