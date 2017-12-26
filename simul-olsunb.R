iterate <- 10000
n1 <- 13
n2 <- 25
n3 <- 12
n <- n1+n2+n3

set.seed(11)

educ < c(rep(12,n1),rep(14,n2),rep(16,n3))
stdevs <- c(rep(0.8,n1),rep(1.0,n2),rep(1.4,n3))

b1hats <- rep(NA,iterate)
for (j in 1:iterate) {
  u <- stdevs*rnorm(n)
  lnwage <- 8.3 + 0.08*educ + u
  ols <- lm(lnwage~educ)
  b1hats[j] <- ols$coef["educ"]
}

print(mean(b1hats))