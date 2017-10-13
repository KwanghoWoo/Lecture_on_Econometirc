#Lectures on Econometric

datadir <- "http://econ.korea.ac.kr/~chirokhan/book/data"
ekc <- read.csv(file.path(datadir, "co2gdp2005.csv"))
names(ekc)
nrow(ekc)
head(ekc)

summary(ekc)

plot(co2pc~gdppcppp, data=ekc)

#p.41 우리나라 군별 공무원수와 재정자립도

serv <- read.csv(file.path(datadir, "serv.csv"))
nrow(serv)
summary(serv)
plot(finind~servpc, data=serv, subset = servpc<28)

if (!requireNamespace("quantreg")) install.packages("quantreg")
library(quantreg)
serv1 <- serv[serv$servpc<28,]
plot(finind~servpc, data=serv1, pch=19)
abline(lm(finind~servpc, data=serv1)$coef, lty=1)
abline(rq(finind~servpc, data=serv1)$coef, lty=2)


# IV 

datadir <- "http://econ.korea.ac.kr/~chirokhan/book/data"
ivdata <- read.csv(file.path(datadir, "ivdata.csv"))
names(ivdata)
nrow(ivdata)

if (!requireNamespace("lmtest")) install.packages("lmtest")
library(lmtest)
coeftest(lm(y~x1+x2, data=ivdata))

# First-stage regression
stage1 <- lm(x2~x1+z2a, data=ivdata)
coeftest(stage1)
ivdata$x2hat <- fitted(stage1)
stage2 <- lm(y~x1+x2hat, data=ivdata)
coeftest(stage2)

# include z2a and z2b in first stage
stage1_1 <- lm(x2~x1+z2a+z2b, data=ivdata)
coeftest(stage1_1)
ivdata$x2_1hat <- fitted(stage1_1)
stage2_1 <- lm(y~x1+x2_1hat, data=ivdata)
coeftest(stage2_1)

