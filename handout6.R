rm(list = ls())
library(s20x)
lsd <- read.csv('data/lsd.csv');lsd
chicken <- read.csv('data/chickens.csv');chicken
chd <- read.csv('data/chd-grouped.csv');chd


# raw residuals increase with mean increase
eovcheck(lm(score~lsd,data = lsd))
plot(predict(lm(score~lsd,data = lsd)),residuals(lm(score~lsd,data = lsd)))
abline(h=0,lty=2)

plot(predict(glm(mo~weight,data = chicken,family=poisson)),
     residuals(glm(mo~weight,data = chicken,family=poisson), type='response'))
abline(h=0,lty=2)

plot(predict(glm(p~age, data = chd, weights = n, family = 'binomial')),
     resid(glm(p~age, data = chd, weights = n, family = 'binomial'), type='response'))
abline(h=0,lty=2)

# pearson residuals:
# for linear is      ri = yi - E(x) / sd
# for poisson is     ri = yi - E(x) / sqrt(E(x))
# for logistic is    ri = yi - ni*pi / sqrt(ni*pi*(1-pi))
plot(predict(glm(mo~weight,data = chicken,family=poisson)),
     residuals(glm(mo~weight,data = chicken,family=poisson), type='pearson'))
abline(h=0,lty=2)

plot(predict(glm(p~age, data = chd, weights = n, family = 'binomial')),
     resid(glm(p~age, data = chd, weights = n, family = 'binomial'), type='pearson'))
abline(h=0,lty=2)

# deviance residuals have roughly same property as pearson resid
plot(predict(glm(mo~weight,data = chicken,family=poisson)),
     residuals(glm(mo~weight,data = chicken,family=poisson), type='deviance'))
abline(h=0,lty=2)

plot(predict(glm(p~age, data = chd, weights = n, family = 'binomial')),
     resid(glm(p~age, data = chd, weights = n, family = 'binomial'), type='deviance'))
abline(h=0,lty=2)


# randomised quantile residual: remove the problem of sparse data
library(statmod)
plot(predict(glm(mo~weight,data = chicken,family=poisson)),
     qresid(glm(mo~weight,data = chicken,family=poisson)))
abline(h=0,lty=2)

plot(predict(glm(p~age, data = chd, weights = n, family = binomial)),
     qresid(glm(p~age, data = chd, weights = n, family = binomial)))
abline(h=0,lty=2)





