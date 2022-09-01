rm(list = ls())
library(s20x)
lsd <- read.csv('data/lsd.csv');lsd
cke <- read.csv('data/chickens.csv');cke
chd <- read.csv('data/chd-grouped.csv');chd


plot(score~lsd,data = lsd)
abline(89.1,-9)
abline(70.3,-3.2,col='red')
abline(32.4,7,col='blue')
legend('topright',legend = expression(paste(beta[0],'= 89.1 ; ',beta[1],'= -9.0')),cex = .85)

plot(score~lsd,data = lsd)
abline(89.1,-9)
arrows(lsd$lsd,fitted(lm(score~lsd,data = lsd)),lsd$lsd,lsd$score,col='red',cex=.5)


glm=glm(p~age, data = chd, weights = n, family = 'binomial')
summary(glm)

pred = predict.glm(glm,data.frame(age = chd$age))
exp(pred) / (1+exp(pred)) # fitted(glm)

# observed prob and log-likelihood for age 37
x=exp(predict.glm(glm,data.frame(age=37)));x
p=x/(1+x);p
db=dbinom(0:3,3,p);db
log(db[2])

odds = exp(predict.glm(glm,age=chd$age));odds
prob = odds/(1+odds);prob;fitted(glm)
preds = dbinom(chd$y,chd$n,prob);preds
log_likelihood = sum(log(preds));log_likelihood # same as below


# log link: Extract Log-Likelihood
logLik(glm)

glm = glm(mo~weight,family = 'poisson',data = cke)
mle = log(with(cke,dpois(mo,glm$fitted.values)));mle # MLE
sum(mle);logLik(glm) == sum(mle)
logLik(glm)


curve(exp(x),1,5)

















