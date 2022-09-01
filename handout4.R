rm(list = ls())
library(s20x)
lsd <- read.csv('data/lsd.csv');lsd
chicken <- read.csv('data/chickens.csv');chicken
chd <- read.csv('data/chd-grouped.csv');chd


# null
null = lm(score~1, data=lsd)
resids = sum(resid(null)^2);resids
plot(score~lsd,data = lsd)
abline(h=mean(lsd$score))
arrows(lsd$lsd,mean(lsd$score),lsd$lsd,lsd$score,col='red')
text(6.2,80,labels=paste('RSS: ', round(resids,2)),cex=1.25)

# linear 
l = lm(score~lsd,data = lsd)
resids = sum(resid(l)^2);resids
plot(score~lsd,data = lsd)
abline(l$coefficients)
arrows(lsd$lsd,l$fitted.values,lsd$lsd,lsd$score,col='red')
text(6.2,80,labels=paste('RSS: ', round(resids,2)),cex=1.25)

# polynomial
p2 = lm(score~lsd+I(lsd^2),data=lsd)
pred = predict(p2,data.frame(lsd=c(0,lsd$lsd,7)));pred
resids = sum(resid(p2)^2);resids
plot(score~lsd,data = lsd)
lines(c(0,lsd$lsd,7),pred)
arrows(lsd$lsd,pred[-c(1,9)],lsd$lsd,lsd$score,col='red')
text(6.2,80,labels=paste('RSS: ', round(resids,2)),cex=1.25)

p3 = lm(score~poly(lsd,3,raw = T),data = lsd)
pred = predict(p3,data.frame(lsd=c(0,lsd$lsd,7)));pred
resids = sum(resid(p3)^2);resids
plot(score~lsd,data = lsd)
lines(c(0,lsd$lsd,7),pred)
arrows(lsd$lsd,pred[-c(1,9)],lsd$lsd,lsd$score,col='red')
text(6.2,80,labels=paste('RSS: ', round(resids,2)),cex=1.25)

p6 = lm(score~poly(lsd,6,raw = T),data = lsd)
pred = predict(p6,data.frame(lsd=seq(0,7,by=0.01)));pred
resids = sum(resid(p6)^2);resids
plot(score~lsd,data = lsd)
lines(seq(0,7,by=0.01),pred)
text(6.2,80,labels='RSS: 0.00',cex=1.25)

# brief summary for linear:
# 1. the saturated model is the most complex u can fit, no further to go
# 2. the RSS is 0
# 3. the regression line go through every point
# 4. RSS(saturated) <=  RSS  <=  RSS(null)

# brief summary for glm:
# 1. the saturated model is the most complex u can fit, no further to go
# 2. the RSS is 0
# 3. the regression line go through every point
# 4. log-likelihood(saturated) >=  log-likelihood  >=  log-likelihood(null)


# deviance: is twice the diff between ML of saturated and ML of fitted model D = 2(ls-lf)
deviance(glm(mo~weight,family = 'poisson',data = chicken)) # D = 2(-43.421 - -310.667) = 554.492










