library(s20x)
# linear model
lsd <- read.csv('data/lsd.csv');lsd
plot(score~lsd, data = lsd,col='red',pch=16,ylim=c(0,100))
text(lsd$lsd, lsd$score-1.5,lsd$lsd)
fit <- lm(score~lsd, data = lsd)
fit.null <- lm(score~1,data=lsd)
abline(coef(fit))
plot(fit, 1)
resid(fit);lsd$score-fit$fitted.values
predict(fit,data.frame(lsd=lsd$lsd),interval='conf')
predict(fit,data.frame(lsd=lsd$lsd),interval='pred')
summary(fit);2*pt(abs(coef(fit)[2]/1.503),fit$df.residual,lower=F)
summary(fit.null)
sum((lsd$score-fit.null$coefficients)^2)
1-sum(fit$residuals^2)/sum(fit.null$residuals^2)
anova(fit)[1,4] # f-value

# glm:poisson
chicken <- read.csv('data/chickens.csv');chicken
plot(mo~weight,data = chicken,type='n')
points(mo~weight,data = chicken[chicken$dose=='High',],pch='+',col='red',cex=1.5)
points(mo~weight,data = chicken[chicken$dose=='Low',],pch=2,col='blue',cex=1.2)
points(mo~weight,data = chicken[chicken$dose=='Control',],pch=1,col='black',cex=1.6)
abline(h=0,col='grey')

fit2 <- glm(mo~weight,family = 'gaussian',data = chicken)
plot(fit2,1)
plot(mo~weight,data = chicken,ylim=c(-100,150))
abline(h=0,col='grey')
abline(coef(fit2));coef(fit2) # unlikely to be negative

fit3 <- glm(mo~weight,family = 'poisson',data = chicken)
plot(fit3,1)
summary(fit3)
1 - pchisq(fit3$deviance,fit3$df.residual)
plot(dpois(0:6,1.5),type = 'h')
barplot(dpois(0:6,1.5),names=0:6) # same as handout
plot(mo~weight,data = chicken)
lines(chicken$weight,fit3$fitted.values) # only from 1st obs to last obs
pred <- exp(predict.glm(fit3, newdata = data.frame(weight=350:560)))
lines(350:560,pred,lty=2)
round(fitted(fit3),2);round(predict(fit3,type = 'resp'),2);exp(predict(fit3,type='link'))
deviance(fit3);sum(resid(fit3)^2)
logLik(fit3);sum(log(dpois(chicken$mo,fitted(fit3))))

# glm: logistic
chd <- read.csv('data/chd-grouped.csv');chd
fit4 <- lm(p ~ age, data = chd)
plot(fit4,1)
plot(p ~ age, data = chd,ylim=c(-.1,1.1),xlim=c(20,75))
abline(fit4$coefficients)
abline(h=c(0,1),col='grey')

barplot(dbinom(0:2,2,.07),names=0:2)
barplot(dbinom(0:3,3,.3),names=0:3,ylim = c(0,1))
box()

fit5 <- glm(p~age, data = chd, weights = n, family = 'binomial')
plot(fit5, 1) # or glm(cbind(y,n-y)~age,data=chd,family='binomial')
summary(fit5)
1-pchisq(fit5$deviance,fit5$df.residual)
plot(p ~ age, data = chd,ylim=c(-.1,1.1),xlim=c(20,75))
abline(h=c(0,1),col='grey')
lines(chd$age,fit5$fitted.values,lwd=3)
preds.logit <- exp(predict.glm(fit5,newdata = data.frame(age=15:80)))
pred = preds.logit / (1+preds.logit)
lines(15:80, pred,col='red')
fitted(fit5);predict(fit5,type = 'resp')


x = exp(c(0.82, 0.68, 0.34, 0.35, 0.24, 0.07))
x / (1+x)

exp(c(0.45,0.3))/(1+exp(c(0.45,0.3)))

# extra
plot(1:43, dpois(1:43, 4), type = 'h')
plot(dnorm(1:10,5,1),type = 'o',col='red',lty=13,pch=17)
plot(dnorm(1:10,5,2),type = 'o',col='red',pch=16)
plot(dnorm(1:10,5,1),type = 'h',col='red',lty=13,pch=17)
lines(1:10,dnorm(1:10,5,1))
plot(dnorm(1:10,5,2),type = 'h',col='red',pch=16)


library(greekLetters)
greeks('alpha')
library(utf8)
utf8_print(greek$alpha)


