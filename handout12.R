paste(rep('#',103),collapse = '')

chd <- read.csv('data/chd-grouped.csv');chd
fit <- glm(p~age, data = chd, weights = n, family = 'binomial')
unname(predict(fit,type = 'response'));fitted(fit)
x=pchisq(deviance(fit),df.residual(fit),lower.tail = T);x

# x=[0,1] returns the smallest value of 𝑞 such that Pr(𝑋≤𝑞)≥𝑥.
qchisq(0.95, df=df.residual(fit)) # 56.94239, when pchisq(56.94239,41)=0.95
pchisq(qchisq(0.95, df=df.residual(fit)), df=df.residual(fit)) # return 0.95
# when deviance <= 56.94239 with df of 41, we could reject null hypothesis


######################################################################################################
######################################################################################################
# next eg we are trying to find if the goodness of fit(deviance) is significant
qchisq(.95,68) # we got 70 obs, if deviance higher than this value, may accept the null hypothesis

# Simulation for generalized linear models poisson
x = 1:10; b0 = 0; b1 = 0.1
means <- exp(b0 + b1*x)
n.sims <- 100
est.b0 <- est.b1 <- numeric(n.sims)
ci.b0 <- ci.b1 <- matrix(0, nrow = n.sims, ncol = 2)
for (i in 1:n.sims) {
  y = rpois(10, means)
  fit <- glm(y ~ x, family = poisson)
  est.b0[i] <- coef(fit)[1]
  est.b1[i] <- coef(fit)[2]
  ci.b0[i, ] <- confint(fit)[1, ] # if we use confint in here, it appears 'Waiting for profiling to be done'
  ci.b1[i, ] <- confint.default(fit)[2, ] # to address this, use confint.default instead
}
est.b0; est.b1
ci.b0; ci.b1


######################################################################################################
# goodness of fit when deviance is ok
set.seed(2)
x = rep(c(1.4, 1.6, 1, 2.0, 2.2, 2.4, 2.6), each = 10)
n = length(x)
y = rpois(n, lambda=exp(1+2*x))
xy.df = data.frame(x=x, y=y)
mod = par(mfrow=c(1,2))
plot(y~x, col="lightgray", data=xy.df)
plot(y~jitter(x), col="lightgray", data=xy.df)
par(mod)

plot(y~jitter(x), col="lightgray", data=xy.df)
xs <- seq(min(x), max(x), length=1e3)
lines(xs, exp(1+2*xs), lty=2, col="blue")
s0 <- glm(y~x, family=poisson, data=xy.df)
summary(s0);fitted.values(s0)
pchisq(deviance(s0), df.residual(s0),lower.tail = F)

Nsim <- 1e3
devs <- numeric(Nsim) 
fitted = matrix(0,nr=Nsim,nc=70) 
for (i in 1:Nsim) {
  ysim = rpois(n, lambda=exp(1+2*x))
  mod_i = glm(ysim~x, family=poisson)
  devs[i] = deviance(mod_i)
  fitted[i,] = fitted(mod_i)
}
head(devs)
hist(devs, freq = FALSE)
lines(density(devs), col="orange")
ds <- seq(min(devs), max(devs), length=1e4)
lines(ds, dchisq(ds, df=df.residual(s0)), col="blue")
mean(devs > qchisq(0.95, df=df.residual(s0)))
table(fitted>5) # all greater than 5(or most of them)

######################################################################################################
# goodness of fit when deviance is large
set.seed(2)
x = rep(seq(-.6, .6, length=7), each = 10)
n = length(x)
y = rpois(n, lambda=exp(1+2*x))
xy.df = data.frame(x=x, y=y)
mod = par(mfrow=c(1,2))
plot(y~x, col="lightgray", data=xy.df)
plot(y~jitter(x), col="lightgray", data=xy.df)
par(mod)

plot(y~jitter(x), col="lightgray", data=xy.df)
xs <- seq(min(x), max(x), length=1e3)
lines(xs, exp(1+2*xs), lty=2, col="blue")
s0 <- glm(y~x, family=poisson, data=xy.df)
summary(s0);fitted(s0)
pchisq(deviance(s0), df.residual(s0),lower.tail = F)

Nsim <- 500
devs <- numeric(Nsim) 
fitted = matrix(0,nr=Nsim,nc=70) 
for (i in 1:Nsim) {
  ysim = rpois(n, lambda=exp(1+2*x))
  mod_i = glm(ysim~x, family=poisson)
  devs[i] = deviance(mod_i)
  fitted[i,] = fitted(mod_i)
}
head(devs)
hist(devs, freq = FALSE)
lines(density(devs), col="orange")
ds <- seq(min(devs), max(devs), length=1e4)
lines(ds, dchisq(ds, df=df.residual(s0)), col="blue") # clearly diff
mean(devs > qchisq(0.95, df=df.residual(s0)))
table(fitted>5) # most of fitted value less than 5, so, the disparity, In this case 
# the deviance aren’t well approximated by df=68 distribution


######################################################################################################
######################################################################################################
# Simulation for generalized linear models logistic
x = 1:10; b0 = -3; b1 = 1;n = rep(100, length(x))
probs <- plogis(b0 + b1*x)
est.b0 <- est.b1 <- numeric(1000)
ci.b0 <- ci.b1 <- matrix(0, nrow = 1000, ncol = 2)
for (i in 1:1000) {
  y = rbinom(length(x),n,probs)
  fit <- glm(cbind(y, n - y) ~ x, family = binomial)
  est.b0[i] <- coef(fit)[1]
  est.b1[i] <- coef(fit)[2]
  ci.b0[i, ] <- confint.default(fit)[1, ]
  ci.b1[i, ] <- confint.default(fit)[2, ]
}
head(est.b0); head(est.b1)
head(ci.b0); head(ci.b1)
mean(devs >= qchisq(0.95, df=df.residual(s0))) #  there is around 5% chance that we randomly obtain 
# a deviance that is too large and hence falsely reject our null hypothesis


sd(c(19.60,141.28,15.76,52.08,26.01,117.40,18.50,73.18,87.66,82.69))
var(c(19.60,141.28,15.76,52.08,26.01,117.40,18.50,73.18,87.66,82.69))
mean(c(19.60,141.28,15.76,52.08,26.01,117.40,18.50,73.18,87.66,82.69))/14.05
se(c(19.60,141.28,15.76,52.08,26.01,117.40,18.50,73.18,87.66,82.69))

mean(c(19.60,141.28,15.76,52.08,26.01,117.40,18.50,73.18,87.66,82.69)/sqrt(5))
sd(c(19.60,141.28,15.76,52.08,26.01,117.40,18.50,73.18,87.66,82.69)/3)


mod = par(mfrow=c(3,3))
par('mfrow')
apply(dat, 2, qqnorm)
par(mod)
par('mfrow')
