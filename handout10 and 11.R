# What is a confidence interval?
# The “confidence” we have in a confidence interval is from the way confidence intervals perform 
# under repetition of the experiment.
# If the experiment is repeated a large number of times, and a 95% confidence interval for β0 
# is calculated each time, then about 95% of those intervals will contain β0. That is, 
# we expect the coverage of those intervals to be about 95%

# Here, repeating the “experiment” means to repeat the process of obtaining data, fitting the model, 
# and calculating the quantities of interest (eg: parameter estimates, CIs, or some other statistics).


dbinom(5,10,0.5);factorial(10)*(0.5^5)^2/factorial(5)/factorial(5)
# simulation:
# Consider the following simple linear regression model:
# µi = β0 + β1xi
# Yi ∼ Normal(µi, σ2)
#  x1 = 2, x2 = 4, · · · , x10 = 20, β0 = 20, β1 = 3 and σ = 10(constant variance)
# then our y1 = 2*2+25, y2 = 4*2+25, so on and so forth 

x = seq(2,20,by=2);x
means = x*3+20;means
y = round(rnorm(10,means,10),1)
lm = lm(y~x) # we assume this dataset is ind and we know it's normal distributed(same variance)
summary(lm)
confint(lm)
mod = par(mfrow=c(1,2))
plot(y~x)
abline(20,3,col='red')              # true
mtext('true',3,col='red',cex=2)
plot(y~x)
abline(lm,col='red')                # estimated
mtext('estimated',3,col='red',cex=2)
par(mod)

plot(y~x)
abline(20,3,col='red')   
abline(lm)  
text(20,70,'true',col='red')
text(19,95,'estimated')


# If we wanted to simulate 10,000 data sets then we need to fit 10,000 models(loop)
n = 10000
est.b0 = numeric(n)
est.b1 = numeric(n)
conf.b0 = matrix(0, nr=n, nc=2)
conf.b1 = matrix(0, nr=n, nc=2)
for (i in 1:n){
  y = round(rnorm(10,means,10),1)
  lm = lm(y~x)
  est.b0[i] = coef(lm)[1]
  est.b1[i] = coef(lm)[2]
  conf.b0[i, ] = confint(lm)[1,]
  conf.b1[i, ] = confint(lm)[2,]
}
head(est.b0,10);head(est.b1,10)
head(conf.b0);head(conf.b1)
hist(est.b0,breaks = 20) # close to 20
hist(est.b1)             # close to 3
mean(est.b0);var(est.b0)
mean(est.b1);var(est.b1)

contains_20 = (conf.b0[,1]<=20)&(conf.b0[,2]>=20)
table(contains_20) # close to 95%

contains_3 = (conf.b1[,1]<=3)&(conf.b1[,2]>=3)
table(contains_3)  # close to 95%


# now we want to test a particular value, let say x=7, means = 7*3+20=41
mu=7*3+20;mu
est.mu = numeric(n)
prd.mu = matrix(0,nr=n,nc=2)
for (i in 1:n) {
  y = rnorm(10,mu,10)
  lm=lm(y~x)
  pred = predict(lm,data.frame(x=7),interval = 'conf')
  est.mu[i] = pred[1]
  prd.mu[i,]=pred[-1]
}
head(est.mu);head(prd.mu)
hist(est.mu);abline(v=mu,lwd=6,col='red')
contain_mu = (mu >= prd.mu[, 1]) & (mu <= prd.mu[, 2])
table(contain_mu) # approximately same 95%


# Now, let’s examine the performance of prediction intervals.
# For 95% prediction intervals, we need the intervals to contain the new value of Y 95% 
# of the time (on average) when both the experiment and the new value of Y are replicated 
# a large number of times.
new.y=numeric(n)
pred.mu = matrix(0, nr=n, nc=2)
for (i in 1:n) {
  new.y[i]=rnorm(1,mu,10)
  y = rnorm(10,mu,10)
  lm = lm(y~x)
  pred = predict(lm,data.frame(x=7),interval = 'pred')
  pred.mu[i, ]=pred[2:3]
}
head(new.y);head(pred.mu)
contain_pred_mu = (new.y >= pred.mu[, 1]) & (new.y <= pred.mu[, 2])
table(contain_pred_mu)
# Out of the 10,000 prediction intervals, around 9500 of them contained the new value of Y .
# So our method for calculating 95% prediction intervals works because approximately 95% of 
# the intervals calculated using this method contain the new value.





