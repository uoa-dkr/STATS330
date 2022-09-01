# Bootstrapping
# recall that sd: it quantifies of how much the data are spread out.
# The se measures the accuracy with which a sample distribution represents a population by using sd
## resampling the known parameter('true' parameter) many times, the sd of these means(eg below) is its se


# Parametric bootstrapping is simply simulation based on the fitted model
# possibly assume that the estimated model is the 'true' relationship.
df = read.table('data/STATS20x.txt',header = T)
lm.fit = lm(Exam ~ Test, data = df)
summary(lm.fit)
betahat = coef(lm.fit);betahat
sigmahat <- summary(lm.fit)$sigma;sigmahat
n = nrow(df)
num=1e3
b0 <- betahat[1]; b1 <- betahat[2]; sigma <- sigmahat
x <- df$Test
means <- b0 + b1*x
est.b0 <- est.b1 <- numeric(n.sims)
for (i in 1:num) {
  y <- rnorm(n, means, sigma)
  fit <- lm(y ~ x)
  est.b1[i] <- coef(fit)[2]
  est.b0[i] <- coef(fit)[1]
}
sd(est.b0);sd(est.b1)   # sd of these means is se at our 'true' model
summary(lm.fit)$coef[,2]

quantile(est.b0, prob=c(0.025, 0.975));mean(est.b0)+c(-1,1)*1.96*sd(est.b0);confint(lm.fit)[1,]
c(2*b0-quantile(est.b0, prob=0.975), 2*b0-quantile(est.b0, prob=0.025))

quantile(est.b1, prob=c(0.025, 0.975));mean(est.b1)+c(-1,1)*1.96*sd(est.b1);confint(lm.fit)[2,]
c(2*b1-quantile(est.b1, prob=0.975), 2*b1-quantile(est.b1, prob=0.025))


# The non-parametric bootstrap simply generates new “data” by resampling with replacement from the actual data
# Don’t need to assume that the estimated model is the true relationship.
est.b0 <- est.b1 <- numeric(n.sims)
for (i in 1:num) {
  samp = sample(1:n,replace = T)
  Boot.df <- df[samp, ]
  fit <- lm(Exam ~ Test, data=Boot.df) 
  est.b0[i] <- coef(fit)[1] 
  est.b1[i] <- coef(fit)[2]
}
sd(est.b0);sd(est.b1)
summary(lm.fit)$coef[,2]

quantile(est.b0, prob=c(0.025, 0.975));mean(est.b0)+c(-1,1)*1.96*sd(est.b0);confint(lm.fit)[1,]
c(2*b0-quantile(est.b0, prob=0.975), 2*b0-quantile(est.b0, prob=0.025)) # more narrow

quantile(est.b1, prob=c(0.025, 0.975));mean(est.b1)+c(-1,1)*1.96*sd(est.b1);confint(lm.fit)[2,]
c(2*b1-quantile(est.b1, prob=0.975), 2*b1-quantile(est.b1, prob=0.025)) #The np CI is somewhat narrower

# Parametric vs non-parametric bootstrap:
# If the fitted model is good then the parametric and non-parametric bootstraps will give similar results.
# In this example, the difference between the parametric and non-parametric bootstraps is due to weaknesses 
# with the fitted linear model. Is the parametric bootstrap better, or the non-parametric bootstrap?

# The answer comes down to asking which bootstrap better emulates(imitate/simulate) the experiment.
# Generating normally distributed exam scores around the fitted line (parametric bootstrap).
# Sampling from a population consisting of infinite copies of the Stats20x data (non-parametric bootstrap).


