rm(list = ls())
library(s20x)
library(statmod)
library(MASS)
library(VGAM)
library(lattice)

lsd <- read.csv('data/lsd.csv');lsd
chicken <- read.csv('data/chickens.csv');chicken
chd <- read.csv('data/chd-grouped.csv');chd


glm = glm(mo~weight,data = chicken,family=poisson)
summary(glm)
sum(resid(glm,type='deviance')^2)


plot(predict(quasi),resid(quasi,type = 'pearson'))
plot(predict(glm),qres.default(glm)) # fixed for poisson

glm = glm(p~age, data = chd, weights = n, family = 'binomial')
plot(predict(glm),qres.default(glm)) # note that qr is randomised for binomial


# quasi-poisson and negative binomial
quasi = glm(mo~weight+dose,data = chicken,family=quasipoisson)
summary(quasi)
plot(quasi,1) # if the model is not correct, raw and pearson residual looks roughly same
plot(predict(quasi),resid(quasi,type = 'pearson'))
abline(h=0,lty=2)

# two parameter for neg-bin u and theta with property:  mean = u  and  var = u + u^2/theta
nb = glm.nb(mo~weight+dose,data = chicken) 
summary(nb)
1 - pchisq(nb$deviance,nb$df.residual)
pchisq(nb$deviance,nb$df.residual,lower.tail = F) # same as previous

resid(nb,type = 'response')
resid(nb,type =  'pearson')
resid(nb,type = 'deviance') # default

x = chicken$mo - exp(predict(nb))
y = sqrt(exp(predict(nb))+exp(predict(nb))^2/nb$theta)
x/y;resid(nb,type =  'pearson')
round(x/y,6)==round(resid(nb,type =  'pearson'),6)



# quasi-binomial and beta-binomial
rm(quasi)
quasi_bin = glm(p~age, data = chd, weights = n, family = quasibinomial)
summary(quasi_bin);summary(quasi_bin)$dispersion

# beta-binomial(n, p, rho)  0(no overdispersion) < rho <= 1, var = np(1-p)(1+n*rho-rho)
barplot(dbetabinom(0:10,10,.4,.9),names=0:10)
barplot(dbetabinom(0:10,10,.4,.5),names=0:10)
barplot(dbetabinom(0:10,10,.4,.1),names=0:10)
barplot(dbetabinom(0:10,10,.4,.01),names=0:10)
# when rho = 0, it's a binomial
barplot(dbetabinom(0:10,10,.4,0),names=0:10)
barplot(dbinom(0:10,10,.4),names=0:10)

summary(glm(cbind(R,N-R)~grp+hb, data = lirat, family = binomial))

df = lirat;df
df$grps = with(df,LETTERS[grp]);df

beta_bin=vglm(cbind(R,N-R)~grps+hb, data = df, family = 'betabinomial')
summary(beta_bin)

fit <- vglm(Species~., family=multinomial, data=iris)
summary(fit)




