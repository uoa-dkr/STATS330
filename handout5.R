rm(list = ls())
library(s20x)
lsd <- read.csv('data/lsd.csv');lsd
chicken <- read.csv('data/chickens.csv');chicken
chd <- read.csv('data/chd-grouped.csv');chd

barplot(dpois(0:11,4.3),names=0:11)
points(.7,0,pch=16,cex=1.3)


# a model that fits poorly will have a large deviance, this may happen bc:
# 1. model is too simple and does not have the right explanatory term
# 2. the response variance has more variance than assumed under a poisson distribution

plot(dchisq(0:83,19),type='l')
abline(h=0,col='grey')
points(83,0,pch=16)
