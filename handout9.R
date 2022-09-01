library(mgcv)
library(tidyverse)
library(s20x)
library(readxl)
search()
detach("package:tibble");search()
detach("package:tidyverse");search()
library(AICcmodavg)
library(MuMIn)

wine = read.csv2('data/winequality-red.csv')
head(wine[c(1:2,12)], 15)
wine$quality = ifelse(wine$quality>=7,1,0)
x = names(wine);x
sapply(wine[x], typeof) # all are char except last one
df=as.data.frame(sapply(wine[1:nrow(wine),], as.numeric)) # convert to double or int

typeof(df$fixed.acidity)
wine.fit <- glm(quality ~ fixed.acidity + residual.sugar + chlorides + free.sulfur.dioxide +
                  density + pH + sulphates + alcohol, family = binomial, data = df)
summary(wine.fit)
pchisq(wine.fit$deviance,wine.fit$df.residual,lower.tail = F)

# It is sensible to consider adding nonlinear terms to our model
# but how do we know which terms are supposed to add
# gam for smooth
pairs20x(df[c(12,1,4:6,8:11)])
gam.fit <- gam(quality ~ s(fixed.acidity) + s(residual.sugar) + s(chlorides) + s(free.sulfur.dioxide) 
               + s(density) + s(pH) + s(sulphates) + s(alcohol), family = binomial, data = df)
summary(gam.fit)
par(mfrow = c(2,4))
plot(gam.fit)
plot(gam.fit,1)
par(mfrow=c(1,1))

#** For each plot, consider whether or not you could draw a straight line that remained within 
#** the dotted lines. If not, consider fitting a polynomial for that variable. The order of your 
#** polynomial is determined by how wiggly your line would need to be to stay within the dotted lines

# 1. We probably need a quadratic term for (alcohol) by volume.
# 2. We might need a quadratic term for concentration of potassium (sulphate).
# 3. We could consider quadratics for concentrations of sodium (chloride) and (free sulfur dioxide).

# recall from handout 4/5 increase the complexity will reduce the deviance, but could result in 
# overfitted and complex, and hard to explain
# in the scenario, fit1 is the submodel of fit, we can determine it via pchisq
# note that in here 
# H0: The submodel is appropriate. We don’t need the extra parameter(s) in the full model.
# H1: The submodel is not appropriate. We need at least one of the additional parameters in the full model.

wine.poly.fit <- glm(quality ~ fixed.acidity + residual.sugar + chlorides + I(chlorides^2) +
                    free.sulfur.dioxide + I(free.sulfur.dioxide^2) + density + pH + sulphates
                    + I(sulphates^2) + alcohol + I(alcohol^2), family = "binomial", data = df)
summary(wine.poly.fit)

wine.poly.fit1 <- glm(quality ~ fixed.acidity + residual.sugar + chlorides + I(chlorides^2) +
                       free.sulfur.dioxide + I(free.sulfur.dioxide^2) + density + pH + sulphates
                     + I(sulphates^2) + alcohol, family = "binomial", data = df)
summary(wine.poly.fit1)

a = deviance(wine.poly.fit);c = wine.poly.fit$df.residual
b = deviance(wine.poly.fit1);d = wine.poly.fit1$df.residual
pchisq(b-a,d-c,lower.tail = F) # diff of deviance and df, and we have evidence to against null hypo

# easier to find in anova test: This is known as a likelihood-ratio test.
anova(wine.poly.fit, wine.poly.fit1, test = 'Chisq')


# when two model are not sub of each other, we'd use information-theoretic approaches to compare models
# 1. AIC(Akaike Information Criterion): AIC = −2l(log-likelihood)+2k(parameter) The smaller the better
# we can think 1st term as measuring model fit, 2nd is the model complexity
AIC(wine.fit,wine.poly.fit1,wine.poly.fit) 

# 2. AICc(corrected AIC): AICc = -2l + (2k + 2k^2+2k/n-k-1) / if n(# of data/# of parameter < 40) 
# then AICc is preferred to be used
# since we have relatively large sample size, it does not matter which AIC or AICc method conduct
(2*c(9,12,13)^2+2*c(9,12,13))/(1599-c(9,12,13)-1);1599/c(9,12,13) < 40 
AIC(wine.fit,wine.poly.fit1,wine.poly.fit)[2]+(2*c(9,12,13)^2+2*c(9,12,13))/(1599-c(9,12,13)-1)
AICc(wine.fit);AICc(wine.poly.fit1);AICc(wine.poly.fit)
AICc(wine.fit,wine.poly.fit1,wine.poly.fit)

## conclusion(for both AIC and AICc):
# If the difference is less than 2, then both models are similarly supported by the data.

# If the difference is between 2 and 4, the one with the smaller AIC is slightly better supported.

# If the difference is between 4 and 10, the one with the smaller AIC is considerably better supported.

# If the difference is over 10, the one with the larger AIC has essentially no support.


# BIC(Bayesian Information Criterion): BIC = −2l + log(n)k (if n>8 we tend to use AIC(c))
BIC(wine.fit,wine.poly.fit1,wine.poly.fit)

# diff between AIC(c) and BIC is that AIC(c) acknowledges that we probably haven’t found the 
# ‘true model’, and we’re just trying to find the closest approximation. On the other hand, 
# BIC assumes that one of the candidates is the ‘true’ one, and attempts to determine which it is



# Now we know how to compare models. But how do we choose our candidate set of models?
options(na.action = "na.fail") # blank is na
all.fits = dredge(wine.fit) 
head(all.fits) # ranked by AICc

# to gain this best model use get.model
first_model = get.models(all.fits,1)[[1]]
summary(first_model)
sec_mode = get.models(all.fits,2)[[1]]
summary(sec_mode)
summary(wine.fit) # slightly diff of some p-value


# another way to do this is using back/forward step or both
best.backwards.model <- step(wine.fit, direction = "backward")
summary(best.backwards.model)

# kind of complicated for forward
summary(step(glm(quality~1,family = binomial,data=df), direction = "forward",scope = 
               ~ alcohol + chlorides+ density+fixed.acidity+free.sulfur.dioxide+
               pH +residual.sugar+sulphates))

summary(step(wine.fit, direction = "both"))


head(dredge(wine.poly.fit1))
summary(wine.poly.fit1)

