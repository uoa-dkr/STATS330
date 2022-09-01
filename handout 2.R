library(s20x)

# interpretation for poisson
chicken <- read.csv('data/chickens.csv');chicken
glm=glm(mo~weight,family = 'poisson',data = chicken)
glm$fitted.values
fitted(glm)
summary(glm)  # log(mu) = B0 + x * weight 
coef(glm)
exp(coef(glm)[1]) # we estimate that the expected num of ... that weight 0g is 0.028(not sensible or trivial)
exp(glm$coefficients[2]) # we estimate that, for an additional gram increase in weight of chicken,
# the expected num of ... is multiplied by 1.013

# alternative way to interpret this(percentage-change, which is 1.013-1 * 100%)
100*(exp(glm$coefficients[2])-1) # everything same as above, but the diff is that, increases by 1.34%
# or 
100*(exp(10*glm$coefficients[2])-1) # for every 10g increase ...... increases by 14.23%.
100*(exp(100*glm$coefficients[2])-1) # for every 100g increase ...... increases by 278.19%.

# more importantly, interpret its CI
100*(exp(confint(glm)[2,])-1)     # increases by between 1.09 and 1.60% 
100*(exp(10*confint(glm)[2,])-1)  # increases by between 11.41 and 17.24%
100*(exp(100*confint(glm)[2,])-1) # increases by between 194.54 and 390.54%


# interpretation for binomial
(chd <- read.csv('data/chd-grouped.csv'))
glm <- glm(cbind(y, n - y) ~ age, data = chd, family = binomial)
summary(glm) # logit(p) = B0 + x*age  /  log(p/1-p) = B0 + x*age  / log(odds) = B0 + x*age
glm$fitted.values
fitted(glm)

exp(coef(glm)) # estimate the odds of newborn baby(age=0) is 0.005(recall that this intercept is trivial)
exp(coef(glm)[2]) # for an additional year increase in age, the odds of ... are multiplied by 1.12
exp(10*coef(glm)[2]) # every 10-year increase in age, the odds of ... are multiplied by 3 

# interpret by odds percentage:
100 * (exp(coef(glm)[2])-1) # every 1-year increase in age, the odds of ... are increase by 11.66%
100 * (exp(25*glm$coefficients[2])-1) # every 25-year increase in age, the odds increase by 1477%

# confint use [2, ]
exp(confint(glm)[2,]) # an additional year increase in age, the odds are multiplied between by 1.1 to 1.2 
exp(10*confint(glm)[2, ]) # 10-year increase, the odds are multiplied by between 1.9 to 5.0 
100*(exp(confint(glm)[2,])-1) # 1-year increase in age, the odds of ... increase 6.86 to 17.5%
100*(exp(15*confint(glm)[2,]-1)) # 15-year ...... odds increase 99.6 to 413.5%


# convert it to probability
odds = exp(coef(glm)[2])
conf.odds = exp(confint(glm)[2,]);conf.odds
p = odds/(1+odds);p
conf.p = conf.odds/(1+conf.odds);conf.p



# interaction of chicken data
chicken$dose = with(chicken,factor(dose,levels = c('Control','High','Low')))
boxplot(mo~dose,data = chicken)
chicken$dose = with(chicken,factor(dose,levels = c('Control','Low','High')))
boxplot(mo~dose,data = chicken)

no.inter = glm(mo~dose,family = 'poisson',data = chicken)
anova(no.inter,test = 'Chisq')
summary(no.inter)
# we estimate that the log of the expected num of ... a chicken in control group is equal to 3.93
# a chicken in high-dose group is 1.87 lower than for those in the control group 
# ...          low-dose ...       1.46 ...

exp(coef(no.inter))
# we estimate that the expected num of ... a chicken in control group is equal to 50.8
# ...          high-dose          0.153 times of those in the control group
# ...          low-dose           0.233 times of ...
100*(exp(coef(no.inter)[2:3])-1)
# ...          high-dose          is 84.7% lower than for those in the control group
# ...          low-dose           is 76.7% lower than for those in the control group

no.inter2 = glm(mo~dose+weight,data=chicken,family=poisson)
anova(no.inter2,test = 'Chisq')
summary(no.inter2)
exp(coef(no.inter2)[4]) # for 1g increase in chicken, the expected num of ... is multiplied by 1.013

inter = glm(mo~dose*weight,data=chicken,family=poisson)
anova(inter,test = 'Chisq')
summary(inter)
exp(inter$coefficients)

(ships <- read.table('data/boat-damage.txt'))
ships$year <- ships$year + 2000

fit.offsets <- glm(incidents~year+type, family = 'poisson', offset = log(service), data = ships)
anova(fit.offsets,test = 'Chisq')
summary(fit.offsets)

# subset() in the model
# exclude obs 1 and 4
glm(incidents~year+type, family = 'poisson', offset = log(service), data = ships,subset=-c(1,4))

# first 20 obs
glm(incidents~year+type, family = 'poisson', offset = log(service), data = ships,subset=1:20)

# special request
glm(incidents~year+type, family = 'poisson', offset = log(service), data = ships,subset=(year>1))
glm(incidents~year+type, family = 'poisson', offset = log(service), data = ships,subset=(type!='A'))




