library(haven)
library(s20x)
sas = read_sas('data/grade2019.sas7bdat')
sas = as.data.frame(sas);sas

lm = lm(Exam~Test,data=sas)
summary(lm)
predict20x(lm,data.frame(Test = c(20,40)))

predict.lm(lm,data.frame(Test = c(20,40)))
predict.lm(lm,data.frame(Test = c(20,40)),interval = 'confidence')
predict.lm(lm,data.frame(Test = c(20,40)),interval = 'prediction')

# t-multiplier
qt(c(.05/2, 1-.05/2),51)
