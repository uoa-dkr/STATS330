library('TeachingDemos')
data(evap)
evap.df = evap[, -(1:3)]
head(evap.df, 5)

df1 = lm(Evap~.,data = evap.df)
df2 = lm(MaxST~.,data = evap.df[,-11])
df3 = lm(MaxST~Wind,data = evap.df[,-11])
summary(df1)
predfun.lm <- function(train.x, train.y, test.x, test.y) {
  lm.fit <- lm(train.y ~ ., data = train.x)
  ynew <- predict(lm.fit, test.x)
  mean((ynew - test.y)^2)
}
cv.out = crossval(predfun.lm, X = evap.df[, 1:10], Y = evap.df[, 11],
                  K = 10, B = 100, verbose = FALSE)
cv.out$stat;cv.out$stat.se
sd(cv.out$stat.cv[,1])/1


summary(df2)
summary(df3)
1/(1-summary(df2)$r.squared)

sum((fitted(df1)-evap.df[,11])^2)/35
sum((fitted(df2)-evap.df[,11])^2)/36
