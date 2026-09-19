# install packages

library(faraway)
library(car)
library(MASS)

data(teengamb)

# show data
head(teengamb)
str(teengamb)

# Q2 i & ii)
# define mlr model & fit  to predict
model <- lm(gamble ~ sex + status + verbal, data = teengamb)

summary(model)

# anova table
anova(model)

# get residual mean square
summary(model)$sigma^2

# coefficient table
coef(summary(model))

# Q2 iii) get multicollinearity

vif(model)


# Q2 iv) get 95% confidence intrevals for  B1(sex) & B3(verbal scoree)

confint(model, c("sex","verbal"), level = 0.95)

# Q2 v) OLS vs robust regression

robustModel <- rlm(gamble ~ sex + status + verbal, data = teengamb)
summary(robustModel)

# compare coefficients of models
# show
coef(model)
coef(robustModel)

# compare rss

olsRSS <- sum(resid(model)^2)
robustRSS <- sum(resid(robustModel)^2)

# show
olsRSS
robustRSS

# compare RMSE 
olsRMSE <- sqrt(mean(resid(model)^2))
robustRMSE <- sqrt(mean(resid(robustModel)^2))

# show
olsRMSE
robustRMSE

# Q2 vi)

outlierModel <- lm(gamble ~ sex + status, data = teengamb)

summary(outlierModel)

# studentized residules
studentRes <- rstudent(outlierModel)
studentRes

# show observations where absol studentized residule > 2
studentRes[abs(studentRes) > 2]

# display actual observations

teengamb[abs(studentRes) > 2,]

