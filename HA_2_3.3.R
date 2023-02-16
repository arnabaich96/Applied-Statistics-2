library("carData")
library("car")
library("effects")
library("DBI")
library("alr4")
library("lme4")
Girls=BGSgirls


#3.3.1
pairs(~HT2+HT9+WT2+WT9+ST9+BMI18,data=Girls,main="Scatterplot Matrix of Girls Data")
cordata=data.frame(Girls[,c(1,2,3,4,6,11)])
cor(cordata)

#3.3.2
Lm1=lm(BMI18~WT9)
Residual_1=resid(Lm1)
Lm2=lm(ST9~WT9)
Residual_2=resid(Lm2)
par(mfrow=c(2,2))
plot(WT9,BMI18,main = c("BMI18 vs WT9"))
plot(ST9,BMI18,main = c("BMI18 vs ST9"))
plot(WT9,ST9,main = c("ST9 vs WT9"))
plot(Residual_2,Residual_1,main="AVP to add ST9")


#3.3.3
Model=lm(BMI18~HT2+WT2+HT9+WT9+ST9)
summary(Model)
summary.aov(Model)
