library("carData")
library("car")
library("effects")
library("DBI")
library("alr4")
library("lme4")
Girls=BGSgirls
attach(Girls)

ave=(WT2+WT9+WT18)/3
lin=WT18-WT2
quad=WT2-(2*WT9)+WT18
summary(lm(BMI18~ave+lin+quad))
