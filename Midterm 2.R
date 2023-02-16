library("alr4")
library("MASS")
attach(BigMac2003)
m2=lm(BigMac^(-0.5)~.,data=BigMac2003)
summary(m2)
summary(powerTransform(cbind(Apt, Bread, Bus, FoodIndex, Rice, TaxRate, TeachGI,TeachHours, TeachNI) ∼ 1,family="yjPower"),data=BigMac2003)
data=BigMac2003
data$BigMac=BigMac^(-0.5)
data$Bread=log(Bread)
data$Bus=1/Bus
data$Rice=Rice^(-0.5)
data$FoodIndex=log(FoodIndex)
data$Apt=Apt^(0.5)
data$TeachGI=TeachGI^(-0.11)
data$TeachNI=TeachNI^(-0.14)


m3=lm(BigMac~1,data=data)
m=lm(BigMac~.,data=data)
step(m3,k=log(length(BigMac)),direction="forward",scope = formula(m))
m4=lm(BigMac~TeachGI + FoodIndex + Bread,data=data)
summary(m4)
SR=studres(m4)
which(abs(SR)>2)
CD=cooks.distance(m4)
which(CD>3)


