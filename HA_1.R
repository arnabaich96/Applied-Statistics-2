#1.2
library(alr4)
data("wblake")
plot(tapply(wblake$Length,wblake$Age,mean),xlab="age",ylab="avg length")
#this plot supports the plot in 1.5 and states
#that there is a linear relationship between age and avg length
plot(tapply(wblake$Length,wblake$Age,sd),xlab="age",ylab="sd of length")
#variance funcion is not constant since the sv vs age plot 
#isn't a null plot





#1.4
library(MASS)
data("oldfaith")
plot(oldfaith$Duration,oldfaith$Interval,xlab="Duration",ylab="Interval",col="blue")
abline(lm(oldfaith$Interval~oldfaith$Duration),col="red")
#the graph shows a upward linear trend in y with incrasing x
summary(lm(oldfaith$Interval~oldfaith$Duration))
#observing the p-value(<0.05) of the x-variable we can conlude
#there is a significant linear relationship between interval and duration





#2.6
data("ftcollinstemp")

#2.6.1
plot(ftcollinstemp$fall,ftcollinstemp$winter,xlab="Fall",ylab="Winter",col="blue")
#the points are scattered all over the plot and no 
#significant pattern can be identified

#2.6.2
summary(lm(ftcollinstemp$winter~ftcollinstemp$fall))
abline(lm(ftcollinstemp$winter~ftcollinstemp$fall),col="red")
#from the result above we can see that the p_value corresponding to the test
# slope = 0  is < 0.05 (level) which means we reject the null hypothesis
# and conclude avg winter and avg fall temp have a significant linear relationship

#2.6.3
#from the summary result above we can see that multiple R_sq(coeff of determination)=0.037
#which means a total of 0.0371*100=3.71% of variablity in winter is expla
#this low value of COD(R_sq) indicates although the relationship is happen to be linear
#the model is not at all a good fit.

#2.6.4
D1=ftcollinstemp[c(1:90),]
D2=ftcollinstemp[c(91:111),]
plot(D1$fall,D1$winter)
abline(lm(D1$winter~D1$fall))
summary(lm(D1$winter~D1$fall))
plot(D2$fall,D2$winter)
abline(lm(D2$winter~D2$fall))
summary(lm(D2$winter~D2$fall))
#in both the plots points are scattered acorss the graph wihtout an identifiable trend
#summary statistics supports the facts that there are no significant linear relationship in both
#the cases as both p_values corresponding to slope are >> 0.05


#2.13
data("Heights")
#2.13.1
summary(lm(Heights$dheight~Heights$mheight))
#Estimate of variance
var=sum((Heights$dheight-(29.92+(0.541*Heights$mheight)))^2)/(length(Heights$mheight)-2);var

#2.13.2
confint(lm(Heights$dheight~Heights$mheight),"Heights$mheight",level=0.99)
#99% confidence interval

#2.13.3
MH=Heights$mheight
DH=Heights$dheight
model=lm(DH~MH)
newdata=data.frame(MH=64)
predict(model,newdata, se.fit=FALSE, interval="prediction", level=0.99)
#99% prediction interval



#2.21
attach(data(wm1))
x=wm1[,3]
y=wm1[,2]

#2.21.1
plot(x,y,xlab="RSpd",ylab="CSpd")
#linear regression seems plausible since data indicates a increasing linear trend
#along y=x line

#2.21.2
model=lm(y~x)
summary(model)
#model summary

#2.21.3
newdata=data.frame(x=7.4285)
predict(model,newdata,interval = "prediction",level=0.95)
#prediction interval

#2.21.4
mean(x)
SXX=sum((x-mean(x))^2);SXX
RSS=sum((y-3.14123-(0.75573*x))^2);RSS
length(x)-2
sigma_sq=RSS/(length(x)-2);sigma_sq
knitr::stitch("Assign 5167.R")

    