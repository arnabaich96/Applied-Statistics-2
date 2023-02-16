library("carData")
library("car")
library("effects")
library("DBI")
library("alr4")
library("lme4")
attach(UN11)
head(UN11)
D=data.frame(fertility, log(ppgdp),pctUrban)
#3.2.1
pairs(D)

#3.2.2
summary(lm(fertility~log(ppgdp)))
summary(lm(fertility~pctUrban))


#3.2.3
par(mfrow=c(2,2))
#part 1
Residual_11=resid(lm(fertility~pctUrban))  #eliminating effect of pctUrban from response
Residual_12=resid(lm(log(ppgdp)~pctUrban)) #eliminating effect of pctUrban from predictor
plot(Residual_12,Residual_11,col="blue",ylab=c("Fertility - pctUrban"),xlab=c("log(ppgdp) - pctUrban"))
LM_11=lm(Residual_11~Residual_12)
summary(LM_11)
abline(LM_11,col="red")
R_sq_1=round(100*cor(Residual_11,Residual_12)^2);R_sq_1    #variablity explained
#adding log(ppgdp) explains 32% of remaining variablity in fertitlity after adjusting pctUrban
plot(log(ppgdp),fertility,col="green")
LM_12=lm(fertility~log(ppgdp))
abline(LM_12,col="cyan")
LM_11$coefficients[2]
LM_12$coefficients[2]
#reduction percentage
Reduce_1=((-0.62)-(-0.6151))/(-0.62)*100;Reduce_1

#part 2
Residual_21=resid(lm(fertility~log(ppgdp)))  #eliminating effect of log(ppgdp) from response
Residual_22=resid(lm(pctUrban~log(ppgdp))) #eliminating effect of log(ppgdp) from predictor
plot(Residual_22,Residual_21,col="blue",ylab=c("Fertility - log(ppgdp)"),xlab=c("pctUrban - log(ppgdp)"))
LM_21=lm(Residual_21~Residual_22)
abline(LM_21,col="red")
R_sq_2=round(100*cor(Residual_21,Residual_22)^2);R_sq_2    #variablity explained
#adding pctUrban explains 32% of remaining variablity in fertitlity after adjusting log(ppgdp)
plot(pctUrban,fertility,col="green")
LM_22=lm(fertility~pctUrban)
abline(LM_22,col="cyan")
LM_21$coefficients[2]
LM_22$coefficients[2]
#reduction percentage
Reduce_2=((-0.031)-(-0.0004))/(-0.031)*100;Reduce_2

#part 3
LM_3=lm(fertility~pctUrban+log(ppgdp))
summary(LM_3)


#3.2.5
residual_3=resid(LM_3)
par(mfrow=c(2,2))

plot(resid(LM_11),main = c("1st added variable"),ylab=c(" "),xlab=c(" "))
plot(resid(LM_21),main = c("2nd added variable"),ylab=c(" "),xlab=c(" "))
plot(residual_3,main = c("Multiple Linear Regression"),ylab=c(" "),xlab=c(" "))
mtext("Residual Plots",side = 3, line = -16, outer = TRUE)






