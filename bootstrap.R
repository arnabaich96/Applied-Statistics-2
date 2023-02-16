set.seed(12345)
n=100
x1=rnorm(n)
x2=rnorm(n)
y=0.1*x1+0.2*x2+rnorm(n)
z1=x1+x2
z2=x2
z3=z1
z4=x1-x2
m1=lm(y~z1+z2)
m2=lm(y~z3+z4)


#p-value from Normal testing Model_1
T.stat <- summary(m1)[["coefficients"]][3, "t value"]
2*pt(abs(T.stat),df=97,lower.tail = FALSE)

#p-value from Normal testing Model_2
T.stat <- summary(m2)[["coefficients"]][3, "t value"]
2*pt(abs(T.stat),df=97,lower.tail = FALSE)

#testing using Bootstrap with Model 1
m.null <- lm(y~z1)  # under the null hypothesis
yhat <- m.null$fitted.values
ehat <- m.null$residuals

B<-999
T.stat.boot<-rep(0,B)
for(i in 1:999){
  id<-sample(1:n,n,replace=T)
  yboot <- yhat + ehat[id]
  m <- lm(yboot~z1+z2)
  T.stat.boot[i]<-summary(m)[["coefficients"]][3, "t value"]
}

#p_Value of Bootstrap Residual testing using Model 1
2*(1+sum(T.stat.boot>abs(T.stat)))/(B+1)


#testing using Bootstrap with Model 2
m.null <- lm(y~z3)  # under the null hypothesis
yhat <- m.null$fitted.values
ehat <- m.null$residuals

B<-999
T.stat.boot<-rep(0,B)
for(i in 1:999){
  id<-sample(1:n,n,replace=T)
  yboot <- yhat + ehat[id]
  m <- lm(yboot~z3+z4)
  T.stat.boot[i]<-summary(m)[["coefficients"]][3, "t value"]
}

#p_Value of Bootstrap Residual testing using Model 2
2*(1+sum(T.stat.boot>abs(T.stat)))/(B+1)

