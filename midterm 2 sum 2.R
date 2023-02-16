library(alr4)
attach(BGSboys)
data=data.frame(WT2,HT2,WT9,HT9,LG9,ST9,HT18)
head(data)
pairs(data)
m1=lm(HT18~.,data=data)
summary(powerTransform(cbind(WT2, HT2, WT9, HT9, LG9, ST9)~1,data=data))
data$WT2=log(WT2)
data$WT9=1/WT9
data$LG9=1/LG9
head(data)
m=lm(HT18~.,data=data)
M=step(m,data=data)
invResPlot(M)
marginalModelPlot(M)
boot_coeff=list()
for( i in 1:999){ 
  id=sample(1:66, 66, replace = TRUE)
  boot_data=NULL
  boot_data=data[id,] 
  m = lm( data = boot_data,  HT18 ~ .)
  boot_coeff[[i]] = m$coefficient 
}
boot_coeff_new=do.call(rbind,boot_coeff)
WT2=c(avg = mean(boot_coeff_new[,"WT2"]), se = sd((boot_coeff_new[,"WT2"])), quantile(x=boot_coeff_new[,"WT2"], probs = c(0.025, 0.975))) 
HT2=c(avg = mean(boot_coeff_new[,"HT2"]), se = sd((boot_coeff_new[,"HT2"])), quantile(x=boot_coeff_new[,"HT2"], probs = c(0.025, 0.975))) 
HT9=c(avg = mean(boot_coeff_new[,"HT9"]), se = sd((boot_coeff_new[,"HT9"])), quantile(x=boot_coeff_new[,"HT9"], probs = c(0.025, 0.975))) 
LG9=c(avg = mean(boot_coeff_new[,"LG9"]), se = sd((boot_coeff_new[,"LG9"])), quantile(x=boot_coeff_new[,"LG9"], probs = c(0.025, 0.975))) 
rbind(WT2,HT2,HT9,LG9)
summary(M)
confint(M)
rm(WT2)
rm(HT2)
rm(HT9)
rm(LG9)

