library(data.table)
library(tidyverse)
library(foreach)
library(doParallel)
library(reshape2)
library(gridExtra)

### Model is fit on the timeseries with 10 knots.
### ran_subset is a trajectory. V2 is measurement and x4 is time.

n_knots<-10
fit<-lm(ran_subset2$V2~poly(ran_subset2$x4,df=n_knots))
plot(ran_subset2$x4,ran_subset2$V2)
lines(ran_subset2$x4,predict(fit,data.frame(ran_subset2$x4)))
asumm<-summary(fit)
asumm<-asumm$coefficients
asumm<-as.data.frame(asumm)

coef_<-which(asumm$`Pr(>|t|)`[-1]<50)
coef_<-coef_+1

pid<-unique(random_subset$j)


test_error<-NULL
original_values<-fit$fitted.values
change_par<-NULL
std_shift<-2

## The parameters of the fitted model are changed to identify the parameter with highest impact on shape.

for(par_change in seq(1,n_knots)){
  fit2<-fit
  std_dev<-a$`Std. Error`[coef_[par_change]]/sqrt(length(fit2$fitted.values))
  mean_val<-fit2$coefficients[coef_[par_change]]
  change_par<-mean_val+(std_shift*std_dev)
  mean_change<-change_par
  new_values<-NULL
  for(v in seq(1,1000)){
    fit2$coefficients[coef_[par_change]]<-rnorm(1,mean_change,std_dev)
    n_v<-predict(fit2,data.frame(ran_subset2$x4))
    new_values<-rbind(new_values,cbind(seq(1,length(n_v)),n_v,sum((n_v-original_values)^2)/(length(n_v)-1),v,par_change))
  }
  test_error<-rbind(test_error,new_values)
}
test_error<-as.data.frame(test_error)
colnames(test_error)<-c("Time","Changed Value","MSE","Test_Item","Parameter Changed")
head(test_error)
par_change_mean<-test_error %>% group_by(`Parameter Changed`,Time) %>% dplyr::summarise(mean(MSE))
unique(par_change_mean$`Parameter Changed`)
par_greatest_mse<-unique(par_change_mean$`Parameter Changed`[par_change_mean$`mean(MSE)`==max(par_change_mean$`mean(MSE)`)])
par_greatest_mse #Distributions were changes for this parameter between classes
