### 3.Doubling time (t) ###

for(k in seq(n.sim)){

#Monter Carlo simulation
cdf.sim<-coef_sim[k,1]/(1 + exp(-(datenum - coef_sim[k,2])/coef_sim[k,3]))
double_cdf_model<-cdf.sim[rt_ind]

for(i in 1:c(length(double_cdf_real)-nday-1)){
cdf_value_model<-range(double_cdf_model[i:c(i+nday)])
double_model[k,i]<-(nday+1)/log(cdf_value_model[2]/cdf_value_model[1],base=2)
}}

#Empirical Mean and 95% intervals
double_mean<-apply(double_model,2,mean)
double_lcl<-apply(double_model,2,quantile,probs=0.025)
double_ucl<-apply(double_model,2,quantile,probs=0.975)

