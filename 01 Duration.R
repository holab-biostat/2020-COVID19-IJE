rm(list=ls())

library(tsModel)
library(mvtnorm)
library(mvmeta)
library(EpiEstim)
library(dlnm)

### Seoul Data (2020.01.20 to 2020.04.13 ###
data<-read.csv("\\seoul_sample_20200413.csv")
data$date2<-substr(data$date,6,10) #Month-day

### Number of Simulation (Monte-Carlo Simulation) ###
n.sim<-1000

##### 1. Duration #####

### SP: Day from the Epidemic Start date (02.18) to Peak point ###
### SE: Day from the Epidemic Start date (02.18) to Plateau (End) point ###
sp<-se<-matrix(NA,1,4) #Emperical Mean, SD, 95% eCI

### SCASE: Confirmed Cases at 7th day ###
### PCASE: Confirmed Cases at Peak Point ###
### ECASE: Confirmed Cases at Plateau (End) Point ###
scase<-pcase<-ecase<-matrix(NA,1,4) #Emperical Mean, SD, 95% eCI

### Daily-cumulative cases ###
confirmed_all<-data$confirmed
date_all<-as.Date(unique(data$date))

datenum<-as.numeric(date_all)-as.numeric(date_all)[1]+1 #Sequence of date

#Logistic modelling
model<- nls(confirmed_all~SSlogis(datenum,phi1, phi2, phi3))

#Modelled series
cdf<-predict(model) #Cumulative
pdf<-round(cdf-Lag(cdf,1),0) #Daily new confirmed

#Simulated coefficients: Monte Carlo simulation
coef_sim<-rmvnorm(n.sim,coef(model),vcov(model))

#Sumulated parameters
sp.sim<-se.sim<-matrix(NA,n.sim,1)
scase.sim<-pcase.sim<-ecase.sim<-matrix(NA,n.sim,1) 

for(k in seq(n.sim)){
cdf.sim<-coef_sim[k,1]/(1 + exp(-(datenum - coef_sim[k,2])/coef_sim[k,3]))
pdf.sim<-cdf.sim-Lag(cdf.sim,1)
dpdf.sim<-pdf.sim-Lag(pdf.sim,1)

day1<-as.numeric(as.Date("2020-02-18"))-as.numeric(date_all)[1]+1 

#Simulated Peak points
sp.sim[k,1]<-datenum[which.max(pdf.sim)]-day1

#Simulated Plateau points
slope<-diff(range(cdf.sim[datenum>day1]))/diff(range(datenum[datenum>day1]))
pdf.sim2<-pdf.sim[datenum>day1]
se.sim[k,1]<-which.min(abs(pdf.sim2[seq(length(pdf.sim2))>datenum[which.max(pdf.sim2)]]-slope))+which.max(pdf.sim2)

#Simulated daily new Cases
scase.sim[k,1]<-pdf.sim[7+day1]
pcase.sim[k,1]<-pdf.sim[sp.sim[k,1]+day1]
ecase.sim[k,1]<-pdf.sim[se.sim[k,1]+day1]
}

#Calculated Durations
sp[,1]<-mean(sp.sim,na.rm=T)
sp[,2]<-sd(sp.sim,na.rm=T)
sp[,3:4]<-quantile(sp.sim,c(0.025,0.975),na.rm=T)

se[,1]<-mean(se.sim,na.rm=T)
se[,2]<-sd(se.sim,na.rm=T)
se[,3:4]<-quantile(se.sim,c(0.025,0.975),na.rm=T)

#Calculated daily new Cases
scase[,1]<-mean(scase.sim,na.rm=T)
scase[,2]<-sd(scase.sim,na.rm=T)
scase[,3:4]<-quantile(scase.sim,c(0.025,0.975),na.rm=T)

pcase[,1]<-mean(pcase.sim,na.rm=T)
pcase[,2]<-sd(pcase.sim,na.rm=T)
pcase[,3:4]<-quantile(pcase.sim,c(0.025,0.975),na.rm=T)

ecase[,1]<-mean(ecase.sim,na.rm=T)
ecase[,2]<-sd(ecase.sim,na.rm=T)
ecase[,3:4]<-quantile(ecase.sim,c(0.025,0.975),na.rm=T)




