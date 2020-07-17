### 04.Test ###
test<-read.csv("\\test_20200413.csv")

tdate_all<-as.Date(unique(test$date))
tdatert<-tdate_all[tdate_all>=as.Date("2020-02-18")]

test_pdf<-test$total-Lag(test$total,1)
nday<-6 #Moving periods (nday+1)
test_ma<-runMean(test_pdf,0:(nday))

test_pdf<-test_pdf[tdate_all>=as.Date("2020-02-18")]
test_ma<-test_ma[tdate_all>=as.Date("2020-02-18")]

#Modelled Changes in daily new cases (i.e. speed of transmission)
dpdf<-pdf-Lag(pdf,1)
dpdf_rev<-dpdf[date_all>=as.Date("2020-02-18")]

### DLM: Association between the speed of transmission & Tests ###
argvar <- list(fun="lin")
lag<-14 
period<-28 #Day of the early phase 
cb<-crossbasis(test_pdf[1:period],varfun="lin",lag=lag,argvar=argvar,arglag=list(knots=logknots(lag,2)))
model<-lm(dpdf_rev[1:period]~cb)

#Prediction
pred<-crosspred(cb,model,at=1000)
red<-crossreduce(cb,model)
-coef(red)*1000 #Reduction in the Speed of Transmission per 1,000 tests 


### Rolling regression: Association between the Rt & Tests ###
coef_time<-coef_timel<-coef_timeu<-c()
test_ma_rev<-test_ma[-c(1:(nday+1))]

for(i in seq(length(test_ma_rev)-6)){
model<-lm(rt[i:c(i+6)]~test_ma_rev[i:c(i+6)])
coef_time[i]<-summary(model)$coef[2,1]*1000
coef_timel[i]<-c(summary(model)$coef[2,1]-1.96*summary(model)$coef[2,2])*1000
coef_timeu[i]<-c(summary(model)$coef[2,1]+1.96*summary(model)$coef[2,2])*1000
}