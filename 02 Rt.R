##### 2. Rt #####
rt_ind<-date_all>=as.Date("2020-02-18")
date2_all<-unique(data$date2)
datert<-date_all[date_all>=as.Date("2020-02-18")]

#Observed Daily new cases
pdf_real<-confirmed_all-Lag(confirmed_all,1)
data_res_real<-data.frame(dates=datert,I=pdf_real[rt_ind])

### Rt calculation (Example code only) ###
#1) The important cases were not considered
#2) Sliding windows: 7 days (default)
#3) Serial Interval (Gamma Dist.): Mean (4.98), SD: 3.22)

res<-estimate_R(incid=data_res_real,method="parametric_si",
                config = make_config(list(mean_si =4.98, std_si=3.22)))
rt<-res$R$Mean #Mean Rts

