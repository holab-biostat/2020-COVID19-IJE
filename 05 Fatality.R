### 05. Fatality ###
#Daily cumulative deaths
death_all<-data$deceased
ddate_all<-as.Date(unique(data$date))
date2_all<-unique(data$date2)

ddatert<-ddate_all[ddate_all>=as.Date("2020-02-18")]

#Daily new death
death_pdf<-death_all-Lag(death_all,1)

#Proportion of new deaths relative to the cumulative confirmed cases 
death_percase<-death_pdf/confirmed_all*100

#Changes in the Proportion of new deaths relative to the cumulative confirmed cases
#Before & After Mar.01 (7th Edition, COVID-19 Action Procedures)
t.test(death_percase[rt_ind][2:13],death_percase[rt_ind][14:56])

## Regression (Regional level): Code only ##
#Var: Number of Regional Medical resources
#Confounders:population, sex ratio, the percentage of people aged กร 60 years, 
#gross regional domestic product (GRDP) per capita, and age-sex standardized smoking prevalence 
#reg_cases: Cumulative numbers of confirmed cases for each region (on Apr. 13, 2020)
model<-lm(deathr[,1]*100~Var+pop+pop_sex+pop_over60+grdp+smoke,weights=reg_cases,data=death_covdata_rev)
