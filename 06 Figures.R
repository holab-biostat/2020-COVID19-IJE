### 06. Figures ###
library(shape)

## Figure 1 ##
plot(datenum,pdf_real,type="h",lwd=6.5,lend=1,ylab="Numner of daily confirmed cases",cex.lab=1,xlab="Date"
     ,xaxt='n',cex.axis=1,ylim=c(0,70),col="goldenrod1",xlim=c(-4,87))
abline(h=0,col="gray")
Axis(side=1,at=c(datenum)[seq(1,length(datenum),7)],
labels=c(date2_all[datenum])[seq(1,length(datenum),7)],cex.axis=1,tck=-.02)

#Add text
Arrows(x0=1,y0=30,y1=1,x1=1,arr.type="triangle",arr.width=0.05,arr.length = 0.05)
text(1,33,"First case \n in Korea",cex=1.1)


## Figure 2 ##
#Plot a (Daily cumulative case)
plot(datenum[datenum>=day1],confirmed_all[datenum>=day1],type="h",lwd=2,ylab="Cummulative cases",cex.lab=1,xlab="Date",xaxt='n',
     cex.axis=0.9,ylim=c(min(confirmed_all,na.rm=T),max(confirmed_all,na.rm=T)),col="azure3",cex=1)
Axis(side=1,at=c(datenum[datenum>day1])[seq(1,length(datenum),7)],
labels=c(date2_all[datenum>=day1])[seq(1,length(datenum),7)],cex.axis=1,tck=-.02)
legend("topleft",legend="A",bty="n",cex=1.2,text.font=2)

#Plot d (Logistic growth curve) 
datenumr<-as.Date(datert)-as.Date(datert)[1]+1

col.main="black"
col1<-"yellowgreen"
col2<-"red2"
col3<-"dodgerblue"

plot(datenum[datenum>=day1],cdf[datenum>=day1],type="l",lwd=1,ylab="Cummulative cases",cex.lab=1,xlab="Date",xaxt='n',
     cex.axis=0.9,ylim=c(min(confirmed_all,na.rm=T),max(confirmed_all,na.rm=T)),col=col.main)
Axis(side=1,at=c(datenum[datenum>=day1])[seq(1,length(datenum),7)],
labels=c(date2_all[datenum>=day1])[seq(1,length(datenum),7)],cex.axis=1,tck=-.02)
legend("topleft",legend="D",bty="n",cex=1.2,text.font=2)

#Peak & Plateau points
abline(v=sp[,1]+day1,col=col2,lty=3)
abline(v=se[,1]+day1,col=col3,lty=3)


#Plot e (Rt)
mycol_real <-"orchid4"
mycol_real2 <- rgb(0.6,0.2,0.8,alpha =0.3)

datenumr<-as.Date(datert)-as.Date(datert)[1]+1
plot(datenumr,c(rep(NA,nday+1),res$R$Mean),type="l",ylim=c(0,4.5),ylab=expression(R[t]),
     cex.lab=1,cex.axis=0.9,xaxt='n',xlab="Date",col=mycol_real,lwd=1)
Axis(side=1,at=seq(1,length(datenumr),7),
labels=substr(datert,6,10)[seq(1,length(datenumr),7)],cex.axis=1,tck=-.02)
polygon(c(res$R$t_start+nday,rev(res$R$t_start+nday)),
       c(res$R$Quantile.0.975,rev(res$R$Quantile.0.025)),border=NA,col=mycol_real2)
lines(datenumr,c(rep(NA,nday+1),res$R$Mean),lwd=1,col=mycol_real)
abline(h=1,lty=2)
abline(v=sp[,1],col=col2,lty=3)
abline(v=se[,1],col=col3,lty=3)
legend("topleft",legend="E",bty="n",cex=1.2,text.font=2)


#Plot f (Doubling time)
mycol_real <- grey(0.2)
mycol_real2 <-rgb(0.8,0.8,0.8,alpha =0.4) 

datenumr<-as.Date(datert)-as.Date(datert)[1]+1
plot(datenumr,c(rep(NA,nday+1),double_mean),type="l",ylim=c(0,50),ylab="Doubling time (day)",
     cex.lab=1,cex.axis=0.9,xaxt='n',xlab="Date",col=mycol_real,lwd=1)
Axis(side=1,at=seq(1,length(datenumr),7),
labels=substr(datert,6,10)[seq(1,length(datenumr),7)],cex.axis=1,tck=-.02)
polygon(c(datenumr,rev(datenumr)),
       c(c(rep(NA,7),double_lcl),rev(c(rep(NA,7),double_ucl))),border=NA,col=mycol_real2)
abline(v=sp[,1]+0.5,col=col2,lty=3)
abline(v=se[,1]+0.5,col=col3,lty=3)
lines(datenumr,c(rep(NA,nday+1),double_mean),cex=1,col=mycol_real)
legend("topleft",legend="F",bty="n",cex=1.2,text.font=2)


## Figure 4B (test) ##
plot(pred,"slice",var=1000,cex.lab=1,cex.axis=1,ylab="Changes in daily confirmed cases",
     ylim=c(-0.3,0.3),xlab="Lag days")
legend("topleft",legend="B",bty="n",cex=1.2,text.font=2)
points(0:14,pred$matfit,cex=1,pch=16)



