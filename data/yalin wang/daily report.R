setwd('C:\\Users\\idea\\Desktop\\R book')
daily_report<-read.csv('DHS_Daily_Report.csv')
times<-strptime(daily_report$Date.of.Census,'%m/%d/%Y')
plot(times,daily_report$Total.Individuals.in.Shelter,type='l',lwd=2,ylab='numbers')
par(new = TRUE)
plot(times,daily_report$Total.Adults.in.Shelter,type='l',lwd=2,col='red',yaxt='n',ylab='')
par(new = TRUE)
plot(times,daily_report$Total.Children.in.Shelter,type='l',lwd=2,col='blue',yaxt='n',xaxt='n',ylab='',axes=F)
legend('topright',c('Indivials','Adults','Children'),pch='———',col=c('black','red','blue'))
grid(6,6)
axis(1,c(1:271),times)

a=sum(daily_report[,2])+sum(daily_report[,3])
a1=sum(daily_report[,2])/a
a2=sum(daily_report[,3])/a

pie(c(sum(daily_report$Total.Adults.in.Shelter),sum(daily_report$Total.Children.in.Shelter)),labels=paste(round(c(a1,a2),2)*100,'%'),col = c('red','blue'))
legend('topright',c('Adults','Children'),cex=0.8,pch='O',col=c('red','blue'))

library(lubridate)
daily_report$month<-month(times)
agg=aggregate(daily_report[,2:3],by=list(daily_report$month),sum)
agg.T=t(agg[,2:3])
colnames(agg.T)<-c(1:9)
barplot(agg.T,beside = TRUE,xlab = 'Month',ylab = 'Number',main='Bar plot for children and adults in shelter every month')


x=barplot(agg.T,beside = TRUE,xlab = 'Month',ylab = 'Number',main='Bar plot for children and adults in shelter every month')


app=apply(agg.T,2,mean)
lines(apply(x,2,mean),app,lwd=1.5)

library(sm)
sm.density.compare(daily_report$Total.Individuals.in.Shelter,month(times),main='数据按月分组核密度曲线')
legend('topright',c('Jan','Feb','Mat','Apr','May','Jun','July','Aug','Sept'),pch=1,col=1:9)