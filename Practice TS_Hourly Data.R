setwd("C:\\Users\\Gaurav\\Documents\\Analytics Vidhya\\Time Series Practice Prob")
require(data.table)
require(dplyr)
require(lubridate)
require(ggplot2)
require(rpart)
require(partykit)
require(caret)
require(randomForest)
require(forecast)
require(zoo)
require(tseries)
require(MLmetrics)

Train<-read.csv("Train.csv",stringsAsFactors = F)

Test<-read.csv("Test.csv",stringsAsFactors = F)


head(Train,50)

head(Test,50)

summary(Train)

train.ts<-msts(Train$Count,seasonal.periods=c(24,168))

d<-decompose(train.ts)

plot(d)

ndiffs(train.ts)

train.ts.diff<-diff(train.ts)

plot(train.ts.diff)

adf.test(train.ts.diff)

acf(train.ts.diff)

nsdiffs(train.ts.diff)

##Hourly distribution of counts

Train$Hour<-hour(dmy_hm(Train$Datetime))

Train$Year<-year(dmy_hm(Train$Datetime))

Train$Day<-day(dmy_hm(Train$Datetime))

Train$Month<-month(dmy_hm(Train$Datetime))

Train$Weekday<-weekdays(dmy_hm(Train$Datetime))

Test$Hour<-hour(dmy_hm(Test$Datetime))

Test$Year<-year(dmy_hm(Test$Datetime))

Test$Day<-day(dmy_hm(Test$Datetime))

Test$Month<-month(dmy_hm(Test$Datetime))

Test$Weekday<-weekdays(dmy_hm(Test$Datetime))

ggplot(data=Train,aes(x=Hour,y=Count))+ geom_bar(stat="identity")

ggplot(data=Train,aes(x=Year,y=Count,fill=Year))+ geom_bar(stat="identity")

ggplot(data=Train[Train$Year==2013,],aes(x=Hour,y=Count,fill=factor(Hour)))+ geom_bar(stat="identity")

ggplot(data=Train[Train$Year==2014,],aes(x=Hour,y=Count,fill=factor(Hour)))+ 
  geom_bar(stat="identity")+ facet_wrap(~Day)


ggplot(data=Train[Train$Year==2014,],aes(x=Hour,y=Count,fill=factor(Hour)))+ 
  geom_bar(stat="identity")+ facet_wrap(~Weekday)

Train1<-Train %>% group_by(Year,Month,Weekday) %>% summarise(Avg.Day.Count=mean(Count,na.rm=T))

?summarise

ggplot(data=Train1[Train1$Year==2014,],aes(x=Weekday,y=Avg.Day.Count))+ geom_bar(stat="identity")+ facet_wrap(~Month)


Train2<-Train %>% group_by(Year,Month) %>% summarise(Avg.Mth.Count=mean(Count,na.rm=T))

ggplot(data=Train2,aes(x=factor(Month),y=Avg.Mth.Count))+ geom_bar(stat="identity")+ facet_wrap(~Year)


##Bnchmark fit

summary(Train[Train$Year==2014,])

Train.test<-Train[c(16955:18288),]

Train.train<-Train[c(1:16944),]

Train.train.ts<-msts(Train.train$Count,seasonal.periods=c(24,168))

fit.stlf<-stlf(diff(Train.train.ts))

summary(fit.stlf)

fcast<-forecast(fit.stlf,h=1344)

fcast1<-predict(fit.stlf,n.ahead=1344)
plot(fcast1)

RMSE(diff(Train.test[c(1:336),"Count"]),fcast1$mean)

?forecast
