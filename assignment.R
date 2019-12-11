library(lubridate)
library(dplyr)
data<-read.csv("data.csv")
data<-data[,c("eventTimestamp","userID","sessionID","platform","userCountry","realCurrencyAmount.1","realCurrencyType")]
data$eventTimestamp<-as.Date(as.character(as.POSIXct(data$eventTimestamp)))
data<-mutate(data,Date=as.Date(data$eventTimestamp))
data$platform<-as.character(data$platform)

data$realCurrencyType<-as.character(data$realCurrencyType)
data[is.na(data$realCurrencyType)==TRUE,]$realCurrencyType<-"FAKE"
data[data$realCurrencyType=="GBP",]$realCurrencyAmount.1<-(data[data$realCurrencyType=="GBP",]$realCurrencyAmount.1)*1.31
data[data$realCurrencyType=="AUD",]$realCurrencyAmount.1<-(data[data$realCurrencyType=="AUD",]$realCurrencyAmount.1)*0.682
data[data$realCurrencyType=="DKK",]$realCurrencyAmount.1<-(data[data$realCurrencyType=="DKK",]$realCurrencyAmount.1)*0.148
data[data$realCurrencyType=="CAD",]$realCurrencyAmount.1<-(data[data$realCurrencyType=="CAD",]$realCurrencyAmount.1)*0.755
data[data$realCurrencyType=="CHF",]$realCurrencyAmount.1<-(data[data$realCurrencyType=="CHF",]$realCurrencyAmount.1)*1.015
data[data$realCurrencyType=="EUR",]$realCurrencyAmount.1<-(data[data$realCurrencyType=="EUR",]$realCurrencyAmount.1)*1.1
data[data$realCurrencyType=="SEK",]$realCurrencyAmount.1<-(data[data$realCurrencyType=="SEK",]$realCurrencyAmount.1)*0.105
data[data$realCurrencyType=="TWD",]$realCurrencyAmount.1<-(data[data$realCurrencyType=="TWD",]$realCurrencyAmount.1)*0.032
data[data$realCurrencyType=="RUB",]$realCurrencyAmount.1<-(data[data$realCurrencyType=="RUB",]$realCurrencyAmount.1)*0.015
data[data$realCurrencyType=="NOK",]$realCurrencyAmount.1<-(data[data$realCurrencyType=="NOK",]$realCurrencyAmount.1)*0.109
data[data$realCurrencyType=="BRL",]$realCurrencyAmount.1<-(data[data$realCurrencyType=="BRL",]$realCurrencyAmount.1)*0.24
data[data$realCurrencyType=="NZD",]$realCurrencyAmount.1<-(data[data$realCurrencyType=="NZD",]$realCurrencyAmount.1)*0.652
data[data$realCurrencyType=="JPY",]$realCurrencyAmount.1<-(data[data$realCurrencyType=="JPY",]$realCurrencyAmount.1)*0.009
data[data$realCurrencyType=="THB",]$realCurrencyAmount.1<-(data[data$realCurrencyType=="THB",]$realCurrencyAmount.1)*0.033

android_data<-data[data$platform=="ANDROID",]
(length(na.omit(match(unique(android_data[android_data$Date=="2019-11-01",]$userID),unique(android_data[android_data$Date=="2019-11-02",]$userID))))/length(unique(android_data[android_data$Date=="2019-11-01",]$userID)))*100
(length(na.omit(match(unique(android_data[android_data$Date=="2019-11-01",]$userID),unique(android_data[android_data$Date=="2019-11-08",]$userID))))/length(unique(android_data[android_data$Date=="2019-11-01",]$userID)))*100
(length(na.omit(match(unique(android_data[android_data$Date=="2019-11-01",]$userID),unique(android_data[android_data$Date=="2019-11-29",]$userID))))/length(unique(android_data[android_data$Date=="2019-11-01",]$userID)))*100

ios_data<-data[data$platform=="IOS",]
(length(na.omit(match(unique(ios_data[ios_data$Date=="2019-11-01",]$userID),unique(ios_data[ios_data$Date=="2019-11-02",]$userID))))/length(unique(ios_data[ios_data$Date=="2019-11-01",]$userID)))*100
(length(na.omit(match(unique(ios_data[ios_data$Date=="2019-11-01",]$userID),unique(ios_data[ios_data$Date=="2019-11-08",]$userID))))/length(unique(ios_data[ios_data$Date=="2019-11-01",]$userID)))*100
(length(na.omit(match(unique(ios_data[ios_data$Date=="2019-11-01",]$userID),unique(ios_data[ios_data$Date=="2019-11-29",]$userID))))/length(unique(ios_data[ios_data$Date=="2019-11-01",]$userID)))*100

(length(na.omit(match(unique(data[data$Date=="2019-11-01",]$userID),unique(data[data$Date=="2019-11-02",]$userID))))/length(unique(data[data$Date=="2019-11-01",]$userID)))*100
(length(na.omit(match(unique(data[data$Date=="2019-11-01",]$userID),unique(data[data$Date=="2019-11-08",]$userID))))/length(unique(data[data$Date=="2019-11-01",]$userID)))*100
(length(na.omit(match(unique(data[data$Date=="2019-11-01",]$userID),unique(data[data$Date=="2019-11-29",]$userID))))/length(unique(data[data$Date=="2019-11-01",]$userID)))*100

daily_user_data<-data %>% group_by(Date) %>% summarise(Daily_users=length(unique(userID)))
daily_user_data

android_data<-data[data$platform=="ANDROID",]
android_data %>% group_by(Date) %>% summarise(ARPDAU=sum(realCurrencyAmount.1)/length(unique(userID)))

ios_data<-data[data$platform=="IOS",]
ios_data %>% group_by(Date) %>% summarise(ARPDAU=sum(realCurrencyAmount.1)/length(unique(userID)))

data %>% group_by(Date) %>% summarise(ARPDAU=sum(realCurrencyAmount.1)/length(unique(userID)))

newdata<-data %>% group_by(userID) %>% mutate(amount_earned=sum(realCurrencyAmount.1))
max(newdata$amount_earned)

newdata[newdata$amount_earned==max(newdata$amount_earned),]$userID[1]

length(data$sessionID)/length(unique(data$userID))

length(unique(data$userID))
