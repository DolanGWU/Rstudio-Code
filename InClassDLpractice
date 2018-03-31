#data cleaning process
getwd()
setwd("C:/Users/jingcong/Desktop/R")
ddata=read.csv("weather.csv", header=T)
tdata=read.csv("train.csv", header=T)
#data cleaning 
#data selection
mycars<-c("Species","Latitude","Longitude","WnvPresent","NumMosquitos")
data<-tdata[mycars]
summary(data)
train<- sample(1:10506,7354)
test<-setdiff(1:10506,train)
train<-data[train,]
test<-data[test,]
try<-as.numeric(factor(train$Species,levels=c('CULEX PIPIENS/RESTUANS', 'CULEX RESTUANS','CULEX PIPIENS','CULEX SALINARIUS','CULEX TERRITANS','CULEX TARSALIS','CULEX ERRATICUS')))

train<-train[,-1]
train<-cbind(train,try)
lm.fit<-glm(WnvPresent~.,data=train)
pr.lm<- predict(lm.fit,test)
library(neuralnet)

n<-names(train)
f <- as.formula(paste("WnvPresent ~", paste(n[!n %in% "WnvPresent"], collapse = " + ")))
nn <- neuralnet(f,data=train,hidden=c(5,3),linear.output=T)
plot(nn)
