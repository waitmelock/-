#Week 9

#Q1
install.packages("Hmisc")
install.packages("ppcor")
setwd("/Users/huangweiting/coding/INTRODUCTION TO SCIENTIFIC COMPUTING SOFTWARE /Week12")
getwd()
dataset1<-read.csv("Tainan_pollution_station.csv") 
cor.test(dataset1$PM25, dataset1$NO2, method="spearman") 

#Q2
library("Hmisc")
library("ppcor")
PM25<-dataset1$PM25
NO2<-dataset1$NO2
Temperature<-dataset1$Temperature
RH<-dataset1$RH
dataset2<-cbind(PM25,NO2,Temperature,RH)rView(dataset2)
rcorr(as.matrix(dataset2),type=c("spearman"))
.test(Ex2$PM252NO22Temperature,od = "pearspearman?p
pcor.test(PM25,NO2,RH,method = "spearman")
