#Q2
setwd("/Users/huangweiting/coding/INTRODUCTION TO SCIENTIFIC COMPUTING SOFTWARE /Week10")
getwd()
dataset1<-read.csv("weight.csv") 
#str(dataset1)
View(dataset1)
#dim(dataset1)
library("lattice")
t.test(dataset1$weight_1, mu =20.80)
t.test(dataset1$weight_2, mu =23.35)

##Two-sample paired T-test
#read dataset2
t.test(dataset1$score_1, dataset1$score_2,paired=TRUE)

#Q3-1
install.packages("nortest") 
setwd("/Users/huangweiting/coding/INTRODUCTION TO SCIENTIFIC COMPUTING SOFTWARE /Week10")
getwd()
dataset2<-read.csv("期中前課堂練習.csv") 
#str(dataset2)
View(dataset2)
#dim(dataset2)
kaohsiung<-subset(dataset2,dataset2$city=="高雄市" & dataset2$year==2010)
Taipai<-subset(dataset2,dataset2$city=="臺北市" & dataset2$year==2010)
View(kaohsiung)
View(Taipai)
library("nortest")
lillie.test(kaohsiung$溫度)
lillie.test(Taipai$溫度)
all <-rbind(kaohsiung,Taipai)
View(all)
library(car)
leveneTest(all$溫度,all$city, center=mean)
t.test(all$溫度[all$city=="高雄市"], all$溫度[all$city=="臺北市"])

#Q3-2
NewTaipai<-subset(dataset2,dataset2$city=="新北市" & dataset2$year==2010)
View(NewTaipai)
library("nortest")
shapiro.test(NewTaipai$溫度)
lillie.test(Taipai$溫度)
all2 <-rbind(NewTaipai,Taipai)
View(all2)
library(car)
leveneTest(all2$溫度,all$city, center=mean)
t.test(all2$溫度[all2$city=="新北市"], all2$溫度[all2$city=="臺北市"],var.equal = TRUE)


