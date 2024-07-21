setwd("D:\\TA_R\\Week3\\C3_ClassData")
dataset <- read.csv("C3_HW.csv", encoding = "utf-8")
str(dataset)
View(dataset)

#############################################作業補充#############################################
############################可以直接用Excel或其他方法整理資料，程式碼僅供參考############################

##subset用法範例##
tainan<-subset(dataset,city=='臺南市')  #取出特定條件資料
str(tainan)           #查看臺南市資料
View(tainan)
hist(dataset$溫度, main="Temperature of Tainan from 2009 to 2011", xlab = "Temperature(°C)", ylab = "Count")      
hist(tainan$溫度, main="Temperature of Tainan from 2009 to 2011", xlab = "Temperature(°C)", ylab = "Count")       

##新增季節欄位範例(其中一種方法)##
season <- NA                                 #新增一名為season之向量為NA
dataset <- cbind(dataset, season)            #合併(cbind)原始表格與season欄位
#dataset <- cbind(dataset, season=NA)                 
dataset[(dataset$month==12 | dataset$month==1 | dataset$month==2),]$season <-"winter"       #篩選出月份為12、1、2的資料列，將季節欄位填值為"winter"

#dataset[1]:第一行、dataset[1,]:第一列
#邏輯判斷符號|:or

##匯出新的csv