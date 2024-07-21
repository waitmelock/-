setwd("/Users/huangweiting/coding/INTRODUCTION TO SCIENTIFIC COMPUTING SOFTWARE /報告/110年傷亡道路交通事故資料/A2")
getwd()
data1 <- read.csv("2021年度A2交通事故資料_12.csv")  
#View(data1)
all1<- as.data.frame(ftable(data1$當事者區分.類別.大類別名稱.車種,data1$處理單位名稱警局層))
View(all1)
?ftable
all2 <- cbind(all1,all2)
View(all2)



###load on the internet
install.packages("tidyverse")
library(readr)
library(data.table)
dir = "/Users/huangweiting/coding/INTRODUCTION TO SCIENTIFIC COMPUTING SOFTWARE /報告/110年傷亡道路交通事故資料/A2"
file_list = list.files(path = dir, pattern = "*.csv$",recursive = TRUE,full.names = TRUE) 
file_list
store_csv = paste(dir,"new.csv")       
for(i in 1:length(file_list))     #循环绝对地址的列表
{
  df = fread(file = file_list[i],encoding = 'UTF-8')
  write.table(df,file="all.csv",sep=",",append=TRUE,row.names = FALSE, col.names = TRUE,fileEncoding = "UTF-8")
}
write
  
###
