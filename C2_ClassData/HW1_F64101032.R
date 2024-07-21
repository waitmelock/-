#########################
# Set Working Directory #
#########################

# Get your current working directory #
getwd() # Re-check the path for the working directory

# Change your current working directory #
setwd("/Users/huangweiting/coding/INTRODUCTION TO SCIENTIFIC COMPUTING SOFTWARE /C2_ClassData")
getwd() # Re-check the path for the working directory

#############################
# Import an example dataset #
#############################

dataset <- read.csv("C2_HW.csv")
#str(dataset)     #Check the variable format 看資料整體的data
#View(dataset)		#Check Dataset
#summary(dataset)	#Get the summary statistics about the object 給基礎統計數據（和資料格式有關）

#Q1
#max
round(max(dataset$temperature,na.rm = TRUE),2)
#min
round(min(dataset$temperature,na.rm = TRUE),2)
#mean
round(mean(dataset$temperature,na.rm=TRUE),2)
#Standard Deviation
round(sd(dataset$temperature,na.rm = TRUE),2)
#quantile
round(quantile(dataset$temperature,0.25,na.rm = TRUE),2)
round(quantile(dataset$temperature,0.50,na.rm = TRUE),2)
round(quantile(dataset$temperature,0.75,na.rm = TRUE),2)

#Q2
#max
round(aggregate(dataset$temperature, by=list(type=dataset$year),FUN=max),2)
#min
round(aggregate(dataset$temperature, by=list(type=dataset$year),FUN=min),2)
#mean
round(aggregate(dataset$temperature, by=list(type=dataset$year),FUN=mean),2)
#standard deviation
round(aggregate(dataset$temperature, by=list(type=dataset$year),FUN=sd),2)
#quantile
round(aggregate(dataset$temperature, by=list(type=dataset$year),FUN=quantile,probs=c(0.25,0.50,0.75)),2)

#Q3
dataset_2 <- read.csv("Tainan_season.csv")
histogram(x= ~temperature|season,data =dataset_2, xlab = "temperature(Degree Celsius)",ylab = "Count",type="count",layout=c(2,2),main='Histogram of temperature in different seasons in Tainan during 2009-2011')

#Q4
dataset_2 <- read.csv("Tainan_season_2009.csv")
histogram(x= ~temperature|season,data =dataset_2, xlab = "temperature(Degree Celsius)",ylab = "Count",type="count",layout=c(2,2),main='Histogram of temperature in different seasons in Tainan in 2009')
dataset_2 <- read.csv("Tainan_season_2010.csv")
histogram(x= ~temperature|season,data =dataset_2, xlab = "temperature(Degree Celsius)",ylab = "Count",type="count",layout=c(2,2),main='Histogram of temperature in different seasons in Tainan in 2010')
dataset_2 <- read.csv("Tainan_season_2011.csv")
histogram(x= ~temperature|season,data =dataset_2, xlab = "temperature(Degree Celsius)",ylab = "Count",type="count",layout=c(2,2),main='Histogram of temperature in different seasons in Tainan in 2011')
