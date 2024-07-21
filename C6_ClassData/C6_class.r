#########################
# Set Working Directory #
#########################

# Get your current working directory #

setwd("/Users/huangweiting/coding/INTRODUCTION TO SCIENTIFIC COMPUTING SOFTWARE /C6_ClassData/HW")
getwd()

data1<-read.csv("C6_HW1.csv") 
data2<-read.csv("C6_HW2.csv") 
str(data1)   #Check the variable format
View(data1)		#Check Dataset
str(data2)   #Check the variable format
View(data2)		#Check Dataset
#dim(data1)		#Check Dataset (how many observations and variables)

#Q1      
aov1 <- aov(Yield~ factor(Brand_ID), data=data1) #factor() for categorical variable
summary(aov1)#Check p-value 

#Q2    
aov2 <- aov(Speed~ factor(Train_ID), data=data2) #factor() for categorical variable
summary(aov2)#Check p-value 

#Test for Homogeneity of Variance 
#install.packages("car")                          
library(car)
leveneTest(data2$Speed, data2$Train_ID, center=mean)

##Variation equal-ANOVA post-hoc test##
install.packages("DescTools") 
library(DescTools)
PostHocTest(aov2, method = "duncan")
