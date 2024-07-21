install.packages("lattice")
install.packages("nortest")     #Lilliefors(Kolmogorov-Smirnov) Normality Test
install.packages("car") 

#########################
# Set Working Directory #
#########################

# Get your current working directory #
setwd("/Users/huangweiting/coding/INTRODUCTION TO SCIENTIFIC COMPUTING SOFTWARE /W5_ClassData/HW")
getwd()
data1 <- read.csv("C5_Q1.csv")  
data2 <- read.csv("C5_Q2.csv")
str(data1)   #Check the variable format
View(data1)		#Check Dataset
str(data2)   #Check the variable format
View(data2)		#Check Dataset
#dim(data1)		#Check Dataset (how many observations and variables)
#########
#Q1
shapiro.test(data1$增重[data1$飼料=="1"])  
shapiro.test(data1$增重[data1$飼料=="2"])
bartlett.test(增重~ 飼料, data=data1) 
t.test(data1$增重[data1$飼料=="1"], data1$增重[data1$飼料=="2"], var.equal=TRUE)

#Q2
shapiro.test(data2$看病次數) 
#The Shapiro-Wilk Normality Test (sample size<50)
data2$看病次數_Change <- sqrt(data2$看病次數)
shapiro.test(data2$看病次數_Change)
library("lattice")
hist(data2$看病次數,main="The distribution histogram of the number of visits to 49 passers-by in a year in the hospital",xlab="Number of visits",breaks = 20)
hist(data2$看病次數_Change,main="distribution histogram of the number of visits to 49 passers-by in a year in the hospital/sqrt version/",xlab="Number of visits",breaks = 20)
#########
