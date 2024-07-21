#########################
# Set Working Directory #
#########################

# Get your current working directory #

setwd("/Users/huangweiting/coding/INTRODUCTION TO SCIENTIFIC COMPUTING SOFTWARE /C4_ClassData/C4_HW")
getwd()

dataset1<-read.csv("C4_1_HW.csv") 
dataset2<-read.csv("C4_2_HW.csv")
str(dataset1)   #Check the variable format
View(dataset1)		#Check Dataset
dim(dataset1)

###################
#      T-test     #
###################
#install.packages("lattice")
#library("lattice")
#densityplot( ~ 材積 ,data=dataset1,main="probability density")
#histogram(x= ~材積,data =dataset1, xlab ="材積" ,ylab = "count" ,type = "count")
## One-sample t-test##
#read dataset1
t.test(dataset1$材積, mu =20) 

##Two-sample paired T-test
#read dataset2
t.test(dataset2$A飼料體重增重, dataset2$B飼料體重增重,paired=TRUE)



