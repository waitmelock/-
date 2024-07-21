install.packages("vcd") 
library(vcd)
########################
#Q1
Q1 <- data.frame(row.names=c("Frequency"), 藍=45 , 綠=55 , 紅=17)
Q1
chisq.test(Q1,p=c(1/2,3/8,1/8),simulate.p.value = FALSE)

#Q2
Q2 <-data.frame(row.names = c("痊癒","未痊癒"),A=c(5,7),B=c(2,6))
Q2
chisq.test(Q2)
independence_table(data.matrix(Q2))
fisher.test(Q2)
