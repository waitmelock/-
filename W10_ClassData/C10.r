#�@Week 12 2020/05/21 Thursday
#�@Linear regression
########################
#�@Example 1 
########################
#fertilizer: 0, 0.5, 1.0, 1.5, 2.0, 2.5
#yield: 10, 18, 32, 48, 55, 62
########################
#�@Create example table
Ex1 <- data.frame(fertilizer=c(0, 0.5, 1.0, 1.5, 2.0, 2.5), 
			yield=c(10, 18, 32, 48, 55, 62)) #Column name = value ���W��: �ƭ�

Ex1 # view Ex1 �T�{Ex1���e

Ex1_lm <- lm(yield ~ fertilizer, data=Ex1) #�u�ʰj�k�ҫ����lm(y ~ x1 + x2 + ... + xn, data = database)

names(Ex1_lm) #�˵�model���t��T�W��
# [1] "coefficients"   "residuals"     "effects"       "rank"         
# [5] "fitted.values" "assign"        "qr"            "df.residual"  
# [9] "xlevels"       "call"          "terms"         "model"     

summary(Ex1_lm) #�˵�model���n

Ex1_lm$residuals #�d��model�ݮt

Ex1_predicted <- predict(Ex1_lm, Ex1[1]) #�p������ǥ�model�����X��yield: predict(model, database[column number])

rd <- data.frame(realY = Ex1$yield, predictedY = Ex1_predicted, residuals = Ex1$yield-Ex1_predicted) #�O���u��Y�ȡB����Y�Ȥδݮt

View(rd) #�i�ܯu��Y�ȡB����Y�Ȥδݮt���G

confint(Ex1_lm) #show �]'s 95%CI

########################
#�@Exercise 1 
########################
#age: 34,39,44,46,48,51,53,60,61,65,66,67
#cholesterol: 141.4,180.5,178.4,212.0,203.2,224.1,186.0,350.0,286.3,287.6,330.3,371.3
########################
#�@Create example table
Exe1 <- data.frame(age=c(34,39,44,46,48,51,53,60,61,65,66,67), 
			cholesterol=c(141.4,180.5,178.4,212.0,203.2,224.1,186.0,350.0,286.3,287.6,330.3,371.3)) #Column name = value ���W��: �ƭ�

Exe1 # view Exe1

Exe1_lm <- lm(cholesterol~ age, data=Exe1) #�u�ʰj�k�ҫ����

summary(Exe1_lm) #�˵�model���n


########################
#�@Example 2 
########################
#fertilizer: 0, 0.5, 1.0, 1.5, 2.0, 2.5
#yield: 10, 18, 32, 48, 55, 62
########################
#�@Create example table
Ex2 <- data.frame(fertilizer=c(0, 0.5, 1.0, 1.5, 2.0, 2.5), 
			yield=c(10, 18, 32, 48, 55, 62)) #Column name = value ���W��: �ƭ�

Ex2 # view Ex2 �T�{Ex2���e

Ex2_lm <- lm(yield ~ fertilizer, data=Ex2) #�u�ʰj�k�ҫ����lm(y ~ x1 + x2 + ... + xn, data = database)
summary(Ex2_lm) #�˵�model���n

#�ܤ���R(ANOVA)#��LANOVA��k�аѦҲĤC�g�ҵ{
anova(lm(yield~fertilizer, data=Ex2))  
confint(Ex2_lm) #show �]'s 95%CI

########################
#�@Exercise 3 
########################
#age: 34,39,44,46,48,51,53,60,61,65,66,67
#cholesterol: 141.4,180.5,178.4,212.0,203.2,224.1,186.0,350.0,286.3,287.6,330.3,371.3
########################
#�@Create example table
Exe3 <- data.frame(age=c(34,39,44,46,48,51,53,60,61,65,66,67), 
			cholesterol=c(141.4,180.5,178.4,212.0,203.2,224.1,186.0,350.0,286.3,287.6,330.3,371.3)) #Column name = value ���W��: �ƭ�

Exe3 # view Exe3

Exe3_lm <- lm(cholesterol~ age, data=Exe3) #�u�ʰj�k�ҫ����

summary(Exe3_lm) #�˵�model���n

confint(Exe3_lm) #show �]'s 95%CI


########################
#�@Exercise 4 
########################
#�s�� 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16
#���Ǥ��� 35,45,58,43,51,24,49,66,37,40,19,26,54,38,36,38
#���A�Ǥ��� 56,63,91,77,86,51,89,84,49,81,44,69,81,60,56,75
########################
#�@Create example table
Exe4 <- data.frame(�s��=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), 
			���Ǥ���=c(35,45,58,43,51,24,49,66,37,40,19,26,54,38,36,38),
			���A�Ǥ���=c(56,63,91,77,86,51,89,84,49,81,44,69,81,60,56,75))

Exe4 # view Exe4

Exe4_lm <- lm(���Ǥ��� ~ ���A�Ǥ���, data=Exe4) #�u�ʰj�k�ҫ����

anova(lm(���Ǥ��� ~ ���A�Ǥ���, data=Exe4)) #ANOVA for �U�O�ܼ�

confint(Exe4_lm) #show �]'s 95%CI

summary(Exe4_lm) #�˵�model���n


########################
#�@Example 3 
########################
#�B�ʮɼ�=0,0,0,2,2,2,2,4,4,4
#��������q=2,4,6,2,4,6,8,4,6,8
#�魫��q=6,2,4,8,6,8,5,11,13,12
########################
#�@Create example table
Ex5 <- data.frame(�B�ʮɼ�=c(0,0,0,2,2,2,2,4,4,4), 
			��������q=c(2,4,6,2,4,6,8,4,6,8),
			�魫��q=c(6,2,4,8,6,8,5,11,13,12))

Ex5 # view Ex5

#install.packages("Hmisc")
library("Hmisc")
#�z�L�������˩w�T�{�U���ܼƶ������p
rcorr(as.matrix(Ex5 ), type=c("pearson"))

Ex5_lm <- lm(�魫��q ~ �B�ʮɼ� + ��������q, data=Ex5) #�u�ʰj�k�ҫ����

summary(Ex5_lm) #�˵�model���n

####### Adj R2 #########
#r2 = 0.8433 #��l�M�w�Y�� R square
#k = 2 #�ܼƼƶq
#n = 10 #�˥���
#p = k + 1 

#Radj = 1-(n-1)/(n-p)*(1-r2) 
#Radj 
#####################################
####### ��۩��˩w(����ҫ��t�A��) #########
##��lanova()�L�k�i�ܾ���ҫ��t�A�סA�ȯ�i�ܦU�O�ܼƤ��t�A�סA�]�����anova_alt()
##�Ĥ@���ϥ�anova_alt()�йw������̤U�����Anova for full model fitting���w�q��

#anova_alt(lm(�魫��q ~ �B�ʮɼ� + ��������q, data=Ex5)) #ANOVA for ����ҫ�
anova_alt(Ex5_lm) #ANOVA for ����ҫ�

#####################################
####### �ӧO�j�k�Y����۩��˩w ##########
summary(Ex5_lm) #�˵�model���n
#####################################

########################
#�@Exercise 6 
########################
#�����ū�=25,28,30,26,24,28,29,27,32,26,27,28
#���=8.6,9,8.7,8.5,7,7.8,10,9.2,8.8,7.5,7.2,8.9
#�}��=13,12.5,11.8,10.4,13.5,9.9,9.5,13.2,10,12.8,11.4,10.9
########################
#�@Create example table
Exe6 <- data.frame(Temperature=c(25,28,30,26,24,28,29,27,32,26,27,28),
		Daylight=c(8.6,9,8.7,8.5,7,7.8,10,9.2,8.8,7.5,7.2,8.9),
		Sugar=c(13,12.5,11.8,10.4,13.5,9.9,9.5,13.2,10,12.8,11.4,10.9))

Exe6 # view Exe6

summary(Exe6) # �i��²���y�z�ʲέp

#library("Hmisc")
#�z�L�������˩w�T�{�U���ܼƶ������p
rcorr(as.matrix(Exe6), type=c("pearson"))

Exe6_lm <- lm(Sugar ~ Temperature + Daylight, data=Exe6) #�u�ʰj�k�ҫ����

summary(Exe6_lm) #�˵�model���n

####### ��۩��˩w(����ҫ��t�A��) #########
#��lanova()�L�k�i�ܾ���ҫ��t�A�סA�ȯ�i�ܦU�O�ܼƤ��t�A�סA�]�����anova_alt()
#�Ĥ@���ϥ�anova_alt()�йw������̤U�����Anova for full model fitting���w�q��
#summary(model)���̤U��]������²�������ҫ�����t�A�׵��G
anova_alt(Exe6_lm) #ANOVA for ����ҫ�

####### �ӧO�j�k�Y����۩��˩w ##########
summary(Exe6_lm) #�˵��ӧO�j�k�Y����۩��˩w���G

########################
#�@Practice 1
########################



########################
#�@Practice 2 
########################




#Appendix
###########################
#�`�δy�z�ʲέp
###########################
#mean() 		������
#median() 		�����
#range() 		���Z
#quantile() 	�|�����
#IQR() 		�|����t
#summary() 		�y�z�έp�K�n
#sd() 		�зǮt
#var() 		�ܲ��� 
#skewness() 	����
#kurtosis() 	�p��
###########################
###########################
#�������˩w
###########################
#cor.test(Exe1$age, Exe1$cholesterol, method="pearson")
#
#library("Hmisc")
#����Ƽ��ܼƤ����������ʤ��R
#rcorr(as.matrix(Ex2), type=c("pearson"))
#install.packages("ppcor")
#library("ppcor")
#�Ҷq�ū׫ᤧ�B�q�P���q���b����
#pcor.test(Ex2$rain, Ex2$yield, Ex2$temp, method = "pearson")
###########################
###########################
#deal with collinearity
###########################
#http://www.sthda.com/english/articles/39-regression-model-diagnostics/160-multicollinearity-essentials-and-vif-in-r/
#install.packages("car")
#library("car")
#vif(model)
#vif(Exe6_lm)

############################
############################
#Anova for full model fitting
############################
#(source: https://community.rstudio.com/t/anova-table-for-full-linear-model/42074/10 created by RussS)
############################Definition function
anova_alt = function (object, reg_collapse=TRUE,...) 
{
  if (length(list(object, ...)) > 1L) 
    return(anova.lmlist(object, ...))
  if (!inherits(object, "lm")) 
    warning("calling anova.lm(<fake-lm-object>) ...")
  w <- object$weights
  ssr <- sum(if (is.null(w)) object$residuals^2 else w * object$residuals^2)
  mss <- sum(if (is.null(w)) object$fitted.values^2 else w * 
               object$fitted.values^2)
  if (ssr < 1e-10 * mss) 
    warning("ANOVA F-tests on an essentially perfect fit are unreliable")
  dfr <- df.residual(object)
  p <- object$rank
  if (p > 0L) {
    p1 <- 1L:p
    comp <- object$effects[p1]
    asgn <- object$assign[stats:::qr.lm(object)$pivot][p1]
    nmeffects <- c("(Intercept)", attr(object$terms, "term.labels"))
    tlabels <- nmeffects[1 + unique(asgn)]
    ss <- c(vapply(split(comp^2, asgn), sum, 1), ssr)
    df <- c(lengths(split(asgn, asgn)), dfr)
    if(reg_collapse){
      if(attr(object$terms, "intercept")){
        collapse_p<-2:(length(ss)-1)
        ss<-c(ss[1],sum(ss[collapse_p]),ss[length(ss)])
        df<-c(df[1],sum(df[collapse_p]),df[length(df)])
        tlabels<-c(tlabels[1],"Source")
      } else{
        collapse_p<-1:(length(ss)-1)
        ss<-c(sum(ss[collapse_p]),ss[length(ss)])
        df<-c(df[1],sum(df[collapse_p]),df[length(df)])
        tlabels<-c("Regression")
      }
    }
  }else {
    ss <- ssr
    df <- dfr
    tlabels <- character()
    if(reg_collapse){
      collapse_p<-1:(length(ss)-1)
      ss<-c(sum(ss[collapse_p]),ss[length(ss)])
      df<-c(df[1],sum(df[collapse_p]),df[length(df)])
    }
  }
  
  ms <- ss/df
  f <- ms/(ssr/dfr)
  P <- pf(f, df, dfr, lower.tail = FALSE)
  table <- data.frame(df, ss, ms, f, P)
  table <- rbind(table, 
                 colSums(table))
  if (attr(object$terms, "intercept")){
   table$ss[nrow(table)]<- table$ss[nrow(table)] - table$ss[1]
  }
  table$ms[nrow(table)]<-table$ss[nrow(table)]/table$df[nrow(table)]
  table[length(P):(length(P)+1), 4:5] <- NA
  dimnames(table) <- list(c(tlabels, "Error","Total"), 
                          c("Df","SS", "MS", "F", 
                            "P"))
  if (attr(object$terms, "intercept")){
    table <- table[-1, ]
    table$MS[nrow(table)]<-table$MS[nrow(table)]*(table$Df[nrow(table)])/(table$Df[nrow(table)]-1)
    table$Df[nrow(table)]<-table$Df[nrow(table)]-1
  }
  structure(table, heading = c("Analysis of Variance Table\n"), 
            class = c("anova", "data.frame"))
}

