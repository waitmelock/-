#�@Week 11
#�@Variable selection and collinearity test
########################
### Import packages ####

install.packages("descriptr")
install.packages("olsrr")		# advancd way to do stepwise regression
install.packages("Hmisc")
install.packages("MASS")

library(descriptr)
library(olsrr)
library(Hmisc)
library(MASS)

#https://cran.r-project.org/web/packages/olsrr/readme/README.html
#https://cran.r-project.org/web/packages/olsrr/vignettes/intro.html




############################################
#  R Script �V olsrr                        
#  further summary of lm by olsrr package  #
############################################
#fertilizer: 0, 0.5, 1.0, 1.5, 2.0, 2.5
#yield: 10, 18, 32, 48, 55, 62
############################################
#�@Create example table
f1 <- data.frame(fertilizer = c(0, 0.5, 1.0, 1.5, 2.0, 2.5), 
			yield = c(10, 18, 32, 48, 55, 62))
   # Column name = value ���W��: �ƭ�
f1 # view f1 �T�{f1���e

ols_regress(yield ~ fertilizer, data = f1) #�z�Lolsrr package��ols_regress��ƫإ߽u�ʼҫ����˵���Ժɪ�model���n




#############################
#�@Example 1 - Collinearity 
###########################################################################
#�@���J��=17.9,16.1,15.7,19.3,15.9,15,18.5,15.7,19.6,20.1,21.2,17
#�饭�����=25.8,24.3,23.5,26.2,24.9,23.8,25.8,24.2,26.5,25.4,26.6,24.3
#��Ӯɼ�=5.5,4.9,5.2,7.8,5.6,4.5,7.5,5.6,6.6,7.4,8.2,5.2
#�������q=6104,5331,5566,6644,5617,5026,6293,5478,6583,6217,6894,5906
############################################################################
ex1 <- data.frame(�@���J�� = c(17.9,16.1,15.7,19.3,15.9,15,18.5,15.7,19.6,20.1,21.2,17),
		�饭����� = c(25.8,24.3,23.5,26.2,24.9,23.8,25.8,24.2,26.5,25.4,26.6,24.3),
		��Ӯɼ� = c(5.5,4.9,5.2,7.8,5.6,4.5,7.5,5.6,6.6,7.4,8.2,5.2),
		�������q = c(6104,5331,5566,6644,5617,5026,6293,5478,6583,6217,6894,5906)) 
View(ex1)

# �z�L�������˩w�˵��U���ܼƶ������p
rcorr(as.matrix(ex1), type = c("pearson"))

ex1_model <- lm(�������q ~ �@���J�� + �饭����� + ��Ӯɼ�, data = ex1)
# �Q��olsrr��Ʈi�ܦ@�u���˩w���G
ols_coll_diag(ex1_model) 

############################
#�@Exercise 1 - Collinearity
############################################################
#�����ū�=25,28,30,26,24,28,29,27,32,26,27,28
#���=8.6,9,8.7,8.5,7,7.8,10,9.2,8.8,7.5,7.2,8.9
#�}��=13,12.5,11.8,10.4,13.5,9.9,9.5,13.2,10,12.8,11.4,10.9
############################################################
exe1 <- data.frame(�����ū� = c(25,28,30,26,24,28,29,27,32,26,27,28),
		��� = c(8.6,9,8.7,8.5,7,7.8,10,9.2,8.8,7.5,7.2,8.9),
		�}�� = c(13,12.5,11.8,10.4,13.5,9.9,9.5,13.2,10,12.8,11.4,10.9))
View(exe1)

rcorr(as.matrix(exe1), type=c("pearson"))
exe1_model <- lm(�}�� ~ �����ū� + ���, data=exe1)
ols_coll_diag(exe1_model)

########################
#�@Example 2
########################

data(birthwt, package = "MASS") #�ե�MASS�Ҳմ��Ѫ��d����birthwt
View(birthwt)

#source: https://www.rdocumentation.org/packages/MASS/versions/7.3-50/topics/birthwt
#Format
#This data frame contains the following columns:
#low	 �s�ͨ�X�ͮ��魫�p��2.5����(Low)
#    indicator of birth weight less than 2.5 kg.
#age	 �s�ͨ���˥Ͳ��~��(MAge)
#    mother's age in years.
#lwt	 �s�ͨ���˲��e�魫(MWt)
#    mother's weight in pounds at last menstrual period.
#race	 �s�ͨ���˱ڸ�(MRace)
#    mother's race (1 = white, 2 = black, 3 = other).
#smoke �s�ͨ�����h���ɬO�_�l��(MSmoke)
#    smoking status during pregnancy.
#ptl	 �s�ͨ���˹L�h��������(PreLabours)
#    number of previous premature labours.
#ht	 �s�ͨ�a�ڦ��L�������f�v(HPT)
#    history of hypertension.
#ui	 �s�ͨ���˦��L�l�c��ļ�g(Uterine_irr.)
#    presence of uterine irritability.
#ftv	 �s�ͨಣ��T�Ӥ뤺�N�妸��(FTV)
#    number of physician visits during the first trimester.
#bwt	 �s�ͨ��魫(BWt)
#    birth weight in grams.

ex2 <- birthwt
colnames(ex2) <- c("Low","MAge","MWt","MRace",
			"MSmoke","PreLabours","HPT",
			"Uterine_irr.","FTV","BWt")
View(ex2)

rcorr(as.matrix(ex2), type=c("pearson"))

#ols_step_forward_p(model, penter = 0.05) #penter(p enter): p value; variables with p value less than penter will enter into the model
#ols_step_backward_p(model, prem = 0.1) #prem(p remove): p value; variables with p more than prem will be removed from the model.
#ols_step_both_p(model, penter = 0.05, prem = 0.1)

############
# �j���J�k #
############
ex2_model <- lm(BWt ~ MAge + MWt + as.factor(MRace) + as.factor(MSmoke) + PreLabours + as.factor(HPT) + as.factor(Uterine_irr.) + FTV, data=ex2)
ols_regress(ex2_model)

############
# ���V�i�J�k #
############
ex2_model <- lm(BWt ~ MAge + MWt + as.factor(MRace) + as.factor(MSmoke) + PreLabours + as.factor(HPT) + as.factor(Uterine_irr.) + FTV, data=ex2)
ols_step_forward_p(ex2_model, penter = 0.05, details = TRUE)

############
# �ϦV�^�O�k #
############
ex2_model <- lm(BWt ~ MAge + MWt + as.factor(MRace) + as.factor(MSmoke) + PreLabours + as.factor(HPT) + as.factor(Uterine_irr.) + FTV, data=ex2)
ols_step_backward_p(ex2_model, prem = 0.1, details = TRUE)

############
# �v�B���R�k #
############
ex2_model <- lm(BWt ~ MAge + MWt + as.factor(MRace) + as.factor(MSmoke) + PreLabours + as.factor(HPT) + as.factor(Uterine_irr.) + FTV, data=ex2)
ols_step_both_p(ex2_model, penter = 0.05, prem = 0.1, details = TRUE)



########################
#�@Example 2
########################################################################
#�@���J��=17.9,16.1,15.7,19.3,15.9,15,18.5,15.7,19.6,20.1,21.2,17
#�饭�����=25.8,24.3,23.5,26.2,24.9,23.8,25.8,24.2,26.5,25.4,26.6,24.3
#��Ӯɼ�=5.5,4.9,5.2,7.8,5.6,4.5,7.5,5.6,6.6,7.4,8.2,5.2
#�������q=6104,5331,5566,6644,5617,5026,6293,5478,6583,6217,6894,5906
########################################################################
exe2 <- data.frame(�@���J��=c(17.9,16.1,15.7,19.3,15.9,15,18.5,15.7,19.6,20.1,21.2,17),
		�饭�����=c(25.8,24.3,23.5,26.2,24.9,23.8,25.8,24.2,26.5,25.4,26.6,24.3),
		��Ӯɼ�=c(5.5,4.9,5.2,7.8,5.6,4.5,7.5,5.6,6.6,7.4,8.2,5.2),
		�������q=c(6104,5331,5566,6644,5617,5026,6293,5478,6583,6217,6894,5906)) 
View(exe2)
rcorr(as.matrix(exe2), type=c("pearson"))

#�j���J�k
exe2_model <- lm(�������q ~ �@���J�� + �饭����� + ��Ӯɼ�, data=exe2)
ols_regress(exe2_model)

#���V�i�J�k
exe2_model <- lm(�������q ~ �@���J�� + �饭����� + ��Ӯɼ�, data=exe2)
ols_step_forward_p(exe2_model, penter = 0.05, details = TRUE)

#�ϦV�^�O�k
#exe2_model <- lm(�������q ~ �@���J�� + �饭����� + ��Ӯɼ�, data=exe2)
#ols_step_backward_p(exe2_model, prem = 0.1, details = TRUE)

#�v�B���R�k
exe2_model <- lm(�������q ~ �@���J�� + �饭����� + ��Ӯɼ�, data=exe2)
ols_step_both_p(exe2_model, penter = 0.05, prem = 0.1, details = TRUE)





########################
#�@Practice 1
########################
install.packages("MASS") 
data(road, package = "MASS")
practice_data <- road
View(practice_data)



#Appendix
############################
#�`�δy�z�ʲέp
############################
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
############################
#��L�ѦҸ��
############################
#https://www.rdocumentation.org/packages/lmSupport/versions/1.05
#https://cran.r-project.org/web/packages/lmSupport/index.html
#https://cran.r-project.org/web/packages/lmSupport/lmSupport.pdf

#https://cran.r-project.org/web/packages/pubh/vignettes/regression.html

#Stepwise by olsrr
#https://www.guru99.com/r-simple-multiple-linear-regression.html#8

#Stepwise by leaps package
#http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/

#Stepwise by lm() function
#http://jackthisisamazing.blogspot.com/2016/12/rstepwise-regression_11.html

