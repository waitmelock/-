setwd("/Users/huangweiting/co ding/INTRODUCTION TO SCIENTIFIC COMPUTING SOFTWARE /W16")
install.packages("ResourceSelection") # For Hosmer & Lemeshow Test
install.packages("caret") # For Example Database 
library(ResourceSelection)
data(Titanic.csv, package = "caret")
ex1 <- data

View(ex1)

#?d??: ?s?ӽФH?H?ε??????T
#Class	(Good/Bad)	???X?????H?Φn/???n
#Age	?~??
#ForeignWorker	(1/0)	?O?_???~?y?Ҥu
#Property.RealEstate	(1/0)	???L?Цa??
#Housing.Own	(1/0)	???L?ۤv???Фl
#CreditHistory.Critical	(1/0)	???L?H?αb??
#???L??

#Generalized linear model  #glm(y ~ x, family = data distribution (poission/gaussian/binomial... etc.), data = database)
mod <- glm(Class ~ Age + ForeignWorker + Property.RealEstate 
		+ Housing.Own + CreditHistory.Critical, family="binomial", data = ex1)

summary(mod)

hoslem.test(mod$y, fitted(mod)) #hoslem.test(model$y, fitted(model))
?hoslem.test

#Calculate Odds ratio
exp(coef(mod))
#?i?H??View???ƥO???H????????
View(exp(coef(mod)))

#Get 95% Confidence Interval
confint(mod)	#95% CI of Coefficient
exp(confint(mod))	
View(exp(confint(mod)))



########################
#?@Example 2 - variable selected logistic regression
########################
data(GermanCredit, package = "caret")
ex2 <- GermanCredit

View(ex2)

mod.null <- glm(Class ~ 1, family="binomial", data = ex2)
mod.full <- glm(Class ~ ., family="binomial", data = ex2)

f.model = step(mod.null, scope =list(lower=mod.null, upper=mod.full), direction = "forward", trace = 1)

summary(f.model)

#b.model = step(mod.full, direction = "backward", trace = 1)
#summary(b.model)

########################
#?@Practice - Backward logistic regression
########################





#Appendix
########################
# ???L Logistic model ?Y?????۩ʴ???
########################
#https://www.r-bloggers.com/evaluating-logistic-regression-models/
########################
#Omnibus6
anova(mod, test ="Chisq")

########################
#Cox & Snell R ??????, Nagelkerke R ??????
install.packages("pscl")
library(pscl)

pR2(mod)  # look for 'McFadden'

########################
#Hosmer ?M Lemeshow
install.packages("ResourceSelection")
library(ResourceSelection)

hoslem.test(mod$y, fitted(mod), g=10)

########################
#Wald ??
install.packages("survey")
library(survey)

regTermTest(mod, "ForeignWorker")

