#INST314 / PROJECT 3 / JULIAN VAZQUEZ
#LAST REVISED DATE: 04/21/2019

load(file.choose())

#Examining data
str(gss$EDUC)
str(gss$CHILDS)
levels(gss$EDUC)
levels(gss$CHILDS)
summary(gss$EDUC)
summary(gss$CHILDS)

library(summarytools)
descr(as.numeric(as.character(gss$CHILDS)))
descr(as.numeric(as.character(gss$EDUC)))
summary(as.numeric(as.character(gss$EDUC)))
sd(as.numeric(as.character(gss$EDUC)), na.rm= T)

#Recoding No. of Children variable
library(car)
gss$CHILDS.r = gss$CHILDS
gss$CHILDS.r = car::recode(gss$CHILDS.r, "'0'=0; '1'=1; '2'=2;'3'=3; '4'=4; '5'=5; '6'=6; '7'=7; 'EIGHT OR MORE'=8")
gss$CHILDS.r = as.numeric(as.character(gss$CHILDS.r))
str(gss$CHILDS.r)
table(gss$CHILDS.r) #checking recode
summary(gss$CHILDS.r)
descr(gss$CHILDS.r)
sd(gss$CHILDS.r, na.rm= T)

gss$EDUC.num = as.numeric(as.character(gss$EDUC)) #copying EDUC as numeric

#Correlation test
cor.test(gss$EDUC.num, gss$CHILDS.r) # -0.23

#OLS Regression
summary(lm(gss$CHILDS.r ~ gss$EDUC.num))

#Scatterplot
plot(jitter(gss$CHILDS.r) ~ jitter(gss$EDUC.num),
     col= "red", xlab= "Years of Education", ylab= "No. of Children", 
     main= "Correlation Plot\n Years of Education and Number of Children", cex= .5, pch= 1)

abline(lm(gss$CHILDS.r ~ gss$EDUC.num),col= "black", lty= 5, lwd= 2)

#Diagnostic plots (sample is random and independent)
par(mfrow=c(2,2))
plot(lm(gss$CHILDS.r ~ gss$EDUC.num))
par(mfrow=c(1,1))
