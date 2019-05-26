#INST314 / PROJECT 4 / JULIAN VAZQUEZ
#LAST REVISED DATE: 05/14/2019

load(file.choose()) #GSS
library(summarytools)
library(car)

# gss$RHIINC ("My income is high")
summary(gss$RHIINC)
levels(gss$RHIINC)
gss$inc.high = as.numeric(gss$RHIINC)
summary(gss$inc.high) # 1: STRONGLY AGREE, 5: STRONGLY DISAGREE
freq(gss$RHIINC)

# gss$POLVIEWS 
summary(gss$POLVIEWS)
levels(gss$POLVIEWS)
gss$pol.num = as.numeric(gss$POLVIEWS) #1: EXTREM LIBERAL 7: EXTREM CONSERVATIVE
summary(gss$pol.num)
freq(gss$POLVIEWS)

# gss$race5 (reference group: white)
# credit to Prof. Janzen for this recoding work 
gss$race5 <- recode(as.numeric(gss$RACECEN1), "1=1; 2=2; 3=5; 4:10=4; 11:15=5; 16=3")
gss$race5 <- factor(gss$race5, levels = c(1,2,3,4,5), 
                    labels = c("white", "black", "hispanic", "asian", "other"))
summary(gss$race5)
freq(gss$race5)

#Examining No. of Children and Education variables
str(gss$EDUC)
str(gss$CHILDS)
levels(gss$EDUC)
levels(gss$CHILDS)
summary(gss$EDUC)
summary(gss$CHILDS)

descr(as.numeric(as.character(gss$CHILDS)))
descr(as.numeric(as.character(gss$EDUC)))
summary(as.numeric(as.character(gss$EDUC)))
sd(as.numeric(as.character(gss$EDUC)), na.rm= T)

#Recoding No. of Children variable
gss$CHILDS.r = gss$CHILDS
gss$CHILDS.r = car::recode(gss$CHILDS.r, "'0'=0; '1'=1; '2'=2;'3'=3; '4'=4; '5'=5; '6'=6; '7'=7; 'EIGHT OR MORE'=8")
gss$CHILDS.r = as.numeric(as.character(gss$CHILDS.r))
str(gss$CHILDS.r)
table(gss$CHILDS.r) #checking recode
summary(gss$CHILDS.r)
descr(gss$CHILDS.r)
sd(gss$CHILDS.r, na.rm= T)
gss$EDUC.num = as.numeric(as.character(gss$EDUC)) #copying EDUC as numeric

#Correlation test (no multicollinearity issues)
gss$race5.num = as.numeric(gss$race5)
gss.mat = gss[, c("EDUC.num", "pol.num", "inc.high", "race5.num")]
round(cor(gss.mat, use = "complete.obs"), 3)

#OLS Regression
summary(lm(CHILDS.r ~ EDUC.num + inc.high + pol.num + race5, data= gss))
model = lm(CHILDS.r ~ EDUC.num + inc.high + pol.num + race5, data= gss)

#Stargazer
install.packages("stargazer")
library(stargazer)
stargazer(model, type="html", dep.var.labels= "No. of Children", 
          covariate.labels= c("Education",
                              "High Income",
                              "Political Views",
                              "Black",
                              "Hispanic",
                              "Asian",
                              "Other"), out= "model.htm")

#Diagnostic plots (sample is random and independent)
par(mfrow=c(2,2))
plot(lm(CHILDS.r ~ EDUC.num + inc.high + pol.num + race5, data= gss))
par(mfrow=c(1,1))
