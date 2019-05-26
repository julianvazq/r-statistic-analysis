#INST314 PROJECT 2 / JULIAN VAZQUEZ & MICHAEL GREGORY
#LAST REVISED DATE: 03/28/2019

avo = read.csv(file.choose())
str(avo)

#MANUALLY CHECKING DATA FOR YEARS 2015 & 2018 FOR ABNORMALITIES
View(avo$Date[avo$year==2015])
View(avo$Date[avo$year==2018])

#SAMPLE 1 (2015)
price.2015 = avo$AveragePrice[avo$year=="2015"]
sum(!is.na(price.2015))
summary(price.2015)
sd(price.2015)

#SAMPLE 2 (2018)
price.2018 = avo$AveragePrice[avo$year=="2018"]
sum(!is.na(price.2018))
summary(price.2018)
sd(price.2018)

#WELCH TWO SAMPLE T-TEST
t.test(price.2015, price.2018, alternative= "greater", pooled= T, paired= F)
library(lsr)
cohensD(price.2015, price.2018)  #effect size
?t.test

#BOXPLOT 
#could've created a bar chart of the means
#but I think a boxplot provides a better visualization 
#of the two different samples even if it doesn't show the means
boxplot(price.2015, price.2018,
        main= "Avocado price per unit in 2015 vs 2018",
        names= c("2015", "2018"),
        col= c("green", "yellow"),
        border= "dark green")


