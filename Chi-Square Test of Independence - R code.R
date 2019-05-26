#INST314 PROJECT 1 / JULIAN VAZQUEZ & COLIN BRADLEY / DATE: 02/28/2019

vg.complete <- read.csv(file.choose())
str(vg.complete)
View(vg.complete)
vg.complete$Year <- as.numeric(as.character(vg.complete$Year))
summary(vg.complete$Year) #Possible limitation: oudated data

#Created a new data frame with only relevant variables
vg <- data.frame(vg.complete$Name, vg.complete$Year, 
                 vg.complete$Genre, vg.complete$Publisher)
names(vg) <- c("Name", "Year", "Genre", "Publisher")
View(vg)

library(dplyr)
(distinct_genre = vg %>% distinct(Genre))
(distinct_publ = vg %>% distinct(Publisher))

#Total sums of games classified as "Fighting", "Action" or "Shooter"
#for each company
sum.nitendo <- sum(sum(vg$Publisher=="Nintendo" & vg$Genre=="Fighting") + 
               sum(vg$Publisher=="Nintendo" & vg$Genre=="Action") +
               sum(vg$Publisher=="SNintendo" & vg$Genre=="Shooter"))
sum.sce <- sum(sum(vg$Publisher=="Sony Computer Entertainment" & vg$Genre=="Fighting") + 
           sum(vg$Publisher=="Sony Computer Entertainment" & vg$Genre=="Action") +
           sum(vg$Publisher=="Sony Computer Entertainment" & vg$Genre=="Shooter"))
sum.activision <- sum(sum(vg$Publisher=="Activision" & vg$Genre=="Fighting") + 
                  sum(vg$Publisher=="Activision" & vg$Genre=="Action") +
                  sum(vg$Publisher=="Activision" & vg$Genre=="Shooter"))
sum.mgs <- sum(sum(vg$Publisher=="Microsoft Game Studios" & vg$Genre=="Fighting") + 
           sum(vg$Publisher=="Microsoft Game Studios" & vg$Genre=="Action") +
           sum(vg$Publisher=="Microsoft Game Studios" & vg$Genre=="Shooter"))

#Sums by Genre
sum.fighting <- sum(vg.table$Fighting)
sum.action <- sum(vg.table$Action)
sum.shooter <- sum(vg.table$Shooter)

#Creates a table for Chi-sQuare testing. Data taken from sums above
vg.table <- data.frame("Fighting"= c(18, 7, 5, 30), 
                        "Action"= c(79, 310, 21, 90),
                        "Shooter"= c(0, 159, 28, 51))
row.names(vg.table) <- c("Nintendo", "Activision",
                         "Microsoft Game Studios", "Sony Computer Entertainment")
vg.table

#Row & Column Percentages
(row.perc <- round(vg.table/rowSums(vg.table),3))

col.perc.fight <- vg.table[,1]/sum.fighting
col.perc.action <- vg.table[,2]/sum.action
col.perc.shooter <- vg.table[,3]/sum.shooter

#Chi-Square Test for Independence
chi.vg <- chisq.test(vg.table)
attributes(chi.vg)
chi.vg$parameter # 6 df
chi.vg$statistic # X^2 value: 115.25
chi.vg$p.value # p-value < .05 

#Chi-Square Effect Size
install.packages("DescTools")
library(DescTools)
DescTools::CramerV(vg.table)
