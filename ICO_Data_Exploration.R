rm(list=ls())
#Loading the dataset----
ico <- read.csv("LUBS5990M_courseworkData_202223.csv")
str(ico)
summary(ico)
library(dplyr)
#Count number of rows where priceUSD is zero----
nrow(filter(ico, priceUSD == 0))
hist(ico$priceUSD)
histd1 <- hist(ico$priceUSD)
histd1
hist(ico$priceUSD, breaks = 20)
hist(ico$priceUSD, breaks = seq(from = 0, to = 40000, by = 1000))
box1 <- boxplot(ico$priceUSD, main = "Boxplot of feature - priceUSD", 
                ylab = "Price of coin/token in US Dollars")
box1
#Removing one outlier where priceUSD = 39384----
sd(ico$priceUSD, na.rm = TRUE)
ico1 <- filter(ico, priceUSD != 39384)
hist(ico1$priceUSD)
hist(ico1$priceUSD, breaks = 100)
histd2 <- hist(ico1$priceUSD, breaks = 100)
histd2
filter(ico, priceUSD > 2326)
#Analysing teamSize now ----
hist(ico$teamSize)
boxplot(ico$teamSize, main = "Boxplot of feature - teamSize", 
        ylab = "Number of members in a team")
#Analysing coinNum now----
summary(ico$coinNum)
str(ico$coinNum)
format(min(ico$coinNum), scientific = FALSE)
format(max(ico$coinNum), scientific = FALSE)
hist(ico$coinNum)
histd3 <- hist(ico$coinNum)
histd3
format(summary(ico$coinNum), scientific = FALSE)
boxplot(ico$coinNum)
box2 <- boxplot(ico$coinNum, main = "Boxplot of feature - coinNum", 
                ylab = "Number of coins to be issued in ICO campaign")
box2
format(box2$stats, scientific = FALSE)
format(sd(ico$coinNum), scientific = FALSE)
filter(ico, ico$coinNum >  1.290005e+15)
#Analysing distributedPercentage ----
summary(ico)
boxplot(ico$distributedPercentage)
box3 <- boxplot(ico$distributedPercentage, main = "Boxplot of feature - distributedPercentage", 
                ylab = "Proportion of coins to be distributed to investors" )
box3
histd4 <- hist(ico$distributedPercentage)
histd4
#Analysing feature - taSuccess ----
table(ico$success)
prop.table(table(ico$success))*100
#Analysing slogan vector ----
lapply(ico$brandSlogan,nchar)
#Analysing country feature ----
table(ico$countryRegion)
rev(sort(table(ico$countryRegion)))
rev(sort(prop.table(table(ico$countryRegion))*100))
sum(is.na(ico$countryRegion))
table(ico$platform)
rev(sort(table(ico$platform)))
table(ico$platform)
plot(ico[,-c("brandSlogan", "countryRegion", "startDate", "endDate", "platform")])
ico_numeric <- select(ico, -c("ID","brandSlogan", "countryRegion", "startDate", "endDate", "platform")) 
names(aa)
plot(aa)
library(corrgram)
corrgram(ico_numeric, lower.panel=panel.pie, upper.panel=panel.pie, main = "Corrgram for numeric feature only in ico dataframe")
names(ico)
plot(ico$teamSize, ico$rating)
table(ico$country)