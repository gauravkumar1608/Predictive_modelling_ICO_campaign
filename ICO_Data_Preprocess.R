library(dplyr)
rm(list=ls())
ico_main <- read.csv("LUBS5990M_courseworkData_202223.csv", encoding = "UTF-8")
##Task 1 starts : Removing outlier in priceUSD = 39384 
filter(ico_main, priceUSD == 39384)
ico_main <- ico_main[-384,]
filter(ico_main, priceUSD == 39384)
##Task 1 ends 
##Task 2 starts: Imputing missing values in priceUSD
summary(ico_main$priceUSD)
ico_main[is.na(ico_main$priceUSD),"priceUSD"] <- median(ico_main$priceUSD, na.rm =TRUE)
summary(ico_main$priceUSD)
##Task 2 ends
##Task 3 starts: Replacing values where priceUSD is zero 
ico_main$priceUSD[ico_main$priceUSD == 0] <- median(ico_main$priceUSD)
summary(ico_main$priceUSD)
##Task 3 ends
##Task 4  starts: Imputing missing values in teamSize 
summary(ico_main$teamSize)
ico_main[is.na(ico_main$teamSize), "teamSize"] <- median(ico_main$teamSize, na.rm = TRUE)
summary(ico_main$teamSize)
##Task 4 ends 
##Task 5 starts: Removing outlier in coinNum 
summary(ico_main$coinNum)
which(ico_main$coinNum == 22619078416760300) # to find out the row number in a dataframe
ico_main <- ico_main[-1593,]
filter(ico_main, coinNum == 22619078416760300)
summary(ico_main$coinNum)
##Task 5 ends 
##Task 6 starts: Handling incorrect values in distributedPercentage 
filter(ico_main, distributedPercentage == 0)
filter(ico_main, distributedPercentage > 1)
ico_main <- ico_main[ico_main$distributedPercentage <= 1,]
ico_main <- ico_main[ico_main$distributedPercentage != 0,]
filter(ico_main, distributedPercentage == 0)
filter(ico_main, distributedPercentage > 1)
##Task 6 ends 
##Task 7 starts: Replacing empty country with unknown 
table(ico_main$countryRegion)
ico_main$countryRegion[nchar(ico_main$countryRegion) == 0] <- "unknown"
table(ico_main$countryRegion)
##Task 7 ends 
##Task 8 starts: Bringing uniformity in names of country 
ico_main$countryRegion[ico_main$countryRegion == "india"]   <- "India"
ico_main$countryRegion[ico_main$countryRegion == "México"]  <- "Mexico"
ico_main$countryRegion[ico_main$countryRegion == "SINGAPORE"]  <- "Singapore"
ico_main$countryRegion[ico_main$countryRegion == "usa"]  <- "USA"
ico_main$countryRegion[ico_main$countryRegion == "Curaçao"] <- "Curacao"
table(ico_main$countryRegion)
##Task 8 ends 
##Task 9 starts: Bringing uniformity in names of platform 
ico_main$platform <- gsub("\\s+","",ico_main$platform)
ico_main$platform <- tolower(ico_main$platform)
table(ico_main$platform)
ico_main$platform[ico_main$platform == "btc"] <- "bitcoin"
ico_main$platform[ico_main$platform == "stellarprotocol"] <- "stellar"
ico_main$platform[ico_main$platform == "x11blockchain"] <- "x11"
ico_main$platform[ico_main$platform == "eth"] <- "ethereum"
ico_main$platform[ico_main$platform == "ethererum"] <- "ethereum"
ico_main$platform[ico_main$platform == "etherum"] <- "ethereum"
ico_main$platform[ico_main$platform == "pos,pow"] <- "pos+pow"
ico_main$platform[ico_main$platform == "pow/pos"] <- "pos+pow"
ico_main$platform[ico_main$platform == "ethereum,waves"] <- "ethereum+waves"
ico_main$platform[nchar(ico_main$platform) == 0] <- "unknown"
ico_main$platform <- gsub("\u200b", "", ico_main$platform)
table(ico_main$platform)
## Task 9 ends 
##Feature Engineering Task 1 starts : Creating new feature : Duration
ico_main[,"startDate"] <- as.Date(ico_main[,"startDate"],format = "%d/%m/%Y")
ico_main[,"endDate"] <- as.Date(ico_main[,"endDate"],format = "%d/%m/%Y")
ico_main$Duration_of_campaign <- difftime(ico_main$endDate,ico_main$startDate, units = "days")
ico_main$Duration_of_campaign <- as.numeric(ico_main$Duration_of_campaign)
str(ico_main$Duration_of_campaign)
## Feature Engineering Task 1 ends
## Feature Selection starts
#Task 1
#str(ico_main)
ico_main <- ico_main[, - c(1,3,8,9)] # removing features - "ID", "brandSlogan", "startDate", "endDate"
#str(ico_main)
##Feature selection ends