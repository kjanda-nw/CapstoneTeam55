header = '
MSDS 498
Spring 2020
Team 55 - Forest Tracker

Purpose: The purpose of this program is to build a simulated dataset for 5 year forest tracker predictions.
         We need to create future values for our 10 continuous predictor variables so we will use linear
         interpolation to carry forward and then make a second version with adjustments for 2020 COVID19
'
#set working directory
setwd("~/Documents/MSDS498/CapstoneTeam55")

#list of needed libraries
library(dplyr)
library(tidyverse)

#read in the data
df <- read.csv("./Data/country_pred_delta_forest.csv")

#save some columns to merge on at the end
sfl <- c("countryname","yr","Country.Code","Sub.Regions.x","IncomeGroup.x")
sfldf <- df[,names(df) %in% sfl]

sfldf <- subset(sfldf,yr==2016)
sfldf <- sfldf[,!(names(sfldf) %in% "yr")]

#we don't need all the columns for this so subset
cols <- c("yr","countryname","lag_pop","gdp","lag_x_1633_5516","lag_x_6798_7246","lag_x_1604_5516","lag_x_1626_5516",
          "lag_x_1634_5516","lag_x_1603_5516","lag_x_237_5910","lag_x_257_5910","forest_area","pct_forest","delta_forest_area",
          "lag_forest_area")
sub <- df[,names(df) %in% cols]
sub[is.na(sub)] <- 0

#save a updated country list
countries <- as.character(unique(sub$countryname)) #210
new <- sub
new <- new[FALSE, ]
for (c in countries) {
  build <- subset(sub,countryname==c)
  #add the missing rows to the end
  build[nrow(build)+1,] <- NA
  build$yr <- ifelse(is.na(build$yr),2017,build$yr)
  build[nrow(build)+1,] <- NA
  build$yr <- ifelse(is.na(build$yr),2018,build$yr)
  build[nrow(build)+1,] <- NA
  build$yr <- ifelse(is.na(build$yr),2019,build$yr)
  build[nrow(build)+1,] <- NA
  build$yr <- ifelse(is.na(build$yr),2020,build$yr)
  build[nrow(build)+1,] <- NA
  build$yr <- ifelse(is.na(build$yr),2021,build$yr)
  
  build$countryname <- ifelse(is.na(build$countryname),c,build$countryname)
  
  #add to the empty dataset
  new <- rbind(new,build)
}

#build rate for the last 5 years
#the rate will be the average of the pct change
change <- c("countryname",'yr','lag_pop','lag_x_1634_5516','lag_gdp','lag_x_1603_5516','lag_x_1633_5516','lag_x_237_5910',
            'lag_x_6798_7246','lag_x_257_5910','lag_x_1604_5516','lag_x_1626_5516','pop','x_1634_5516','gdp',
            'x_1603_5516','x_1633_5516','x_237_5910','x_6798_7246','x_257_5910','x_1604_5516','x_1626_5516')
pct <- df[,names(df) %in% change]
pct$rate_pop <- pct$pop/pct$lag_pop
pct$rate_gdp <- pct$gdp/pct$lag_gdp
pct$rate_x_1634_5516 <- pct$x_1634_5516/pct$lag_x_1634_5516
pct$rate_x_1603_5516 <- pct$x_1603_5516/pct$lag_x_1603_5516
pct$rate_x_1633_5516 <- pct$x_1633_5516/pct$lag_x_1633_5516
pct$rate_x_237_5910 <- pct$x_237_5910/pct$lag_x_237_5910
pct$rate_x_6798_7246 <- pct$x_6798_7246/pct$lag_x_6798_7246
pct$rate_x_257_5910 <- pct$x_257_5910/pct$lag_x_257_5910
pct$rate_x_1604_5516 <- pct$x_1604_5516/pct$lag_x_1604_5516
pct$rate_x_1626_5516 <- pct$x_1626_5516/pct$lag_x_1626_5516
rate <- subset(pct, yr>2011)
rate2 <- rate[,names(rate) %in% c("countryname",'yr','rate_pop','rate_x_1634_5516','rate_gdp','rate_x_1603_5516',
                                  'rate_x_1633_5516','rate_x_237_5910',
                                  'rate_x_6798_7246','rate_x_257_5910','rate_x_1604_5516','rate_x_1626_5516')]

ratio <- rate2 %>% group_by(countryname) %>% summarise_at(vars(-yr),list(mean))
#set any missing ratios to 1
is.na(ratio) <- sapply(ratio,is.infinite)
ratio[is.na(ratio)] <- 1

#merge ratios onto data
tog <- merge(new,ratio,by="countryname")

new2 <- tog
new2 <- new2[FALSE, ]
#DO THE CARRY FORWARD
for (c in countries) {
  build <- subset(tog,countryname==c)
  #update
  for (i in 27:nrow(build)) {
    build$lag_pop[i]<-build$lag_pop[i-1]*build$rate_pop[i]
    build$lag_x_1634_5516[i]<-build$lag_x_1634_5516[i-1]*build$rate_x_1634_5516[i]
    build$gdp[i]<-build$gdp[i-1]*build$rate_gdp[i]
    build$lag_x_1603_5516[i]<-build$lag_x_1603_5516[i-1]*build$rate_x_1603_5516[i]
    build$lag_x_1633_5516[i]<-build$lag_x_1633_5516[i-1]*build$rate_x_1633_5516[i]
    build$lag_x_237_5910[i]<-build$lag_x_237_5910[i-1]*build$rate_x_237_5910[i]
    build$lag_x_6798_7246[i]<-build$lag_x_6798_7246[i-1]*build$rate_x_6798_7246[i]
    build$lag_x_257_5910[i]<-build$lag_x_257_5910[i-1]*build$rate_x_257_5910[i]
    build$lag_x_1604_5516[i]<-build$lag_x_1604_5516[i-1]*build$rate_x_1604_5516[i]
    build$lag_x_1626_5516[i]<-build$lag_x_1626_5516[i-1]*build$rate_x_1626_5516[i]
  }
  #add to the empty dataset
  new2 <- rbind(new2,build)
}

new2[is.na(new2)] <- 0

#merge back on the data we saved off earlier
all <- merge(new2,sfldf,by="countryname")

all2 <- all %>% select(-contains("rate"))
all2$forest_area <- ifelse(all2$yr>2016,NA,all2$forest_area)
all2$pct_forest <- ifelse(all2$yr>2016,NA,all2$pct_forest)
all2$delta_forest_area <- ifelse(all2$yr>2016,NA,all2$delta_forest_area)
all2$lag_forest_area <- ifelse(all2$yr>2016,NA,all2$lag_forest_area)

#output unadjusted
write.csv(all2,"./Data/Data_Pred_Noadj.csv")

#build adjusted data (we will adjust both 2020 and 2021)
#sources for adjustment factors
#SOYBEAN (No adjustment): https://www.agriculture.com/news/the-ripple-effect-of-covid-19-for-soybean-farmers-demand-and-transportation
#PALM OIL (2020: 75%, 2021: 88%): https://www.foodnavigator-asia.com/Article/2020/04/28/COVID-19-palm-oil-depression-Indonesia-sees-volumes-fall-drastically-for-country-s-top-export-commodity
#POPULATION (2020: 98%, 2021:99%)
#GDP (2020: 99%, 2021: 99%):https://www.un.org/sustainabledevelopment/blog/2020/04/covid-19-likely-to-shrink-global-gdp-by-almost-one-per-cent-in-2020/
#FORESTRY (2020: 85%, 2021: 90%): https://www.atibt.org/en/impact-of-covid-19-on-timber-trade-3/
#BURNING BIOMASS (2020: 95%, 2021: 97%)

all2$lag_pop<-ifelse(all2$yr==2020,all2$lag_pop*0.98,all2$lag_pop)
all2$lag_x_1634_5516<-ifelse(all2$yr==2020,all2$lag_x_1634_5516*0.85,all2$lag_x_1634_5516)
all2$gdp<-ifelse(all2$yr==2020,all2$gdp*0.99,all2$gdp)
all2$lag_x_1603_5516<-ifelse(all2$yr==2020,all2$lag_x_1603_5516*0.85,all2$lag_x_1603_5516)
all2$lag_x_1633_5516<-ifelse(all2$yr==2020,all2$lag_x_1633_5516*0.85,all2$lag_x_1633_5516)
all2$lag_x_6798_7246<-ifelse(all2$yr==2020,all2$lag_x_6798_7246*0.95,all2$lag_x_6798_7246)
all2$lag_x_257_5910<-ifelse(all2$yr==2020,all2$lag_x_257_5910*0.75,all2$lag_x_257_5910)
all2$lag_x_1604_5516<-ifelse(all2$yr==2020,all2$lag_x_1604_5516*0.85,all2$lag_x_1604_5516)
all2$lag_x_1626_5516<-ifelse(all2$yr==2020,all2$lag_x_1626_5516*0.85,all2$lag_x_1626_5516)

all2$lag_pop<-ifelse(all2$yr==2021,all2$lag_pop*0.99,all2$lag_pop)
all2$lag_x_1634_5516<-ifelse(all2$yr==2021,all2$lag_x_1634_5516*0.9,all2$lag_x_1634_5516)
all2$gdp<-ifelse(all2$yr==2021,all2$gdp*0.99,all2$gdp)
all2$lag_x_1603_5516<-ifelse(all2$yr==2021,all2$lag_x_1603_5516*0.9,all2$lag_x_1603_5516)
all2$lag_x_1633_5516<-ifelse(all2$yr==2021,all2$lag_x_1633_5516*0.9,all2$lag_x_1633_5516)
all2$lag_x_6798_7246<-ifelse(all2$yr==2021,all2$lag_x_6798_7246*0.97,all2$lag_x_6798_7246)
all2$lag_x_257_5910<-ifelse(all2$yr==2021,all2$lag_x_257_5910*0.88,all2$lag_x_257_5910)
all2$lag_x_1604_5516<-ifelse(all2$yr==2021,all2$lag_x_1604_5516*0.9,all2$lag_x_1604_5516)
all2$lag_x_1626_5516<-ifelse(all2$yr==2021,all2$lag_x_1626_5516*0.9,all2$lag_x_1626_5516)

#output adjusted
write.csv(all2,"./Data/Data_Pred_adj.csv")
