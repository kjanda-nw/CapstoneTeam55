#This file does work for EDA, examining missing data etcetera


#list of needed libraries
#install.packages("fBasics")
library(fBasics)
library(dplyr)
library(tidyr)
library(plyr)

#read in the data
df <- read.csv("country_pred_data.csv")

#get a listing of numeric columns
nums <- unlist(lapply(df, is.numeric))  

#Try with basicStats from fBasics package
summary <- basicStats(df[ , nums])
turned <- as.data.frame(t(summary))
turned2 <- turned[order(turned$NAs, decreasing = TRUE),]

#build a unique country list
countries <- as.character(unique(df$countryname)) #210

by_country <- function(data,country) {
  subdata <- subset(data,countryname==country)
  summaryc <- basicStats(subdata[ , nums])
  turnedc <- as.data.frame(t(summaryc))
  turnedc$countryname <- country
  return(turnedc)
}

turned3 <- turned2
turned3$countryname <-"All"
#build an empty dataframe to append to
miss_by_country <- turned3[FALSE, ]

#get missingness summaries by country
for (c in countries) {
  miss_by_country <- rbind(miss_by_country,by_country(df,c))
}

#output 
#write.csv(turned2,"../ForecastData/summary_overall.csv")
#write.csv(miss_by_country,"../ForecastData/summary_by_country.csv")

#merge on metadata
meta <- read.csv("Predictive_model_TABLE_FOR_PRED_MODEL_FILTERS_Metadata_Country_API_AG.LND.FRST.K2_DS2_en_csv_v2_989381.csv")
colnames(meta)[1] <- "Country.Code"
meta <- meta[ ,c("Country.Code","IncomeGroup")]
df2 <- merge(df,meta,by="Country.Code")

#drop vars based on EDA
drop_vars <- c("x_6611_5110","x_6616_5110","x_6630_5110","x_6633_5110","x_6640_5110","x_6641_5110",
               "x_6642_5110","x_6643_5110","x_6644_5110","x_6645_5110","x_6649_5110","x_6656_5110",
               "x_6657_5110","x_6659_5110","x_6664_5110","x_6665_5110","x_6666_5110","x_6668_5110",
               "x_6669_5110","x_6671_5110","x_6672_5110","x_6681_5110","x_6682_5110","x_6694_5110",
               "x_6726_719511","x_6726_722411","x_6726_722911","x_6762_5110","x_6767_5110","x_6771_5110",
               "x_6773_5110","x_6775_5110","x_6796_722411","x_6796_722911","x_6797_722411","x_6797_722911",
               "x_6970_5007","x_6971_5007","x_6972_5007","x_6974_5007","x_6976_5007","x_6980_5007",
               "x_6981_5007","x_6983_5007","x_6726_719411","x_6726_722511","x_6726_723011","x_6726_723111",
               "x_6726_724311","x_6726_724411","x_6726_7245","x_6726_7246","x_6680_5110","x_6690_5110",
               "x_6797_722911","x_6798_719411","X","Year.y","yrc.x","Country.Name",
               #Vars being dropped being too similar to target
               "x_6646_5110","x_6714_5110","x_6974_5008","x_6975_5008",
               #drops from imp package
               'x_6973_5007', 'x_6973_5008', 'x_6975_5007', 'x_6977_5007', 'x_6978_5007', 'x_6979_5007', 'x_6982_5007', 'x_6982_5008' 
               )

df3 <- df2[ ,!(names(df2) %in% drop_vars)]

#drop countries based on missing data evaluation
#high missingess first 7 countries
#Countries with high missingness in the target (missing more than 10 years)
drop_countries <- c("Gibraltar","Macao SAR, China", "Monaco","Montenegro","Serbia","South Sudan","Sudan",
                    "Hong Kong SAR, China","Luxembourg","Belgium")
df4 <- subset(df3, !(countryname %in% drop_countries))


#Start by imputing variables found important in initial DataRobot pass
#pop, gdp, x_6978_5008, x_6979_5008, x_6796_723011, x_6601_5110, x_6602_5110, x_6620_5110, x_6621_5110,
#x_6650_5110, x_6655_5110

#build_ratio <- function(input_data,var,country) {
#  df <- input_data[ ,names(input_data) %in% c(var,"Country.Code")]
#  data <- subset(df,Country.Code == country)
#  data$lag1 <- c(data[-1, 2], NA)
#  data$fratio <- data[ ,2]/data$lag1
#  data$lag2 <- c(data[+1, 2], NA)
#  data$rratio <- data[ ,2]/data$lag2
#  #average by country
#  ratios <- data %>% dplyr::group_by(Country.Code) %>% summarize(avg_fratio=mean(fratio, na.rm=T),avg_rratio=mean(rratio, na.rm=T))
#  return(ratios)
#}

#input_data <- df4
#impute <- function(input_data,ratio,country) {
#  data <- input_data[ , names(input_data) %in% c(var,"Country.Code","yr")]
#  data2 <- merge(data,gdp_ratio,by="Country.Code")
#  subset <- subset(data2, Country.Code == "AFG")
#  subset$lag <- c(subset[-1, 3], NA)
#  subset$lag <- c(subset[+1, 3], NA)
#}

#for (c in countries) {
#  miss_by_country <- rbind(miss_by_country,by_country(df,c))
#}

#gdp_ratio <- build_ratio(df4,"gdp")

#check for highly correlated variables (causing issues with imp package)
df5 <- df4[complete.cases(df4), ]
nums <- unlist(lapply(df5, is.numeric))  
df6 <- df5[ , nums]
library(caret)
fi1 = cor(df6)
hc = findCorrelation(fi1, cutoff=0.4) # putt any value as a "cutoff" 
hc = sort(hc)
reduced_Data = df6[,-c(hc)]

#keep_names <- names(reduced_Data)
#keep_names <- c(keep_names,"countryname","Area","IncomeGroup","Country.Code")
time <- df4[ ,nums]
keep_names <- c(colnames(time),"countryname","yr")
new <- df4[ ,names(df4) %in% keep_names]
new <- new[FALSE, ]
library(imputeTS)
char <- df4[ , names(df4) %in% c("countryname","Area","IncomeGroup","Country.Code","yr")]
countries <- as.character(unique(df4$countryname)) #210
for (c in countries) {
  bc <- subset(df4, countryname==c)
  bc <- bc[ , !(names(bc) %in% c("Area"))]
  build <- subset(char,countryname==c)
  build <- build[ ,names(build) %in% c("countryname","yr")]
  for (i in 4:ncol(bc)) {
    if (sum(is.na(bc[ ,i])) < 20 & sum(is.na(bc[ ,i])) > 0) {
      n <- bc[ ,i]
      b <- na_ma(n,weighting="linear")
      dfc <- as.data.frame(b)
      names(dfc)[1] <- names(bc)[i]
      build <- cbind(build,dfc)
    } else {
     dfc <- as.data.frame(bc[ ,i])
     names(dfc)[1] <- names(bc)[i]
     build <- cbind(build,dfc)
   }
    #new <- build[FALSE, ]
  }
  new <- rbind(new,build)
}

output <- merge(new,char,by=c("countryname","yr"))
write.csv(output,"country_pred_data_v2.csv")


#rerun our EDA on the imputed data for evaluating the quality of the imputations
#read in the data
df <- read.csv("country_pred_data_v2.csv")

#get a listing of numeric columns
nums <- unlist(lapply(df, is.numeric))  

#Try with basicStats from fBasics package
summary <- basicStats(df[ , nums])
turned <- as.data.frame(t(summary))
turned2 <- turned[order(turned$NAs, decreasing = TRUE),]

#build a unique country list
countries <- as.character(unique(df$countryname)) #210

by_country <- function(data,country) {
  subdata <- subset(data,countryname==country)
  summaryc <- basicStats(subdata[ , nums])
  turnedc <- as.data.frame(t(summaryc))
  turnedc$countryname <- country
  return(turnedc)
}

turned3 <- turned2
turned3$countryname <-"All"
#build an empty dataframe to append to
miss_by_country <- turned3[FALSE, ]

#get missingness summaries by country
for (c in countries) {
  miss_by_country <- rbind(miss_by_country,by_country(df,c))
}

#output 
write.csv(turned2,"summary_overall_postimp.csv")
write.csv(miss_by_country,"summary_by_country_postimp.csv")

cor(df[ ,nums])
