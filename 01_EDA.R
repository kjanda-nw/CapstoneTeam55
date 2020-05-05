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
meta <- read.csv("../ForecastData/Predictive_model_TABLE_FOR_PRED_MODEL_FILTERS_Metadata_Country_API_AG.LND.FRST.K2_DS2_en_csv_v2_989381.csv")
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

#create some new features
new$datagroup <- ifelse(new$countryname %in% c('Rusian Federation','Brazil', 'Canada', 'United States','China','Congo, Dem. Rep.',
                                               'Australia', 'Indonesia', 'Peru', 'India',  'Mexico',
                                               'Columbia',
                                               'Angola',
                                               'Bolivia',
                                               'Zambia',
                                               'Venezuela, RB',
                                               'Tanzania',
                                               'Papua New Guinea',
                                               'Myanmar',
                                               'Sweden',
                                               'Argentina',
                                               'Japan',
                                               'Congo, Rep.',
                                               'Finland',
                                               'Malaysia',
                                               'Central African Republic',
                                               'Lao PDR',
                                               'Cameroon',
                                               'Spain',
                                               'Chile',
                                               'France',
                                               'Guyana',
                                               'Thailand',
                                               'Suriname',
                                               'Paraguay',
                                               'Zimbabwe',
                                               'Ethiopia',
                                               'Ecuador',
                                               'Madagascar',
                                               'Mongolia',
                                               'Norway',
                                               'Turkey',
                                               'Germany',
                                               'Botswana',
                                               'Iran, Islamic Republic',
                                               "Cote d'Ivoire",
'New Zealand',
'Ukraine',
'Poland',
'Ghana'),1,ifelse(new$countryname %in% c('Afghanistan',
                                         'Albania',
                                         'Algeria',
                                         'Antigua and Barbuda',
                                         'Armenia',
                                         'Austria',
                                         'Azerbaijan',
                                         'Bahamas, The',
                                         'Bahrain',
                                         'Bangladesh',
                                         'Barbados',
                                         'Belarus',
                                         'Belize',
                                         'Benin',
                                         'Bhutan',
                                         'Bosnia and Herzegovina',
                                         'Brunei Darussalam',
                                         'Bulgaria',
                                         'Burkina Faso',
                                         'Burundi',
                                         'Cabo Verde',
                                         'Cambodia',
                                         'Cayman Islands',
                                         'Chad',
                                         'Comoros',
                                         'Costa Rica',
                                         'Croatia',
                                         'Cuba',
                                         'Cyprus',
                                         'Czech Republic',
                                         'Denmark',
                                         'Dominica',
                                         'Dominican Republic',
                                         'El Salvador',
                                         'Equatorial Guinea',
                                         'Eritrea',
                                         'Estonia',
                                         'Fiji',
                                         'French Polynesia',
                                         'Gambia, The',
                                         'Georgia',
                                         'Greece',
                                         'Grenada',
                                         'Guam',
                                         'Guatemala',
                                         'Guinea',
                                         'Haiti',
                                         'Honduras',
                                         'Hungary',
                                         'Iraq',
                                         'Ireland',
                                         'Israel',
                                         'Italy',
                                         'Jamaica',
                                         'Jordan',
                                         'Kazakhstan',
                                         'Kenya',
                                         'Korea, Rep.',
                                         'Kuwait',
                                         'Kyrgyz Republic',
                                         'Latvia',
                                         'Lebanon',
                                         'Lesotho',
                                         'Liberia',
                                         'Libya',
                                         'Lithuania',
                                         'Malawi',
                                         'Maldives',
                                         'Mali',
                                         'Mauritania',
                                         'Mauritius',
                                         'Micronesia, Fed. Sts.',
                                         'Morocco',
                                         'Namibia',
                                         'Nepal',
                                         'Netherlands',
                                         'New Caledonia',
                                         'Nicaragua',
                                         'Niger',
                                         'Nigeria',
                                         'North Macedonia',
                                         'Northern Mariana Islands',
                                         'Oman',
                                         'Pakistan',
                                         'Palau',
                                         'Panama',
                                         'Philippines',
                                         'Portugal',
                                         'Puerto Rico',
                                         'Qatar',
                                         'Romania',
                                         'Rwanda',
                                         'Samoa',
                                         'Saudi Arabia',
                                         'Senegal',
                                         'Sierra Leone',
                                         'Slovak Republic',
                                         'Slovenia',
                                         'Solomon Islands',
                                         'South Africa',
                                         'Sri Lanka',
                                         'St. Kitts and Nevis',
                                         'St. Lucia',
                                         'St. Vincent and the Grenadines',
                                         'Switzerland',
                                         'Syrian Arab Republic',
                                         'Tajikistan',
                                         'Timor-Leste',
                                         'Togo',
                                         'Tonga',
                                         'Trinidad and Tobago',
                                         'Tunisia',
                                         'Turkmenistan',
                                         'Uganda',
                                         'United Arab Emirates',
                                         'United Kingdom',
                                         'Uruguay',
                                         'Uzbekistan',
                                         'Vanuatu',
                                         'Virgin Islands (U.S.)',
                                         'Yemen, Rep.'),2,3))

new$forestgroup <- ifelse(new$countryname %in% c('Suriname',
                                                 'Micronesia, Fed. Sts.',
                                                 'Palau',
                                                 'Guyana',
                                                 'Solomon Islands',
                                                 'Lao PDR',
                                                 'Brunei Darussalam',
                                                 'Papua New Guinea',
                                                 'Finland',
                                                 'Congo, Dem. Rep.',
                                                 'Sweden',
                                                 'Northern Mariana Islands',
                                                 'Japan',
                                                 'Zambia',
                                                 'Malaysia',
                                                 'St. Vincent and the Grenadines',
                                                 'Bhutan',
                                                 'Congo, Rep.',
                                                 'Panama',
                                                 'Korea, Rep.',
                                                 'Belize',
                                                 'Cambodia',
                                                 'Dominica',
                                                 'Slovenia',
                                                 'Brazil',
                                                 'Equatorial Guinea',
                                                 'Samoa',
                                                 'Peru',
                                                 'Tanzania',
                                                 'Virgin Islands (U.S.)',
                                                 'Timor-Leste',
                                                 'Colombia',
                                                 'Venezuela, RB',
                                                 'Bolivia',
                                                 'Indonesia',
                                                 'Fiji',
                                                 'Honduras',
                                                 'Cayman Islands',
                                                 'Estonia',
                                                 'Latvia',
                                                 'Ecuador',
                                                 'Myanmar',
                                                 'Puerto Rico',
                                                 'Bahamas, The',
                                                 'Grenada',
                                                 'Russian Federation',
                                                 'Costa Rica',
                                                 'Angola',
                                                 'Paraguay',
                                                 'Liberia',
                                                 'Austria',
                                                 'Zimbabwe',
                                                 'Guam',
                                                 'Gambia, The',
                                                 'New Caledonia',
                                                 'Senegal',
                                                 'Trinidad and Tobago'),1,ifelse(new$countryname %in% c('Cameroon',
                                                                                                        'Benin',
                                                                                                        'Bosnia and Herzegovina',
                                                                                                        'St. Kitts and Nevis',
                                                                                                        'Belarus',
                                                                                                        'Sierra Leone',
                                                                                                        'Slovak Republic',
                                                                                                        'Georgia',
                                                                                                        'Ghana',
                                                                                                        'New Zealand',
                                                                                                        'Canada',
                                                                                                        'North Macedonia',
                                                                                                        'Guatemala',
                                                                                                        'Malawi',
                                                                                                        'Portugal',
                                                                                                        'Vanuatu',
                                                                                                        'Central African Republic',
                                                                                                        'Mexico',
                                                                                                        'St. Lucia',
                                                                                                        'Spain',
                                                                                                        'Sri Lanka',
                                                                                                        'Czech Republic',
                                                                                                        'Croatia',
                                                                                                        'United States',
                                                                                                        'Lithuania',
                                                                                                        'Norway',
                                                                                                        'Dominican Republic',
                                                                                                        'French Polynesia',
                                                                                                        "Cote d'Ivoire",
'Germany',
'Bulgaria',
'Thailand',
'Jamaica',
'Switzerland',
'Nicaragua',
'Poland',
'Italy',
'Greece',
'France',
'Albania',
'Romania',
'Guinea',
'Nepal',
'Cuba',
'Philippines',
'Comoros',
'India',
'Antigua and Barbuda',
'Madagascar',
'Burkina Faso',
'Hungary',
'Botswana',
'Chile',
'Cabo Verde',
'China',
'Mauritius',
'Cyprus'),2,ifelse(new$countryname %in% c('Uganda',
                                          'Australia',
                                          'Ukraine',
                                          'Eritrea',
                                          'El Salvador',
                                          'Rwanda',
                                          'Barbados',
                                          'Turkey',
                                          'Denmark',
                                          'Ethiopia',
                                          'Lebanon',
                                          'Nigeria',
                                          'Tonga',
                                          'United Kingdom',
                                          'Morocco',
                                          'Armenia',
                                          'Argentina',
                                          'Bangladesh',
                                          'Netherlands',
                                          'Azerbaijan',
                                          'Ireland',
                                          'Namibia',
                                          'Burundi',
                                          'Turkmenistan',
                                          'Uruguay',
                                          'Mongolia',
                                          'Togo',
                                          'South Africa',
                                          'Uzbekistan',
                                          'Kenya',
                                          'Israel',
                                          'Iran, Islamic Rep.',
                                          'Tunisia',
                                          'Chad',
                                          'Mali',
                                          'Kyrgyz Republic',
                                          'United Arab Emirates',
                                          'Haiti',
                                          'Maldives',
                                          'Tajikistan',
                                          'Pakistan',
                                          'Syrian Arab Republic',
                                          'Afghanistan',
                                          'Iraq',
                                          'Lesotho',
                                          'Kazakhstan',
                                          'Jordan',
                                          'Yemen, Rep.',
                                          'Niger',
                                          'Algeria',
                                          'Bahrain',
                                          'Saudi Arabia',
                                          'Kuwait',
                                          'Mauritania',
                                          'Libya',
                                          'Oman',
                                          'Qatar'),3,4)))

new$gdpgroup <- ifelse(new$countryname %in% c('United States',
                                              'Japan',
                                              'Germany',
                                              'United Kingdom',
                                              'France',
                                              'China',
                                              'Italy',
                                              'Spain',
                                              'Canada',
                                              'Brazil',
                                              'Mexico',
                                              'Korea, Rep.',
                                              'India',
                                              'Netherlands',
                                              'Russian Federation',
                                              'Australia',
                                              'Switzerland',
                                              'Sweden',
                                              'Turkey',
                                              'Argentina',
                                              'Austria',
                                              'Indonesia',
                                              'Norway',
                                              'Denmark',
                                              'Poland',
                                              'Saudi Arabia',
                                              'Greece',
                                              'South Africa',
                                              'Thailand',
                                              'Finland',
                                              'Portugal',
                                              'Ireland',
                                              'Iran, Islamic Rep.',
                                              'Israel',
                                              'Iraq',
                                              'United Arab Emirates',
                                              'Venezuela, RB',
                                              'Malaysia',
                                              'Colombia',
                                              'Nigeria',
                                              'Czech Republic',
                                              'New Zealand',
                                              'Hungary',
                                              'Chile',
                                              'Philippines',
                                              'Pakistan',
                                              'Ukraine',
                                              'Puerto Rico',
                                              'Algeria',
                                              'Bangladesh',
                                              'Peru',
                                              'Romania',
                                              'Morocco',
                                              'Kuwait',
                                              'Slovak Republic',
                                              'Cuba',
                                              'Croatia'),1,ifelse(new$countryname %in% c('Libya',
                                                                                         'Ecuador',
                                                                                         'Kazakhstan',
                                                                                         'Slovenia',
                                                                                         'Tunisia',
                                                                                         'Dominican Republic',
                                                                                         'Qatar',
                                                                                         'Uruguay',
                                                                                         'Guatemala',
                                                                                         'Syrian Arab Republic',
                                                                                         'Belarus',
                                                                                         'Oman',
                                                                                         'Bulgaria',
                                                                                         'Lebanon',
                                                                                         'Sri Lanka',
                                                                                         'Lithuania',
                                                                                         'Angola',
                                                                                         'Costa Rica',
                                                                                         'Cameroon',
                                                                                         "Cote d'Ivoire",
'Tanzania',
'Kenya',
'Uzbekistan',
'Cyprus',
'Panama',
'El Salvador',
'Ethiopia',
'Yemen, Rep.',
'Latvia',
'Trinidad and Tobago',
'Bahrain',
'Congo, Dem. Rep.',
'Myanmar',
'Jordan',
'Estonia',
'Paraguay',
'Jamaica',
'Bahamas, The',
'Azerbaijan',
'Senegal',
'Bolivia',
'Bosnia and Herzegovina',
'Honduras',
'Ghana',
'Botswana',
'Zimbabwe',
'Uganda',
'Brunei Darussalam',
'Georgia',
'Nepal',
'Turkmenistan',
'Mauritius',
'Albania',
'Nicaragua',
'Papua New Guinea',
'North Macedonia',
'Zambia'),2,ifelse(new$countryname %in% c('Namibia',
                                          'Afghanistan',
                                          'Mali',
                                          'Cambodia',
                                          'Madagascar',
                                          'Burkina Faso',
                                          'Haiti',
                                          'Benin',
                                          'Guinea',
                                          'Guam',
                                          'French Polynesia',
                                          'Virgin Islands (U.S.)',
                                          'Congo, Rep.',
                                          'Malawi',
                                          'Barbados',
                                          'Cayman Islands',
                                          'New Caledonia',
                                          'Armenia',
                                          'Chad',
                                          'Niger',
                                          'Equatorial Guinea',
                                          'Kyrgyz Republic',
                                          'Mongolia',
                                          'Fiji',
                                          'Tajikistan',
                                          'Togo',
                                          'Rwanda',
                                          'Lao PDR',
                                          'Mauritania',
                                          'Sierra Leone',
                                          'Central African Republic',
                                          'Suriname',
                                          'Northern Mariana Islands',
                                          'Lesotho',
                                          'Burundi',
                                          'Maldives',
                                          'Belize',
                                          'Liberia',
                                          'St. Lucia',
                                          'Eritrea',
                                          'Antigua and Barbuda',
                                          'Gambia, The',
                                          'Cabo Verde',
                                          'Guyana',
                                          'Bhutan',
                                          'Grenada',
                                          'Comoros',
                                          'Timor-Leste',
                                          'St. Kitts and Nevis',
                                          'Solomon Islands',
                                          'St. Vincent and the Grenadines',
                                          'Dominica',
                                          'Samoa',
                                          'Vanuatu',
                                          'Micronesia, Fed. Sts.',
                                          'Tonga',
                                          'Palau'),3,4)))

new$popgroup <- ifelse(new$countryname %in% c('China',
                                              'India',
                                              'United States',
                                              'Indonesia',
                                              'Brazil',
                                              'Pakistan',
                                              'Russian Federation',
                                              'Bangladesh',
                                              'Nigeria',
                                              'Japan',
                                              'Mexico',
                                              'Philippines',
                                              'Germany',
                                              'Ethiopia',
                                              'Iran, Islamic Rep.',
                                              'Turkey',
                                              'Thailand',
                                              'France',
                                              'United Kingdom',
                                              'Italy',
                                              'Congo, Dem. Rep.',
                                              'Myanmar',
                                              'Korea, Rep.',
                                              'Ukraine',
                                              'South Africa',
                                              'Spain',
                                              'Colombia',
                                              'Poland',
                                              'Argentina',
                                              'Tanzania',
                                              'Kenya',
                                              'Algeria',
                                              'Canada',
                                              'Morocco',
                                              'Peru',
                                              'Uganda',
                                              'Iraq',
                                              'Uzbekistan',
                                              'Venezuela, RB',
                                              'Nepal',
                                              'Malaysia',
                                              'Afghanistan',
                                              'Saudi Arabia',
                                              'Romania',
                                              'Ghana',
                                              'Australia',
                                              'Sri Lanka',
                                              'Yemen, Rep.',
                                              'Angola',
                                              "Cote d'Ivoire",
'Syrian Arab Republic',
'Madagascar',
'Cameroon',
'Netherlands',
'Chile',
'Kazakhstan',
'Ecuador'),1,ifelse(new$countryname %in% c('Cambodia',
                                           'Burkina Faso',
                                           'Niger',
                                           'Guatemala',
                                           'Malawi',
                                           'Mali',
                                           'Zimbabwe',
                                           'Zambia',
                                           'Cuba',
                                           'Greece',
                                           'Senegal',
                                           'Portugal',
                                           'Czech Republic',
                                           'Hungary',
                                           'Tunisia',
                                           'Belarus',
                                           'Chad',
                                           'Sweden',
                                           'Bolivia',
                                           'Haiti',
                                           'Dominican Republic',
                                           'Guinea',
                                           'Rwanda',
                                           'Azerbaijan',
                                           'Austria',
                                           'Bulgaria',
                                           'Benin',
                                           'Switzerland',
                                           'Honduras',
                                           'Burundi',
                                           'Israel',
                                           'Tajikistan',
                                           'Papua New Guinea',
                                           'El Salvador',
                                           'Paraguay',
                                           'Libya',
                                           'Lao PDR',
                                           'Jordan',
                                           'Denmark',
                                           'Slovak Republic',
                                           'Togo',
                                           'Nicaragua',
                                           'Finland',
                                           'Sierra Leone',
                                           'Kyrgyz Republic',
                                           'Turkmenistan',
                                           'Norway',
                                           'Lebanon',
                                           'Croatia',
                                           'Costa Rica',
                                           'New Zealand',
                                           'Ireland',
                                           'Georgia',
                                           'Central African Republic',
                                           'Bosnia and Herzegovina',
                                           'Puerto Rico',
                                           'United Arab Emirates'),2,ifelse(new$countryname %in% c('Lithuania',
                                                                                                   'Congo, Rep.',
                                                                                                   'Uruguay',
                                                                                                   'Panama',
                                                                                                   'Liberia',
                                                                                                   'Albania',
                                                                                                   'Armenia',
                                                                                                   'Mauritania',
                                                                                                   'Jamaica',
                                                                                                   'Eritrea',
                                                                                                   'Mongolia',
                                                                                                   'Oman',
                                                                                                   'Latvia',
                                                                                                   'Kuwait',
                                                                                                   'North Macedonia',
                                                                                                   'Slovenia',
                                                                                                   'Lesotho',
                                                                                                   'Namibia',
                                                                                                   'Botswana',
                                                                                                   'Gambia, The',
                                                                                                   'Estonia',
                                                                                                   'Trinidad and Tobago',
                                                                                                   'Mauritius',
                                                                                                   'Cyprus',
                                                                                                   'Timor-Leste',
                                                                                                   'Fiji',
                                                                                                   'Bahrain',
                                                                                                   'Guyana',
                                                                                                   'Equatorial Guinea',
                                                                                                   'Qatar',
                                                                                                   'Bhutan',
                                                                                                   'Comoros',
                                                                                                   'Suriname',
                                                                                                   'Cabo Verde',
                                                                                                   'Solomon Islands',
                                                                                                   'Brunei Darussalam',
                                                                                                   'Bahamas, The',
                                                                                                   'Maldives',
                                                                                                   'Barbados',
                                                                                                   'Belize',
                                                                                                   'French Polynesia',
                                                                                                   'New Caledonia',
                                                                                                   'Vanuatu',
                                                                                                   'Samoa',
                                                                                                   'St. Lucia',
                                                                                                   'Guam',
                                                                                                   'Virgin Islands (U.S.)',
                                                                                                   'St. Vincent and the Grenadines',
                                                                                                   'Micronesia, Fed. Sts.',
                                                                                                   'Grenada',
                                                                                                   'Tonga',
                                                                                                   'Antigua and Barbuda',
                                                                                                   'Dominica',
                                                                                                   'Northern Mariana Islands',
                                                                                                   'Cayman Islands',
                                                                                                   'St. Kitts and Nevis',
                                                                                                   'Palau'),3,4)))


output <- merge(new,char,by=c("countryname","yr"))

oth <- c("x_6646_5110","x_6646_72151","IncomeGroup.y")
output2 <- output[ ,!(names(output) %in% oth)]
write.csv(output2,"country_pred_data_v2.csv")

#build our delta dataset
forlag <- output2[ ,names(output2) %in% c("Country.Code","yr","forest_area","gdp","pop")]
forlag2 <- subset(forlag,yr<2016)  
forlag2$yr <- forlag2$yr+1

colnames(forlag2)[2] <- "lag_gdp"
colnames(forlag2)[3] <- "lag_pop"
colnames(forlag2)[4] <- "lag_forest"

output3 <- merge(output2,forlag2,by=c("Country.Code","yr"))
output3$delta_forest <- (output3$forest_area - output3$lag_forest)/output3$forest_area
output3$delta_gdp <- (output3$gdp - output3$lag_gdp)/output3$gdp
output3$delta_pop <- (output3$pop - output3$lag_pop)/output3$pop

#output this data
write.csv(output3,"country_pred_delta_forest.csv")
  
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

