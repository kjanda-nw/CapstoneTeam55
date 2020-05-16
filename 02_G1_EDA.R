header = '
MSDS 498
Spring 2020
Team 55 - Forest Tracker

Purpose: The purposed of this program was dig into the current file. It examines missingness and then
         has some of our data cleaning including record drops, variable drops, and imputation. Then 
         the EDA summary is rerun to verify the quality of the imputations
'
#list of needed libraries
library(fBasics)
library(dplyr)
library(tidyr)
library(plyr)
library(caret)
library(imputeTS)

#set working directory
setwd("~/Documents/MSDS498/CapstoneTeam55")

#read in the data
df <- read.csv("./Data/country_pred_data.csv")

#get a listing of numeric columns
nums <- unlist(lapply(df, is.numeric))  

#Build a data frame of the summary statistcs for each variable
#This allows us to output to excel and manipulate
summary <- basicStats(df[ , nums])
turned <- as.data.frame(t(summary))
turned2 <- turned[order(turned$NAs, decreasing = TRUE),]

#build a unique country list
countries <- as.character(unique(df$countryname)) #210

#run the same summaries by country
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

#output for examination
write.csv(turned2,"../ForecastData/summary_overall.csv")
write.csv(miss_by_country,"../ForecastData/summary_by_country.csv")

#merge on metadata
meta <- read.csv("../ForecastData/Predictive_model_20200505_TABLE_FOR_PRED_MODEL_FILTERS_Metadata_Country_API_AG.LND.FRST.K2_DS2_en_csv_v2_989381.csv")
colnames(meta)[1] <- "Country.Code"
meta <- meta[ ,c("Country.Code","IncomeGroup","Sub.Regions","Commodity.driven.deforestation","Forestry","Wildfire","Shifting.agriculture")]
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
               #drop for being highly correlated to existing vars
               'x_6973_5007', 'x_6973_5008', 'x_6975_5007', 'x_6977_5007', 'x_6978_5007', 'x_6979_5007', 'x_6982_5007', 'x_6982_5008' 
               )
df3 <- df2[ ,!(names(df2) %in% drop_vars)]

#drop countries based on missing data evaluation
#high missingess first 7 countries
#Countries with high missingness in the target (missing more than 10 years)
drop_countries <- c("Gibraltar","Macao SAR, China", "Monaco","Montenegro","Serbia","South Sudan","Sudan",
                    "Hong Kong SAR, China")
df4 <- subset(df3, !(countryname %in% drop_countries))

#check for highly correlated variables 
df5 <- df4[complete.cases(df4), ]
nums <- unlist(lapply(df5, is.numeric))  
df6 <- df5[ , nums]
fi1 = cor(df6)
hc = findCorrelation(fi1, cutoff=0.4) # put any value as a "cutoff" 
hc = sort(hc)
reduced_Data = df6[,-c(hc)]

#create a dataframe of just the numeric variables
time <- df4[ ,nums]

#create an empty dataframe with the numeric variables and identifier variables
keep_names <- c(colnames(time),"countryname","yr")
new <- df4[ ,names(df4) %in% keep_names]
new <- new[FALSE, ]

#Save off the categorical/character variables for later
char <- df4[ , names(df4) %in% c("countryname","Area","IncomeGroup","Country.Code","yr","Sub.Regions","Commodity.driven.deforestation","Forestry","Wildfire","Shifting.agriculture")]

#save an updated country list
countries <- as.character(unique(df4$countryname)) #210

#IMPUTATION
#imputation was done by country by variable, so we start by iterating through the countries
for (c in countries) {
  #subset data to a single country
  bc <- subset(df4, countryname==c)
  #drop off some extraneous variables
  bc <- bc[ , !(names(bc) %in% c("Area"))]
  #subset categorical/character only data down to country with only identifier variables
  build <- subset(char,countryname==c)
  build <- build[ ,names(build) %in% c("countryname","yr")]
  #iterate through the columns
  for (i in 4:ncol(bc)) {
    #only impute when we have missing data and at least 7 years of data
    if (sum(is.na(bc[ ,i])) < 20 & sum(is.na(bc[ ,i])) > 0) {
      #impute using a moving average
      n <- bc[ ,i]
      b <- na_ma(n,weighting="linear")
      dfc <- as.data.frame(b)
      #apply the variable name to the imputations
      names(dfc)[1] <- names(bc)[i]
      #bind to the subset identifiers
      build <- cbind(build,dfc)
    } else {
     #if we aren't imputing (either due to too much missing data or no missing data)
     #bind to the subset identifiers
     dfc <- as.data.frame(bc[ ,i])
     names(dfc)[1] <- names(bc)[i]
     build <- cbind(build,dfc)
   }
  }
  #add to the empty dataset
  new <- rbind(new,build)
}

#create some new features that group countries
#group based on data availability
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
'Poland','Vietnam'),1,ifelse(new$countryname %in% c('Ghana','Afghanistan','Belgium',
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

#data based on pct forest area
new$forestgroup <- ifelse(new$countryname %in% c('Suriname','Belgium',
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
                                                 'Senegal'),1,ifelse(new$countryname %in% c('Trinidad and Tobago','Cameroon',
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
'Vietnam'),2,ifelse(new$countryname %in% c('Cyprus','Uganda',
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

#data based on gdp
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
                                              'Belgium',
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
                                              'Iran, Islamic Rep.',
                                              'South Africa',
                                              'Thailand',
                                              'Finland',
                                              'Portugal',
                                              'Ireland',
                                              'Iraq',
                                              'Israel',
                                              'United Arab Emirates',
                                              'Venezuela, RB',
                                              'Malaysia',
                                              'Colombia',
                                              'Nigeria',
                                              'Czech Republic',
                                              'Hungary',
                                              'New Zealand',
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
                                              'Croatia'),1,ifelse(new$countryname %in% c('Vietnam','Cuba','Libya',
                                                                                         'Ecuador', 'Myanmar',
                                                                                         'Kazakhstan',
                                                                                         'Slovenia', 'Lithuania',
                                                                                         'Tunisia',
                                                                                         'Dominican Republic',
                                                                                         'Qatar',
                                                                                         'Uruguay',
                                                                                         'Guatemala',
                                                                                         'Belarus',
                                                                                         'Oman',
                                                                                         'Bulgaria',
                                                                                         'Lebanon', 'Latvia',
                                                                                         'Sri Lanka', 'Angola',
                                                                                         'Costa Rica','Syrian Arab Republic',
                                                                                         'Cameroon',
                                                                                         "Cote d'Ivoire",
'Tanzania',
'Kenya',
'Uzbekistan',
'Cyprus',
'Panama',
'El Salvador',
'Estonia',
'Afghanistan',
'Ethiopia', 'Yemen, Rep.',
'Trinidad and Tobago','Bosnia and Herzegovina',
'Bahrain',
'Congo, Dem. Rep.',
'Jordan',
'Paraguay',
'Jamaica',
'Bahamas, The',
'Azerbaijan',
'Senegal',
'Bolivia',
'Honduras',
'Ghana',
'Botswana',
'Zimbabwe',
'Uganda',
'Brunei Darussalam',
'Georgia',
'Nepal',
'Turkmenistan',
'Mauritius','Cambodia',
'Albania',
'Nicaragua'),2,ifelse(new$countryname %in% c('Papua New Guinea','North Macedonia','Zambia','Namibia','Guam',
                                          'Mali',
                                          'Madagascar','Cayman Islands',
                                          'Burkina Faso','Virgin Islands (U.S.)',
                                          'Haiti',
                                          'Benin',
                                          'Guinea',
                                          'French Polynesia',
                                          'Congo, Rep.',
                                          'Malawi',
                                          'Barbados',
                                          'New Caledonia','Timor-Leste',
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
                                          'Liberia',
                                          'Mauritania',
                                          'Sierra Leone',
                                          'Central African Republic',
                                          'Suriname','Lesotho','Burundi','Maldives',
                                          'Belize',
                                          'Northern Mariana Islands',
                                          'St. Lucia',
                                          'Eritrea',
                                          'Antigua and Barbuda',
                                          'Gambia, The',
                                          'Cabo Verde',
                                          'Guyana',
                                          'Bhutan',
                                          'Grenada',
                                          'Comoros',
                                          'St. Kitts and Nevis',
                                          'Solomon Islands',
                                          'St. Vincent and the Grenadines',
                                          'Dominica',
                                          'Samoa',
                                          'Vanuatu',
                                          'Micronesia, Fed. Sts.',
                                          'Tonga',
                                          'Palau'),3,4)))

#data based on population
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
                                              'Vietnam',
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
'Kazakhstan'),1,ifelse(new$countryname %in% c('Ecuador','Cambodia',
                                           'Burkina Faso',
                                           'Niger',
                                           'Guatemala',
                                           'Malawi',
                                           'Mali',
                                           'Zimbabwe',
                                           'Zambia',
                                           'Cuba',
                                           'Greece',
                                           'Belgium',
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
                                           'Puerto Rico'),2,ifelse(new$countryname %in% c('United Arab Emirates','Lithuania',
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


#merge the imputations back with the categorical/charcter data
output <- merge(new,char,by=c("countryname","yr"))

#drop some stragglers we picked up
oth <- c("x_6646_5110","x_6646_72151","IncomeGroup.y","yrc.x.1","Year.y.1","Sub.Regions.y","Commodity.driven.deforestation.y","Forestry.y","Wildfire.y","Shifting.agriculture.y")
output2 <- output[ ,!(names(output) %in% oth)]
#output
write.csv(output2,"./Data/country_pred_data_v2.csv")

#BUILD DELTA TARGET
#read in current dataset
updated <- read.csv("./Data/country_pred_data_v2.csv")
#build our delta dataset
#subset to variables that are being lagged
forlag <- updated[ ,names(updated) %in% c("Country.Code","yr","forest_area","gdp","pop",'x_257_5910',
                                          'x_236_5910',
                                          'x_2071_5910',
                                          'x_258_5910',
                                          'x_237_5910',
                                          'x_1601_5516',
                                          'x_1604_5516',
                                          'x_1623_5516',
                                          'x_1626_5516',
                                          'x_1633_5516',
                                          'x_1602_5516',
                                          'x_1603_5516',
                                          'x_1634_5516',
                                          'x_6798_7246',
                                          'x_6798_7245',
                                          'x_6610_5110',
                                          'x_6716_5110',
                                          'x_6717_5110')]
#lag data
forlag2 <- subset(forlag,yr<2016)  
forlag2$yr <- forlag2$yr+1

#rename lagged columns
colnames(forlag2)[2] <- "lag_gdp"
colnames(forlag2)[3] <- "lag_pop"
colnames(forlag2)[4] <- "lag_forest_area"
colnames(forlag2)[13] <- 'lag_x_257_5910'
colnames(forlag2)[11] <- 'lag_x_236_5910'
colnames(forlag2)[10] <- 'lag_x_2071_5910'
colnames(forlag2)[14] <- 'lag_x_258_5910'
colnames(forlag2)[12] <- 'lag_x_237_5910'
colnames(forlag2)[15] <- 'lag_x_1601_5516'
colnames(forlag2)[18] <- 'lag_x_1604_5516'
colnames(forlag2)[19] <- 'lag_x_1623_5516'
colnames(forlag2)[20] <- 'lag_x_1626_5516'
colnames(forlag2)[21] <- 'lag_x_1633_5516'
colnames(forlag2)[16] <- 'lag_x_1602_5516'
colnames(forlag2)[17] <- 'lag_x_1603_5516'
colnames(forlag2)[22] <- 'lag_x_1634_5516'
colnames(forlag2)[6] <- 'lag_x_6798_7246'
colnames(forlag2)[5] <- 'lag_x_6798_7245'
colnames(forlag2)[7] <- 'lag_x_6610_5110'
colnames(forlag2)[8] <- 'lag_x_6716_5110'
colnames(forlag2)[9] <- 'lag_x_6717_5110'

#merge lagged data onto original data
output3 <- merge(updated,forlag2,by=c("Country.Code","yr"))
#calculate percent change
output3$delta_gdp<-ifelse(output3$lag_gdp==0 | is.na(output3$lag_gdp),0,(output3$gdp-output3$lag_gdp)/output3$lag_gdp)
output3$delta_forest_area<-ifelse(output3$lag_forest_area==0 | is.na(output3$lag_forest_area),0,(output3$forest_area-output3$lag_forest_area)/output3$lag_forest_area)
output3$delta_pop<-ifelse(output3$lag_pop==0 | is.na(output3$lag_pop),0,(output3$pop-output3$lag_pop)/output3$lag_pop)
output3$delta_x_257_5910<-ifelse(output3$lag_x_257_5910==0 | is.na(output3$lag_x_257_5910),0,(output3$x_257_5910-output3$lag_x_257_5910)/output3$lag_x_257_5910)
output3$delta_x_236_5910<-ifelse(output3$lag_x_236_5910==0 | is.na(output3$lag_x_236_5910),0,(output3$x_236_5910-output3$lag_x_236_5910)/output3$lag_x_236_5910)
output3$delta_x_2071_5910<-ifelse(output3$lag_x_2071_5910==0 | is.na(output3$lag_x_2071_5910),0,(output3$x_2071_5910-output3$lag_x_2071_5910)/output3$lag_x_2071_5910)
output3$delta_x_258_5910<-ifelse(output3$lag_x_258_5910==0 | is.na(output3$lag_x_258_5910),0,(output3$x_258_5910-output3$lag_x_258_5910)/output3$lag_x_258_5910)
output3$delta_x_237_5910<-ifelse(output3$lag_x_237_5910==0 | is.na(output3$lag_x_237_5910),0,(output3$x_237_5910-output3$lag_x_237_5910)/output3$lag_x_237_5910)
output3$delta_x_1601_5516<-ifelse(output3$lag_x_1601_5516==0 | is.na(output3$lag_x_1601_5516),0,(output3$x_1601_5516-output3$lag_x_1601_5516)/output3$lag_x_1601_5516)
output3$delta_x_1604_5516<-ifelse(output3$lag_x_1604_5516==0 | is.na(output3$lag_x_1604_5516),0,(output3$x_1604_5516-output3$lag_x_1604_5516)/output3$lag_x_1604_5516)
output3$delta_x_1623_5516<-ifelse(output3$lag_x_1623_5516==0 | is.na(output3$lag_x_1623_5516),0,(output3$x_1623_5516-output3$lag_x_1623_5516)/output3$lag_x_1623_5516)
output3$delta_x_1626_5516<-ifelse(output3$lag_x_1626_5516==0 | is.na(output3$lag_x_1626_5516),0,(output3$x_1626_5516-output3$lag_x_1626_5516)/output3$lag_x_1626_5516)
output3$delta_x_1633_5516<-ifelse(output3$lag_x_1633_5516==0 | is.na(output3$lag_x_1633_5516),0,(output3$x_1633_5516-output3$lag_x_1633_5516)/output3$lag_x_1633_5516)
output3$delta_x_1602_5516<-ifelse(output3$lag_x_1602_5516==0 | is.na(output3$lag_x_1602_5516),0,(output3$x_1602_5516-output3$lag_x_1602_5516)/output3$lag_x_1602_5516)
output3$delta_x_1603_5516<-ifelse(output3$lag_x_1603_5516==0 | is.na(output3$lag_x_1603_5516),0,(output3$x_1603_5516-output3$lag_x_1603_5516)/output3$lag_x_1603_5516)
output3$delta_x_1634_5516<-ifelse(output3$lag_x_1634_5516==0 | is.na(output3$lag_x_1634_5516),0,(output3$x_1634_5516-output3$lag_x_1634_5516)/output3$lag_x_1634_5516)
output3$delta_x_6798_7246<-ifelse(output3$lag_x_6798_7246==0 | is.na(output3$lag_x_6798_7246),0,(output3$x_6798_7246-output3$lag_x_6798_7246)/output3$lag_x_6798_7246)
output3$delta_x_6798_7245<-ifelse(output3$lag_x_6798_7245==0 | is.na(output3$lag_x_6798_7245),0,(output3$x_6798_7245-output3$lag_x_6798_7245)/output3$lag_x_6798_7245)
output3$delta_x_6610_5110<-ifelse(output3$lag_x_6610_5110==0 | is.na(output3$lag_x_6610_5110),0,(output3$x_6610_5110-output3$lag_x_6610_5110)/output3$lag_x_6610_5110)
output3$delta_x_6716_5110<-ifelse(output3$lag_x_6716_5110==0 | is.na(output3$lag_x_6716_5110),0,(output3$x_6716_5110-output3$lag_x_6716_5110)/output3$lag_x_6716_5110)
output3$delta_x_6717_5110<-ifelse(output3$lag_x_6717_5110==0 | is.na(output3$lag_x_6717_5110),0,(output3$x_6717_5110-output3$lag_x_6717_5110)/output3$lag_x_6717_5110)
#output this data
write.csv(output3,"./Data/country_pred_delta_forest.csv")
  
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

