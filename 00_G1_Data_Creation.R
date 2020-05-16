#This files turns and merges all the data sources

#final product has one row per country per year from 1990 to 2016

#set working directory
setwd("~/Documents/MSDS498/CapstoneTeam55")

#list of needed libraries
library(dplyr)
library(tidyr)
library(plyr)

#read in forest area data to identify data window
forest_area <- read.csv('../ForecastData/API_AG.LND.FRST.ZS_DS2_en_csv_v2_935916.csv',skip=4)
summary(forest_area)

#subset out non-country rows
forest_area <- subset(forest_area,!(Country.Name %in% c("Arab World","Caribbean small states","Central Europe and the Baltics",
                                                        "East Asia & Pacific","East Asia & Pacific (excluding high income)","Euro Area",
                                                        "Europe & Central Asia","Europe & Central Asia (excluding high income)","European Union",
                                                        "Fragile and conflict affected situations","Heavily indebted poor countries (HIPC)",
                                                        "Latin America & Caribbean","Latin America & Caribbean (excluding high income)",
                                                        "Least developed countries: UN classification","Middle East & North Africa","Middle East & North Africa (excluding high income)",
                                                        "North America","OECD Members","Other small states","Pacific island small states","Small states",
                                                        "South Asia","Sub-Saharan Africa","Sub-Saharan Africa (excluding high income)","High income",
                                                        "Low & middle income","Low income","Lower middle income","Middle income","Upper middle income",
                                                        "Sub-Saharan Africa (IDA & IBRD countries)","South Asia (IDA & IBRD)","Pre-demographic dividend",
                                                        "Post-demographic dividend","OECD members","Not classified","Middle East & North Africa (IDA & IBRD countries)",
                                                        "Latin America & the Caribbean (IDA & IBRD countries)","Late-demographic dividend","IBRD only","IDA & IBRD total",
                                                        "IDA only","IDA blend","IDA total","Europe & Central Asia (IDA & IBRD countries)","Euro Area",
                                                        "East Asia & Pacific (IDA & IBRD countries)","Early-demographic dividend","World","Euro area")))
#table(forest_area$Country.Name)

gdp <- read.csv('../ForecastData/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_988718.csv',skip=4)
summary(gdp)
#miminal missing data: max is 43 NAs in 1990

pop <- read.csv('../ForecastData/API_SP.POP.TOTL_DS2_en_csv_v2_988606.csv',skip=4)
summary(pop)
#minimal missing data

area <- read.csv('../ForecastData/API_AG.LND.FRST.K2_DS2_en_csv_v2_989381.csv',skip=4)
summary(area)

burn_bio <- read.csv('../ForecastData/Emissions_Land_Use_Burning_Biomass_E_All_data_NOFLAG.csv')
summary(burn_bio)
#averaging about 1000 NAs per year

land_cover <- read.csv('../ForecastData/Environment_LandCover_E_All_data_NOFLAG.csv')
summary(land_cover)
#only starts in 1992, about 500 missings (we should assess missing data by category; currently multi-row per country)

landuse <- read.csv('../ForecastData/Inputs_LandUse_E_All_data_NOFLAG.csv')
summary(landuse)
#averaging about 3500 missings in early years, decreases closer to the present (we should assess missing data by category; currently multi-row per country)

crop_raw <- read.csv('../ForecastData/Trade_Crops_Livestock_E_All_Data_NOFLAG.csv')
#subset down to export, soybeans, bovine meat, oil, palm palm kernal, oil, soybean
crop <- subset(crop_raw, Element.Code==5910 & Item.Code %in% c(236, 2071, 257, 258, 237))

fexport_raw <- read.csv('../ForecastData/Forestry_E_All_Data_NOFLAG.csv')
#subset down to Production, sawlogs, other industrial roundtree,pulpwood, and sawnwood
fexport <- subset(fexport_raw, Element.Code==5516 & Item.Code %in% c(1601,1602,1603,1604,1623,1626,1634,1633))

#start by turning forest area: this is the target and we need to match the rest of the data
#to this
forest_long <- gather(forest_area, Year, pct_forest, X1990:X2016, factor_key=TRUE)
col_subset <- c("Country.Code","Country.Name","Year","pct_forest")
#remove extra columns
forest_long2 <- forest_long[,names(forest_long) %in% col_subset]
#convert year to numeric
forest_long2$yrc <- lapply(forest_long2$Year, as.character)
forest_long2$yr <- as.numeric(substr(forest_long2$yrc,2,5))

#GDP and population follow the same pattern as above
gdp_long <- gather(gdp, Year, gdp, X1990:X2016, factor_key=TRUE)
col_subset <- c("Country.Code","Year","gdp")
gdp_long2 <- gdp_long[,names(gdp_long) %in% col_subset]
gdp_long2$yrc <- lapply(gdp_long2$Year, as.character)
gdp_long2$yr <- as.numeric(substr(gdp_long2$yrc,2,5))

pop_long <- gather(pop, Year, pop, X1990:X2016, factor_key=TRUE)
col_subset <- c("Country.Code","Year","pop")
pop_long2 <- pop_long[,names(pop_long) %in% col_subset]
pop_long2$yrc <- lapply(pop_long2$Year, as.character)
pop_long2$yr <- as.numeric(substr(pop_long2$yrc,2,5))

#arealong
area_long <- gather(area, Year, forest_area, X1990:X2016, factor_key = TRUE)
col_subset <- c("Country.Code","Year","forest_area")
area_long2 <- area_long[ ,names(area_long) %in% col_subset]
area_long2$yrc <- lapply(area_long2$Year, as.character)
area_long2$yr <- as.numeric(substr(area_long2$yrc,2,5))

#This function turns/spreads the FAO data sources
make_useable <- function(data) {
  #build new var names
  data$nvar <- paste('x',data$Item.Code,data$Element.Code,sep="_")
  data_long <- gather(data, Year, datapoint,Y1990:Y2016)
  #subset out extra columns
  col_subset <- c("Area","Year","datapoint","nvar")
  data_long2 <- data_long[ ,names(data_long) %in% col_subset]
  data_wide <- spread(data_long2,nvar,datapoint)
  data_wide$yrc <- lapply(data_wide$Year, as.character)
  data_wide$yr <- as.numeric(substr(data_wide$yrc,2,5))
  return(data_wide)
}

#this function makes a dictionary that explains what the variables mean
make_dict <- function(data) {
  data$nvar <- paste('x',data$Item.Code,data$Element.Code,sep="_")
  #keep only the dict columns
  col_subset <- c("nvar","Item","Element","Item.Code","Element.Code")
  data2 <- data[ ,names(data) %in% col_subset]
  dedup <- unique(data2)
  return(dedup)
}

fin_burn_bio <- make_useable(burn_bio)
burn_bio_dict <- make_dict(burn_bio)

#land cover data does not have 1990 or 1991 so make empty vars
land_cover$Y1990 <- NA
land_cover$Y1991 <- NA
#we need to reorder columns
col_order <- c("Area.Code"  ,  "Area"    ,     "Item.Code"   , "Item"     ,    "Element.Code", "Element" ,    
               "Unit","Y1990","Y1991" ,"Y1992"     ,   "Y1993"  ,      "Y1994"   ,     "Y1995"  ,     "Y1996"  ,     
               "Y1997"    ,    "Y1998"    ,    "Y1999"     ,   "Y2000"   ,     "Y2001"   ,     "Y2002" ,      
               "Y2003"     ,   "Y2004"    ,    "Y2005"    ,    "Y2006"    ,    "Y2007"   ,     "Y2008" ,      
               "Y2009"      ,  "Y2010"    ,    "Y2011"    ,    "Y2012"   ,     "Y2013"   ,     "Y2014" ,      
               "Y2015"    ,    "Y2016"    ,    "Y2017")
land_cover <- land_cover[ , col_order]
fin_land_cover <- make_useable(land_cover)
land_cover_dict <- make_dict(land_cover)
fin_land_use <- make_useable(landuse)
land_use_dict <- make_dict(landuse)
fin_crop <- make_useable(crop)
crop_dict <- make_dict(crop)
fin_export <- make_useable(fexport)
export_dict <- make_dict(fexport)

#Now we have to start merging

#target and gdp (5886 & 7128 obs)
step1 <- merge(forest_long2,gdp_long2, by=c("Country.Code","yr")) #5886

#step 1 and pop (5886 & 7128 obs)
step2 <- merge(step1, pop_long2, by =c("Country.Code","yr")) #5886

#forest area in sq. km
step2a <- merge(step2, area_long2, by=c("Country.Code","yr"))

#subset columns (we've picked up some extras)
col_subset <- c("Country.Code","Country.Name","yr","pct_forest","gdp","pop","forest_area")
step3 <- step2a[ ,names(step2a) %in% col_subset]

#now merge the FAO data together
#burn & land cover (7533 & 7587)
side1 <- merge(fin_burn_bio,fin_land_cover, by=c("Area","yr"), all = TRUE) #7344

#anti join will show use what is in one but not the other
only_burn <- anti_join(fin_burn_bio,fin_land_cover,by=c("Area","yr")) #some subcategories
only_land_coer <- anti_join(fin_land_cover,fin_burn_bio,by=c("Area","yr")) #mostly islands 

#side 1 & land use (7344 & 7398)
side2 <- merge(side1,fin_land_use, by=c("Area","yr"), all = TRUE) #7290 orig/ keep left 7344

#check our mismatches
only_side1 <- anti_join(side1,fin_land_use, by=c("Area","yr")) #Vatican & Monaco
only_land_use <- anti_join(fin_land_use,side1, by=c("Area","yr")) #USSR (which may be useful for missing Russia data)

#side 2 & crop
side2a <- merge(side2,fin_crop, by=c("Area","yr"), all=TRUE)
side2b <- merge(side2a,fin_export, by=c("Area","yr"),all=TRUE)

#drop some extraneous columns and do some create a new country name var that will be our combiner
col_drop <- c("Year.x","yrc","Year","yrc.y")
side3 <- side2b[ , !(names(side2b) %in% col_drop)]
side3 <- subset(side3, Area!="Belgium")

side3$countryname <- as.character(side3$Area)
#some fixes for better matches
side3$countryname <- ifelse(side3$countryname=="Bahamas","Bahamas, The",
                     ifelse(side3$countryname=="Bolivia (Plurinational State of)","Bolivia",
                     ifelse(side3$countryname=="CÙte d'Ivoire","Cote d'Ivoire",
                     ifelse(side3$countryname=="Democratic Republic of the Congo","Congo, Dem. Rep.",
                     ifelse(side3$countryname=="Czechoslovakia" & side3$yr <1993,"Czech Republic",
                     ifelse(side3$countryname=="Czechia" & side3$yr>=1993,"Czech Republic", 
                     ifelse(side3$countryname=="Egypt","Egypt, Arab Rep.",
                     ifelse(side3$countryname=="Micronesia","Micronesia, Fed. Sts.",
                     ifelse(side3$countryname=="Congo","Congo, Rep.",
                     ifelse(side3$countryname=="China, Hong Kong SAR", "Hong Kong SAR, China",
                     ifelse(side3$countryname=="Iran (Islamic Republic of)", "Iran, Islamic Rep.",
                     ifelse(side3$countryname=="Saint Kitts and Nevis", "St. Kitts and Nevis",
                     ifelse(side3$countryname=="Kyrgyzstan", "Kyrgyz Republic",
                     ifelse(side3$countryname=="Republic of Korea", "Korea, Rep.",
                     ifelse(side3$countryname=="Lao People's Democratic Republic", "Lao PDR",
                     ifelse(side3$countryname=="Gambia", "Gambia, The",
                     ifelse(side3$countryname=="Saint Lucia", "St. Lucia",
                     ifelse(side3$countryname=="China, Macao SAR", "Macao SAR, China",
                     ifelse(side3$countryname=="Moldova", "Republic of Maldova",
                     ifelse(side3$countryname=="Democratic People's Republic of Korea", "Korea, Dem. People’s Rep.",
                     ifelse(side3$countryname=="Slovakia", "Slovak Republic",
                     ifelse(side3$countryname=="United Republic of Tanzania", "Tanzania",
                     ifelse(side3$countryname=="United States of America", "United States",
                     ifelse(side3$countryname=="Saint Vincent and the Grenadines", "St. Vincent and the Grenadines",
                     ifelse(side3$countryname=="Venezuela (Bolivarian Republic of)","Venezuela, RB",
                     ifelse(side3$countryname=="United States Virgin Islands","Virgin Islands (U.S.)",
                     ifelse(side3$countryname=="Yemen","Yemen, Rep.",
                     ifelse(side3$countryname=="Viet Nam","Vietnam",
                     ifelse(side3$countryname=="Belgium-Luxembourg","Belgium",
                            side3$countryname)))))))))))))))))))))))))))))
step3$countryname <- as.character(step3$Country.Name)

#step1 try to merge them together (5886 & 7344)
step4 <- merge (step3,side3, by=c("countryname","yr"))
#first try 4968
#second 5049
#third 5183
#fourth 5373
#final 5670

only_step3 <- anti_join(step3,side3,by=c("countryname","yr")) #we'll save this
only_side3 <- anti_join(side3,step3,by=c("countryname","yr"))

#full dict
full_dict <- rbind(burn_bio_dict,land_use_dict,land_cover_dict,crop_dict,export_dict)

#output all our data
out4 <- apply(step4,2,as.character)
write.csv(out4, file="country_pred_data.csv")
write.csv(only_step3, file="extra_targets.csv")
write.csv(full_dict, file="Data_dictionary.csv")

