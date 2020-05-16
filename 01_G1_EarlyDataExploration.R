#some initial file creation for forest area forecast

setwd("~/Documents/MSDS498/CapstoneTeam55")

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
                                                        "East Asia & Pacific (IDA & IBRD countries)","Early-demographic dividend")))
table(forest_area$Country.Name)

#1990 is the first available data year
#let's check the other datasets
gdp <- read.csv('../ForecastData/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_988718.csv',skip=4)
summary(gdp)
#miminal missing data: max is 43 NAs in 1990

pop <- read.csv('../ForecastData/API_SP.POP.TOTL_DS2_en_csv_v2_988606.csv',skip=4)
summary(pop)
#minimal missing data

burn_bio <- read.csv('../ForecastData/Emissions_Land_Use_Burning_Biomass_E_All_data_NOFLAG.csv')
summary(burn_bio)
#averaging about 1000 NAs per year

forest_land <- read.csv('../ForecastData/Emissions_Land_Use_Forest_Land_E_All_data_NOFLAG.csv')
summary(forest_land)
#averaging abou 100 NAs per year

land_cover <- read.csv('../ForecastData/Environment_LandCover_E_All_data_NOFLAG.csv')
summary(land_cover)
#only starts in 1992, about 500 missings (we should assess missing data by category; currently multi-row per country)

landuse <- read.csv('../ForecastData/Inputs_LandUse_E_All_data_NOFLAG.csv')
summary(landuse)
#averaging about 3500 missings in early years, decreases closer to the present (we should assess missing data by category; currently multi-row per country)

library(ggplot2)
library(dplyr)
library(tidyr)

forest_long <- gather(forest_area, Year, pct_forest, X1990:X2016, factor_key=TRUE)
col_subset <- c("Country.Code","Country.Name","Year","pct_forest")
forest_long2 <- forest_long[,names(forest_long) %in% col_subset]
forest_long2$yrc <- lapply(forest_long2$Year, as.character)
forest_long2$yr <- as.numeric(substr(forest_long2$yrc,2,5))
par(mfrow=c(1,3))
#create a subset for interesting plotting
subdata <- subset(forest_long2, Country.Code %in% c("TGO",
                                                    "UGA",
                                                    "NGA",
                                                    "PAK",
                                                    "HND",
                                                    "TCD",
                                                    "PRK",
                                                    "ZWE",
                                                    "PRY",
                                                    "MMR"))

p <- ggplot(subdata, aes(x=yr, y=pct_forest, group=Country.Name, colour = Country.Name)) +
      geom_line() +
      labs(title = "Forest area (% of land area)",
           x="Year",
           y="% of land area")
p

gdp_long <- gather(gdp, Year, gdp, X1990:X2016, factor_key=TRUE)
col_subset <- c("Country.Code","Country.Name","Year","gdp")
gdp_long2 <- gdp_long[,names(gdp_long) %in% col_subset]
gdp_long2$yrc <- lapply(gdp_long2$Year, as.character)
gdp_long2$yr <- as.numeric(substr(gdp_long2$yrc,2,5))

#create a subset for interesting plotting
subdata <- subset(gdp_long2, Country.Code %in% c("TGO",
                                                    "UGA",
                                                    "NGA",
                                                    "PAK",
                                                    "HND",
                                                    "TCD",
                                                    #"PRK",
                                                    "ZWE",
                                                    "PRY",
                                                    "MMR"))

dat <- as.data.frame(scale(subdata[,c(4)]))
subdata2 <- cbind(subdata,dat)
p <- ggplot(subdata2, aes(x=yr, y=V1, group=Country.Name, colour = Country.Name)) +
  geom_line() +
  labs(title = "GDP (1990-2016) Scaled",
       x="Year",
       y="GDP (scaled)")
p
pop_long <- gather(pop, Year, pop, X1990:X2016, factor_key=TRUE)
col_subset <- c("Country.Code","Country.Name","Year","pop")
pop_long2 <- pop_long[,names(pop_long) %in% col_subset]
pop_long2$yrc <- lapply(pop_long2$Year, as.character)
pop_long2$yr <- as.numeric(substr(pop_long2$yrc,2,5))

#create a subset for interesting plotting
subdata <- subset(pop_long2, Country.Code %in% c("TGO",
                                                 "UGA",
                                                 "NGA",
                                                 "PAK",
                                                 "HND",
                                                 "TCD",
                                                 "PRK",
                                                 "ZWE",
                                                 "PRY",
                                                 "MMR"))

dat <- as.data.frame(scale(subdata[,c(4)]))
subdata2 <- cbind(subdata,dat)
p <- ggplot(subdata2, aes(x=yr, y=V1, group=Country.Name, colour = Country.Name)) +
  geom_line() +
  labs(title = "Population (1990-2016) Scaled",
       x="Year",
       y="Population (scaled)")
p