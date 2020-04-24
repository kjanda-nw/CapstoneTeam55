#some initial file creation for forest area forecast

setwd("~/Documents/MSDS498/CapstoneTeam55")

#read in forest area data to identify data window
forest_area <- read.csv('../ForecastData/API_AG.LND.FRST.ZS_DS2_en_csv_v2_935916.csv',skip=4)
summary(forest_area)

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