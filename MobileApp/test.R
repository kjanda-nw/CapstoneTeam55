
ind <- read.csv("Combined_1991_2021_Adj.csv", stringsAsFactors = FALSE)
ind %>% 
  filter(yr >= 2013,
         yr <= 2016,
         countryname %in% "Brazil"
  ) %>% subset(select=c('yr','countryname',"forest_change","gdp"))


ftd <- melt(ind, id.vars = c("yr","countryname","Sub.Regions"))

ftd %>% 
  filter(yr >= 2013,
         yr <= 2021,
         countryname %in% "Brazil",
         variable == 'gdp'
  )
countries <- ind[,names(ind) %in% c("countryname","Sub.Regions")]
countries2 <- unique(countries)
