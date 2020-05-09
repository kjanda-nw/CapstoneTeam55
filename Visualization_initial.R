library(ggplot2)

setwd("~/Documents/MSDS498/CapstoneTeam55")
#read in the data
df <- read.csv("country_pred_data.csv")

#subset to 2016;
df16 <- subset(df, yr==2016)

par(mfrow=c(1,2))
#forest area
p <- ggplot(df16, aes(x=forest_area)) + 
     geom_histogram(binwidth = 500000, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
     ggtitle("Histogram of 2016 Forest Area") + 
     theme_light() +
     theme (plot.title = element_text(size=15))
p
#pct forest
p <- ggplot(df16, aes(x=pct_forest)) + 
  geom_histogram(binwidth = 10, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Histogram of 2016 Percent Forest") + 
  theme_light() +
  theme (plot.title = element_text(size=15))
p

#delta forest
df <- read.csv("country_pred_delta_forest.csv")
dfd16 <- subset(df,yr==2016)
p <- ggplot(dfd16, aes(x=delta_forest_area)) + 
  geom_histogram(binwidth = 0.01, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Histogram of 2015-16 Change in Forest Area") + 
  theme_light() +
  theme (plot.title = element_text(size=15))
p
