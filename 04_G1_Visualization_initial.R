library(ggplot2)

setwd("~/Documents/MSDS498/CapstoneTeam55")
#read in the data
df <- read.csv("./Data/country_pred_data.csv")

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
df <- read.csv("./Data/country_pred_delta_forest.csv")
dfd16 <- subset(df,yr==2016)
p <- ggplot(dfd16, aes(x=delta_forest_area)) + 
  geom_histogram(binwidth = 0.01, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Histogram of 2015-16 Change in Forest Area") + 
  theme_light() +
  theme (plot.title = element_text(size=15))
p

#boxplots for categorical data
ggplot(df, aes(x=IncomeGroup.x, y=delta_forest_area))  +
  geom_boxplot(fill="#69b3a2",  alpha=0.9) +
  ggtitle("Boxplot of Change in Forest Area by Income Group") +
  theme_light() + 
  theme (plot.title = element_text(size=15))  
ggplot(df, aes(x=Sub.Regions.x, y=delta_forest_area))  +
    geom_boxplot(fill="#69b3a2",  alpha=0.9) +
    ggtitle("Boxplot of Change in Forest Area by Sub-Region") +
  theme_light() + 
    theme (plot.title = element_text(size=15), axis.text.x = element_text(angle = 60, hjust = 1))  
#convert binaries to factors for plotting
df$Commodity.driven.deforestation <- ifelse(df$Commodity.driven.deforestation.x==1,"Yes","No")
df$Forestry <- ifelse(df$Forestry.x==1,"Yes","No")
df$Wildfire <- ifelse(df$Wildfire.x==1,"Yes","No")
df$Shifting.agriculture <- ifelse(df$Shifting.agriculture.x==1,"Yes","No")
ggplot(df, aes(x=Commodity.driven.deforestation, y=delta_forest_area))  +
  geom_boxplot(fill="#69b3a2",  alpha=0.9) +
  ggtitle("Boxplot of Change in Forest Area by Commodity Driven Deforestation") +
  theme_light() + 
  theme (plot.title = element_text(size=15), axis.text.x = element_text(angle = 90, hjust = 1))  
ggplot(df, aes(x=Forestry, y=delta_forest_area))  +
  geom_boxplot(fill="#69b3a2",  alpha=0.9) +
  ggtitle("Boxplot of Change in Forest Area by Forestry") +
  theme_light() + 
  theme (plot.title = element_text(size=15), axis.text.x = element_text(angle = 90, hjust = 1))  
ggplot(df, aes(x=Wildfire, y=delta_forest_area))  +
  geom_boxplot(fill="#69b3a2",  alpha=0.9) +
  ggtitle("Boxplot of Change in Forest Area by Wildfire") +
  theme_light() + 
  theme (plot.title = element_text(size=15), axis.text.x = element_text(angle = 90, hjust = 1))  
ggplot(df, aes(x=Shifting.agriculture, y=delta_forest_area))  +
  geom_boxplot(fill="#69b3a2",  alpha=0.9) +
  ggtitle("Boxplot of Change in Forest Area by Shifting.agriculture") +
  theme_light() + 
  theme (plot.title = element_text(size=15), axis.text.x = element_text(angle = 90, hjust = 1))  

df$topf <- ifelse(df$forestgroup==1,"High",ifelse(df$forestgroup==2,"Mid",ifelse(df$forestgroup==3,"Low","Other")))
df$topp <- ifelse(df$popgroup==1,"High",ifelse(df$popgroup==2,"Mid",ifelse(df$popgroup==3,"Low","Other")))
df$topg <- ifelse(df$gdpgroup==1,"High",ifelse(df$gdpgroup==2,"Mid",ifelse(df$gdpgroup==3,"Low","Other")))
dfd16 <- subset(df,yr==2016)
ggplot(dfd16, aes(x=topf, y=delta_forest_area))  +
  geom_boxplot(fill="#69b3a2",  alpha=0.9) +
  ggtitle("Boxplot of Change in Forest Area by Percent Forest grouping") +
  theme_light() + 
  theme (plot.title = element_text(size=15), axis.text.x = element_text(angle = 90, hjust = 1))  
ggplot(dfd16, aes(x=topp, y=delta_forest_area))  +
  geom_boxplot(fill="#69b3a2",  alpha=0.9) +
  ggtitle("Boxplot of Change in Forest Area by Population grouping") +
  theme_light() + 
  theme (plot.title = element_text(size=15), axis.text.x = element_text(angle = 90, hjust = 1))  
ggplot(dfd16, aes(x=topf, y=delta_forest_area))  +
  geom_boxplot(fill="#69b3a2",  alpha=0.9) +
  ggtitle("Boxplot of Change in Forest Area by GDP grouping") +
  theme_light() + 
  theme (plot.title = element_text(size=15), axis.text.x = element_text(angle = 90, hjust = 1))  


ggplot(dfd16, aes(x=Sub.Regions.x))  +
  geom_bar(fill="#69b3a2",  alpha=0.9) +
  ggtitle("Barchart of Country Sub-Region") +
  theme_light() + 
  theme (plot.title = element_text(size=15), axis.text.x = element_text(angle = 60, hjust = 1))  
