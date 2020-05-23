header = '
MSDS 498
Spring 2020
Team 55 - Forest Tracker

Purpose: The purpose of this program is to do the final post-processing of the forest data.
'

#list of needed libraries
library(dplyr)

#set working directory
setwd("~/Documents/MSDS498/CapstoneTeam55")

#read in the data and post-process the predictions with no COVID adjustment
df_n <- read.csv("./Data/Data_Pred_Noadj.csv")
df_n_fm <- df_n[,!(names(df_n) %in% c("X","delta_forest_area"))]
known_n <- df_n[,names(df_n) %in% c("countryname","yr","delta_forest_area")]
known_n2 <- subset(known_n,yr<2017)

pred17_n <- read.csv("./Data/Predictive_model_Predictions_Predictive_model_Data_Pred_Noadj_(19)_2017.csv")
pred18_n <- read.csv("./Data/Predictive_model_Predictions_Predictive_model_Data_Pred_Noadj_(19)_2018.csv")
pred19_n <- read.csv("./Data/Predictive_model_Predictions_Predictive_model_Data_Pred_Noadj_(19)_2019.csv")
pred20_n <- read.csv("./Data/Predictive_model_Predictions_Predictive_model_Data_Pred_Noadj_(19)_2020.csv")
pred21_n <- read.csv("./Data/Predictive_model_Predictions_Predictive_model_Data_Pred_Noadj_(19)_2021.csv")

#stack predictions
pred_n <- rbind(pred17_n,pred18_n,pred19_n,pred20_n,pred21_n)
pred_n <- pred_n[,names(pred_n) %in% c("countryname","yr","Prediction")]
pred_n <- pred_n %>% rename(delta_forest_area=Prediction)
pred_n <- rbind(known_n2,pred_n)

#merge with the original data
df_n2 <- merge(df_n_fm,pred_n,by=c("countryname","yr"),all.x="True")

#build interim forest features
df_n2$forest_change <- df_n2$forest_area - df_n2$lag_forest_area

#build country list
countries <- as.character(unique(df_n2$countryname))

new2 <- df_n2
new2 <- new2[FALSE, ]
#DO THE Processing
for (c in countries) {
  build <- subset(df_n2,countryname==c)
  #update
  for (i in 27:nrow(build)) {
    build$forest_change[i] <- build$forest_area[i-1]*build$delta_forest_area[i]
    build$forest_area[i] <- build$forest_change[i] + build$forest_area[i-1]
    build$lag_forest_area[i] <- build$forest_area[i-1]
  }
  #add to the empty dataset
  new2 <- rbind(new2,build)
}

new2$abs_forest_change <- abs(new2$forest_change)

new2 <- new2[,!(names(new2) == "pct_forest")]
write.csv(new2,"./Data/Combined_1991_2021_noAdj.csv")


#read in the data and post-process the predictions with COVID adjustment
df <- read.csv("./Data/Data_Pred_adj.csv")
df_fm <- df[,!(names(df) %in% c("X","delta_forest_area"))]
known <- df[,names(df) %in% c("countryname","yr","delta_forest_area")]
known2 <- subset(known,yr<2017)

pred17 <- read.csv("./Data/Predictive_model_Predictions_Predictive_model_Data_Pred_adj_(3)_2017.csv")
pred18 <- read.csv("./Data/Predictive_model_Predictions_Predictive_model_Data_Pred_adj_(3)_2018.csv")
pred19 <- read.csv("./Data/Predictive_model_Predictions_Predictive_model_Data_Pred_adj_(3)_2019.csv")
pred20 <- read.csv("./Data/Predictive_model_Predictions_Predictive_model_Data_Pred_adj_(3)_2020.csv")
pred21 <- read.csv("./Data/Predictive_model_Predictions_Predictive_model_Data_Pred_adj_(3)_2021.csv")

#stack predictions
pred <- rbind(pred17,pred18,pred19,pred20,pred21)
pred <- pred[,names(pred) %in% c("countryname","yr","Prediction")]
pred <- pred %>% rename(delta_forest_area=Prediction)
pred <- rbind(known2,pred)

#merge with the original data
df2 <- merge(df_fm,pred,by=c("countryname","yr"),all.x="True")

#build interim forest features
df2$forest_change <- df2$forest_area - df2$lag_forest_area

#build country list
countries <- as.character(unique(df2$countryname))

new3 <- df2
new3 <- new3[FALSE, ]
#DO THE Processing
for (c in countries) {
  build <- subset(df2,countryname==c)
  #update
  for (i in 27:nrow(build)) {
    build$forest_change[i] <- build$forest_area[i-1]*build$delta_forest_area[i]
    build$forest_area[i] <- build$forest_change[i] + build$forest_area[i-1]
    build$lag_forest_area[i] <- build$forest_area[i-1]
  }
  #add to the empty dataset
  new3 <- rbind(new3,build)
}

new3$abs_forest_change <- abs(new3$forest_change)

new3 <- new3[,!(names(new3) == "pct_forest")]
write.csv(new3,"./Data/Combined_1991_2021_Adj.csv")