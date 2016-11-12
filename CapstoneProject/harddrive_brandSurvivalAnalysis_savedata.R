# Springboard Foundations of Data Science Capstone Project
# Hard Drive Reliability and Failures
# Chinpei Tang

######################################################################################
# This function is to save data for survival analysis for different brand/manufacturer
######################################################################################

################
# Load Libraries
################
library(dplyr)
library(survival)
library(beepr)

rm(list = ls(all = TRUE))

############
# Load Files
############

file2013 <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/data_2013/2013/hd2013_brandSurvivalAnalysis.RData"
file2014 <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/data_2014/2014/hd2014_brandSurvivalAnalysis.RData"
file2015 <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/data_2015/2015/hd2015_brandSurvivalAnalysis.RData"
file2016Q1 <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/data_Q1_2016/data_Q1_2016/hdQ1_2016_brandSurvivalAnalysis.RData"
file2016Q2 <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/data_Q2_2016/data_Q2_2016/hdQ2_2016_brandSurvivalAnalysis.RData"

load(file = file2013)
data_brandSurvivalAnalysis_df_2013 <- data_brandSurvivalAnalysis_df
rm(data_brandSurvivalAnalysis_df)

load(file = file2014)
data_brandSurvivalAnalysis_df_2014 <- data_brandSurvivalAnalysis_df
rm(data_brandSurvivalAnalysis_df)

load(file = file2015)
data_brandSurvivalAnalysis_df_2015 <- data_brandSurvivalAnalysis_df
rm(data_brandSurvivalAnalysis_df)

load(file = file2016Q1)
data_brandSurvivalAnalysis_df_2016Q1 <- data_brandSurvivalAnalysis_df
rm(data_brandSurvivalAnalysis_df)

load(file = file2016Q2)
data_brandSurvivalAnalysis_df_2016Q2 <- data_brandSurvivalAnalysis_df
rm(data_brandSurvivalAnalysis_df)

# Combine all data frames into one
data_brandSurvivalAnalysis_df <- bind_rows(data_brandSurvivalAnalysis_df_2013, data_brandSurvivalAnalysis_df_2014, data_brandSurvivalAnalysis_df_2015, data_brandSurvivalAnalysis_df_2016Q1, data_brandSurvivalAnalysis_df_2016Q2)

############################
# Create time and event data
############################

# Time/period
data_time <- data_brandSurvivalAnalysis_df %>% 
  group_by(serial_number) %>% 
  summarise(n())

names(data_time) <- c("serial_number", "spell")

summary(data_time)

# Event
data_event <- data_brandSurvivalAnalysis_df %>% 
  group_by(serial_number) %>% 
  summarise(max(failure))

names(data_event) <- c("serial_number", "event")

summary(data_event)

# HGST
data_HGST <- data_brandSurvivalAnalysis_df %>% 
  group_by(serial_number) %>% 
  summarise(max(HGST))
names(data_HGST) <- c("serial_number", "HGST")
summary(data_HGST)

# Hitachi
data_Hitachi <- data_brandSurvivalAnalysis_df %>% 
  group_by(serial_number) %>% 
  summarise(max(Hitachi))
names(data_Hitachi) <- c("serial_number", "Hitachi")
summary(data_Hitachi)

# Samsung
data_Samsung <- data_brandSurvivalAnalysis_df %>% 
  group_by(serial_number) %>% 
  summarise(max(Samsung))
names(data_Samsung) <- c("serial_number", "Samsung")
summary(data_Samsung)

# Toshiba
data_Toshiba <- data_brandSurvivalAnalysis_df %>% 
  group_by(serial_number) %>% 
  summarise(max(Toshiba))
names(data_Toshiba) <- c("serial_number", "Toshiba")
summary(data_Toshiba)

# WDC
data_WDC <- data_brandSurvivalAnalysis_df %>% 
  group_by(serial_number) %>% 
  summarise(max(WDC))
names(data_WDC) <- c("serial_number", "WDC")
summary(data_WDC)

# Seagate
data_Seagate <- data_brandSurvivalAnalysis_df %>% 
  group_by(serial_number) %>% 
  summarise(max(Seagate))
names(data_Seagate) <- c("serial_number", "Seagate")
summary(data_Seagate)

# Combine time and event into one data frame
allBrandSurvival <-  left_join(
  left_join(
    left_join(
      left_join(
        left_join(
          left_join(
            left_join(data_time, data_event, by = "serial_number"), 
            data_HGST, by = "serial_number"),
          data_Hitachi, by = "serial_number"),
        data_Samsung, by = "serial_number"),
      data_Toshiba, by = "serial_number"),
    data_WDC, by = "serial_number"),
  data_Seagate, by = "serial_number")

summary(allBrandSurvival)

allBrandSurvival$brand <- ""
allBrandSurvival$brand[allBrandSurvival$HGST == 1] <- "HGST"
allBrandSurvival$brand[allBrandSurvival$Hitachi == 1] <- "Hitachi"
allBrandSurvival$brand[allBrandSurvival$Samsung == 1] <- "Samsung"
allBrandSurvival$brand[allBrandSurvival$Toshiba == 1] <- "Toshiba"
allBrandSurvival$brand[allBrandSurvival$WDC == 1] <- "WDC"
allBrandSurvival$brand[allBrandSurvival$Seagate == 1] <- "Seagate"

save(allBrandSurvival, file = "C:/Users/Chinpei/Documents/R/HardDriveReliability/SurvivalAnalysisData/allBrandSurvival.RData")

beep()