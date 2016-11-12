# Springboard Foundations of Data Science Capstone Project
# Hard Drive Reliability and Failures
# Chinpei Tang

#####################################################################
# This function is to save data for survival analysis for all overall
#####################################################################

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

file2013 <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/data_2013/2013/hd2013_allSurvivalAnalysis.RData"
file2014 <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/data_2014/2014/hd2014_allSurvivalAnalysis.RData"
file2015 <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/data_2015/2015/hd2015_allSurvivalAnalysis.RData"
file2016Q1 <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/data_Q1_2016/data_Q1_2016/hdQ1_2016_allSurvivalAnalysis.RData"
file2016Q2 <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/data_Q2_2016/data_Q2_2016/hdQ2_2016_allSurvivalAnalysis.RData"

load(file = file2013)
data_allSurvivalAnalysis_df_2013 <- data_allSurvivalAnalysis_df
rm(data_allSurvivalAnalysis_df)

load(file = file2014)
data_allSurvivalAnalysis_df_2014 <- data_allSurvivalAnalysis_df
rm(data_allSurvivalAnalysis_df)

load(file = file2015)
data_allSurvivalAnalysis_df_2015 <- data_allSurvivalAnalysis_df
rm(data_allSurvivalAnalysis_df)

load(file = file2016Q1)
data_allSurvivalAnalysis_df_2016Q1 <- data_allSurvivalAnalysis_df
rm(data_allSurvivalAnalysis_df)

load(file = file2016Q2)
data_allSurvivalAnalysis_df_2016Q2 <- data_allSurvivalAnalysis_df
rm(data_allSurvivalAnalysis_df)

# Combine all data frames into one
data_allSurvivalAnalysis_df <- bind_rows(data_allSurvivalAnalysis_df_2013, data_allSurvivalAnalysis_df_2014, data_allSurvivalAnalysis_df_2015, data_allSurvivalAnalysis_df_2016Q1, data_allSurvivalAnalysis_df_2016Q2)

############################
# Create time and event data
############################

# Time/period
data_time <- data_allSurvivalAnalysis_df %>% 
  group_by(serial_number) %>% 
  summarise(n())

names(data_time) <- c("serial_number", "spell")

summary(data_time)

# Event
data_event <- data_allSurvivalAnalysis_df %>% 
  group_by(serial_number) %>% 
  summarise(max(failure))

names(data_event) <- c("serial_number", "event")

summary(data_event)

# Combine time and event into one data frame
allSurvival <- left_join(data_time, data_event, by = "serial_number")
summary(allSurvival)

save(allSurvival, file = "C:/Users/Chinpei/Documents/R/HardDriveReliability/SurvivalAnalysisData/allSurvival.RData")

beep()