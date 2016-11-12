# Springboard Foundations of Data Science Capstone Project
# Hard Drive Reliability and Failures
# Chinpei Tang

#################################################################################
# This function is to save data for survival analysis for different size/capacity
#################################################################################

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

file2013 <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/data_2013/2013/hd2013_sizeSurvivalAnalysis.RData"
file2014 <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/data_2014/2014/hd2014_sizeSurvivalAnalysis.RData"
file2015 <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/data_2015/2015/hd2015_sizeSurvivalAnalysis.RData"
file2016Q1 <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/data_Q1_2016/data_Q1_2016/hdQ1_2016_sizeSurvivalAnalysis.RData"
file2016Q2 <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/data_Q2_2016/data_Q2_2016/hdQ2_2016_sizeSurvivalAnalysis.RData"

load(file = file2013)
data_sizeSurvivalAnalysis_df_2013 <- data_sizeSurvivalAnalysis_df
rm(data_sizeSurvivalAnalysis_df)

load(file = file2014)
data_sizeSurvivalAnalysis_df_2014 <- data_sizeSurvivalAnalysis_df
rm(data_sizeSurvivalAnalysis_df)

load(file = file2015)
data_sizeSurvivalAnalysis_df_2015 <- data_sizeSurvivalAnalysis_df
rm(data_sizeSurvivalAnalysis_df)

load(file = file2016Q1)
data_sizeSurvivalAnalysis_df_2016Q1 <- data_sizeSurvivalAnalysis_df
rm(data_sizeSurvivalAnalysis_df)

load(file = file2016Q2)
data_sizeSurvivalAnalysis_df_2016Q2 <- data_sizeSurvivalAnalysis_df
rm(data_sizeSurvivalAnalysis_df)

# Combine all data frames into one
data_sizeSurvivalAnalysis_df <- bind_rows(data_sizeSurvivalAnalysis_df_2013, data_sizeSurvivalAnalysis_df_2014, data_sizeSurvivalAnalysis_df_2015, data_sizeSurvivalAnalysis_df_2016Q1, data_sizeSurvivalAnalysis_df_2016Q2)

############################
# Create time and event data
############################

# Time/period
data_time <- data_sizeSurvivalAnalysis_df %>% 
  group_by(serial_number) %>% 
  summarise(n())

names(data_time) <- c("serial_number", "spell")

summary(data_time)

# Event
data_event <- data_sizeSurvivalAnalysis_df %>% 
  group_by(serial_number) %>% 
  summarise(max(failure))

names(data_event) <- c("serial_number", "event")

summary(data_event)

# size0p08TB
data_size0p08TB <- data_sizeSurvivalAnalysis_df %>% 
  group_by(serial_number) %>% 
  summarise(max(size0p08TB))
names(data_size0p08TB) <- c("serial_number", "size0p08TB")
summary(data_size0p08TB)

# size0p16TB
data_size0p16TB <- data_sizeSurvivalAnalysis_df %>% 
  group_by(serial_number) %>% 
  summarise(max(size0p16TB))
names(data_size0p16TB) <- c("serial_number", "size0p16TB")
summary(data_size0p16TB)

# size0p25TB
data_size0p25TB <- data_sizeSurvivalAnalysis_df %>% 
  group_by(serial_number) %>% 
  summarise(max(size0p25TB))
names(data_size0p25TB) <- c("serial_number", "size0p25TB")
summary(data_size0p25TB)

# size0p32TB
data_size0p32TB <- data_sizeSurvivalAnalysis_df %>% 
  group_by(serial_number) %>% 
  summarise(max(size0p32TB))
names(data_size0p32TB) <- c("serial_number", "size0p32TB")
summary(data_size0p32TB)

# size0p50TB
data_size0p50TB <- data_sizeSurvivalAnalysis_df %>% 
  group_by(serial_number) %>% 
  summarise(max(size0p50TB))
names(data_size0p50TB) <- c("serial_number", "size0p50TB")
summary(data_size0p50TB)

# size1p00TB
data_size1p00TB <- data_sizeSurvivalAnalysis_df %>% 
  group_by(serial_number) %>% 
  summarise(max(size1p00TB))
names(data_size1p00TB) <- c("serial_number", "size1p00TB")
summary(data_size1p00TB)

# size1p50TB
data_size1p50TB <- data_sizeSurvivalAnalysis_df %>% 
  group_by(serial_number) %>% 
  summarise(max(size1p50TB))
names(data_size1p50TB) <- c("serial_number", "size1p50TB")
summary(data_size1p50TB)

# size2p00TB
data_size2p00TB <- data_sizeSurvivalAnalysis_df %>% 
  group_by(serial_number) %>% 
  summarise(max(size2p00TB))
names(data_size2p00TB) <- c("serial_number", "size2p00TB")
summary(data_size2p00TB)

# size3p00TB
data_size3p00TB <- data_sizeSurvivalAnalysis_df %>% 
  group_by(serial_number) %>% 
  summarise(max(size3p00TB))
names(data_size3p00TB) <- c("serial_number", "size3p00TB")
summary(data_size3p00TB)

# size4p00TB
data_size4p00TB <- data_sizeSurvivalAnalysis_df %>% 
  group_by(serial_number) %>% 
  summarise(max(size4p00TB))
names(data_size4p00TB) <- c("serial_number", "size4p00TB")
summary(data_size4p00TB)

# size5p00TB
data_size5p00TB <- data_sizeSurvivalAnalysis_df %>% 
  group_by(serial_number) %>% 
  summarise(max(size5p00TB))
names(data_size5p00TB) <- c("serial_number", "size5p00TB")
summary(data_size5p00TB)

# size6p00TB
data_size6p00TB <- data_sizeSurvivalAnalysis_df %>% 
  group_by(serial_number) %>% 
  summarise(max(size6p00TB))
names(data_size6p00TB) <- c("serial_number", "size6p00TB")
summary(data_size6p00TB)

# Combine time and event into one data frame
allSizeSurvival <- 
left_join(
  left_join(
    left_join(
      left_join(
        left_join(
          left_join(
            left_join(
              left_join(
                left_join(
                  left_join(
                    left_join(
                      left_join(
                        left_join(data_time, data_event, by = "serial_number"),
                      data_size0p08TB, by = "serial_number"),
                    data_size0p16TB, by = "serial_number"),
                  data_size0p25TB, by = "serial_number"),
                data_size0p32TB, by = "serial_number"),
              data_size0p50TB, by = "serial_number"),
            data_size1p00TB, by = "serial_number"),
          data_size1p50TB, by = "serial_number"),
        data_size2p00TB, by = "serial_number"),
      data_size3p00TB, by = "serial_number"),
    data_size4p00TB, by = "serial_number"),
  data_size5p00TB, by = "serial_number"),
data_size6p00TB, by = "serial_number")

summary(allSizeSurvival)

allSizeSurvival$TB <- ""
allSizeSurvival$TB[allSizeSurvival$size0p08TB == 1] <- "0.08 TB"
allSizeSurvival$TB[allSizeSurvival$size0p16TB == 1] <- "0.16 TB"
allSizeSurvival$TB[allSizeSurvival$size0p25TB == 1] <- "0.25 TB"
allSizeSurvival$TB[allSizeSurvival$size0p32TB == 1] <- "0.32 TB"
allSizeSurvival$TB[allSizeSurvival$size0p50TB == 1] <- "0.50 TB"
allSizeSurvival$TB[allSizeSurvival$size1p00TB == 1] <- "1.00 TB"
allSizeSurvival$TB[allSizeSurvival$size1p50TB == 1] <- "1.50 TB"
allSizeSurvival$TB[allSizeSurvival$size2p00TB == 1] <- "2.00 TB"
allSizeSurvival$TB[allSizeSurvival$size3p00TB == 1] <- "3.00 TB"
allSizeSurvival$TB[allSizeSurvival$size4p00TB == 1] <- "4.00 TB"
allSizeSurvival$TB[allSizeSurvival$size5p00TB == 1] <- "5.00 TB"
allSizeSurvival$TB[allSizeSurvival$size6p00TB == 1] <- "6.00 TB"

save(allSizeSurvival, file = "C:/Users/Chinpei/Documents/R/HardDriveReliability/SurvivalAnalysisData/allSizeSurvival.RData")

beep()