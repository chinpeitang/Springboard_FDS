################
# Load Libraries
################
library(ggplot2)
library(dplyr)

rm(list = ls(all = TRUE))

# Load the required libraries
file2013 <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/data_2013/2013/hd2013_totalDriveCnt_FailureCnt.RData"
file2014 <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/data_2014/2014/hd2014_totalDriveCnt_FailureCnt.RData"
file2015 <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/data_2015/2015/hd2015_totalDriveCnt_FailureCnt.RData"

load(file = file2013)
stat_totalDriveCnt_FailureCnt_df_2013 <- stat_totalDriveCnt_FailureCnt_df
rm(stat_totalDriveCnt_FailureCnt_df)

load(file = file2014)
stat_totalDriveCnt_FailureCnt_df_2014 <- stat_totalDriveCnt_FailureCnt_df
rm(stat_totalDriveCnt_FailureCnt_df)

load(file = file2015)
stat_totalDriveCnt_FailureCnt_df_2015 <- stat_totalDriveCnt_FailureCnt_df
rm(stat_totalDriveCnt_FailureCnt_df)

stat_totalDriveCnt_FailureCnt_df <- bind_rows(stat_totalDriveCnt_FailureCnt_df_2013, stat_totalDriveCnt_FailureCnt_df_2014, stat_totalDriveCnt_FailureCnt_df_2015)

#Plot it
ggplot(stat_totalDriveCnt_FailureCnt_df, aes(x = date_col, y = numDrive, size = numFailed)) + 
  geom_point(alpha = 0.4)

ggplot(stat_totalDriveCnt_FailureCnt_df, aes(x = date_col, y = numDrive)) + 
  geom_line(size = 0.5) +
  scale_x_date("Date") +
  scale_y_discrete("Number of Drives")

ggplot(stat_totalDriveCnt_FailureCnt_df, aes(x = date_col, y = numFailed)) + 
  geom_point() +
  scale_x_date("Date") +
  scale_y_discrete("Number of Failed Drives")