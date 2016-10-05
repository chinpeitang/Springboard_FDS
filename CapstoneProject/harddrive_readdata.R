# Springboard Foundations of Data Science Capstone Project
# Hard Drive Reliability and Failures
# Chinpei Tang

###########################################################
# This function is to read csv files and create data frames
###########################################################

# Load the required libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Change this for different year of 2013, 2014 or 2015. It will automatically read the appropriate data.
year <- 2015

dataVarName <- paste("hd", as.character(year), sep = "")
dataFileName <- dataVarName
  
# Clear the original data - just in case
if (exists(dataVarName)){
  rm(dataVarName)
}

if(T){
  # Set file path and list all the csv files based on different years (2013, 2014, 2015)
  filePathRoot <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/"
  filePathDir <- paste("data_", as.character(year), "/", as.character(year), sep = "") 
  filePath <- paste(filePathRoot, filePathDir, sep = "")
} else{
  filePath <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/data_2015/chunk120"
}
fileList <- list.files(path = filePath, pattern = "*.csv")
setwd(filePath)

# Column classes definition
# date, serial number, model, capacity in bytes, binary failure, 90 SMART data
#  not importing the 90 SMART data since they take up too much memory
if (year == 2015) {numSMART <- 90} else {numSMART <- 80}
colClassDef <- c("Date", "character", "character", "numeric", "integer", rep("NULL",numSMART))

# Time taken to read data is kept tracked
# Start clock
ptm_loadFile <- proc.time()

# Row combined the data frames, make the data "tidy"
# Used bind_rows instead of rbind (bind_row is more than 50% faster than rbind)
# Need to force the column class during importing,
#  otherwise there was issue with different classes when combining rows
#  It is argued that the import can be faster as well
for (i in 1:length(fileList)){
  temp_df <- read.csv(fileList[i], colClasses = colClassDef, header = T, sep = ",")
  temp_df$failure <- as.logical(temp_df$failure)
  
  # Create new column for brands
  # There is still issue when grepping all 365 data of a year, put a break somewhere?
  temp_df$brand <- "" #HUH?!
  temp_df$brand[grep(pattern = "HGST", x = temp_df$model)] = "HGST"
  temp_df$brand[grep(pattern = "Hitachi", x = temp_df$model)] = "Hitachi"
  temp_df$brand[grep(pattern = "SAMSUNG", x = temp_df$model)] = "Samsung"
  temp_df$brand[grep(pattern = "TOSHIBA", x = temp_df$model)] = "Toshiba"
  temp_df$brand[grep(pattern = "WDC", x = temp_df$model)] = "Western Digital"
  temp_df$brand[grep(pattern = "^ST", x = temp_df$model)] = "Seagate"
  
  # Convert capacity numbers into TB number
  # Pretty stupid codes - improve!
  temp_df$TB <- 0
  temp_df$TB[temp_df$capacity_bytes >= 8.0E10 & temp_df$capacity_bytes < 8.1E10] = 0.08
  temp_df$TB[temp_df$capacity_bytes >= 1.6E11 & temp_df$capacity_bytes < 1.7E11] = 0.16
  temp_df$TB[temp_df$capacity_bytes >= 2.5E11 & temp_df$capacity_bytes < 2.6E11] = 0.25
  temp_df$TB[temp_df$capacity_bytes >= 3.2E11 & temp_df$capacity_bytes < 3.3E11] = 0.32
  temp_df$TB[temp_df$capacity_bytes >= 5.0E11 & temp_df$capacity_bytes < 5.1E11] = 0.50
  temp_df$TB[temp_df$capacity_bytes >= 1.0E12 & temp_df$capacity_bytes < 1.1E12] = 1.00
  temp_df$TB[temp_df$capacity_bytes >= 1.5E12 & temp_df$capacity_bytes < 1.6E12] = 1.50
  temp_df$TB[temp_df$capacity_bytes >= 2.0E12 & temp_df$capacity_bytes < 2.1E12] = 2.00
  temp_df$TB[temp_df$capacity_bytes >= 3.0E12 & temp_df$capacity_bytes < 3.1E12] = 3.00
  temp_df$TB[temp_df$capacity_bytes >= 4.0E12 & temp_df$capacity_bytes < 4.1E12] = 4.00
  temp_df$TB[temp_df$capacity_bytes >= 5.0E12 & temp_df$capacity_bytes < 5.1E12] = 5.00
  temp_df$TB[temp_df$capacity_bytes >= 6.0E12 & temp_df$capacity_bytes < 6.1E12] = 6.00
  
  # Do some calculation to get useful details from this csv file
  # save to stat_df
  date_col <- temp_df[1,1]
  # total number of drives
  numDrive <- nrow(temp_df)
  # total number of failed drives
  numFailed <- colSums(temp_df[5])
  stat_temp_df <- data.frame(date_col, numDrive, numFailed)
  
  # TO DO!
  # count number of drives of each brand (use dplyr)
  # count number of drives of each size (use dplyr)
  # failure rate of each day for overall, each brand/size - how to define failure rate?
  # think what else can be computed in each csv prior doing more processing
  
  if (i == 1){
    # Initialize to create the first data frames
    dataVarName <- temp_df
    stat_df <- stat_temp_df
  } else{
    # Combine the rest of the data frames row-wise
    dataVarName <- bind_rows(dataVarName, temp_df)
    stat_df <- bind_rows(stat_df, stat_temp_df)
  }
}
# remove the temporary data frame
rm(temp_df)
rm(stat_temp_df)

# Stop clock
ptm_loadFile <- proc.time() - ptm_loadFile
ptm_loadFile

# List the data
tbl_df(dataVarName)
tbl_df(stat_df)

# Save the file
# Looks like it takes quite some times to save files, track the time too.
ptm_savefile <- proc.time()
# Save both combined data and calculated data
save(dataVarName, stat_df, file = paste(dataFileName, ".RData", sep = ""))
ptm_savefile <- proc.time() - ptm_savefile
ptm_savefile

# Plot the time series of the statistics over time
ggplot(stat_df, aes(x = date_col, y = numDrive, size = numFailed)) + 
  geom_point(alpha = 0.4)

# TO DO!
# Do more plots for different brands/sizes, may be layer them in the same plot?