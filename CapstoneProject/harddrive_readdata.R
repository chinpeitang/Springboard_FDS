# Springboard Foundations of Data Science Capstone Project
# Hard Drive Reliability and Failures
# Chinpei Tang

##########################################################
# This function is to read csv files and create data frame
##########################################################

# Load the required libraries
library(dplyr)
library(tidyr)

# Change this for different year of 2013, 2014 or 2015. It will automatically read the appropriate data.
year <- 2015

dataVarName <- paste("hd", as.character(year), sep = "")
dataFileName <- dataVarName
  
# Clear the original data - just in case
if (exists(dataVarName)){
  rm(dataVarName)
}

# Set file path and list all the csv files based on different years (2013, 2014, 2015)
filePathRoot <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/"
filePathDir <- paste("data_", as.character(year), "/", as.character(year), sep = "") 
filePath <- paste(filePathRoot, filePathDir, sep = "")
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
  if (i == 1){
    # Initialize to create the first data frame
    dataVarName <- temp_df
  } else{
    # Combine the rest of the data frames row-wise
    dataVarName <- bind_rows(dataVarName, temp_df)
  }
}
# remove the temporary data frame
rm(temp_df)

# Stop clock
ptm_loadFile <- proc.time() - ptm_loadFile
ptm_loadFile

# List the data
tbl_df(dataVarName)

# Save the file
# Looks like it takes quite some times to save files, track the time too.
ptm_savefile <- proc.time()
save(dataVarName, file = paste(dataFileName, ".RData", sep = ""))
ptm_savefile <- proc.time() - ptm_savefile
ptm_savefile