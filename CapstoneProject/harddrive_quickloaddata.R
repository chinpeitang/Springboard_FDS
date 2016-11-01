################
# Load Libraries
################
library(dplyr)
library(ggplot2)
library(beepr)

rm(list = ls(all = TRUE))

########
# INPUTS
########
# What data file to create?
# Count only total number of drive and failure each day
ImportFunction <- "totalDriveCnt_FailureCnt"
# Survival analysis of full data
#ImportFunction <- "allSurvivalAnalysis"
# Include drive brands
#ImportFunction <- "totalBrandDriveCnt_FailureCnt"

# Test or production?
ProductionOrTest <- "Production"
#ProductionOrTest <- "Test"

# Change this for different year of 2013, 2014 or 2015. It will automatically read the appropriate data.
year <- 2013

##############################
# Select the correct file path
##############################

switch(ProductionOrTest,
  Production = {
    # Set file path and list all the csv files based on different years (2013, 2014, 2015)
    filePathRoot <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/"
    filePathDir <- paste("data_", as.character(year), "/", as.character(year), sep = "") 
    filePath <- paste(filePathRoot, filePathDir, sep = "")
    switch(ImportFunction,
      totalDriveCnt_FailureCnt = {
        fileName <- paste("/hd", as.character(year), "_totalDriveCnt_FailureCnt", ".RData", sep = "")
      },
      allSurvivalAnalysis = {
        fileName <- paste("/hd", as.character(year), "_allSurvivalAnalysis", ".RData", sep = "")        
      },
      stop("Internal Error")
    )
  },
  Test = {
    filePath <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/data2015_chunk15"
    #filePath <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/data2015_chunk60"
    switch(ImportFunction,
      totalDriveCnt_FailureCnt = {
      fileName <- "/hd2015_totalDriveCnt_FailureCnt.RData"
    },
      allSurvivalAnalysis = {
      fileName <- "/hd2015_allSurvivalAnalysis.RData"
    },
    stop("Internal Error")
    )
  },
  stop("Internal Error")
)

# Load the file
load(file = paste(filePath, fileName, sep = ""))

# Display and plot the data

switch(ImportFunction,
  totalDriveCnt_FailureCnt = {
    tbl_df(stat_totalDriveCnt_FailureCnt_df)
    
    ggplot(stat_totalDriveCnt_FailureCnt_df, aes(x = date_col, y = numDrive)) + 
      geom_line(size = 0.5) +
      scale_x_date("Date") +
      scale_y_discrete("Number of Drives")
    
    ggplot(stat_totalDriveCnt_FailureCnt_df, aes(x = date_col, y = numFailed)) + 
      geom_point() +
      scale_x_date("Date") +
      scale_y_discrete("Number of Failed Drives")
  },
  allSurvivalAnalysis = {
    tbl_df(data_allSurvivalAnalysis_df)  
  },
  stop("Internal Error")
)

beep()