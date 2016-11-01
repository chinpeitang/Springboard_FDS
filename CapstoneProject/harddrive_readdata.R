# Springboard Foundations of Data Science Capstone Project
# Hard Drive Reliability and Failures
# Chinpei Tang

###########################################################
# This function is to read csv files and create data frames
###########################################################

################
# Load Libraries
################
library(dplyr)
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
#ImportFunction <- "otherwise"

# Test or production?
ProductionOrTest <- "Production"
#ProductionOrTest <- "Test"

# Change this for different year of 2013, 2014 or 2015. It will automatically read the appropriate data.
year <- 2013

# Where the root of the data is (which can be adjusted)
filePathRoot <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/"

###############################
# File path and file name setup
###############################

# Select the correct file path
# Production if taking from the 2013, 2014 or 2015 directories
# Test if taking from the data2015_chunkXX directories with smaller set of data
switch(ProductionOrTest,
  Production = {
    # Set file path and list all the csv files based on different years (2013, 2014, 2015)
    filePathDir <- paste("data_", as.character(year), "/", as.character(year), sep = "") 
    filePath <- paste(filePathRoot, filePathDir, sep = "")
  },
  Test = {
    year <- 2015 # force year to 2015
    filePath <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/data2015_chunk15"
    #filePath <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/data2015_chunk60"
    #filePath <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/data2015_chunk90"
  },
  stop("Internal Error")
)
setwd(filePath)
# Gather the file list with only csv files
fileList <- list.files(path = filePath, pattern = "*.csv")
# Number of files
numFile <- length(fileList)

# File name in the format of hdyyyy
dataVarName <- paste("hd", as.character(year), sep = "")
dataFileName <- dataVarName

###########################
# Column classes definition
###########################
# date, serial number, model, capacity in bytes, binary failure, 90 SMART data

# 2015 has 90 SMART, 2013 and 2014 have 80 SMART
if (year == 2015) {numSMART <- 90} else {numSMART <- 80}

# Importing all data takes up too much memory, select only necessary data
# Select the corresponding columns to input. "NULL" those are not interested.
switch(ImportFunction,
  totalDriveCnt_FailureCnt = {
    # Count only total number of drive and failure each day:
    # No need serial number, model, and capability in bytes
    colClassDef <- c("Date", "NULL", "NULL", "NULL", "integer", rep("NULL",numSMART))
  },
  allSurvivalAnalysis = {
    # Take serial number and failure for full survival analysis:
    colClassDef <- c("NULL", "character", "NULL", "NULL", "integer", rep("NULL",numSMART))
  },
  totalBrandDriveCnt_FailureCnt = {
    # Count with model total number of drive and failure each day:
    colClassDef <- c("Date", "NULL", "character", "NULL", "integer", rep("NULL",numSMART))
  },
  otherwise = {
    # Take everything but no SMART
    colClassDef <- c("Date", "character", "character", "numeric", "integer", rep("NULL",numSMART))
  },
  stop("Internal Error")
) # end switch

#########################################
# Start importing and creating data frame
#########################################
# Time taken to read data is kept tracked
# Start clock
ptm_loadFile <- proc.time()
# create progress bar
pb <- txtProgressBar(min = 0, max = numFile, style = 3)

for (i in 1:numFile) {

  # Need to force the column class during importing,
  #  otherwise there was issue with different classes when combining rows
  
  # Create temporary data frame importing the corresponding columns
  temp_df <- read.csv(fileList[i], colClasses = colClassDef, header = T, sep = ",")
  # If failure data is important, convert integer to binary to save memory
  temp_df$failure <- as.logical(temp_df$failure)
  
  # Create data frame based on different import functions
  switch(ImportFunction,
    
    totalDriveCnt_FailureCnt = {
      # get date of the day
      date_col <- temp_df[1,1]
      # get total number of drives of the day
      numDrive <- nrow(temp_df)
      # get total number of failed drive of the day
      numFailed <- colSums(temp_df[2])
      # Collect the calculated data into stat data frame
      stat_totalDriveCnt_FailureCnt_temp_df <- data.frame(date_col, numDrive, numFailed)
    }, # end totalDriveCnt_FailureCnt
  
    allSurvivalAnalysis = {
      data_allSurvivalAnalysis_temp_df <- temp_df
    }, # end allSurvivalAnalysis
  
    totalBrandDriveCnt_FailureCnt = {
      # Do factoring for hard drive models and sizes
      
      # Create new column for brands
      # There is still issue when grepping all 365 data of a year, put a break somewhere?
      #  - this doesn't matter
      # Note: do factor instead of brand name, character is taking too much memory
      #temp_df$brand <- "" #HUH?!
      #temp_df$brand[grep(pattern = "HGST", x = temp_df$model)] = as.factor(1)
      #temp_df$brand[grep(pattern = "Hitachi", x = temp_df$model)] = as.factor(2)
      #temp_df$brand[grep(pattern = "SAMSUNG", x = temp_df$model)] = as.factor(3)
      #temp_df$brand[grep(pattern = "TOSHIBA", x = temp_df$model)] = as.factor(4)
      #temp_df$brand[grep(pattern = "WDC", x = temp_df$model)] = as.factor(5)
      #temp_df$brand[grep(pattern = "^ST", x = temp_df$model)] = as.factor(6)
  
      temp_df$HGST <- F
      temp_df$Hitachi <- F
      temp_df$Samsung <- F
      temp_df$Toshiba <- F
      temp_df$WDC <- F
      temp_df$Seagate <- F
      
      temp_df$HGST[grep(pattern = "HGST", x = temp_df$model)] <- T
      temp_df$Hitachi[grep(pattern = "Hitachi", x = temp_df$model)] <- T
      temp_df$Samsung[grep(pattern = "SAMSUNG", x = temp_df$model)] <- T
      temp_df$Toshiba[grep(pattern = "TOSHIBA", x = temp_df$model)] <- T
      temp_df$WDC[grep(pattern = "WDC", x = temp_df$model)] <- T
      temp_df$Seagate[grep(pattern = "^ST", x = temp_df$model)] <- T
      
      # Convert capacity numbers into TB number
      # Note: delete the capacity_bytes after creating a new column, taking too much memory
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
      
      if(F) {
        # count number of drives of each brand (use dplyr)
        numHGST <- temp_df %>% filter(brand == "HGST")  %>% nrow()
        numHitachi <- temp_df %>% filter(brand == "Hitachi")  %>% nrow()
        numSamsung <- temp_df %>% filter(brand == "Samsung")  %>% nrow()
        numToshiba <- temp_df %>% filter(brand == "Toshiba")  %>% nrow()
        numWDC <- temp_df %>% filter(brand == "Western Digital")  %>% nrow()
        numSeagate <- temp_df %>% filter(brand == "Seagate")  %>% nrow()
        
        # count number of drives of each size (use dplyr)
        num0p08TB <- temp_df %>% filter(TB == 0.08) %>% nrow()
        num0p16TB <- temp_df %>% filter(TB == 0.16) %>% nrow()
        num0p25TB <- temp_df %>% filter(TB == 0.25) %>% nrow()
        num0p32TB <- temp_df %>% filter(TB == 0.32) %>% nrow()
        num0p50TB <- temp_df %>% filter(TB == 0.50) %>% nrow()
        num1p00TB <- temp_df %>% filter(TB == 1.00) %>% nrow()
        num1p50TB <- temp_df %>% filter(TB == 1.50) %>% nrow()
        num2p00TB <- temp_df %>% filter(TB == 2.00) %>% nrow()
        num3p00TB <- temp_df %>% filter(TB == 3.00) %>% nrow()
        num4p00TB <- temp_df %>% filter(TB == 4.00) %>% nrow()
        num5p00TB <- temp_df %>% filter(TB == 5.00) %>% nrow()
        num6p00TB <- temp_df %>% filter(TB == 6.00) %>% nrow()
        
        # TO DO!
        # failure rate of each day for overall, each brand/size - how to define failure rate?
        # think what else can be computed in each csv prior doing more processing
        
        # Consolidate all calculated data in a data frame
        stat_temp_df <- data.frame(date_col, numDrive, numFailed, numHGST, numHitachi, numSamsung, 
                                   numToshiba, numWDC, numSeagate, num0p08TB, num0p16TB, num0p25TB,
                                   num0p32TB, num0p50TB, num1p00TB, num1p50TB, num2p00TB, num3p00TB, 
                                   num4p00TB, num5p00TB, num6p00TB)
      }
    }, # end totalBrandDriveCnt_FailureCnt
    stop("Internal Error")
  ) # end switch

  #########################
  # Row binding data frames
  #########################
  # Row combined the data frames, make the data "tidy"
  # Used bind_rows instead of rbind (bind_row is more than 50% faster than rbind)
  #  It is argued that the import can be faster as well
  switch(ImportFunction, 
    totalDriveCnt_FailureCnt = {
      if (i == 1) {
        # Initialize to create the first data frames  
        stat_totalDriveCnt_FailureCnt_df <- stat_totalDriveCnt_FailureCnt_temp_df
      } else {
        # Combine row-wise for the rest of imported data frames
        stat_totalDriveCnt_FailureCnt_df <- bind_rows(stat_totalDriveCnt_FailureCnt_df, stat_totalDriveCnt_FailureCnt_temp_df)
      } # end if
    },
    
    allSurvivalAnalysis = {
      if (i == 1) {
        # Initialize to create the first data frames  
        data_allSurvivalAnalysis_df <- data_allSurvivalAnalysis_temp_df
      } else {
        # Combine row-wise for the rest of imported data frames
        data_allSurvivalAnalysis_df <- bind_rows(data_allSurvivalAnalysis_df, data_allSurvivalAnalysis_temp_df)
      } # end if
    },
    otherwise = {
      if (i == 1) {    
        # Initialize to create the first data frames  
        dataVarName <- temp_df
      } else {
        # Combine row-wise for the rest of imported data frames
        dataVarName <- bind_rows(dataVarName, temp_df)
      }
    },
    stop("Internal Error")
  ) # end switch
  
  # Update progress bar
  setTxtProgressBar(pb, i)
}

close(pb)

# remove the temporary data frame
rm(temp_df)
switch(ImportFunction,
  totalDriveCnt_FailureCnt = {
    rm(stat_totalDriveCnt_FailureCnt_temp_df)
  },
  allSurvivalAnalysis = {
    rm(data_allSurvivalAnalysis_temp_df)
  },
  stop("Internal Error")
) # end switch

# Stop clock
ptm_loadFile <- proc.time() - ptm_loadFile
ptm_loadFile

#########################
# Save data frame to file
#########################
# Looks like it takes quite some times to save files, track the time too.
ptm_savefile <- proc.time()

# Save to data file
switch(ImportFunction, 
  totalDriveCnt_FailureCnt = {
    save(stat_totalDriveCnt_FailureCnt_df, ptm_loadFile, file = paste(dataFileName, "_totalDriveCnt_FailureCnt.RData", sep = ""))
  }, # end totalDriveCnt_FailureCnt
  allSurvivalAnalysis = {
    save(data_allSurvivalAnalysis_df, ptm_loadFile, file = paste(dataFileName, "_allSurvivalAnalysis.RData", sep = ""))
  }, # end allSurvivalAnalysis
  otherwise = {
    save(dataVarName, ptm_loadFile, file = paste(dataFileName, "_full.RData", sep = ""))
  }, # end otherwise
  stop("Internal Error")
)

ptm_savefile <- proc.time() - ptm_savefile
ptm_savefile

# Clear everything after importing done
rm(list = ls(all = TRUE))

# Beep when done
beep()