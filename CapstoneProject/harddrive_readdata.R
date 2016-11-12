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
# What data file to create? [Uncomment to use the corresponding "ImportFunction"]
# Count only total number of drive and failure each day
#ImportFunction <- "totalDriveCnt_FailureCnt"
# Count number of drive for each BRAND and the number of failure of each BRAND each day
#ImportFunction <- "brandDriveCnt_FailureCnt"
# Count number of drive for each SIZE and the number of failure of each SIZE each day
#ImportFunction <- "sizeDriveCnt_FailureCnt"
# Survival analysis of full data
#ImportFunction <- "allSurvivalAnalysis"
# Survival analysis for different BRAND
#ImportFunction <- "brandSurvivalAnalysis"
# Survival analysis for different SIZE
#ImportFunction <- "sizeSurvivalAnalysis"
# Otherwise function
#ImportFunction <- "otherwise"

# Test or production? [Uncomment to use the corresponding "ProductionOrTest"]
#ProductionOrTest <- "Production"
#ProductionOrTest <- "Test"

# Change this for different year directory. It will automatically read the appropriate data.
#year <- "2013"
#year <- "2014"
#year <- "2015"
#year <- "Q1_2016"
#year <- "Q2_2016"

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
    if (year == "2013" | year == "2014" | year == "2015") {
      filePathDir <- paste("data_", year, "/", year, sep = "")
    } else {
      filePathDir <- paste("data_", year, "/", "data_", year, sep = "")
    }
    filePath <- paste(filePathRoot, filePathDir, sep = "")
  },
  Test = {
    year <- "2015" # force year to 2015
    filePath <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/data2015_chunk15"
    #filePath <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/data2015_chunk60"
    #filePath <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/data2015_chunk90"
  },
  stop("Production or Test Error")
)
setwd(filePath)
# Gather the file list with only csv files
fileList <- list.files(path = filePath, pattern = "*.csv")
# Number of files
numFile <- length(fileList)

# File name in the format of hdyyyy
dataVarName <- paste("hd", year, sep = "")
dataFileName <- dataVarName

###########################
# Column classes definition
###########################
# date ("Date"), serial number ("character"), 
#  model ("character"), capacity in bytes ("numeric"), 
#  binary failure ("integer"), 90 SMART data ("numeric")

# 2015 and 2016 has 90 SMART, 2013 and 2014 have 80 SMART
if (year == "2015" | year == "Q1_2016" | year == "Q2_2016") {numSMART <- 90} else {numSMART <- 80}

# Importing all data takes up too much memory, select only necessary data
# Select the corresponding columns to input. "NULL" those are not interested.
switch(ImportFunction,
  totalDriveCnt_FailureCnt = {
    # Count only total number of drive and failure each day:
    # No need serial number, model, and capability in bytes
    colClassDef <- c("Date", "NULL", "NULL", "NULL", "integer", rep("NULL",numSMART))
  }, # end totalDriveCnt_FailureCnt
  
  brandDriveCnt_FailureCnt = {
    colClassDef <- c("Date", "NULL", "character", "NULL", "integer", rep("NULL",numSMART))
  }, # end brandDriveCnt_FailureCnt
  
  sizeDriveCnt_FailureCnt = {
    colClassDef <- c("Date", "NULL", "NULL", "numeric", "integer", rep("NULL",numSMART))
  }, # end sizeDriveCnt_FailureCnt
  
  allSurvivalAnalysis = {
    # Take serial number and failure for full survival analysis:
    colClassDef <- c("NULL", "character", "NULL", "NULL", "integer", rep("NULL",numSMART))
  }, # end allSurvivalAnalysis
  
  brandSurvivalAnalysis = {
    # Take serial number, failure and model number to extract brand info
    colClassDef <- c("NULL", "character", "character", "NULL", "integer", rep("NULL",numSMART))
  }, # end brandSurvivalAnalysis
  
  sizeSurvivalAnalysis = {
    colClassDef <- c("NULL", "character", "NULL", "numeric", "integer", rep("NULL",numSMART))
  }, # end sizeSurvivalAnalysis
  
  otherwise = {
    # Take everything but no SMART
    colClassDef <- c("Date", "character", "character", "numeric", "integer", rep("NULL",numSMART))
  }, # end otherwise
  
  stop("colClassDef Error")
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

    brandDriveCnt_FailureCnt = {
      # get date of the day
      date_col <- temp_df[1,1]
      
      # Initialize binary columns for each brand
      temp_df$HGST <- F
      temp_df$Hitachi <- F
      temp_df$Samsung <- F
      temp_df$Toshiba <- F
      temp_df$WDC <- F
      temp_df$Seagate <- F
      
      # grep model details to produce various binary column of the brands
      temp_df$HGST[grep(pattern = "HGST", x = temp_df$model)] <- T
      temp_df$Hitachi[grep(pattern = "Hitachi", x = temp_df$model)] <- T
      temp_df$Samsung[grep(pattern = "SAMSUNG", x = temp_df$model)] <- T
      temp_df$Toshiba[grep(pattern = "TOSHIBA", x = temp_df$model)] <- T
      temp_df$WDC[grep(pattern = "WDC", x = temp_df$model)] <- T
      temp_df$Seagate[grep(pattern = "^ST", x = temp_df$model)] <- T
      
      # count number of drives of each brand
      numHGST <- sum(temp_df$HGST)
      numHitachi <- sum(temp_df$Hitachi)
      numSamsung <- sum(temp_df$Samsung)
      numToshiba <- sum(temp_df$Toshiba)
      numWDC <- sum(temp_df$WDC)
      numSeagate <- sum(temp_df$Seagate)
      
      temp_df$HGST_failed <- F
      temp_df$Hitachi_failed <- F
      temp_df$Samsung_failed <- F
      temp_df$Toshiba_failed <- F
      temp_df$WDC_failed <- F
      temp_df$Seagate_failed <- F
      
      temp_df$HGST_failed <- temp_df$HGST & temp_df$failure
      temp_df$Hitachi_failed <- temp_df$Hitachi & temp_df$failure
      temp_df$Samsung_failed <- temp_df$Samsung & temp_df$failure
      temp_df$Toshiba_failed <- temp_df$Toshiba & temp_df$failure
      temp_df$WDC_failed <- temp_df$WDC & temp_df$failure
      temp_df$Seagate_failed <- temp_df$Seagate & temp_df$failure
      
      numHGST_failed <- sum(temp_df$HGST_failed)
      numHitachi_failed <- sum(temp_df$Hitachi_failed)
      numSamsung_failed <- sum(temp_df$numSamsung_failed)
      numToshiba_failed <- sum(temp_df$Toshiba_failed)
      numWDC_failed <- sum(temp_df$WDC_failed)
      numSeagate_failed <- sum(temp_df$Seagate_failed)
      
      obs_HGST <- data.frame(date_col, "HGST", numHGST, numHGST_failed)
      obs_Hitachi <- data.frame(date_col, "Hitachi", numHitachi, numHitachi_failed)
      obs_Samsung <- data.frame(date_col, "Samsung", numSamsung, numSamsung_failed)
      obs_Toshiba <- data.frame(date_col, "Toshiba", numToshiba, numToshiba_failed)
      obs_WDC <- data.frame(date_col, "WDC", numWDC, numWDC_failed)
      obs_Seagate <- data.frame(date_col, "Seagate", numSeagate, numSeagate_failed)
      
      obs_names <- c("date", "brand", "total", "failed")
      
      names(obs_HGST) <- names(obs_Hitachi) <- names(obs_Samsung) <- names(obs_Toshiba) <- names(obs_WDC) <- names(obs_Seagate) <- obs_names
      
      # Consolidate all calculated data in a data frame
      stat_brandDriveCnt_FailureCnt_temp_df <-
        bind_rows(obs_HGST, obs_Hitachi, obs_Samsung, obs_Toshiba, obs_WDC, obs_Seagate)
    }, # end brandDriveCnt_FailureCnt
    
    sizeDriveCnt_FailureCnt = {
      # get date of the day
      date_col <- temp_df[1,1]
      
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
      
      # count number of drives of each size
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
      
      num0p08TB_failed <- temp_df %>% filter(TB == 0.08 & failure == T) %>% nrow()
      num0p16TB_failed <- temp_df %>% filter(TB == 0.16 & failure == T) %>% nrow()
      num0p25TB_failed <- temp_df %>% filter(TB == 0.25 & failure == T) %>% nrow()
      num0p32TB_failed <- temp_df %>% filter(TB == 0.32 & failure == T) %>% nrow()
      num0p50TB_failed <- temp_df %>% filter(TB == 0.50 & failure == T) %>% nrow()
      num1p00TB_failed <- temp_df %>% filter(TB == 1.00 & failure == T) %>% nrow()
      num1p50TB_failed <- temp_df %>% filter(TB == 1.50 & failure == T) %>% nrow()
      num2p00TB_failed <- temp_df %>% filter(TB == 2.00 & failure == T) %>% nrow()
      num3p00TB_failed <- temp_df %>% filter(TB == 3.00 & failure == T) %>% nrow()
      num4p00TB_failed <- temp_df %>% filter(TB == 4.00 & failure == T) %>% nrow()
      num5p00TB_failed <- temp_df %>% filter(TB == 5.00 & failure == T) %>% nrow()
      num6p00TB_failed <- temp_df %>% filter(TB == 6.00 & failure == T) %>% nrow()
      
      obs_num0p08TB <- data.frame(date_col, 0.08, num0p08TB, num0p08TB_failed)
      obs_num0p16TB <- data.frame(date_col, 0.16, num0p16TB, num0p16TB_failed)
      obs_num0p25TB <- data.frame(date_col, 0.25, num0p25TB, num0p25TB_failed)
      obs_num0p32TB <- data.frame(date_col, 0.32, num0p32TB, num0p32TB_failed)
      obs_num0p50TB <- data.frame(date_col, 0.50, num0p50TB, num0p50TB_failed)
      obs_num1p00TB <- data.frame(date_col, 1.00, num1p00TB, num1p00TB_failed)
      obs_num1p50TB <- data.frame(date_col, 1.50, num1p50TB, num1p50TB_failed)
      obs_num2p00TB <- data.frame(date_col, 2.00, num2p00TB, num2p00TB_failed)
      obs_num3p00TB <- data.frame(date_col, 3.00, num3p00TB, num3p00TB_failed)
      obs_num4p00TB <- data.frame(date_col, 4.00, num4p00TB, num4p00TB_failed)
      obs_num5p00TB <- data.frame(date_col, 5.00, num5p00TB, num5p00TB_failed)
      obs_num6p00TB <- data.frame(date_col, 6.00, num6p00TB, num6p00TB_failed)
      
      obs_names <- c("date", "size", "total", "failed")
      
      names(obs_num0p08TB) <- names(obs_num0p16TB) <- names(obs_num0p25TB) <- names(obs_num0p32TB) <- names(obs_num0p50TB) <- names(obs_num1p00TB) <- names(obs_num1p50TB) <- names(obs_num2p00TB) <- names(obs_num3p00TB) <- names(obs_num4p00TB) <- names(obs_num5p00TB) <- names(obs_num6p00TB) <- obs_names
      
      stat_sizeDriveCnt_FailureCnt_temp_df <- 
        bind_rows(obs_num0p08TB, obs_num0p16TB, obs_num0p25TB, obs_num0p32TB, obs_num0p50TB,
                  obs_num1p00TB, obs_num1p50TB, obs_num2p00TB, obs_num3p00TB, obs_num4p00TB,
                  obs_num5p00TB, obs_num6p00TB)
    }, # end sizeDriveCnt_FailureCnt

    allSurvivalAnalysis = {
      data_allSurvivalAnalysis_temp_df <- temp_df
    }, # end allSurvivalAnalysis
  
    brandSurvivalAnalysis = {
      # Initialize binary columns for each brand
      temp_df$HGST <- F
      temp_df$Hitachi <- F
      temp_df$Samsung <- F
      temp_df$Toshiba <- F
      temp_df$WDC <- F
      temp_df$Seagate <- F
      
      # grep model details to produce various binary column of the brands
      temp_df$HGST[grep(pattern = "HGST", x = temp_df$model)] <- T
      temp_df$Hitachi[grep(pattern = "Hitachi", x = temp_df$model)] <- T
      temp_df$Samsung[grep(pattern = "SAMSUNG", x = temp_df$model)] <- T
      temp_df$Toshiba[grep(pattern = "TOSHIBA", x = temp_df$model)] <- T
      temp_df$WDC[grep(pattern = "WDC", x = temp_df$model)] <- T
      temp_df$Seagate[grep(pattern = "^ST", x = temp_df$model)] <- T
      
      data_brandSurvivalAnalysis_temp_df <- 
        data.frame(temp_df$serial_number, temp_df$failure, 
                   temp_df$HGST, temp_df$Hitachi, temp_df$Samsung, 
                   temp_df$Toshiba, temp_df$WDC, temp_df$Seagate)
      
      # Rename the columns
      names(data_brandSurvivalAnalysis_temp_df) <- 
        c("serial_number", "failure", "HGST", "Hitachi", "Samsung", "Toshiba", "WDC", "Seagate")
      
    }, # end totalBrandDriveCnt_FailureCnt
    
    sizeSurvivalAnalysis = {
      
      temp_df$size0p08TB <- F
      temp_df$size0p16TB <- F
      temp_df$size0p25TB <- F
      temp_df$size0p32TB <- F
      temp_df$size0p50TB <- F
      temp_df$size1p00TB <- F
      temp_df$size1p50TB <- F
      temp_df$size2p00TB <- F
      temp_df$size3p00TB <- F
      temp_df$size4p00TB <- F
      temp_df$size5p00TB <- F
      temp_df$size6p00TB <- F
      
      temp_df$size0p08TB[temp_df$capacity_bytes >= 8.0E10 & temp_df$capacity_bytes < 8.1E10] <- T
      temp_df$size0p16TB[temp_df$capacity_bytes >= 1.6E11 & temp_df$capacity_bytes < 1.7E11] <- T
      temp_df$size0p25TB[temp_df$capacity_bytes >= 2.5E11 & temp_df$capacity_bytes < 2.6E11] <- T
      temp_df$size0p32TB[temp_df$capacity_bytes >= 3.2E11 & temp_df$capacity_bytes < 3.3E11] <- T
      temp_df$size0p50TB[temp_df$capacity_bytes >= 5.0E11 & temp_df$capacity_bytes < 5.1E11] <- T
      temp_df$size1p00TB[temp_df$capacity_bytes >= 1.0E12 & temp_df$capacity_bytes < 1.1E12] <- T
      temp_df$size1p50TB[temp_df$capacity_bytes >= 1.5E12 & temp_df$capacity_bytes < 1.6E12] <- T
      temp_df$size2p00TB[temp_df$capacity_bytes >= 2.0E12 & temp_df$capacity_bytes < 2.1E12] <- T
      temp_df$size3p00TB[temp_df$capacity_bytes >= 3.0E12 & temp_df$capacity_bytes < 3.1E12] <- T
      temp_df$size4p00TB[temp_df$capacity_bytes >= 4.0E12 & temp_df$capacity_bytes < 4.1E12] <- T
      temp_df$size5p00TB[temp_df$capacity_bytes >= 5.0E12 & temp_df$capacity_bytes < 5.1E12] <- T
      temp_df$size6p00TB[temp_df$capacity_bytes >= 6.0E12 & temp_df$capacity_bytes < 6.1E12] <- T
      
      data_sizeSurvivalAnalysis_temp_df <-
        data.frame(temp_df$serial_number, temp_df$failure, 
                   temp_df$size0p08TB,
                   temp_df$size0p16TB, 
                   temp_df$size0p25TB, 
                   temp_df$size0p32TB, 
                   temp_df$size0p50TB, 
                   temp_df$size1p00TB, 
                   temp_df$size1p50TB, 
                   temp_df$size2p00TB, 
                   temp_df$size3p00TB, 
                   temp_df$size4p00TB, 
                   temp_df$size5p00TB, 
                   temp_df$size6p00TB)
      
      # Rename the columns
      names(data_sizeSurvivalAnalysis_temp_df) <- 
        c("serial_number", "failure", "size0p08TB", 
          "size0p16TB", "size0p25TB", "size0p32TB", 
          "size0p50TB", "size1p00TB", "size1p50TB",
          "size2p00TB", "size3p00TB", "size4p00TB",
          "size5p00TB", "size6p00TB")
      
    }, # end sizeSurvivalAnalysis

    stop("Import Function Error")
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
    }, # end totalDriveCnt_FailureCnt
    
    brandDriveCnt_FailureCnt = {
      if (i == 1) {
        # Initialize to create the first data frames  
        stat_brandDriveCnt_FailureCnt_df <- stat_brandDriveCnt_FailureCnt_temp_df
      } else {
        # Combine row-wise for the rest of imported data frames
        stat_brandDriveCnt_FailureCnt_df <- bind_rows(stat_brandDriveCnt_FailureCnt_df, stat_brandDriveCnt_FailureCnt_temp_df)
      } # end if
    }, # end brandDriveCnt_FailureCnt
    
    sizeDriveCnt_FailureCnt = {
      if (i == 1) {
        # Initialize to create the first data frames  
        stat_sizeDriveCnt_FailureCnt_df <- stat_sizeDriveCnt_FailureCnt_temp_df
      } else {
        # Combine row-wise for the rest of imported data frames
        stat_sizeDriveCnt_FailureCnt_df <- bind_rows(stat_sizeDriveCnt_FailureCnt_df, stat_sizeDriveCnt_FailureCnt_temp_df)
      } # end if
    }, # sizeDriveCnt_FailureCnt
    
    allSurvivalAnalysis = {
      if (i == 1) {
        # Initialize to create the first data frames  
        data_allSurvivalAnalysis_df <- data_allSurvivalAnalysis_temp_df
      } else {
        # Combine row-wise for the rest of imported data frames
        data_allSurvivalAnalysis_df <- bind_rows(data_allSurvivalAnalysis_df, data_allSurvivalAnalysis_temp_df)
      } # end if
    }, # end allSurvivalAnalysis
    
    brandSurvivalAnalysis = {
      if (i == 1) {
        # Initialize to create the first data frames  
        data_brandSurvivalAnalysis_df <- data_brandSurvivalAnalysis_temp_df
      } else {
        # Combine row-wise for the rest of imported data frames
        data_brandSurvivalAnalysis_df <- bind_rows(data_brandSurvivalAnalysis_df, data_brandSurvivalAnalysis_temp_df)
      } # end if
    }, # end brandSurvivalAnalysis
    
    sizeSurvivalAnalysis = {
      if (i == 1) {
        # Initialize to create the first data frames  
        data_sizeSurvivalAnalysis_df <- data_sizeSurvivalAnalysis_temp_df
      } else {
        # Combine row-wise for the rest of imported data frames
        data_sizeSurvivalAnalysis_df <- bind_rows(data_sizeSurvivalAnalysis_df, data_sizeSurvivalAnalysis_temp_df)
      } # end if
    }, # end sizeSurvivalAnalysis
    
    otherwise = {
      if (i == 1) {    
        # Initialize to create the first data frames  
        dataVarName <- temp_df
      } else {
        # Combine row-wise for the rest of imported data frames
        dataVarName <- bind_rows(dataVarName, temp_df)
      }
    }, # end otherwise
    stop("Row Binding Data Error")
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
  }, # end totalDriveCnt_FailureCnt
  
  brandDriveCnt_FailureCnt = {
    rm(stat_brandDriveCnt_FailureCnt_temp_df)
  }, # end brandDriveCnt_FailureCnt
  
  sizeDriveCnt_FailureCnt ={
    rm(stat_sizeDriveCnt_FailureCnt_temp_df)
  }, # end sizeDriveCnt_FailureCnt
  
  allSurvivalAnalysis = {
    rm(data_allSurvivalAnalysis_temp_df)
  }, # end allSurvivalAnalysis
  
  brandSurvivalAnalysis = {
    rm(data_brandSurvivalAnalysis_temp_df)
  }, # end brandSurvivalAnalysis
  
  sizeSurvivalAnalysis = {
    rm(data_sizeSurvivalAnalysis_temp_df)
  }, # end sizeSurvivalAnalysis
  
  stop("Temporary Data Removal Error")
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
    save(stat_totalDriveCnt_FailureCnt_df, ptm_loadFile, 
         file = paste(dataFileName, "_totalDriveCnt_FailureCnt.RData", sep = ""))
  }, # end totalDriveCnt_FailureCnt
  
  brandDriveCnt_FailureCnt = {
    save(stat_brandDriveCnt_FailureCnt_df, ptm_loadFile, 
         file = paste(dataFileName, "_brandDriveCnt_FailureCnt.RData", sep = ""))
  }, # end brandDriveCnt_FailureCnt
  
  sizeDriveCnt_FailureCnt = {
    save(stat_sizeDriveCnt_FailureCnt_df, ptm_loadFile, 
         file = paste(dataFileName, "_sizeDriveCnt_FailureCnt.RData", sep = ""))
  }, # end sizeDriveCnt_FailureCnt
  
  allSurvivalAnalysis = {
    save(data_allSurvivalAnalysis_df, ptm_loadFile, 
         file = paste(dataFileName, "_allSurvivalAnalysis.RData", sep = ""))
  }, # end allSurvivalAnalysis
  
  brandSurvivalAnalysis = {
    save(data_brandSurvivalAnalysis_df, ptm_loadFile, 
         file = paste(dataFileName, "_brandSurvivalAnalysis.RData", sep = ""))
  }, # end brandSurvivalAnalysis
  
  sizeSurvivalAnalysis = {
    save(data_sizeSurvivalAnalysis_df, ptm_loadFile, 
         file = paste(dataFileName, "_sizeSurvivalAnalysis.RData", sep = ""))
  }, # end sizeSurvivalAnalysis
  
  otherwise = {
    save(dataVarName, ptm_loadFile, file = paste(dataFileName, "_full.RData", sep = ""))
  }, # end otherwise
  stop("Saving Data Error")
)

ptm_savefile <- proc.time() - ptm_savefile
ptm_savefile

# Clear everything after importing done
#rm(list = ls(all = TRUE))

# Beep when done
beep()