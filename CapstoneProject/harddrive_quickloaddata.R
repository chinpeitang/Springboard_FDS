################
# Load Libraries
################
library(dplyr)
library(ggplot2)
library(beepr)
library(survival)

rm(list = ls(all = TRUE))

########
# INPUTS
########
# What data file to create?
# Count only total number of drive and failure each day
#ImportFunction <- "totalDriveCnt_FailureCnt"
# Count number of drive for each BRAND and the number of failure of each BRAND each day
#ImportFunction <- "brandDriveCnt_FailureCnt"
# Count number of drive for each SIZE and the number of failure of each SIZE each day
#ImportFunction <- "sizeDriveCnt_FailureCnt"
# Survival analysis of full data
#ImportFunction <- "allSurvivalAnalysis"
# Survival analysis for different brand
#ImportFunction <- "brandSurvivalAnalysis"
# Survival analysis for different size
ImportFunction <- "sizeSurvivalAnalysis"

#ImportFunction <- "otherwise"

# Test or production?
#ProductionOrTest <- "Production"
ProductionOrTest <- "Test"

# Change this for different year of 2013, 2014 or 2015. It will automatically read the appropriate data.
year <- 2015

# Where the root of the data is (which can be adjusted)
filePathRoot <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/"

##############################
# Select the correct file path
##############################

switch(ProductionOrTest,
  Production = {
    # Set file path and list all the csv files based on different years (2013, 2014, 2015)
    filePathDir <- paste("data_", as.character(year), "/", as.character(year), sep = "") 
    filePath <- paste(filePathRoot, filePathDir, sep = "")
    switch(ImportFunction,
           
      totalDriveCnt_FailureCnt = {
        fileName <- paste("/hd", as.character(year), "_totalDriveCnt_FailureCnt", ".RData", sep = "")
      }, # end totalDriveCnt_FailureCnt
      
      brandDriveCnt_FailureCnt = {
        fileName <- paste("/hd", as.character(year), "_brandDriveCnt_FailureCnt", ".RData", sep = "")
      }, # end brandDriveCnt_FailureCnt
      
      sizeDriveCnt_FailureCnt = {
        fileName <- paste("/hd", as.character(year), "_sizeDriveCnt_FailureCnt", ".RData", sep = "")
      }, # end sizeDriveCnt_FailureCnt
      
      allSurvivalAnalysis = {
        fileName <- paste("/hd", as.character(year), "_allSurvivalAnalysis", ".RData", sep = "")        
      }, # end allSurvivalAnalysis
      
      brandSurvivalAnalysis = {
        fileName <- paste("/hd", as.character(year), "_brandSurvivalAnalysis", ".RData", sep = "")
      }, # end brandSurvivalAnalysis
      
      sizeSurvivalAnalysis = {
        fileName <- paste("/hd", as.character(year), "_sizeSurvivalAnalysis", ".RData", sep = "")
      }, # end sizeSurvivalAnalysis
      
      stop("Import Function Error in Production")
    ) # end switch
  },
  Test = {
    year <- 2015 # force year to 2015
    filePath <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/data2015_chunk15"
    #filePath <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/data2015_chunk60"
    switch(ImportFunction,
           
      totalDriveCnt_FailureCnt = {
        fileName <- "/hd2015_totalDriveCnt_FailureCnt.RData"
      }, # end totalDriveCnt_FailureCnt
      
      brandDriveCnt_FailureCnt = {
        fileName <- "/hd2015_brandDriveCnt_FailureCnt.RData"
      }, # end brandDriveCnt_FailureCnt
      
      sizeDriveCnt_FailureCnt = {
        fileName <- "/hd2015_sizeDriveCnt_FailureCnt.RData"
      }, # end sizeDriveCnt_FailureCnt
    
      allSurvivalAnalysis = {
        fileName <- "/hd2015_allSurvivalAnalysis.RData"
      }, # end allSurvivalAnalysis

      brandSurvivalAnalysis = {
        fileName <- "/hd2015_brandSurvivalAnalysis.RData"
      }, # end brandSurvivalAnalysis
      
      sizeSurvivalAnalysis = {
        fileName <- "/hd2015_sizeSurvivalAnalysis.RData"
      }, # end sizeSurvivalAnalysis
      
      stop("Import Function Error in Test")
    ) # end switch
  },
  stop("Import Function Error")
) # end switch

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
  }, # end totalDriveCnt_FailureCnt
  
  brandDriveCnt_FailureCnt = {
    ggplot(stat_brandDriveCnt_FailureCnt_df, aes(x = date, y = total)) + 
      geom_line(aes (color = brand), size = 1.25) +
      scale_x_date() +
      scale_y_continuous()
  }, # end brandDriveCnt_FailureCnt
  
  sizeDriveCnt_FailureCnt = {
    ggplot(stat_sizeDriveCnt_FailureCnt_df, aes(x = date, y = total)) + 
      geom_line(aes (color = as.factor(size)), size = 1.25) +
      scale_x_date() +
      scale_y_continuous()
  }, # end sizeDriveCnt_FailureCnt
  
  allSurvivalAnalysis = {
    tbl_df(data_allSurvivalAnalysis_df)  
  }, # end allSurvivalAnalysis
  
  brandSurvivalAnalysis = {
    tbl_df(data_brandSurvivalAnalysis_df)
    
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
    newdata <-  left_join(
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
    
    summary(newdata)
    
    #group <- cbind(newdata$HGST, newdata$Hitachi, newdata$Samsung)
    
    # Kaplan-Meier non-parametric analysis
    kmsurvival <- survfit(Surv(newdata$spell,newdata$event) ~ newdata$Samsung)
    plot(kmsurvival, xlab="Time", ylab="Survival Probability")
    summary(kmsurvival)
    
  }, # end brandSurvivalAnalysis
  
  sizeSurvivalAnalysis = {
    tbl_df(data_sizeSurvivalAnalysis_df)
    
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

    # Combine time and event into one data frame
    newdata <-  left_join(
                left_join(data_time, data_event, by = "serial_number"), 
                                     data_size0p08TB, by = "serial_number")
    
    summary(newdata)

    # Kaplan-Meier non-parametric analysis
    kmsurvival <- survfit(Surv(newdata$spell,newdata$event) ~ newdata$size0p08TB)
    plot(kmsurvival, xlab="Time", ylab="Survival Probability")
    summary(kmsurvival)
  }, # end sizeSurvivalAnalysis
  
  stop("Output Function Error")
)

beep()