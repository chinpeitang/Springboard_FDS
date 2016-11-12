# Springboard Foundations of Data Science Capstone Project
# Hard Drive Reliability and Failures
# Chinpei Tang

################################################################
# This function is to read RData file and do quick check of data
# It also plots the corresponding year data
################################################################

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

##############################
# Select the correct file path
##############################

switch(ProductionOrTest,
  Production = {
    # Set file path and list all the csv files based on different years (2013, 2014, 2015)
    if (year == "2013" | year == "2014" | year == "2015") {
      filePathDir <- paste("data_", year, "/", year, sep = "")
    } else { # for 2016
      filePathDir <- paste("data_", year, "/", "data_", year, sep = "")
    }
    filePath <- paste(filePathRoot, filePathDir, sep = "")
    
    switch(ImportFunction,
           
      totalDriveCnt_FailureCnt = {
        fileName <- paste("/hd", year, "_totalDriveCnt_FailureCnt", ".RData", sep = "")
      }, # end totalDriveCnt_FailureCnt
      
      brandDriveCnt_FailureCnt = {
        fileName <- paste("/hd", year, "_brandDriveCnt_FailureCnt", ".RData", sep = "")
      }, # end brandDriveCnt_FailureCnt
      
      sizeDriveCnt_FailureCnt = {
        fileName <- paste("/hd", year, "_sizeDriveCnt_FailureCnt", ".RData", sep = "")
      }, # end sizeDriveCnt_FailureCnt
      
      allSurvivalAnalysis = {
        fileName <- paste("/hd", year, "_allSurvivalAnalysis", ".RData", sep = "")        
      }, # end allSurvivalAnalysis
      
      brandSurvivalAnalysis = {
        fileName <- paste("/hd", year, "_brandSurvivalAnalysis", ".RData", sep = "")
      }, # end brandSurvivalAnalysis
      
      sizeSurvivalAnalysis = {
        fileName <- paste("/hd", year, "_sizeSurvivalAnalysis", ".RData", sep = "")
      }, # end sizeSurvivalAnalysis
      
      stop("Import Function Error in Production")
    ) # end switch
  },
  Test = {
    year <- "2015" # force year to 2015
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
    
    ggplot(stat_totalDriveCnt_FailureCnt_df, aes(x = date_col, y = numDrive, colour = numFailed)) + 
      geom_line(size = 3) +
      scale_x_date("Date") +
      scale_y_discrete("Number of Drives")
    
    #ggplot(stat_totalDriveCnt_FailureCnt_df, aes(x = date_col, y = numFailed)) + 
      #geom_point() +
      #scale_x_date("Date") +
      #scale_y_discrete("Number of Failed Drives")
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
    
    # Kaplan-Meier non-parametric analysis
    kmsurvival <- survfit(Surv(allSurvival$spell, allSurvival$event) ~ 1)
    str(kmsurvival)
    plot(kmsurvival, lwd = 2.0, xlab = "Days", ylab = "Survival Probability")
    title(paste(year, "Kaplan-Meier - Overall"))
    
    # Nelson-Aalen non-parametric analysis
    nasurvival <- survfit(coxph(Surv(allSurvival$spell, allSurvival$event) ~ 1), type = "aalen")
    #summary(nasurvival)
    plot(nasurvival, lwd = 2.0, xlab = "Days", ylab = "Survival Probability")
    title(paste(year, "Nelson-Aalen - Overall"))
    
    # Cox proportional hazard model - coefficients and hazard rates
    coxph <- coxph(Surv(allSurvival$spell, allSurvival$event) ~ 1, method = "breslow")
    summary(coxph)
    
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
    
    # Kaplan-Meier non-parametric analysis
    kmsurvival <- survfit(Surv(allBrandSurvival$spell, allBrandSurvival$event) ~ allBrandSurvival$brand)
    #summary(kmsurvival)
    plot(kmsurvival, col = c(1:6), lwd = 2.0, xlab = "Days", ylab = "Survival Probability")
    legend("bottom", c("HGST", "Hitachi", "Samsung", "Toshiba", "WDC", "Seagate"), col = c(1:6), lwd = 2.0)
    title(paste(year, " - Kaplan-Meier - Different Brand/Manufacturer"))
    
    # Nelson-Aalen non-parametric analysis
    nasurvival <- survfit(coxph(Surv(allBrandSurvival$spell, allBrandSurvival$event) ~ allBrandSurvival$brand), type = "aalen")
    #summary(nasurvival)
    plot(nasurvival, col = c(1:6), lwd = 2.0, xlab = "Days", ylab = "Survival Probability")
    legend("bottom", c("HGST", "Hitachi", "Samsung", "Toshiba", "WDC", "Seagate"), col = c(1:6), lwd = 2.0)
    title(paste(year, "- Nelson-Aalen - Different Brand/Manufacturer"))
    
    # Cox proportional hazard model - coefficients and hazard rates
    coxph <- coxph(Surv(allBrandSurvival$spell, allBrandSurvival$event) ~ allBrandSurvival$brand, method = "breslow")
    summary(coxph)
    
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
    
    # Kaplan-Meier non-parametric analysis
    kmsurvival <- survfit(Surv(allSizeSurvival$spell, allSizeSurvival$event) ~ allSizeSurvival$TB)
    #summary(kmsurvival)
    plot(kmsurvival, col = c(1:12), lwd = 2.0, xlab = "Days", ylab = "Survival Probability")
    legend("bottomleft", c("0.08 TB", "0.16 TB", "0.25 TB", "0.32 TB", "0.50 TB", "1.00 TB", "1.50 TB", "2.00 TB", "3.00 TB", "4.00 TB", "5.00 TB", "6.00 TB"), col = c(1:12), lwd = 2.0)
    title(paste(year, "Kaplan-Meier - Different Size/Capacity"))
    
    # Nelson-Aalen non-parametric analysis
    nasurvival <- survfit(coxph(Surv(allSizeSurvival$spell, allSizeSurvival$event) ~ allSizeSurvival$TB), type = "aalen")
    #summary(nasurvival)
    plot(nasurvival, col = c(1:12), lwd = 2.0, xlab = "Days", ylab = "Survival Probability")
    legend("bottomleft", c("0.08 TB", "0.16 TB", "0.25 TB", "0.32 TB", "0.50 TB", "1.00 TB", "1.50 TB", "2.00 TB", "3.00 TB", "4.00 TB", "5.00 TB", "6.00 TB"), col = c(1:12), lwd = 2.0)
    title(paste(year, "Nelson-Aalen - Different Size/Capacity"))
    
    # Cox proportional hazard model - coefficients and hazard rates
    coxph <- coxph(Surv(allSizeSurvival$spell, allSizeSurvival$event) ~ allSizeSurvival$TB, method = "breslow")
    summary(coxph)
    
  }, # end sizeSurvivalAnalysis
  
  stop("Output Function Error")
)

beep()