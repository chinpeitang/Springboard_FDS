# Hard Drive Reliability Capstone Project
# Chinpei Tang
# This function is to perform survival analysis to understand hard drive reliability

############################################################################
# THIS IS NOT A PRODUCTION CODES YET!
# Most were dplyr knowledge archive that may be useful later
############################################################################

# Load the required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(survival)

# Clear the original data - just in case
rm(list = ls(all = TRUE))

# Set file path where the .RData is located
filePath <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/data_2015/2015"
setwd(filePath)

# Load data
# Do also a duration track for loading file
ptm_loadfile <- proc.time()
load(file = "hd2015.RData")
ptm_loadfile <- proc.time() - ptm_loadfile
ptm_loadfile

# Quick check of the data frame
tbl_df(dataVarName)

# Seperate "year", "month" and "day", may be useful to see some trend
# For instance, more failures during which month?
dataVarName_date <- separate(dataVarName, date, c("year", "month", "day"), sep ='-')
tbl_df(dataVarName_date)
# Need to figure out way to separate hard drive name

# Select only date, serial number, model, capacity and failure columns:
dataVarName_noSMART <- 
  dataVarName %>%
  select(serial_number, model, capacity_bytes, failure, date)
tbl_df(dataVarName_noSMART)
# Can use select function "ends_with()" to get only raw or normalized SMART data.

# Filter only failed hard drives
dataVarName_failure <- 
  dataVarName %>%
  select(serial_number, model, capacity_bytes, failure, date) %>%
  filter(failure == 1)
tbl_df(dataVarName_failure)

# What are the possible invalid data or data checking that may be needed?
# See if there is NA values?
#dataVarName_NA <-
#  dataVarName %>%
#  filter(failure is.na)

# Mutate GB numbers to number that makes more sense?
dataVarName_GB <- 
  dataVarName %>% 
  select(serial_number, model, capacity_bytes, failure, date) %>% 
  mutate(GB = round(capacity_bytes/1e+12,2))
tbl_df(dataVarName_GB)
# Number of different hard drive sizes
summarise(dataVarName_GB, n_distinct(capacity_bytes))
summary(dataVarName_GB$capacity_bytes)
summary(dataVarName_GB$GB)

# Number of distinct hard drive serial numbers:
summarise(dataVarName_GB, n_distinct(serial_number))

# Number of distinct hard drive models:
summarise(dataVarName_GB, n_distinct(model))


# Arrange the data alphabetical order

dataVarName %>% 
  select(date:failure) %>% 
  filter(failure == 1 & capacity_bytes > 4e+12) %>% 
  summarise(n())

dataVarName %>% 
  select(date:failure) %>% 
  filter(failure == 0 & capacity_bytes > 4e+12) %>% 
  summarise(n())

# Group the hard drive by sizes
dataVarName %>% 
  select(date:failure) %>% 
  mutate(GB = round(capacity_bytes/1e+12, 2)) %>% 
  select(-capacity_bytes) %>% 
  group_by(GB) %>% 
  summarise(n())

# Group the hard drive by sizes
dataVarName %>% 
  select(date:failure) %>% 
  mutate(GB = round(capacity_bytes/1e+12, 2)) %>% 
  select(-capacity_bytes) %>% 
  group_by(GB, failure) %>% 
  summarise(n())

# Look at only unique information
dataVarName %>% 
  select(serial_number:failure) %>% 
  mutate(GB = round(capacity_bytes/1e+12, 2)) %>% 
  #select(-capacity_bytes) %>% 
  distinct(capacity_bytes) %>% 
  arrange(capacity_bytes)