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

file2013 <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/data_2013/2013/hd2013_allSurvivalAnalysis.RData"
file2014 <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/data_2014/2014/hd2014_allSurvivalAnalysis.RData"
file2015 <- "C:/Users/Chinpei/Documents/R/HardDriveReliability/data_2015/2015/hd2015_allSurvivalAnalysis.RData"

load(file = file2013)
data_allSurvivalAnalysis_df_2013 <- data_allSurvivalAnalysis_df
rm(data_allSurvivalAnalysis_df)

load(file = file2014)
data_allSurvivalAnalysis_df_2014 <- data_allSurvivalAnalysis_df
rm(data_allSurvivalAnalysis_df)

load(file = file2015)
data_allSurvivalAnalysis_df_2015 <- data_allSurvivalAnalysis_df
rm(data_allSurvivalAnalysis_df)

# Combine all data frames into one
data_allSurvivalAnalysis_df <- bind_rows(data_allSurvivalAnalysis_df_2013, data_allSurvivalAnalysis_df_2014, data_allSurvivalAnalysis_df_2015)

############################
# Create time and event data
############################

# Time/period
data_time <- data_allSurvivalAnalysis_df %>% 
  group_by(serial_number) %>% 
  summarise(n())

summary(data_time)

# Event
data_event <- data_allSurvivalAnalysis_df %>% 
  group_by(serial_number) %>% 
  summarise(max(failure))

summary(data_event)

# Combine time and event into one data frame
newdata <- left_join(data_time, data_event, by = "serial_number")
summary(newdata)

###################
# Survival Analysis
###################

# Kaplan-Meier non-parametric analysis
kmsurvival <- survfit(Surv(newdata$`n()`,newdata$`max(failure)`) ~ 1)
plot(kmsurvival, xlab="Time", ylab="Survival Probability")
summary(kmsurvival)

# Cox proportional hazard model - coefficients and hazard rates
coxph <- coxph(Surv(newdata$`n()`,newdata$`max(failure)`) ~ 1, method="breslow")
summary(coxph)

# Exponential, Weibull, and log-logistic parametric model coefficients
# Opposite signs from Stata results, Weibull results differ; same as SAS
exponential <- survreg(Surv(newdata$`n()`,newdata$`max(failure)`) ~ 1, dist="exponential")
summary(exponential)

weibull <- survreg(Surv(newdata$`n()`,newdata$`max(failure)`) ~ 1, dist="weibull")
summary(weibull)

loglogistic <- survreg(Surv(newdata$`n()`,newdata$`max(failure)`) ~ 1, dist="loglogistic")
summary(loglogistic)

beep()