# Set working directory
setwd("C:/Users/Chinpei/Documents/GitHub/Springboard_FDS/DW_Ex2")
# Load original data (_ex since Titanic is an available dataset in RStudio)
titanic_ex = read.csv("titanic_original.csv", header = T, na.strings = c(""," "))

dim(titanic_ex)
summary(titanic_ex)

# Port of embarkation (There are actually 3 missing values instead of 1)
summary(titanic_ex$embarked)
titanic_ex$embarked[is.na(titanic_ex$embarked)] = "S"
summary(titanic_ex$embarked)

# Age
summary(titanic_ex$age)
mean(titanic_ex$age, na.rm = T)
titanic_ex$age[is.na(titanic_ex$age)] = mean(titanic_ex$age, na.rm = T)
summary(titanic_ex$age)

#median(titanic_ex$age, na.rm = T)

# Lifeboat
summary(titanic_ex$boat)
titanic_ex$boat[is.na(titanic_ex$boat)]
summary(titanic_ex$boat)

# Cabin
summary(titanic_ex$cabin)
titanic_ex$has_cabin_number = as.integer(!is.na(titanic_ex$cabin))

# Write to clean file
write.csv(titanic_ex, file = "titanic_clean.csv")