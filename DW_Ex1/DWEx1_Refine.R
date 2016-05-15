# Set working directory
setwd("C:/Users/Chinpei/Documents/GitHub/Springboard_FDS/DW_Ex1")
# Load original data
refine = read.csv("refine_original.csv", header = T)
# Check data
names(refine)
summary(refine)
summary(refine$company)
dim(refine)
# Consolidate "philips"
phil_names = grep(pattern = "ps", x = refine$company, ignore.case = T)
refine$company[phil_names] = "philips"
summary(refine$company)
# Consolidate "akzo"
akzo_names = grep(pattern = "ak", x = refine$company, ignore.case = T)
refine$company[akzo_names] = "akzo"
summary(refine$company)
# Consolidate "van houten"
van_names = grep(pattern = "van", x = refine$company, ignore.case = T)
refine$company[van_names] = "van houten"
summary(refine$company)
# Consolidate "unilever"
uni_names = grep(pattern = "uni", x = refine$company, ignore.case = T)
refine$company[uni_names] = "unilever"
summary(refine$company)
# Write to clean file
write.csv(refine, file = "refine_clean.csv")