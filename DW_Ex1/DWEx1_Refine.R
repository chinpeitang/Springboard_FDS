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

# Separate "-" between codes and numbers
product_code_number = as.character(refine$Product.code...number)
product_code_number = strsplit(product_code_number, split = "-")
product_code = vapply(product_code_number, function(x){x[1]}, character(1))
product_code
product_number = vapply(product_code_number, function(x){x[2]}, character(1))
product_number
# Rearrange to columns
refine = cbind(refine[1], product_code, product_number, refine[3:6])

# Rename products
refine$product_code = as.character(refine$product_code)
refine$product_code[grep(pattern = "p", x = refine$product_code)] = "Smartphone"
refine$product_code[grep(pattern = "v", x = refine$product_code)] = "TV"
refine$product_code[grep(pattern = "x", x = refine$product_code)] = "Laptop"
refine$product_code[grep(pattern = "q", x = refine$product_code)] = "Tablet"

# Create full address column
full_address = paste(refine$address, refine$city, refine$country, sep = ", ")
refine = cbind(refine[1:6], full_address, refine[7])

# Write to clean file
write.csv(refine, file = "refine_clean.csv")