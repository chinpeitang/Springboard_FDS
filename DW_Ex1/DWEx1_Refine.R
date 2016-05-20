# Set working directory
setwd("C:/Users/Chinpei/Documents/GitHub/Springboard_FDS/DW_Ex1")
# Load original data
refine = read.csv("refine_original.csv", header = T)
refine

# Consolidate company names
summary(refine$company)
# Convert all company names to lower case
refine$company = vapply(refine$company, tolower, character(1))
refine$company[grep(pattern = "ps", x = refine$company)] = "philips"
refine$company[grep(pattern = "ak", x = refine$company)] = "akzo"
refine$company[grep(pattern = "van", x = refine$company)] = "van houten"
refine$company[grep(pattern = "uni", x = refine$company)] = "unilever"

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
refine$product_code[refine$product_code == "p"] = "Smartphone"
refine$product_code[refine$product_code == "v"] = "TV"
refine$product_code[refine$product_code == "x"] = "Laptop"
refine$product_code[refine$product_code == "q"] = "Tablet"

# Create full address column
full_address = paste(refine$address, refine$city, refine$country, sep = ", ")
refine = cbind(refine[1:6], full_address, refine[7])

# Create binary variables columns for companies
refine$company_philips = as.integer(refine$company == "philips")
refine$company_akzo = as.integer(refine$company == "akzo")
refine$company_van_houten = as.integer(refine$company == "van houten")
refine$company_unilever = as.integer(refine$company == "unilever")

# Create binary variables columns for product categories
refine$product_smartphone = as.integer(refine$product_code == "Smartphone")
refine$product_tv = as.integer(refine$product_code == "TV")
refine$product_laptop = as.integer(refine$product_code == "Laptop")
refine$product_tablet = as.integer(refine$product_code == "Tablet")

# Write to clean file
write.csv(refine, file = "refine_clean.csv")