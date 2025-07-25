#DATA MANAGEMENT COURSE SCRIPT1

install.packages("tidyverse")
install.packages("readxl")
install.packages("writexl")
install.packages("haven")
install.packages("data.table")
install.packages("rvest")
install.packages("readr")

library(readr)

library(data.table) 
library(tidyverse)
library(haven)
library(readxl)
library(writexl)
library(rvest)


#1.Reading/importing Data:Can be done in different fomats
# Reads data from a CSV file.
kisumu_main_vendor <- read_csv("kisumu_main_vendor.csv")

# Reads data from a text file (more general than read.csv)
kisumu1 <- read.table("kisumu.txt")

# Reads data from an Excel file (requires the readxl package).
kisumu2 <-read_excel("kisumu.xlsx")

# Reads data from a Stata file (requires the haven package). 
kisumu3 <- read_dta("kisumu.dta")

# Reads data from a Stata file (requires the haven package). 
kisumu4 <- read_xpt("kisumu.sas")

# Read animal health data
Animal_Data <- read_csv("YSM Animal Health Data.csv")

#Read data from the web
url <- paste0("https://stats.oecd.org/sdmx-json/data/DP_LIVE/",
              ".M3.TOT.IDX2015.M/OECD?contentType=csv&detail=code",
              "&separator=comma&csv-lang=en&startPeriod=2012")
df <- read.csv(url)

#2. Data Structure Inspection:
# View the data set
View(kisumu1)
#Displays the structure of a data frame (data types and first few observations).
str(kisumu_main_vendor)
#Shows the first few rows of the data frame
head(kisumu_main_vendor)
#Shows the last few rows of the data frame.
tail(kisumu_main_vendor)
#Returns the dimensions (rows and columns) of the data frame
dim(kisumu_main_vendor)
#Returns the column names of the data frame. 
names(kisumu_main_vendor)

#3. Filtering Rows (using dplyr): 
  filter(data, condition)
#Selects rows that satisfy the given condition.
#Example:  selects rows where the county column is kisumu
obungaslum <- filter(kisumu_main_vendor, slum == "Obunga")

Male <- filter(kisumu_main_vendor, gender == "Male")

obungamale <- filter(kisumu_main_vendor, slum == "Obunga" & gender =="Male")

#Example:  selects rows where the animal age is less than 12 weeks
data <- filter(Animal_Data, age < 12)

#4. arrange data/sort data using the arrange function
#Example: Sort the age in ascending order 
data1 <- arrange(Animal_Data, age)  
#Sort age in ascending order
data2 <- arrange(Animal_Data,(-age)) 

#5. Selecting Columns (using dplyr): 
 # select(data, column_names)
#Selects specific columns by name.
#Example:  selects the "age", "age_weeks", "age_units" columns.
data3 <- select(Animal_Data,age,age_weeks,age_units) 
data4 <- select(kisumu_main_vendor,consent,county,location_type)
#Example:  selects all columns except "subcounty"
data5 <- select(kisumu_main_vendor,-subcounty)  
#Select colunms starting with certain letters,ends with or contains
data6 <- select(kisumu_main_vendor, starts_with("s")) 


#6. Mutating (Creating New Variables) (using dplyr): 
  # mutate(data, new_variable = expression)
#Creates a new variable based on an expression involving existing variables.
#Example: creates a new variable "new_age" by squaring the "age" variable 
new_age <- mutate(Animal_Data, new_age = age^2)

##mutate
logage <- mutate(Animal_Data, logage = log(age))

#7. rename columns using rename() 
#creates new names for variables in the data set
#Example: rename id to vendor_id in kisumu1 data set
data7 <- rename(kisumu1, location = region)

#8.relocate() gives new order to column names
#Example: relocate/reorder slum, ward and subcounty from their original order
kisumu1 <- relocate(kisumu1, slum, ward, subcounty)


#9. Exporting data out of R
write_csv(logage,"logage.csv")
write_xlsx(Male,"Male.xls")
write_dta(data4,"data4.dta")
write_sav(obungamale,"obungamale.sav")
write_xpt(obungaslum, "obungaslum.sas")
write.table(df,"df.txt")

#10. Data summary
#frequency tables, use transform for vertical view
table(kisumu_main_vendor$region)
table(kisumu_main_vendor$consent)
table(kisumu_main_vendor$county)
table(kisumu_main_vendor$subcounty)
table(kisumu_main_vendor$ward)
transform(table(kisumu_main_vendor$ward))
table(Animal_Data$breed)
table(Animal_Data$Species)

#Summary and missingness
summary(kisumu_main_vendor$ward)
summary(Animal_Data$age)
#
table(is.na(Animal_Data$age))
table(is.na(kisumu_main_vendor$ward))

#outlier detection using box plots
boxplot(Animal_Data$age)
boxplot(Animal_Data$dam_milkyield_liters)

#data distributions using visual approach and statistical tests
hist(Animal_Data$age)
hist(Animal_Data$dam_milkyield_liters)

#statistical test for normality. Null:Data is normal. Alternative: data is not normally distributed
shapiro.test(Animal_Data$age)
shapiro.test(Animal_Data$dam_milkyield_liters)

#merging data from various sources
#Checking for duplicates
install.packages(dplyr)
install.packages(readr)

library(dplyr)
library(readr)
#check for duplicates
duplicated(agriculture)
table(duplicated(agriculture))

duplicated(agriculture1)
table(duplicated(agriculture1))


#create a unique data set
cleandata <- distinct(agriculture1)
cleandata2 <- unique(agriculture1)

#appending data in R
agric_AB <- bind_rows(agriculture_townA,agriculture_townB)

#Exercise: check for duplicates in combined data


#merging data
#full join will keep all variables
agric_new <- full_join(agriculture, agriculture2, by = "Farm_ID")
#innerjoin will keep only unique columns
agric_gender <- inner_join(agriculture, agriculture2, by = "Farm_ID")



#Testing relationships within variables
#Visualisation approach
# Relationships between two categorical variables
#These relationships are tested using chi square distributions
table(agriculture$location, agriculture$Irrigation_Type)

chisq.test(table(agriculture$location, agriculture$Irrigation_Type))

#Exercise:1.gender and location, croptype and location, gender and croptype

#ttest for equality of means
#Categorical variables and numerical variables
shapiro.test(agriculture$yield_tonnes)
t.test(yield_tonnes ~ location, data = agriculture)

#Exercise:2.using agric_gender data, ttest on gender vs yield_tonnes

#Wilcoxon test for equality of medians
shapiro.test(agriculture$Farm_Area_acres)
wilcox.test(Farm_Area_acres ~ location, data = agriculture)

#Exercise:3.perform wilcox on Fertizer by location, pesticide use by location














