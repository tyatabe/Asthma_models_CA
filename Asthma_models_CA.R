# Set wd in laptop's developer's folder
setwd('/home/tyatabe/Onedrive/Dev folder/Asthma_models')

# read in and prepare asthma attacks data
asthma <- read.csv("Asthma_Emergency_Department_Visit_Rates_by_ZIP_Code.csv")

# clean ZIP code data (separate zip from coordinates)
library(zipcode)
asthma$zip <- as.factor(clean.zipcodes(substr(asthma$ZIP.code, 1,5 )))


# Get total emergency visits by zipcode
cases <- asthma[asthma$Age.Group=="All Ages",]
cases <- cases[,-2]
cases$Age.adjusted.rate <- log(cases$Age.adjusted.rate)
# zipcode coordinates

data(zipcode)
zipcode$zip <- as.factor(zipcode$zip)
cases <- merge(cases, zipcode, by="zip")
