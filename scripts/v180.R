# for calculating new measure v180 disability 
library(tidyverse)
library(readr)

#this one contains state/ntl values 
dis = read_csv("raw_data/PLACES/Disability_and_Health_Data_System__DHDS__20250107.csv")

#this one contains county values 
places = read_csv("raw_data/PLACES/PLACES__County_Data__GIS_Friendly_Format___2024_release_20250107.csv")


states = dis %>% filter(Response == "Any Disability") %>% 
  filter(DataValueTypeID == "AGEADJPREV") %>% 
  filter(StratificationCategory1 == "Overall") %>% 
  filter(Year == "2022") %>% 
  rename(stabb = LocationAbbr, v180_rawvalue = Data_Value, 
         v180_cilow = Low_Confidence_Limit, v180_cihigh = High_Confidence_Limit) %>% 
  select(stabb, v180_rawvalue, v180_cilow, v180_cihigh)

counties = places %>% select(CountyFIPS, DISABILITY_Adj95CI, DISABILITY_AdjPrev)
# Extract the numeric values from the strings
ci_split <- gsub("[()]", "", counties$DISABILITY_Adj95CI) # Remove parentheses
ci_split <- strsplit(ci_split, ", ") # Split by ", "

# Create the new columns
counties$v180_cilow <- as.numeric(sapply(ci_split, `[`, 1)) # First value
counties$v180_cihigh <- as.numeric(sapply(ci_split, `[`, 2)) # Second value

counties = counties %>% rename(v180_rawvalue = DISABILITY_AdjPrev) %>% select(-c(DISABILITY_Adj95CI))




#get fips 

cfips = haven::read_sas("P:/CH-Ranking/Data/2025/2 Cleaned data ready for Calculation or Verification/county_fips_with_ct_old.sas7bdat")
sfips = haven::read_sas("P:/CH-Ranking/Data/2025/2 Cleaned data ready for Calculation or Verification/state_fips.sas7bdat")

statestot = merge(sfips, states, by.x= "state", by.y = "stabb", all.x = TRUE, no.dups = TRUE)
countiestot = merge(cfips, counties, by.x = "fipscode", by.y = "CountyFIPS", all.x = TRUE)




howtot = rbind(statestot, countiestot)
columns_to_divide <- c("v180_rawvalue", "v180_cilow", "v180_cihigh")
howtot[, columns_to_divide] <- howtot[, columns_to_divide] / 100

save(howtot, file = "duplicated_data/v180_how.csv")



#######################################################################
# compare to GL 
year = 2025
measurenum = "v180"
gan = haven::read_sas(paste0("P:/CH-Ranking/Data/",year,"/3 Data calculated needs checking/", measurenum, ".sas7bdat"))

summary(gan %>% select(paste0(measurenum, "_rawvalue")))
summary(howtot$v180_rawvalue)

ghow = merge(gan, howtot, by = c("statecode", "countycode")) 
ghow$diff = ghow$v180_rawvalue.x - ghow$v180_rawvalue.y
ghow$difflow = ghow$v180_cilow.x- ghow$v180_cilow.y
ghow$diffhigh = ghow$v180_cihigh.x - ghow$v180_cihigh.y

