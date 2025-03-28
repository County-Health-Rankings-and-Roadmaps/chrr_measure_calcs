# for children in poverty subgroups 

year = "2025"
measurenum = "v024"


# load data
#this was pulled using GL's awesome tool 
totraw = read.csv("raw_data/ACS/v024_subgroups.csv")


library(dplyr)
library(readr)
library(tidyr)



# Add derived columns
totraw1 <- totraw %>%
  mutate(
    fipscode = sprintf("%05d", GEOID),
    statecode = substr(fipscode, 1, 2),
    countycode = substr(fipscode, 3, 5)
  )


# Calculate variables
tt <- totraw1 %>%
  mutate(
    aiandenom = B17020C_011E + B17020C_012E + B17020C_013E + B17020C_003E + B17020C_004E + B17020C_005E, 
    aiannum = B17020C_003E + B17020C_004E + B17020C_005E, 
    v024aian = ifelse(aiannum/aiandenom != 0 & aiannum/aiandenom !=1, aiannum / aiandenom, NA), 
    aiansedenom = sqrt(B17020C_011M^2 + B17020C_012M^2 + B17020C_013M^2 + B17020C_003M^2 + B17020C_004M^2 + B17020C_005M^2) / 1.645,
    aiansenum = sqrt(B17020C_003M^2 + B17020C_004M^2 + B17020C_005M^2) / 1.645,
    aianse = ifelse((aiansenum^2 - (v024aian^2) * (aiansedenom^2)) > 0,
                    (1 / aiandenom) * sqrt(aiansenum^2 - (v024aian^2) * (aiansedenom^2)),
                    (1 / aiandenom) * sqrt(aiansenum^2 + (v024aian^2) * (aiansedenom^2))
    ),
    v024aianlow = ifelse(v024aian- 1.96* aianse <0 & v024aian+ 1.96* aianse>1, NA, pmax(v024aian - 1.96 * aianse, 0)),
    v024aianhigh = ifelse(v024aian- 1.96* aianse <0 & v024aian+ 1.96* aianse>1, NA, pmin(v024aian + 1.96 * aianse, 1)),
    v024aian = ifelse(v024aian- 1.96* aianse <0 & v024aian+ 1.96* aianse>1, NA, v024aian), 
    
    hispanicdenom = B17020I_011E + B17020I_012E + B17020I_013E + B17020I_003E + B17020I_004E + B17020I_005E,
    hispanicnum = B17020I_003E + B17020I_004E + B17020I_005E,
    v024hispanic = ifelse(hispanicnum/hispanicdenom != 0 & hispanicnum/hispanicdenom !=1, hispanicnum / hispanicdenom, NA), 
    
    hispanicsedenom = sqrt(B17020I_011M^2 + B17020I_012M^2 + B17020I_013M^2 + B17020I_003M^2 + B17020I_004M^2 + B17020I_005M^2) / 1.645,
    hispanicsenum = sqrt(B17020I_003M^2 + B17020I_004M^2 + B17020I_005M^2) / 1.645,
    hispanicse = ifelse((hispanicsenum^2 - (v024hispanic^2) * (hispanicsedenom^2)) > 0,
                        (1 / hispanicdenom) * sqrt(hispanicsenum^2 - (v024hispanic^2) * (hispanicsedenom^2)),
                        (1 / hispanicdenom) * sqrt(hispanicsenum^2 + (v024hispanic^2) * (hispanicsedenom^2))
    ),
    v024hispaniclow = ifelse(v024hispanic- 1.96* hispanicse <0 & v024hispanic+ 1.96* hispanicse>1, NA, pmax(v024hispanic - 1.96 * hispanicse, 0)),
    v024hispanichigh = ifelse(v024hispanic- 1.96* hispanicse <0 & v024hispanic+ 1.96* hispanicse>1, NA, pmin(v024hispanic + 1.96 * hispanicse, 1)),
    v024hispanic = ifelse(v024hispanic- 1.96* hispanicse <0 & v024hispanic+ 1.96* hispanicse>1, NA, v024hispanic), 
    
    
    
    whitedenom = B17020H_011E + B17020H_012E + B17020H_013E + B17020H_003E + B17020H_004E + B17020H_005E,
    whitenum = B17020H_003E + B17020H_004E + B17020H_005E,
    v024white = ifelse(whitenum/whitedenom != 0 & whitenum/whitedenom !=1, whitenum / whitedenom, NA), 
    
    whitesedenom = sqrt(B17020H_011M^2 + B17020H_012M^2 + B17020H_013M^2 + B17020H_003M^2 + B17020H_004M^2 + B17020H_005M^2) / 1.645,
    whitesenum = sqrt(B17020H_003M^2 + B17020H_004M^2 + B17020H_005M^2) / 1.645,
    whitese = ifelse((whitesenum^2 - (v024white^2) * (whitesedenom^2)) > 0,
                     (1 / whitedenom) * sqrt(whitesenum^2 - (v024white^2) * (whitesedenom^2)),
                     (1 / whitedenom) * sqrt(whitesenum^2 + (v024white^2) * (whitesedenom^2))
    ),
    v024whitelow = ifelse(v024white- 1.96* whitese <0 & v024white+ 1.96* whitese>1, NA, pmax(v024white - 1.96 * whitese, 0)),
    v024whitehigh = ifelse(v024white- 1.96* whitese <0 & v024white+ 1.96* whitese>1, NA, pmin(v024white + 1.96 * whitese, 1)),
    v024white = ifelse(v024white- 1.96* whitese <0 & v024white+ 1.96* whitese>1, NA, v024white), 
    
    
    blackdenom = B17020B_011E + B17020B_012E + B17020B_013E + B17020B_003E + B17020B_004E + B17020B_005E,
    blacknum = B17020B_003E + B17020B_004E + B17020B_005E,
    v024black = ifelse(blacknum/blackdenom != 0 & blacknum/blackdenom !=1, blacknum / blackdenom, NA), 
    
    blacksedenom = sqrt(B17020B_011M^2 + B17020B_012M^2 + B17020B_013M^2 + B17020B_003M^2 + B17020B_004M^2 + B17020B_005M^2) / 1.645,
    blacksenum = sqrt(B17020B_003M^2 + B17020B_004M^2 + B17020B_005M^2) / 1.645,
    blackse = ifelse((blacksenum^2 - (v024black^2) * (blacksedenom^2)) > 0,
                     (1 / blackdenom) * sqrt(blacksenum^2 - (v024black^2) * (blacksedenom^2)),
                     (1 / blackdenom) * sqrt(blacksenum^2 + (v024black^2) * (blacksedenom^2))
    ),
    v024blacklow = ifelse(v024black- 1.96* blackse <0 & v024black+ 1.96* blackse>1, NA, pmax(v024black - 1.96 * blackse, 0)),
    v024blackhigh = ifelse(v024black- 1.96* blackse <0 & v024black+ 1.96* blackse>1, NA, pmin(v024black + 1.96 * blackse, 1)),
    v024black = ifelse(v024black- 1.96* blackse <0 & v024black+ 1.96* blackse>1, NA, v024black), 
    
    
    
    asiandenom = B17020D_011E + B17020D_012E + B17020D_013E + B17020D_003E + B17020D_004E + B17020D_005E +
      B17020E_011E + B17020E_012E + B17020E_013E + B17020E_003E + B17020E_004E + B17020E_005E,
    asiannum = B17020D_003E + B17020D_004E + B17020D_005E + B17020E_003E + B17020E_004E + B17020E_005E,
    v024asian = ifelse(asiannum/asiandenom != 0 & asiannum/asiandenom !=1, asiannum / asiandenom, NA), 
    
    asiansedenom = sqrt(B17020D_011M^2 + B17020D_012M^2 + B17020D_013M^2 + B17020D_003M^2 + B17020D_004M^2 + B17020D_005M^2 +
                          B17020E_011M^2 + B17020E_012M^2 + B17020E_013M^2 + B17020E_003M^2 + B17020E_004M^2 + B17020E_005M^2) / 1.645,
    asiansenum = sqrt(B17020D_003M^2 + B17020D_004M^2 + B17020D_005M^2 + B17020E_003M^2 + B17020E_004M^2 + B17020E_005M^2) / 1.645,
    asianse = ifelse((asiansenum^2 - (v024asian^2) * (asiansedenom^2)) > 0,
                     (1 / asiandenom) * sqrt(asiansenum^2 - (v024asian^2) * (asiansedenom^2)),
                     (1 / asiandenom) * sqrt(asiansenum^2 + (v024asian^2) * (asiansedenom^2))
    ),
    v024asianlow = ifelse(v024asian- 1.96* asianse <0 & v024asian+ 1.96* asianse>1, NA, pmax(v024asian - 1.96 * asianse, 0)),
    v024asianhigh = ifelse(v024asian- 1.96* asianse <0 & v024asian+ 1.96* asianse>1, NA, pmin(v024asian + 1.96 * asianse, 1)),
    v024asian = ifelse(v024asian- 1.96* asianse <0 & v024asian+ 1.96* asianse>1, NA, v024asian), 
    
    
  ) %>%
  # Filter and clean state and county codes
  filter(statecode != "72") %>%
  mutate(
    statecode = ifelse(is.na(statecode), "00", statecode),
    countycode = ifelse(is.na(countycode), "000", countycode)
  )

# Select relevant columns
tt2 <- tt %>%
  select(
    statecode,countycode, v024aian, v024aianlow, v024aianhigh, aiannum, aiandenom,
    v024hispanic, v024hispaniclow, v024hispanichigh, hispanicnum, hispanicdenom,
    v024white, v024whitelow, v024whitehigh, whitenum, whitedenom,
    v024black, v024blacklow, v024blackhigh, blacknum, blackdenom,
    v024asian, v024asianlow, v024asianhigh, asiannum, asiandenom
  )

cfips = haven::read_sas("P:/CH-Ranking/Data/2025/2 Cleaned data ready for Calculation or Verification/county_fips_with_ct_old.sas7bdat")
sfips = haven::read_sas("P:/CH-Ranking/Data/2025/2 Cleaned data ready for Calculation or Verification/state_fips.sas7bdat")
fips = rbind(cfips, sfips)

how = merge(fips, tt2, by = c("statecode", "countycode"), all.x = TRUE)

# some suppressions 
# Update the values in the how dataset
how <- how %>%
  mutate(
    # Set the specified Black variables to NA for specific statecode and countycode combinations
    v024black = ifelse((statecode == 51 & countycode == 580) | (statecode == 53 & countycode == "027"), NA, v024black),
    blacknum = ifelse((statecode == 51 & countycode == 580) | (statecode == 53 & countycode == "027"), NA, blacknum),
    blackdenom = ifelse((statecode == 51 & countycode == 580) | (statecode == 53 & countycode == "027"), NA, blackdenom),
    v024blacklow = ifelse((statecode == 51 & countycode == 580) | (statecode == 53 & countycode == "027"), NA, v024blacklow),
    v024blackhigh = ifelse((statecode == 51 & countycode == 580) | (statecode == 53 & countycode == "027"), NA, v024blackhigh),
    # Set the specified Hispanic variables to NA for a specific statecode and countycode combination
    v024hispanic = ifelse(statecode == 13 & countycode == 235, NA, v024hispanic),
    hispanicnum = ifelse(statecode == 13 & countycode == 235, NA, hispanicnum),
    hispanicdenom = ifelse(statecode == 13 & countycode == 235, NA, hispanicdenom),
    v024hispaniclow = ifelse(statecode == 13 & countycode == 235, NA, v024hispaniclow),
    v024hispanichigh = ifelse(statecode == 13 & countycode == 235, NA, v024hispanichigh)
  )

# Verify the updated dataset
how %>%
  filter((statecode == 51 & countycode == 580) |
           (statecode == 53 & countycode == "027") |
           (statecode == 13 & countycode == 235))



###################################################################################
# compare to Ganhua! 
gan = haven::read_sas(paste0("P:/CH-Ranking/Data/",year,"/3 Data calculated needs checking/", measurenum, "_otherdata.sas7bdat"))

summary(gan %>% select(paste0(measurenum, "_race_white")))
summary(as.numeric(how$v024white))

summary(gan %>% select(paste0(measurenum, "_race_aian")))
summary(as.numeric(how$v024aian))

summary(gan %>% select(paste0(measurenum, "_race_asian")))
summary(as.numeric(how$v024asian))

summary(gan %>% select(paste0(measurenum, "_race_black")))
summary(as.numeric(how$v024black))

summary(gan %>% select(paste0(measurenum, "_race_hispanic")))
summary(as.numeric(how$v024hispanic))

######################################

summary(gan %>% select(paste0(measurenum, "_race_white_cilow")))
summary(as.numeric(how$v024whitelow))

summary(gan %>% select(paste0(measurenum, "_race_aian_cilow")))
summary(as.numeric(how$v024aianlow))

summary(gan %>% select(paste0(measurenum, "_race_asian_cilow")))
summary(as.numeric(how$v024asianlow))

summary(gan %>% select(paste0(measurenum, "_race_black_cilow")))
summary(as.numeric(how$v024blacklow))

summary(gan %>% select(paste0(measurenum, "_race_hispanic_cilow")))
summary(as.numeric(how$v024hispaniclow))

######################################

summary(gan %>% select(paste0(measurenum, "_race_white_cihigh")))
summary(as.numeric(how$v024whitehigh))

summary(gan %>% select(paste0(measurenum, "_race_aian_cihigh")))
summary(as.numeric(how$v024aianhigh))

summary(gan %>% select(paste0(measurenum, "_race_asian_cihigh")))
summary(as.numeric(how$v024asianhigh))

summary(gan %>% select(paste0(measurenum, "_race_black_cihigh")))
summary(as.numeric(how$v024blackhigh))

summary(gan %>% select(paste0(measurenum, "_race_hispanic_cihigh")))
summary(as.numeric(how$v024hispanichigh))



#save to project 
write.csv(how, file = "duplicated_data/v024_subgroup_how.csv", row.names = FALSE)


#save to p drive 
write.csv(how, paste0("P:/CH-Ranking/Data/", year, "/3 Data calculated needs checking/Hannah datasets/", measurenum, "subgroup_h.csv"), row.names = FALSE)



