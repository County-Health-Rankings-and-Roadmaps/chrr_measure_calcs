# for v153 calculations 

year = "2025"
measurenum = "v153"


# load data
#this was pulled using GL's awesome tool 
totraw = read.csv("raw_data/ACS/v153_raw.csv")


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

totraw1 = totraw1 %>% mutate(
  v153_numerator = B25003_002E, 
  v153_denominator = B25003_001E,
  v153_rawvalue = v153_numerator / v153_denominator, 
  se = ifelse(v153_rawvalue == 1, 
              (B25003_002M/1.645) / v153_denominator, 
              ifelse((B25003_002M/1.645) ^2 - (v153_rawvalue^2 * (B25003_001M/1.645) ^2) >=0,
                     (1/v153_denominator) * (sqrt((B25003_002M/1.645)^2 - (v153_rawvalue^2 * (B25003_001M/1.645) ^2))),
                     (1/v153_denominator) * (sqrt((B25003_002M/1.645)^2 + (v153_rawvalue^2 * (B25003_001M/1.645) ^2))))
))


totraw2 = totraw1 %>% mutate(
  v153_cilow = ifelse(v153_rawvalue - (1.96* se) <0, 0, v153_rawvalue - (1.96 * se)),
  v153_cihigh = ifelse(v153_rawvalue + (1.96* se) > 1, 1, v153_rawvalue + (1.96 * se))
)



totraw2 = totraw1 %>% mutate(
  v153_cilow = ifelse(v153_rawvalue - 1.96* se <0 & v153_rawvalue + 1.96* se>1, NA, pmax(v153_rawvalue - 1.96 * se, 0)),
  v153_cihigh = ifelse(v153_rawvalue- 1.96* se <0 & v153_rawvalue+ 1.96* se>1, NA, pmin(v153_rawvalue + 1.96 * se, 1)),
)


####################################################################################
# add in fipscodes 
cfips = haven::read_sas("P:/CH-Ranking/Data/2025/2 Cleaned data ready for Calculation or Verification/county_fips_with_ct_old.sas7bdat")
sfips = haven::read_sas("P:/CH-Ranking/Data/2025/2 Cleaned data ready for Calculation or Verification/state_fips.sas7bdat")
fips = rbind(cfips, sfips)

all = merge(fips, totraw2, by= c("statecode", "countycode"), all.x = TRUE)



##################################################################################
# compare w ganhua 

gan = haven::read_sas("P:/CH-Ranking/Data/2025/3 Data calculated needs checking/v153.sas7bdat")

summary(all$v153_cihigh)
summary(gan$v153_cihigh)

summary(all$v153_cilow)
summary(gan$v153_cilow)

ghow = merge(gan, all, by = c("statecode", "countycode"))
ghow$diff = ghow$v153_rawvalue.x - ghow$v153_rawvalue.y
ghow$cilowdiff = ghow$v153_cilow.x - ghow$v153_cilow.y
ghow$cihighdiff = ghow$v153_cihigh.x - ghow$v153_cihigh.y



# clean it up a lil 

v153_how = all %>% select(statecode, countycode, v153_rawvalue, v153_cihigh, v153_cilow, v153_numerator, v153_denominator)

write.csv(v153_how, "duplicated_data/v153_how.csv")
write.csv(v153_how, file = "P:/CH-Ranking/Data/2025/3 Data calculated needs checking/Hannah datasets/v153_how.csv") 
 