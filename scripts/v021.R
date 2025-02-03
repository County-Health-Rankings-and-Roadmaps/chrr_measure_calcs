# for high school graduation 

# v021 

# data downloaded from ED Data Express 

library(dplyr)

ed = readr::read_csv("raw_data/EDFacts/edfactsdownload.csv")
hi = readr::read_csv("raw_data/EDFacts/edfactsdownload_hi.csv")
names = readr::read_csv("raw_data/EDFacts/leacrosswalk.csv", skip = 6)
schoolnames = readr::read_csv("raw_data/EDFacts/schoolcrosswalk.csv", skip = 6)


edn = merge(ed, names, by.x ="NCES LEA ID" , by.y = "Agency ID - NCES Assigned [District] Latest available year", all.x = TRUE)

nomatch = ed[is.na(match(ed$`NCES LEA ID`, names$`Agency ID - NCES Assigned [District] Latest available year`)),]


library(dplyr)

edn <- edn %>%
  mutate(
    # Remove extra characters like '%' and whitespace for processing
    clean_value = str_replace_all(Value, "[^0-9<>=.-]", ""),
    newvalue = case_when(
      clean_value == "PS" ~ NA_real_,
      str_detect(clean_value, "^<=") ~ as.numeric(sub("<=", "", clean_value)) / 2,
      str_detect(clean_value, "^<") ~ as.numeric(sub("<", "", clean_value)) / 2,
      str_detect(clean_value, "^>=") ~ as.numeric(sub(">=", "", clean_value)) + (100 - as.numeric(sub(">=", "", clean_value)))/2,
      str_detect(clean_value, "^>") ~ as.numeric(sub(">", "", clean_value)) + (100- as.numeric(sub(">", "", clean_value)))/2,
      str_detect(clean_value, "-") ~ {
        # Process ranges row-wise
        range_vals <- str_split(clean_value, "-", simplify = FALSE) # Produce a list of splits
        sapply(range_vals, function(r) {
          r_numeric = as.numeric(r) 
          
          # Check if both r_numeric[1] and r_numeric[2] are divisible by 5
          # We use modulo (%%) to handle special rounding cases where, for example, a graduation rate reported as 20-29
          # would be treated as 25. This ensures rounding aligns with special cases like these.
          ifelse(
            (r_numeric[1] %% 5 == 0 && r_numeric[2] %% 5 == 0), 
            # If both values are divisible by 5, add 1 to both numbers before calculating the mean
            # This handles cases where both are in a special rounding scenario, rounding both to the next increment of 5.
            mean(c(r_numeric[1] + 1, r_numeric[2] + 1), na.rm = TRUE), 
            # Check if either r_numeric[1] or r_numeric[2] is divisible by 5
            # This would apply when one number is in a special rounding range and needs adjustment.
            ifelse(
              (r_numeric[1] %% 5 == 0 || r_numeric[2] %% 5 == 0), 
              # If one of the values is divisible by 5, add 1 only to r_numeric[2]
              # This ensures we adjust the value of the second number to properly handle rounding scenarios.
              mean(c(r_numeric[1], r_numeric[2] + 1), na.rm = TRUE), 
              # If neither number is divisible by 5, just take the mean without adjustments.
              mean(c(r_numeric[1], r_numeric[2]), na.rm = TRUE)
            )
          )
          
        }) 
      },
      TRUE ~ as.numeric(clean_value) # Convert remaining values directly to numeric
    ),
    # Add spec variable
    spec = case_when(
      str_detect(clean_value, "-") ~ {
        range_vals <- str_split(clean_value, "-", simplify = TRUE)
        range_diff <- abs(as.numeric(range_vals[, 2]) - as.numeric(range_vals[, 1]))
        if_else(range_diff > 5, 0, 1)
      },
      # For >= or <= or > or < values where the value is more than 5 from 0 or 100
      str_detect(clean_value, "^>=") & (as.numeric(sub(">=", "", clean_value)) > 5 & as.numeric(sub(">=", "", clean_value)) < 95) ~ 0,
      str_detect(clean_value, "^<=") & (as.numeric(sub("<=", "", clean_value)) < 95 & as.numeric(sub("<=", "", clean_value)) > 5) ~ 0,
      str_detect(clean_value, "^>") & (as.numeric(sub(">", "", clean_value)) > 5 & as.numeric(sub(">", "", clean_value)) < 95) ~ 0,
      str_detect(clean_value, "^<") & (as.numeric(sub("<", "", clean_value)) < 95 & as.numeric(sub("<", "", clean_value)) > 5) ~ 0,
      is.na(newvalue) ~ 0, 
      
      # Default to 0
      TRUE ~ 1 # Default to 0 for non-range values
    )
  )



edn1 <- edn %>%
  mutate(
    statecode = substr(`County Number [District] 2021-22`, 1, 2),
    countycode = substr(`County Number [District] 2021-22`, nchar(`County Number [District] 2021-22`) - 2, nchar(`County Number [District] 2021-22`)),
    v021_denominator = ifelse(!is.na(newvalue), as.numeric(Denominator), NA),  
    #v021_numerator = ifelse(newvalue != 0, newvalue/100 * v021_denominator, NA)
    v021_numerator = newvalue/100 * v021_denominator 
  ) %>% 
  filter(statecode != "15") #remove Hawaii from this subset since it gets added in separately 



#The "Mountain Education Center School" in Georgia has switched counties and covers students all over the state, so it should be excluded from county totals but kept in state totals: 
edng = edn1 %>% filter(
  !(str_detect(LEA, regex("mountain education", ignore_case = TRUE)) & 
      State == "GEORGIA")
)


ccc = edng %>% group_by(statecode, countycode) %>% 
  summarize(v021_numerator = sum(v021_numerator, na.rm = TRUE),
            v021_denominator = sum(v021_denominator, na.rm = TRUE),
            v021_rawvalue = v021_numerator / v021_denominator, 
            spec_sum = sum(spec, na.rm = TRUE) # Calculate the sum of spec for the group
  ) %>%
  mutate(
    v021_numerator = ifelse(spec_sum == 0, NA, v021_numerator),
    v021_denominator = ifelse(spec_sum == 0, NA, v021_denominator),
    v021_rawvalue = ifelse(spec_sum == 0 | v021_numerator ==0, NA, v021_rawvalue)
  ) %>%
  select(-spec_sum) # Remove the temporary spec_sum column

#add the national value manually. this comes from https://nces.ed.gov/programs/coe/indicator/coi/high-school-graduation-rates
nnn = data.frame(statecode = "00",
                 countycode = "000", 
                 v021_numerator = NA, 
                 v021_denominator = NA, 
                 v021_rawvalue = 0.87)

# for state totals, keep the special georgia case and ignore the specs 
sss = edn1 %>% group_by(statecode) %>% 
  summarize(v021_numerator = sum(newvalue/100 * v021_denominator, na.rm = TRUE),
            v021_denominator = sum(v021_denominator, na.rm = TRUE),
            v021_rawvalue = v021_numerator /v021_denominator) %>% 
  mutate(countycode = "000")

cn = rbind(ccc, nnn)
cns = rbind(cn, sss)



###################################################################################
# for HI only 
hinames = schoolnames %>% filter(`State Abbr [Public School] Latest available year`== "HI") 

# Load necessary library for string manipulation
library(stringr)

# Function to clean and standardize school names
clean_names <- function(name) {
  name <- str_to_upper(name)               # Convert to uppercase
  name <- str_replace_all(name, "[[:punct:]]", "") # Remove punctuation
  name <- str_replace_all(name, "AMP", "") # AMP (which is probably an incorrectly transformed &)
  name <- str_replace_all(name, "\\s+", " ")       # Replace multiple spaces with a single space
  name <- str_trim(name)                   # Trim leading and trailing spaces
  return(name)
}

# Apply the function to standardize both columns
hinames$`School Name` <- clean_names(hinames$`School Name`)
hi$School <- clean_names(hi$School)

common_names <- intersect(hinames$`School Name`, hi$School)



hin = merge(hi, hinames, by.x = "School", by.y = "School Name", all.x = TRUE)
nomatch = hi[is.na(match(hi$School, hinames$`School Name`)),]
# nomatch has zero rows - all joined correctly


hic <- hin %>%
  mutate(
    # Remove extra characters like '%' and whitespace for processing
    clean_value = str_replace_all(Value, "[^0-9<>=.-]", ""),
    newvalue = case_when(
      clean_value == "PS" ~ NA_real_,
      str_detect(clean_value, "^<=") ~ as.numeric(sub("<=", "", clean_value)) / 2,
      str_detect(clean_value, "^<") ~ as.numeric(sub("<", "", clean_value)) / 2,
      str_detect(clean_value, "^>=") ~ as.numeric(sub(">=", "", clean_value)) + (100 - as.numeric(sub(">=", "", clean_value)))/2,
      str_detect(clean_value, "^>") ~ as.numeric(sub(">", "", clean_value)) + (100- as.numeric(sub(">", "", clean_value)))/2,
      str_detect(clean_value, "-") ~ {
        # Process ranges row-wise
        range_vals <- str_split(clean_value, "-", simplify = FALSE) # Produce a list of splits
        sapply(range_vals, function(r) {
          r_numeric = as.numeric(r) 
          
          # Check if both r_numeric[1] and r_numeric[2] are divisible by 5
          # We use modulo (%%) to handle special rounding cases where, for example, a graduation rate reported as 20-29
          # would be treated as 25. This ensures rounding aligns with special cases like these.
          ifelse(
            (r_numeric[1] %% 5 == 0 && r_numeric[2] %% 5 == 0), 
            # If both values are divisible by 5, add 1 to both numbers before calculating the mean
            # This handles cases where both are in a special rounding scenario, rounding both to the next increment of 5.
            mean(c(r_numeric[1] + 1, r_numeric[2] + 1), na.rm = TRUE), 
            # Check if either r_numeric[1] or r_numeric[2] is divisible by 5
            # This would apply when one number is in a special rounding range and needs adjustment.
            ifelse(
              (r_numeric[1] %% 5 == 0 || r_numeric[2] %% 5 == 0), 
              # If one of the values is divisible by 5, add 1 only to r_numeric[2]
              # This ensures we adjust the value of the second number to properly handle rounding scenarios.
              mean(c(r_numeric[1], r_numeric[2] + 1), na.rm = TRUE), 
              # If neither number is divisible by 5, just take the mean without adjustments.
              mean(c(r_numeric[1], r_numeric[2]), na.rm = TRUE)
            )
          )
          
        }) 
      },
      TRUE ~ as.numeric(clean_value) # Convert remaining values directly to numeric
    ),
    # Add spec variable
    spec = case_when(
      str_detect(clean_value, "-") ~ {
        range_vals <- str_split(clean_value, "-", simplify = TRUE)
        range_diff <- abs(as.numeric(range_vals[, 2]) - as.numeric(range_vals[, 1]))
        if_else(range_diff > 5, 0, 1)
      },
      # For >= or <= or > or < values where the value is more than 5 from 0 or 100
      str_detect(clean_value, "^>=") & (as.numeric(sub(">=", "", clean_value)) > 5 & as.numeric(sub(">=", "", clean_value)) < 95) ~ 0,
      str_detect(clean_value, "^<=") & (as.numeric(sub("<=", "", clean_value)) < 95 & as.numeric(sub("<=", "", clean_value)) > 5) ~ 0,
      str_detect(clean_value, "^>") & (as.numeric(sub(">", "", clean_value)) > 5 & as.numeric(sub(">", "", clean_value)) < 95) ~ 0,
      str_detect(clean_value, "^<") & (as.numeric(sub("<", "", clean_value)) < 95 & as.numeric(sub("<", "", clean_value)) > 5) ~ 0,
      is.na(newvalue) ~ 0, 
      
      # Default to 0
      TRUE ~ 1 # Default to 0 for non-range values
    )
  )






# this is hawaii only.... so im setting fipscodes manually 
hic1 = hic %>% 
  mutate(
    statecode = "15" , 
    countycode = case_when(
      `County Name [Public School] 2021-22` == "Honolulu County" ~ "003", 
      `County Name [Public School] 2021-22` == "Hawaii County" ~ "001", 
      `County Name [Public School] 2021-22` == "Maui County" ~ "009", 
      `County Name [Public School] 2021-22` == "Kauai County" ~ "007"
    ), 
    v021_denominator = ifelse(!is.na(newvalue), as.numeric(Denominator), NA), 
    v021_numerator = ifelse(newvalue != 0 & !is.na(newvalue), newvalue/100 * v021_denominator, NA)
  ) 

hiccc = hic1 %>% group_by(statecode, countycode) %>% 
  summarize(v021_numerator = sum(v021_numerator, na.rm = TRUE),
            v021_denominator = sum(v021_denominator, na.rm = TRUE),
            v021_rawvalue = ifelse(v021_numerator == 0, NA, v021_numerator/v021_denominator))

# for hawaii state total
hisss = hic1 %>% group_by(statecode) %>% 
  summarize(v021_numerator = sum(newvalue/100 * v021_denominator, na.rm = TRUE),
            v021_denominator = sum(v021_denominator, na.rm = TRUE),
            v021_rawvalue = v021_numerator /v021_denominator) %>% 
  mutate(countycode = "000")


cnsh = rbind(hiccc, cns)
cnshs = rbind(cnsh, hisss)


##################################################################################
# add fips in 

cfips = haven::read_sas("P:/CH-Ranking/Data/2025/2 Cleaned data ready for Calculation or Verification/county_fips_with_ct_old.sas7bdat")
sfips = haven::read_sas("P:/CH-Ranking/Data/2025/2 Cleaned data ready for Calculation or Verification/state_fips.sas7bdat")
fips = rbind(cfips, sfips)

cnsf = merge(cnshs, fips, by = c("statecode","countycode"), all.y = TRUE)


cnsf = cnsf %>%
  select(statecode, countycode, v021_numerator, v021_denominator, v021_rawvalue)

##################################################################################
# suppress county 13043 in 2025 due to outlier denominator 

# Suppress values in specific columns when the condition is met
cnsf$v021_numerator[cnsf$statecode == "13" & cnsf$countycode == "043"] <- NA
cnsf$v021_denominator[cnsf$statecode == "13" & cnsf$countycode == "043"] <- NA
cnsf$v021_rawvalue[cnsf$statecode == "13" & cnsf$countycode == "043"] <- NA


###################################################################################
# compare with ganhua 

# compare to GL 
year = 2025
measurenum = "v021"
gan = haven::read_sas(paste0("P:/CH-Ranking/Data/",year,"/3 Data calculated needs checking/", measurenum, ".sas7bdat"))


ghow = merge(gan, cnsf, by = c("statecode", "countycode"))
ghow$diffraw = ghow$v021_rawvalue.x - ghow$v021_rawvalue.y
ghow$diffdenom = ghow$v021_denominator.x - ghow$v021_denominator.y
ghow$diffnum = ghow$v021_numerator.x - ghow$v021_numerator.y

nomiss = ghow %>% filter(is.na(v021_rawvalue.x) & !is.na(v021_rawvalue.y))

summary(gan %>% select(paste0(measurenum, "_rawvalue")))
summary(cnsf %>% select(v021_rawvalue))

needfix = ghow %>% filter(abs(diffraw) > 0.001)


################################################################################
# save data 
write.csv(cnsf, file = "duplicated_data/v021_how.csv")
write.csv(cnsf, file = "P:/CH-Ranking/Data/2025/3 Data calculated needs checking/Hannah datasets/v021_how.csv")
        