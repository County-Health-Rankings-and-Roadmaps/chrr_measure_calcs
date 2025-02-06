# code to duplicate v181 (new!)
# libraries 

# get raw data 

aeraw = read.csv("raw_data/PLS_FY2022 PUD_CSV/PLS_FY22_AE_pud22i.csv")
outletraw = read.csv("raw_data/PLS_FY2022 PUD_CSV/pls_fy22_outlet_pud22i.csv")


aeraw = aeraw %>% mutate(
  visits_per_pop = VISITS/POPU_LSA
)

outletae = merge(aeraw, outletraw, by = "FSCSKEY", all = TRUE) 
nojoin = anti_join(outletraw, aeraw, by = "FSCSKEY")
#all merged 

outletae_clean = outletae %>% 
  filter(!is.na(HOURS)) %>% 
  filter(!(STATSTRU.x %in% c(10, 23))) %>% 
  filter(!(PHONE.x %in% c(-3, -4))) %>% 
  filter(HOURS != -3) %>% 
  filter(WKS_OPEN >= 2) %>% 
  filter(C_FSCS.x != "N") %>% 
    filter(C_OUT_TY != "BM") %>% 
    filter(!(STABR.x %in% c("AS", "GU", "MP", "PR", "VI"))) %>% 
    filter(!(VISITS %in% c(-3, -1, 0))) 

# load fipscodes 
cfips = haven::read_sas("P:/CH-Ranking/Data/2025/2 Cleaned data ready for Calculation or Verification/county_fips_with_ct_old.sas7bdat")
sfips = haven::read_sas("P:/CH-Ranking/Data/2025/2 Cleaned data ready for Calculation or Verification/state_fips.sas7bdat")
fips = rbind(cfips, sfips)

# Clean the 'county' column in fips by removing the "county" string and converting to lowercase
fips_cleaned <- fips %>%
  mutate(county_clean = tolower(stringr::str_remove_all(county, "\\b( County | Planning Region| City| Borough| Parish| Census Area)\\b")))

# Clean the 'CNTY.x' column in outletae_clean by converting to lowercase
outletae_clean <- outletae_clean %>%
  mutate(CNTY_clean = tolower(CNTY.x))

# Merge the datasets on the cleaned columns
outletae_fips = merge(outletae_clean, fips_cleaned, by.x = "CNTY_clean", by.y = "county_clean", all.x = TRUE)
nojoino = anti_join(outletae_clean, fips_cleaned, by = c("CNTY_clean" = "county_clean"))
nojoinf = anti_join(fips_cleaned, outletae_clean, by = c("county_clean" = "CNTY_clean"))

