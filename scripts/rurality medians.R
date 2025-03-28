# code to duplicate RUCC code median values for all measures 
# written by Hannah Olson-Williams 
# May 2023 
# goal: a single dataset with median measure values for each RUCC code for all measures 
# the final dataset with have three columns: measureid, RUCC code, and median value 
# final dataset stored here: P:\CH-Ranking\Data\Rankings nonannual data maintenance\Compare Counties Tool\Rurality Feature\how

td = haven::read_sas("P:/CH-Ranking/Data/Cumulative Analytic Datasets/2025 Data/t_measure_data.sas7bdat")
td$fips = paste0(td$state_fips, td$county_fips)

# RUCC codes come from https://www.ers.usda.gov/data-products/rural-urban-continuum-codes.aspx

ruc = readxl::read_xlsx("P:/CH-Ranking/Data/2024/9 Results/Rurality medians/Ruralurbancontinuumcodes2023.xlsx")
ruc13 = readxl::read_xls("P:/CH-Ranking/Data/2024/9 Results/Rurality medians/ruralurbancodes2013.xls") %>% 
  filter(State == "CT") %>% rename(RUCC_2023 = RUCC_2013)

rucall = bind_rows(ruc, ruc13)

#check to see that all counties have ruc codes and all ruc codes have counties 
nojoin = dplyr::anti_join(td, rucall, by = c("fips" = "FIPS"))
nojoin_opp = dplyr::anti_join(rucall, td, by = c("FIPS" = "fips"))
unique(nojoin_opp$FIPS)
unique(nojoin$fips)
#no join for connecticut due to county changes - this is expected 


tdr = merge(td, rucall, by.x ="fips", by.y = "FIPS", all.x = TRUE)

nans = tdr %>% filter(is.na(RUCC_2023)) # check that only CT has missings 

tdr$RUCC_2023[is.na(tdr$RUCC_2023)] = 99 #replace missing with a new category to make sure that i can match w kate's sas output (something funky w the way sas versus R handles missings
#created a new category to avoid potential weirdness w missing 

library(dplyr)

tdrl = tdr %>% filter(measure_id != 124) %>% 
  group_by(RUCC_2023, measure_id) %>%
  summarize(measure_med = median(raw_value, na.rm = TRUE))

#verify that the NA vals are in fact CT vals 
#ruccna = tdrl %>% filter(RUCC_2023 == 99)
ruccna = tdrl %>% filter(is.na(RUCC_2023))
ctna = tdr %>% filter(state_fips == "09") %>% 
  group_by(measure_id) %>% 
  summarize(measure_med = median(raw_value, na.rm = TRUE))

ctcheck = merge(ruccna, ctna, by = "measure_id")
ctcheck$diff = ctcheck$measure_med.x - ctcheck$measure_med.y
summary(ctcheck$diff)


write.csv(tdrl, 
          "P:/CH-Ranking/Data/2025/9 Results/Rurality Medians/ruralitymedians_how.csv",
          row.names = FALSE)




#####################################################################################
# compare with kate 
###################################################################################


kate = haven::read_sas("P:/CH-Ranking/Data/2025/9 Results/Rurality Medians/ruc_measure_data_clean.sas7bdat")

hk = merge(kate, tdrl, by = c("measure_id", "RUCC_2023"))
hk$diff = hk$measure_med.x - hk$measure_med.y
summary(hk$diff)

uhoh = hk[abs(hk$diff)>0.01,]
