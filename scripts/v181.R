# code to duplicate v181 (new!)
# libraries 

# get raw data 

library(tidyverse)

aeraw = read.csv("raw_data/PLS_FY2022 PUD_CSV/PLS_FY22_AE_pud22i.csv")
outletraw = read.csv("raw_data/PLS_FY2022 PUD_CSV/pls_fy22_outlet_pud22i.csv")




aeclean = aeraw %>% 
  #filter(!is.na(HOURS)) %>% 
  filter(!(STATSTRU %in% c(23))) %>% #c(10, 23))) %>% 
  filter(!(PHONE %in% c(-3))) %>% #, -4))) %>% 
  #filter(HOURS != -3) %>% 
  #filter(STARTDAT != -3) %>% #start and end date shoudl be chekced - unclear that they need to be deleted..... 
  #filter(ENDDATE != -3) %>% 
  filter(C_LEGBAS != "SD") %>% 
  filter(HRS_OPEN != -3) %>% 
  #filter(WKS_OPEN >= 2) %>% 
  filter(C_FSCS != "N") %>% 
  #filter(C_OUT_TY != "BM") %>% 
  filter(!(STABR %in% c("AS", "GU", "MP", "PR", "VI"))) %>% 
  filter(!(VISITS %in% c(-3, -1, 0))) %>% 
  mutate(totcode = str_pad(CENTRACT, width = 11, pad = "0", side = "left"),
    statecode = substr(totcode, 1, 2), 
         countycode = substr(totcode, 3,5),
    visits_per_pop = VISITS/POPU_UND)




statevals = aeclean %>% group_by(statecode) %>% 
  summarize(numerator = sum(VISITS, na.rm = TRUE),
            denominator = sum(POPU_UND, na.rm = TRUE), 
            med_visit = numerator/denominator) %>% 
  mutate(countycode = "000")

ntlval = aeclean %>%
  summarize(numerator = sum(VISITS, na.rm = TRUE),
            denominator = sum(POPU_UND, na.rm = TRUE), 
            med_visit = numerator/denominator) %>% 
  mutate(statecode = "00", 
         countycode = "000")



outletclean = outletraw %>% 
  filter(!is.na(HOURS)) %>% 
  filter(!(STATSTRU %in% c(10, 23))) %>% 
  filter(!(PHONE %in% c(-3))) %>%  # , -4))) %>% 
  filter(HOURS != -3) %>% 
# filter(STARTDAT != -3) %>% #start and end date shoudl be chekced - unclear that they need to be deleted..... 
# filter(ENDDATE != -3) %>% 
# filter(C_LEGBAS != "SD") %>% 
# filter(HRS_OPEN != -3) %>% 
  filter(WKS_OPEN >= 2) %>% 
  filter(C_FSCS != "N") %>% 
  filter(C_OUT_TY != "BM") %>% 
  filter(!(STABR %in% c("AS", "GU", "MP", "PR", "VI"))) %>% 
# filter(!(VISITS %in% c(-3, -1, 0))) %>% 
  mutate(totcode = str_pad(CENTRACT, width = 11, pad = "0", side = "left"),
         statecode = substr(totcode, 1, 2), 
         countycode = substr(totcode, 3,5),
        # visits_per_pop = VISITS/POPU_UND,
        onelib = 1) %>% 
  group_by(FSCSKEY) %>%
  mutate(nlibrary = n()) %>%
  ungroup()

# the section below is for handling the instances where ae fipscode and outlet fipscode do not match - it is NOT NEEDED 
#outletae = merge(outletclean, aeclean, by = "FSCSKEY", all= TRUE)

#nodups = outletae %>% filter(countycode.x != countycode.y) %>% select(VISITS, POPU_UND, LIBNAME.x, LIBNAME.y, statecode.x, countycode.x, statecode.y, countycode.y)

#look at an example to understand 
#outletae_spec = outletae %>% filter(statecode.x =="05" & countycode.x == "109" | statecode.y == "05" & countycode.y == "119")  %>% select(VISITS, POPU_UND, LIBNAME.x, LIBNAME.y, statecode.x, countycode.x, statecode.y, countycode.y)


# Subset for cases where countycode.x is equal to countycode.y
#outletae_equal <- outletae %>%
#  filter(countycode.x == countycode.y) %>%
#  mutate(countycode = countycode.x, statecode = statecode.x)

# Subset for cases where countycode.x is not equal to countycode.y
#outletae_diff <- outletae %>%
#  filter(countycode.x != countycode.y) %>%
#  gather(key = "countycode_type", value = "countycode", countycode.x, countycode.y) %>%
#  select(-countycode_type)

# Combine the two datasets back together
#outletae_combined <- bind_rows(outletae_equal, outletae_diff)


#outletaecc = outletae_combined %>% filter(!is.na(HOURS))



#meds = outletae_combined %>% group_by(statecode, countycode) %>%
#  summarize(med_visit = median(visits_per_pop, na.rm = TRUE))

outletae = merge(outletclean, aeclean, by = "FSCSKEY", all.x = TRUE)


# export a dataset to run cipctldf in sas 
#outletae_forsas = outletae %>% select(statecode.x, countycode.x, visits_per_pop)

#write.csv(outletae_forsas, file = "tempfiles/v181_needcis.csv", row.names = FALSE)

meds = outletae %>% group_by(statecode.x, countycode.x) %>%
  rename(statecode = statecode.x, countycode = countycode.x) %>% 
  summarize(med_visit = median(visits_per_pop, na.rm = TRUE))


# load fipscodes 
cfips = haven::read_sas("P:/CH-Ranking/Data/2025/2 Cleaned data ready for Calculation or Verification/county_fips_with_ct_old.sas7bdat")
sfips = haven::read_sas("P:/CH-Ranking/Data/2025/2 Cleaned data ready for Calculation or Verification/state_fips.sas7bdat")
fips = rbind(cfips, sfips)


oa_cfips = merge(cfips, meds, by = c("statecode", "countycode"), all.x = TRUE) %>% filter(countycode != "000")


how = bind_rows(oa_cfips, statevals, ntlval)


########################################################
# compare to kate 
#kate = haven::read_sas("P:/CH-Ranking/Data/2025/3 Data calculated needs checking/Kate datasets/Libraries/v181_st_natl.sas7bdat")
kate = haven::read_sas("P:/CH-Ranking/Data/2025/3 Data calculated needs checking/v181.sas7bdat")


katehow = merge(kate, how, by = c("statecode", "countycode"))

katehow$diffnum = katehow$numerator - katehow$v181_numerator

katehow$diffdenom = katehow$denominator - katehow$v181_denominator
katehow$diffraw = katehow$med_visit - katehow$v181_rawvalue
katehow$difflow = katehow$cilow - katehow$v181_cilow
katehow$diffhigh = katehow$cihigh - katehow$v181_cihigh


nomatch = katehow %>% filter(diffnum != 0)


checkci = katehow %>% filter(abs(cilow-v181_cilow) > 0.001 | abs(cihigh -v181_cihigh) >0.001 | 
                               is.na(cilow) & !is.na(v181_cilow) | is.na(v181_cilow) & !is.na(cilow) | 
                               is.na(cihigh) & !is.na(v181_cihigh) | is.na(v181_cihigh) & !is.na(cihigh))

#check a subset to see 
outletae$fips = paste0(outletae$statecode.x, outletae$countycode.x)
temp = outletae %>% filter(fips %in% nomatch$fips) %>% 
  select(statecode.y, countycode.y, statecode.x, countycode.x,visits_per_pop, VISITS, POPU_UND, LIBNAME.x, LIBNAME.y)

katehowtemp = katehow %>% filter(statecode == "05" & countycode %in% c("035", "111", "000") | statecode == "05" & countycode %in% c("105", "119")) 







