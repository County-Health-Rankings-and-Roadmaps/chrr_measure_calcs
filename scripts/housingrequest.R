# housing cost special request 

# 4 columns for housing cost burden (v154): 2 cols for renters, 2 cols for owners housing cost burden (30% or more - sum some columns directly from ACS 5 year table for 2023- check admin page for clear documentation)and also 50% or more (this is already on the pdrive (num and denom of v154)  fipscode, county name, state and then v153 (homeownership)  

cfips = haven::read_sas("P:/CH-Ranking/Data/2025/2 Cleaned data ready for Calculation or Verification/county_fips_with_ct_old.sas7bdat")
sfips = haven::read_sas("P:/CH-Ranking/Data/2025/2 Cleaned data ready for Calculation or Verification/state_fips.sas7bdat")
fips = rbind(cfips, sfips)


#use v154 to check my new calcs 
v154 = haven::read_sas("P:/CH-Ranking/Data/2025/6 Measure Datasets/Additional Measures/v154.sas7bdat")

# need v153 as the final column for keith  
v153 = haven::read_sas("P:/CH-Ranking/Data/2025/3 Data calculated needs checking/v153.sas7bdat")


v153 = v153 %>% 
  select(statecode, countycode, v153_rawvalue)






##################################################################################
# pull in raw ACS data from tidycensus 

acsvars = c(#renters 50 or more  
            "B25074_009",
            "B25074_018",
            "B25074_027",
            "B25074_036",
            "B25074_045",
            "B25074_054",
            "B25074_063",
            
            "B25074_001", #total renters 
            "B25074_010", #renters not computed  
            "B25074_019",
            "B25074_028",
            "B25074_037",
            "B25074_046",
            "B25074_055", 
            "B25074_064", 
            
            #homeowners 50 or more 
            "B25095_001", #total homeowners 
            "B25095_009", 
            "B25095_018",
            "B25095_027",
            "B25095_036",
            "B25095_045",
            "B25095_054",
            "B25095_063",
            "B25095_072",
            
            #homeowners not computed 50 or more 
            "B25095_010", 
            "B25095_019",
            "B25095_028",
            "B25095_037",
            "B25095_046",
            "B25095_055",
            "B25095_064",
            "B25095_073",
            
            #renters 30, 35, 40 
            "B25074_006",
            "B25074_007",
            "B25074_008",
            "B25074_015",
            "B25074_016",
            "B25074_017",
            "B25074_024",
            "B25074_025",
            "B25074_026",
            "B25074_033",
            "B25074_034",
            "B25074_035",
            "B25074_042",
            "B25074_043",
            "B25074_044",
            "B25074_051",
            "B25074_052",
            "B25074_053",
            "B25074_060",
            "B25074_061",
            "B25074_062",
            
           # owners 30, 35, 40 
           "B25095_006",
           "B25095_007",
           "B25095_008", 
           "B25095_015",
           "B25095_016",
           "B25095_017",
           "B25095_024",
           "B25095_025",
           "B25095_026",
           "B25095_033",
           "B25095_034",
           "B25095_035",
           "B25095_042",
           "B25095_043",
           "B25095_044",
           "B25095_051",
           "B25095_052",
           "B25095_053",
           "B25095_060",
           "B25095_061",
           "B25095_062")

new154 = tidycensus::get_acs(geography = "county", 
                    variables = acsvars, 
                    year = 2023, 
                    survey = "acs5", 
                    geometry = FALSE)


new154sums = new154 %>% group_by(GEOID) %>% 
  summarize(
  renters30_num = sum(estimate[variable %in% c( "B25074_006",
                                                "B25074_007",
                                                "B25074_008",
                                                "B25074_015",
                                                "B25074_016",
                                                "B25074_017",
                                                "B25074_024",
                                                "B25074_025",
                                                "B25074_026",
                                                "B25074_033",
                                                "B25074_034",
                                                "B25074_035",
                                                "B25074_042",
                                                "B25074_043",
                                                "B25074_044",
                                                "B25074_051",
                                                "B25074_052",
                                                "B25074_053",
                                                "B25074_060",
                                                "B25074_061",
                                                "B25074_062",
                                                "B25074_009",
                                                "B25074_018",
                                                "B25074_027",
                                                "B25074_036",
                                                "B25074_045",
                                                "B25074_054",
                                                "B25074_063")], na.rm = TRUE), 
  
  owners30_num = sum(estimate[variable %in% c("B25095_006",
                                              "B25095_007",
                                              "B25095_008", 
                                              "B25095_015",
                                              "B25095_016",
                                              "B25095_017",
                                              "B25095_024",
                                              "B25095_025",
                                              "B25095_026",
                                              "B25095_033",
                                              "B25095_034",
                                              "B25095_035",
                                              "B25095_042",
                                              "B25095_043",
                                              "B25095_044",
                                              "B25095_051",
                                              "B25095_052",
                                              "B25095_053",
                                              "B25095_060",
                                              "B25095_061",
                                              "B25095_062",
                                              "B25095_009", 
                                              "B25095_018",
                                              "B25095_027",
                                              "B25095_036",
                                              "B25095_045",
                                              "B25095_054",
                                              "B25095_063",
                                              "B25095_072")], na.rm = TRUE), 
  
  
  owners50_num = sum(estimate[variable %in% c("B25095_009",
                                          "B25095_018",
                                          "B25095_027",
                                          "B25095_036",
                                          "B25095_045",
                                          "B25095_054",
                                          "B25095_063",
                                          "B25095_072")], na.rm = TRUE), 
  owners_denom = estimate[variable == "B25095_001"] - 
    sum(estimate[variable %in% c("B25095_010",
                                     "B25095_019",
                                     "B25095_028",
                                     "B25095_037",
                                     "B25095_046",
                                     "B25095_055",
                                     "B25095_064",
                                     "B25095_073")], na.rm = TRUE),
  renters50_num = sum(estimate[variable %in% c("B25074_009",
                                           "B25074_018",
                                           "B25074_027",
                                           "B25074_036",
                                           "B25074_045",
                                           "B25074_054",
                                           "B25074_063")], na.rm = TRUE),
  renters_denom = estimate[variable == "B25074_001"] - 
          sum(estimate[variable %in% c("B25074_010",
                                    "B25074_019",
                                    "B25074_028",
                                    "B25074_037",
                                    "B25074_046",
                                    "B25074_055", 
                                    "B25074_064")], na.rm = TRUE)
  )




newnew = merge(new154sums, fips, by.x = "GEOID", by.y = "fipscode", all.y = TRUE) %>% 
  filter(statecode != "00", countycode != "000")

newnewstates = newnew %>% group_by(statecode) %>%
  filter(statecode != "00") %>% 
  summarize(renters30_num = sum(renters30_num, na.rm = TRUE), 
            owners30_num = sum(owners30_num, na.rm = TRUE), 
            renters50_num = sum(renters50_num, na.rm = TRUE), 
            owners50_num = sum(owners50_num, na.rm = TRUE), 
            owners_denom = sum(owners_denom, na.rm = TRUE),
            renters_denom = sum(renters_denom, na.rm = TRUE)) %>% 
  mutate(countycode = "000")

newnewntl = newnew %>% filter(statecode != "00" | countycode != "000") %>% 
  summarize(renters30_num = sum(renters30_num, na.rm = TRUE), 
            owners30_num = sum(owners30_num, na.rm = TRUE), 
            renters50_num = sum(renters50_num, na.rm = TRUE), 
            owners50_num = sum(owners50_num, na.rm = TRUE), 
            owners_denom = sum(owners_denom, na.rm = TRUE),
            renters_denom = sum(renters_denom, na.rm = TRUE)) %>% 
  mutate(countycode = "000",
         statecode = "00")


newtot = bind_rows(newnewntl, newnewstates, newnew)

neww153 = merge(newtot, v153, by = c("statecode", "countycode"))



request = neww153 %>% mutate(
  renters30 = renters30_num / renters_denom, 
  owners30 = owners30_num/ owners_denom, 
  renters50 = renters50_num/ renters_denom, 
  owners50 = owners50_num / owners_denom
) %>% select(-c(state, county, GEOID))



#add the fips back in one more time to get state names etc 
requestf = merge(request, fips, by = c("statecode", "countycode"), all.y = TRUE)

write.csv(requestf, "tempfiles/housingdata.csv", row.names = FALSE)

