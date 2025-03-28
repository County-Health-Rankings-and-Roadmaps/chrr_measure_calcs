sfa = haven::read_sas("P:/CH-Ranking/Data/2025/6 Measure Datasets/Additional Measures/v169.sas7bdat")
rs = haven::read_sas("P:/CH-Ranking/Data/2025/6 Measure Datasets/Additional Measures/v159.sas7bdat")

sfact = sfa %>% 
  mutate(v169_rawvalue = ifelse(statecode == "09" & is.na(v169_rawvalue), 
                                              sfa$v169_rawvalue[sfa$statecode == "09" & sfa$countycode == "000"],
                                              v169_rawvalue)) %>% 
  filter(v169_flag_CT != "A") %>% 
  filter(countycode != "000")

rsct = rs %>% 
  mutate(v159_rawvalue = ifelse(statecode == "09" & is.na(v159_rawvalue), 
                                              rs$v159_rawvalue[rs$statecode == "09" & rs$countycode == "000"],
                                              v159_rawvalue)) %>% 
  filter(v159_flag_CT != "A") %>% 
  filter(countycode != "000")



sdrs = sd(rsct$v159_rawvalue, na.rm = TRUE)
mrs = mean(rsct$v159_rawvalue, na.rm = TRUE)


msfa = mean(sfact$v169_rawvalue, na.rm = TRUE)
sdsfa = sd(sfact$v169_rawvalue, na.rm = TRUE)
