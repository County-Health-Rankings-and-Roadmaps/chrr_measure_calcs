# for calculating v167 school segregation 

# load raw csv downloaded from ELSI 

raw = read.csv("C:/Users/holsonwillia/Documents/chrr_measure_calcs/raw_data/ELSI/ELSI_csv_export.csv", skip = 6)


raw_formatted = raw %>%
  filter(!grepl("JUVENILE|JAIL|PRISON|CORRECTIONAL|VIRTUAL|CYBER", School.Name, ignore.case = TRUE)) %>% 
  filter(School.Type..Public.School..2023.24 != "4-Alternative Education School") %>% 
  filter(!(Start.of.Year.Status..Public.School..2023.24 %in% c("6-Inactive", "2-Closed"))) %>% 
  filter(Total.Students.All.Grades..Excludes.AE...Public.School..2023.24 != 0) %>% 
  filter(!(Total.Students.All.Grades..Excludes.AE...Public.School..2023.24 %in% c("–","†", "‡"))) %>% #need to add this to admin 
  filter(Start.of.Year.Status..Public.School..2023.24 != "7-Future") %>% #need to add this to admin 
  filter(!is.na(County.Number..Public.School..2023.24)) %>% #gotta add to admin 
  mutate(fipscode = str_pad(County.Number..Public.School..2023.24, width = 5, side = "left", pad = "0"), 
         statecode = substr(fipscode, 1,2), 
         countycode = substr(fipscode, 3,5),
         
         # Replace special characters such as '†' and '‡' and '-' with NA
         n_school = str_replace_all(Total.Students.All.Grades..Excludes.AE...Public.School..2023.24, "[†‡-]", NA_character_),
         n_amer_ind = str_replace_all(American.Indian.Alaska.Native.Students..Public.School..2023.24, "[†‡-]", NA_character_),
         n_haw = str_replace_all(Nat..Hawaiian.or.Other.Pacific.Isl..Students..Public.School..2023.24, "[†‡-]", NA_character_),
         n_asian_pi = str_replace_all(Asian.or.Asian.Pacific.Islander.Students..Public.School..2023.24, "[†‡-]", NA_character_),
         n_hispanic = str_replace_all(Hispanic.Students..Public.School..2023.24, "[†‡-]", NA_character_),
         n_black = str_replace_all(Black.or.African.American.Students..Public.School..2023.24, "[†‡-]", NA_character_),
         n_white = str_replace_all(White.Students..Public.School..2023.24, "[†‡-]", NA_character_),
         n_two = str_replace_all(Two.or.More.Races.Students..Public.School..2023.24, "[†‡-]", NA_character_),
         
         
         
         # Convert to numeric and replace NA with 0
         n_school = coalesce(as.numeric(n_school), 0), 
         n_amer_ind = coalesce(as.numeric(n_amer_ind), 0),
         n_asian_pi_haw = coalesce(as.numeric(n_asian_pi),0) + coalesce(as.numeric(n_haw),0),
         n_hispanic = coalesce(as.numeric(n_hispanic), 0),
         n_black = coalesce(as.numeric(n_black), 0),
         n_white = coalesce(as.numeric(n_white), 0),
         n_two = coalesce(as.numeric(n_two), 0),
         
         
         amer_ind_prop = n_amer_ind/n_school,  
         asian_pi_haw_prop = n_asian_pi_haw/n_school, 
         hispanic_prop = n_hispanic/n_school,  
         black_prop = n_black/n_school, 
         white_prop = n_white/n_school, 
         two_prop = n_two/n_school,
         
         theil_school = -(amer_ind_prop * log(ifelse(amer_ind_prop==0, 1, amer_ind_prop)) + 
                            asian_pi_haw_prop * log(ifelse(asian_pi_haw_prop==0, 1, asian_pi_haw_prop)) + 
                            hispanic_prop * log(ifelse(hispanic_prop==0, 1, hispanic_prop)) + 
                            black_prop * log(ifelse(black_prop==0, 1, black_prop)) + 
                            white_prop * log(ifelse(white_prop==0, 1, white_prop)) + 
                            two_prop * log(ifelse(two_prop==0, 1, two_prop)))
         ) 



theil_county = raw_formatted %>% group_by(statecode, countycode) %>% 
  mutate(
    
    nc_totalstudents = sum(n_school, na.rm = TRUE),
    nc_amer_ind = coalesce(sum(n_amer_ind, na.rm = TRUE), 0),  
    nc_asian_pi_haw = coalesce(sum(n_asian_pi_haw, na.rm = TRUE), 0),  
    nc_hispanic = coalesce(sum(n_hispanic, na.rm = TRUE), 0),   
    nc_black = coalesce(sum(n_black, na.rm = TRUE), 0),  
    nc_white = coalesce(sum(n_white, na.rm = TRUE), 0),  
    nc_two = coalesce(sum(n_two, na.rm = TRUE), 0), 
    
    amer_ind_propc = nc_amer_ind/nc_totalstudents,  
    asian_pi_haw_propc = nc_asian_pi_haw/nc_totalstudents, 
    hispanic_propc = nc_hispanic/nc_totalstudents,  
    black_propc = nc_black/nc_totalstudents, 
    white_propc = nc_white/nc_totalstudents, 
    two_propc = nc_two/nc_totalstudents, 
    
    theil_county =  -(amer_ind_propc * log(ifelse(amer_ind_propc==0, 1, amer_ind_propc)) + 
                        asian_pi_haw_propc * log(ifelse(asian_pi_haw_propc==0, 1, asian_pi_haw_propc)) + 
                        hispanic_propc * log(ifelse(hispanic_propc==0, 1, hispanic_propc)) + 
                        black_propc * log(ifelse(black_propc==0, 1, black_propc)) + 
                        white_propc * log(ifelse(white_propc==0, 1, white_propc)) + 
                        two_propc * log(ifelse(two_propc==0, 1, two_propc)))
      )
      



theiltot = theil_county %>% mutate(
  school_weight = n_school / nc_totalstudents,
  theil_school_weight = theil_school*school_weight 
) 

theilf = theiltot %>% 
  group_by(statecode, countycode) %>% 
  mutate(
    theil_county_weight = sum(theil_school_weight, na.rm = TRUE),
    theil_county_final = ifelse(
      rowSums(cbind(nc_amer_ind >= 25, nc_asian_pi_haw >= 25, nc_hispanic >= 25, 
                                               nc_black >= 25, nc_white >= 25, nc_two >= 25)) >1 & 
        n()>1,
                                abs(theil_county - theil_county_weight)/theil_county, 
      NA)
    )

how = theilf %>% group_by(statecode, countycode) %>% select(theil_county_final) %>% distinct()


###################################################################################
# 
gan = haven::read_sas(paste0("P:/CH-Ranking/Data/2025/3 Data calculated needs checking/v167.sas7bdat"))

ganNA = gan %>% filter(is.na(v167_rawvalue))

ghow = merge(gan, how, by = c("statecode", "countycode"))
ghow$diff = ghow$v167_rawvalue - ghow$theil_county_final

check = ghow %>% filter(abs(diff) > 0.002 | (is.na(v167_rawvalue) & !is.na(theil_county_final)) | 
                          (!is.na(v167_rawvalue) & is.na(theil_county_final)))

checkf = theilf %>% 
  semi_join(check, by=c("statecode", "countycode")) 





ganschool = read_csv("tempfiles/theil_value_schools_gl.csv")
ganschool = ganschool %>% group_by(statecode, countycode) %>% 
  mutate(ctot = sum(total_students,na.rm = TRUE), 
         caian = sum(aian, na.rm = TRUE), 
         casian = sum(asian, na.rm = TRUE), 
         chisp = sum(hisp, na.rm =  TRUE),
         cblack = sum(black, na.rm = TRUE), 
         cwhite = sum(white, na.rm = TRUE), 
         ctwo = sum(two, na.rm = TRUE))
example = ganschool %>% filter(school_name == "509J ON-LINE") %>% 
  mutate(theilall = -(aian_prop*log(aian_prop) + asian_prop*log(asian_prop) + hisp_prop*log(hisp_prop) + black_prop*log(black_prop) + white_prop*log(white_prop) + two_prop*log(two_prop)),
         theilc = -(aian_prop*log(aian_prop) + hisp_prop*log(hisp_prop) + white_prop*log(white_prop) + two_prop*log(two_prop)))


checkg = merge(checkf, ganschool, by.x = "School.Name..Public.School..2023.24", by.y = "school_name", all.x = TRUE)

checkg$theil_school_check = checkg$theil_school.x - checkg$theil_school.y


example = checkf %>% filter(statecode == "53" & countycode == "037")
