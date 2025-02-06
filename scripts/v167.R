# for calculating v167 school segregation 

# load raw csv downloaded from ELSI 

raw = read.csv("C:/Users/holsonwillia/Documents/chrr_measure_calcs/raw_data/ELSI/ELSI_csv_export.csv", skip = 6)


raw_formatted = raw %>%
  filter(!grepl("JUVENILE|JAIL|PRISON|CORRECTIONAL|VIRTUAL|CYBER", School.Name, ignore.case = TRUE)) %>% 
  filter(School.Type..Public.School..2023.24 != "4-Alternative Education School") %>% 
  filter(!(Start.of.Year.Status..Public.School..2023.24 %in% c("6-Inactive", "2-Closed"))) %>% 
  filter(Total.Students.All.Grades..Excludes.AE...Public.School..2023.24 != 0) %>% 
  mutate(amer_ind_prop = as.numeric(American.Indian.Alaska.Native.Students..Public.School..2023.24) / as.numeric(Total.Students.All.Grades..Excludes.AE...Public.School..2023.24), 
         asian_pi_haw_prop = as.numeric(Asian.or.Asian.Pacific.Islander.Students..Public.School..2023.24) / as.numeric(Total.Students.All.Grades..Excludes.AE...Public.School..2023.24),
         hispanic_prop = as.numeric(Hispanic.Students..Public.School..2023.24) / as.numeric(Total.Students.All.Grades..Excludes.AE...Public.School..2023.24),
         black_prop = as.numeric(Black.or.African.American.Students..Public.School..2023.24) / as.numeric(Total.Students.All.Grades..Excludes.AE...Public.School..2023.24),
         white_prop = as.numeric(White.Students..Public.School..2023.24) / as.numeric(Total.Students.All.Grades..Excludes.AE...Public.School..2023.24),
         two_prop = as.numeric(Two.or.More.Races.Students..Public.School..2023.24)/ as.numeric(Total.Students.All.Grades..Excludes.AE...Public.School..2023.24))





theils = raw_formatted %>% mutate(
  amer_ind_prop = ifelse(as.numeric(American.Indian.Alaska.Native.Students..Public.School..2023.24) >=25, 
                        as.numeric(American.Indian.Alaska.Native.Students..Public.School..2023.24), NA), 
  asian_pi_haw_prop = ifelse(as.numeric(Asian.or.Asian.Pacific.Islander.Students..Public.School..2023.24)>=25, 
                            as.numeric(Asian.or.Asian.Pacific.Islander.Students..Public.School..2023.24), NA), 
  hispanic_prop = ifelse(as.numeric(Hispanic.Students..Public.School..2023.24) >=25, 
                        as.numeric(Hispanic.Students..Public.School..2023.24), NA), 
  black_prop = ifelse(as.numeric(Black.or.African.American.Students..Public.School..2023.24)>=25, 
                     as.numeric(Black.or.African.American.Students..Public.School..2023.24), NA),
  white_prop = ifelse(as.numeric(White.Students..Public.School..2023.24) >=25, 
                     as.numeric(White.Students..Public.School..2023.24), NA), 
  two_prop = ifelse(as.numeric(Two.or.More.Races.Students..Public.School..2023.24) >=25, 
                   as.numeric(Two.or.More.Races.Students..Public.School..2023.24), NA), 
  theil_school = -(amer_ind_prop*log(amer_ind_prop)+asian_pi_haw_prop*log(asian_pi_haw_prop)+
              hispanic_prop*log(hispanic_prop)+black_prop*log(black_prop)+white_prop*log(white_prop)+two_prop*log(two_prop)) 
)


# need to get fipscodes added in here somewhere 


thiels_county = raw_formatted %>% mutate(
  amer_ind_prop = ifelse(as.numeric(American.Indian.Alaska.Native.Students..Public.School..2023.24) >=25, 
                         as.numeric(American.Indian.Alaska.Native.Students..Public.School..2023.24), NA), 
  asian_pi_haw_prop = ifelse(as.numeric(Asian.or.Asian.Pacific.Islander.Students..Public.School..2023.24)>=25, 
                             as.numeric(Asian.or.Asian.Pacific.Islander.Students..Public.School..2023.24), NA), 
  hispanic_prop = ifelse(as.numeric(Hispanic.Students..Public.School..2023.24) >=25, 
                         as.numeric(Hispanic.Students..Public.School..2023.24), NA), 
  black_prop = ifelse(as.numeric(Black.or.African.American.Students..Public.School..2023.24)>=25, 
                      as.numeric(Black.or.African.American.Students..Public.School..2023.24), NA),
  white_prop = ifelse(as.numeric(White.Students..Public.School..2023.24) >=25, 
                      as.numeric(White.Students..Public.School..2023.24), NA), 
  two_prop = ifelse(as.numeric(Two.or.More.Races.Students..Public.School..2023.24) >=25, 
                    as.numeric(Two.or.More.Races.Students..Public.School..2023.24), NA), 
  theil = -(amer_ind_prop*log(amer_ind_prop)+asian_pi_haw_prop*log(asian_pi_haw_prop)+
              hispanic_prop*log(hispanic_prop)+black_prop*log(black_prop)+white_prop*log(white_prop)+two_prop*log(two_prop)) 
)

         