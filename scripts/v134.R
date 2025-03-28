# for calculating alcohol impaired driving deaths (v134) 

# load raw data
# first load and process the 2018 - 2020 datasets (handle 2021 and 2022 differently)
acc18 = haven::read_sas("P:/CH-Ranking/Data/2025/1 Raw Data/NHTSA/FARS2018/accident.sas7bdat")
acc19 = haven::read_sas("P:/CH-Ranking/Data/2025/1 Raw Data/NHTSA/FARS2019/accident.sas7bdat")
acc20 = haven::read_sas("P:/CH-Ranking/Data/2025/1 Raw Data/NHTSA/FARS2020/accident.sas7bdat")


comb_acc <- bind_rows(acc18, acc19, acc20)
comb_acc = comb_acc %>% mutate(
  v134_numerator = DRUNK_DR, 
  alcohol_impaired_driving_death = ifelse(v134_numerator >0, 
                                          "FATALS", 
                                          0)
)




acc21 = haven::read_sas("P:/CH-Ranking/Data/2025/1 Raw Data/NHTSA/FARS2021/accident.sas7bdat")
acc22 = haven::read_sas("P:/CH-Ranking/Data/2025/1 Raw Data/NHTSA/FARS2022/accident.sas7bdat")
per21 = haven::read_sas("P:/CH-Ranking/Data/2025/1 Raw Data/NHTSA/FARS2021/person.sas7bdat")
per22 = haven::read_sas("P:/CH-Ranking/Data/2025/1 Raw Data/NHTSA/FARS2022/person.sas7bdat")

comb_acc2 = bind_rows(acc21, acc22)
comb_per = bind_rows(per21, per22)
drunk_cases = comb_per %>% mutate(
  st_case = ifelse(PER_TYPE == 1 & police_report == 1 | alcoholtest >0 & alcoholtest<95,
                   1, 0)
  
)