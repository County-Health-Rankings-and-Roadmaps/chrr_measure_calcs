# for duplicating v133 food environment index 


# get verified measures from folder 6 
fi = haven::read_sas("P:/CH-Ranking/Data/2025/6 Measure Datasets/Additional Measures/v139.sas7bdat")


# this one isn't updated in 2025 so pulling from previous year's folder 6 
la = haven::read_sas("P:/CH-Ranking/Data/2024/6 Measure Datasets/Additional Measures/v083.sas7bdat")


# merge the two 
fila = merge(fi, la, by = c("statecode", "countycode"), all.x = TRUE)


#do the county calcs 
countylevel = fila %>% filter(countycode != "000") %>% 
  mutate(zdenom = ifelse(!is.na(v139_rawvalue), 
                         (v139_rawvalue - mean(v139_rawvalue, na.rm = TRUE)) / sd(v139_rawvalue, na.rm = TRUE), 
                         NA), 
         znum = ifelse(!is.na(v083_rawvalue), 
                       (v083_rawvalue - mean(v083_rawvalue, na.rm = TRUE)) / sd(v083_rawvalue, na.rm = TRUE), 
                       NA),
        combinedz = (znum + zdenom)/2)
      

# do state level calcs 
statelevel = fila %>% filter(countycode == "000") %>% 
 mutate(  # Using reframe instead of summarize to avoid the deprecation warning
    zdenom = ifelse(!is.na(v139_rawvalue), 
                    (v139_rawvalue - mean(v139_rawvalue, na.rm = TRUE)) / sd(v139_rawvalue, na.rm = TRUE), 
                    NA), 
    znum = ifelse(!is.na(v083_rawvalue), 
                  (v083_rawvalue - mean(v083_rawvalue, na.rm = TRUE)) / sd(v083_rawvalue, na.rm = TRUE), 
                  NA),
    combinedz = (znum + zdenom) / 2)


# get the max and min county level combinedz 
maxvalue = max(countylevel$combinedz, na.rm = TRUE) 
minvalue = min(countylevel$combinedz, na.rm = TRUE)


sc = rbind(countylevel, statelevel)

sc = sc %>% mutate(
  # Ensure no -Inf by excluding NA values
  # caclulate indices for states and counties 
    index = ifelse(!is.na(combinedz), 
                   (maxvalue - combinedz) / 
                     (maxvalue - minvalue) * 10, 
                   NA),
    v133_rawvalue = round(index, 1)
  ) %>% rename(v133_numerator = v083_rawvalue, 
               v133_denominator = v139_rawvalue)





v133 = sc %>% select(v133_rawvalue, statecode, countycode, v133_numerator, v133_denominator)


##################################################################################
# compare to ganhua 

# compare to GL 
year = 2025
measurenum = "v133"
gan = haven::read_sas(paste0("P:/CH-Ranking/Data/",year,"/3 Data calculated needs checking/", measurenum, ".sas7bdat"))

summary(gan %>% select(paste0(measurenum, "_rawvalue")))
summary(v133$v133_rawvalue)

ghow = merge(gan, v133, by = c("statecode", "countycode")) 
ghow$diff = ghow$v133_rawvalue.x - ghow$v133_rawvalue.y


check = ghow %>% filter(abs(diff) > 0)


################################################################################
# save final datasets 

write.csv(v133, file = "P:/CH-Ranking/Data/2025/3 Data Calculated needs checking/Hannah datasets/v133_how.csv")

write.csv(v133, file = "duplicated_data/v133_how.csv")





