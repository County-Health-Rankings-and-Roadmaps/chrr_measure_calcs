library(tidyverse)

#################################################################################################
# v177- Voter turnout
# Author: KG
#Description: Percentage of citizen population aged 18 or older who voted in the 2020 U.S. Presidential election.
#Data Source: MIT Election Data and Science Lab; American Community Survey, five-year estimates
  #https://dataverse.harvard.edu/dataverse/medsl_president
  #https://www.census.gov/programs-surveys/decennial-census/about/voting-rights/cvap.2020.html#list-tab-1518558936

#Notes: 
  #Population is the 2020 5-year estimate (2016-2020)
  #There are a couple of weird observations either without a fipscode or with one that doesn't exist
  #All of Alaska is set to missing because their voting data is by district
    #you also have to account for the creation of the new county in AK, 20261 from 02063 and 02066
##################################################################################################



##########COUNTIES###################

####population###

#read in County voting age population 
#grab the 2020 5 year estimate (2016-2020)
county_pop <- read_csv("raw_data/US Census Bureau_CVAP/County_2016_2020.csv") %>% 
#create fipscode variable from geoid
  mutate(fipscode = str_sub(geoid, -5))

#subset the dataset with only the total population 
county_pop_tot<-county_pop %>% filter(lntitle == "Total")


###votes###

#read in the dataset that has votes
county_votes <- read_csv("raw_data/MIT Election Lab/countypres_2000-2024.csv")%>%
#subet to just year 2020
filter(year == 2020)

#there are 4 rows for each county and we just need one
county_votes_clean <- county_votes %>% distinct(fipscode = county_fips, v177_numerator = totalvotes)

###merge and clean up###
v177_merged <- full_join(county_votes_clean, county_pop_tot, by ="fipscode")

#sub-setting the variables and rename the denominator
v177_merged_2 <- v177_merged %>% 
  #get rid of the RI county without a fipscode, AK and PR
  filter(
    !is.na(fipscode),
         !substr(as.character(fipscode), 1, 2) %in% c("02","72")) %>% 
   #get rid of unnecessary variables
  select(geoname, fipscode, v177_numerator, v177_denominator = cvap_est) %>% 
  #calculate v177_rawvalue, cap at 100%
  mutate(v177_rawvalue = v177_numerator / v177_denominator) %>% 
  mutate(v177_rawvalue = pmin(v177_rawvalue, 1)) %>% 
  #create statecode and countycode from fipscode
  mutate(statecode = substr(fipscode, 1, 2),
         countycode = substr(fipscode, 3, 5))


#check to see if that worked
summary(v177_merged_2)

v177_merged_2 %>%
  summarise(
    n = sum(!is.na(v177_rawvalue)),
    mean = mean(v177_rawvalue, na.rm = TRUE),
    sd = sd(v177_rawvalue, na.rm = TRUE),
    min = min(v177_rawvalue, na.rm = TRUE),
    q1 = quantile(v177_rawvalue, 0.25, na.rm = TRUE),
    median = median(v177_rawvalue, na.rm = TRUE),
    q3 = quantile(v177_rawvalue, 0.75, na.rm = TRUE),
    max = max(v177_rawvalue, na.rm = TRUE)
  )

v177_merged_2 %>%
  summarise(
    n = sum(!is.na(v177_denominator)),
    mean = mean(v177_denominator, na.rm = TRUE),
    sd = sd(v177_denominator, na.rm = TRUE),
    min = min(v177_denominator, na.rm = TRUE),
    q1 = quantile(v177_denominator, 0.25, na.rm = TRUE),
    median = median(v177_denominator, na.rm = TRUE),
    q3 = quantile(v177_denominator, 0.75, na.rm = TRUE),
    max = max(v177_denominator, na.rm = TRUE)
  )

##everything matches at the county level



######################STATES########################
###population

state_pop <- read_csv("raw_data/US Census Bureau_CVAP/State_2016_2020.csv") %>% 
  #create statecode variable from geoid
    mutate(statecode = str_sub(geoid, -2))

#subset the dataset with only the total population 
state_pop_tot<-state_pop %>% filter(lntitle == "Total")

###votes

state_votes <- read_csv("raw_data/MIT Election Lab/1976-2020-president.csv")%>% 
#subet to just year 2020
filter(year == 2020)

#there are 4 rows for each county and we just need one
state_votes_clean <- state_votes %>% 
  distinct(statecode = state_fips, v177_numerator = totalvotes)

###merge and clean up
v177_merged_state <- left_join(state_votes_clean, state_pop_tot, by ="statecode") %>% 
  #get rid of unnecessary variables
  select(geoname, statecode, v177_numerator, v177_denominator = cvap_est) %>% 
    #calculate v177_rawvalue, cap at 100
  mutate(v177_rawvalue = v177_numerator / v177_denominator) %>% 
  mutate(v177_rawvalue = pmin(v177_rawvalue, 1)) %>% 
  #add in countycode and fipscode because you'll need them for the merge
  mutate(countycode = "000") %>% 
  mutate(fipscode = paste0(statecode, countycode))
  
#check to see if that worked
summary(v177_merged_state)

v177_merged_state %>%
  summarise(
    n = sum(!is.na(v177_rawvalue)),
    mean = mean(v177_rawvalue, na.rm = TRUE),
    sd = sd(v177_rawvalue, na.rm = TRUE),
    min = min(v177_rawvalue, na.rm = TRUE),
    q1 = quantile(v177_rawvalue, 0.25, na.rm = TRUE),
    median = median(v177_rawvalue, na.rm = TRUE),
    q3 = quantile(v177_rawvalue, 0.75, na.rm = TRUE),
    max = max(v177_rawvalue, na.rm = TRUE)
  )

v177_merged_state %>%
  summarise(
    n = sum(!is.na(v177_denominator)),
    mean = mean(v177_denominator, na.rm = TRUE),
    sd = sd(v177_denominator, na.rm = TRUE),
    min = min(v177_denominator, na.rm = TRUE),
    q1 = quantile(v177_denominator, 0.25, na.rm = TRUE),
    median = median(v177_denominator, na.rm = TRUE),
    q3 = quantile(v177_denominator, 0.75, na.rm = TRUE),
    max = max(v177_denominator, na.rm = TRUE)
  )

##everything matches at the state level


#########National############
#the national value for votes is the sum of the state totals

###population

nation_pop <- read_csv("raw_data/US Census Bureau_CVAP/Nation_2016_2020.csv") %>% 
  #assign fips codes
  mutate(statecode = "00") %>% 
  mutate(countycode = "000") %>% 
  mutate(fipscode = "00000")

#subset the dataset with only the total population 
nation_pop_tot<-nation_pop %>% filter(lntitle == "Total")

########votes

#national votes are the sum of the states votes
state_votes_clean %>%
  summarise(total = sum(v177_numerator, na.rm = TRUE))
#the result of this is 158,528,503

v177_merged_nat <-nation_pop_tot %>% 
  select(geoname, statecode, countycode, fipscode, v177_denominator = cvap_est) %>% 
   mutate(v177_numerator = 158528503) %>% 
#calculate v177_rawvalue, cap at 100
  mutate(v177_rawvalue = v177_numerator / v177_denominator) %>% 
  mutate(v177_rawvalue = pmin(v177_rawvalue, 1))

#####Final Merge and clean up######
v177_a <- bind_rows(v177_merged_nat, v177_merged_state, v177_merged_2) %>% 
  mutate(v177_cilow = NA) %>% 
  mutate(v177_cihigh = NA) %>% 
  mutate(problem = NA)

###Add back in AK and make all the values null. Should end with 3194 observations
library(haven)
#grab the official list of CHRR AK counties in 2023
chrr_fips <-  read_sas("inputs/vintage2023.sas7bdat") %>% 
  filter(statecode=="02",
         countycode != "000") %>% 
  mutate(geoname=county) %>% 
  select(geoname, statecode, countycode, fipscode)

#concatenate with the merged v177 dataset
v177_b<- bind_rows(v177_a, chrr_fips) %>% 
  #delete weird extra county with a non-existent fipscode and the two AK counties that get combined into 02261
  filter(fipscode != "2938000",
         fipscode != "02063",
         fipscode !="02066")

#add in the new AK county
v177<-v177_b %>% 
  select(-geoname) %>% 
  add_row(
    statecode  = "02",
    countycode = "261",
    fipscode   = "02261"
  )
  
#check to see if it all worked.  
summary(v177)

v177 %>%
  summarise(
    n = sum(!is.na(v177_rawvalue)),
    mean = mean(v177_rawvalue, na.rm = TRUE),
    sd = sd(v177_rawvalue, na.rm = TRUE),
    min = min(v177_rawvalue, na.rm = TRUE),
    q1 = quantile(v177_rawvalue, 0.25, na.rm = TRUE),
    median = median(v177_rawvalue, na.rm = TRUE),
    q3 = quantile(v177_rawvalue, 0.75, na.rm = TRUE),
    max = max(v177_rawvalue, na.rm = TRUE)
  )

v177 %>%
  summarise(
    n = sum(!is.na(v177_denominator)),
    mean = mean(v177_denominator, na.rm = TRUE),
    sd = sd(v177_denominator, na.rm = TRUE),
    min = min(v177_denominator, na.rm = TRUE),
    q1 = quantile(v177_denominator, 0.25, na.rm = TRUE),
    median = median(v177_denominator, na.rm = TRUE),
    q3 = quantile(v177_denominator, 0.75, na.rm = TRUE),
    max = max(v177_denominator, na.rm = TRUE)
  )

#saving csv
write.csv(v177, "measure_datasets/v177_r2026.csv",row.names = FALSE)
  
  