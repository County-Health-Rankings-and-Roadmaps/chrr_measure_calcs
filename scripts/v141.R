# ganhua suggests changing to using tract-level data only for this measure 

# get data from Ganhua's nifty tool 

ttt = read.csv("raw_data/ACS/v141_tract.csv")
#ccc = read.csv("raw_data/ACS/v141_county.csv")


#ccc$statecode = stringr::str_pad(ccc$statecode, width = 2,side = "left", pad = "0")
#ccc$countycode = stringr::str_pad(ccc$countycode, width = 3, side = "left", pad = "0")
ttt$statecode = stringr::str_pad(ttt$statecode, width = 2,side = "left", pad = "0")
ttt$countycode = stringr::str_pad(ttt$countycode, width = 3, side = "left", pad = "0")




#ccc$btot = ccc$B11002B_001E #county level black pop  
#ccc$wtot = ccc$B11002A_001E #county level white pop 
#ccc$tot = ccc$B11002_001E #total county tot pop 

ttt$bi = ttt$B11002B_001E #tract level black pop
ttt$wi = ttt$B11002A_001E #tract level white pop 
ttt$toti = ttt$B11002_001E #tract level tot pop 


ccc = ttt %>% group_by(statecode, countycode) %>% 
  summarize(btot = sum(bi, na.rm = TRUE), 
            wtot = sum(wi, na.rm = TRUE), 
            tot = sum(toti, na.rm = TRUE))


####################################################
# connecticut stuff..... 
#load the ct tract crosswalk 
cttract = haven::read_sas("P:/CH-Ranking/Data/2025/2 Cleaned data ready for Calculation or Verification/ct_tract_crosswalk.sas7bdat")

ctonly = ttt %>% filter(statecode == "09") %>% 
  mutate(GEOID = stringr::str_pad(GEOID, width =11,side = "left", pad = "0"))

ctwalk = merge(cttract, ctonly, by.x = "tract_fipscode_2022",
               by.y = "GEOID")

ctwalk = ctwalk %>% select(statecode, countycode, bi, wi, toti) %>% 
  group_by(countycode) %>% 
  mutate(btot = sum(bi, na.rm = TRUE), 
         wtot = sum(wi, na.rm = TRUE))

#ctwalk now contains the corrected ct fipscodes for 2025 release 




data <- ttt %>% 
  left_join(
    ccc %>% select(statecode, countycode, btot, wtot),
    by = c("statecode", "countycode")
  ) 


data_ct = data %>% filter(statecode != "09") %>% 
  bind_rows(ctwalk)

# Identify GEOIDs in `data` but not in `data_ct`
geoids_not_in_data_ct <- data_ct %>%
  anti_join(data, by = "GEOID") %>%
  select(GEOID) %>%
  distinct()


# calculate the index of dissimilarity for each state 

# get state totals first 
# note that no suppression is applied!! 
s1 = data_ct %>% 
  filter(countycode != "000") %>% 
  group_by(statecode) %>% 
  summarize(
    B = sum(bi),  # Total black population in state
    W = sum(wi), # Total white population in state
    bi_B = bi / B,  # Proportion of black population in tract
    wi_W = wi / W,  # Proportion of white population in tract
    abs_diff = abs(bi_B - wi_W))  # Set to NA if missing

states = s1 %>% group_by(statecode) %>% 
  summarize(
    rawvalue = 50 * sum(abs_diff, na.rm = TRUE))  # (1/2) SUM|(bi/B) - (wi/W)| * 100)
states$countycode = "000"

# Calculate the index of dissimilarity components for each tract for countycalcs
c1 <- data_ct %>% filter(countycode != "000") %>% 
  mutate(
    B = btot,  # Total black population in county
    W = wtot,  # Total white population in county
    bi_B = bi / B,  # Proportion of black population in tract
    wi_W = wi / W,  # Proportion of white population in tract
    abs_diff = if_else(is.na(bi_B) | is.na(wi_W), NA_real_, abs(bi_B - wi_W))  # Set to NA if missing
  )

# Step 3: Sum the components by county and calculate the index
county <- c1 %>%
  group_by(statecode, countycode) %>% 
  filter(B>=100) %>% 
  summarize(
   rawvalue = 50 * sum(abs_diff))  # (1/2) SUM|(bi/B) - (wi/W)| * 100)

county$rawvalue = ifelse(county$rawvalue == 0, NA, county$rawvalue)




sctot = dplyr::bind_rows(states, county) 


cfips = haven::read_sas("P:/CH-Ranking/Data/2025/2 Cleaned data ready for Calculation or Verification/county_fips_with_ct_old.sas7bdat")
sfips = haven::read_sas("P:/CH-Ranking/Data/2025/2 Cleaned data ready for Calculation or Verification/state_fips.sas7bdat")
fips = rbind(cfips, sfips)

scall = merge(fips, sctot, by= c("statecode", "countycode"), all.x = TRUE)

scall <- scall %>%
  mutate(
    # Calculate the median for countycode = 000
    rawvalue = if_else((statecode == "00" & countycode == "000"),
      median(rawvalue[countycode == "000"], na.rm = TRUE),
      rawvalue
    )
  )






#######################################################################
# compare to GL 
year = 2025
measurenum = "v141"
gan = haven::read_sas(paste0("P:/CH-Ranking/Data/",year,"/3 Data calculated needs checking/", measurenum, ".sas7bdat"))

summary(gan %>% select(paste0(measurenum, "_rawvalue")))
summary(scall$rawvalue)

ghow = merge(gan, scall, by = c("statecode", "countycode")) 
ghow$diff = ghow$v141_rawvalue - ghow$rawvalue


####################################################################################
### trouble shooting 
weird = data_ct %>% filter(statecode == "36" & countycode %in% c("103", "111"))
#the tract level values here match GL's ... but it's unclear if the county level values match 

# using dane as a test case 
daneccc = ccc %>% filter(statecode == "55" & countycode == "025")
danettt = ttt %>% filter(statecode == "55" & countycode == "025")
danetots = danettt %>% summarize(bcounty = sum(bi), 
                      wcounty = sum(wi), 
                      countytot = sum(toti))
#the values in daneccc match the values in danetots .... so this should be true of the ny counties too 


# now suffolk 
suffolkccc = ccc %>% filter(statecode == "36" & countycode == "103")
suffolkttt = ttt %>% filter(statecode == "36" & countycode == "103")
suffolktots = suffolkttt %>% summarize(bcounty = sum(bi), 
                                 wcounty = sum(wi), 
                                 countytot = sum(toti))

# now ulster 
ulsterccc = ccc %>% filter(statecode == "36" & countycode == "111")
ulsterttt = ttt %>% filter(statecode == "36" & countycode == "111")
ulstertots = ulsterttt %>% summarize(bcounty = sum(bi), 
                                       wcounty = sum(wi), 
                                       countytot = sum(toti))

# all at once 
discrepancies = data_ct %>% group_by(statecode, countycode) %>% 
  summarize(b_tractsum = sum(bi, na.rm = TRUE),
            w_tractsum = sum(wi, na.rm = TRUE), 
            tot_tractsum = sum(toti, na.rm = TRUE),
            bdiff = btot - b_tractsum,
            wdiff = wtot - w_tractsum)

write.csv(discrepancies, file = "tempfiles/discrepancies.csv")
