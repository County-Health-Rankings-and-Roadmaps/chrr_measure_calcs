##################################################################################
# function to pull ACS ratios 
#################################################################################

library(tidyverse)


acsratios = function(measurenum, acscolumns, apitype, num, denom) {

colstring = paste0(acscolumns, collapse = ",")
key = "&key=a834029996fccc1b2766431ebcd51332c2c6e0d0"
initial = paste0("https://api.census.gov/data/", year-2, "/acs/acs5")
typeurl = ifelse(apitype == "detailed", "?", 
                 ifelse(apitype == "subject", "/subject?", 
                        ifelse(apitype == "profile", "/profile?",
                               "/cprofile")))
urlc = paste0(initial, typeurl, "get=NAME,", colstring, "&for=county:*", key)
urls = paste0(initial, typeurl, "get=NAME,", colstring, "&for=state:*", key)
urln = paste0(initial, typeurl, "get=NAME,", colstring, "&for=us:1", key)

c = read.csv(urlc)
s = read.csv(urls)
n = read.csv(urln)

c$county = c$county.
s$state = s$state.
s$county = "000"
n$state = "00"
n$county = "000"

a = rbind(c[,intersect(colnames(c), colnames(s))], s[,intersect(colnames(c), colnames(s))])
b = rbind(a[,intersect(colnames(a), colnames(n))], n[,intersect(colnames(a), colnames(n))])

b$state = gsub("]", "", b$state)
b$statecode = stringr::str_pad(b$state, 2, side = "left", "0")
b$county = gsub("]","", b$county)
b$countycode = stringr::str_pad(b$county, 3, side = "left", "0")

b$num = b[,num]
b$denom = b[,denom]

# the following fips need update/correction 
# 02261 v-c -> 02063 chugach 
# 02261 v-c -> 02066 copper river



b$rawvalue = ifelse(b$num<250000, b$num/b$denom, NA)

b$sedenom = ifelse(b[,gsub("E", "M",denom)] >0, b[,gsub("E", "M",denom)]/1.645, NA) 
b$senum = ifelse(b[,gsub("E", "M",num)]>0, b[,gsub("E", "M",num)]/1.645, NA)

if(calctype == "ratio"){
  b$sep = (1/b$denom)*sqrt(b$senum^2 + b$rawvalue^2*b$sedenom^2)
  b$cihigh = b$rawvalue +(1.96*b$sep)
}else{ 
  b$sep = ifelse(b$rawvalue == 1, b$senum/b$denom, 
                  ifelse(b$senum^2 - (b$rawvalue^2 * b$sedenom^2) <0, 
                         (1/b$denom)*sqrt(b$senum^2 + (b$rawvalue^2*b$sedenom^2)),
                         (1/b$denom)*sqrt(b$senum^2 - (b$rawvalue^2*b$sedenom^2))))
  b$cihigh = ifelse(b$rawvalue + (1.96*b$sep)>=1, 1, b$rawvalue + (1.96*b$sep))
}


b$cilow = ifelse(b$rawvalue -(1.96*b$sep)<0, 0, b$rawvalue - (1.96*b$sep))
# b$cihigh = ifelse(calctype == "ratio", b$rawvalue +(1.96*b$sep), 
#                    ifelse(b$rawvalue + (1.96*b$sep)>=1, 1, b$rawvalue + (1.96*b$sep)))


cfips = haven::read_sas("P:/CH-Ranking/Data/2025/2 Cleaned data ready for Calculation or Verification/county_fips_with_ct_old.sas7bdat")
sfips = haven::read_sas("P:/CH-Ranking/Data/2025/2 Cleaned data ready for Calculation or Verification/state_fips.sas7bdat")
fips = rbind(cfips, sfips)

h = b[b$statecode <57,]
hsub = h[,c("statecode", "countycode","num","denom","rawvalue","sep", "cilow", "cihigh")]


hf = merge(fips, hsub, by = c("statecode", "countycode"), all.x = TRUE)


# compare w ganhua 

return(hf)
} 


measurenum = "v044"
acscolumns = c("B19080_001E", "B19080_001M", "B19080_004E", "B19080_004M")
apitype = "detailed" #other options include subject, profile, and comparison 
num = "B19080_004E"
denom = "B19080_001E"
year = 2025 
calctype = "ratio" #other options include percent 

hf = acsratios(measurenum, acscolumns, apitype, num, denom)

#keeping both old and new ct codes in the data!!! 
hf = hf %>% select(-c(state, county, fipscode, sep))

colnames(hf) = c("statecode", "countycode", paste0(measurenum, "_numerator"), 
                   paste0(measurenum, "_denominator"), 
                   paste0(measurenum, "_rawvalue"), 
                   paste0(measurenum, "_cilow"),
                   paste0(measurenum, "_cihigh"))




gan = haven::read_sas(paste0("P:/CH-Ranking/Data/",year,"/3 Data calculated needs checking/", measurenum, ".sas7bdat"))

summary(gan %>% select(paste0(measurenum, "_rawvalue")))
summary(as.numeric(hf$v044_rawvalue))

summary(gan %>% select(paste0(measurenum, "_cihigh")))
summary(hf$v044_cihigh)

summary(gan[,paste0(measurenum, "_cilow")])
summary(hf$v044_cilow)



write.csv(hf, paste0("P:/CH-Ranking/Data/", year, "/3 Data calculated needs checking/Hannah datasets/", measurenum, "h.csv"), row.names = FALSE)

write.csv(hf, file = paste0("duplicated_data/", measurenum, "_how.csv"), row.names = FALSE)

