##################################################################################
# function to pull ACS ratios 
#################################################################################

library(tidyverse)


measurenum = "v059"
acscolumns = c("B16005_001E", "B16005_001M", 
               "B16005_007E", "B16005_007M", "B16005_012E", "B16005_012M",
               "B16005_008E", "B16005_008M", "B16005_013E", "B16005_013M",
               "B16005_017E", "B16005_017M", "B16005_022E", "B16005_022M",
               "B16005_018E", "B16005_018M", "B16005_023E", "B16005_023M",
               "B16005_029E", "B16005_029M", "B16005_030E", "B16005_030M",
               "B16005_034E", "B16005_034M", "B16005_035E", "B16005_035M", 
               "B16005_039E", "B16005_039M", "B16005_040E", "B16005_040M", 
               "B16005_044E", "B16005_044M", "B16005_045E", "B16005_045M")
apitype = "detailed" #other options include subject, profile, and comparison 
denom = "B16005_001E"
year = 2025 
calctype = "percent" #other options include percent 


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

# Identify columns with names containing "M" and not matching "denom"
selected_columns <- grep("E", colnames(b), value = TRUE)
selected_columns <- setdiff(selected_columns, denom)  # Exclude the column matching denom

# Convert selected columns to numeric
b$num <- rowSums(as.data.frame(lapply(b[, selected_columns, drop = FALSE], as.numeric)), na.rm = TRUE)

# Assign the denom column
b$denom <- b[[denom]]



b$rawvalue = (b$num/b$denom)

# Identify columns with names containing "M" and not matching "denom"
selected_columns <- grep("M", colnames(b), value = TRUE)

# Remove entries that match `denom` except for the last character
pattern <- paste0("^", substr(denom, 1, nchar(denom) - 1), ".?$")
selected_columns <- selected_columns[!grepl(pattern, selected_columns)]


# Calculate b$senum as the square root of the sum of squares
b$senum <- sqrt(rowSums(as.data.frame(lapply(b[, selected_columns, drop = FALSE], function(x) (as.numeric(x)/1.645)^2)), na.rm = TRUE))
#b$senum = ifelse(b$senum >0, b$senum/1.645, NA) 


b$sedenom = ifelse(b[,gsub("E", "M",denom)] >0, b[,gsub("E", "M",denom)]/1.645, NA) 

b$sep = ifelse(b$rawvalue == 1, b$senum/b$denom, 
                  ifelse((b$senum^2 - (b$rawvalue^2 * b$sedenom^2)) <0, 
                         (1/b$denom)*sqrt(b$senum^2 + (b$rawvalue^2*b$sedenom^2)),
                         (1/b$denom)*sqrt(b$senum^2 - (b$rawvalue^2*b$sedenom^2))))

b$cihigh = ifelse(b$rawvalue + (1.96*b$sep)>=1, 1, b$rawvalue + (1.96*b$sep))



b$cilow = ifelse(b$rawvalue -(1.96*b$sep)<0, 0, b$rawvalue - (1.96*b$sep))
# b$cihigh = ifelse(calctype == "ratio", b$rawvalue +(1.96*b$sep), 
#                    ifelse(b$rawvalue + (1.96*b$sep)>=1, 1, b$rawvalue + (1.96*b$sep)))


cfips = haven::read_sas("P:/CH-Ranking/Data/2025/2 Cleaned data ready for Calculation or Verification/county_fips_with_ct_old.sas7bdat")
sfips = haven::read_sas("P:/CH-Ranking/Data/2025/2 Cleaned data ready for Calculation or Verification/state_fips.sas7bdat")
fips = rbind(cfips, sfips)

h = b[b$statecode <57,]
hsub = h[,c("statecode", "countycode","num","denom","rawvalue","sep", "cilow", "cihigh")]


hf = merge(fips, hsub, by = c("statecode", "countycode"), all.x = TRUE)




#keeping both old and new ct codes in the data!!! 
hf = hf %>% select(-c(state, county, fipscode, sep))

colnames(hf) = c("statecode", "countycode", paste0(measurenum, "_numerator"), 
                   paste0(measurenum, "_denominator"), 
                   paste0(measurenum, "_rawvalue"), 
                   paste0(measurenum, "_cilow"),
                   paste0(measurenum, "_cihigh"))




gan = haven::read_sas(paste0("P:/CH-Ranking/Data/",year,"/3 Data calculated needs checking/", measurenum, ".sas7bdat"))

summary(gan %>% select(paste0(measurenum, "_rawvalue")))
summary(as.numeric(hf$v059_rawvalue))

summary(gan %>% select(paste0(measurenum, "_cihigh")))
summary(as.numeric(hf$v059_cihigh))

summary(gan[,paste0(measurenum, "_cilow")])
summary(as.numeric(hf$v059_cilow))


#save to project 
write.csv(hf, file = "duplicated_data/v059_how.csv", row.names = FALSE)


#save to p drive 
write.csv(hf, paste0("P:/CH-Ranking/Data/", year, "/3 Data calculated needs checking/Hannah datasets/", measurenum, "h.csv"), row.names = FALSE)

