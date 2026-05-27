
/*****************************************************************************
 * v062 - Mental Health Providers
 * Author: JH
 * Description: Ratio of population to mental health providers.
 * Data Source: CMS, National Provider Identifier (NPI) Downloadable File
 * Data Download Link: https://download.cms.gov/nppes/NPI_Files.html
 * Numerator: The left side of the ratio represents the county population.
 * Denominator: The right side of the ratio represents the mental health providers corresponding to county population. Mental health providers are defined as psychiatrists, psychologists, licensed clinical social workers, counselors, marriage and family therapists, mental health providers that treat alcohol and other drug abuse, and advanced practice nurses specializing in mental health care.

 * v131 - Other Primary Care Providers
 * Author: JH
 * Description: Ratio of population to primary care providers other than physicians.
 * Data Source: CMS, National Provider Identifier (NPI) Downloadable File
 * Data Download Link: https://download.cms.gov/nppes/NPI_Files.html
 * Numerator: The left side of the ratio is the total county population.
 * Denominator: The right side of the ratio is the number of other primary care providers in a county. Other primary care providers include NPs and PAs.

 * NOTE: This analysis involves a lot of manual checking and correcting data entry errors AND geocoding in ArcGIS Pro. Geocoding could be 
   attempted in other software (like R). Results of this analysis will almost certainly differ from our dataset due to judgement calls we made
   when correcting data entry errors. We hope this code will help you understand the complexity and difficulty of working with the NPI NPPES 
   downloadable monthly replacement file. You are encouraged to modify or simplify this code to fit your needs. 
 *****************************************************************************/

*set mypath to be the root of your local cloned chrr_measure_calcs repository; 
%let mypath = \chrr_measure_calcs;
%let outpath = &mypath.\measure_datasets; 
libname out "&outpath."; 
libname inputs "&mypath.\inputs";

*check macro variable values to ensure data paths look correct;
%put &mypath;
%put &outpath;

/************************************************************************************
					Initial import of data and cleaning
/************************************************************************************/

/* import data */
/************************************************************************************
Options VALIDVARNAME=V7, forces spaces to underscore.
IF RUNNING THIS CODE IN SAS STUDIO: set VALIDVARNAME=V7 in SAS Studio under 
Preferences>Tables>Policies>SAS variable name policy - change it to V7.

2024: Total rows: 7847427 Total columns: 330
2025: Total rows: 8376716 Total columns: 330
2026: Total rows: 8960259 Total columns: 330

Unzip the NPI NPPES_Data_Dissemination_June_2025 data or other NPI NNPES montly 
download file on your local machine in the chrr_measure_cals>raw_data>NPI folder 
first. The below .csv file is the one from the June 2025 Dissemination. 
/************************************************************************************/
proc import 
datafile = "&mypath.\raw_data\NPI\npidata_pfile_20050523-20250608.csv"
out=npidata_raw 
dbms=csv replace; 
getnames=yes; 
guessingrows = 5000; 
run;

/* get a list of column names */
/************************************************************************************
To see all column names, see the npidata_pfile_20050523-20250608_fileheader csv file.
To see a full list of all the columns, see the NPPES_Data_Dissemination_Readme. 

var32: provider_business_practice_location_address_state_name
var33: provider_business_practice_location_address_postal_code
var34: provider_business_practice_location_address_country_code_if_outside_u_s 
/************************************************************************************/
proc sql;
	create table dict_col as
	select name
	from dictionary.columns
	where libname = 'WORK' 
	and memname = UPCASE('npidata_raw')  
	;
quit;

/* retaining provider NPI, entity type code, name, business practice location information, 
and primary taxonomy code - Healthcare_Provider_Taxonomy_Co */
/************************************************************************************
2024: Total rows: 7847427 Total columns: 13
2025: Total rows: 8376716 Total columns: 13
2026: Total rows: 8960259 Total columns: 13 
/************************************************************************************/
data npidata_raw_2;
	set npidata_raw (keep = NPI entity_type_code 
	Provider_Last_Name__Legal_Name_
	Provider_First_name Provider_Middle_Name Provider_Other_Last_Name
	Provider_First_Line_Business_Pr Provider_Second_Line_Business_P 
	Provider_Business_Practice_Loca	VAR32 VAR33 VAR34
	Healthcare_Provider_Taxonomy_Co);
	rename Provider_First_Line_Business_Pr = first_address_line;
	rename Provider_Second_Line_Business_P = second_address_line;
	rename Provider_Business_Practice_Loca = city_name;
	rename VAR32 = state_name;
	rename VAR33 = postal_code;
	rename VAR34 = country_code;
	rename Healthcare_Provider_Taxonomy_Co = Taxonomy_Code;
run;

/* entity_type_code 1 is individuals, entity_type_code 2 is organizations - remove 
all entities with a type code of 2 to only retain individual providers. Create new 
column with 5 digit zip code  */
/************************************************************************************
2026: Total rows: 6827527 Total columns: 14
/************************************************************************************/
data npidata_individuals;
	set npidata_raw_2;
	where entity_type_code = '1';
	length zip_5 $ 5;
	zip_5 = substr(postal_code, 1, 5);
run;

/* OPTIONAL: Save npi_individuals SAS dataset to your files */
data out.npidata_individuals;
	set npidata_individuals;
run;

/************************************************************************************
					v062 - Mental Health Providers - data cleaning
/************************************************************************************/

/* retaining only the taxonomy codes we include in mental health providers measure */
/************************************************************************************
This website can be used to search all taxonomy codes: 
https://taxonomy.nucc.org/?searchTerm=&x=6&y=12

CHR&R has used the same list of taxonomy codes for mental health providers for several 
years. In July 2026, all taxonomy codes were verified to have not changed.

2024: Total rows: 1052782 Total columns: 14
2025: Total rows: 1120718 Total columns: 15
2026: Total rows: 1192507 Total columns: 16
/************************************************************************************/
*run if SAS was closed between initial data cleaning step and this step and you saved
the npi_individuals SAS dataset to your files;
data npidata_individuals;
	set out.npidata_individuals;
run;

data npidata_mentalhealth;
set npidata_individuals;
length type $15;
if Taxonomy_Code = '2084P0800X' then do; code = 1;type = 'psychiatry'; end;*psychiatry; 
if Taxonomy_Code = '103T00000X' then do; code = 1;type = 'psychologist'; end;*psychologist;
if Taxonomy_Code = '103TC2200X' then do; code = 1;type = 'psychologist'; end;*clinical child & adolescent psychologist;
if Taxonomy_Code = '103TB0200X' then do; code = 1;type = 'psychologist'; end;*cognitive & behavioral psychologist;
if Taxonomy_Code = '103TC1900X' then do; code = 1;type = 'psychologist'; end;*counseling psychologist;
if Taxonomy_Code = '103TF0000X' then do; code = 1;type = 'psychologist'; end;*family psychologist;
if Taxonomy_Code = '103TS0200X' then do; code = 1;type = 'psychologist'; end;*school psychologist;
if Taxonomy_Code = '103TC0700X' then do; code = 1;type = 'psychologist'; end;*psychologist,clinical;
if Taxonomy_Code = '1041C0700X' then do; code = 1;type = 'social worker'; end;*licensed clinical social worker;
if Taxonomy_Code = '363LP0808X' then do; code = 1;type = 'np-psychiatry'; end;*nurse practitioner - psychiatric/mental health;
if Taxonomy_Code = '2084P0804X' then do; code = 1;type = 'psychiatry'; end;*child & adolescent psychiatry physician;
if Taxonomy_Code = '2084P0805X' then do; code = 1;type = 'psychiatry'; end;*geriatric psychiatry physician;
if Taxonomy_Code = '364SP0808X' then do; code = 1;type = 'np-psychiatry'; end;*Psychiatric/Mental Health Clinical Nurse Specialist;
if Taxonomy_Code = '364SP0809X' then do; code = 1;type = 'np-psychiatry'; end;*Psychiatric/Mental Health, Adult, Clinical Nurse Specialist;
if Taxonomy_Code = '364SP0807X' then do; code = 1;type = 'np-psychiatry'; end;*Psychiatric/Mental Health, Child & Adolescent, Clinical Nurse Specialist;
if Taxonomy_Code = '364SP0810X' then do; code = 1;type = 'np-psychiatry'; end;*Psychiatric/Mental Health, Child & Family, Clinical Nurse Specialist;
if Taxonomy_Code = '364SP0811X' then do; code = 1;type = 'np-psychiatry'; end;*Psychiatric/Mental Health, Chronically Ill, Clinical Nurse Specialist;
if Taxonomy_Code = '364SP0812X' then do; code = 1;type = 'np-psychiatry'; end;*Psychiatric/Mental Health, Community, Clinical Nurse Specialist;
if Taxonomy_Code = '364SP0813X' then do; code = 1;type = 'np-psychiatry'; end;*Psychiatric/Mental Health, Geropsychiatric, Clinical Nurse Specialist;
if Taxonomy_Code = '101Y00000X' then do; code = 1;type = 'counselor'; end;*Counselor;
if Taxonomy_Code = '101YM0800X' then do; code = 1;type = 'counselor'; end;*Mental Health Counselor;
if Taxonomy_Code = '101YA0400X' then do; code = 1;type = 'counselor'; end;*counselor, addiction (substance abuse disorder);
if Taxonomy_Code = '101YP2500X' then do; code = 1;type = 'counselor'; end;*counselor, professional;
if Taxonomy_Code = '103TA0400X' then do; code = 1;type = 'psychologist'; end;*psychologist, addiction (substance abuse disorder);
if Taxonomy_Code = '2084A0401X' then do; code = 1;type = 'psychiatry'; end;*psychiatry, addiction medicine physician (DO);
if Taxonomy_Code = '2084P0802X' then do; code = 1;type = 'psychiatry'; end;*psychiatry, addiction psychiatry physician;
if Taxonomy_Code = '106H00000X' then do; code = 1;type = 'marriage'; end;*marriage and family therapist;
if code = 1;
run;

proc freq data = npidata_mentalhealth;
	tables type;
run;

/* examine provider zip codes and addresses to look for inconsistencies */
/************************************************************************************
Providers have to apply for their NPI - there are likely data errors from mistakes 
providers made when filling out the forms, or when information from the forms is 
compiled into the full NPI dataset. Additionally, the USPS changes zip codes with 
some frequency, and zip codes cross county and state boundaries. 
/************************************************************************************/

proc freq data = npidata_mentalhealth;
	tables zip_5;
run;
*there are some 00000 zip codes, and zip codes that are a combo of numbers and letters;

proc freq data = npidata_mentalhealth;
	tables state_name;
run;
*There are 3 providers with numbers instead of a state abbreviation - accidentally 
entered a zip code instead of a state? There are many values in the state_name column 
that don't match a US state abbreviation (see links below). It looks like some US city 
names may have also been accidentally entered into the state abbreviation field. Some 
state names are spelled out instead of abbreviated.;

proc freq data = npidata_mentalhealth;
	tables country_code;
run;
*1,191,911 providers have a US country code - but since it seems there are data 
entry errors, not restricting based on country code yet.;

/************************************************************************************
See this webpage for valid state abbreviations in the file: 
https://npiregistry.cms.hhs.gov/help-api/state

AE, AP, AA "state name" abbreviations are for overseas military addresses.
APO or FPO "city" designations are also required for overseas military addresses.  
https://pe.usps.com/text/pub28/28c2_010.htm#:~:text=AE%20is%20used%20for%20armed,is%20the%20Americas%20excluding%20Canada.
https://faq.usps.com/s/article/How-Do-I-Address-Military-Mail

Overseas military providers will be excluded from the analysis where possible but 
military providers in the contiguous US states will be included. 

See this webpage for valid country abbreviations in the file: 
https://npiregistry.cms.hhs.gov/help-api/country

All providers outside the US will be excluded from the analysis. 
/************************************************************************************/

/* Bringing in the SAS zipcode dataset */
/************************************************************************************
Download newset SAS zipcode files here: http://support.sas.com/rnd/datavisualization/mapsonline/html/misc.html
A readme file in the zipped downloaded file explains how to set up the zip code file. Find the zipped May25 file
and the zipcode_may2025 SAS dataset in the chrr_measure_cals>inputs folder. 

2025: Total rows: 40996 Total columns: 7
2026: Total rows: 40927 Total columns: 10
/************************************************************************************/
data zip_raw;
	set inputs.zipcode_may2025;
run;

data zip_raw_2;
	set zip_raw;
	zip_5_z = put(zip, z5.);
	*adding a _z after column names to indicate where fields came from;
	rename statecode = state_name_z;
	rename statename = state_name_full_z;
	rename countynm = county_name_z;
	countycode_z = put(county, z3.);
	statecode_z = put(state, z2.);
run;

data zip;
	set zip_raw_2 (drop = zip state county ZIP_CLASS CITY MSA AREACODE AREACODES
	TIMEZONE GMTOFFSET DST PONAME ALIAS_CITY ALIAS_CITYN STATENAME2);
	fipscode_z = statecode_z || countycode_z;
run;

proc datasets lib = work;
	modify zip;
	attrib _all_ label = ' ';
run;
quit;

*Process note - running the SAS zip code geocoding (assigning provider to a county based on zip 
code) before deleting any providers based off state abbreviation or country code. Also going
to test geocoding by joining just on zip codes and then on zip codes and state abbreviations
or name;

/* Merging mental health data with ZIP CODES to assign counties */
/************************************************************************************
Joining just on zip codes. 

2025 Total rows: 1120718 Total columns: 16
2026 Total rows: 1192507 Total columns: 16 
/************************************************************************************/
proc sql;
	create table npi_mentalhealth_zipjoin as
	select x.*, y.*
	from npidata_mentalhealth as x
	left join zip as y
	on x.zip_5 = y.zip_5_z;
quit;

/* Looking for weird observations or providers that did not have a zip code join */
proc sort data = npi_mentalhealth_zipjoin; by state_name; run;
*There's a provider in NY that looks like they have a zip code entered
in the state abbreviation line on accident. There are two providers in AK at 4020 Folker
St. in Ancorage that have a zipcode error - it's listed as 97508 when it should be 99508 - 
prevented the join with the SAS zip code dataset. There's a provider in Wyoming at 1800 
Edinburg St, Rawlins, that incorrectly entered UM instead of US for country code.;

proc sort data = npi_mentalhealth_zipjoin; by state_name zip_5_z; run;
*There are 2 providers in AK - one that had no zip code match and one that had an incorrect
zip code match (matched to a zip code in Oregon) that have the address 555 Zeamer Ave which 
is a military base.;

/* Checking all providers that didn't have a zip code join */
/************************************************************************************
2025: Total rows: 1339 Total columns: 16
2026: Total rows: 1613 Total columns: 26
/************************************************************************************/
data mh_zipjoin_notassignedcounty; 
	set npi_mentalhealth_zipjoin;
	where county_name_z = '';
run;

proc sort data = mh_zipjoin_notassignedcounty; by zip_5; run;

proc sort data = mh_zipjoin_notassignedcounty; by state_name; run;

proc freq data = mh_zipjoin_notassignedcounty;
	tables state_name;
run;
*Most of the providers with zip codes that didn't join to the SAS zip code 
file have a state code of AE or AP - overseas military providers. Providers 
with valid state codes that didn't join:
125 in AZ, 
110 in DC,
34 in CA,
10 in FL, 
12 in NC, 
32 in NY, 
70 in OH, 
13 in WA, etc.;

proc sort data = mh_zipjoin_notassignedcounty; by country_code; run;
*Looks like the vast majority of providers who don't have a US country code don't 
have a state code for any of the states in the US - probably safe to drop all 
providers that don't have a US country code??;

/* Checking the providers that had a zip code join, but where the state in the NPI 
dataset and the state in the SAS zip code dataset don't match - incorrect geocode */
data mh_zipjoin_incorrect;
	set npi_mentalhealth_zipjoin;
	where state_name ne state_name_z;
run;
*2064 providers - pulled in providers that didn't have a zip join at all as well;

proc sort data = mh_zipjoin_incorrect; by country_code state_name; run;
*There are a lot of providers with a country code of UM that look like they have US 
addresses - many with a full state name spelled out in the state_name column instead 
of a state abbreviation. DON'T drop all non US country code providers yet.;

/* Merging mental health data with STATE NAME AND ZIP CODE to assign counties */
/************************************************************************************
Joining on zip codes and state_name fields (state abbreviations). 

2026 Total rows: 1192507 Total columns: 26 
/************************************************************************************/
proc sql;
	create table npi_mentalhealth_zipstjoin as
	select x.*, y.*
	from npidata_mentalhealth as x
	left join zip as y
	on x.zip_5 = y.zip_5_z and
	x.state_name = y.state_name_z;
quit;

/* Checking the providers that didn't have a zip code and state join */
/************************************************************************************
2026: Total rows: 2064 Total columns: 26
/************************************************************************************/
data mh_zipstjoin_notassignedcounty; 
	set npi_mentalhealth_zipstjoin;
	where county_name_z = '';
run;
*number of providers without a join went up - this makes sense if there were incorrect 
joins happening in the first join based just on zip codes;

proc sort data = mh_zipstjoin_notassignedcounty; by zip_5; run;

proc sort data = mh_zipstjoin_notassignedcounty; by state_name; run;

proc freq data = mh_zipstjoin_notassignedcounty;
	tables state_name;
run;
*Most of the providers with zip codes and state abbreviations that didn't join
have a state code of AA, AE or AP - overseas military providers. Providers with
valid state codes that didn't join:
131 in AZ, 
55 in CA, 
17 in CO, 
12 in CT, 
118 in DC, 
13 in FL, 
42 in MA, 
16 in NC, 
14 in NJ, 
43 in NY, 
75 in OH, 
11 in OK, 
18 in WA, etc.; 

proc freq data = mh_zipstjoin_notassignedcounty;
	tables country_code;
run;
*There are a good number of providers with country code DE and UM - taking a look at 
ALL the providers with a country code DE below;

data mh_zipstjoin_DE;
	set npi_mentalhealth_zipstjoin;
	where country_code = "DE";
	proc sort;
		by state_name;
run;
*156 providers - majority have city name APO and state name AE, some have state names that 
sound like locations in Germany. Not seeing indications of data entry error.;

*providers with country code UM;
data mh_zipstjoin_UM;
	set npi_mentalhealth_zipstjoin;
	where country_code = "UM";
	proc sort;
		by state_name;
run;
*128 providers - some have full US state names in the state_name (abbreviation)
field. Looks like data entry errors. Many have a state name PR or Puerto Rico
- dropping those;

data mh_zipstjoin_UM_2;
	set npi_mentalhealth_zipstjoin;
	where country_code = "UM";
	if state_name = "PR" then delete;
	if state_name = "PUERTO RICO" then delete;
	proc sort;
		by state_name;
run;
*67 providers;

*Process note - maybe preserve all providers with a country code of US or UM, then delete 
providers that have a state_name of AP, AA, AE, or PR, PUERTO RICO? From there, make the 
updates to AZ and OH zipcodes that we know of and then try to geocode the providers that 
still didn't join based on zip code and state_name in ArcGIS Pro.;

/* Remove providers who are outside of the US or overseas military providers */
/************************************************************************************
Starting from the dataset where mental health providers were joined to zip codes AND 
state abbreviations. Retaining UM providers for now because there seem to be data 
entry errors, but dropping providers with a state_name PR, P.R., PUERTO RICO, 
AMERICAN SAMO, and overseas military providers (indicated by state_name AA, AE, or AP). 
/************************************************************************************/

data npi_mentalhealth_2;
	set npi_mentalhealth_zipstjoin;
	where country_code = "US" OR country_code = "UM";
run;
*1192039 providers;

proc freq data = npi_mentalhealth_2;
	tables state_name;
run;
*Will also drop providers in VI - after a quick visual scan it looks like all the providers 
with a state name VI have a city name in the Virgin Islands (ie no entry errors);

data npi_mentalhealth_3;
	set npi_mentalhealth_2;
	if state_name = "AA" then delete;
	if state_name = "AE" then delete;
	if state_name = "AP" then delete;
	if state_name = "PR" then delete;
	if state_name = "PUERTO RICO" then delete;
	if state_name = "AMERICAN SAMO" then delete;
	if state_name = "P.R." then delete;
	if state_name = "VI" then delete;
run;
*1185028 providers;

proc freq data = npi_mentalhealth_3;
	tables state_name;
run;
*Still some state names that are spelled out instead of abbreviated and some other potentially 
incorrect state names.;

/* Assigning a code to all providers with valid state_names (abbreviations) */

%let st_ls = 'AK', 'AL', 'AR', 'AZ', 'CA', 'CO', 'CT', 
'DC', 'DE', 'FL', 'GA', 'HI', 'IA', 'ID', 'IL', 'IN', 'KS', 'KY', 'LA', 
'MA', 'MD', 'ME', 'MI', 'MN', 'MO', 'MS', 'MT', 'NC', 'ND', 'NE', 'NH', 
'NJ', 'NM', 'NV', 'NY', 'OH', 'OK', 'OR', 'PA', 'RI', 'SC', 'SD', 'TN', 
'TX', 'UT', 'VA', 'VT', 'WA', 'WI', 'WV', 'WY';
%put &=st_ls;

data npi_mentalhealth_4;
	set npi_mentalhealth_3;
	if state_name in (&st_ls) then valid_state_name = 1;
run;

proc means data = npi_mentalhealth_4 sum;
	var valid_state_name;
run;
*1184855 out of 1185028 providers has a valid state name (abbreviation);

/* Creating a dataset of the mental health providers with an incorrect state_name or 
that didn't join to the SAS zip code file */

data npi_mentalhealth_tofix;
	set npi_mentalhealth_4;
	where valid_state_name ne 1 OR county_name_z = '';
run;
*960 providers;

proc freq data = npi_mentalhealth_tofix;
	tables state_name;
run;
*Based on visual inspection of the tofix dataset and proq freq results, there 
are providers in Guam (GU) and the Marianas Islands (MP) to drop; 

proc sort data = npi_mentalhealth_tofix; by state_name; run;

/************************************************************************************
Manual checking and fixes to make. Looked up addresses and city names quickly:
- Drop providers with a state name of GU, GUAM, or MP
- Change the New York provider with a state name of 10025 to NY
- Change state name ARIZONA to AZ
- Where city name is Tucson and state name is AS, change state name to AZ
- Drop providers with a state name of AS (American Samoa)
- Change zip code for providers at 4020 FOLKER ST in Ancorage, AK from 97508 to 99508
- Change state names CA-CALIFORN, CALIFORNIA to CA
- Drop provider with state name CAGUAS (has a PR address)
- Change state name CLARK COUNTY to NV (address is a valid NV address and Clark County is in NV)
- Change state name COLORADO to CO
- Change provider in Sebring with state name FM to FL
- Change provider in Plymouth with state name FM to MN
- Change state name GEORGIA to GA
- Drop all providers with city names of APO, FPO, or APO AE?
- Change state name HAWAII to HI
- Change state name ILLINOIS and ILLINOIX to IL
- Change state name LOUISIANA to LA
- Change state name MAINE to ME
- Change state name MASSACHUSETTS to MA
- Change state name MD-MARYLAND to MD
- Change state name MICHIGAN to MI
- Change state name MISSOURI to MO
- Change state name MONTANA to MT
- Change state name NA to TX (the only provider with this state name has a valid TX address and zip code)
- Change state name NEBRASKA to NE
- Change state name NEW HAMPSHIRE to NH
- Change state name NEW HANOVER C to NC (provider has a valid NC address and zip code)
- Change state name NEW JERSEY to NJ
- Change state name NEW MEXICO to NM
- Change state name NEW YORK to NY
- Change state name NEZ PERCE to ID (provider has a valid ID address and zip code)
- Change state name OKLAHOMA to OK
- Drop providers with state name P.R.
- Change state name TENNESSEE to TN
- Change state name of provider in Brockton from TERRITORY to MA
- Change state name of provider in COLORADO SPRINGS from TERRITORY to CO
- Change state name UNITED STATES to CA (provider has a valid CA address and zip code)
- Change state name US to VA (provider has a valid VA address and zip code)
- Change state name of provider in EAST LONGMEADOW from USA to MA (provider has a valid MA address and zip code)
- Change state name of provider in HINSDALE from USA to IL (provider has a valid IL address and zip code)
- Change state name UTAH to UT
- Change state name WISCONSIN to WI
- Change state name WYOMING to WY

From previous years of this code being run, zip codes in AZ and OH are updated to reflect 
changes we're aware of. Washington, D.C. zip code of 20307 belongs to Walter Reed Medical 
Center which closed in 2011 and moved to Bethesda. Delete any provider that still uses 
this zipcode. 

Drop all providers with city names of APO, FPO, or APO AE as well??
/************************************************************************************/
data npi_mentalhealth_5;
	set npi_mentalhealth_4;
	if state_name = "GU" then delete;
	if state_name = "GUAM" then delete;
	if state_name = "MP" then delete;
 	if city_name = "NEW YORK" and state_name = "10025" then state_name = "NY";
	if state_name = "ARIZONA" then state_name = "AZ";
	if city_name = "Tucson" and state_name = "AS" then state_name = "AZ";
	if state_name = "AS" then delete;
	if first_address_line = "4020 FOLKER ST" and state_name = "AK" then zip_5 = "99508";
	if state_name = "CA - CALIFORN" then state_name = "CA";
	if state_name = "CALIFORNIA" then state_name = "CA";
	if state_name = "CAGUAS" then delete;
	if state_name = "CLARK COUNTY" and city_name = "HENDERSON" then state_name = "NV";
	if state_name = "COLORADO" then state_name = "CO";
	if city_name = "SEBRING" and state_name = "FM" then state_name = "FL";
	if city_name = "PLYMOUTH" and state_name = "FM" then state_name = "MN";
	if state_name = "GEORGIA" then state_name = "GA";	
	if state_name = "HAWAII" then state_name = "HI";
	if state_name = "ILLINOIS" then state_name = "IL";
	if state_name = "ILLINOIX" then state_name = "IL";
	if state_name = "LOUISIANA" then state_name = "LA";
	if state_name = "MAINE" then state_name = "ME";
	if state_name = "MASSACHUSETTS" then state_name = "MA";
	if state_name = "MD-MARYLAND" then state_name = "MD";
	if state_name = "MICHIGAN" then state_name = "MI";
	if state_name = "MISSOURI" then state_name = "MO";
	if state_name = "MONTANA" then state_name = "MT";
	if state_name = "NA" and city_name = "CYPRESS" then state_name = "TX";
	if state_name = "NEBRASKA" then state_name = "NE";
	if state_name = "NEW HAMPSHIRE" then state_name = "NH";
	if state_name = "NEW HANOVER C" then state_name = "NC";
	if state_name = "NEW JERSEY" then state_name = "NJ";
	if state_name = "NEW MEXICO" then state_name = "NM";
	if state_name = "NEW YORK" then state_name = "NY";
	if state_name = "NEZ PERCE" then state_name = "ID";
	if state_name = "OKLAHOMA" then state_name = "OK";
	if state_name = "TENNESSEE" then state_name = "TN";
	if state_name = "TERRITORY" and city_name = "BROCKTON" then state_name = "MA";
	if state_name = "TERRITORY" and city_name = "COLORADO SPRINGS" then state_name = "CO";
	if state_name = "UNITED STATES" and city_name = "SAN FERNANDO" then state_name = "CA";
	if state_name = "US" and city_name = "ALEXANDRIA" then state_name = "VA";
	if state_name = "USA" and city_name = "EAST LONGMEADOW" then state_name = "MA";
	if state_name = "USA" and city_name = "HINSDALE" then state_name = "IL";
	if state_name = "UTAH" then state_name = "UT";
	if state_name = "WISCONSIN" then state_name = "WI";
	if state_name = "WYOMING" then state_name = "WY";
run;
*1184906 providers;

proc freq data = npi_mentalhealth_5;
	tables state_name;
run;
*almost fixed - still a provider with a GERMANY state_name but that will be dropped 
when providers with a city_name of APO, FPO and APO_AE are dropped;

/************************************************************************************
*update Arizona zip codes, May 2009 changes http://about.usps.com/postal-bulletin/2009/pb22259/html/info_001.htm;
*update Ohio zip codes, July 2006 changes https://about.usps.com/postal-bulletin/2006/html/pb22184/pb8p-s_002.html,
and August 2009 changes https://about.usps.com/postal-bulletin/2009/pb22265/html/info1_001.htm
/************************************************************************************/
data npi_mentalhealth_6;
	set npi_mentalhealth_5;
	*Arizona;
	if zip_5 = '85222' then zip_5 = '85122';
	if zip_5 = '85220' then zip_5 = '85120';
	if zip_5 = '85242' then zip_5 = '85142';
	if zip_5 = '85273' then zip_5 = '85173';
	if zip_5 = '85247' then zip_5 = '85147';
	if zip_5 = '85239' then zip_5 = '85139';
	if zip_5 = '85237' then zip_5 = '85137';
	if zip_5 = '85223' then zip_5 = '85123';
	if zip_5 = '85231' then zip_5 = '85131';
	if zip_5 = '85238' then zip_5 = '85138';
	if zip_5 = '85240' then zip_5 = '85140';
	if zip_5 = '85243' then zip_5 = '85143';
	if zip_5 = '85232' then zip_5 = '85132';
	if zip_5 = '85228' then zip_5 = '85128';
	if zip_5 = '85219' then zip_5 = '85119';
	if zip_5 = '85218' then zip_5 = '85118';
	*Ohio;
	if zip_5 = '43624' then zip_5 = '43604';
	if zip_5 in ('45408' '45418' '45427') then zip_5 = '45417';
	*Drop Walter Reed Medical Center providers with old zip code;
	if zip_5 = "20307" then delete;
run;
*1184797 providers;

proc sort data = npi_mentalhealth_6;	
	by city_name;
run;

data npi_mentalhealth_OSmilitary;
	set npi_mentalhealth_6;
	where city_name like 'APO%' or city_name like 'FPO%'
	or city_name like '% AA %' or city_name like '% AE %'
	or city_name like '% AP %';
run;
*135 providers;

proc freq data = npi_mentalhealth_OSmilitary;
	tables city_name;
run;
*assuming that providers with city names of 'APO', 'APO AE', 'APO, AP', and 
'FPO' are also overseas military providers - looked up some addresses and this seems to be true;

/* dropping more overseas military providers */

data npi_mentalhealth_7;
	set npi_mentalhealth_6;
	if city_name in ('APO', 'APO AE', 'APO, AP', 'FPO') then delete;
run;
*1184779 providers;

proc sort data = npi_mentalhealth_7;	
	by city_name;
run;

/* Re-do SAS zip code geocoding to assign counties, joining on STATE NAME AND ZIP CODE */
/************************************************************************************
Joining on zip codes and state_name fields (state abbreviations).
Dropping all the zip columns from npi_mentalhealth_7 before re-doing the join.
/************************************************************************************/
data npi_mentalhealth_8;
	set npi_mentalhealth_7;
	drop X Y state_name_z state_name_full_z county_name_z city2 zip_5_z countycode_z
	statecode_z fipscode_z;
run;

proc sql;
	create table npi_mentalhealth_zipstjoin2 as
	select x.*, y.*
	from npi_mentalhealth_8 as x
	left join zip as y
	on x.zip_5 = y.zip_5_z and
	x.state_name = y.state_name_z;
quit;

/* Checking the providers that didn't have a zip code and state join */
/************************************************************************************
Total rows before additional data cleaning above: 2064 Total columns: 26
Total rows now: 486 Total columns: 27
/************************************************************************************/
data mh_zipstjoin2_notassignedcounty; 
	set npi_mentalhealth_zipstjoin2;
	where county_name_z = '';
run;

proc sort data = mh_zipstjoin2_notassignedcounty;
	by state_name;
run;

proc freq data = mh_zipstjoin2_notassignedcounty;
	tables state_name;
run;
*there are still a good number of providers in each state with zip codes that didn't match: 
53 in CA
42 in MA 
16 in NC
33 in NY;

/* Saving a dataset of the providers who didn't have a zip code match for geocoding in ArcGIS 
Pro and then dropping those providers from the larger dataset. Will add those providers back in 
after geocoding outside of SAS */

data sasuser.mh_zipstjoin2_notassignedcounty;
	set mh_zipstjoin2_notassignedcounty;
run;

data npi_mentalhealth_9;
	set npi_mentalhealth_zipstjoin2;
	if county_name_z = '' then delete;
run;
*1184293 providers;

/* OUTSIDE OF SAS - Geocode addresses in Pro with ArcGIS World Geocoding Service */
/************************************************************************************
Finding point locations of providers based on the first and second address lines, 
city name and state name. Not using zip code for geocoding since some zip codes might 
now be inaccurate. The categories for geocoding results were Point Address, Street 
Address, Primary Postal, or Postal Locality. After first round of geocoding there were
57 unmatched addresses and 6 ties. Used the Rematch Addresses tool to manually go through
the unmatched addresses and make edits to the data to find the correct match (point 
location) for the provider (based on Google search results and mathcing zip codes - which 
were not included in this first round of geocoding). A list of edits made are below: 

Some providers had addresses I couldn't figure out - will do one more round of geocoding with 
zipcodes included and will try to match to POIs or city names.  
- changed first address line for a provider in Fort Rucker, AL to 3RD AVE (info was in second address line)- still didn't match
- changed first address line for a provider at HIGHWAY 160 M.P. 394.3 to 320 PACIFIC PL - still didn't match
- changed state name from AZ to WA for a provider at 320 PACIFIC PL in Mount Vernon
- deleted a provider with address CMR 431 in DARMSTADT - this is a military installation in Germany 
- changed state name from CA to NJ for a provider at 36 DRAKES CORNER RD in Princeton
- changed state name from CA to FL for a provider at 3094 S BLACKMOUNTAIN DR in Invereness 
- changed first address line for a provider in Montrose, CO from 605 E MIAMI RD to 605 MIAMI RD and changed state name from CA to CO (I googled the address and found the likely correct one based on the zip code provided in the NPI file)
- changed state name from CA to UT for a provider at 439 E 900 S in Salt Lake City (googled provider's name - her zip code is wrong in the NPI data as well)
- deleted a provider with address APARTADO DE CORREOS 33 in ROTA - this is a military installation in Spain
- changed first address line for a provider in Middletown CT from DUTTON HOME SILVER ST to 351 Silver Street Dutton Home Building
- deleted a provider with address WIESBADEN ARMY HEALTH CLINIC in WIESBADEN - this is a military installation in Germany
- changed first address line for a provider in Fort Bragg from WOMACK ARMY MEDICAL CENTER to 2817 Rock Merritt Ave
- changed first address line for a provider in Schofield Barracks from DESMOND DOSS HEALTH CLINIC to 683 Waianae Ave. and changed city name from SCHOFIELD BARRACKS to Wahiawa - still didn't match
- changed state name from IA to IN for a provider at 8200 GEORGIA ST. in Merrillville
- changed first address line for a provider in Iowa City from HIGHWAY 6 WEST to 601 HIGHWAY 6 WEST
- changed state name from IL to IN for a provider at 6655 E. US 36 in Avon
- changed state name from IL to IN for a provider at 3111 W JACKSON ST in Muncie
- changed state name from IL to NM for a provider at 501 AIRPORT DR. in Farmington
- changed state name from IN to IL for a provider at 521 S. LAGRANGE in La Grange
- changed state name from KS to KY for a provider at 934 SOUTH LAUREL ROAD in London
- changed state name from KS to MO for a provider at 4106 CENTRAL in Kansas City
- changed state name from KY to OH for a provider at 5400 EDALBERT DR. in Cincinnati
- changed state name from LA to IN for a provider at 1501 STATE ST in New Albany 
- changed state name from MA to MD for a provider at 540 RIVERSIDE DR in Salisbury
- changed state name from MA to MI for a provider at 5031 PARK LAKE ROAD in East Lansing
- changed state name from MD to MA for a provider at 41 MASON STREET in Salem 
- changed zip code for provider at 402 GOODRICH AVE in Kittery, ME from 03940 to 03904 - still didn't match
- changed state name from MI to MA for a provider at 965 CHURCH STREET in New Bedford
- changed state name from MI to MO for a provider at 225 SOUTH MERAMEC in Clayton/St. Louis
- changed state name from MN to NM for a provider at 1001D W. BROADWAY in Farmington
- changed first address line for a provider in Pineville, MO from 5265 S BUSINESS 71 to 5265 S BUSINESS HWY 71 - still didn't match 
- changed state name from NC to NY for a provider at 134-42 227 STREET in Laurelton
- changed state name from NE to NJ for a provider at BRANDYWINE WATCHUNG 680 MOUNTAIN ROD. in Watchung
- changed state name from NJ to PA for provider at 17 BARCLAY STREET in Newtown
- changed first address line for a provider in McDermitt, NV from FORT MCDERMITT WELLNESS CENTER to 112 N Reservation Rd, McDermitt and changed zip code from 98421 to 89421 - still didn't match 
- changed state name from NY to CT for a provider at 74 EAST STREET in PLainville
- deleted a provider with address GRAFENWOEHR ARMY HEALTH CLINIC - this is a military installation in Germany 
- changed state name from OH to NC for provider at 8928 HWY 70 W BUSINESS SUITE 100 in Clayton
- moved second line address to first line address for provider at 2211 CHARLOTTE STREET, # 314 in Kansas City and changed state name from OH to MO
- deleted a provider with address CARR 2 KM 92 in Camuy - looks like a Puerto Rico address
- changed first address line for a provider in Killeen, TX from 1711 CENTEX EXPRESSWAY to 1711 E Central Texas Expy
- changed state name from VA to MA for a provider at 15 PRISCILLA RD in Boston
- deleted a provider with address 227 GOLDEN ROCK, OFFICE 1 in Christiansted - this is an address in the Virgin Islands 
- changed state name from WA tp WY for a provider at 1603 COMMERCE ST in Cheyenne - still didn't match 
- changed state name from WI to IL for a provider at 4160 ROUTE 83 in Long Grove 
- changed state name from WI to MN for a provider at 44 GOOD COUNSEL DR in Lakeville - still didn't match 

Edits for providers that tied to matching with multiple locations 
or that had a match confidence of less than 84%: 
- changed state name from CA to NC for a provider at 211 E. SIX FORKS RD in Raleigh
- changed state name from OH to IL for a provider at 358 GLENWOOD DR APT 201 in Bloomingdale
- changed first address line for a provider in Fort Bragg, NC, WOMACK ARMY MEDICAL CENTER from 2817 REILLY ROAD to 2817 Rock Merritt Ave
- changed state name from MD to VA for provider at 2501 NORTH GLEBE ROAD #303 in Arlington
- deleted provider at BO PUNTAS CARR 413 KM 4 8 in Rincon - this is a Puerto Rico address
- deleted provider at VISTAS DE RIO GRANDE II ALMACIGO ST. 540 in Rio Grande - this is a Puerto Rico address 
- changed state name from NE to NY for a provider at VISTAS DE RIO GRANDE II ALMACIGO ST. 540 in New York
- changed state name from LA to OH for a provider at 151 MARION in Mansfield

Also deleted providers at CONDOMINIO PASEO ESMERALDA in Fajarado and VA CARIBBEAN HEALTHCARE SYSTEM
in San Juan because they're in Puerto Rico. 

Ran the geocoding again based on the first and second address lines, city name, state name AND 
zip code. The categories for geocoding results were Point Address, Street Address, Primary Postal,
or Postal Locality. After first round of geocoding there were 13 unmatched addresses and 8 ties. 
Reviewed unmatches and ties again with Rematch Addresses tool. Edits made (some zip codes were invalid
and I googled what zip codes were associated with city names to make edits): 
- changed zip code for a provider at 394.3 US-160 in Kayenta, AZ from 68033 to 86033
- changed zip code for a provider with DESMOND DOSS HEALTH CLINIC ANNEX O SCHOFIELD BARRACK in first and second address lines from 28310 to 96786
- changed zip code for a provider at 8762 HWY 162 in OPELOUSAS, LA from 70590 to 70570
- changed city name from BATON to Baton Rogue for a provider at 3084 WESTFORK DR. in LA
- changed zip code for a provider at 7335 MAIN STREET in Sykesville, MD, from 21785 to 21784
- changed zip code for a provider at 645 BOUROUGHS RD in Bowdoin, ME, from 04525 to 04287
- changed zip code for a provider at 21770 FDR BOULEVARD in Lexington Park, MD from 29653 to 20653
- changed zip code for a provider at 15TH STREET in Lincoln, NE, from 08588 to 68588
- changed zip code for a provider at FORT MCDERMITT WELLNESS CENTER SUITE 702G in Fort McDermitt from 98421 to 89421
- changed zip code for a provider at 10000 HAMPTON PARKWAY in Fort Jackson, SC, from 20207 to 29207
- changed zip code for a provider at 5265 S US-71 in Pineville, MO, from 65865 to 64856
- changed zip code for a provider at 100 CUMMINGS RD in Danvers, MA, from 07848 to 01923

After edits and reviewing matches with a confidence under 84%, there were still 5 providers that didn't 
match to a point location - these providers will be dropped, becuase there addresses had too many errors 
to narrow down where they might be located. 471 providers that did match to a point location based on 
address information were assigned a county with a spatial join to 2022 Cartographic Boundary county
boundaries.
/************************************************************************************/
data mh_GISassigned_county;
	set sasuser.MentalHealth_GISGeocoded_081825;
run;
*471 providers;

/* Combining datasets to create final clean mental health providers dataset where 
every provider has a county assigned */
data npi_mentalhealth_10;
	set npi_mentalhealth_9;
	statecode = statecode_z;
	countycode = countycode_z;
	fipscode = fipscode_z;
	county = county_name_z;
	state_abbr = state_name;
	drop statecode_z countycode_z fipscode_z county_name_z state_name_full_z X Y state_name state_name_z
	valid_state_name code country_code postal_code entity_type_code city2 zip_5_z
	Provider_Last_Name__Legal_Name_ Provider_First_Name Provider_Middle_Name Provider_Other_Last_Name;
run;
*1184293 providers and 12 columns;

data mh_GISassigned_county_2;
	set mh_GISassigned_county;
	statecode = STATEFP;
	countycode = COUNTYFP;
	fipscode = GEOID;
	county = NAME;
	state_abbr = STUSPS;
	drop STATEFP COUNTYFP GEOID NAME STUSPS Join_Count
	valid_state_name code country_code postal_code entity_type_code state_name
	Provider_Last_Name__Legal_Name_ Provider_First_Name Provider_Middle_Name Provider_Other_Last_Name
	Loc_name Status Score Match_type Match_addr Addr_type BldgComp MatchID StrucType StrucDet;
run;
*471 providers and 12 columns;

/************************************************************************************
2026 Total rows: 1184761 Total columns: 14
/************************************************************************************/
data  npi_mentalhealth_clean;
	set npi_mentalhealth_10 mh_GISassigned_county_2;
run;
*1184764 providers;

/* OPTIONAL: Save the clean NPI mental health providers dataset to your files */
data out.npi_mentalhealth_clean;
	set npi_mentalhealth_clean;
run;

/************************************************************************************
					v062 - Mental Health Providers - calculation
/************************************************************************************/
/* Aggregating to the county level and joining with master list of county fipscodes to check for
old county fipscodes */

*run if SAS was closed between initial data cleaning step and this step and you saved
the npi_mentalhealth_clean SAS dataset to your files;
data npi_mentalhealth_clean;
	set out.npi_mentalhealth_clean;
run;

/* Get column names */
proc sql;
	 select name
	 from dictionary.columns
	 where 	libname = 'WORK' 
	 		and memname = UPCASE('npi_mentalhealth_clean')  
	 ;
quit;

/************************************************************************************
Total rows before additional data cleaning: 2956 Total columns: 6
Total rows now after additional data cleaning: 2956 Total columns: 5
/************************************************************************************/
proc sql;
create table v062_county as
	select distinct statecode, countycode, fipscode, state_abbr, count(*) as v062_numerator
	from npi_mentalhealth_clean
	group by fipscode;
quit;

/* Join to master list of counties - includes new CT counties, but dropping any data
for new or old CT counties */
data fips;
	set inputs.county_fips_with_ct_old;
	rename county = chrr_county;
run;

proc sort data = v062_county; by fipscode; run;
*2956 counties;

proc sort data = fips; by fipscode; run;
*3152 counties;

data v062_county_check;
	merge v062_county fips;
	by statecode countycode fipscode;
run;
*3153 counties;

proc sort data = v062_county_check;
	by v062_numerator;
run;

proc sort data = v062_county_check;
	by chrr_county;
run;
*one county has data but didn't match anything in the chrr master county list - 02280;

data npidata_mentalhealth_02280;
	set npi_mentalhealth_clean;
	if fipscode ne "02280" then delete;
run;
*13 providers in this county;

/************************************************************************************
Wrangell-Petersburg Census Area, AK (02280) has become Wrangell City, AK (02275) and 
Petersburg Census Area, AK (02195). Reviewed npi_mentalhealth_clean dataset and 
all the providers that were assigned to this county code have a business practice 
address in Petersburg. Petersburg is the county seat of Petersburg Borough/Census Area.

There are already 2 providers that were assigned to the correct county, 02195, so 
dropping 02280 and manually editing v062_numerator to combine the providers. 
/************************************************************************************/

data v062_county_2;
	set v062_county;
	if fipscode = "02280" then delete;
	if fipscode = "02195" then v062_numerator = 2 + 13;
run;
*2955 counties;

proc sort data = v062_county_2; by fipscode; run;
proc sort data = fips; by fipscode; run;

data v062_county_3;
	merge v062_county_2 fips;
	by fipscode statecode countycode;
run;
*3152 counties;

/* Calculating state and national values */ 
proc means data = v062_county_3 sum noprint;
	by statecode;
	var v062_numerator;
	output out = v062_state sum =;
run;
*51 states;

data v062_state_2;
	set v062_state;
	drop _TYPE_ _FREQ_;
	countycode = "000";
	fipscode = statecode || countycode;
run;

proc means data = v062_state_2 sum noprint;
	var v062_numerator;
	output out = v062_national sum =;
run;
*1184764 mental health providers in the US;

data v062_national_2;
	set v062_national;
	drop _TYPE_ _FREQ_;
	statecode = "00";
	countycode = "000";
	fipscode = statecode || countycode;
run;

/* Merge county, state, and national values together to create the 
numerator dataset */
/************************************************************************************
National numerator;
2024: 1046182
2025: 1113571
2026 before additional data cleaning: 1184739
2026 after additional data cleaning: 1184764
/************************************************************************************/
proc sort data = v062_county_3; by fipscode; run;
proc sort data = v062_state_2; by fipcosde; run;
proc sort data = v062_national_2; by fipscode; run;

data v062_numerator;
	merge v062_county_3 v062_state_2 v062_national_2;
	by fipscode;
run;
*3204 observations;

/* Pulling in 2024 Census vintage population estimates for the denominator */ 
data vintage2024;
	set inputs.vintage2024 ;
	fipscode = statecode||countycode; 
proc sort;
	by statecode countycode;
run;
*3196 observations;

data vintage2024_2;
	set vintage2024;
	v062_denominator = popestimate2024;
	keep stname ctyname statecode countycode fipscode v062_denominator;
run;

/************************************************************************************
National denominator;
2024: 333287557
2025: 333287557
2026: 340110988
/************************************************************************************/
proc sql;
	select sum(v062_denominator) format best12.
		into : us_denominator
	from vintage2024_2
	where countycode = '000';
quit;

%put & = us_denominator;

data v062_denominator;
	set vintage2024_2;
	if statecode = "00" then do;
		v062_denominator = &us_denominator; 
	end;
run;

/* Merge v062 numerator and denominator datasets and calculate measure raw and 
alternate values */
/************************************************************************************
v062_rawwalue = v062_numerator / v062_denominator (the rate)
v062_rawalternatevalue = v062_denominator / v062_numerator (informs the ratio that's 
shown on the website - total pop to number of mental health providers)
/************************************************************************************/
proc sort data = v062_numerator; by fipscode; run;
proc sort data = v062_denominator; by fipscode; run;

data v062_calculation;
	merge v062_numerator v062_denominator;
	by statecode countycode fipscode;
run;
*3204 observations; 

data v062_calculation_2;
	set v062_calculation;
	v062_rawvalue = v062_numerator / v062_denominator;
	v062_rawalternatevalue = v062_denominator / v062_numerator;
run;

data v062_calculation_3;
	set v062_calculation_2;
	v062_cilow = .;
	v062_cihigh = .;
	v062_sourceflag = .;
	drop state_abbr state chrr_county stname ctyname;
run;

/* Suppression and final dataset */
/************************************************************************************
If a county has a population greater than 1,000 and 0 mental health providers, we set 
that county's value to missing.

Deleting all data for CT - we didn't making any updates to CT data in the 2025 rolling 
data updates. 
/************************************************************************************/
data v062;
	set v062_calculation_3;
	if v062_numerator = . then v062_numerator = 0;
	*Treating missing numerator values as 0 mental health providers in a county;
	if v062_numerator = 0 and v062_denominator > 1000 then v062_rawvalue = .;
	if v062_numerator = 0 and v062_denominator > 1000 then v062_rawalternatevalue = .;
	if v062_numerator = 0 and v062_denominator <= 1000 then v062_rawalternatevalue = 0;
	if v062_numerator = 0 and v062_denominator <= 1000 then v062_rawvalue = 0;
	*Fixing divide by 0 error for counties with 0 providers and less than 1000 population;
	if statecode = "09" then do;
		v062_numerator = .; v062_denominator = .; v062_rawvalue = .; v062_rawalternatevalue = .;
	end;
run;

/* Save in measure_datasets folder (create new outpath if you want to save dataset in 
a different folder on your local machine, otherwise this will overwrite what's saved in 
measure_datasets already) */
data out.v062_s2026;
	set v062;
run;

/************************************************************************************
			   v131 - Other Primary Care Providers - data cleaning
/************************************************************************************/

/* retaining only the taxonomy codes we include in other primary care providers measure */
/************************************************************************************
This website can be used to search all taxonomy codes: 
https://taxonomy.nucc.org/?searchTerm=&x=6&y=12

CHR&R has used the same list of taxonomy codes for mental health providers for several 
years. In August 2026, all taxonomy codes were verified to have not changed.

2024: Total rows: 438809 Total columns: 15 
2025: Total rows: 469775 Total columns: 15 
2026: Total rows: 499536 Total columns: 16
/************************************************************************************/

*Bring in npi_individuals SAS dataset saved in your files OR
re-run the initial steps to create the npi_individuals SAS dataset;
data npidata_individuals;
	set out.npidata_individuals;
run;

data npidata_otherpcp;
	set npidata_individuals;
	length type $20;
	if Taxonomy_Code = '363L00000X' then do; code = 1;type = 'nurse practitioner'; 	end;*nurse practitioner;
	if Taxonomy_Code = '363LC1500X' then do; code = 1;type = 'nurse practitioner'; 	end;*nurse practitioner, community health;
	if Taxonomy_Code = '363LF0000X' then do; code = 1;type = 'nurse practitioner'; 	end;*nurse practitioner, family;
	if Taxonomy_Code = '363LX0001X' then do; code = 1;type = 'nurse practitioner'; 	end;*nurse practitioner, ob-gyn;
	if Taxonomy_Code = '363LP0200X' then do; code = 1;type = 'nurse practitioner'; 	end;*nurse practitioner, pediatrics;
	if Taxonomy_Code = '363LP2300X' then do; code = 1;type = 'nurse practitioner'; 	end;*nurse practitioner, primary care;
	if Taxonomy_Code = '363A00000X' then do; code = 1;type = 'phys assistant';	 	end;*physician assistant;
	if Taxonomy_Code = '363AM0700X' then do; code = 1;type = 'phys assistant'; 		end;*physician assistant, medical;
	if code = 1;
run;

proc freq data = npidata_otherpcp;
	tables type;
run;

/* examine provider zip codes and addresses to look for inconsistencies */
/************************************************************************************
Providers have to apply for their NPI - there are likely data errors from mistakes 
providers made when filling out the forms, or when information from the forms is 
compiled into the full NPI dataset. Additionally, the USPS changes zip codes with 
some frequency, and zip codes cross county and state boundaries. 
/************************************************************************************/
proc freq data = npidata_otherpcp;
	tables zip_5;
run;
*there are 2 zip codes with just 0s, and a few zip codes that are a combo of numbers and letters;

proc freq data = npidata_otherpcp;
	tables state_name;
run;
*There are many values in the state_name column that don't match a US state abbreviation 
(see links below). It looks like a couple US city names may have also been accidentally 
entered into the state abbreviation field. Some state names are spelled out instead of 
abbreviated. Foriegn city and country names have also been entered in the field, should 
be dropped when dropping providers outside the US;

proc freq data = npidata_otherpcp;
	tables country_code;
run;
*499,369 providers have a US country code - but since it seems there are data 
entry errors, not restricting based on country code yet.;

/************************************************************************************
See this webpage for valid state abbreviations in the file: 
https://npiregistry.cms.hhs.gov/help-api/state

AE, AP, AA "state name" abbreviations are for overseas military addresses.
APO or FPO "city" designations are also required for overseas military addresses.  
https://pe.usps.com/text/pub28/28c2_010.htm#:~:text=AE%20is%20used%20for%20armed,is%20the%20Americas%20excluding%20Canada.
https://faq.usps.com/s/article/How-Do-I-Address-Military-Mail

Overseas military providers will be excluded from the analysis where possible 
but military providers in the contiguous US states will be included. 

See this webpage for valid country abbreviations in the file: 
https://npiregistry.cms.hhs.gov/help-api/country

All providers outside the US will be excluded from the analysis. 
/************************************************************************************/

/* Run BRINGING IN ZIP CODE DATASET section from above to create zip dataset */

*Process note - running the SAS zip code geocoding (assigning provider to a county based on zip 
code) before deleting an providers based off state abbreviation or country code. Also going
to test geocoding by joining just on zip codes and then on zip codes and state abbreviations
or name;

/* Merging other primary care data with ZIP CODES to assign counties */
/************************************************************************************
Joining just on zip codes. 

2025: Total rows: 469775 Total columns: 36
2026: Total rows: 499536 Total columns: 26
/************************************************************************************/
proc sql;
	create table npi_otherpcp_zipjoin as
	select x.*, y.*
	from npidata_otherpcp as x
	left join zip as y
	on x.zip_5 = y.zip_5_z;
quit;

/* Looking for weird observations or providers that did not have a zip code join */
proc sort data = npi_otherpcp_zipjoin; by state_name; run;
*There are overseas military providers (state_name AA, AE, AP) and a few providers in 
what looks like Japan with longer than a two letter abbreviation in the state_name field;

proc sort data = npi_otherpcp_zipjoin; by state_name zip_5_z; run;
*There are at least a few providers where their listed state abbreviation doesn't match 
with the state of the zip code they were joined to in the SAS zip code file;

/* Checking the providers that didn't have a zip code join */
/************************************************************************************
2025: Total rows: 564 Total columns: 36
2026: Total rows: 603 Total columns: 26
/************************************************************************************/
data pcp_zipjoin_notassignedcounty; 
	set npi_otherpcp_zipjoin;
	where county_name_z = '';
run;

proc sort data = pcp_zipjoin_notassignedcounty;	by zip_5; run;
*Many that weren't assigned a county have state abbreviations (state_name) for a 
an overseas military base or country codes that are outside of the US;

proc sort data = pcp_zipjoin_notassignedcounty; by state_name; run;
*There are overseas military providers (state_name AA, AE, AP) and some with state_names
that aren't 2 letter abbreviations; 

proc freq data = pcp_zipjoin_notassignedcounty;
	tables state_name;
run;
*Most of the providers with zip codes that didn't join to the SAS zip code 
file have a state_name of AE or AP - overseas military providers. Providers 
with valid state codes that didn't join:
27 in AZ, 
16 in CO, 
22 in DC,
10 in NY, etc.;

proc sort data = pcp_zipjoin_notassignedcounty;	by country_code; run;
*Looks like the vast majority of providers who don't have a US country code don't 
have a state code for any of the states in the US - probably safe to drop all 
providers that don't have a US country code??;

/* Checking the providers that had a zip code join, but where the state in the NPI 
dataset and the state in the SAS zip code dataset don't match - incorrect geocode */
data pcp_zipjoin_incorrect;
	set npi_otherpcp_zipjoin;
	where state_name ne state_name_z;
run;
*759 providers - pulling in providers that didn't have a zip join at all as well;

proc sort data = pcp_zipjoin_incorrect;	by country_code state_name; run;
*There are a lot of providers with a country code of UM that look like they have US 
addresses. DON'T drop all non US country code providers yet.;

/* Merging other primary care provider data with STATE NAME AND ZIP CODE to assign counties */
/************************************************************************************
Joining on zip codes and state_name fields (state abbreviations). 

2026 Total rows: 499536 Total columns: 26 
/************************************************************************************/
proc sql;
	create table npi_otherpcp_zipstjoin as
	select x.*, y.*
	from npidata_otherpcp as x
	left join zip as y
	on x.zip_5 = y.zip_5_z and
	x.state_name = y.state_name_z;
quit;

/* Checking the providers that didn't have a zip code and state join */
/************************************************************************************
2026: Total rows: 759 Total columns: 26
/************************************************************************************/
data pcp_zipstjoin_notassignedcounty; 
	set npi_otherpcp_zipstjoin;
	where county_name_z = '';
run;
*number of providers without a join went up - this makes sense if there were incorrect 
joins happening in the first join based just on zip codes;

proc sort data = pcp_zipstjoin_notassignedcounty; by zip_5; run;

proc sort data = pcp_zipstjoin_notassignedcounty; by state_name; run;

proc freq data = pcp_zipstjoin_notassignedcounty;
	tables state_name;
run;
*Most of the providers with zip codes and state abbreviations that didn't join
have a state code of AA, AE or AP - overseas military providers. Providers with 
valid state codes that didn't join:
27 in AZ, 
12 in CA, 
17 in CO, 
25 in DC, 
14 in NY, etc.; 

proc freq data = pcp_zipstjoin_notassignedcounty;
	tables country_code;
run;
*There are a good number of providers with country code UM or DE - taking a look at 
all the providers with a country code UM and DE below;

data pcp_zipstjoin_UM_DE;
	set npi_otherpcp_zipstjoin;
	where country_code = "UM" or country_code = "DE";
	proc sort;
		by country_code state_name;
run;
*84 providers - all providers with a country code of DE have a state_name of AE or a German 
city - overseas military providers. Some providers with a county code of UM have US state 
abbreviations or have full US state names in the state_name (abbreviation)
field. Some have a state name PR, PUERTO RICO, VI or VIRGIN ISLAND - dropping those;

data pcp_zipstjoin_UM;
	set npi_otherpcp_zipstjoin;
	where country_code = "UM";
	if state_name = "PR" then delete;
	if state_name = "PUERTO RICO" then delete;
	if state_name = "VI" then delete;
	if state_name = "VIRGIN ISLAND" then delete;
	proc sort;
		by state_name;
run;
*36 providers;

*Process note - preserve all providers with a country code of US or UM, then delete 
providers that have a state_name that indicates and overseas military installation 
or Puerto Rico or the Virgin Islands. From there, make the updates to AZ and OH zipcodes 
that we know of and then try to geocode the providers that still didn't join based
on zip code and state_name in ArcGIS Pro.;

/* Remove providers who are outside of the US or overseas military providers */
/************************************************************************************
Starting from the dataset other primary care providers were joined to zip codes AND 
state abbreviations. Retaining UM providers for now because there seem to be data 
entry errors, but dropping providers with a state_name PR, PUERTO RICO, VI or VIRGIN ISLAND,
and overseas military providers (indicated by state_name AA, AE, or AP).
/************************************************************************************/
data npi_otherpcp_2;
	set npi_otherpcp_zipstjoin;
	where country_code = "US" OR country_code = "UM";
run;
*499412 providers;

proc sort data = npi_otherpcp_2; by state_name; run;
*There are many providers with a state code of PR and a country code of US but upon visual 
inspection most geocoded with matching city names to cities in Puerto Rico; 

proc freq data = npi_otherpcp_2;
	tables state_name;
run;
*After visual inspection of providers with state_name VI it looks like all but one are in the 
Virgin Islands. There is one provider in ANNENDALE which is a town in VA - state_name was 
likely entered incorrectly;

data npi_otherpcp_3;
	set npi_otherpcp_2;
	if state_name = "AA" then delete;
	if state_name = "AE" then delete;
	if state_name = "AP" then delete;
	if state_name = "PR" then delete;
	if state_name = "PUERTO RICO" then delete;
	if state_name = "VI" and city_name = "ANNENDALE" then state_name = "VA";
run;
*498591 providers;

proc sort data = npi_otherpcp_3; by state_name; run;

proc freq data = npi_otherpcp_3;
	tables state_name;
run;
*Still some state names that are spelled out instead of abbreviated and some other potentially 
incorrect state names. AS and GU providers weren't dropped yet, after a quick visual scan it looks like
3 of 4 of the providers with a state name AS have a city name in American Samoa and one looks like it might 
be an error (the provider's city name is WEST MEMPHIS, state_name should be AZ). Providers with a state
name GU have a city name in Guam (ie no entry errors);

/* Assigning a code to all providers with valid state names (abbreviations) */
%let st_ls = 'AK', 'AL', 'AR', 'AZ', 'CA', 'CO', 'CT', 
'DC', 'DE', 'FL', 'GA', 'HI', 'IA', 'ID', 'IL', 'IN', 'KS', 'KY', 'LA', 
'MA', 'MD', 'ME', 'MI', 'MN', 'MO', 'MS', 'MT', 'NC', 'ND', 'NE', 'NH', 
'NJ', 'NM', 'NV', 'NY', 'OH', 'OK', 'OR', 'PA', 'RI', 'SC', 'SD', 'TN', 
'TX', 'UT', 'VA', 'VT', 'WA', 'WI', 'WV', 'WY';
%put &=st_ls;

data npi_otherpcp_4;
	set npi_otherpcp_3;
	if city_name = "WEST MEMPHIS" and state_name = "AS" then state_name = "AR";
	if state_name = "GU" then delete;
	if state_name = "AS" then delete;
	if state_name = "VI" then delete;
	if state_name = "VIRGIN ISLAND" then delete;
	if state_name in (&st_ls) then valid_state_name = 1;
run;
*498475 providers;

proc means data = npi_otherpcp_4 sum;
	var valid_state_name;
run;
*498437 out of 498475 providers has a valid state name (abbreviation), 
38 need fixing or need to be dropped;

/* Creating a dataset of the other primary care providers with an incorrect state name or 
that didn't join to the SAS zip code file */
data npi_otherpcp_tofix;
	set npi_otherpcp_4;
	where valid_state_name ne 1 OR county_name_z = '';
run;
*292 providers;

proc freq data = npi_otherpcp_tofix;
	tables state_name;
run;
*Based on visual inspection of the tofix dataset and proq freq results, there 
are providers in Korea and the Marianas Islands (MP, NMI) to drop; 

proc sort data = npidata_otherpcp_tofix; by state_name; run;

/************************************************************************************
Manual checking and fixes to make. Looked up addresses and city names quickly:
- Change state name AK- ALASKA to AK
- Change state name AZ- ARIZONA to AZ
- Change state name CALIFORNIA to CA
- Change state names FLORIDA, FLORIDA (FL) to FL
- Change state name IDAHO to ID
- Change state name ILLINOIS to IL
- Change state name KANSAS to KS
- Change state name KENTUCKY to KY
- Change state name MICHIGAN to MI
- Change state name NEW YORK to NY
- Change state name OAKLAND to MI (provider's address is in MI)
- Change state name TEXAS to TX
- Change provider in STERLING with state_name USA to MA (provider's address is in MA)
- Change provider in BROOKLYN with state_name USA to NY (provider's address is in NY)
- Change state name VIRGINIA to VA
- Drop providers with a state name of KOREA, MP or NMI

From previous years of this code being run, updating zip codes in AZ and OH to reflect 
changes we're aware of. Washington, D.C. zip code of 20307 belongs to Walter Reed Medical 
Center which closed in 2011 and moved to Bethesda. Delete any provider that uses this 
zipcode. 

Drop all providers with city names like/include APO, A.P.O. A.P., APO/AE, FPO, APO AE?
/************************************************************************************/
data npi_otherpcp_5;
	set npi_otherpcp_4;
	if state_name = "AK- ALASKA" then state_name = "AK";
	if state_name = "AZ- ARIZONA" then state_name = "AZ";
	if state_name = "CALIFORNIA" then state_name = "CA";
	if state_name = "FLORIDA" then state_name = "FL";
	if state_name = "FLORIDA (FL)" then state_name = "FL";
	if state_name = "IDAHO" then state_name = "ID";
	if state_name = "ILLINOIS" then state_name = "IL";
	if state_name = "KANSAS" then state_name = "KS";
	if state_name = "KENTUCKY" then state_name = "KY";
	if state_name = "MICHIGAN" then state_name = "MI";
	if state_name = "NEW YORK" then state_name = "NY";
	if state_name = "OAKLAND" then state_name = "MI";
	if state_name = "TEXAS" then state_name = "TX";
	if city_name = "STERLING" and state_name = "USA" then state_name = "MA";
	if city_name = "BROOKLYN" and state_name = "USA" then state_name = "NY";
	if state_name = "VIRGINIA" then state_name = "VA";
	if state_name = "KOREA" then delete;
	if state_name = "MP" then delete;
	if state_name = "NMI" then delete;
run;
*498456 providers;

proc freq data = npi_otherpcp_5;
	tables state_name;
run;

/************************************************************************************
update Arizona zip codes, May 2009 changes http://about.usps.com/postal-bulletin/2009/pb22259/html/info_001.htm;
update Ohio zip codes, July 2006 changes https://about.usps.com/postal-bulletin/2006/html/pb22184/pb8p-s_002.html,
and August 2009 changes https://about.usps.com/postal-bulletin/2009/pb22265/html/info1_001.htm
/************************************************************************************/
data npi_otherpcp_6;
	set npi_otherpcp_5;
	*Arizona;
	if zip_5 = '85222' then zip_5 = '85122';
	if zip_5 = '85220' then zip_5 = '85120';
	if zip_5 = '85242' then zip_5 = '85142';
	if zip_5 = '85273' then zip_5 = '85173';
	if zip_5 = '85247' then zip_5 = '85147';
	if zip_5 = '85239' then zip_5 = '85139';
	if zip_5 = '85237' then zip_5 = '85137';
	if zip_5 = '85232' then zip_5 = '85132';
	if zip_5 = '85228' then zip_5 = '85128';
	if zip_5 = '85219' then zip_5 = '85119';
	if zip_5 = '85218' then zip_5 = '85118';
	if zip_5 = '85223' then zip_5 = '85123';
	if zip_5 = '85231' then zip_5 = '85131';
	if zip_5 = '85238' then zip_5 = '85138';
	if zip_5 = '85240' then zip_5 = '85140';
	if zip_5 = '85243' then zip_5 = '85143';
	*Ohio;
	if zip_5 = '43624' then zip_5 = '43604';
	if zip_5 in ('45408' '45418' '45427') then zip_5 = '45417';
	*/
	*Drop Walter Reed Medical Center providers with old zip code;
	if zip_5 = "20307" then delete;
run;
*498437 providers;

proc sort data = npi_otherpcp_6;	
	by city_name;
run;

proc freq data = npi_otherpcp_6;
	tables city_name;
run;

/* Looking for more military providers - play with the wildcard operators to make sure 
only military installations are being pulled in */
data npi_otherpcp_OSmilitary;
	set npi_otherpcp_6;
	where city_name like 'APO %'
		  /*or city_name like '%APO%'*/
		  or city_name like '%APO AE%'
		  or city_name like 'FPO%'
		  or city_name like '% AA %'
		  or city_name like '% AE %'
	      or city_name like '% AP %'
		  or city_name like 'APO/AE'
		  or city_name like 'APO AE'
		  or city_name like 'A.P.O. A.P.';
		  *do a visual inspection of the results and google any addresses where it's unclear if the city name is actually referring to a military installation;
		  *a lot of city names include APO - just look at the resulting dataset and adjust wild card to find providers with city names where APO stands alone;
		  *after messing with wildcards, there seems to be only one provider with a city name of FPO or A.P.O. A.P.;
run;
*10 providers;
*assuming providers with city names like these are overseas military providers - 
looked up some addresses and this seems to be true;

/* dropping more overseas military providers */
data npi_otherpcp_7;
	set npi_otherpcp_6;
	if city_name in ('A.P.O. A.P.', 'APO', 'APO AE', 'APO/AE', 'FPO', 'RAMSTEIN AB APO AE') then delete;
run;
*498427 providers;

/* Re-do SAS zip code geocoding to assign counties, joining on STATE NAME AND ZIP CODE */
/************************************************************************************
Joining on zip codes and state_name fields (state abbreviations).
Dropping all the zip columns from npi_otherpcp_7 before re-doing the join.
/************************************************************************************/
data npi_otherpcp_8;
	set npi_otherpcp_7;
	drop X Y state_name_z state_name_full_z county_name_z city2 zip_5_z countycode_z
	statecode_z fipscode_z;
run;

proc sql;
	create table npi_otherpcp_zipstjoin2 as
	select x.*, y.*
	from npi_otherpcp_8 as x
	left join zip as y
	on x.zip_5 = y.zip_5_z and
	x.state_name = y.state_name_z;
quit;

/* Checking the providers that didn't have a zip code and state join */
data pcp_zipstjoin2_notassignedcounty; 
	set npi_otherpcp_zipstjoin2;
	where county_name_z = '';
run;
*198 providers;

proc sort data = pcp_zipstjoin2_notassignedcounty; by state_name; run;

proc freq data = pcp_zipstjoin2_notassignedcounty;
	tables state_name;
run;
*there are still some providers in each state with zip codes that didn't match: 
11 in CA
16 in CO
9 in GA
9 in NJ, etc.;

/* Saving a dataset of the other primary care providers who didn't have a zip code match for 
geocoding in ArcGIS Pro and then dropping those providers from the larger dataset. Will add 
those providers back in after geocoding outside of SAS */
data sasuser.pcp_zipstjoin2_notassignedcounty;
	set pcp_zipstjoin2_notassignedcounty;
run;

data npi_otherpcp_9;
	set npi_otherpcp_zipstjoin2;
	if county_name_z = '' then delete;
run;
*498229 providers;

/* OUTSIDE OF SAS - Geocode addresses in Pro with ArcGIS World Geocoding Service */
/************************************************************************************
Finding point locations of providers based on the first and second address lines, 
city name and state name. Not using zip code for geocoding since some zip codes might 
now be inaccurate. The categories for geocoding results were Point Address, Street 
Address, Primary Postal, or Postal Locality. After first round of geocoding there were
29 unmatched addresses and 6 ties. Used the Rematch Addresses tool to manually go through
the unmatched and tied addresses and make edits to the data to find the correct match (point 
location) for the provider (based on Google search results and mathcing zip codes - which 
were not included in this first round of geocoding). A list of edits made are below:

Some providers had addresses I couldn't figure out - will do one more round of geocoding with 
zipcodes included and will try to match to zip codes or city names.
- changed first address line for a provider in Richmond, VA, from MCGUIRE VA MEDICAL CENTER to 1201 BROAD ROCK BLVD. (info was in second address line) and changed state name from AL to VA (zip code matched VA address) - still didn't match on first round
- changed state name from CA to CO for provider at 1650 COCHRANE CIRCLE in Fort Carson
- changed state name from CT to MA for a provider at 759 CHESTNUT ST. in Springfield
- changed state name from GA to SC for a provider at 8201 PINELLAS DR in Bluffton
- changed first address line for a provider in Port St. Lucie from LUMAR PLAZA, 1847 PSL BLVD to 1847 Port Saint Lucie Blvd - matched with an address that had a zip code one number off - 34592 instead of 94952
- changed first address line for a provider in HI from JARRETT WHITE RD. to 1 Jarrett White Road and changed city name from TRIPLER AMC to Honolulu
- changed state name from IL to IN for a provider at 1481 W 10TH STREET in Indianapolis 
- changed state name from NE to NY for a provider at 318 RUHLE RD S in Ballston Lake
- changed state name from NM to NY for a provider at 6949 BRITTONFIELD PARKWAY in East Syracuse
- changed state name from NY to NJ for a provider at 1200 PARK AVE in Plainfield - didn't match yet in first round
- changed state name from NM to AZ for a provider at 3411 N 5TH AVE., STE. 209 in Phoenix
- changed state name from NY to NJ for a provider at 468 PARISH DR in Wayne
- changed state name from NY to NC for a provider at 1705 S. TARBORO in Wilson 
- changed state name from OH to WV for a provider at 408 ALEXANDER STREET in Cedar Grove 
- changed state name from PA to MN for a provider at 990 BREN ROAD EAST in Minnetonka - still didn't match on first round 
- changed state name from TN to TX for a provider at 1504 TAUB LOOP in Houston
- changed second address line for a provider in Southlake, TX, from 9140 E. STATE HWY 114, SUITE 150 to 1940 E State Hwy 114

Edits for providers that tied to matching with multiple locations 
or that had a match confidence of less than 84%: 
- changed state name from NJ to NY for a provider at ONE SOUTH BROADWAY in Hastings-On-Hudson

Ran the geocoding again based on the first and second address lines, city name, state name AND 
zip code. The categories for geocoding results were Point Address, Street Address, Primary Postal,
or Postal Locality. After second round of geocoding there were 10 unmatched addresses and 4 ties. 
Reviewed unmatches and ties again with Rematch Addresses tool. Edits made (some zip codes were invalid
and I googled what zip codes were associated with city names to make edits): 
- changed zip code for a provider at BLDG. H 2005 KNIGHT LANE ATTN: MEDICAL STAFF SERVICES in Jacksonville, FL, from 92055 to 32212 (a zip code in Jacksonville)
- changed zip code for a provider at 45465 5TH AVE. in Jacksonville, FL, from 30374 to 32011 (a zip code in Jacksonville)
- changed zip code for a provider at BUILDING 683 WAIANAE AVE in Schofield Barracks, HI, from 96876 to 96786
- changed zip code for a provider at 12000 STONE LAKE ROAD in Dulce, NM, from 97528 to 87528
- changed zip code for a provider at 31-51 STONEY ST. in Shrub Oak, NY, from 10058 to 10588
- changed zip code for a provider at UNIVERSITY DRIVE C PITTSBURGH VA HEALTH CARE SYSTEM in Pittsburgh, PA, from 14240 to 15240
- changed zip code for a provider at 2 MI NORTH OF WHITEDEER (?) in Whitedeer, PA, from 17717 to 17887 (only zip code for Whitedeer, PA, on USPS website)
- changed zip code for a provider at 138 CANAL STREET, UNIT 308 in West Columnia, SC, from 26169 to 29169
- changed zip code for a provider at 3601 THE VANDERBILT CLINIC in Nashville, TN, from 38204 to 37232 (zip code for clinic listed on Google)
- changed zip code for a provider at S 34TH ST AND CIVIC CENTER BLVD CHILDREN'S HOSPITAL OF PHILADELPHIA from 08107 to 19104 (hospital's zip code listed on Google)
- changed zip code for a provider at 3500 I-30 BOX in Mesquite, TX, from 78185 to 75185
- changed zip code for a provider at 130 HWY 252 in Columbia, SC, from 29261 to 29621

After edits and reviewing matches with a confidence under 84%, all providers were matched 
with an address or zip code location. 198 providers were assigned a county with a spatial 
join to 2022 Cartographic Boundary county boundaries.
/************************************************************************************/
data otherPCP_GISassigned_county;
	set sasuser.OtherPCPs_GISGeocoded_082025;
run;
*198 providers;

/* Combining datasets to create final clean other primary care providers dataset where 
every provider has a county assigned */
data npi_otherpcp_10;
	set npi_otherpcp_9;
	statecode = statecode_z;
	countycode = countycode_z;
	fipscode = fipscode_z;
	county = county_name_z;
	state_abbr = state_name;
	drop statecode_z countycode_z fipscode_z county_name_z state_name_full_z X Y state_name state_name_z
	valid_state_name code country_code postal_code entity_type_code city2 zip_5_z
	Provider_Last_Name__Legal_Name_ Provider_First_Name Provider_Middle_Name Provider_Other_Last_Name;
run;
*498229 providers and 12 columns;

data otherPCP_GISassigned_county_2;
	set otherPCP_GISassigned_county;
	statecode = STATEFP;
	countycode = COUNTYFP;
	fipscode = GEOID;
	county = NAME;
	state_abbr = state_name;
	drop STATEFP COUNTYFP GEOID NAME STUSPS Join_Count
	valid_state_name code country_code postal_code entity_type_code state_name
	Provider_Last_Name__Legal_Name_ Provider_First_Name Provider_Middle_Name Provider_Other_Last_Name
	Loc_name Status Score Match_type Match_addr Addr_type BldgComp MatchID StrucType StrucDet;
run;
*198 providers and 12 columns;

/************************************************************************************
2026 Total rows: 498427 Total columns: 12
/************************************************************************************/
data npi_otherpcp_clean;
	set npi_otherpcp_10 otherPCP_GISassigned_county_2;
run;
*498427 providers;

/* OPTIONAL: Save the clean NPI other primary care providers dataset to your files */
data out.npi_otherpcp_clean;
	set npi_otherpcp_clean;
run;

/************************************************************************************
			     v131 - Other Primary Care Providers - calculation
/************************************************************************************/
*run if SAS was closed between initial data cleaning step and this step and you saved
the npi_otherpcp_clean SAS dataset to your files;
data npi_otherpcp_clean;
	set out.npi_otherpcp_clean;
run;
*498427 providers;

/* Aggregating to the county level and joining with master list of county fipscodes to check for
old county fipscodes */
/************************************************************************************
2026 Total rows before additional data cleaning: 3073 Total columns: 4
2026 Total rows now after additional data cleaning: 3105 Total columns: 5
/************************************************************************************/
proc sql;
create table v131_county as
	select distinct statecode, countycode, fipscode, state_abbr, count(*) as v131_numerator
	from npi_otherpcp_clean
	group by fipscode;
quit;

/* Join to master list of counties - includes new CT counties, but dropping any data
for new or old CT counties */
data fips;
	set inputs.county_fips_with_ct_old;
	rename county = chrr_county;
run;

proc sort data = v131_county; by fipscode; run;
*3105 counties;

proc sort data = fips; by fipscode; run;
*3152 counties;

data v131_county_check;
	merge v131_county fips;
	by statecode countycode fipscode;
run;
*3184 counties - some county fipscodes likely need updating;

*Checking for duplicates in v131_county - some providers might have been assigned to a county 
but there state abbreviation was incorrect, leading to double records for counties since 
state_abbr was retained in the proc sql step;
proc sql;
	select *, count(*) as Count
		from v131_county
		group by statecode, countycode, fipscode
		having count(*) > 1;
run;
quit;
*there are many duplicates - and they do all seem to be a result of two different state 
abbreviations being associated with one county (based on fipscode); 

/* Re-creating v131_county dataset */
/************************************************************************************
2026 Total rows before additional data cleaning: 3073 Total columns: 4
2026 Total rows now after additional data cleaning: 3074 Total columns: 4
/************************************************************************************/
proc sql;
create table v131_county_2 as
	select distinct statecode, countycode, fipscode, count(*) as v131_numerator
	from npi_otherpcp_clean
	group by fipscode;
quit;
*3074 rows;

proc sort data = v131_county_2; by fipscode; run;
*3074 counties;

proc sort data = fips; by fipscode; run;
*3152 counties;

data v131_county_check_2;
	merge v131_county_2 fips;
	by statecode countycode fipscode;
run;
*3153 counties;

proc sort data = v131_county_check_2;
	by v131_numerator;
run;

proc sort data = v131_county_check_2;
	by chrr_county;
run;
*one county has data but didn't match anything in the chrr master county list - 02280;

data npi_otherpcp_02280;
	set npi_otherpcp_clean;
	if fipscode ne "02280" then delete;
run;
*4 providers in this county;

/************************************************************************************
Wrangell-Petersburg Census Area, AK (02280) has become Wrangell City, AK (02275) and 
Petersburg Census Area, AK (02195). Reviewed npi_otherpcp_clean dataset and 
all the providers that were assigned to this county code have a business practice 
address in Petersburg. Petersburg is the county seat of Petersburg Borough/Census Area.
/************************************************************************************/
data npi_otherpcp_02195;
	set npi_otherpcp_clean;
	if fipscode ne "02195" then delete;
run;
/************************************************************************************
There are already 2 providers that were assigned to the correct county, 02195, so 
dropping 02280 and manually editing v131_numerator to combine the providers. 
/************************************************************************************/

data v131_county_3;
	set v131_county_2;
	if fipscode = "02280" then delete;
	if fipscode = "02195" then v131_numerator = 2 + 4;
run;
*3073 counties;

proc sort data = v131_county_3; by fipscode; run;
proc sort data = fips; by fipscode; run;

data v131_county_4;
	merge v131_county_3 fips;
	by fipscode statecode countycode;
run;
*3152 counties;

/* Calculating state and national values */ 
proc means data = v131_county_4 sum noprint;
	by statecode;
	var v131_numerator;
	output out = v131_state sum =;
run;
*51 states;

data v131_state_2;
	set v131_state;
	drop _TYPE_ _FREQ_;
	countycode = "000";
	fipscode = statecode || countycode;
run;

proc means data = v131_state_2 sum noprint;
	var v131_numerator;
	output out = v131_national sum =;
run;
*498427 other primary care providers in the US;

data v131_national_2;
	set v131_national;
	drop _TYPE_ _FREQ_;
	statecode = "00";
	countycode = "000";
	fipscode = statecode || countycode;
run;

/* Merge county, state, and national values together to create the 
numerator dataset */
/************************************************************************************
National numerator;
2024: 437906
2025: 468761
2026 before additional data cleaning: 498418
2026 after additional data cleaning: 498427
/************************************************************************************/
proc sort data = v131_county_4; by fipscode; run;
proc sort data = v131_state_2; by fipscode; run;
proc sort data = v131_national_2; by fipscode; run;

data v131_numerator;
	merge v131_county_4 v131_state_2 v131_national_2;
	by fipscode;
run;
*3204 observations;

/* Pulling in 2024 Census vintage population estimates for the denominator */ 
data vintage2024;
	set inputs.vintage2024 ;
	fipscode = statecode||countycode; 
proc sort;
	by statecode countycode;
run;
*3196 observations;

data vintage2024_2;
	set vintage2024;
	v131_denominator = popestimate2024;
	keep stname ctyname statecode countycode fipscode v131_denominator;
run;

/************************************************************************************
National denominator;
2024: 333287557
2025: 333287557
2026: 340110988
/************************************************************************************/
proc sql;
	select sum(v131_denominator) format best12.
		into : us_denominator
	from vintage2024_2
	where countycode = '000';
quit;

%put & = us_denominator;

data v131_denominator;
	set vintage2024_2;
	if statecode = "00" then do;
		v131_denominator = &us_denominator; 
	end;
run;

/* Merge v131 numerator and denominator datasets and calculate measure raw and 
alternate values */
/************************************************************************************
v131_rawwalue = v131_numerator / v131_denominator (the rate)
v131_rawalternatevalue = v131_denominator / v131_numerator (informs the ratio that's 
shown on the website - total pop to number of mental health providers)
/************************************************************************************/
proc sort data = v131_numerator; by fipscode; run;
proc sort data = v131_denominator; by fipscode; run;

data v131_calculation;
	merge v131_numerator v131_denominator;
	by statecode countycode fipscode;
run;
*3204 observations; 

data v131_calculation_2;
	set v131_calculation;
	v131_rawvalue = v131_numerator / v131_denominator;
	v131_rawalternatevalue = v131_denominator / v131_numerator;
run;

data v131_calculation_3;
	set v131_calculation_2;
	v131_cilow = .;
	v131_cihigh = .;
	v131_sourceflag = .;
	drop state chrr_county stname ctyname;
run;

/* Suppression and final dataset */

/************************************************************************************
If a county has a population greater than 4,000 and 0 other primary care, we set 
that county's value to missing.

Deleting all data for CT - we aren't making any updates to CT data in the 2025 rolling 
data updates. 
/************************************************************************************/
data v131;
set v131_calculation_3;
	if v131_numerator = . then v131_numerator = 0;
	*Treating missing numerator values as 0 other primary care providers in a county;
	if v131_numerator = 0 and v131_denominator > 4000 then v131_rawvalue = .;
	if v131_numerator = 0 and v131_denominator > 4000 then v131_rawalternatevalue = .;
	if v131_numerator = 0 and v131_denominator <= 4000 then v131_rawvalue = 0;
	if v131_numerator = 0 and v131_denominator <= 4000 then v131_rawalternatevalue = 0;
	*Fixing divide by 0 error for counties with 0 providers and less than 4000 population;
	if statecode = "09" then do;
		v131_numerator = .; v131_denominator = .; v131_rawvalue = .; v131_rawalternatevalue = .;
	end;
run;

/* Save in measure_datasets folder (create new outpath if you want to save dataset in 
a different folder on your local machine, otherwise this will overwrite what's saved in 
measure_datasets already) */
data out.v131_s2026;
	set v131;
run;
