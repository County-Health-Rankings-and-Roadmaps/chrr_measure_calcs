
/*****************************************************************************
 * v182 - Adverse Climate Events
 * Author: JH
 * Description: Indicator of thresholds met for the following adverse climate and weather-related event categories: extreme heat (300 or more days above 90F), moderate or greater drought (65 or more weeks), and disaster (2 or more presidential disaster declarations) over the five-year period.
 * Data Source: Environmental Public Health Tracking (EPHT) Network; U.S. Drought Monitor (USDM); OPEN FEMA Disaster Declaration Summaries
 * Data Download Link: Heat: https://ephtracking.cdc.gov/DataExplorer/?query=6b72eecd-f28f-43c4-a966-a5e496645182
					   Disasters: https://www.fema.gov/openfema-data-page/disaster-declarations-summaries-v2
					   Drought: https://droughtmonitor.unl.edu/DmData/DataDownload/WeeksInDrought.aspx
 * Additional notes about data sources can be found in the chrr_measure_calcs/raw_data/EPHT, chrr_measure_calcs/raw_data/FEMA, and chrr_measure_calcs/raw_data/USDM folders
 *****************************************************************************/

*set mypath to be the root of your local cloned chrr_measure_calcs repository; 
%let mypath = \chrr_measure_calcs;
%let outpath = &mypath.\measure_datasets; 

*check macro variable values to ensure data paths look correct;
%put &mypath;
%put &outpath;

libname out "&outpath."; 
libname inputs "&mypath.\inputs";

/* 10/27/2025 Correction - wrote SAS code to clean disaster declaration raw data. 
Updated old fipscodes for 3 counties that were dropped in the 2025 Annual Release 
calculation. 

2/22/25 Initial calc for the new climate measure (v182). 
Using data from 2019-2023.
Check for county changes between 2019-2023 */

*County dataset (use list of counties from Release 2025 that includes old CT counties). 
All the datasets have old CT counties;

data countyfips25;
	set inputs.county_fips_with_ct_old;
run;
*3152 counties;

/***********************Heat data***********************/

proc import datafile = "&mypath.\raw_data\EPHT\data_171322_heat.csv"
	out = heat_rawdata
	dbms = csv 
	replace;
	getnames = yes;
	guessingrows = 300;
run;

data heat_1;
	set heat_rawdata;
	rename countyfips = fipscode;
	rename statefips = statecode;
	heatdays = input(Value, 8.);
	drop data_comment var8 heat_metric_absolute_threshold value;
run;

proc sort data = heat_1;
	by fipscode;
run;

proc means data = heat_1 noprint;
	by fipscode;
	var heatdays;
	output out = heat_2 sum = v182_heat;
run;

data heat_3;
	set heat_2;
	drop _type_ _freq_;
run;

proc sort data = heat_3;
	by fipscode;
run;

proc sort data = countyfips25;
	by fipscode;
run;

data v182_otherdata_1;
	merge heat_3 countyfips25;
	by fipscode;
run;
*3152 counties;

*Note - heat data is missing for AK and HI. AK and HI counties have a missing v182_heat value, 
other counties with no days above 90F in the 5 year period have a v182_heat value of 0;

/***********************Disaster data***********************/

*Cleaning FEMA disaster summaries data;

proc import datafile = "&mypath.\raw_data\FEMA\DisasterDeclarationsSummaries.csv"
	out = disaster_rawdata
	dbms = csv
	replace;
	getnames = yes;
	guessingrows = 10000;
run;

*put declaration date in character format and extract the year of the declaration;
data disaster_rawdata_1;
	set disaster_rawdata;
	declarationDate_txt = put(declarationDate, B8601DZ20.);
	declarationYear = substr(declarationDate_txt, 1, 4);
	statecode = put(fipsStateCode, z2.);
	countycode = put(fipsCountyCode, z3.);
	fipscode = statecode||countycode;
run;

*examine states and years included;
proc freq data = disaster_rawdata_1; tables statecode; run;
proc freq data = disaster_rawdata_1; tables declarationYear; run;

*create a list of years of disaster declarations to include;
%let yr_ls = '2019', '2020', '2021', '2022', '2023';
%put & = yr_ls;

*retain disasters within time frame of interest and for all states and DC;

data disaster_rawdata_2;
	set disaster_rawdata_1;
	if declarationYear not in (&yr_ls) then delete;
run;

*check that all disasters in included 5 year period were retained;
proc freq data = disaster_rawdata_2; tables declarationYear; run;

*remove US disasters declared in US territories;
data disaster_rawdata_3;
	set disaster_rawdata_2;
	if statecode > 56 then delete;
run;

proc freq data = disaster_rawdata_3; tables statecode; run;

*checking included fipscodes;
proc freq data = disaster_rawdata_3; tables fipscode; run;
proc freq data = disaster_rawdata_3; tables countycode; run;
*there are 749 declarations with a countycode 0f "000" - statewide declarations? data entry error?;

*investigate disasters with a "000" countycode;
data disaster_rawdata_000;
	set disaster_rawdata_3;
	if countycode ne "000" then delete;
run;
*upon visual inspection it looks like all of these disaster declarations were for reservations or Alaskan Native Villages,
will drop all declarations with countycode of "000";

*investigate incident types and titles;
proc freq data = disaster_rawdata_3; tables incidentType; run;
proc freq data = disaster_rawdata_3; tables declarationTitle; run;

*review "Other" incident type to look for non climate or weather related incidents;
data disaster_rawdata_other;
	set disaster_rawdata_3;
	if incidentType ne "Other" then delete;
run;

*Ultimately, we decided to delete two entries from this category- one listed as
'59th Presidential Inauguration’ which seemed to be in relation to the January 6th
terrorist attack, and one listed as 'Explosion' which was a bombing in Tennessee.; 

*review "Biological" incident type;
data disaster_rawdata_biological;
	set disaster_rawdata_3;
	if incidentType ne "Biological" then delete;
run;

proc freq data = disaster_rawdata_biological; tables declarationTitle; run;
*all COVID-19 declarations fall under "Biological";

*remove COVID-19, '59th Presidential Inauguration’, and 'Explosion' declartations;
*drop declarations with countycode of "000";
data disaster_rawdata_4;
	set disaster_rawdata_3;
	if incidentType = "Biological" then delete;
	if declarationTitle = "59TH PRESIDENTIAL INAUGURATION" then delete;
	if declarationTitle = "EXPLOSION" then delete;
	if countycode = "000" then delete;
run;

proc freq data = disaster_rawdata_4; tables incidentType; run;
proc freq data = disaster_rawdata_4; tables declarationTitle; run;

*create table with count of disaster declarations over 5 year period, from 2019-2023, per county;

proc sort data = disaster_rawdata_4; by fipscode; run;

proc freq data = disaster_rawdata_4 noprint;
	tables fipscode / out = disaster_rawdata_clean;
run;

*create statecode and countycode fields, drop frequency field;
data disaster_1;
	set disaster_rawdata_clean;
	statecode = substr(fipscode, 1, 2);
	countycode = substr(fipscode, 3, 3);
	v182_disaster = COUNT;
	drop COUNT PERCENT;
run;
*2259 counties;

*check county fipscodes to see if any need to be updated; 
proc sort data = disaster_1; by fipscode; run;
proc sort data = countyfips25; by fipscode; run;

data disaster_fipscheck;
	merge disaster_1 countyfips25;
	by fipscode;
run;
*3156 counties;

proc sort data = disaster_fipscheck; by county; run;
*there are counties in AK (02270, 02280), SD (46113), and VA (51515) that have disaster
declarations but don't join to our master list of fipscodes - these counties have old 
fipscodes that need to be updated; 

*update old fipscodes in AK, SD, and VA;
*02270 -> 02158;
*46113 -> 46102;
*51515 -> 51019;
*there is one declaration with fipscode 02280 that we couldn't determine the accurate new 
county for - 02280 split into two counties (02275 and 02195) and the declaration location 
name is for a school district that doesn't appear to be in either of the two counties.;

*check to see if data already exists for the current county fipscodes;
data disaster_fipscheck_2;
	set disaster_1;
	if fipscode not in ("02270", "02158", "46113", "46102", "51515", "51019") then delete;
run;

*data already exists for two of the current fipscodes - 02158 and 51019;
*manually editing the v182_disaster value for those two fipscodes;
data disaster_2;
	set disaster_1;
	*AK update;
	if fipscode = "02270" then delete;
	if fipscode = "02158" then v182_disaster = 1 + 1;
	*VA update;
	if fipscode = "51515" then delete;
	if fipscode = "51019" then v182_disaster = 1 + 1;
	*SD update;
	if fipscode = "46113" then fipscode = "46102";
	if fipscode = "46102" then countycode = "102";
	*deleting AK fipscode we couldn't correctly update;
	if fipscode = "02280" then delete;
run;

*check fipscodes one more time; 
proc sort data = disaster_2; by fipscode; run;
proc sort data = countyfips25; by fipscode; run;

data disaster_fipscheck_3;
	merge disaster_2 countyfips25;
	by fipscode;
run;
*3152 counties;

proc sort data = disaster_fipscheck_3; by county; run;

proc sort data = v182_otherdata_1; by fipscode; run;
*3152 counties;
proc sort data = disaster_2; by fipscode; run;
*2256 counties with disaster declarations in time period; 

data v182_otherdata_2;
	merge v182_otherdata_1 disaster_2;
	by statecode countycode fipscode;
	if v182_disaster = . and statecode ne "09" then v182_disaster = 0;
run;

*Note - counties with missing v182_disaster values had 0 disaster declarations in 
the time frame EXCEPT for the new CT counties, which were not included in the FEMA
disaster declarations dataset, which should retain missing v182_disaster values; 

/***********************Drought data***********************/

proc import datafile = "&mypath.\raw_data\USDM\dm_export__20190101_20231231.csv"
	out = drought_rawdata
	dbms = csv
	replace;
	guessingrows = 500;
run;

data drought_1;
	set drought_rawdata;
	fipscode = put(fips, z5.);
	rename NonConsecutiveWeeks = v182_drought;
	drop fips;
	if state = "PR" then delete;
run;

*check county fipscodes;

proc sort data = drought_1; by fipscode; run;
proc sort data = countyfips25; by fipscode; run;

data drought_fipscheck;
	merge drought_1 countyfips25;
	by fipscode;
run;
*3152 counties;

proc sort data = drought_fipscheck; by county; run;

*join to v182_otherdata_2;

proc sort data = drought_1;
	by fipscode;
run;
*3137 counties;

proc sort data = v182_otherdata_2;
	by fipscode;
run;

data v182_otherdata_3;
	merge drought_1 v182_otherdata_2;
	by fipscode;
run;
*3152 counties;

/***********************Full measure***********************/

*Checking which counties are missing for each component measure;

data heat_missing;
	set v182_otherdata_3;
	if v182_heat ne . then delete;
run;
*44 counties missing heat data - AK, HI, and new counties in CT;

data disasters_missing;
	set v182_otherdata_3;
	if v182_disaster ne . then delete;
run;
*9 counties missing disaster data - new counties in CT;

data drought_missing;
	set v182_otherdata_3;;
	if v182_drought ne . then delete;
run;
*15 counties missing drought data - 2 counties in AK, new counties in CT, 2 counties in NY, 1 county in OH, 1 county in PA;

*v182 dataset;

data v182_1;
	set v182_otherdata_3;
	if v182_drought >= 65 then v182_drought_code = 1;
		else if v182_drought = . then v182_drought_code = .;
		else v182_drought_code = 0;
	if v182_heat >= 300 then v182_heat_code = 1;
		else if v182_heat = . then v182_heat_code = .;
		else v182_heat_code = 0;
	if v182_disaster >= 2 then v182_disaster_code = 1;
		else if v182_disaster = . then v182_disaster_code = .;
		else v182_disaster_code = 0;
run;

*Leaving climate measure component codes missing if the underlying data is 
missing for a county;

data v182_2;
	set v182_1;
	v182_rawvalue = sum(v182_drought_code, v182_heat_code, v182_disaster_code);
run;

proc freq data = v182_2;
	tables v182_rawvalue;
run;

data v182_3;
	set v182_2;
	if v182_drought = . and v182_heat = . and v182_disaster = . then v182_flag_CT = "U";
	if statecode = "09" and v182_flag_CT ne "U" then v182_flag_CT = "A";
run;

data v182_final;
	set v182_3;
	v182_numerator = .;
	v182_denominator = .;
	v182_cilow = .;
	v182_cihigh = .;
	problem = .;
	drop county state v182_drought v182_heat v182_disaster v182_drought_code v182_heat_code v182_disaster_code;
run;

*v182_otherdata datset;

data v182_otherdata_final;
	set  v182_otherdata_3;
	by fipscode;
	if v182_drought = . and v182_heat = . and v182_disaster = . then v182_otherdata_flag_CT = "U";
	if statecode = "09" and v182_otherdata_flag_CT ne "U" then v182_otherdata_flag_CT = "A";
	drop state county fipscode;
run;

/* Save in measure_datasets folder (create new outpath if you want to save dataset in 
a different folder on your local machine, otherwise this will overwrite what's saved in 
measure_datasets already) */

data out.v182_s2025;
	set v182_final;
run;

data out.v182_otherdata_s2025;
	set v182_otherdata_final;
run;
