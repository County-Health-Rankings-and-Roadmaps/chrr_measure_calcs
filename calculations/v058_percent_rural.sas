
/* Measure name: v058 % Rural demographic measure
/* Author initials: JH
/* Data source: 2020 Deccenial Census Demographic Housing Characteristics Table P2. For Connecticut, download Table P2 census tract level data.
/* Data download link: 
/* Numerator: The numerator is the number of people living in areas classified as rural by the Census Bureau - that is, all territory not included in an urban area. Urban areas are densely developed territory that have at least 2,000 housing units or a population of at least 5,000.
/* Denominator: The denominator is the 2020 decinnial census county population.
/* Notes: If downloading the data directly from the Census delete the second row of headers before importing to SAS. Connecticut uses a separate census tract level dataset and a crosswalk to crosswalk the old census tracts to their new codes (to match new Connecticut county equivalents). 
*/

*set mypath to be the root of your local cloned chrr_measure_calcs repository; 
%let mypath = ;
%let outpath = &mypath.\measure_datasets; 
libname out "&outpath."; 
libname inputs "&mypath.\inputs";

*check macro variable values to ensure data paths look correct;
%put &mypath;
%put &outpath;

proc import 
	out = rural_data
	datafile = "&mypath.\raw_data\USCB\DECENNIALDHC2020.P2-Data_clean.csv"
	dbms = csv;
	getnames = yes;
run;

data rural_data_2;
	set rural_data;
	length fipscode $5;
	length statecode $2;
	length countycode $3;
	fipscode = substr(geo_id, 10);
	statecode = substr(fipscode, 1, 2);
	countycode = substr(fipscode, 3);
	drop geo_id name P2_001NA P2_002NA P2_003NA P2_004N P2_004NA;
run;

*Calculating measure and dropping counties in Puerto Rico.
*Setting CT county values to missing. For the 2025 Release, only data for CT's new county equivalents (planning regions) was included. 
*The CT county equivalent data will be added in later. 
	P2_001N = Total
	P2_002N = Total Urban
	P2_003N = Total Rural; 

data rural_data_3;
	set rural_data_2;
		v058_denominator = P2_001N;
	v058_numerator = P2_003N;
	v058_rawvalue = v058_numerator / v058_denominator;
	if statecode = "72" then delete;
	if statecode = "09" then v058_denominator = .;
	if statecode = "09" then v058_numerator = .;
	if statecode = "09" then v058_rawvalue = .;
	if statecode = "09" then v058_flag_CT = "U";
	*A "U" flag indicates which CT data is unavailable in the CHR&R dataset;
	drop P2_001N P2_002N P2_003N;
run;

*Using Table P2 census tract data to calculate county values for new CT county equivalents (planning regions);

*Bring in census tract crosswalk;
data tract_cross;
	set inputs.ct_tract_crosswalk;
run;

*Cleaning Census data;
proc import 
	out = CT_rural_data
	datafile = "&mypath.\raw_data\USCB\DECENNIALDHC2020.P2-Data_clean_CT.csv"
	dbms = csv;
	getnames = yes;
run;

data CT_rural_data_2;
	set CT_rural_data;
	length fipscode $5;
	length statecode $2;
	length countycode $3;
	length tract_fipscode_2020 $11;
	fipscode = substr(geo_id, 10);
	statecode = substr(fipscode, 1, 2);
	countycode = substr(fipscode, 3);
	tract_fipscode_2020 = substr(geo_id, 10, 20);
	drop geo_id name P2_004N;
run;

*	P2_001N = Total
	P2_002N = Total Urban
	P2_003N = Total Rural; 

data CT_rural_data_3;
	set CT_rural_data_2;
	v058_denominator = P2_001N;
	v058_numerator = P2_003N;
	drop P2_001N P2_002N P2_003N;
run;

*crosswalking to updated census tracts;
proc sort data = CT_rural_data_3;
	by tract_fipscode_2020;
run;
*883 tracts;

proc sort data = tract_cross;
	by tract_fipscode_2020;
run;
*879 tracts - tracts with 0 population were not included in crosswalk file;

data CT_rural_data_4;
	merge CT_rural_data_3 tract_cross;
	by tract_fipscode_2020;
run;

*checking that all the tracts without a 2022 tract fipscode have 0 population, view the CT_rural_data_4 dataset after sorting;
proc sort data = CT_rural_data_4;
	by tract_fipscode_2022;
run;

*all tracts without 2022 tract fipscodes have 0 population, dropping them;
data CT_rural_data_5;
	set CT_rural_data_4;
	if tract_fipscode_2022 = . then delete;
run;
*879 tracts;

*aggregating to the county level with the 2022 county (equivalent) fipscodes;
proc means data = CT_rural_data_5 noprint;
	class county_fipscode_2022;
	var v058_numerator;
	output out = v058_numerator_county sum = v058_numerator;
run;

data v058_numerator_county_2;
	set v058_numerator_county;
	if _TYPE_ = 0 then delete;
	drop _TYPE_ _FREQ_;
run;

proc means data = CT_rural_data_5 noprint;
	class county_fipscode_2022;
	var v058_denominator;
	output out = v058_denominator_county sum = v058_denominator;
run;

data v058_denominator_county_2;
	set v058_denominator_county;
	if _TYPE_ = 0 then delete;
	drop _TYPE_ _FREQ_;
run;

*Calculating measure for new CT county equivalents;
proc sort data = v058_numerator_county_2;
	by county_fipscode_2022;
run;

proc sort data = v058_denominator_county_2;
	by county_fipscode_2022;
run;

data CT_rural_data_6;
	merge v058_numerator_county_2 v058_denominator_county_2;
	by county_fipscode_2022;
	rename county_fipscode_2022 = fipscode;
run;

data CT_rural_data_7;
	set CT_rural_data_6;
	length statecode $2;
	length countycode $3;
	statecode = substr(fipscode, 1, 2);
	countycode = substr(fipscode, 3);
	v058_rawvalue = v058_numerator/v058_denominator;
	v058_flag_CT = "A";
	*An "A" flag indicates which CT data is available in the CHR&R dataset;;
run;

*Incorporating new CT county equivalent values into national measure dataset;

proc sort data = CT_rural_data_7;
	by fipscode;
run;

proc sort data = rural_data_3;
	by fipscode;
run;

data rural_data_4;
	merge CT_rural_data_7 rural_data_3;
	by fipscode;
run;
*3,152 observations;

*Calculating the state and national values;

proc means data = rural_data_4 sum noprint;
	class statecode;
	var v058_numerator;
	output out = v058_statenum sum = v058_numerator;
run;

data v058_statenum_2;
	set v058_statenum;
	if _type_ = 0 then statecode = "00";
	countycode = "000";
	fipscode = statecode||countycode;
	drop _type_ _freq_;
run;

proc means data = rural_data_4 sum noprint;
	class statecode;
	var v058_denominator;
	output out = v058_stateden sum = v058_denominator;
run;

data v058_stateden_2;
	set v058_stateden;
	if _type_ = 0 then statecode = "00";
	countycode = "000";
	fipscode = statecode||countycode;
	drop _type_ _freq_;
run;

proc sort data = v058_statenum_2;
	by fipscode;
run;

proc sort data = v058_stateden_2;
	by fipscode;
run;

data v058_state_nat;
	merge v058_statenum_2 v058_stateden_2;
	by fipscode;
	v058_rawvalue = v058_numerator / v058_denominator;
run;

*Creating final dataset with county, state and national values;

proc sort data = rural_data_4;
	by fipscode;
run;

proc sort data = v058_state_nat;
	by fipscode;
run;

data v058;
	merge rural_data_4 v058_state_nat;
	by statecode countycode fipscode;
run; 
*3,204 observations;

*Double check that all counties and state and national values are included by comparing against the county_fips_with_ct_old and state_fips datasets in the inputs folder;
*The county_fips_with_ct_old dataset includes all the counties that were included in the 2025 Annual Release. It includes both the 8 old counties and the 9 new county equivalents (planning regions) in Connecticut;
*No data was included for the old CT counties in the final v058 dataset, but they are retained to indicate we didn't include data for them; 

data countyfips;
	set inputs.county_fips_with_ct_old;
run;

data statefips;
	set inputs.state_fips;
run;

data fips;
	set countyfips statefips;
run;

proc sort data = fips;
	by fipscode;
run;

proc sort data = v058;
	by fipscode;
run;

data v058_fipscheck;
	merge v058 fips;
	by statecode countycode fipscode;
run;
*3,204 observations read from each dataset and 3,204 observations in the merged dataset - all fips included;

data v058_final;
	set v058;
	problem = .;
	v058_cilow = .;
	v058_cihigh = .;
run;

*Saving final dataset;

data out.v058_s2025;
	set v058_final;
run;

