/*****************************************************************************
 * v088 - Dentists
 * Author: JH
 * Description: Ratio of population to dentists.
 * Data Source: Area Health Resource File/National Provider Identifier Downloadable File
 * Data Download Link: https://data.hrsa.gov/data/download
 * Numerator: The left side of the ratio represents the county population.
 * Denominator: The right side of the ratio represents the dentists corresponding to county population. Registered dentists with a National Provider Identifier are counted.
 *****************************************************************************/

/* v088 is the Dentists measure. This measure was not updated for the CHR&R 2025 Annual Release, 
but was updated in the post 2025 release rolling CHR&R updates in the Fall of 2025. This code uses the 2024-2025
release of Area Health Resources File data, which includes provider data through 2023. The output dataset is 
saved as v088_s2026 in measure_datasets to reflect that it was not calculated for the 2025 annual release. 

NOTE that this measure was not updated for Connecticut with the 2024-2025 release of the Area Health Resources File. 
The AHRF contains data for the eight former CT counties but there is no recent population data available to use as a 
denominator for those counties for this measure. Additionally, CHR&R decided not to update CT data for any measures in
the rolling 2025 data updates that took place after the 2025 Annual Release due to difficulties created by the geography changes. */

*set mypath to be the root of your local cloned chrr_measure_calcs repository; 
%let mypath = \chrr_measure_calcs;
%let outpath = &mypath.\measure_datasets; 
libname out "&outpath."; 
libname inputs "&mypath.\inputs";

*check macro variable values to ensure data paths look correct;
%put &mypath;
%put &outpath;

*create library to access AHRF raw data;
libname ahrf "&mypath.\raw_data\AHRF";

data dentists_county_1;
	set ahrf.ahrf2024_feb2025;
run; 

/************ Creating numerator tables - the number of dentists in each county, state and the nation ************/

/* field dent_npi_23 = Year "2023", "Variable Name "Dentists w/NPI" and Characteristics "With CMS NPI; See User Doc"
aka the number of dentists in a county with an NPI ID number;
see the bottom of the AHRF 2023-2024 Technical DocumentationCSV_Feb2025 for the field name key */

/* keeping the Header - FIPS, State Name, State Name Abbreviation, County Name, 
FIPS State Code, FIPS County Code, and dent_npi_23 and changing names of fields */

data dentists_county_2;
	set dentists_county_1 (keep = fips_st_cnty st_name st_name_abbrev cnty_name fips_st fips_cnty dent_npi_23);
	rename dent_npi_23 = dentists;
	rename fips_st = statecode;
	rename fips_cnty = countycode;
	rename fips_st_cnty = fipscode;
run;
*3,240 observations; 

/* Check to see what counties/county equivalents are included in Connecticut (CT) */

data dentists_county_CT;
	set dentists_county_2;
	if statecode ne 09 then delete;
run;
*Records for both the 8 old CT counties and 9 new CT planning districts are included
but there is only data available for old CT counties.;

/* Removing counties in Puerto Rico and US terriroties. Data for CT counties will be dropped 
after the national value for dentists is calculated. 
Merging with master list of fips codes to check for old, incorrect, or missing fips codes */

data dentists_county_3;
	set dentists_county_2;
	if statecode > 56 then delete;
run;
*3,158 observations;

data fips;
	set inputs.county_fips_with_ct_old;
	*this dataset includes fipscodes for both sets of CT counties;
	*values for all CT geographies will be set to missing later in the code to indicate to users we didn't update data for CT geographies;
run;
*3,152 observations;

proc sort data = dentists_county_3;
	by statecode countycode fipscode;
run;

proc sort data = fips;
	by statecode countycode fipscode;
run;

data fips_check_county;
	merge dentists_county_3 fips;
	by statecode countycode fipscode;
run;

/* sort data by dentists, county, etc. to see which counties in the raw data for dentists don't match counties
in the master list of fipscodes */

proc sort data = fips_check_county;
	by dentists;
run;

proc sort data = fips_check_county;
	by county;
run;

/* Counties below in AK and VA have missing values in the dentists dataset AND aren't listed in our master list of counties. 
These are counties that no longer exist due to name changes or being combined with other counties. See the NOTE at the bottom of the 
AHRF 2023-2024 Technical DocumentationCSV_Feb2025 and this webpage from the Census 
for more details: https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes.2010.html#list-tab-957819518

Delete the old/incorrect counties (but retain CT counties with missing data). */

data dentists_county_4;
	set dentists_county_3;
	if fipscode = "02201" then delete;
	if fipscode = "02232" then delete;
	if fipscode = "02261" then delete;
	if fipscode = "02280" then delete; 
	if fipscode = "51515" then delete; 
	if fipscode = "51560" then delete; 
run;

/* check log to see number of counties remaining - 
3,152, this is the correct number when old and new CT counties are retained */

proc sort data = dentists_county_4;
	by fipscode;
run;

/* creating a table of the sum of dentists in each state */

proc means data = dentists_county_4 noprint;
	by statecode; 
	var dentists;
	output out = dentists_state_1 sum=;
run; 

/* removing the TYPE and FREQ columns and 
adding columns for countycode and fipscode;
set CT state value to missing since CT data won't be updated */

data dentists_state_2 (drop = _TYPE_ _FREQ_);	
	set dentists_state_1;
	countycode = "000";
	if statecode = "09" then dentists = .;
	fipscode = statecode || countycode;
run;

/* creating a table with a national sum of dentists */

proc means data = dentists_county_4 noprint; 
	var dentists;
	output out = dentists_national_1 sum=;
run;

/* removing the TYPE and FREQ columns and 
adding columns for statecode, countycode, and fipscode */

data dentists_national_2 (drop = _TYPE_ _FREQ_);
	set dentists_national_1;
	statecode = "00";
	countycode = "000";
	fipscode = statecode || countycode;
run;

/* create a table of county dentists estimates with only fields for statecode, 
countycode, fipscode and dentists. Drop county dentists values for CT. */

data dentists_county_5;
	set dentists_county_4 (keep = statecode countycode fipscode dentists);
	if statecode = "09" then dentists = .;
run;

/* combine dentists_county_5, dentists_state_2, and dentists_national_2 tables */

data v088_numerator;
	set dentists_county_5 dentists_state_2 dentists_national_2;
	rename dentists = v088_numerator;
run;

/* remove labels */

proc datasets lib = work noprint;
  modify v088_numerator;
  attrib _all_ label = '';
run;

/************ Creating denominator tables - population estimates ************/

/* creating a table with 2023 county pop estimates */

data pop2023_county_1;
	set inputs.vintage2023;
	if countycode = "000" then delete;
	fipscode = statecode || countycode;
run;
/* 3,144 counties in pop2023_county_1 */

/* creating a table with just the fipscodes and the 2023 county pop estimates. */

data pop2023_county_2;
	set pop2023_county_1;
	pop_est = POPESTIMATE2023;
	keep statecode countycode fipscode pop_est;
run;
*3,144 counties, only new CT counties are included in Vintage 2023 population estimates;

/* CT county data will be dropped after a national population is calculated to be used 
as the denominator for the national dentists measure value */

/* creating a table of the sum of population in each state */

proc means data = pop2023_county_2 noprint;
	by statecode; 
	var pop_est;
	output out = pop2023_state_1 sum=;
run; 

/* removing the TYPE and FREQ columns and adding columns for countycode and fipscode;
set CT state value to missing since CT data won't be updated */

data pop2023_state_2 (drop = _TYPE_ _FREQ_);	
	set pop2023_state_1;
	countycode = "000";
	if statecode = "09" then pop_est = .;
	fipscode = statecode || countycode;
run;

/* creating a table with a national sum of population based 
on a sum of county populations */

proc means data = pop2023_county_2 noprint;
	var pop_est; 
	output out = pop2023_national sum=;
run;

/* removing the TYPE and FREQ columns and 
adding columns for statecode, countycode, and fipscode */

data pop2023_national_2 (drop = _TYPE_ _FREQ_);
	set pop2023_national;
	statecode = "00";
	countycode = "000";
	fipscode = statecode || countycode;
run;

/* combine pop2023_county_2, pop2023_state_2, and pop2023_national_2 tables
into one table with population estimates that will be used as the dentists denominator */

data v088_denominator;
	set pop2023_county_2 pop2023_state_2 pop2023_national_2;
	if statecode = "09" then pop_est = .;
	rename pop_est = v088_denominator;
run;

proc sort data = v088_denominator;
	by fipscode;
run;

/************ Combine numerator and denominator tables and calculate measure ************/

/* merge v088_numerator and v088_denominator, should be 3,204 records */

proc sort data = v088_numerator; 
	by statecode countycode;
run; 

proc sort data = v088_denominator;
	by statecode countycode;
run; 

data v088_1;
	merge v088_numerator v088_denominator; 
	by statecode countycode;
run; 

proc means data = v088_1 nmiss n; 
run; 
*18 missing values - this matches what should be missing for CT - 1 state value, 
8 values for old CT counties, 9 values for new CT counties/planning regions;

/* calculating the measure with supression criteria. 
v088_rawalternatevalue is the ratio (this measure is displayed as a ratio on our website).
If a county pop is greater than 4000 and has 0 dentists, the county's v088 value is set to missing.
Becuase of errors with division by 0 for the rawalternatevalue, if a county pop is less than 4000 and 
has 0 dentists, the rawalternatevalue is missing. On the website, the measure value will display as 
a ratio that looks like denominator:0. */

data v088_2;
	set v088_1;
	v088_rawvalue = v088_numerator/v088_denominator;
	v088_rawalternatevalue = v088_denominator/v088_numerator;
	v088_cilow = .;
	v088_cihigh = .;
	v088_sourceflag = .;
	if v088_numerator = 0 and v088_denominator > 4000 
		then v088_rawvalue = .;
		*rawalternatevalue will be missing as well becuase of division by 0 error;
run;

/* final formatting of v088 */

data v088_final (drop = fipscode);
	set v088_2;
	if statecode = "09" then v088_flag_CT = "U";
	problem = .;
run;

/* save in measure_datasets folder (create new outpath if you want to save dataset in 
a different folder on your local machine, otherwise this will overwrite what's saved in 
measure_datasets already) */

data out.v088_s2026;
	set v088_final;
run;



