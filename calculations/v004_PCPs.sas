/*****************************************************************************
 * v004 - Primary Care Physicians
 * Author: JH
 * Description: Ratio of population to primary care physicians.
 * Data Source: Area Health Resource File/American Medical Association
 * Data Download Link: https://data.hrsa.gov/data/download
 * Numerator: The left side of the ratio represents the county population.
 * Denominator: The right side of the ratio represents the primary care physicians corresponding to county population. Primary care physicians include practicing non-federal physicians (M.D.s and D.O.s) under age 75 specializing in general practice medicine, family medicine, internal medicine, and pediatrics.
 *****************************************************************************/

/* v004 is the Primary Care Physicians measure. This measure was not updated for the CHR&R 2025 Annual Release, 
but was updated in the post 2025 release rolling CHR&R updates in the Fall of 2025. This code uses the 2024-2025
release of Area Health Resources File data, which includes provider data through 2023. The output dataset is 
saved as v004_s2026 in measure_datasets to reflect that it was not calculated for the 2025 annual release. 

NOTE that this measure was not updated for Connecticut with the 2024-2025 release of the AHRF. The AHRF contains 
data for the eight former CT counties but there is no recent population data available to use as a denominator for 
those counties for this measure. Additionally, CHR&R decided not to update CT data for any measures in the rolling 
2025 data updates that took place after the 2025 Annual Release due to difficulties created by the geography changes. */

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

data pcp_county_1;
	set ahrf.ahrf2024_feb2025;
run; 

/************ Creating numerator tables - the number of primary care providers in each county, state and the nation ************/

/* field phys_nf_prim_care_pc_exc_rsdt_22 = Year "2022", Variable Name "Phys,Primary Care, Patient Care" and Characteristics "Non-Fed;Excl Hsp Res & 75+ Yrs"
aka the number of primary care physicians in a county;
see the bottom of the AHRF 2023-2024 Technical DocumentationCSV_Feb2025 for the field name key */

/* keeping the Header - FIPS, State Name, State Name Abbreviation, County Name, 
FIPS State Code, FIPS County Code, and phys_nf_prim_care_pc_exc_rsdt_22 and changing names of fields */ 

data pcp_county_2;
	set pcp_county_1 (keep = fips_st_cnty st_name st_name_abbrev cnty_name fips_st fips_cnty phys_nf_prim_care_pc_exc_rsdt_22);
	rename phys_nf_prim_care_pc_exc_rsdt_22 = pcp;
	rename fips_st = statecode;
	rename fips_cnty = countycode;
	rename fips_st_cnty = fipscode;
run; 
*3,240 observations; 

/* Check to see what counties/county equivalents are included in Connecticut (CT) */

data pcp_county_CT;
	set pcp_county_2;
	if statecode ne 09 then delete;
run;
*Records for both the 8 old CT counties and 9 new CT planning districts are included
but there is only data available for old CT counties.;
                                                                                                                                 
/* Removing counties in Puerto Rico and US terriroties. Data for CT counties will be dropped 
after the national value for dentists is calculated. 
Merging with master list of fips codes to check for old, incorrect, or missing fips codes */

data pcp_county_3;
	set pcp_county_2;
	if statecode > 56 then delete;
run;
*3,158 observations;

data fips;
	set inputs.county_fips_with_ct_old;
	*this dataset includes fipscodes for both sets of CT counties;
	*values for all CT geographies will be set to missing later in the code to indicate to users we didn't update data for CT geographies;
run;
*3,152 observations;

proc sort data = pcp_county_3;
	by statecode countycode fipscode;
run;

proc sort data = fips;
	by statecode countycode fipscode;
run;

data fips_check_county;
	merge pcp_county_3 fips;
	by statecode countycode fipscode;
run;

/* Sort data by "pcp" and "county" to see which counties in the raw data for primary care physicians 
don't match counties in the master list of fipscodes */

proc sort data = fips_check_county;
	by pcp;
run;

proc sort data = fips_check_county;
	by county;
run;
 
/* Counties below in AK and VA have missing values in the pcp dataset AND aren't listed in our master list of counties. 
These are counties that no longer exist due to name changes or being combined with other counties. See the NOTE at the bottom of the 
AHRF 2023-2024 Technical DocumentationCSV_Feb2025 and this webpage from the Census 
for more details: https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes.2010.html#list-tab-957819518

Delete the old/incorrect counties (but retain CT counties with missing data). */

data pcp_county_4;
	set pcp_county_3;
	if fipscode = "02201" then delete;
	if fipscode = "02232" then delete;
	if fipscode = "02261" then delete; 
	if fipscode = "02280" then delete; 
	if fipscode = "51515" then delete; 
	if fipscode = "51560" then delete; 
run;

/* check log to see number of counties remaining - 
3,152, this is the correct number when old and new CT counties are retained */

proc sort data = pcp_county_4;
	by fipscode;
run;

/* creating a table of the sum of pcp in each state */

proc means data = pcp_county_4 noprint;
	by statecode; 
	var pcp;
	output out = pcp_state_1 sum=;
run; 

/* removing the TYPE and FREQ columns from and 
adding columns for countycode and fipscode.
set CT state value to missing since CT data won't be updated */

data pcp_state_2 (drop = _TYPE_ _FREQ_);	
	set pcp_state_1;
	countycode = "000";
	if statecode = "09" then pcp = .;
	fipscode = statecode || countycode;
run;

/* creating a table with a national sum of pcp */

proc means data = pcp_county_4 noprint; 
	var pcp;
	output out = pcp_national_1 sum=;
run;

/* removing the TYPE and FREQ columns from and 
adding columns for statecode, countycode, and fipscode */

data pcp_national_2 (drop = _TYPE_ _FREQ_);
	set pcp_national_1;
	statecode = "00";
	countycode = "000";
	fipscode = statecode || countycode;
run;

/* create a table of county pcp estimates with only fields for statecode, 
countycode, fipscode and pcp. Drop county pcp values for CT. */

data pcp_county_5;
	set pcp_county_4 (keep = statecode countycode fipscode pcp);
	if statecode = "09" then pcp = .;
run;

/* combine pcp_county_5, pcp_state_2, and pcp_national_2 tables */

data v004_numerator;
	set pcp_county_5 pcp_state_2 pcp_national_2;
	rename pcp = v004_numerator;
run;

/* remove labels */

proc datasets lib = work noprint;
  modify v004_numerator;
  attrib _all_ label = '';
run;

/************ Creating denominator tables - population estimates ************/

/* creating a table with 2022 county pop estimates. */

data pop2022_county_1;
	set inputs.vintage2022_with_ct_new;
	if countycode = "000" then delete;
	fipscode = statecode || countycode;
run;
/* 3,144 counties in pop2022_county */

/* creating a table with just the fipscodes and the 2022 county pop estimate */

data pop2022_county_2;
	set pop2022_county_1;
	pop_est = POPESTIMATE2022;
	keep statecode countycode fipscode pop_est;
run;
*3,144 counties, only new CT counties are included in Vintage 2022 population estimates;

/* CT county data will be dropped after the national population is calculated 
to be used as the denominator for the national dentists measure value */

/* creating a table of the sum of population in each state */

proc means data = pop2022_county_2 noprint;
	by statecode;
	var pop_est;
	output out = pop2022_state_1 sum=;
run;

/* removing the TYPE and FREQ columns and adding columns for countycode and fipscode;
set CT state value to missing since CT data won't be updated */

data pop2022_state_2 (drop = _TYPE_ _FREQ_);
	set pop2022_state_1;
	countycode = "000";
	if statecode = "09" then pop_est = .;
	fipscode = statecode || countycode;
run;

/* creating a table with a national sum of population based 
on a sum of county populations */

proc means data = pop2022_county_2 noprint;
	var pop_est; 
	output out = pop2022_national sum=;
run;

/* removing the TYPE and FREQ columns and 
adding columns for statecode, countycode, and fipscode */

data pop2022_national_2 (drop = _TYPE_ _FREQ_);
	set pop2022_national;
	statecode = "00";
	countycode = "000";
	fipscode = statecode || countycode;
run;

/* combine pop2022_county_2, pop2022_state_2, and pop_2022_national_2 tables
into one table with population estimates that will be used as the primary care providers denominator*/

data v004_denominator;
	set pop2022_national_2 pop2022_state_2 pop2022_county_2;
	if statecode = "09" then pop_est = .;
	rename pop_est = v004_denominator;
run;

proc sort data = v004_denominator;
	by fipscode;
run;

/************ Combine numerator and denominator tables and calculate measure ************/

/* merge v004_numerator and v004_denominator, should be 3,204 records */

proc sort data = v004_numerator; 
	by statecode countycode;
run; 

proc sort data = v004_denominator;
	by statecode countycode;
run; 

data v004_1;
	merge v004_numerator v004_denominator; 
	by statecode countycode;
run; 

proc means data = v004_1 nmiss n; 
run; 
*18 missing values - this matches what should be missing for CT - 1 state value, 
8 values for old CT counties, 9 values for new CT counties/planning regions;

/* calculating the measure with supression criteria. 
v004_rawalternatevalue is the ratio (this measure is displayed as a ratio on our website).
If a county has a population greater than 2,000 and 0 primary care providers, the countys v004
value is set to missing. Becuase of errors with division by 0 for the rawalternatevalue, if a county pop 
is less than 2000 and has 0 primary care providers, the rawalternatevalue is missing. On the website, the
measure value will display as a ratio that looks like denominator:0. */

data v004_2;
	set v004_1;
	v004_rawvalue = v004_numerator/v004_denominator;
	v004_rawalternatevalue = v004_denominator/v004_numerator;
	v004_cilow = .;
	v004_cihigh = .;
	v004_sourceflag = .;
	if v004_numerator = 0 and v004_denominator > 2000 
		then v004_rawvalue = .;
		*rawalternatevalue will be missing as well becuase of division by 0 error;
		*counties with 0 registered pcp have a numerator of 0;
run;

/* final formatting of v004 */

data v004 (drop = fipscode);
	set v004_2;
	if statecode = "09" then v004_flag_CT = "U";
	problem = .;
run;

/* save in measure_datasets folder (create new outpath if you want to save dataset in 
a different folder on your local machine, otherwise this will overwrite what's saved in 
measure_datasets already) */

data out.v004_s2026;
	set v004;
run;





