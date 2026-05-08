/*v169 School Funding Adequacy*/

/*Author: SN*/

/*Description: The average gap in dollars between actual and required spending per pupil among public school districts. Required spending is an estimate of dollars needed to achieve U.S. average test scores in each district.*/

/*Numerator: N/A*/
/*Denominator: N/A*/

/*Data sources:

-	State (StateIndicatorsDatabase_2025.xlsx) and district-level (DistrictCostDatabase_2025.xlsx) data are available and can be downloaded from this link: https://www.schoolfinancedata.org/download-data/
    For the Data Release 2025, SFID was late in publicly releasing the data; we received the raw data via email, however the data are now publicly accessible at the link above. 
	Note some discrepancies were found between the data we received via email and the data available from schoolfinanacedata.com as of March 2025. 

-	NCES school data, leauniverse2021_2022.sas7bdat, is available in the inputs folder: chrr_measure_calcs/inputs/leasuniverse2021_2022.sas7bdat 

-	STATE FIPS data, state_fips.sas7bdat, is available in the inputs folder: chrr_measure_calcs/inputs/state_fips.sas7bdat

-	COUNTY FIPS WITH CONNECTICUT OLD, county_fips_with_ct_old.sas7bdat, is available in the inputs folder: chrr_measure_calcs/inputs/county_fips_with_ct_old.sas7bdat 

/*Note: Counties 53013 and 36071 were suppressed due to significant changes from the previous year identified during verification.*/


/*CLEAN THE RAW DATASETS*/

/*DISTRICT-LEVEL DATASET*/

/*
1.	Clean the dataset by only including the following columns:
    ?	leaid (District ID)
    ?	year
    ?	predcost
    ?	ppcstot (note: the requested data sent via email has a column called ?spend?. We need to rename the column to ?ppcstot?. 

2.	Subset the data with year=2022 (note: the requested data sent via email only has 2022 data)

3.	Calculate the funding gap for each district: fundinggap=ppcstotpredcost 

4.	Import the cleaned v169_raw_district.xlsx to sas and save as ?v169_raw_district.sas7bdat?*/


/*STATE-LEVEL DATASET*/
/*
1. Clean the dataset by only including the following columns:

   ?	stabbr (State abbreviation)
   ?	state_name (State name)
   ?	necm_predcost_state 
   ?	necm_ppcstot_state
   ?	necm_fundinggap_state

2. Subset the data with year=2022

3. Import the cleaned v169_raw_state.xlsx to sas and save as ?v169_raw_state.sas7bdat?*/


/*Some set up */

*set mypath to be the root of your local repository ; 
%let mypath = C:\Users\holsonwillia\Documents\chrr_measure_calcs;
%let outpath = &mypath.\measure_datasets; 
libname out "&outpath."; 
libname inputs "&mypath.\inputs"; 

*check macro variable values to ensure data paths look correct; 
%put &mypath; 
%put &outpath; 

/*calculate the district-level data*/

/*Import district level data from SFID folder */

/*use the "data" sheet of the xlsx file*/

proc import 
out = one
datafile = "&mypath.\raw_data\SFID\SFIDestimates_request.xlsx"
dbms = xlsx;
getnames = yes;
sheet = "District"; 
run; 


proc sort; by leaid;
where year = 2022; 
run;
/* 12666 observations*/


/*using the leauniverse data (leauniverse2021_2022.sas7bdat) to add statecode*/

/*Import leauniverse2021_2022.sas7bdat from inputs folder */

data two; set inputs.leauniverse2021_2022;
proc sort; by agency_id_nces_assigned_district;
run;
/*19409 observations*/


/*rename agency_id_nces_assigned_district as leaid*/

data three; set two;
rename agency_id_nces_assigned_district = leaid;
proc sort; by leaid;
run;


data four;
merge one (in=in1) three;
by leaid;
if in1;
run;


/* include leading zero for county_number*/

data five; set four;
fipscode = put(county_number, z5.);
run;

data six; set five;
proc sort; by fipscode;
run;
/*12666 observations*/

/*remove fipscode 36071 and 53013*/

data six_a; set six;
if fipscode = '36071' then delete;
if fipscode = '53013' then delete;
ppcstot_num = input(ppcstot, best12.);
fundinggap_num = input(fundinggap, best12.);
run;
/*12647 observations?19 observations removed*/

/*Aggregate district-level data to county level*/
/*Group by county*/

proc sql; create table seven as
select fipscode
, count (*) as district_freq
, sum (ppcstot) as ppcstot_cty
, sum (fundinggap) as fundinggap_cty
from six_a
group by fipscode;
quit;
/*3053 observations*/
*3035 observation; 

/*calculate v169_rawvalue and v169_other_data_1*/

data eight; set seven;
v169_other_data_1 = ppcstot_cty/district_freq;
v169_rawvalue = fundinggap_cty/district_freq;
run;
/*3053 observations*/



/*calculate the state-level data*/

/*Import state level data from SFID folder */

/*use the "data" sheet of the xlsx file*/

proc import 
out = nine
datafile = "&mypath.\raw_data\SFID\SFIDestimates_request.xlsx"
dbms = xlsx;
getnames = yes;
sheet = "State"; 
run; 


data ten; set nine;
rename necm_ppcstot_state = v169_other_data_1;
label necm_ppcstot_state = 'v169_other_data_1';
rename necm_fundinggap_state = v169_rawvalue;
label necm_fundinggap_state = 'v169_rawvalue';
run;
/*51 observ*/


/*use state_fips.sas7bdat from inputs folder to get statecode, countycode, and fipscode*/


data eleven; set inputs.state_fips;
run;

data twelve; set eleven;
rename state = stabbr;
label state = 'stabbr';
run;

/*left join ten and twelve*/

data ten; set ten;
proc sort; by stabbr;
run;

data thirteen;
merge ten twelve;
by stabbr;
run;
/*52 obsrv*/

/*continue using the raw district data (data one) by including the suppressed values*/

data fourteen; set one;
fundinggap = spend - predcost; run; 

proc sort; by fundinggap;
run;

/*find the national value that is the median of school district-level funding gap values*/

proc means mean median data = fourteen; var fundinggap;
run;

data fifteen; set thirteen;
if stabbr = 'US' then v169_rawvalue = 1411.08;
run;

/*sort the state data by fipscode*/

data fifteen; set fifteen;
proc sort; by fipscode;
run;

/*merge eight (district data) and fifteen (state data) to combine all datasets*/

data sixteen; merge fifteen eight;
by fipscode;
run;
/*3105 observ*/

/*clean the complete data*/

data seventeen; set sixteen;
keep fipscode v169_rawvalue v169_other_data_1;
run;

/*create statecode countycode*/

data eighteen; set seventeen;
statecode = substr (fipscode, 1, 2);
countycode = substr (fipscode, 3, 3);
run;
 /*3105 observations*/

/*Add the following standard columns, all set to missing:
?	v169_numerator
?	v169_denominator
?	v169_cilow
?	v169_cihigh
?	v169_sourceflag*/

data nineteen; set eighteen;
v169_numerator = .;
v169_denominator = .;
v169_cilow = .;
v169_cihigh = .;
v169_sourceflag = .;
run;

/*standardize dataset with master FIPS codes*/

/*use county_fips_with_ct_old.sas7bdat data to get old Connecticut counties
Ensure the dataset contains exactly 3,204 rows*/

/*Import county_fips_with_ct_old.sas7bdat*/


data twenty; 
set	inputs.county_fips_with_CT_old
	inputs.state_fips; 
keep statecode countycode fipscode;
proc sort; by statecode countycode;
run;
/*3204 observations*/

data twentyone;
merge nineteen twenty;
by fipscode;
run;
/*3204 observations*/

/*Add v169_flag_CT to indicate data availability in Connecticut counties*/

/*Set to 'A' (Available) for old CT counties.
Set to 'U' (Unavailable) for new CT counties
Leave missing for counties in all other states.*/


data twentytwo; set twentyone;
if fipscode in ('09001', '09003', '09005', '09007', '09009', '09011', '09013', '09015') then v169_flag_CT = 'A';
else if fipscode in ('09110', '09120', '09130', '09140', '09150', '09160', '09170', '09180', '09190') then v169_flag_CT = 'U'; 
else v169_flag_CT = "";
drop fipscode;
run;
/*3204 obsrv*/

/*export as v169_s2025.sas7bdat*/

data out.v169_s2025; set twentytwo; run;

/*export as v169_s2025.xlsx*/
proc export data=work.twentytwo
    outfile="&outpath.\v169_s2025.xlsx"
    dbms=xlsx
	replace; 
run;
