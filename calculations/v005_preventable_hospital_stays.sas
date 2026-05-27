/*Measure v005 - Preventable Hosptial Stays 

Description: Rate of hospital stays for ambulatory-care sensitive conditions per 100,000 Medicare enrollees.

Author: MB

Numerator: The numerator is the number of discharges for Medicare beneficiaries ages 18 years or older continuously enrolled in Medicare fee-for-service Part A and hospitalized for any of the following reasons: diabetes with short or long-term complications, uncontrolled diabetes without complications, diabetes with lower-extremity amputation, chronic obstructive pulmonary disease, asthma, hypertension, heart failure, dehydration, bacterial pneumonia, or urinary tract infection.

Denominator: The denominator is the number of Medicare beneficiaries ages 18 years or older continuously enrolled in Medicare fee-for-service Part A. Individuals enrolled in Medicare Advantage at any point during the year are excluded. In addition, beneficiaries who died during the year, but otherwise were continuously enrolled up until the date of death, as well as beneficiaries who became eligible for enrollment following the first of the year, but were continuously enrolled from that date to the end of the year, are included in the denominator.

Data Source: Mapping Medicare Disparities Tool (https://data.cms.gov/tools/mapping-medicare-disparities-by-population)

1. Make sure "Population View" is highlighted.

2. Year: "2022" (for Rankings 2025)

3. Geography: "County" and "State/Territory" as a separate download

4. Measure: "Prevention quality indicator (PQI)"

5. Adjustment: "Unsmoothed age standardized"

6. Condition/Service: "Prevention Quality Overall Composite (PQI#90)

7. Click "Download Data"

-National data were accessed by clicking on the map and selecting "trend view."

Note: For the 2025 release, the data source continues to use Connecticut's 8 legacy counties. Raw values for the 9 new CT counties are marked as missing, while those for the 8 old CT counties are included in the measure dataset.
Data source(s) and link */

*set mypath to be the root of your local repository ; 
%let mypath = ;
%let outpath = &mypath.\measure_datasets; 
libname out "&outpath."; 

PROC IMPORT
OUT= v005_county
datafile= "&mypath.\raw_data\MMDT\PQI_90_county.csv"
DBMS= csv REPLACE;
getnames=yes;
run;

Data county1(keep= statecode countycode v005_rawvalue);;
set v005_county;
If fips = 51515 then delete;
If fips = 51019 then delete;
If fips=46113 then fips=46102; /*correcting changing county fipscodes*/
If fips=02270 then fips=02158; /*correcting changing county fipscodes*/
fipss=put(fips, z5.);
statecode = substr(fipss,1,length(fipss)-3);
countycode = substr(fipss, 3);
v005_rawvalue = analysis_value;
if statecode>59 then delete;
run;

PROC IMPORT
OUT= v005_state
datafile= "&mypath.\raw_data\MMDT\PQI_90_state.csv"
DBMS= csv REPLACE;
getnames=yes;
run;

Data State1 (keep= statecode countycode v005_rawvalue);
set v005_state;
statecode = put(fips,z2.);
countycode="000";
v005_rawvalue = analysis_value;
if statecode>59 then delete;
run;

/* National value found from clicking on the map and selecting "trend view" in Disparities Tool
https://data.cms.gov/tools/mapping-medicare-disparities-by-population */
Data National;
statecode="00";
countycode="000";
v005_rawvalue = 2681 ; /*Remember to update with National Value from Tool*/
run;

/*Merge*/

proc sort data= County1; by statecode countycode; run;
proc sort data= State1; by statecode countycode; run;

Data v005_combined;
merge County1 State1 National;
by statecode countycode;
run;

Data v005_combined_1 (drop= v005_rawvalue);
set v005_combined;
If v005_rawvalue=0 then v005_rawvalue=.;
raw=v005_rawvalue;
run;

/*Export*/

data out.v005_s2025;
set v005_combined_1;
run; 
