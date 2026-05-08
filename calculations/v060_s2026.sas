/*****************************************************************************
 * v060 - Diabetes Prevalence
 * Author: MB
 * Description: Percentage of adults aged 18 and above with diagnosed diabetes (age-adjusted).
 * Data Source: Behavioral Risk Factor Surveillance System
 * Data Download Link: https://data.cdc.gov/500-Cities-Places/PLACES-County-Data-GIS-Friendly-Format-2025-releas/i46a-9kgh/about_data
 * Numerator: The numerator is the number of adults 18 years and older who responded "yes" to the question, "Has a doctor ever told you that you have diabetes?" Both Type 1 and Type 2 diabetes diagnoses are included. Women who indicated that they only had diabetes during pregnancy were not considered to have diabetes.
 * Denominator: The denominator is the total number of respondents (age 18 and older) in a county.
 *****************************************************************************/

/*For 2026 updates, state and nation are median of counties*/

libname geo "P:\CH-Ranking\Data\2026\1 Raw Data\BRFSS";
libname calcs "C:\Users\mburdine\Desktop\Current working codes";

%LET measure=v060;
%LET PLACES=PLACES__County_Data_(GIS_Friendly_Format),_2025_release_20251205;
%LET BRFSS=ExportCSV_BRFSSv060;
%LET ADJPREV = Diabetes_adjprev; 
%LET ADJ95 = Diabetes_adj95CI;

PROC IMPORT
OUT= PLACES
DATAFILE= "P:\CH-Ranking\Data\2026\1 Raw Data\BRFSS\PLACES__County_Data_(GIS_Friendly_Format),_2025_release_20251205.csv"
DBMS=csv REPLACE; 
GETNAMES=YES;
RUN;

Data O_county_1 (keep= fipscode statecode countycode &measure._numerator &measure._denominator &measure._rawvalue &measure._cilow &measure._cihigh &measure._sourceflag);
set PLACES;
fipscode= CountyFIPS;
statecode = substr(fipscode,1,length(fipscode)-3);
countycode = substr(fipscode,3);
if statecode="12" then delete;
&measure._numerator = .;
&measure._denominator = .;
&measure._rawvalue = &ADJPREV./100;
cilow = input(scan(scan(&ADJ95., 1, '()'), 1, ','), BEST12.)  ;
cihigh = input(scan(scan(&ADJ95., 1, '()'), 2, ','), BEST12.)  ;
&measure._cilow = cilow/100;
&measure._cihigh = cihigh/100;
&measure._sourceflag = .;
run;

proc sort; by statecode countycode; run;

proc means data = O_county_1 median;
class statecode;
var &measure._rawvalue;
output out = state median = ;
run;

Data state_2 (keep=statecode v060_rawvalue);
merge state;
if statecode="" then statecode="00";
run;

Data state_3;
set state_2;
countycode = "000";
&measure._numerator = .;
&measure._denominator = .;
&measure._cilow = .;
&measure._cihigh = .;
&measure._sourceflag = .;
run;

Data FL_fix;
statecode="12";
countycode="000";
run;

Data state_4;
set state_3 FL_fix;
run;

proc sort; by countycode statecode; run;

/*Pull in final fipscode list*/

libname fips "P:\CH-Ranking\Data\2026\2 Cleaned data ready for Calculation or Verification";

Data fipscodes (keep=statecode countycode);
set fips.county_fips_with_ct_old;
run;

Data O_county_2;
merge O_county_1 fipscodes (In=A);
by statecode countycode;
If A;
run;

proc sort data = O_county_2;
by statecode countycode;
run;

proc sort data = State_4;
by statecode countycode;
run;

Data &measure. (drop=fipscode);
merge state_4 O_county_2;
by statecode countycode;
run;

proc sort; by statecode countycode; run;

/* Checking CIs are reasonable

proc means data = &measure.;
var &measure._numerator &measure._denominator &measure._rawvalue &measure._cilow &measure._cihigh;
run;
*/

Data calcs.&measure.;
RETAIN statecode countycode &measure._numerator &measure._denominator &measure._rawvalue &measure._cilow &measure._cihigh;
set &measure.;
run;



