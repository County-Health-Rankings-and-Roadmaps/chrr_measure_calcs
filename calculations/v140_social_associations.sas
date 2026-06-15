/*****************************************************************************
 * v140 - Social Associations
 * Author: MB
 * Description: Number of membership associations per 10,000 population.
 * Data Source: County Business Patterns
 * Data Download Link: https://www.census.gov/data/datasets/2023/econ/cbp/2023-cbp.html
 * Numerator: The numerator is the total number of membership associations in a county. The membership organizations (NAICS code) in this measure include civic organizations (813410), bowling centers (713950), golf clubs (713910), fitness centers (713940), sports organizations (711211), religious organizations (813110), political organizations (813940), labor organizations (813930), business organizations (813910), and professional organizations (813920).
 * Denominator: The denominator is the total resident population of a county.
 *****************************************************************************/

PROC IMPORT
datafile= "P:\CH-Ranking\Data\2026\1 Raw Data\County Business Patterns\cbp23us.txt"
OUT= v140_national
DBMS= dlm 
REPLACE;
getnames=yes;
Delimiter= ",";
run;

/*[total number of associations (NAICS = 813410 + 713950 + 713910 + 713940 + 711211 + 813110 + 813940 + 813930 + 813910 + 813920) / ( population) *10,000]*/

Data v140_national_1;
set v140_national;
KAT=2;
If NAICS = 813410 then KAT=3;
If NAICS = 713950 then KAT=3;
If NAICS = 713910 then KAT=3;
If NAICS = 713940 then KAT=3;
If NAICS = 711211 then KAT=3;
If NAICS = 813110 then KAT=3;
If NAICS = 813940 then KAT=3;
If NAICS = 813930 then KAT=3;
If NAICS = 813910 then KAT=3;
If NAICS = 813920 then KAT=3;
run;

Data v140_national_2;
set v140_national_1;
If KAT=2 then delete; 
run;

PROC IMPORT
datafile= "P:\CH-Ranking\Data\2026\1 Raw Data\County Business Patterns\cbp23st.txt"
OUT= v140_state
DBMS= dlm 
REPLACE;
getnames=yes;
Delimiter= ",";
run;

Data v140_state_1;
set v140_state;
KAT=2;
If NAICS = 813410 then KAT=3;
If NAICS = 713950 then KAT=3;
If NAICS = 713910 then KAT=3;
If NAICS = 713940 then KAT=3;
If NAICS = 711211 then KAT=3;
If NAICS = 813110 then KAT=3;
If NAICS = 813940 then KAT=3;
If NAICS = 813930 then KAT=3;
If NAICS = 813910 then KAT=3;
If NAICS = 813920 then KAT=3;
run;

Data v140_state_2;
set v140_state_1;
If KAT=2 then delete; 
run;

PROC IMPORT
datafile= "P:\CH-Ranking\Data\2026\1 Raw Data\County Business Patterns\cbp23co.txt"
OUT= v140_county
DBMS= dlm 
REPLACE;
getnames=yes;
Delimiter= ",";
run;

Data v140_county_1;
set v140_county;
KAT=2;
If NAICS = 813410 then KAT=3;
If NAICS = 713950 then KAT=3;
If NAICS = 713910 then KAT=3;
If NAICS = 713940 then KAT=3;
If NAICS = 711211 then KAT=3;
If NAICS = 813110 then KAT=3;
If NAICS = 813940 then KAT=3;
If NAICS = 813930 then KAT=3;
If NAICS = 813910 then KAT=3;
If NAICS = 813920 then KAT=3;
run;

Data v140_county_2;
set v140_county_1;
If KAT=2 then delete; 
run;

libname step "P:\CH-Ranking\Data\2025\2 Cleaned data ready for Calculation or Verification";

Data vintage;
set step.vintage2023; /*Note: Ensure Population document matches data year, unlikely to be mosr recent*/
run;

Data pop2020(keep= POPESTIMATE2023 statecode countycode KAT);
set vintage;
KAT=2;
run;

/*County*/
proc means data=v140_county_2 nway noprint;
 class fipstate fipscty;
 var est;
 output out=v140_county_3 sum= /autoname;
run;

/*Ensure statecode/ countycode are Character varables in all 3 documents before merging*/ 

data v140_county_3 (drop= _TYPE_ _FREQ_ fipstate fipscty);
set v140_county_3;
statecode = put(input(fipstate, best2.), z2.); /*Charater*/
countycode = put(input(fipscty, best3.), z3.); /*Charater*/
run;

Data Pop2020cty (drop= KAT statecode countycode);
set Pop2020;
statecode1 = /*put(*/ statecode /*,z2.)*/;
countycode1 = /*put(*/ countycode/*, z3.)*/;
If countycode= 0 then delete;
run;

Data Pop2020cty (drop= countycode1 statecode1);
set Pop2020cty;
statecode= statecode1;
countycode= countycode1;
run;

data v140_county_4;
set v140_county_3;
If countycode="999" then delete;
run;

Data County_Final;
merge v140_county_4 Pop2020cty;
by statecode countycode;
run;

Data County_Final3 (keep= statecode countycode v140_numerator v140_denominator v140_rawvalue);
set County_Final;
v140_numerator= est_Sum;
v140_denominator= POPESTIMATE2023;
v140_rawvalue= (v140_numerator/ v140_denominator)*10000;
run;

/*Removed old county codes (02261 present)*/

Data County_Final3;
set County_Final3;
if statecode= "02" and countycode= "261" then delete;
run;

/*State*/
Data v140_state_3;
set v140_state_2;
where lfo= "-";
run;

proc means data=v140_state_3 nway noprint;
 class fipstate;
 var est;
 output out=v140_state_4 sum= /autoname;
run;

data v140_state_4 (drop= _TYPE_ _FREQ_ fipstate);
set v140_state_4;
statecode1 = /*put(*/fipstate /*,z2.)*/;
countycode1 = "000";
run;

Data Pop2020state (drop= KAT statecode countycode);
set Pop2020;
statecode1 = /*put(*/ statecode /*,z2.)*/;
countycode1 = /*put(*/ countycode /*, z3.)*/;
If countycode NE 0 then delete;
run;

Data Pop2020state (drop= countycode1 statecode1);
set Pop2020state;
statecode= statecode1;
countycode= countycode1;
run;

data v140_state_5 (drop= statecode1 countycode1);
set v140_state_4;
statecode=statecode1;
countycode=countycode1;
run;

Data State_Final;
merge v140_state_5 Pop2020state;
by statecode countycode;
run;

Data State_Final2 (keep= statecode countycode v140_numerator v140_denominator v140_rawvalue);
set State_Final;
v140_numerator= est_Sum;
v140_denominator= POPESTIMATE2023;
v140_rawvalue= (v140_numerator/ v140_denominator)*10000;
if statecode="00" then delete;
run;

/*National-*/
Data v140_national_3;
set v140_national_2;
where lfo= "-";
run;

Data pop20212;
set pop2020;
where countycode="000";
run;

proc means data=pop20212 nway noprint;
 class KAT;
 var POPESTIMATE2023;
 output out=nationalpop sum= /autoname;
run;

proc means data=v140_national_3 nway noprint;
 class uscode;
 var est;
 output out=v140_national_4 sum= /autoname;
run;

Data v140_national_4 (drop= _TYPE_ _FREQ_ uscode);
set v140_national_4;
statecode="00";
countycode="000";
run;

Data Nationalpop (drop= _TYPE_ _FREQ_ KAT);
set Nationalpop;
statecode="00";
countycode="000";
run;

Data National_Final;
merge v140_national_4 Nationalpop;
by statecode countycode;
run;

Data National_Final2 (keep= statecode countycode v140_numerator v140_denominator v140_rawvalue);
set National_Final;
v140_numerator= est_Sum;
v140_denominator= POPESTIMATE2023_Sum;
v140_rawvalue= (v140_numerator/ v140_denominator)*10000;
run;

/*Combine*/ 

proc sort data=State_Final2;
by statecode countycode;
run;

proc sort data=County_Final3;
by statecode countycode;
run;

Data Full_140;
merge National_Final2 State_Final2 County_Final3;
by statecode countycode;
run;

/*"Full" dataset excludes previous CT counties without data 09001-09015*/

/*Export*/
libname final "C:\Users\mburdine\Desktop\Duplications";

Data final.v140; set Full_140; run;



