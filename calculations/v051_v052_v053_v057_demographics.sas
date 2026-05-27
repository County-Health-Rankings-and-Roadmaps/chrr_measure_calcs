/* for v051, v052, v053, and v057 use the same combined set of all state files */

/*****************************************************************************
 * Author: MB 
 * Data Source: Census Population Estimates Program

 * v051 - Population
 * Description: Resident population.

 * v052 - % Below 18 Years of Age
 * Description: Percentage of population below 18 years of age.

 * v053 - % 65 and Older
 * Description: Percentage of population ages 65 and older.

 * v057 - % Female
 * Description: Percentage of population identifying as female.
 *****************************************************************************/

PROC IMPORT
OUT= full
datafile= "C:\Users\mburdine\Desktop\Inital Calcuations\cc-est2023-agesex-all.csv"
DBMS= csv REPLACE;
getnames=yes;
run;

Data full_1 (drop = SUMLEV STNAME CTYNAME YEAR);
set full;
if YEAR = 5;
run;

Data state (drop= COUNTY);
set full_1;
run;

proc means data = state sum;
	class STATE;
	output out = state_1 sum = ;
	run;

Data state_2 (drop= _TYPE_ _FREQ_);
set state_1;
COUNTY=0;
run;

/*combine*/

Data Full_3;
set state_2 full_1;
run;

proc sort; by STATE COUNTY; run;

/*
v051 - Population
v052 - % below 18 years of age
v053 - % 65 and older	
v057 - % Females

*/

Data Full_5;
set Full_3;
BELOW18PERCENT = ((POPESTIMATE - AGE18PLUS_TOT)/POPESTIMATE);
OLDER65PERCENT = (AGE65PLUS_TOT/POPESTIMATE);
FEMALEPERCENT = (POPEST_FEM/POPESTIMATE);
run;

proc sort data=Full_5;
by STATE COUNTY;
run;

Data Full_5 (keep= statecode countycode v053_rawvalue v053_numerator v057_numerator v057_rawvalue v051_rawvalue v052_rawvalue v052_numerator v057_denominator v052_denominator v053_denominator);
set Full_5;
statecode = put(state,z2.);
countycode = put(county, z3.);
v057_numerator=POPEST_FEM;
v057_rawvalue=FEMALEPERCENT;
v051_rawvalue=POPESTIMATE;
v052_rawvalue=BELOW18PERCENT;
v052_numerator=POPESTIMATE-AGE18PLUS_TOT;
v057_denominator=POPESTIMATE;
v052_denominator=POPESTIMATE;
v053_denominator=POPESTIMATE;
v053_rawvalue=OLDER65PERCENT;
v053_numerator=AGE65PLUS_TOT;
run;


/* Split into single measure datasets*/

libname savetoexport "C:\Users\mburdine\Desktop\pause";

Data savetoexport.v051 (Keep = statecode countycode v051_rawvalue);
set Full_5;
run;

Data savetoexport.v052 (Keep = statecode countycode v052_rawvalue v052_numerator v052_denominator);
set Full_5;
run;

Data savetoexport.v053 (Keep = statecode countycode v053_rawvalue v053_numerator v053_denominator);
set Full_5;
run;

Data savetoexport.v057 (Keep = statecode countycode v057_rawvalue v057_numerator v057_denominator);
set Full_5;
run;
