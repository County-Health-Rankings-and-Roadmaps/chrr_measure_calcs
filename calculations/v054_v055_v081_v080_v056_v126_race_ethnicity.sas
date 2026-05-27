/* v054, v055, v081, v080, v056, v126 from same single large document 
 * Author: MB
 * Data Source: Census Population Estimates Program                   */

/*****************************************************************************
 * v054 - % Non-Hispanic Black
 * Description: Percentage of population identifying as non-Hispanic Black or African American.
 * Numerator: The numerator is the number of residents identifying as non-Hispanic Black or African American.
 * Denominator: The denominator is the total county resident population.
 *****************************************************************************/
/*****************************************************************************
 * v055 - % American Indian or Alaska Native
 * Description: Percentage of population identifying as American Indian or Alaska Native.
 * Numerator: The numerator is the number of residents identifying as American Indian or Alaska Native.
 * Denominator: The denominator is the total county resident population.
 *****************************************************************************/
/*****************************************************************************
 * v081 - % Asian
 * Description: Percentage of population identifying as Asian.
 * Numerator: The numerator is the number of residents identifying as Asian.
 * Denominator: The denominator is the total county resident population.
 *****************************************************************************/
/*****************************************************************************
 * v080 - % Native Hawaiian or Other Pacific Islander
 * Description: Percentage of population identifying as Native Hawaiian or Other Pacific Islander.
 * Numerator: The numerator is the number of residents identifying as Native Hawaiian or Other Pacific Islander.
 * Denominator: The denominator is the total county resident population.
 *****************************************************************************/
/*****************************************************************************
 * v056 - % Hispanic
 * Description: Percentage of population identifying as Hispanic.
 * Numerator: The numerator is the number of residents identifying as Hispanic. This measure includes people of any race whose ethnicity is Hispanic.
 * Denominator: The denominator is the total county resident population.
 *****************************************************************************/
/*****************************************************************************
 * v126 - % Non-Hispanic White
 * Description: Percentage of population identifying as non-Hispanic white.
 * Numerator: The numerator is the number of residents identifying as non-Hispanic White.
 * Denominator: The denominator is the total county resident population.
 *****************************************************************************/

PROC IMPORT
OUT= v1
datafile= "C:\Users\mburdine\Desktop\Inital Calcuations\cc-est2023-alldata.csv"
DBMS= csv REPLACE;
getnames=yes;
run;
data v2;
set v1;
if YEAR=5;
run;
Data v3 (drop= YEAR AGEGRP SUMLEV STNAME CTYNAME);
set v2;
if AGEGRP=0;
run;

Data state (drop= COUNTY);
set v3;
run;

proc means data = state sum;
	class STATE;
	/* var v003_numerator v003_denominator */
	output out = state_1 sum = ;
	run;

Data state_2 (drop= _TYPE_ _FREQ_);
set state_1;
COUNTY=0;
run;

/*combine*/

Data v_4;
set state_2 v3;
run;

proc sort; by STATE COUNTY; run;

Data v_5 (keep=pause state county TOT_POP NHBA_MALE NHBA_FEMALE IA_MALE IA_FEMALE AA_MALE AA_FEMALE NA_MALE NA_FEMALE H_MALE H_FEMALE NHWA_MALE NHWA_FEMALE);
set v_4;
run;

Data v_6;
set v_5;
if STATE=. then STATE=0;
run;


/* 
v054 - % Non-Hispanic Black	

v054_numertor = NHBA_MALE + NHBA_FEMALE

v055 - % American Indian & Alaska Native

v055_numertor = IA_MALE + IA_FEMALE

v081 - % Asian	

v081_numertor = AA_MALE + AA_FEMALE

v080 - % Native Hawaiian/Other Pacific Islander

v080_numertor = NA_MALE + NA_FEMALE
	
v056 - % Hispanic

v056_numertor = H_MALE + H_FEMALE

v126 - % Non-Hispanic White	

v126_numertor = NHWA_MALE + NHWA_FEMALE; */

Data v_7 (keep= STATE COUNTY TOT_POP v126_rawvalue v056_rawvalue v080_rawvalue v081_rawvalue v055_rawvalue v054_rawvalue v054_numerator v055_numerator v081_numerator v080_numerator v056_numerator v126_numerator v126_denominator v056_denominator v080_denominator v081_denominator v055_denominator v054_denominator);
set v_6;
v054_numerator = NHBA_MALE + NHBA_FEMALE;
v054_rawvalue = v054_numerator/ TOT_POP;
v054_denominator = TOT_POP;
v055_numerator = IA_MALE + IA_FEMALE;
v055_rawvalue = v055_numerator/ TOT_POP;
v055_denominator = TOT_POP;
v081_numerator = AA_MALE + AA_FEMALE;
v081_rawvalue = v081_numerator/ TOT_POP;
v081_denominator = TOT_POP;
v080_numerator = NA_MALE + NA_FEMALE;
v080_rawvalue = v080_numerator/ TOT_POP;
v080_denominator = TOT_POP;
v056_numerator = H_MALE + H_FEMALE;
v056_rawvalue = v056_numerator/ TOT_POP;
v056_denominator = TOT_POP;
v126_numerator = NHWA_MALE + NHWA_FEMALE;
v126_rawvalue = v126_numerator/ TOT_POP;
v126_denominator = TOT_POP;
run;

proc sort Data=v_7;
by STATE COUNTY;
run;

Data v_8 (drop= STATE TOT_POP COUNTY);
set v_7;
statecode = put(state,z2.);
countycode = put(county, z3.);
run;


/* Split out into single measure datasets*/

libname savetoexport "C:\Users\mburdine\Desktop\pause";

Data savetoexport.v126 (Keep = statecode countycode v126_rawvalue v126_numerator v126_denominator);
set v_8;
run;

Data savetoexport.v056 (Keep = statecode countycode v056_rawvalue v056_numerator v056_denominator);
set v_8;
run;

Data savetoexport.v080 (Keep = statecode countycode v080_rawvalue v080_numerator v080_denominator);
set v_8;
run;

Data savetoexport.v081 (Keep = statecode countycode v081_rawvalue v081_numerator v081_denominator);
set v_8;
run;

Data savetoexport.v055 (Keep = statecode countycode v055_rawvalue v055_numerator v055_denominator);
set v_8;
run;

Data savetoexport.v054 (Keep = statecode countycode v054_rawvalue v054_numerator v054_denominator);
set v_8;
run;
