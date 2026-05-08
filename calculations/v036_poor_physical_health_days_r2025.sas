/*****************************************************************************
 * v036 - Poor Physical Health Days
 * Author: MB
 * Description: Average number of physically unhealthy days reported in past 30 days (age-adjusted).
 * Data Source: Behavioral Risk Factor Surveillance System
 * Data Download Link: N/A, Requested data
 * Numerator: The numerator is the average number of days reported by respondents to the question "Now thinking about your physical health, which includes physical illness and injury, for how many days during the past 30 days was your physical health not good?"
 * Denominator: The denominator is the total number of adult respondents in a county.
 *****************************************************************************/

/* V036 – Poor Physical Health Days

Measure v036:
State: file = "Copy of Estimates_2022_CHRR_final.xlsx"
sheet = "State_direct_estimates"
Variables: "pday", "pday_LCL", "pday_UCL"

County: file = "Copy of Estimates_2022_CHRR_final.xlsx"
sheet = "County_modelled_estimates"
Variables: "pday", "pctlpday2_5 ", and "pctlpday97_5" */


/*Current Macro*/

%LET measureID=v036;
%LET FIPS=CountyFIPS;
%LET day=pday;
%LET p2=pctlpday2_5;
%LET p97=pctlpday97_5;

%LET state=StateFIPS;
%LET adjusted= pday;
%LET LCL = pday_LCL;
%LET UCL = pday_UCL;

/**/

*NOTE: These data are requested directly from PLACES and are not publicly available;
PROC IMPORT
OUT= v036_county
DATAFILE= "P:\CH-Ranking\Data\2025\1 Raw Data\BRFSS\Copy of Estimates4CHRR_2022_Final.xlsx"
DBMS=xlsx REPLACE; 
GETNAMES=YES;
RUN;

DATA v036_county_2(KEEP = &FIPS. &day. &p2. &p97.); 
SET v036_county;
RUN;

Data v036_county_3;
set v036_county_2;
statecode = substr(&FIPS.,1,length(&FIPS.)-3);
countycode = substr(&FIPS.,3);
run;

DATA v036_county_3(KEEP = statecode countycode &day. &p2. &p97.); 
SET v036_county_3;
RUN;

data v036_county_3;
set v036_county_3;
v036_denominator=.;
v036_numerator=.;
v036_sourceflag=.;
run;

data v036_county_4;
set v036_county_3;
v036_rawvalue = input(&day., comma12.); 
v036_cilow = input(&p2., comma12.);
v036_cihigh = input(&p97., comma12.);
run;

DATA v036_county_4(KEEP = statecode countycode v036_numerator v036_denominator v036_rawvalue v036_cilow v036_cihigh v036_sourceflag); 
SET v036_county_4;
RUN;

proc sort data = v036_county_4;
by statecode countycode;
run;

PROC IMPORT
OUT= v036_state
DATAFILE= "P:\CH-Ranking\Data\2025\1 Raw Data\BRFSS\Copy of Estimates4CHRR_2022_Final.xlsx"
DBMS= xlsx REPLACE;
sheet="State_estimates";
getnames=yes;
run;

DATA v036_state_2(KEEP = &state. &adjusted. &LCL. &UCL.); 
SET v036_state;
RUN;

data v036_state_2;
set v036_state_2;
countycode="000";
run;

data v036_state_2;
set v036_state_2;
v036_denominator=.;
v036_numerator=.;
v036_sourceflag=.;
run;

data v036_state_3;
set v036_state_2;
v036_rawvalue = input(&adjusted., comma12.);
v036_cilow = input(&LCL., comma12.);
v036_cihigh = input(&UCL., comma12.);
run;

data v036_state_4;
set v036_state_3;
statecode= &state. ; /*put (&state., z2.);*/
run;

DATA v036_state_4(KEEP = statecode countycode v036_numerator v036_denominator v036_rawvalue v036_cilow v036_cihigh v036_sourceflag); 
attrib statecode length=$2;
SET v036_state_4;
RUN;

proc sort data = v036_state_4;
by statecode countycode;
run;

data v036_countystate;
merge v036_state_4 v036_county_4;
by statecode countycode;
run;

proc sort data = v036_countystate;
by statecode countycode;
run;

libname savetoexport "P:\CH-Ranking\Data\2025\3 Data calculated needs checking";

Data savetoexport.&measureID.;
set v036_countystate_3;
run;
