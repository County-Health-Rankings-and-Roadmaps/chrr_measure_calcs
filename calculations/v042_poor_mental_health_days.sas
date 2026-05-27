/*****************************************************************************
 * v042 - Poor Mental Health Days
 * Author: MB
 * Description: Average number of mentally unhealthy days reported in past 30 days (age-adjusted).
 * Data Source: Behavioral Risk Factor Surveillance System
 * Data Download Link: N/A, Requested data
 * Numerator: The numerator is the number of days respondents reported to the question "Now thinking about your mental health, which includes stress, depression, and problems with emotions, for how many days during the past 30 days was your mental health not good?"
 * Denominator: The denominator is the total number of adult respondents in a county.
 *****************************************************************************/

/* V042 – Poor Mental Health Days

Measure v042:
State: file = "Copy of Estimates_2022_CHRR_final.xlsx"
sheet = "State_direct_estimates"
Variables: "mday", "mday_LCL", "mday_UCL"

County: file = "Copy of Estimates_2022_CHRR_final.xlsx"
sheet = "County_modelled_estimates"
Variables: "mday", "pctlmday2_5 ", and "pctlmday97_5" */


/*Current Macro*/

%LET measureID=v042;
%LET FIPS=CountyFIPS;
%LET day=mday;
%LET p2=pctlmday2_5;
%LET p97=pctlmday97_5;

%LET state=StateFIPS;
%LET adjusted= mday;
%LET LCL = mday_LCL;
%LET UCL = mday_UCL;

/**/

*NOTE: These data are requested directly from PLACES and are not publicly available;
PROC IMPORT
OUT= v042_county
DATAFILE= "P:\CH-Ranking\Data\2025\1 Raw Data\BRFSS\Copy of Estimates4CHRR_2022_Final.xlsx"
DBMS=xlsx REPLACE; 
GETNAMES=YES;
RUN;

DATA v042_county_2(KEEP = &FIPS. &day. &p2. &p97.); 
SET v042_county;
RUN;

Data v042_county_3;
set v042_county_2;
statecode = substr(&FIPS.,1,length(&FIPS.)-3);
countycode = substr(&FIPS.,3);
run;

DATA v042_county_3(KEEP = statecode countycode &day. &p2. &p97.); 
SET v042_county_3;
RUN;

data v042_county_3;
set v042_county_3;
v042_denominator=.;
v042_numerator=.;
v042_sourceflag=.;
run;

data v042_county_4;
set v042_county_3;
v042_rawvalue = input(&day., comma12.); 
v042_cilow = input(&p2., comma12.);
v042_cihigh = input(&p97., comma12.);
run;

DATA v042_county_4(KEEP = statecode countycode v042_numerator v042_denominator v042_rawvalue v042_cilow v042_cihigh v042_sourceflag); 
SET v042_county_4;
RUN;

proc sort data = v042_county_4;
by statecode countycode;
run;

PROC IMPORT
OUT= v042_state
DATAFILE= "P:\CH-Ranking\Data\2025\1 Raw Data\BRFSS\Copy of Estimates4CHRR_2022_Final.xlsx"
DBMS= xlsx REPLACE;
sheet="State_estimates";
getnames=yes;
run;

DATA v042_state_2(KEEP = &state. &adjusted. &LCL. &UCL.); 
SET v042_state;
RUN;

data v042_state_2;
set v042_state_2;
countycode="000";
run;

data v042_state_2;
set v042_state_2;
v042_denominator=.;
v042_numerator=.;
v042_sourceflag=.;
run;

data v042_state_3;
set v042_state_2;
v042_rawvalue = input(&adjusted., comma12.);
v042_cilow = input(&LCL., comma12.);
v042_cihigh = input(&UCL., comma12.);
run;

data v042_state_4;
set v042_state_3;
statecode= &state. ; /*put (&state., z2.);*/
run;

DATA v042_state_4(KEEP = statecode countycode v042_numerator v042_denominator v042_rawvalue v042_cilow v042_cihigh v042_sourceflag); 
attrib statecode length=$2;
SET v042_state_4;
RUN;

proc sort data = v042_state_4;
by statecode countycode;
run;

data v042_countystate;
merge v042_state_4 v042_county_4;
by statecode countycode;
run;

proc sort data = v042_countystate;
by statecode countycode;
run;

libname savetoexport "P:\CH-Ranking\Data\2025\3 Data calculated needs checking";

Data savetoexport.&measureID.;
set v042_countystate_3;
run;
