/*****************************************************************************
 * v049 - Excessive Drinking
 * Author: MB
 * Description: Percentage of adults reporting binge or heavy drinking (age-adjusted).
 * Data Source: Behavioral Risk Factor Surveillance System
 * Data Download Link: N/A, Requested data
 * Numerator: The numerator is the number of adult respondents reporting either binge drinking or heavy drinking. Binge drinking is defined as a woman consuming more than four alcoholic drinks during a single occasion or a man consuming more than five alcoholic drinks during a single occasion. Heavy drinking is defined as a woman drinking more than one drink on average per day or a man drinking more than two drinks on average per day.
 * Denominator: The denominator is the total number of adult respondents in a county.
 *****************************************************************************/

/* V049 – Excessive Drinking

State: file = "Estimates_2022_CHRR_final.xlsx"
sheet = "State_direct_estimates"
Variables: "excessd", "excessd_LCL", "excessd_UCL"

County: file = "Estimates_2022_CHRR_final.xlsx"
sheet = "County_modelled_estimates"
Variables: "excessd", "pctlexcessd2_5 ", and "pctlexcessd97_5" */


/*Current Macro*/

%LET measureID=v049;
%LET FIPS=CountyFIPS;
%LET day=excessd;
%LET p2=pctlexcessd2_5;
%LET p97=pctlexcessd97_5;

%LET state=StateFIPS;
%LET adjusted= excessd;
%LET LCL = excessd_LCL;
%LET UCL = excessd_UCL;


/**/

PROC IMPORT
OUT= v049_county
DATAFILE= "C:\Users\mburdine\Desktop\Duplications\Data\v036_49_49\Estimates4CHRR_2022_Final.xlsx"
DBMS=xlsx REPLACE; 
GETNAMES=YES;
RUN;

DATA v049_county_2(KEEP = &FIPS. &day. &p2. &p97.); 
SET v049_county;
RUN;

Data v049_county_3;
set v049_county_2;
statecode = substr(&FIPS.,1,length(&FIPS.)-3);
countycode = substr(&FIPS.,3);
run;

DATA v049_county_3(KEEP = statecode countycode &day. &p2. &p97.); 
SET v049_county_3;
RUN;

data v049_county_3;
set v049_county_3;
v049_denominator=.;
v049_numerator=.;
v049_sourceflag=.;
run;

data v049_county_4;
set v049_county_3;
v049_rawvalue = input(&day., comma12.); 
v049_cilow = input(&p2., comma12.);
v049_cihigh = input(&p97., comma12.);
run;

DATA v049_county_4(KEEP = statecode countycode v049_numerator v049_denominator v049_rawvalue v049_cilow v049_cihigh v049_sourceflag); 
SET v049_county_4;
RUN;

proc sort data = v049_county_4;
by statecode countycode;
run;

PROC IMPORT
OUT= v049_state
DATAFILE= "C:\Users\mburdine\Desktop\Duplications\Data\v036_49_49\Estimates4CHRR_2022_Final.xlsx"
DBMS= xlsx REPLACE;
sheet="State_estimates";
getnames=yes;
run;

DATA v049_state_2(KEEP = &state. &adjusted. &LCL. &UCL.); 
SET v049_state;
RUN;

data v049_state_2;
set v049_state_2;
countycode="000";
run;

data v049_state_2;
set v049_state_2;
v049_denominator=.;
v049_numerator=.;
v049_sourceflag=.;
run;

data v049_state_3;
set v049_state_2;
v049_rawvalue = input(&adjusted., comma12.);
v049_cilow = input(&LCL., comma12.);
v049_cihigh = input(&UCL., comma12.);
run;

data v049_state_4;
set v049_state_3;
statecode= &state. ; /*put (&state., z2.);*/
run;

DATA v049_state_4(KEEP = statecode countycode v049_numerator v049_denominator v049_rawvalue v049_cilow v049_cihigh v049_sourceflag); 
attrib statecode length=$2;
SET v049_state_4;
RUN;

proc sort data = v049_state_4;
by statecode countycode;
run;

data v049_countystate;
merge v049_state_4 v049_county_4;
by statecode countycode;
run;

proc sort data = v049_countystate;
by statecode countycode;
run;

libname savetoexport "C:\Users\mburdine\Desktop\Dups";

Data savetoexport.&measureID.;
set v049_countystate_3;
run;
