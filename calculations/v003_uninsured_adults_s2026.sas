/*****************************************************************************
 * v003 - Uninsured Adults
 * Author: MB
 * Description: Percentage of adults under age 65 without health insurance.
 * Data Source: Small Area Health Insurance Estimates
 * Data Download Link: https://www.census.gov/data/datasets/time-series/demo/sahie/estimates-acs.html
 * Numerator: The numerator is the number of people ages 18 to 64 who currently have no health insurance coverage. A person is uninsured if they are not currently covered by insurance through a current/former employer or union, purchased from an insurance company, Medicare, Medicaid, Medical Assistance, any kind of government-assistance plan for those with low incomes or disability, TRICARE or other military health care, Indian Health Services, VA, or any other health insurance or health coverage plan.
 * Denominator: The denominator is the county population ages 18 to 64.
 *****************************************************************************/

%LET YEAR = 2023; 

PROC IMPORT
OUT= v003
datafile= "C:\Users\mburdine\Desktop\Duplications\RawData\sahie_&YEAR..csv"
DBMS= csv REPLACE;
guessingrows=500;
datarow=86;
run;

Data v003_1 (drop= Filename___sahie_&YEAR._csv VAR2 VAR3 VAR4 VAR5 VAR6 VAR7 VAR8 VAR9 VAR10 VAR11 VAR12 VAR13 VAR14 VAR15 VAR16 VAR17 VAR18 VAR19 VAR20 VAR21 VAR22 VAR23 VAR24 VAR25 VAR26);
set v003;
statefips= VAR3;
countyfips= VAR4;
geocat= VAR5;
agecat= VAR6;
racecat= VAR7;
sexcat= VAR8;
iprcat= VAR9;
NIPR= VAR10;
nipr_moe= VAR11;
NUI= VAR12;
nui_moe= VAR13;
NIC= VAR14;
nic_moe= VAR15;
PCTUI= VAR16;
pctui_moe= VAR17;
PCTIC= VAR18;
pctic_moe= VAR19;
PCTELIG= VAR20;
pctelig_moe= VAR21;
PCTLIIC= VAR22;
pctliic_moe=VAR23;
state_name= VAR24;
county_name= VAR25;
run;

Data v003_2 (drop= racecat geocat sexcat iprcat agecat);
set v003_1;
where racecat=0 and sexcat=0 and iprcat=0 and agecat=1;
run;

Data v003_3 (keep= statefips countyfips v003_numerator v003_denominator nui_moe nipr_moe v003_moe);
set v003_2;
v003_numerator= NUI;
v003_denominator= NIPR;
v003_moe = pctui_moe;
run;

Data v003_5 (drop= nipr_moe nui_moe);
set v003_3;
run;

proc sort Data=v003_5; by statefips countyfips; run;

/*Create national value*/
Data National_003 (keep= v003_numerator v003_denominator countyfips);
set v003_3;
where countyfips=0;
run;

proc means data = National_003 sum;
	class countyfips;
	var v003_numerator v003_denominator;
	output out = National_1_003 sum = ;
	run;

Data National_2_003 (drop= _TYPE_ _FREQ_);
set National_1_003;
if countyfips=. then delete;
statefips=0;
run;

Data Final_003 (drop=statefips countyfips v003_moe);
set National_2_003 v003_5;
v003_sourceflag=.;
v003_cihigh= (v003_numerator/v003_denominator) + ((1.96*v003_moe)/164.5);
v003_cilow= (v003_numerator/v003_denominator) - ((1.96*v003_moe)/164.5);
v003_rawvalue= (v003_numerator/v003_denominator);
statecode = put(statefips,z2.);
countycode = put(countyfips, z3.);
run;

/*Note: These files are missing the older CT counties (09001-09015)

/*Export final documents*/

libname final "C:\Users\mburdine\Desktop\final SAS exports";

Data final.v003;
RETAIN statecode countycode v003_numerator v003_denominator v003_rawvalue v003_cilow v003_cihigh v003_sourceflag;
set Final_003;
run;
