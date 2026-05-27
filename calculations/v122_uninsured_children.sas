/*****************************************************************************
 * v122 - Uninsured Children
 * Author: MB
 * Description: Percentage of children under age 19 without health insurance.
 * Data Source: Small Area Health Insurance Estimates
 * Data Download Link: https://www.census.gov/data/datasets/time-series/demo/sahie/estimates-acs.html
 * Numerator: The numerator is the number of people under age 19 who currently have no health insurance coverage. A person is uninsured if they are not currently covered by insurance through a current/former employer or union, purchased from an insurance company, Medicare, Medicaid, Medical Assistance, any kind of government-assistance plan for those with low incomes or disability, TRICARE or other military health care, Indian Health Services, VA, or any other health insurance or health coverage plan.
 * Denominator: The denominator is the county population under age 19.
 *****************************************************************************/

%LET YEAR = 2023; 

PROC IMPORT
OUT= v122
datafile= "C:\Users\mburdine\Desktop\Duplications\RawData\sahie_&YEAR..csv"
DBMS= csv REPLACE;
guessingrows=500;
datarow=86;
run;

Data v122_1 (drop= Filename___sahie_&YEAR._csv VAR2 VAR3 VAR4 VAR5 VAR6 VAR7 VAR8 VAR9 VAR10 VAR11 VAR12 VAR13 VAR14 VAR15 VAR16 VAR17 VAR18 VAR19 VAR20 VAR21 VAR22 VAR23 VAR24 VAR25 VAR26);
set v122;
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

Data v122_2 (drop= racecat geocat sexcat iprcat agecat);
set v122_1;
where racecat=0 and sexcat=0 and iprcat=0 and agecat=4;
run;

Data v122_3 (keep= statefips countyfips v122_numerator v122_denominator nui_moe nipr_moe v122_moe);
set v122_2;
v122_numerator= NUI;
v122_denominator= NIPR;
v122_moe = pctui_moe;
run;

Data v122_5;
set v122_3;
run;

proc sort Data=v122_5; by statefips countyfips; run;

/*National value*/

Data National_122 (keep= v122_numerator v122_denominator countyfips);
set v122_3;
where countyfips=0;
run;

proc means data = National_122 sum;
	class countyfips;
	var v122_numerator v122_denominator;
	output out = National_1_122 sum = ;
	run;

Data National_2_122 (drop= _TYPE_ _FREQ_);
set National_1_122;

if countyfips=. then delete;
statefips=0;
run;

Data Final_122 (drop=statefips countyfips v122_moe nipr_moe nui_moe);
set National_2_122 v122_5;
v122_sourceflag=.;
v122_rawvalue= (v122_numerator/v122_denominator);
v122_cihigh= (v122_numerator/v122_denominator) + ((1.96*v122_moe)/164.5);
v122_cilow= (v122_numerator/v122_denominator) - ((1.96*v122_moe)/164.5);
statecode = put(statefips,z2.);
countycode = put(countyfips, z3.);
run;

/*Note: These files are missing the older CT counties (09001-09015)

/*Export final documents*/

libname final "C:\Users\mburdine\Desktop\final SAS exports";

Data final.v122;
RETAIN statecode countycode v122_numerator v122_denominator v122_rawvalue v122_cilow v122_cihigh v122_sourceflag;
set Final_122;
run;
