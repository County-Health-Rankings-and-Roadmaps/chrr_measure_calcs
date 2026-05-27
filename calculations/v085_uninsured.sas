/*****************************************************************************
 * v085 - Uninsured
 * Author: MB
 * Description: Percentage of population under age 65 without health insurance.
 * Data Source: Small Area Health Insurance Estimates
 * Data Download Link: https://www.census.gov/data/datasets/time-series/demo/sahie/estimates-acs.html
 * Numerator: The numerator is the number of people currently uninsured in the county under the age of 65.
 * Denominator: The denominator is the number of people in the county under age 65.
 *****************************************************************************/

%LET YEAR = 2023; 

PROC IMPORT
OUT= v085
datafile= "C:\Users\mburdine\Desktop\Duplications\RawData\sahie_&YEAR..csv"
DBMS= csv REPLACE;
guessingrows=500;
datarow=86;
run;

Data v085_1 (drop= Filename___sahie_&YEAR._csv VAR2 VAR3 VAR4 VAR5 VAR6 VAR7 VAR8 VAR9 VAR10 VAR11 VAR12 VAR13 VAR14 VAR15 VAR16 VAR17 VAR18 VAR19 VAR20 VAR21 VAR22 VAR23 VAR24 VAR25 VAR26);
set v085;
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

Data v085_2 (drop= racecat geocat sexcat iprcat agecat);
set v085_1;
where racecat=0 and sexcat=0 and iprcat=0 and agecat=0;
run;

Data v085_3 (keep= statefips countyfips v085_numerator v085_denominator nui_moe nipr_moe v085_moe);
set v085_2;
v085_numerator= NUI;
v085_denominator= NIPR;
v085_moe = pctui_moe;
run;

Data v085_5 (drop= nipr_moe nui_moe);
set v085_3;
run;

proc sort Data=v085_5; by statefips countyfips; run;

/*Creating the national value*/

Data National (keep= v085_numerator v085_denominator countyfips);
set v085_3;
where countyfips=0;
run;

proc means data = National sum;
	class countyfips;
	var v085_numerator v085_denominator;
	output out = National_1 sum = ;
	run;

Data National_2 (drop= _TYPE_ _FREQ_);
set National_1;
if countyfips=. then delete;
statefips=0;
run;

Data Final_085 (drop=statefips countyfips v085_moe);
set National_2 v085_5;
v085_sourceflag=.;
v085_cihigh= (v085_numerator/v085_denominator) + ((1.96*v085_moe)/164.5);
v085_cilow= (v085_numerator/v085_denominator) - ((1.96*v085_moe)/164.5);
v085_rawvalue= (v085_numerator/v085_denominator);
statecode = put(statefips,z2.);
countycode = put(countyfips, z3.);
run;

/*Note: These files are missing the older CT counties (09001-09015)

/*Export final document*/

libname final "C:\Users\mburdine\Desktop\final SAS exports";

Data final.v085;
RETAIN statecode countycode v085_numerator v085_denominator v085_rawvalue v085_cilow v085_cihigh v085_sourceflag;
set Final_085;
run;
