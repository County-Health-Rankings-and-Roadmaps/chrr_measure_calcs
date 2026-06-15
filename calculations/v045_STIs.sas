/* v045 - Sexually Transmitted Infections
 * Author: MB
 * Description: Number of newly diagnosed chlamydia cases per 100,000 population.
 * Data Source: National Center for HIV/AIDS, Viral Hepatitis, STD, and TB Prevention
 * Data Download Link: https://gis.cdc.gov/grasp/nchhstpatlas/main.html?value=atlas
 * Numerator: The numerator is the number of reported chlamydia cases in a county.
 * Denominator: The denominator is the total county population 

How do we calculate this measure?
1. Select "STD"

2. Select "Tables"

3. Indicator: Select "Chlamydia" and click "Next"

4. Geography: Select "National," "State," or "County" if selecting "State" or "County" select the ones you wish to view and click "Next"

5. Year: Click "2022" (for 2025) and click "Next"

6. Demographics: Do not change Demographics selections, click "Create my table"

7. Click "Underlying data" and then click "Export"

Suppression: Counties having 3, 2, or 1 case(s) are suppressed. However, data are not suppressed for counties having 0 case. 

For 2025 from the documentaion: County level data for Connecticut is suppressed, County 48389 was suppressed
*/

*set mypath to be the root of your local repository ; 
%let mypath = ;
%let outpath = &mypath.\measure_datasets; 
libname out "&outpath."; 

PROC IMPORT
OUT= v045_national
datafile= "&mypath.\raw_data\NCHHSTP\AtlasPlusTableData_v045_national.csv"
DBMS= csv REPLACE;
getnames=yes;
guessingrows=100;
datarow=8;
run;

PROC IMPORT
OUT= v045_state
datafile= "&mypath.\raw_data\NCHHSTP\AtlasPlusTableData_v045_state.csv"
DBMS= csv REPLACE;
getnames=yes;
guessingrows=100;
datarow=8;
run;

PROC IMPORT
OUT= v045_county
datafile= "&mypath.\raw_data\NCHHSTP\AtlasPlusTableData_v045_county.csv"
DBMS= csv REPLACE;
getnames=yes;
guessingrows=100;
datarow=8;
run;

Data v045_county_1 (Keep= Fipscode VAR5 VAR6 /*VAR12*/);
set v045_county;
fipscode=VAR4*1;
if VAR5="Cases" then delete;
if VAR6="Rate per 100000" then delete;
/*if VAR12="Population" then delete;*/
run;

Data v045_state_1 (Keep= Fipscode VAR5 VAR6 /*VAR10*/);
set v045_state;
fipscode= VAR4*1000;
if VAR5="Cases" then delete;
if VAR6="Rate per 100000" then delete;
/*if VAR10="Population" then delete;*/
run;

Data v045_national_1 (keep= Fipscode VAR3 VAR4 /*VAR10*/);
set v045_national;
fipscode=00000;
if VAR3="Cases" then delete;
if VAR4="Rate per 100000" then delete;
/*if VAR10="Population" then delete;*/
run;

Data v045_national_2 (keep= Fipscode v045_numerator /*v045_denominator*/ v045_rawvalue);
set v045_national_1;
v045_numerator= VAR3;
/*v045_denominator= VAR10;*/
v045_rawvalue= VAR4;
run;

Data v045_state_2 (keep= Fipscode v045_numerator /*v045_denominator*/ v045_rawvalue);
set v045_state_1;
v045_numerator= VAR5;
/*v045_denominator= VAR10;*/
v045_rawvalue= VAR6;
run;

proc sort data=v045_county_1;
by fipscode;
run;

Data v045_county_2 (keep= Fipscode v045_numerator /*v045_denominator*/ v045_rawvalue);
set v045_county_1;
v045_numerator= VAR5;
/*v045_denominator= VAR12;*/
v045_rawvalue= VAR6;
If Fipscode= 2201 then delete;
If Fipscode= 2232 then delete;
If Fipscode= 2280 then delete;
If Fipscode= 2261 then delete;
/*If v045_denominator="NA" then v045_numerator=.;
If v045_denominator="NA" then v045_rawvalue=.;
If v045_denominator="NA" then v045_denominator=.;*/
run;

proc sort data=v045_county_2;
by fipscode;
run;

Data Full; /*Potential Error: Double check National value isn't being cut off*/
set v045_county_2 v045_state_2 v045_national_2;
if fipscode > 57000 then delete;
run;

proc sort data=Full;
by fipscode;
run;

Data Full_1 (drop = pause v045_num fips fipscode);
set Full;
v045_num = v045_numerator*1;
pause=1;
if 0< v045_num <4 then pause=0;
if v045_num =. then pause=1;
If pause=0 then v045_numerator="0";
If pause=0 then v045_rawvalue="0";
If pause=0 then v045_denominator="0";
if v045_numerator="Data suppressed" the v045_rawvalue="";
/*if v045_numerator="Data suppressed" the v045_denominator="";*/
if v045_numerator="Data suppressed" the v045_numerator="";
fips= put(fipscode, z5.);
statecode = substr(fips,1,length(fips)-3);
countycode = substr(fips, 3);
run;

/*2025 Rankings only - supression*/

Data Full_1;
set Full_1;
if statecode=48 and countycode=389 then v045_numerator="";
if statecode=48 and countycode=389 then v045_rawvalue="0";
if statecode=48 and countycode=389 then v045_denominator="0";
run;

/*Export*/

data out.v045_s2025;
set Full_1;
run; 
