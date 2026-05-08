/*****************************************************************************
 * v139 - Food Insecurity
 * Author: MB
 * Description: Percentage of population who lack adequate access to food.
 * Data Source: Map the Meal Gap
 * Data Download Link:
 * Numerator: The numerator is the population with a lack of access, at times, to enough food for an active, healthy life or with uncertain availability of nutritionally adequate foods.
 * Denominator: The denominator is the total county population.
 *****************************************************************************/

PROC IMPORT
OUT= v139_state
datafile= "C:\Users\mburdine\Desktop\Duplications\RawData\MMG2025_2019-2023_Data_To_Share.xlsx"
DBMS= xlsx REPLACE;
sheet="State";
run;

PROC IMPORT
OUT= v139_county
datafile= "C:\Users\mburdine\Desktop\Duplications\RawData\MMG2025_2019-2023_Data_To_Share.xlsx"
DBMS= xlsx REPLACE;
sheet="County";
run;

Data v139_state_2 (keep= v139_numerator v139_rawvalue fipscode);
set v139_state;
where year=2023;
v139_numerator=__of_Food_Insecure_Persons_Overa*1;
v139_rawvalue=Overall_Food_Insecurity_Rate*1;
fipscode=FIPS || "000";
run;

/*
Pull state DC data and make sure it matches DC county (state value wins) 
*/

Data v139_county_2 (keep= v139_numerator v139_rawvalue fipscode);
set v139_county;
where year=2023;
v139_numerator=__of_Food_Insecure_Persons_Overa*1;
v139_rawvalue=Overall_Food_Insecurity_Rate*1;
fipscode=FIPS;
if fipscode=11001 then v139_numerator=83960;/*Pull and replace the "county" DC with "state" DC info*/
if fipscode=11001 then v139_rawvalue=0.124;
run;

libname codes "P:\CH-Ranking\Data\2026\2 Cleaned data ready for Calculation or Verification";

Data v139_county_3 (drop= statecode countycode state county);
merge codes.county_fips_with_ct_old (In=A) v139_county_2;
by fipscode;
If A;
run;

/*Pull national data from website*/

Data National;
fipscode="00000";
v139_rawvalue=0.143;
v139_numerator=47389000;
run;

Data Full;
merge v139_county_3 v139_state_2 National;
by fipscode;
run;

Data Full_1 (drop= v139_rawvalue v139_numerator);
set Full;
	v139_rawvalue_1 = v139_rawvalue;
	v139_numerator_1 = v139_numerator;
run;

data v139_final (drop= fipscode v139_rawvalue_1 v139_numerator_1);
	set Full_1;
	statecode = substr(fipscode,1,length(fipscode)-3);
    countycode = substr(fipscode, 3);
	v139_rawvalue = v139_rawvalue_1;
	v139_numerator = v139_numerator_1;
	v139_denominator = .;
	v139_cilow = .;
	v139_cihigh = .;
	v139_sourceflag = .;
	v139_flag_CT = "";
run;

data v139_final_1;
	set v139_final;
	if v139_rawvalue = . and v139_numerator = . and statecode = "09" then v139_flag_CT = "U";
	if statecode = "09" and v139_flag_CT ne "U" then v139_flag_CT = "A";
	if countycode ="000" and statecode = "09" then v139_flag_CT = "";
run;

/*save*/
libname v139 "C:\Users\mburdine\Desktop\Duplications\v139update";

Data v139.v139; set v139_final_1; run;

