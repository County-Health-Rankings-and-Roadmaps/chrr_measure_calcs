/*v024 - Children in Poverty
author: MB; 

/* Description: Percentage of people under age 18 in poverty. 
Numerator: The numerator is the number of people under age 18 living in a household whose income is below the poverty level. Poverty status is defined by family; either everyone in the family is in poverty or no one in the family is in poverty. The characteristics of the family used to determine the poverty threshold are: number of people, number of related children under 18, and whether or not the primary householder is over age 65. Family income is then compared to the poverty threshold; if that family’s income is below that threshold, the family is in poverty.
Denominator: The total number of people under age 18 in a country. */ 

/* Website to download data: https://www.census.gov/data/datasets/2023/demo/saipe/2023-state-and-county.html 
 Online documentation: https://www.census.gov/programs-surveys/saipe/technical-documentation/methodology/counties-states/county-level.html 

/*Create 95% confidence intervals from the given 90% confidence intervals.

cidistance_pov = 1.96/1.645*(POV_90UCL-POV_U18PCT);
v024_cilow =  v024_rawvalue - cidistance_pov;
v024_cihigh =  v024_rawvalue + cidistance_pov*/


*set mypath to be the root of your local repository ; 
%let mypath = ;
%let outpath = &mypath.\measure_datasets; 
libname out "&outpath."; 


PROC IMPORT
OUT= v024
datafile= "&mypath.\raw_data\SAIPE\estall.xls"
out = work.v024
DBMS= xls REPLACE;
getnames=yes;
datarow = 5;
run;



Data v024_1 (keep=StateFips CountyFips rawvalue numerator v024_cilow v024_cihigh);
set v024;
StateFips = Table_with_column_headers_in_row;
CountyFips = B;
numerator = K*1;
rawvalue= N/100;
pause = O/100;
cidistance_pov = 1.96/1.645*(rawvalue-pause);
v024_cilow =  rawvalue - cidistance_pov;
v024_cihigh =  rawvalue + cidistance_pov;
if v024_cilow <0 then v024_cilow = 0;
if v024_cihigh > 1 then v024_cihigh = 1;
run;


data out.v024_s2025;
set v024_1;
run; 
