/* v023 Unemployment Measures */
/* author: MB 
/* Description: Percentage of population ages 16 and older unemployed but seeking work.
/* Numerator: The numerator is the total number of people in the civilian labor force, ages 16 and older, who are unemployed but seeking work. Unemployed persons are defined as persons who had no employment during the reference week, were available for work, except for temporary illness, and had made specific efforts to find employment some time during the four-week period ending with the reference week. Persons who were waiting to be recalled to a job from which they had been laid off need not have been looking for work to be classified as unemployed.

Denominator Included in CHR&R Database
The denominator is the total number of people in the civilian labor force, ages 16 and older. The civilian labor force includes all persons in the civilian noninstitutional population classified as either employed or unemployed. Employed persons are all persons who, during the reference week (the week including the 12th day of the month), (a) did any work as paid employees, worked in their own business or profession or on their own farm, or worked 15 hours or more as unpaid workers in an enterprise operated by a member of their family, or (b) were not working but who had jobs from which they were temporarily absent because of vacation, illness, bad weather, childcare problems, maternity or paternity leave, labor-management dispute, job training, or other family or personal reasons, whether or not they were paid for the time off or were seeking other jobs. Each employed person is counted only once, even if he or she holds more than one job.
; 
/* For 2025 Rankings
use only old CT counties, no data avalaible for new
*/

*set mypath to be the root of your local repository ; 
%let mypath = ;
%let outpath = &mypath.\measure_datasets; 
libname measure_datasets "&outpath."; 


PROC IMPORT
OUT= v023
datafile= "&mypath.\raw_data\BLS\laucnty24.xlsx"
out = work.v023
DBMS= xls REPLACE;
getnames=yes;
datarow = 7;
run;



Data v023_county (keep = fipscode B C v023_numerator v023_denominator v023_rawvalue);
set v023;
If B>70 then delete;
fipscode= B||C;
LaborForce = G;
Employed = H;
Unemployed = I;
UnemploymentRate = J;
v023_denominator = input(LaborForce,best12.);
v023_numerator = input(Unemployed,best12.);
v023_rawvalue = v023_numerator / v023_denominator;
If v023_rawvalue=. then delete;
run;


/* keep from v023_county */

proc summary data=v023_county;

   var v023_denominator v023_numerator;

   by B;

   output out=v023_state sum=;
run;

Data v023_state (drop= _TYPE_ _FREQ_);
set v023_state;
fipscode= B||"000";
run;

Data v023_state_1;
set v023_state;
v023_rawvalue = v023_numerator / v023_denominator;
C = "000";
run;

/* keep v023_state_1*/

Data v023_national;
set v023_state_1;
K="0";
run;

proc summary data=v023_national;


   var v023_denominator v023_numerator;
   by K;

   output out=v023_national_1 sum=;
run;

Data v023_national_2 (drop= K _TYPE_ _FREQ_);
set v023_national_1;
fipscode="00000";
B="00";
C="000";
v023_rawvalue = v023_numerator / v023_denominator;
run;

/*keep v023_national_2 */

Data v023_Full;
merge v023_national_2 v023_state_1 v023_county;
by B C;
run;

Data v023_Full (drop= fipscode B C);
set v023_Full;
statecode = B;
countycode = C;
run;

Data Full_02261;
v023_denominator=0;
v023_numerator=0;
v023_rawvalue=0;
statecode = "15";
countycode = "005";
run;

Data Full_5;
set v023_Full Full_02261;
run;

proc sort data=Full_5;
by STATECODE COUNTYCODE;
run;



data out.v023_s2025;
set Full_5;
run; 
