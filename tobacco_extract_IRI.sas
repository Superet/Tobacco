%let dirname= E:\Users\Projects\Project_Marketing_IRI\kelloggirir;
options compress=yes reuse=yes;

*************;
* Read data *; 
*************;
* A function of reading data;
%macro readfile(dirlist,outname);
	proc sql noprint;
		select count(fname) into :num_files
		from &dirlist;
	quit;

	%do j=1 %to &num_files;
		proc sql noprint;
		select fname into :fname
		from &dirlist
		where n=&j;
		quit;
	
		%let fpath=&dirname\&fname;
		%put &fpath;
		proc import out=tmp&j
			datafile="&fpath"
			dbms=dlm replace;
			delimiter='|';
			getnames=no;
			datarow=1;
		run;
	%end;

	%let tmpend=%sysfunc(catx(%str(),tmp,&num_files));
	%put &tmpend;
	data &outname;
		set tmp1-&tmpend;
	run;

	* Delete separate files;
	proc datasets noprint; delete tmp1-&tmpend;run;	
%mend readfile;		

* Read the treatment counties and control counties;
PROC IMPORT OUT= WORK.mycounty
            DATAFILE= "\\tsclient\Resear1\Tobacco\processed_data\county_treatment.csv" 
            DBMS=csv REPLACE;
     		GETNAMES=YES; 
RUN;

*** Read demographics ***;
PROC IMPORT OUT= demo 
            DATAFILE= "E:\Users\Projects\Project_Marketing_IRI\kelloggirir\DEMO.txt" 
            DBMS=dlm REPLACE;
	 delimiter='|';
     GETNAMES=NO;
     DATAROW=1; 
RUN;

data demo;
	set demo(drop=VAR25);
	rename VAR1=PANID VAR2=FAMSIZE VAR3=INCOME VAR4=RACE VAR5=CHILDREN
	 	VAR6=FMLE_AGE VAR7=FMLE_EDU VAR8=FMLE_HRS VAR9=FMLE_OCC	VAR10=MALE_AGE 
		VAR11=MALE_EDU VAR12=MALE_HRS VAR13=MALE_OCC VAR14=M_STATUS VAR15=RENTOWN 
		VAR16=NUM_CATS VAR17=NUM_DOGS VAR18=REGION VAR19=MKT VAR20=PROJ09 VAR21=PROJ08 
		VAR22=PROJ07 VAR23=PROJ06 VAR24=ZIPCODE;
run;
	
*** Read demo code ***; 
PROC IMPORT OUT=democode
            DATAFILE="E:\Users\Projects\Project_Marketing_IRI\kelloggirir\Academic Household File.xls"
            DBMS=EXCELCS REPLACE;
   SHEET='Demo Code';
RUN;

data democode;
	set democode (firstobs=3 rename=(DEMO_CODE=VARIABLE F2=START F3=LABEL));
	if START=. then delete;
run;

*** Read trip code;
PROC IMPORT OUT=tripcode
            DATAFILE="E:\Users\Projects\Project_Marketing_IRI\kelloggirir\Academic Household File.xls"
            DBMS=EXCELCS REPLACE;
   SHEET='Trip Code';
RUN;

data tripcode;
	set tripcode(firstobs=3 drop=F4-F7);
	if F2=. then delete;
	rename F2=CODE F3=TRIP_DSC;
run;

*** Read UPC ***;
PROC IMPORT OUT= dict 
            DATAFILE= "E:\Users\Projects\Project_Marketing_IRI\kelloggirir\DICT.txt" 
            DBMS=dlm REPLACE;
	 delimiter='|';
     GETNAMES=NO;
     DATAROW=1; 
RUN;

data dict;
	set dict(drop=VAR10);
	rename VAR1=UPC VAR2=UPCDSC VAR3=CATEGORY VAR4=TYPE VAR5=VENDOR VAR6=BRAND VAR7=DEPARTMENT VAR8=VOL_EQ VAR9=UNIT_MEASURE;
proc sort; by UPC;
run;
proc contents data=dict;run;

*** Read trip detail data ***;
filename dirlist pipe "dir /B &dirname\Trip_detail*.txt";
data dirlist;
	length fname $30;
	infile dirlist length=reclen;
	input fname $varying30. reclen;
	n=_n_;
	*call symput ('num_files',_n_);
run;
proc print data=dirlist;run;

%readfile(dirlist,trip_det_full);

data trip_det_full;
	set trip_det_full(drop=VAR13 rename=(VAR1=PANID VAR2=CHAIN VAR3=DATE VAR4=TRIP_ID VAR5=WEEK 
				VAR6=UPC VAR7=DOL VAR8=UNITS VAR9=COUPON VAR10=DISPLAY VAR11=FEATUR VAR12=PRICEOFF));
	DATE = INPUT(PUT(DATE,8.),YYMMDD8.);
	FORMAT DATE YYMMDD10.;
run;

************************;
* Households selection *;
************************;
* Restrict to the product data only for cigarettes; 
proc freq data = dict(where = (category = "CATEGORY - TOBACCO PRODUCTS")); table type; run;

data my_products; 
	set dict; 
	where category = "CATEGORY - CIGARETTES"; 
run; 

* Select households in the geographic areas; 
proc sql noprint;
	create table my_demo as 
	select *
	from demo
	where mkt in (select start from democode where label in ("San Francisco/Oakland" "Boston"));
quit; 

* Subset the purchase data to the selected households and category purchase; 
proc sql noprint;
	create table purchases as 
	select *
	from trip_det_full 
	where panid in (select panid from my_demo) and upc in (select upc from my_products); 
quit; 
	
* Check the purchase incidence; 
proc sql noprint;
	create table tmp as 
	select panid, count(date) as n
	from purchases
	group by panid; 
quit; 
proc univariate data = tmp; var n; run;


