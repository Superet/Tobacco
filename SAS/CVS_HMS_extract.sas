%let dirname= U:\Users\ccv103\Documents\Research\Nielsen\nielsen_extracts\HMS;
libname mylib "U:\Users\ccv103\Documents\Research\tobacco\SAS_temp";

*****************;
* Read HMS data *; 
*****************;
* Read master retailer data;
%let fpath = &dirname\Master_Files\Latest\retailers.tsv;
%put &fpath;
PROC IMPORT OUT= WORK.hms_retailers
            DATAFILE= "&fpath" 
            DBMS=dlm REPLACE;
			DELIMITER ='09'x;
     		DATAROW=2; 
			GUESSINGROWS=5000;
RUN;
proc sql noprint;
	create table hms_drugstore as 
	select *
	from hms_retailers
	where channel_type = "Drug Store"; 
quit; 

* Read master product data;
%let fpath = &dirname\Master_Files\Latest\products.tsv;
%put &fpath;
PROC IMPORT OUT= WORK.tmp
            DATAFILE= "&fpath" 
            DBMS=dlm REPLACE;
			DELIMITER ='09'x;
     		DATAROW=2; 
			GUESSINGROWS=5000;
RUN;
proc sql noprint;
	create table hms_products as 
	select *, cats(upc, "-", upc_ver_uc) as upcv
	from tmp
	where product_module_descr in ("CIGARETTES");
	
	drop table tmp; 
quit; 

* Read UPC-level purchase data;
%macro read_groc_fun;
	%do i=2013 %to 2014;
		* Read panelists data for geographic market information; 
		%let fpath = &dirname\&i\Annual_Files\panelists_&i..tsv;
		%put &fpath;
		PROC IMPORT OUT= WORK.panelists&i
		            DATAFILE= "&fpath" 
		            DBMS=dlm REPLACE;
					DELIMITER ='09'x;
		     		DATAROW=2; 
					GUESSINGROWS=5000;
		RUN;
		
		* Import trip data;
		%let fpath = &dirname\&i\Annual_Files\trips_&i..tsv;
		%put &fpath;
		PROC IMPORT OUT= WORK.trips&i
		            DATAFILE= "&fpath" 
		            DBMS=dlm REPLACE;
					DELIMITER ='09'x;
		     		DATAROW=2; 
		RUN;
		
		* Import purchase data;
		%let fpath = &dirname\&i\Annual_Files\purchases_&i..tsv;
		%put &fpath;
		PROC IMPORT OUT= WORK.purchases&i
		            DATAFILE= "&fpath" 
		            DBMS=dlm REPLACE;
					DELIMITER ='09'x;
		     		DATAROW=2; 
		RUN;
		
		data panelists&i(drop = projection_factor_magnet_char);
			length scantrack_market_descr $50.;
			set panelists&i(rename = (projection_factor_magnet = projection_factor_magnet_char)); 
			projection_factor_magnet = input(projection_factor_magnet_char, best12.);
			drop dma_descr fips:; 
			drop kitchen_appliances tv_items household_internet_connection wic_indicator_current wic_indicator_ever_notcurrent Member:;
		run;
		
		%if &i=2013 %then %do; 
			data panelists;		set panelists&i; run; 
			data purchases; 	set purchases&i; run;
			data trips; 		set trips&i; run;
		%end;
		%else %do;
			data panelists;		set panelists panelists&i; run;
			data purchases; 	set purchases purchases&i; run; 
			data trips; 		set trips trips&i; run;
		%end; 
		proc datasets noprint; delete purchases&i tmp tmp1 tmp2 tmp3 tmp4 trips&i panelists&i; run;
	%end;
%mend read_groc_fun;
%read_groc_fun;

* Merge hosuehold and purchase data to purchase data; 
proc sql noprint; 
	create table tmp as 
	select household_code, purchase_date, A.*
	from (select * from purchases where upc in (select upc from hms_products)) as A
	left join 
	trips as B
	on A.trip_code_uc = B.trip_code_uc 
	order by household_code, purchase_date; 
	
	* Merge product size; 
	create table cig_purchases as 
	select A.*, size1_amount*multi as size
	from tmp as A 
	left join hms_products as B
	on A.upc = B.upc and A.upc_ver_uc = B.upc_ver_uc; 
		
	drop table tmp;
quit; 

proc sql; select nmiss(size)/count(*) from cig_purchases; quit; 

***********************;
* Household selection *; 
***********************;
/*
Selection criteria: 
1. households stay in data in 2013 and 2014
2. Make at least 5 purchases of both years
*/
proc sql noprint;
	* Tenure for each household; 
	create table tmp1 as 
	select household_code, count(panel_year) as n
	from panelists
	where panel_year in (2013, 2014)
	group by household_code; 
	
	* Purchase incidence each year; 
	create table tmp2 as 
	select household_code, year, count(unique(purchase_date)) as n
	from (	select *, year(purchase_date) as year 
			from cig_purchases where household_code in (select household_code from tmp1 where n = 2) and size is not missing)
	group by household_code, year
	order by household_code, year; 
	
	* Average purchase incidence; 
	create table tmp3 as 
	select household_code, mean(n) as n
	from tmp2 
	group by household_code;
	
	* Extract the selected households; 
	create table keephh as
	select household_code 
	from tmp3 
	where n >= 5; 
	
	* Extract non-smokers; 
	create table nonsmkers as 
	select distinct household_code
	from tmp1
	where household_code not in (select household_code from tmp3) and n = 2; 
quit; 
proc sql; 
	select sum(n=2)/count(*) as pct_hh from tmp1; 
	select mean(n) as mean_purchases from tmp2; 
	select sum(n>=5)/count(*) as pct_sel from tmp3; 
	select count(unique(household_code)) from nonsmkers; 
	select count(unique(household_code)) from keephh;
quit; 
proc datasets noprint; delete tmp1 tmp2 tmp3; run;

* Check if interpurchase days are too long;
proc sql noprint;
	create table tmp as 
	select *
	from trips 
	where household_code in (select household_code from keephh)
	order by household_code, purchase_date; 
quit; 
data tmp;
	set tmp;
	gap = dif(purchase_date); 
	by household_code; 
	if first.household_code then gap = .;
run;

proc univariate data = tmp; var gap; run;

* Subset data; 
proc sql noprint;
	create table sub_panelists as 
	select *
	from panelists
	where household_code in (select household_code from keephh); 
	
	create table sub_trips as 
	select A.*, channel_type
	from (select * from trips 
		where household_code in (select household_code from keephh)) as A 
	left join hms_retailers as B
	on A.retailer_code = B.retailer_code
	order by household_code, purchase_date;  
	
	create table subcig_purchases as 
	select * from cig_purchases 
	where household_code in (select household_code from keephh) 
	order by household_code, purchase_date; 
quit; 
	
* Add distance to the panelist data; 
PROC IMPORT OUT= location
    DATAFILE= "U:\Users\ccv103\Documents\Research\Store switching\store_data\cvs.csv" 
    DBMS=csv REPLACE;
   	GETNAMES=YES; 
	GUESSINGROWS=5000;
RUN;

* Merge latitude and longitude; 
proc sql noprint;
	create table tmp1 as 
	select A.*, substr(put(panelist_zip_code, z5.), 1, 3) as zip3, B.Y as hh_latitude, B.X as hh_longitude, COUNTY
	from sub_panelists as A
	left join SASHELP.zipcode as B
	on A.panelist_zip_code = B.zip; 
	
	create table tmp as 
	select household_code, panel_year, panelist_zip_code, hh_latitude, hh_longitude, A.county, B.latitude, B.longitude, B.zip_code as store_zip
	from tmp1 as A
	left join 
	(select C.*, substr(put(zip_code, z5.), 1, 3) as zip3, D.county from location(drop=county) as C
	left join SASHELP.zipcode as D
	on C.zip_code = D.zip)  as B	
	on A.county = B.county;
quit; 

%let pi180=0.0174532925199433;
data tmp;
	set tmp;
	if latitude ^=. and hh_latitude ^=. then do; 
		a = sin((hh_latitude*&pi180-latitude*&pi180)/2)**2 + 
			cos(latitude*&pi180)*cos(hh_latitude*&pi180)*sin((longitude*&pi180-hh_longitude*&pi180)/2)**2;
		b =  2*atan2(sqrt(a), sqrt(1-a));
		distance = 3959*b; 
	end; 
	drop a b;
run; 

proc sql noprint;
	create table dist as 
	select household_code, panel_year, min(distance) as distance
	from tmp
	group by household_code, panel_year
	order by household_code, panel_year;
quit; 

* Get the distance to Walgreens; 
PROC IMPORT OUT= location
    DATAFILE= "U:\Users\ccv103\Documents\Research\Store switching\store_data\walgreens.csv" 
    DBMS=csv REPLACE;
   	GETNAMES=YES; 
	GUESSINGROWS=5000;
RUN;

* Merge latitude and longitude; 
proc sql noprint;
	create table tmp1 as 
	select A.*, substr(put(panelist_zip_code, z5.), 1, 3) as zip3, B.Y as hh_latitude, B.X as hh_longitude, COUNTY
	from sub_panelists as A
	left join SASHELP.zipcode as B
	on A.panelist_zip_code = B.zip; 
	
	create table tmp as 
	select household_code, panel_year, panelist_zip_code, hh_latitude, hh_longitude, A.county, B.latitude, B.longitude, B.zip_code as store_zip
	from tmp1 as A
	left join 
	(select C.*, substr(put(zip_code, z5.), 1, 3) as zip3, D.county from location(drop=county) as C
	left join SASHELP.zipcode as D
	on C.zip_code = D.zip)  as B	
	on A.county = B.county;
quit; 

%let pi180=0.0174532925199433;
data tmp;
	set tmp;
	if latitude ^=. and hh_latitude ^=. then do; 
		a = sin((hh_latitude*&pi180-latitude*&pi180)/2)**2 + 
			cos(latitude*&pi180)*cos(hh_latitude*&pi180)*sin((longitude*&pi180-hh_longitude*&pi180)/2)**2;
		b =  2*atan2(sqrt(a), sqrt(1-a));
		distance = 3959*b; 
	end; 
	drop a b;
run; 

proc sql noprint;
	create table dist1 as 
	select household_code, panel_year, min(distance) as distance_wgr
	from tmp
	group by household_code, panel_year
	order by household_code, panel_year;
quit; 

data dist; 
	merge dist dist1; 
	by household_code panel_year; 
run;
proc means data = dist; var distance distance_wgr; run;

proc sql noprint; 
	create table tmp as 
	select A.household_code, A.panel_year, panelist_zip_code, distance, distance_wgr, scantrack_market_descr, 
		household_income, household_size, male_head_age, male_head_employment, female_head_age, female_head_employment, 
		age_and_presence_of_children, race, projection_factor
	from sub_panelists as A 
	left join dist as B
	on A.household_code = B.household_code and A.panel_year = B.panel_year
	order by household_code, panel_year; 
	
	* Append county; 
	create table sub_pan as 
	select A.*, B.city, B.countynm, B.statecode
	from tmp as A left join sashelp.zipcode as B
	on A.panelist_zip_code = B.zip
	order by household_code, panel_year; 
quit; 

data subcig_purchases;
	format UPC 15.; 
	set subcig_purchases; 
run;

* Export data; 
PROC EXPORT DATA= sub_pan
            OUTFILE= "U:\Users\ccv103\Desktop\tob_CVS_pan.csv"
            DBMS=csv REPLACE;
     PUTNAMES=YES;
RUN;

PROC EXPORT DATA= sub_trips
            OUTFILE= "U:\Users\ccv103\Desktop\tob_CVS_trips.csv"
            DBMS=csv REPLACE;
     PUTNAMES=YES;
RUN;

PROC EXPORT DATA= subcig_purchases
            OUTFILE= "U:\Users\ccv103\Desktop\tob_CVS_purchases.csv"
            DBMS=csv REPLACE;
     PUTNAMES=YES;
RUN;


*********************************;
* Shopping trips of non-smokers *; 
*********************************;
* Subset data; 
proc sql noprint;
	create table sub_nonsmkers as 
	select *
	from panelists
	where household_code in (select household_code from nonsmkers); 
	
	create table sub_trips_nonsmkers as 
	select A.*, channel_type
	from (select * from trips 
		where household_code in (select household_code from nonsmkers)) as A 
	left join hms_retailers as B
	on A.retailer_code = B.retailer_code
	order by household_code, purchase_date;  
quit; 
	
* Add distance to the panelist data; 
PROC IMPORT OUT= location
    DATAFILE= "U:\Users\ccv103\Documents\Research\Store switching\store_data\cvs.csv" 
    DBMS=csv REPLACE;
   	GETNAMES=YES; 
	GUESSINGROWS=5000;
RUN;

* Merge latitude and longitude; 
proc sql noprint;
	create table tmp1 as 
	select A.*, substr(put(panelist_zip_code, z5.), 1, 3) as zip3, B.Y as hh_latitude, B.X as hh_longitude, COUNTY
	from sub_nonsmkers as A
	left join SASHELP.zipcode as B
	on A.panelist_zip_code = B.zip; 
	
	create table tmp as 
	select household_code, panel_year, panelist_zip_code, hh_latitude, hh_longitude, A.county, B.latitude, B.longitude, B.zip_code as store_zip
	from tmp1 as A
	left join 
	(select C.*, substr(put(zip_code, z5.), 1, 3) as zip3, D.county from location(drop=county) as C
	left join SASHELP.zipcode as D
	on C.zip_code = D.zip)  as B	
	on A.county = B.county;
quit; 

%let pi180=0.0174532925199433;
data tmp;
	set tmp;
	if latitude ^=. and hh_latitude ^=. then do; 
		a = sin((hh_latitude*&pi180-latitude*&pi180)/2)**2 + 
			cos(latitude*&pi180)*cos(hh_latitude*&pi180)*sin((longitude*&pi180-hh_longitude*&pi180)/2)**2;
		b =  2*atan2(sqrt(a), sqrt(1-a));
		distance = 3959*b; 
	end; 
	drop a b;
run; 

proc sql noprint;
	create table dist as 
	select household_code, panel_year, min(distance) as distance
	from tmp
	group by household_code, panel_year
	order by household_code, panel_year;
quit; 

* Get the distance to Walgreens; 
PROC IMPORT OUT= location
    DATAFILE= "U:\Users\ccv103\Documents\Research\Store switching\store_data\walgreens.csv" 
    DBMS=csv REPLACE;
   	GETNAMES=YES; 
	GUESSINGROWS=5000;
RUN;

* Merge latitude and longitude; 
proc sql noprint;
	create table tmp1 as 
	select A.*, substr(put(panelist_zip_code, z5.), 1, 3) as zip3, B.Y as hh_latitude, B.X as hh_longitude, COUNTY
	from sub_nonsmkers as A
	left join SASHELP.zipcode as B
	on A.panelist_zip_code = B.zip; 
	
	create table tmp as 
	select household_code, panel_year, panelist_zip_code, hh_latitude, hh_longitude, A.county, B.latitude, B.longitude, B.zip_code as store_zip
	from tmp1 as A
	left join 
	(select C.*, substr(put(zip_code, z5.), 1, 3) as zip3, D.county from location(drop=county) as C
	left join SASHELP.zipcode as D
	on C.zip_code = D.zip)  as B	
	on A.county = B.county;
quit; 

%let pi180=0.0174532925199433;
data tmp;
	set tmp;
	if latitude ^=. and hh_latitude ^=. then do; 
		a = sin((hh_latitude*&pi180-latitude*&pi180)/2)**2 + 
			cos(latitude*&pi180)*cos(hh_latitude*&pi180)*sin((longitude*&pi180-hh_longitude*&pi180)/2)**2;
		b =  2*atan2(sqrt(a), sqrt(1-a));
		distance = 3959*b; 
	end; 
	drop a b;
run; 

proc sql noprint;
	create table dist1 as 
	select household_code, panel_year, min(distance) as distance_wgr
	from tmp
	group by household_code, panel_year
	order by household_code, panel_year;
quit; 

data dist; 
	merge dist dist1; 
	by household_code panel_year; 
run;
proc means data = dist; var distance distance_wgr; run;

proc sql noprint; 
	create table tmp as 
	select A.household_code, A.panel_year, panelist_zip_code, distance, distance_wgr, scantrack_market_descr, 
		household_income, household_size, male_head_age, male_head_employment, female_head_age, female_head_employment, 
		age_and_presence_of_children, race, projection_factor
	from sub_nonsmkers as A 
	left join dist as B
	on A.household_code = B.household_code and A.panel_year = B.panel_year
	order by household_code, panel_year; 
	
	drop table sub_nonsmkers; 
	
	* Append county; 
	create table sub_nonsmkers as
	select A.*, B.city, B.countynm, B.statecode
	from tmp as A left join sashelp.zipcode as B
	on A.panelist_zip_code = B.zip
	order by household_code, panel_year;
quit; 

proc contents data = sub_nonsmkers; run;
proc contents data = sub_trips_nonsmkers; run;


* Export data; 
PROC EXPORT DATA= sub_nonsmkers
            OUTFILE= "U:\Users\ccv103\Desktop\tob_CVS_nonsmk_pan.csv"
            DBMS=csv REPLACE;
     PUTNAMES=YES;
RUN;

PROC EXPORT DATA= sub_trips_nonsmkers
            OUTFILE= "U:\Users\ccv103\Desktop\tob_CVS_nonsmk_trips.csv"
            DBMS=csv REPLACE;
     PUTNAMES=YES;
RUN;
