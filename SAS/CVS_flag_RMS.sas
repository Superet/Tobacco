%let dirname= U:\Users\ccv103\Documents\Research\Nielsen\nielsen_extracts\RMS;
libname mylib "U:\Users\ccv103\Documents\Research\tobacco\SAS_temp";

*****************;
* Read RMS data *;
*****************;
*------------------------*;
* Read master product data;
%let fpath = &dirname\Master_Files\Latest\products.tsv;
%put &fpath;
PROC IMPORT OUT= WORK.orig_products
            DATAFILE= "&fpath" 
            DBMS=dlm REPLACE;
			DELIMITER ='09'x;
     		DATAROW=2; 
			GUESSINGROWS=5000;
RUN;

data my_products; 
	set orig_products; 
	where product_group_descr = "TOBACCO & ACCESSORIES";
	upcv = cats(upc, "-", upc_ver_uc); 
run;
proc freq data = orig_products; table product_module_descr; run;

* Only focus on cigaretts and cigar; 
data my_products; 
	retain upcv upc; 
	set my_products; 
	where product_module_descr in ("CIGARETTES"); 
proc contents; 	
run;

*--------------------------*; 
* Read master retailer data ;
data orig_stores; 
	infile datalines; 
	length file2read $300; 
	input file2read $;
	infile dummy filevar=file2read delimiter='09'x MISSOVER DSD LRECL=32767 FIRSTOBS=2 end = done;
	do while(not done);
		informat channel_code $1. fips_state_descr $2. fips_county_descr $50. dma_descr $50.;
		input 	store_code_uc year parent_code retailer_code channel_code $ store_zip3 fips_state_code fips_state_descr $
				fips_county_code fips_county_descr $ dma_code dma_descr $;
		output; 
	end;
	datalines;
	U:\Users\ccv103\Documents\Research\Nielsen\nielsen_extracts\RMS\2013\Annual_Files\stores_2013.tsv
	U:\Users\ccv103\Documents\Research\Nielsen\nielsen_extracts\RMS\2014\Annual_Files\stores_2014.tsv
	;
run;

proc sql noprint;
	create table drugstore as 
	select * 
	from orig_stores 
	where channel_code = "D"; 
quit; 

*--------------*; 
* UPC versions *; 
data orig_upc_ver; 
	infile datalines; 
	length file2read $300; 
	input file2read $;
	infile dummy filevar=file2read delimiter='09'x MISSOVER DSD LRECL=32767 FIRSTOBS=2 end = done;
	do while(not done);
/*		informat upc $20.;*/
		input 	upc upc_ver_uc year;
		output; 
	end;
	datalines;
	U:\Users\ccv103\Documents\Research\Nielsen\nielsen_extracts\RMS\2013\Annual_Files\rms_versions_2013.tsv		
	U:\Users\ccv103\Documents\Research\Nielsen\nielsen_extracts\RMS\2014\Annual_Files\rms_versions_2014.tsv		
	;
run;

* Subset upc_version to cigarette category; 
proc sql noprint; 
	create table upc_ver as 
	select *, cats(upc, "-", upc_ver_uc) as upcv
	from orig_upc_ver 
	where upc in (select upc from my_products); 
quit; 
proc freq data = upc_ver; table upc_ver_uc; run;

* Check if number of upc in product data is the same as the number of upc in upc_ver data; 
proc sql; 
	select count(unique(upc)) as n1_upc, count(unique(upcv)) as n1_upcv from my_products; 
	select count(unique(upc)) as n2_upc, count(unique(upcv)) as n2_upcv from upc_ver;
quit; 

* Check if the upc with mutiple upc versions from the product data exist in upc_ver data; 
* ONLY 6 products in the RMS can not be matched with upc_ver_uc; 
proc sql noprint; 
	create table tmp as 
	select *
	from (select * from my_products where upc in (select upc from my_products where upc_ver_uc > 1))
	where upc not in (select upc from upc_ver) and dataset_found_uc ^= "HMS";
quit; 
proc datasets noprint; delete tmp; run; 

*---------------------------------------------------*;
* Manually input category number of filename; 
* RMS file schema: folder name = DepartmentNumber_year; 
*				   file name = ModuleNumber_year; 
data movename; 
	input department module;
	datalines; 
	4510	7460
	;
run;

* Read data from 2013 to 2014; 
data tmp_file; 
	length filep $300. folder $300.; 
	set movename; 
	do year=2013 to 2014;
		folder = cats("U:\Users\ccv103\Documents\Research\Nielsen\nielsen_extracts\RMS\",year,"\Movement_Files\",department,"_",year);
		filen = cats(module,'_',year,'.tsv');
		filep = cats(folder, "\",filen);
		output;
	end;
run;

data movement;
	set tmp_file(keep=filep);
	infile dummy filevar = filep delimiter='09'x MISSOVER DSD LRECL=32767 FIRSTOBS=2 end = done;
	do while(not done);
		informat week_end YYMMDD10.;
		input store_code_uc upc week_end units prmult price feature display; 
		output; 
	end;
	drop filep;
	format week_end YYMMDD10.;
run;
proc contents data=movement; run;

data mylib.movement; set movement; run;

proc datasets noprint; delete movename tmp_file orig_products orig_upc_ver upc_ver; run;

*****************;
* Read HMS data *; 
*****************;
%let dirname= U:\Users\ccv103\Documents\Research\Nielsen\nielsen_extracts\HMS;

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

***********************;
* Compare HMS and RMS *; 
***********************;
* Check if products from HMS and RMS overlap; 
* Product files are identical; 
proc sort data = my_products; by upcv; 
proc sort data = hms_products; by upcv; run;
proc compare base = my_products compare = hms_products; 
	var upcv; 
run;

* ------------------------------------------------------*; 
* Check retailer_code and parent_code in RMS store files*; 
proc sql;
select nmiss(retailer_code)/count(*) as nm_retailer, nmiss(parent_code)/count(*) as nm_parent
from drugstore; 
quit; 

proc sql noprint; 
	create table nstore as 
	select parent_code, count(unique(retailer_code)) as n_retailer, count(unique(store_code_uc)) as n_store
	from drugstore
	group by parent_code;
quit;

* -------------------------------------------------------*;
* Check if HMS and RMS have all the retailer_code overlap*; 
* Fewer retailers in RMS data; 
proc sql; 
	select count(unique(retailer_code)) as nretail_hms from hms_drugstore;
	select count(unique(retailer_code)) as nretail_rms from drugstore;
quit; 

* Check if the largest drug store identified by RMS and HMS is the same; 
proc sql noprint;
	* Overall expenditure at each retailer; 
	create table tmp1 as 
	select retailer_code, sum(total_spent) as dol
	from (select * from trips where retailer_code in (select retailer_code from hms_drugstore))
	group by retailer_code
	order by dol descending; 
	
	* Expenditure on cigarrets at each retailer from HMS; 
	create table tmp2 as 
	select retailer_code, sum(total_price_paid - coupon_value) as dol
	from 
	(select * from purchases where upc in (select upc from hms_products)) as A 
	inner join 
	(select * from trips where retailer_code in (select retailer_code from hms_drugstore)) as B
	on A.trip_code_uc = B.trip_code_uc
	group by retailer_code
	order by dol descending; 
	
	* Revenue on cigarretts at each retailer from RMS; 
	create table tmp3 as 
	select retailer_code, sum(price/prmult*units) as dol
	from (select * from mylib.movement where year(week_end)>= 2013) as A
	inner join
	drugstore as B
	on A.store_code_uc = B.store_code_uc
	group by retailer_code
	order by dol descending; 
quit;

* Print the largest drug stores; 
* In HMS, the store with retailer_code = 4904 is the largest, and the one with 4914 is the second; 
* In RMS, the store with retailer_code = 4904 is the largest, followed by 4901; 
* RMS does not track sales at retailer 4914; 
proc print data = tmp1(obs = 20); run; 
proc print data = tmp2(obs = 20); run;
proc print data = tmp3; run;

* Check if purchases before and after the CVS event fall similarly; 
proc sql noprint;
	* Weekly cigarrett sales for each retailer from HMS; 
	create table hms_week as 
	select retailer_code, week, sum(total_price_paid - coupon_value) as dol
	from 
	(select * from purchases where upc in (select upc from hms_products)) as A 
	inner join 
	(select *, purchase_date format=weekv6. as week 
		from trips where retailer_code in (select retailer_code from hms_drugstore) and year(purchase_date) = 2014) as B
	on A.trip_code_uc = B.trip_code_uc
	group by retailer_code, week
	order by retailer_code, week; 
	
	* Weekly cigarette sales for each store from RMS; 
	create table rms_week as 
	select retailer_code, week_end, sum(price/prmult*units) as dol
	from (select * from mylib.movement where year(week_end)=2014) as A
	inner join
	drugstore as B
	on A.store_code_uc = B.store_code_uc
	group by retailer_code, week_end
	order by retailer_code, week_end;
quit; 

* Plot the weekly sales; 
* In HMS, it is clear that the store with retailer_code 4914 had zero sales after October 2014; 
proc sgplot data = hms_week(where=(retailer_code in (4904, 4914, 4901, 4999, 4952, 4955))); 
	series x = week y = dol / group = retailer_code;
	title "Weekly cigarrette revenue by store from HMS"; 
run;

proc sgplot data = rms_week(where = (retailer_code in (4904, 4914, 4901, 4999, 4952, 4955) )); 
	series x = week_end y = dol / group = retailer_code;
	title "Weekly cigarrette revenue by store from RMS"; 
run;
proc sgplot data = rms_week(where = (retailer_code = 4914 )); 
	series x = week_end y = dol / group = retailer_code;
run;

* CVS has retailer_code = 4914 in HMS; 
* Subset RMS movement data for CVS; 
* retailer_code = 4914 has a few parent_code, 4901 is the drug store; 
data tmp; set orig_stores; where retailer_code = 4914; run;
data tmp; set orig_stores; where parent_code = 4901; run;
proc freq data = tmp; table retailer_code; run;
