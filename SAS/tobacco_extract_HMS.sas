/*
Two selection criterion: 
1. Households live in the counties that are either in the treatment or control; 
2. Households must have made at least two purchases from the category; 
*/

LIBNAME mysqllib OLEDB
OLEDB_SERVICES=NO
Datasource="kdc01\kdwh02"
PROVIDER=SQLOLEDB.1
Properties=('Initial Catalog'=USRDB_ccv103
                  'Integrated Security'=SSPI)
SCHEMA=DBO
BULKLOAD=YES
bl_options='ROWS_PER_BATCH = 500000';

%let dirname= E:\Users\ccv103\Documents\Research\Nielsen\nielsen_extracts\HMS;
options compress=yes reuse=yes; 

* Read the treatment counties and control counties;
PROC IMPORT OUT= WORK.mycounty
            DATAFILE= "\\tsclient\Resear1\Tobacco\processed_data\county_treatment.csv" 
            DBMS=csv REPLACE;
     		GETNAMES=YES; 
RUN;

data mycounty; 
	set mycounty; 
	fips	= cats(put(fips_state_code, z2.), put(fips_county_code, z3.)); 
run; 

* Read product file and only focus on cigarettes; 
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
	length upcv $25.; 
	set orig_products; 
	where product_module_descr in ("CIGARETTES"); 
	upcv = cats(upc, "-", upc_ver_uc); 
run;

* Read master retailer data;
%let fpath = &dirname\Master_Files\Latest\retailers.tsv;
%put &fpath;
PROC IMPORT OUT= WORK.orig_retailers
            DATAFILE= "&fpath" 
            DBMS=dlm REPLACE;
			DELIMITER ='09'x;
     		DATAROW=2; 
			GUESSINGROWS=5000;
RUN;

* Read demographic and purchase data; 
* Read UPC-level purchase data;
%macro read_groc_fun;
	%do i=2008 %to 2012;
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
		
		* Sebsetting 1: restrict to the households who live in the focal counties; 
		data panelists&i; 
			set panelists&i(drop = projection_factor_magnet male_head_birth female_head_birth scantrack_market_code dma_code region_code
									kitchen_appliances tv_items household_internet_connection wic_indicator_current 
									wic_indicator_ever_notcurrent Member:); 
			fips = cats(put(fips_state_code, z2.), put(fips_county_code, z3.)); 
		run;
		
		proc sql noprint;
			create table tmp_demo as 
			select *
			from panelists&i 
			where fips in (select fips from mycounty); 
		
			create table tmp_trip as 
			select *
			from trips&i 
			where household_code in (select household_code from tmp_demo); 
			
		* Subsetting 2: only the purchase made on the category; 
			create table tmp_purchase as 
			select household_code, purchase_date, upc, upc_ver_uc, upcv, quantity as units, total_price_paid, 
					retailer_code, total_spent
			from (select C.*, upcv from purchases&i as C
					inner join my_products as D 
					on C.upc = D.upc and C.upc_ver_uc = D.upc_ver_uc) as A
			inner join tmp_trip as B
			on A.trip_code_uc = b.trip_code_uc; 
		quit; 
		
		%if &i=2008 %then %do; 
			data panelists;	set tmp_demo; run; 
			data purchases; length upcv $25.; set tmp_purchase; run;
		%end;
		%else %do;
			data panelists;	set panelists tmp_demo; run;
			data purchases; length upcv $25.; set purchases tmp_purchase; run; 
		%end; 
		proc datasets noprint; delete purchases&i trips&i panelists&i tmp:; run;
	%end;
%mend read_groc_fun;
%read_groc_fun;

* Notice that we only have 681 households who have purchased the category from all 3226 houoseholds in the panel; 
proc sql; 
	select count(unique(household_code)) as n1 from panelists; 
	select count(unique(household_code)) as n2 from purchases; 	
quit; 

/* Select households with at least 5 purchases ; */
%let min_n = 5; 
proc sql noprint;
	create table tmp as 
	select household_code, count(purchase_date) as n
	from purchases
	group by household_code; 
quit; 

proc univariate data = tmp; var n; run;

data tmp; 
	set tmp; 
	keep_ind = 1 * (n >= &min_n); 
run;
proc freq data = tmp; table keep_ind; run; 

* Subset panelist and purchase to the households with keep flag; 
proc sql noprint; 
	create table tmp_panelists as 
	select *
	from panelists
	where household_code in (select household_code from tmp where keep_ind = 1)
	order by household_code; 
	
	create table tmp_purchases as 
	select *
	from purchases 
	where household_code in (select household_code from tmp where keep_ind = 1)
	order by household_code, purchase_date; 
	
	* Append channel type;
	create table purchases as 
	select A.*, B.channel_type
	from tmp_purchases as A left join orig_retailers as B
	on A.retailer_code = B.retailer_code
	order by household_code, purchase_date; 
quit; 
data panelists; set tmp_panelists; run; 

* Export to CSV format; 
PROC EXPORT DATA= my_products
            OUTFILE= "E:\Users\ccv103\Desktop\cig_prod.csv"
            DBMS=csv REPLACE;
     PUTNAMES=YES;
RUN;

PROC EXPORT DATA= panelists
            OUTFILE= "E:\Users\ccv103\Desktop\cig_panelists.csv"
            DBMS=csv REPLACE;
     PUTNAMES=YES;
RUN;

PROC EXPORT DATA= purchases
            OUTFILE= "E:\Users\ccv103\Desktop\cig_purchases.csv"
            DBMS=csv REPLACE;
     PUTNAMES=YES;
RUN;


*****************************;
* R code to generate .rdata *; 
*****************************;
/*
setwd("E:/Users/ccv103/Desktop")
prod		<- read.csv("cig_prod.csv", header = T)
names(prod)	<- tolower(names(prod))

panelists	<- read.csv("cig_panelists.csv", header = T)
names(panelists)	<- tolower(names(panelists))

purchases	<- read.csv("cig_purchases.csv", header = T)
names(purchases)	<-	tolower(names(purchases))

county		<- read.csv("//tsclient/Resear1/Tobacco/processed_data/county_treatment.csv", header = T)

save(prod, panelists, purchases, county, file = "cigar_HMS.rdata")

*/