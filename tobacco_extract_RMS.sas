%let dirname= E:\Users\ccv103\Documents\Research\Nielsen\nielsen_extracts\RMS;
libname mylib "E:\Users\ccv103\Documents\Research\tobacco\SAS_temp";

%let outfile = %sysfunc(catx(%str(), E:\Users\ccv103\Desktop\graph_cigar_, &sysdate9.,.pdf));
%put &outfile; 

*************;
* Read data *;
*************;
PROC IMPORT OUT= WORK.mycounty
            DATAFILE= "\\tsclient\Resear1\Tobacco\processed_data\county_treatment.csv" 
            DBMS=csv REPLACE;
     		GETNAMES=YES; 
RUN;

data mycounty; 
	set mycounty; 
	fips	= cats(put(fips_state_code, z2.), put(fips_county_code, z3.)); 
run;

PROC IMPORT OUT= WORK.store_pharmacy
            DATAFILE= "\\tsclient\Resear1\Tobacco\processed_data\pharmacy_store.csv" 
            DBMS=csv REPLACE;
     		GETNAMES=YES; 
RUN;

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

* Check size units; 
proc freq data = my_products; table size1_units size1_amount size1_change_flag_uc upc_ver_uc; run;

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
	E:\Users\ccv103\Documents\Research\Nielsen\nielsen_extracts\RMS\2006\Annual_Files\stores_2006.tsv
	E:\Users\ccv103\Documents\Research\Nielsen\nielsen_extracts\RMS\2007\Annual_Files\stores_2007.tsv
	E:\Users\ccv103\Documents\Research\Nielsen\nielsen_extracts\RMS\2008\Annual_Files\stores_2008.tsv
	E:\Users\ccv103\Documents\Research\Nielsen\nielsen_extracts\RMS\2009\Annual_Files\stores_2009.tsv
	E:\Users\ccv103\Documents\Research\Nielsen\nielsen_extracts\RMS\2010\Annual_Files\stores_2010.tsv
	E:\Users\ccv103\Documents\Research\Nielsen\nielsen_extracts\RMS\2011\Annual_Files\stores_2011.tsv
	E:\Users\ccv103\Documents\Research\Nielsen\nielsen_extracts\RMS\2012\Annual_Files\stores_2012.tsv
	E:\Users\ccv103\Documents\Research\Nielsen\nielsen_extracts\RMS\2013\Annual_Files\stores_2013.tsv
	;
run;

* Subset the focial retailers in the treatment/control counties; 
proc sql noprint; 
	create table my_stores as 
	select *
	from (select *, cats(put(fips_state_code, z2.), put(fips_county_code, z3.)) as fips from orig_stores)
	where fips in (select fips from mycounty); 
quit;

* Check if any duplicated stores in each county-channel; 
proc sql; 
	select count(unique(store_code_uc)) as num_unique, 
		   count(store_code_uc) as num_all
	from my_stores;
quit; 

proc sort data = my_stores nodupkeys out = my_unique_stores(drop = year); 
	by fips_county_descr channel_code store_code_uc; 
run;

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
	E:\Users\ccv103\Documents\Research\Nielsen\nielsen_extracts\RMS\2006\Annual_Files\rms_versions_2006.tsv
	E:\Users\ccv103\Documents\Research\Nielsen\nielsen_extracts\RMS\2007\Annual_Files\rms_versions_2007.tsv
	E:\Users\ccv103\Documents\Research\Nielsen\nielsen_extracts\RMS\2008\Annual_Files\rms_versions_2008.tsv
	E:\Users\ccv103\Documents\Research\Nielsen\nielsen_extracts\RMS\2009\Annual_Files\rms_versions_2009.tsv
	E:\Users\ccv103\Documents\Research\Nielsen\nielsen_extracts\RMS\2010\Annual_Files\rms_versions_2010.tsv
	E:\Users\ccv103\Documents\Research\Nielsen\nielsen_extracts\RMS\2011\Annual_Files\rms_versions_2011.tsv
	E:\Users\ccv103\Documents\Research\Nielsen\nielsen_extracts\RMS\2012\Annual_Files\rms_versions_2012.tsv
	E:\Users\ccv103\Documents\Research\Nielsen\nielsen_extracts\RMS\2013\Annual_Files\rms_versions_2013.tsv		
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

* Read data from 2008 to 2011; 
data tmp_file; 
	length filep $300. folder $300.; 
	set movename; 
	do year=2006 to 2013;
		folder = cats("E:\Users\ccv103\Documents\Research\Nielsen\nielsen_extracts\RMS\",year,"\Movement_Files\",department,"_",year);
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

proc datasets noprint; delete movename tmp_file orig_products orig_upc_ver; run;

******************;
* Data selection *; 
******************;
* Select the RMS data in the focal geographic markets; 
proc sql noprint;
	* Merge upc_ver_uc into the movement data; 
	create table tmp as 
	select A.*, ifn(upc_ver_uc=., 1, upc_ver_uc) as upc_ver_uc
	from (	select *, year(week_end) as year 
		  	from movement(drop = feature display) where upc in (select upc from my_products )) as A 
		left join upc_ver as B
	on A.upc = B.upc and A.year = B.year;
	
	create table sales as 
	select A.*, B.channel_code, B.fips_state_descr, B.fips_county_descr, pharmacy2 as pharmacy
	from (select C.*, D.size1_amount*D.multi as size, brand_descr, upc_descr
		  from tmp as C inner join my_products as D on 
		  C.upc = D.upc and C.upc_ver_uc = D.upc_ver_uc) as A 
		inner join store_pharmacy as B
	on A.store_code_uc = B.store_code_uc and A.year = B.year; 
	
	drop table tmp; 
quit;

**********************; 
* Export to CSV data *; 
**********************; 
PROC EXPORT DATA= my_unique_stores
            OUTFILE= "E:\Users\ccv103\Desktop\stores.csv"
            DBMS=csv REPLACE;
     PUTNAMES=YES;
RUN;

* CA data; 
PROC EXPORT DATA= sales(where = (fips_state_descr = "CA"))
            OUTFILE= "E:\Users\ccv103\Desktop\sale_CA.csv"
            DBMS=csv REPLACE;
     PUTNAMES=YES;
RUN;

* MA data; 
PROC EXPORT DATA= sales(where = (fips_state_descr = "MA"))
            OUTFILE= "E:\Users\ccv103\Desktop\sale_MA.csv"
            DBMS=csv REPLACE;
     PUTNAMES=YES;
RUN;

*****************************;
* R code to generate .rdata *; 
*****************************;
/*
setwd("E:/Users/ccv103/Desktop")
stores <- read.csv("stores.csv", header = T)
names(stores)	<- tolower(names(stores))

sales_CA	<- read.csv("sale_CA.csv", header = T)
names(sales_CA)	<- tolower(names(sales_CA))

save(stores, sales_CA, file = "sales_CA.rdata")

sales_MA	<- read.csv("sale_MA.csv", header = T)
names(sales_MA)	<- tolower(names(sales_MA))
save(stores, sales_MA, file = "sales_MA.rdata")

*/






 