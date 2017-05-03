%let dirname= U:\Users\ccv103\Documents\Research\Nielsen\nielsen_extracts\RMS;
libname mylib "U:\Users\ccv103\Documents\Research\tobacco\SAS_temp";
%let cvs_code  	= 4914; 
%let pack_q		= 20; 
%let event_date	= %sysfunc (inputn(20140901,YYMMDD8.)); 
%put &event_date; 

* 4998 is also CVS phamacy; 
* 4904 is Walgreen; 
* 4901 is RITE AID; 

*************;
* Read data *;
*************;
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
proc freq data = orig_products; table product_module_descr; run;

* Only focus on cigaretts and cigar; 
data my_products; 
	set orig_products; 
	where product_group_descr = "TOBACCO & ACCESSORIES";
	upcv = cats(upc, "-", upc_ver_uc); 
run;

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
	U:\Users\ccv103\Documents\Research\Nielsen\nielsen_extracts\RMS\2013\Annual_Files\stores_2013.tsv
	U:\Users\ccv103\Documents\Research\Nielsen\nielsen_extracts\RMS\2014\Annual_Files\stores_2014.tsv
	U:\Users\ccv103\Documents\Research\Nielsen\nielsen_extracts\RMS\2015\Annual_Files\stores_2015.tsv		
	;
run;
proc freq data = orig_stores; table channel_code; run;

* NOTE: RMS does not track CVS; 
proc freq data = orig_stores(where = (channel_code = "D")); table retailer_code; run;

* Check if any duplicated stores in each county-channel; 
proc sql; 
	select count(unique(store_code_uc)) as num_unique, 
		   count(store_code_uc) as num_all
	from orig_stores;
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
	U:\Users\ccv103\Documents\Research\Nielsen\nielsen_extracts\RMS\2015\Annual_Files\rms_versions_2015.tsv				
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

* Read data from 2006 to 2013; 
data tmp_file; 
	length filep $300. folder $300.; 
	set movename; 
	do year=2013 to 2015;
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

proc datasets noprint; delete movename tmp_file orig_products orig_upc_ver; run;

/*data sub_move;
	set movement; 
	if ranuni(11) > .05 then delete; 
run;
*/
*******************************;
* Price level at each channel *; 
*******************************;
ods pdf file="U:\Users\ccv103\Desktop\cvs_rms.pdf";

proc sql noprint;
	* Merge upc_ver_uc into the movement data; 
	create table tmp as 
	select A.*, ifn(upc_ver_uc=., 1, upc_ver_uc) as upc_ver_uc
	from (	select *, year(week_end) as year 
		  	from movement(drop = feature display) 
			where upc in (select upc from my_products ) ) as A 
	inner join upc_ver as B
	on A.upc = B.upc and A.year = B.year;
	
	create table sales as 
	select A.*, (price/prmult)*units as dol_sales, units*size/&pack_q as volume,  price/prmult/(size/&pack_q) as pack_price,
			B.channel_code, B.fips_state_descr, B.fips_county_descr, B.retailer_code,
			ifn(week_end > &event_date, 1,0) as after, month(week_end) as month
	from (select C.*, D.size1_amount*D.multi as size, brand_descr
		  from tmp as C inner join my_products as D on 
		  C.upc = D.upc and C.upc_ver_uc = D.upc_ver_uc) as A 
		left join orig_stores as B
	on A.store_code_uc = B.store_code_uc and A.year = B.year
	order by upc, week_end; 

	drop table tmp; 
	
	create table retailer_sales as 
	select upc, upc_ver_uc, week_end, channel_code, retailer_code, year, month, mean(after) as after,
			sum(dol_sales) as dol_sales, sum(volume) as volume, sum(dol_sales)/sum(volume) as pack_price
	from sales
	group by upc, upc_ver_uc, week_end, channel_code, retailer_code, year, month
	order by upc, week_end, channel_code; 
quit;	

data retailer_sales; 
	set retailer_sales; 
	log_price = log(pack_price); 
run;

/** Average price per pack at each chanel; 
proc sql noprint; 
	create table tmp as 
	select channel_code, sum(dol_sales) as revenue, sum(volume) as q, sum(dol_sales)/sum(volume) as price
	from retailer_sales 
	group by channel_code; 
quit; 
proc sort data = tmp; by descending price; run;
proc print data = tmp; run;*/

* Average price before and after the even at each channel; 
proc sql noprint; 
	create table tmp1 as 
	select channel_code, after, sum(dol_sales) as revenue, sum(volume) as q, sum(dol_sales)/sum(volume) as price
	from retailer_sales 
	group by channel_code, after; 
quit; 
proc sort data = tmp1; by channel_code after; run;
proc print data = tmp1; run;

* Regressions of price at diffent channels; 
proc sql; 
	select count(unique(upc)) as n_upc, count(unique(week_end)) as n_week from retailer_sales; 
quit; 
proc sort data = retailer_sales; by upc week_end; run;

proc glm data = retailer_sales(where = (week_end < &event_date)); 
	absorb upc week_end; 
	class channel_code (ref="D"); 
	model log_price = channel_code/solution noint; 
	title 'Price difference across channels before event'; 
run;

*************************************;
* Market adjustment after the event *; 
*************************************;
%let sht_date1	= %sysfunc (inputn(20140601,YYMMDD8.)); 
%let sht_date2	= %sysfunc (inputn(20141201,YYMMDD8.)); 
%put &sht_date1, &sht_date2; 

proc freq data = retailer_sales(where = (channel_code = "D")); table retailer_code; run;

proc format;
value drug_fmt
	4999 = 'Other'
	4901 = 'Rite Aid'
	4904 = 'Walgreens'
	4926 = 'Duane Reade'
	4931 = 'Kerr drug'
	4934 = 'Super D drug'
	4954 = 'Kinney drug'
	4960 = 'Shopko'
	; 
run;

* Price within drug stores and during the 3-window period; 
proc glm data = retailer_sales(where = (channel_code = "D" & week_end >= &sht_date1 & week_end < &sht_date2 )) ; 
	format retailer_code drug_fmt.; 
	absorb upc; 
	class retailer_code; 
	model log_price = retailer_code after retailer_code*after/solution; 
	title 'Retailer price adjustment within drug stores during 3-month window'; 	
run;

* Price within drug stores and during the 3 years; 
proc glm data = retailer_sales(where = (channel_code = "D" )) ; 
	format retailer_code drug_fmt.; 
	absorb upc month; 
	class retailer_code; 
	model log_price = retailer_code after retailer_code*after/solution; 
	title 'Retailer price adjustment within drug stores during 3 years'; 
run;

* Price across channels during the 3-window period; 
proc glm data = retailer_sales(where = ( week_end >= &sht_date1 & week_end < &sht_date2 )) ; 
	absorb upc; 
	class channel_code(ref = "D"); 
	model log_price = channel_code after channel_code*after/solution; 
	title 'Retailer price adjustment across channels during 3-month window'; 
run;

* Price across channels during the 3-window period; 
proc glm data = retailer_sales ; 
	absorb upc; 
	class channel_code(ref = "D"); 
	model log_price = channel_code after channel_code*after/solution; 
	title 'Retailer price adjustment across channels during 3 years'; 	
run;

ods pdf close;



