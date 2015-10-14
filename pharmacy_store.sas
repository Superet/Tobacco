%let dirname= E:\Users\ccv103\Documents\Research\Nielsen\nielsen_extracts\RMS;
libname mylib "E:\Users\ccv103\Documents\Research\tobacco\SAS_temp";

%let outfile = %sysfunc(catx(%str(), E:\Users\ccv103\Desktop\store_pharmacy,.csv));
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
	where product_group_descr = "MEDICATIONS/REMEDIES/HEALTH AIDS";  
	upcv = cats(upc, "-", upc_ver_uc); 
run;
proc freq data = my_products; table product_module_descr; run;

* Only focus on cigaretts and cigar; 
data my_products; 
	retain upcv upc; 
	set my_products; 
	where product_module_descr in ("ANTI-GAS PRODUCTS" "BLOOD PRESSURE KIT AND ACCESSORY" "BLOOD URINE STOOL TEST PRODUCTS" 
			"DIARRHEA REMEDIES" "DIURETIC REMEDIES" "OVULATION AND FERTILITY TEST KITS" 
			"PAIN REMEDIES - ARTHRITIS" "PAIN REMEDIES - BACK & LEG" "PAIN REMEDIES - CHILDREN'S LIQUID" 
			"PAIN REMEDIES - PRE MENSTRUAL" "PREGNANCY TEST KITS" ); 	
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

* Check if the key is unique;
proc sort data = my_stores nodupkeys out = tmp; by store_code_uc year; run;

* Check if any duplicated stores in each county-channel; 
proc sql; 
	select count(unique(store_code_uc)) as num_unique, 
		   count(store_code_uc) as num_all
	from my_stores;
quit; 

proc sort data = my_stores nodupkeys out = my_unique_stores(drop = year); 
	by fips_county_descr channel_code store_code_uc; 
run;


*---------------------------------------------------*;
* Manually input category number of filename; 
* RMS file schema: folder name = DepartmentNumber_year; 
*				   file name = ModuleNumber_year; 
proc sort data = my_products nodupkeys out = movename(keep = product_group_code product_module_code);
by product_group_code product_module_code; 
run;

* Read data from 2008 to 2011; 
data tmp_file; 
	length filep $300. folder $300.; 
	set movename; 
	do year=2006 to 2013;
		folder = cats("E:\Users\ccv103\Documents\Research\Nielsen\nielsen_extracts\RMS\",year,"\Movement_Files\",product_group_code,"_",year);
		filen = cats(product_module_code,'_',year,'.tsv');
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

*****************************;
* Determine pharmacy stores *; 
*****************************;
proc sql noprint;
	* Select the RMS data in the focal geographic markets; 
	create table sales as 
	select *, year(week_end) as year from movement(drop = feature display) 
	where store_code_uc in (select store_code_uc from my_unique_stores) and 
		upc in (select upc from my_products); 
	
	* Aggregate sales by year; 
	create table annual_sale as
	select store_code_uc, year, sum(units * price) as dol
	from sales 
	group by store_code_uc, year
	order by store_code_uc, year; 
quit;


* Plot the distribution of annual sales of grouped products; 
proc univariate data = annual_sale; 
	var dol; 
	histogram;
run; 

*---------------*; 
* Classify stores; 
proc sql noprint; 
	create table store_pharm as
	select A.*, dol as medication_sale
	from my_stores as A left join annual_sale as B
	on A.store_code_uc = B.store_code_uc and A.year = B.year; 
quit; 

proc univariate data = store_pharm; 
	class channel_code; 
	var medication_sale; 
	histogram;
run;

* Set the sales threshold such that no convenience store is classified as pharmacy; 
proc sql noprint;
	select max(medication_sale) into: mypharm_thresh 
	from (select * from store_pharm where channel_code = "C"); 
quit; 
%put &mypharm_thresh;

* Set P1 of sales in drug stores as threshold; 
proc univariate data = store_pharm(where = (channel_code = 'D' and medication_sale > 0)); 
	var medication_sale; 
	output out = tmp P1 = value; 
run;
data _NULL_;
	set tmp; 
	call symput('mypharm_thresh2', value); 
run;
%put &mypharm_thresh2; 

data store_pharm;
	set store_pharm; 
	pharmacy1 = 1* (medication_sale > &mypharm_thresh + 1); 
	if channel_code = 'D' then pharmacy1 = 1; 
	pharmacy2 = 1* (medication_sale > &mypharm_thresh2 + 1); 
	if channel_code = 'D' then pharmacy2 = 1; 
run;
proc contents data = store_pharm; run;

proc freq data = store_pharm;
	table channel_code * pharmacy1; 
	table channel_code * pharmacy2; 
run;

*-------------------------------------------------------------------------- *;
* Correct classification to keep consistency of pharmacy for the same store *; 
* Check if all stores belong to the same channel over years; 
/*There are three stores that changed channel types because of merges; */
proc sql noprint; 
	create table tmp as 
	select *
	from store_pharm 
	where store_code_uc in 
	(select store_code_uc from 
			(select store_code_uc, count(unique(channel_code)) as n_channel
			from store_pharm group by store_code_uc)
	where n_channel > 1); 
quit; 

* Check if stores are consistently pharmacy over year; 
proc means data =store_pharm noprint nway; 
	class channel_code parent_code store_code_uc; 
	var pharmacy2; 
	output out = tmp1 Mean = ; 
run;

* For 3 stores that changed channel types, their pharmacy identity also changed; 
proc sql; 
	select * from tmp1 where store_code_uc in (select store_code_uc from tmp); 
	select channel_code, store_code_uc, year, parent_code, medication_sale, pharmacy1, pharmacy2, fips_county_descr 
	from store_pharm where store_code_uc in (select store_code_uc from tmp); 
quit; 

* For the same stores that belong to the same parent code, we assign the same pharmacy flag; 
* Using votes by majority rule, the stores that are classified as pharmacy more than half the times are pharmacy; 
data tmp1; 
	set tmp1(rename = (pharmacy2 = p) drop = _TYPE_ _FREQ_); 
	pharmacy2 = 1*(p > .5); 
proc sort; by store_code_uc; 
proc freq; table channel_code * pharmacy2; 
run;

* Merge the new pharmacy flag to the store data; 
proc sort data = store_pharm; by channel_code parent_code store_code_uc; run;
proc sort data = tmp1; 		  by channel_code parent_code store_code_uc; run;
data store_pharm; 
	merge store_pharm(drop = pharmacy1 pharmacy2) tmp1(drop = p);
	by channel_code parent_code store_code_uc;
run;
proc sort data = store_pharm; by store_code_uc year; run;

proc datasets noprint; delete tmp tmp1 _doctmp:; run;

****************;
* Export data * ;
****************;
PROC EXPORT DATA= store_pharm
            OUTFILE= "\\tsclient\Resear1\Tobacco\processed_data\pharmacy_store.csv"
            DBMS=csv REPLACE;
     PUTNAMES=YES;
RUN;

