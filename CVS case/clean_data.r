library(reshape2)
library(data.table)
library(lubridate)
library(xlsx)

setwd("~/Documents/Research/Tobacco/processed_data")
# plot.wd		<- "~/Desktop"
# setwd("U:/Users/ccv103/Documents/Research/tobacco/processed_data")
# setwd("/sscc/home/c/ccv103/Tobacco") 

panelists 	<- read.csv("tob_CVS_pan.csv", header = T)
purchases	<- read.csv("tob_CVS_purchases.csv", header = T)
trips		<- read.csv("tob_CVS_trips.csv", header = T)
names(panelists)	<- tolower(names(panelists))
ma.policy	<- read.xlsx("MA_policy.xlsx", 1)

# Define the construction of treatment and control
# 1: Control (smokers who never shopped at CVS), Treament (smokers who are also CVS shoppers)
# 2: Control (smokers who shopped at CVS but did not buy cigarettes), Treament (smokers who also purchased cigarettes at CVS)
# 3: Control (smokers who shopped at CVS but did not buy cigarettes), Treament (smokers who spent more than 17% CVS spending on cigarettes)
treat.code	<- 1	
(out.file 	<- paste(out.file, treat.code, sep="")	)	

# # Select a random sample
# sel		<- sample(unique(panelists$household_code), .1*length(unique(panelists$household_code)))
# panelists	<- subset(panelists, household_code %in% sel)
# trips 		<- subset(trips, household_code %in% sel)
# purchases	<- subset(purchases, household_code %in% sel )

#################
# Organize data # 
#################
# Add week
event.date	<- as.Date("2014-09-01", format = "%Y-%m-%d")			# Placebo event 
event.month	<- month(event.date) + 12
cvs.ret	<- 4914		# retailer_code for CVS
qunit	<- 20		# 20 cigaretts per pack 
firstw	<- as.Date("2012-12-31", format = "%Y-%m-%d") 	# The first week in 2013
purchases$purchase_date	<- as.Date(as.character(purchases$purchase_date), format = "%Y-%m-%d")
purchases$week		<- ((as.numeric(purchases$purchase_date - firstw)) %/%7 + 1)* 7 + firstw - 1
purchases$year		<- year(purchases$purchase_date)
purchases$month		<- month(purchases$purchase_date)
purchases$month		<- ifelse(purchases$year == 2012, 1, ifelse(purchases$year == 2013, purchases$month, purchases$month + 12))

trips$purchase_date	<- as.Date(as.character(trips$purchase_date), format = "%Y-%m-%d")
trips$week			<- ((as.numeric(trips$purchase_date - firstw)) %/%7 + 1)* 7 + firstw - 1
trips$year			<- year(trips$purchase_date)
trips$month			<- month(trips$purchase_date)
trips$month			<- ifelse(trips$year == 2012, 1, ifelse(trips$year == 2013, trips$month, trips$month + 12))
endweek				<- c(min(purchases$week), max(purchases$week))

# Mark CVS
trips$cvs	<- ifelse(trips$retailer_code == cvs.ret, 1, 0)
purchases	<- merge(purchases, trips[,c("trip_code_uc", "cvs", "channel_type")], by = "trip_code_uc", all.x=T)

# Mark the places that already implement tobacco ban
ma.policy$countynm	<- paste(toupper(substring(ma.policy$COUNTY, 1, 1)), tolower(substring(ma.policy$COUNTY, 2)), sep = "")
sort(unique(panelists[panelists$statecode == "MA","countynm"]))
cnty				<- c("Berkeley","Daly City","Healdsburg","Hollister","Marin","Richmond","San Francisco","Santa Clara", "Sonoma" )
panelists$ban_ard	<- with(panelists, 1*((statecode=="MA" & countynm %in% ma.policy$countynm)| (statecode=="CA" & countynm %in% cnty)))
sort(unique(panelists[panelists$ban_ard==1,"countynm"]))
table(panelists$ban_ard)

# Classify households distance to CVS
tmp	<- data.table(panelists)
tmp	<- tmp[,list(nzip = length(unique(panelist_zip_code)), nd = length(unique(distance_cvs))), by = list(household_code)]
summary(tmp)
mean(tmp$nzip>1)
mypan	<- panelists[panelists$panel_year == 2014,]			# Use 2014 panelist profile
median(mypan$distance_cvs, na.rm = T)
mypan$cvs_in2	<- ifelse(mypan$distance_cvs <=2, 1, 0)
mypan$wgr_in2	<- ifelse(mypan$distance_walgreens <=2, 1, 0)

# Classify light vs heavy smokers
tmp1		<- data.table(purchases)
setkeyv(tmp1, c("household_code", "purchase_date"))
tmp1		<- tmp1[, list(consum = sum(quantity*size/qunit, na.rm=T)/(as.numeric(max(purchase_date)-min(purchase_date)))*7), 
					by = list(household_code)]
summary(tmp1$consum)
tmp1$heavy 	<- 1*(tmp1$consum > 2.5)
tmp			<- setNames(tmp1$heavy, tmp1$household_code)
mypan$heavy <- tmp[as.character(mypan$household_code)]

# Distribution of the fraction of cigarette spending conditional on CVS visit
tmp	<- data.table(subset(purchases, cvs==1 & purchase_date < event.date))
tmp	<- tmp[,list(total_price_paid = sum(total_price_paid - coupon_value)), by = list(trip_code_uc)]
tmp	<- merge(trips[trips$cvs==1 & trips$purchase_date < event.date, ], tmp, by = "trip_code_uc", all.x = T)
tmp[is.na(tmp$total_price_paid), "total_price_paid"]	<- 0 
tmp <- data.table(tmp[,c("household_code", "total_price_paid", "total_spent", "purchase_date")])
tmp	<- tmp[,list(cig_frac = sum(total_price_paid)/sum(total_spent), 
                  cig_frac_cond = sum(total_price_paid[total_price_paid>0])/sum(total_spent[total_price_paid>0]) ),
            by = list(household_code)]
tmp[is.na(tmp)]	<- 0
median(tmp[cig_frac>0,cig_frac])		
tmp$frac_seg	<- ifelse(tmp$cig_frac ==0, "Zero", ifelse(tmp$cig_frac <= median(tmp[cig_frac>0,cig_frac]), "S1", "S2"))
mypan	<- merge(mypan, tmp[,list(household_code, frac_seg)], by = "household_code", all.x=T)
mypan[is.na(mypan$frac_seg), "frac_seg"]	<- "Never"
mypan$frac_seg	<- factor(mypan$frac_seg, levels = c("Never","Zero", "S1", "S2"))
table(mypan$frac_seg)

# Collapse demographic levels
new.col		<- list(setNames(c(2500,6500, 	9000,	11000,	13500, 	17500,	22500,	27500, 	32500,	37500, 	42500,	47500, 55000, 	65000, 	75000, 100000), 
							 c(3,	4,		6,		8,		10,		11,		13,		15,		16,		17,		18,		19,		21,		23,		26,		27)), 		# Income code		# Income code
					setNames(c(NA, 23, 27, 32, 37, 42, 47, 52, 60, 65), 0:9),				# Age code
					setNames(c(rep(1,8), 0), 1:9),											# Kids code
					setNames(c(rep(c("House","Condo"), 3), "Mobile"), 1:7), 				# Residence code
					setNames(c("White", "African American", "Asian", "Other"), 1:4),		# Race code
					setNames(c(NA, rep("Employed", 3), "Unemployed", 
								rep(c("Employed", "Both employed", "Both employed", "Both employed", "Only one employed"), 3), 
								"Unemployed", "Only one employed", "Only one employed", "Only one employed", "Both unemployed"), 
							do.call(paste, expand.grid(c(0,1:3,9), c(0, 1:3, 9))) ) )						
names(new.col)	<- c("household_income", "age", "age_and_presence_of_children", "residence", "race", "employment")
new.col

mypan$income			<- new.col[["household_income"]][as.character(mypan$household_income)]
mypan$male_head_age		<- new.col[["age"]][as.character(mypan$male_head_age)]
mypan$female_head_age	<- new.col[["age"]][as.character(mypan$female_head_age)]
mypan$age				<- rowMeans(mypan[,c("female_head_age", "male_head_age")], na.rm=T)	
mypan$have_kids			<- new.col[["age_and_presence_of_children"]][as.character(mypan$age_and_presence_of_children)]
mypan$employment		<- paste(mypan$male_head_employment, mypan$female_head_employment)
mypan$employment		<- new.col[["employment"]][as.character(mypan$employment)]
mypan$employment		<- factor(mypan$employment, levels = c("Unemployed", "Employed", "Only one employed", "Both employed", "Both unemployed"))
mypan$race				<- factor(new.col[["race"]][as.character(mypan$race)], levels = new.col[["race"]])
demo.col				<- c("income", "age", "have_kids", "employment", "race")
sel						<- sapply(demo.col, function(i) is.numeric(mypan[,i]))
summary(mypan[,demo.col[sel]])
lapply(demo.col[!sel], function(i) table(mypan[,i]))

# -------------------------- #
# Fill in non-puchases months # 
# Complete month for each household 
tmp		<- data.table(trips)
tmp		<- tmp[,list(start = min(month), end = max(month)), by = list(household_code)]
tmp		<- tmp[, n:= end-start]
tmp1	<- lapply(1:nrow(tmp), function(i) tmp[i,start] + c(0:tmp[i,n]))
names(tmp1)	<- tmp$household_code
tmp1	<- melt(tmp1)
names(tmp1)	<- c("month", "household_code")
tmp1$household_code	<- as.numeric(tmp1$household_code)

# Actual cigarette purchases 
tmp2	<- data.table(purchases)
tmp2	<- tmp2[,list(	q = sum(quantity*size/qunit, na.rm=T), 
						q_othdrug = sum(quantity*size*(1-cvs)*1*(channel_type == "Drug Store")/qunit, na.rm=T), 
						q_othchannel = sum(quantity*size*1*(channel_type != "Drug Store")/qunit, na.rm=T), 
						q_convenience = sum(quantity*size*1*(channel_type == "Convenience Store")/qunit, na.rm=T), 
						q_grocery	= sum(quantity*size*1*(channel_type == "Grocery")/qunit, na.rm=T), 
						q_discount	= sum(quantity*size*1*(channel_type == "Discount Store")/qunit, na.rm=T), 
						q_service	= sum(quantity*size*1*(channel_type %in% c("Service Station"))/qunit, na.rm=T),
						q_gas		= sum(quantity*size*1*(channel_type %in% c("Gas Mini Mart"))/qunit, na.rm=T), 
						q_tobacco 	= sum(quantity*size*1*(channel_type %in% c("Tobacco Store"))/qunit, na.rm=T), 
						cigdol 		= sum(total_price_paid - coupon_value, na.rm=T), 
						cigdol_cvs	= sum((total_price_paid - coupon_value)*cvs, na.rm=T),
						cigdol_othdrug 	= sum((total_price_paid - coupon_value)*(1-cvs)*1*(channel_type == "Drug Store"), na.rm=T), 
						cigdol_othchannel= sum((total_price_paid - coupon_value)*1*(channel_type != "Drug Store"), na.rm=T)), 
				by = list(household_code, month)]
dim(tmp1); dim(tmp2)
mydata	<- merge(tmp1, tmp2, by = c("household_code", "month"), all.x = T)
dim(mydata)

# Trips and spending 
tmp1 	<- data.table(trips)
tmp1	<- tmp1[,list(total_spent = sum(total_spent)), by = list(household_code, month, purchase_date, channel_type, retailer_code, cvs)]
tmp1	<- tmp1[,list(	trip_cvs 		= length(purchase_date[cvs==1]), 
						trip_othdrug 	= length(purchase_date[channel_type == "Drug Store" & cvs ==0] ), 
						trip_othchannel = length(purchase_date[channel_type != "Drug Store"]), 
						trip_grocery	= length(purchase_date[channel_type == "Grocery"]), 
						trip_discount	= length(purchase_date[channel_type == "Discount Store"]), 
						trip_convenience= length(purchase_date[channel_type == "Convenience Store"]), 
						trip_service	= length(purchase_date[channel_type == "Service Station"]), 
						trip_gas		= length(purchase_date[channel_type == "Gas Mini Mart"]), 
						trip_total		= length(purchase_date),
						dol_cvs 		= sum(total_spent*cvs, na.rm = T), 
						dol_othdrug		= sum(total_spent*(1-cvs)*1*(channel_type == "Drug Store"), na.rm = T), 
						dol_othchannel	= sum(total_spent*1*(channel_type != "Drug Store"), na.rm = T), 
						dol_grocery		= sum(total_spent*1*(channel_type == "Grocery"), na.rm = T), 
						dol_discount	= sum(total_spent*1*(channel_type == "Discount Store"),na.rm=T), 
						dol_convenience	= sum(total_spent*1*(channel_type == "Convenience Store"),na.rm=T),  
						dol_service		= sum(total_spent*1*(channel_type == "Service Station"), na.rm=T),  
						dol_gas			= sum(total_spent*1*(channel_type == "Gas Mini Mart"),na.rm=T), 
						dol_total		= sum(total_spent)
						), 
				by = list(household_code, month)]
dim(tmp1)		
summary(tmp1[,list(trip_cvs, trip_othdrug, trip_othchannel)])
mydata	<- merge(mydata, tmp1, by = c("household_code", "month"), all.x = T)
sel 	<- is.na(mydata)
mydata[sel]	<- 0
mydata$netdol			<- with(mydata, dol_total - cigdol)
mydata$netdol_cvs		<- with(mydata, dol_cvs - cigdol_cvs)
mydata$netdol_othdrug	<- with(mydata, dol_othdrug - cigdol_othdrug)
mydata$netdol_othchannel<- with(mydata, dol_othchannel - cigdol_othchannel)
cat("Summary stats:\n"); print(summary(mydata)); cat("\n")

# Calculate pre-event shopping behavior for each household
# tmp2	<- tmp1[month < event.month,]
tmp2	<- data.table(subset(mydata, month < event.month))
tmp2	<- tmp2[,list(	pre_q				= mean(q), 
						pre_trip_cvs 		= mean(trip_cvs), 
						pre_trip_othdrug 	= mean(trip_othdrug), 
						pre_trip_othchannel = mean(trip_othchannel), 
						pre_trip_total		= mean(trip_total),
						pre_dol_cvs 		= mean(dol_cvs), 
						pre_dol_othdrug		= mean(dol_othdrug), 
						pre_dol_othchannel	= mean(dol_othchannel), 
						pre_dol_total		= mean(dol_total)), by = list(household_code)]
mypan	<- merge(mypan, tmp2, by = "household_code", all.x = T)	
bhv.col		<- c("pre_q", "pre_trip_total", "pre_trip_othchannel", "pre_dol_total", "pre_dol_othchannel")
sapply(bhv.col, function(i) sum(is.na(mypan[,i])))				
sel		<- apply(mypan[,bhv.col], 1, function(x) any(is.na(x)))
sum(sel)
#mypan	<- mypan[!sel,]

# Drop outliers
summary(mypan$pre_q)
sum(mypan$pre_q > 50); mean(mypan$pre_q > 50)
cat(sum(mypan$pre_q > 50), "housheolds are dropped as outliers because the average monthly cigarette consumption is greater than 50 packs. \n")
mypan <- subset(mypan, pre_q <= 50)
mydata<- subset(mydata, household_code %in% mypan$household_code)
dim(mypan); length(unique(mydata$household_code))

# Create other control variables: month 
mydata$year		<- ifelse(mydata$month > 12, 2014, 2013)
mydata$month1	<- mydata$month %% 12
mydata$month1	<- ifelse(mydata$month1 == 0, 12, mydata$month1)
mydata$month1	<- factor(mydata$month1)
mydata			<- merge(mydata, mypan[,c("household_code", "panelist_zip_code","distance_cvs", "ban_ard","cvs_in2", "wgr_in2", "heavy", "frac_seg")], 
						by = "household_code", all.x=T)
dim(mydata)
mydata$after	<- 1*(mydata$month >= event.month)

# Drop the households who did not have two observations every period
tmp		<- data.table(mydata)
tmp		<- unique(tmp, by = c("household_code", "after"))
tmp		<- tmp[, list(drop= 1*(length(after) < 2)), by = list(household_code)]
table(tmp$drop)
mypan	<- subset(mypan, household_code %in% tmp[drop==0,]$household_code)
mydata	<- subset(mydata, household_code %in% mypan$household_code)

# Define treatment group 
mydata$treat1		<- with(mydata, 1*(frac_seg != "Never" & ban_ard == 0 ))		
mypan$treat1 		<- with(mypan, 1*(frac_seg != "Never" & ban_ard == 0 ))         # Define treatment dummy

mydata$treat2		<- NA
mypan$treat2		<- NA
sel					<- mydata$frac_seg != "Never"
mydata[sel,"treat2"]<- with(mydata[sel,], 1*(frac_seg != "Zero" & ban_ard == 0 ))
sel					<- mypan$frac_seg != "Never"
mypan[sel,"treat2"]	<- with(mypan[sel,], 1*(frac_seg != "Zero" & ban_ard == 0 ))

mydata$treat3		<- NA
mypan$treat3		<- NA
sel					<- mydata$frac_seg %in% c("Zero", "S2")
mydata[sel,"treat3"]<- with(mydata[sel,], 1*(frac_seg != "Zero" & ban_ard == 0 ))
sel					<- mypan$frac_seg %in% c("Zero", "S2")
mypan[sel,"treat3"]	<- with(mypan[sel,], 1*(frac_seg != "Zero" & ban_ard == 0 ))	
table(mypan$treat1); table(mypan$treat2); table(mypan$treat3); 

smk.pan		<- mypan
smk.trips	<- mydata

rm(list = setdiff(ls(), c("smk.pan", "smk.trips")))

###################################
# Data of smokers and non-smokers # 
###################################
panelists 	<- read.csv("tob_CVS_pan.csv", header = T)
purchases	<- read.csv("tob_CVS_purchases.csv", header = T)
trips		<- read.csv("tob_CVS_trips.csv", header = T)
nsmk.pan 	<- read.csv("tob_CVS_nonsmk_pan.csv", header = T)
nsmk.trips	<- read.csv("tob_CVS_nonsmk_trips.csv", header = T)
ma.policy	<- read.xlsx("MA_policy.xlsx", 1)

panelists$smk	<- 1
nsmk.pan$smk	<- 0
trips$smk		<- 1
nsmk.trips$smk	<- 0
panelists		<- rbind(panelists, nsmk.pan)
trips			<- rbind(trips, nsmk.trips)
names(panelists)	<- tolower(names(panelists))

# # Select a random sample
# sel		<- sample(unique(panelists$household_code), .1*length(unique(panelists$household_code)))
# panelists	<- subset(panelists, household_code %in% sel)
# trips 		<- subset(trips, household_code %in% sel)
# purchases	<- subset(purchases, household_code %in% sel )

# Add week
event.date	<- as.Date("2014-09-01", format = "%Y-%m-%d")			# Placebo event 
event.month	<- month(event.date) + 12
cvs.ret	<- 4914		# retailer_code for CVS
qunit	<- 20		# 20 cigaretts per pack 
firstw	<- as.Date("2012-12-31", format = "%Y-%m-%d") 	# The first week in 2013
purchases$purchase_date	<- as.Date(as.character(purchases$purchase_date), format = "%Y-%m-%d")
purchases$week		<- ((as.numeric(purchases$purchase_date - firstw)) %/%7 + 1)* 7 + firstw - 1
purchases$year		<- year(purchases$purchase_date)
purchases$month		<- month(purchases$purchase_date)
purchases$month		<- ifelse(purchases$year == 2012, 1, ifelse(purchases$year == 2013, purchases$month, purchases$month + 12))

trips$purchase_date	<- as.Date(as.character(trips$purchase_date), format = "%Y-%m-%d")
trips$week			<- ((as.numeric(trips$purchase_date - firstw)) %/%7 + 1)* 7 + firstw - 1
trips$year			<- year(trips$purchase_date)
trips$month			<- month(trips$purchase_date)
trips$month			<- ifelse(trips$year == 2012, 1, ifelse(trips$year == 2013, trips$month, trips$month + 12))
endweek				<- c(min(purchases$week), max(purchases$week))

# Mark CVS
trips$cvs	<- ifelse(trips$retailer_code == cvs.ret, 1, 0)
purchases	<- merge(purchases, trips[,c("trip_code_uc", "cvs", "channel_type")], by = "trip_code_uc", all.x=T)

# Mark the places that already implement tobacco ban
ma.policy$countynm	<- paste(toupper(substring(ma.policy$COUNTY, 1, 1)), tolower(substring(ma.policy$COUNTY, 2)), sep = "")
sort(unique(panelists[panelists$statecode == "MA","countynm"]))
cnty		<- c("Berkeley","Daly City","Healdsburg","Hollister","Marin","Richmond","San Francisco","Santa Clara", "Sonoma" )
panelists$ban_ard	<- with(panelists, 1*((statecode=="MA" & countynm %in% ma.policy$countynm)| (statecode=="CA" & countynm %in% cnty)))
sort(unique(panelists[panelists$ban_ard==1,"countynm"]))
table(panelists$ban_ard)

# Classify households distance to CVS
tmp	<- data.table(panelists)
tmp	<- tmp[,list(nzip = length(unique(panelist_zip_code)), nd = length(unique(distance_cvs))), by = list(household_code)]
summary(tmp)
mean(tmp$nzip>1)
mypan	<- panelists[panelists$panel_year == 2014,]			# Use 2014 panelist profile
median(mypan$distance_cvs, na.rm = T)
mypan			<- subset(mypan, !is.na(distance_cvs))
mypan$cvs_in2	<- ifelse(mypan$distance_cvs <=2, 1, 0)
mypan$wgr_in2	<- ifelse(mypan$distance_walgreens <=2, 1, 0)

# Classify light vs heavy smokers
tmp1		<- data.table(purchases)
setkeyv(tmp1, c("household_code", "purchase_date"))
tmp1		<- tmp1[, list(consum = sum(quantity*size/qunit, na.rm=T)/(as.numeric(max(purchase_date)-min(purchase_date)))*7), 
					by = list(household_code)]
summary(tmp1$consum)
tmp1$heavy 	<- 1*(tmp1$consum > 2.5)
tmp			<- setNames(tmp1$heavy, tmp1$household_code)
mypan$heavy 	<- tmp[as.character(mypan$household_code)]
mypan[is.na(mypan$heavy), "heavy"]	<- 0

# Distribution of the fraction of cigarette spending conditional on CVS visit
tmp	<- data.table(subset(purchases, cvs==1 & purchase_date < event.date))
tmp	<- tmp[,list(total_price_paid = sum(total_price_paid - coupon_value)), by = list(trip_code_uc)]
tmp	<- merge(trips[trips$cvs==1 & trips$purchase_date < event.date, ], tmp, by = "trip_code_uc", all.x = T)
tmp[is.na(tmp$total_price_paid), "total_price_paid"]	<- 0 
tmp 	<- data.table(tmp[,c("household_code", "total_price_paid", "total_spent", "purchase_date")])
tmp		<- tmp[,list(cig_frac = sum(total_price_paid)/sum(total_spent), 
                  cig_frac_cond = sum(total_price_paid[total_price_paid>0])/sum(total_spent[total_price_paid>0]) ),
            by = list(household_code)]
tmp[is.na(tmp)]	<- 0
median(tmp[cig_frac>0,cig_frac])		
tmp$frac_seg	<- ifelse(tmp$cig_frac ==0, "Zero", ifelse(tmp$cig_frac <= median(tmp[cig_frac>0,cig_frac]), "S1", "S2"))
mypan	<- merge(mypan, tmp[,list(household_code, frac_seg)], by = "household_code", all.x=T)
mypan[is.na(mypan$frac_seg), "frac_seg"]	<- "Never"
mypan$frac_seg	<- factor(mypan$frac_seg, levels = c("Never","Zero", "S1", "S2"))
table(mypan$frac_seg)

# Collapse demographic levels
new.col		<- list(setNames(c(2500,6500, 	9000,	11000,	13500, 	17500,	22500,	27500, 	32500,	37500, 	42500,	47500, 55000, 	65000, 	75000, 100000), 
							 c(3,	4,		6,		8,		10,		11,		13,		15,		16,		17,		18,		19,		21,		23,		26,		27)), 		# Income code		# Income code
					setNames(c(NA, 23, 27, 32, 37, 42, 47, 52, 60, 65), 0:9),				# Age code
					setNames(c(rep(1,8), 0), 1:9),											# Kids code
					setNames(c(rep(c("House","Condo"), 3), "Mobile"), 1:7), 				# Residence code
					setNames(c("White", "African American", "Asian", "Other"), 1:4),		# Race code
					setNames(c(NA, rep("Employed", 3), "Unemployed", 
								rep(c("Employed", "Both employed", "Both employed", "Both employed", "Only one employed"), 3), 
								"Unemployed", "Only one employed", "Only one employed", "Only one employed", "Both unemployed"), 
							do.call(paste, expand.grid(c(0,1:3,9), c(0, 1:3, 9))) ) )						
names(new.col)	<- c("household_income", "age", "age_and_presence_of_children", "residence", "race", "employment")
new.col

mypan$income			<- new.col[["household_income"]][as.character(mypan$household_income)]
mypan$male_head_age		<- new.col[["age"]][as.character(mypan$male_head_age)]
mypan$female_head_age	<- new.col[["age"]][as.character(mypan$female_head_age)]
mypan$age				<- rowMeans(mypan[,c("female_head_age", "male_head_age")], na.rm=T)	
mypan$have_kids			<- new.col[["age_and_presence_of_children"]][as.character(mypan$age_and_presence_of_children)]
mypan$employment		<- paste(mypan$male_head_employment, mypan$female_head_employment)
mypan$employment		<- new.col[["employment"]][as.character(mypan$employment)]
mypan$employment		<- factor(mypan$employment, levels = c("Unemployed", "Employed", "Only one employed", "Both employed", "Both unemployed"))
mypan$race				<- factor(new.col[["race"]][as.character(mypan$race)], levels = new.col[["race"]])
demo.col				<- c("income", "age", "have_kids", "employment", "race")
sel						<- sapply(demo.col, function(i) is.numeric(mypan[,i]))
summary(mypan[,demo.col[sel]])
lapply(demo.col[!sel], function(i) table(mypan[,i]))
sel						<- apply(mypan[,demo.col], 1, function(x) any(is.na(x)))
cat(sum(sel), "Households have missing demogrpahics.\n")
mypan					<- mypan[!sel,]

# For this analysis, we only focus on CVS shoppers. 
cat(sum(mypan$frac_seg == "Never"),"households out of ", nrow(mypan), "never shopped at CVS, so drop them for this current analysis.\n")
mypan		<- subset(mypan, frac_seg != "Never")
purchases	<- subset(purchases, household_code %in% mypan$household_code)
trips		<- subset(trips, household_code %in% mypan$household_code)

# -------------------------- #
# Fill in non-puchases months # 
# Complete month for each household 
tmp		<- data.table(trips)
tmp		<- tmp[,list(start = min(month), end = max(month)), by = list(household_code)]
tmp		<- tmp[, n:= end-start]
tmp1	<- lapply(1:nrow(tmp), function(i) tmp[i,start] + c(0:tmp[i,n]))
names(tmp1)	<- tmp$household_code
tmp1	<- melt(tmp1)
names(tmp1)	<- c("month", "household_code")
tmp1$household_code	<- as.numeric(tmp1$household_code)

# Trips and spending 
tmp2 	<- data.table(trips)
tmp2	<- tmp2[,list(total_spent = sum(total_spent)), by = list(household_code, month, purchase_date, channel_type, retailer_code, cvs)]
tmp2	<- tmp2[,list(	trip_cvs 		= length(purchase_date[cvs==1]),  
						trip_othdrug 	= length(purchase_date[channel_type == "Drug Store" & cvs ==0] ), 
						trip_othchannel = length(purchase_date[channel_type != "Drug Store"]), 
						trip_grocery	= length(purchase_date[channel_type == "Grocery"]), 
						trip_discount	= length(purchase_date[channel_type == "Discount Store"]), 
						trip_convenience= length(purchase_date[channel_type == "Convenience Store"]), 
						trip_service	= length(purchase_date[channel_type == "Service Station"]), 
						trip_gas		= length(purchase_date[channel_type == "Gas Mini Mart"]), 
						dol_cvs 		= sum(total_spent*cvs, na.rm = T), 
						dol_othdrug		= sum(total_spent*(1-cvs)*1*(channel_type == "Drug Store"), na.rm = T), 
						dol_othchannel	= sum(total_spent*1*(channel_type != "Drug Store"), na.rm = T), 
						dol_grocery		= sum(total_spent*1*(channel_type == "Grocery"), na.rm = T), 
						dol_discount	= sum(total_spent*1*(channel_type == "Discount Store"),na.rm=T), 
						dol_convenience	= sum(total_spent*1*(channel_type == "Convenience Store"),na.rm=T),  
						dol_service		= sum(total_spent*1*(channel_type == "Service Station"), na.rm=T),  
						dol_gas			= sum(total_spent*1*(channel_type == "Gas Mini Mart"),na.rm=T), 
						dol_total		= sum(total_spent)
						), 
				by = list(household_code, month)]
dim(tmp1); dim(tmp2)		
sum(is.na(tmp2))
summary(tmp2[,list(trip_cvs, trip_othdrug, trip_othchannel)])
mydata	<- merge(tmp1, tmp2, by = c("household_code", "month"), all.x = T)
dim(mydata)

# Cigarette spending
# Actual cigarette purchases 
tmp3	<- data.table(purchases)
tmp3	<- tmp3[,list(	q = sum(quantity*size/qunit, na.rm=T),
						cigdol 		= sum(total_price_paid - coupon_value, na.rm=T), 
						cigdol_cvs	= sum((total_price_paid - coupon_value)*cvs, na.rm=T),
						cigdol_othdrug 	= sum((total_price_paid - coupon_value)*(1-cvs)*1*(channel_type == "Drug Store"), na.rm=T), 
						cigdol_othchannel= sum((total_price_paid - coupon_value)*1*(channel_type != "Drug Store"), na.rm=T)), 
				by = list(household_code, month)]
mydata	<- merge(mydata, tmp3, by = c("household_code", "month"), all.x = T)
sel 	<- is.na(mydata)
mydata[sel]	<- 0
mydata$netdol			<- with(mydata, dol_total - cigdol)
mydata$netdol_cvs		<- with(mydata, dol_cvs - cigdol_cvs)
mydata$netdol_othdrug	<- with(mydata, dol_othdrug - cigdol_othdrug)
mydata$netdol_othchannel<- with(mydata, dol_othchannel - cigdol_othchannel)
cat("Summary stats:\n"); print(summary(mydata[, -c(1:2)])); cat("\n")

# Calculate pre-event shopping behavior for each household
# NOTE: we have NAs for some trend measurement; 
tmp2	<- data.table(subset(mydata, month < event.month))
tmp2	<- tmp2[,list(	pre_q				= mean(q),
						pre_trip_cvs 		= mean(trip_cvs), 
						pre_trip_othdrug 	= mean(trip_othdrug), 
						pre_trip_othchannel = mean(trip_othchannel), 
						pre_dol_cvs 		= mean(dol_cvs), 
						pre_dol_othdrug		= mean(dol_othdrug), 
						pre_dol_othchannel	= mean(dol_othchannel)
						), by = list(household_code)]
summary(tmp2)
mypan	<- merge(mypan, tmp2, by = "household_code", all.x = T)	

# Check any missing values in the panelist data
demo.col
bhv.col		<- c("pre_trip_cvs", "pre_trip_othdrug", "pre_trip_othchannel", "pre_dol_cvs", "pre_dol_othdrug", "pre_dol_othchannel")
(bhv.col   <- paste(rep(bhv.col, 4), "_H", rep(1:4, each = 6), sep=""))
sapply(bhv.col, function(i) sum(is.na(mypan[,i])))				
sel		<- apply(mypan[,c(demo.col,bhv.col)], 1, function(x) any(is.na(x)))
if(sum(sel) > 0){
	cat(sum(sel), "households have missing values in their behavioral metrics, so we drop them for this analysis. \n")
	mypan	<- mypan[!sel,]
	mydata	<- subset(mydata, household_code %in% mypan$household_code)
	trips	<- subset(trips, household_code %in% mypan$household_code)
	purchases	<- subset(purchases, household_code %in% mypan$household_code)
}

# Drop outliers
summary(mypan$pre_q)
sum(mypan$pre_q > 50); mean(mypan$pre_q > 50)
cat(sum(mypan$pre_q > 50), "housheolds are dropped as outliers because the average monthly cigarette consumption is greater than 50 packs. \n")
mypan <- subset(mypan, pre_q <= 50)
mydata<- subset(mydata, household_code %in% mypan$household_code)
dim(mypan); length(unique(mydata$household_code))

# Create other control variables: month 
mydata$year		<- ifelse(mydata$month > 12, 2014, 2013)
mydata$month1	<- mydata$month %% 12
mydata$month1	<- ifelse(mydata$month1 == 0, 12, mydata$month1)
mydata$month1	<- factor(mydata$month1)
dim(mydata)
mydata			<- merge(mydata, mypan[,c("household_code", "panelist_zip_code", "distance_cvs", "cvs_in2", "wgr_in2", "heavy", "smk", "ban_ard", "frac_seg",demo.col)], 
						by = "household_code", all.x=T)
dim(mydata)
mydata$after	<- 1*(mydata$month >= event.month)

# Drop the households who did not have two observations every period
tmp		<- data.table(mydata)
tmp		<- unique(tmp, by = c("household_code", "after"))
tmp		<- tmp[, list(drop= 1*(length(after) < 2)), by = list(household_code)]
table(tmp$drop)
mypan	<- subset(mypan, household_code %in% tmp[drop==0,]$household_code)
mydata	<- subset(mydata, household_code %in% mypan$household_code)

# Construct treatment and control 
mydata$treat1	<- with(mydata, 1*(smk == 1 & ban_ard == 0))
mypan$treat1	<- with(mypan, 1*(smk == 1 & ban_ard == 0))

mydata$treat2		<- NA
mypan$treat2		<- NA
sel					<- !(mydata$frac_seg == "Zero" & mydata$smk == 1 & mydata$treat1 == 1)
mydata[sel,"treat2"]<- mydata[sel,"treat1"]
sel					<- !(mypan$frac_seg == "Zero" & mypan$smk == 1 & mypan$treat1 == 1)
mypan[sel,"treat2"]	<- mypan[sel,"treat1"]

mydata$treat3		<- NA
mypan$treat3		<- NA
sel					<- !(mydata$frac_seg %in% c("Zero", "S1") & mydata$smk == 1 & mydata$treat1 == 1)
mydata[sel,"treat3"]<- mydata[sel,"treat1"]
sel					<- !(mypan$frac_seg %in% c("Zero", "S1") & mypan$smk == 1 & mypan$treat1 == 1)
mypan[sel,"treat3"]	<- mypan[sel,"treat1"]
table(mypan$treat1); table(mypan$treat2); table(mypan$treat3)

nonsmk.pan		<- mypan
nonsmk.trips	<- mydata


save(smk.pan, smk.trips, nonsmk.pan, nonsmk.trips, file = "~/Desktop/tobacco_cvs.rdata")

