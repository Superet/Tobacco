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

########################
# Clean household data # 
########################
# Add week and format month variable
event.date	<- as.Date("2014-09-01", format = "%Y-%m-%d")			
event.month	<- month(event.date) + 12
cvs.ret	<- 4914		# retailer_code for CVS
qunit	<- 20		# 20 cigaretts per pack 
firstw	<- as.Date("2012-12-31", format = "%Y-%m-%d") 	# The first week in 2013
purchases$purchase_date	<- as.Date(as.character(purchases$purchase_date), format = "%Y-%m-%d")
purchases$week		<- ((as.numeric(purchases$purchase_date - firstw)) %/%7 + 1)* 7 + firstw - 1
purchases$year		<- year(purchases$purchase_date)
purchases$month		<- month(purchases$purchase_date)
purchases$month		<- (purchases$year - 2013)*12 + purchases$month

trips$purchase_date	<- as.Date(as.character(trips$purchase_date), format = "%Y-%m-%d")
trips$week			<- ((as.numeric(trips$purchase_date - firstw)) %/%7 + 1)* 7 + firstw - 1
trips$year			<- year(trips$purchase_date)
trips				<- subset(trips, year > 2012)
trips$month			<- month(trips$purchase_date)
trips$month			<- (trips$year - 2013)*12 + trips$month

# Mark CVS
trips$cvs	<- ifelse(trips$retailer_code == cvs.ret, 1, 0)
purchases	<- merge(purchases, trips[,c("trip_code_uc", "cvs", "channel_type")], by = "trip_code_uc", all.x=T)

# Mark the places that already implement tobacco ban
sort(unique(panelists[panelists$statecode == "MA","city"]))
cnty				<- c("Berkeley","Daly City","Healdsburg","Hollister","Marin","Richmond","San Francisco","Santa Clara", "Sonoma" )
panelists$ban_ard	<- with(panelists, 1*((statecode=="MA" & city %in% ma.policy$MUNICIPALITY)| (statecode=="CA" & countynm %in% cnty)))
sort(unique(panelists[panelists$ban_ard==1,"countynm"]))
table(panelists$ban_ard)

# Use 2014 panelist profile
tmp	<- data.table(panelists)
tmp	<- tmp[,list(nzip = length(unique(panelist_zip_code)), nd = length(unique(distance_cvs))), by = list(household_code)]
summary(tmp)
mean(tmp$nzip>1)
mypan	<- panelists[panelists$panel_year == 2014,]			

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
demo.col				<- c("income", "age", "have_kids", "employment", "race", "distance_cvs")
sel						<- sapply(demo.col, function(i) is.numeric(mypan[,i]))
summary(mypan[,demo.col[sel]])
lapply(demo.col[!sel], function(i) table(mypan[,i]))

# Check if any missing demographics
sel1 	<- apply(mypan[,demo.col[sel]], 1, function(x) any(is.na(x)))
cat(sum(sel1), "households have missing demographics.\n")

# --------------------# 
# Household selection #
# Household shopping profiles are constructed using data prior to the event, consumption are on the monthly basis 
# Selection criterion: 
# 1. Stay in the data before and after the event, 
# 2. No missing demographics 
# 3. Monthly cigarette consumption is less than 50 packs/month. 
# 4. Monthly cigarette spending at CVS is less than $300.
# 5. If Live in the cities that passed bans of tobacco sales at pharmacies, then they cannot have cigarette purchases at CVS. 
# 6. Distance to CVS is within 100 miles if they are CVS shoppers
max.q		<- 50
max.cvs		<- 300

tmppan		<- data.table(trips)
tmppan		<- tmppan[,list(start = min(month), end = max(month), dol_cvs = sum(total_spent*cvs*1*(month < event.month))), 
						by = list(household_code)]
tmppan		<- tmppan[, stay2 := 1*(start < event.month & end > event.month)]

tmp			<- data.table(subset(purchases, month < event.month))
tmp			<- tmp[,list(q = sum(quantity*size/qunit, na.rm=T), cigdol = sum(total_price_paid - coupon_value, na.rm=T), 
						 cigdol_cvs = sum(cvs*(total_price_paid - coupon_value), na.rm=T)), 
					by = list(household_code)]
tmppan		<- merge(tmppan, tmp, by = "household_code", all.x = T)
tmppan		<- tmppan[,':='(q = q/(event.month - start), cigdol = cigdol/(event.month - start), 
							cigdol_cvs = cigdol_cvs/(event.month - start), dol_cvs = dol_cvs/(event.month- start))]
tmppan		<- merge(tmppan, mypan[,c("household_code", "ban_ard", "distance_cvs")], by = "household_code", all.x=T)							
summary(tmppan)
sel			<- is.na(tmppan$q)
cat("We start with", nrow(tmppan), "households \n")
cat(sum(sel), "households did not purchased cigarettes before the event. \n")
cat(sum(tmppan$stay2==0), "households did not stay before and after the event.\n")
cat(sum(tmppan$q > max.q, na.rm=T), "households have monthly cigarette consumption greater than", max.q, "packs.\n")
cat(sum(tmppan$cigdol_cvs > max.cvs, na.rm=T), "households have monthly cigarette spending at CVS greater than", max.cvs, ".\n")
cat(sum(tmppan$ban_ardy == 1 & tmppan$cigdol_cvs > 0), "households live in the cities that passed tobacco ban but still have puchased cigar at CVS.\n")
cat(sum(tmppan$dol_cvs > 0 & tmppan$distance_cvs > 100),"CVS shoppers live beyond 100 miles from CVS.\n")

tmppan		<- tmppan[,drop := 1*(stay2 == 0 | q>max.q | cigdol_cvs > max.cvs | (ban_ard == 1 & cigdol_cvs > 0) | (dol_cvs > 0 & distance_cvs > 100))]
table(tmppan$drop)
tmppan		<- subset(tmppan, drop == 0)
summary(tmppan)

# Subset panelist data, purchase data and trip data
mypan		<- subset(mypan, household_code %in% tmppan$household_code)
purchases	<- subset(purchases, household_code %in% tmppan$household_code)
trips		<- subset(trips, household_code %in% tmppan$household_code)
mypan		<- mypan[order(mypan$household_code),]
max(abs(mypan$household_code - tmppan$household_code))

# ------------------ #
# Household segments # 
# Classify households distance to CVS
median(mypan$distance_cvs)
mypan$cvs_in2	<- ifelse(mypan$distance_cvs <=2, 1, 0)
mypan$wgr_in2	<- ifelse(mypan$distance_walgreens <=2, 1, 0)

# Classify light vs heavy smokers
median(tmppan$q)
mypan$heavy		<- ifelse(tmppan$q > 10, 1, 0)

# Distribution of the fraction of cigarette spending conditional on CVS visit
tmppan			<- tmppan[,cig_frac_cvs := cigdol_cvs/dol_cvs]
summary(tmppan$cig_frac_cvs)
summary(tmppan[cig_frac_cvs>0,cig_frac_cvs])
mypan$frac_seg	<- ifelse(is.na(tmppan$cig_frac_cvs), "Never", ifelse(tmppan$cig_frac_cvs ==0, "Zero", 
							ifelse(tmppan$cig_frac <= .2, "S1", "S2")))
mypan$frac_seg	<- factor(mypan$frac_seg, levels = c("Never","Zero", "S1", "S2"))
cat("Table of CVS shopper segment:\n"); print(table(mypan$frac_seg)); cat("\n")
cat(sum(mypan$ban_ard==1 & mypan$frac_seg %in% c("S1", "S2")), "households in SF or MA have bought cigarettes in CVS.\n")

# Define the construction of treatment and control
# 1: Control (smokers who never shopped at CVS), Treament (smokers who are also CVS shoppers)
# 2: Control (smokers who shopped at CVS but did not buy cigarettes), Treament (smokers who also purchased cigarettes at CVS)
# 3: Control (smokers who shopped at CVS but did not buy cigarettes), Treament (smokers who spent more than 17% CVS spending on cigarettes)
mypan$treat1 		<- with(mypan, 1*(frac_seg != "Never" & ban_ard == 0 ))         
cat("Table of frist construction:\n")
table(mypan$treat1)
table(mypan$treat1, mypan$frac_seg) 

mypan$treat2		<- NA
sel					<- mypan$frac_seg != "Never"
mypan[sel,"treat2"]	<- with(mypan[sel,], 1*(frac_seg != "Zero" & ban_ard == 0 ))
cat("Table of second construction:\n")
table(mypan$treat2)
table(mypan$treat2, mypan$frac_seg)

mypan$treat3		<- NA
sel					<- mypan$frac_seg %in% c("Zero", "S2") | (mypan$ban_ard == 1 & mypan$frac_seg != "Never")
mypan[sel,"treat3"]	<- with(mypan[sel,], 1*(frac_seg != "Zero" & ban_ard == 0 ))	
cat("Table of third construction:\n")
table(mypan$treat3)
table(mypan$treat3, mypan$frac_seg)

############################
# Organize regression data # 
############################
# -------------------------- #
# Fill in non-puchases months # 
# Complete month for each household 
tmp		<- data.table(trips)
tmp		<- tmp[,list(start = min(month), end = max(month)), 
					by = list(household_code)]
tmp		<- tmp[, n:= end-start]
tmp1	<- lapply(1:nrow(tmp), function(i) tmp[i,start] + c(0:tmp[i,n]))
names(tmp1)	<- tmp$household_code
tmp1	<- melt(tmp1)
names(tmp1)	<- c("month", "household_code")
tmp1$household_code	<- as.numeric(tmp1$household_code)

# Actual cigarette purchases 
tmp2	<- data.table(purchases)
tmp2	<- tmp2[,list(	q = sum(quantity*size/qunit, na.rm=T), 
						q_cvs 		= sum(quantity*size*cvs/qunit, na.rm=T),
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
mydata	<- mydata[order(mydata$household_code),]
for(i in 0:7){
	if(i == 0){
		tmpp	<- data.table(subset(mydata, month < event.month - 3*i))
	}else{
		tmpp	<- data.table(subset(mydata, month < event.month - 3*(i-1) & month >= event.month - 3*i))
	}
	print(unique(tmpp$month))
	tmpp	<- tmpp[,list(	pre_q				= mean(q), 
							pre_trip_cvs 		= mean(trip_cvs), 
							pre_trip_othdrug 	= mean(trip_othdrug), 
							pre_trip_othchannel = mean(trip_othchannel), 
							pre_trip_total		= mean(trip_total),
							pre_dol_cvs 		= mean(dol_cvs), 
							pre_dol_othdrug		= mean(dol_othdrug), 
							pre_dol_othchannel	= mean(dol_othchannel), 
							pre_dol_total		= mean(dol_total)), 
						by = list(household_code)]
	if(i == 0){
		predat	<- tmpp
		cat("dim(predat) = ", dim(predat), "\n")
	}else{
		names(tmpp)[-1]	<- paste(names(tmpp)[-1], i, sep="")
		print(identical(predat$household, tmpp$household_code))
		predat	<- merge(predat, tmpp, by = "household_code", all.x = T)
	}	
}	
cat("dim(predat) = ", dim(predat), "\n")	

dim(mypan)
mypan	<- merge(mypan, predat, by = "household_code", all.x = T)	
dim(mypan)
bhv.col		<- c("pre_q", "pre_trip_total", "pre_trip_othchannel", "pre_dol_total", "pre_dol_othchannel")
sapply(bhv.col, function(i) sum(is.na(mypan[,i])))				
sel		<- apply(mypan[,bhv.col], 1, function(x) any(is.na(x)))
sum(sel)

# Create other control variables: month 
mydata$year		<- ifelse(mydata$month > 12, 2014, 2013)
mydata$month1	<- mydata$month %% 12
mydata$month1	<- ifelse(mydata$month1 == 0, 12, mydata$month1)
mydata$month1	<- factor(mydata$month1)
mydata			<- merge(mydata, mypan[,c("household_code", "treat1", "treat2", "treat3","panelist_zip_code","distance_cvs", "ban_ard","cvs_in2", "wgr_in2", "heavy", "frac_seg")], 
						by = "household_code", all.x=T)
dim(mydata)
mydata$after	<- 1*(mydata$month >= event.month)
length(unique(mydata$household_code))
dim(mypan)

smk.pan		<- mypan
smk.trips	<- mydata

save(smk.pan, smk.trips, file = "cvs_smk.rdata")

