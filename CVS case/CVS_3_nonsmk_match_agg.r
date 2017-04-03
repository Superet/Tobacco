library(reshape2)
library(ggplot2)
library(data.table)
library(lubridate)
library(stargazer)
library(zipcode)
library(plm)
library(lme4)
library(xlsx)
library(MatchIt)
library(lmtest)
library(Matching)

# setwd("~/Documents/Research/Tobacco/processed_data")
# plot.wd		<- "~/Desktop"
setwd("U:/Users/ccv103/Documents/Research/tobacco/processed_data")
# setwd("/sscc/home/c/ccv103/Tobacco") 
plot.wd		<- getwd()
out.file	<- "cvs_nonsmk_match_agg"
ww			<- 6.5
ar			<- .6

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

############
# Function # 
############
Cls.se.fn <- function(model, cluster.vec, return.se = TRUE){
# Var(beta) = (X'X)^(-1) [(eps*X)'(eps*X)](X'X)^(-1)
	X 	<- model.matrix(model)
	uj 	<- residuals(model)  * X
	uj 	<- apply(uj, 2, function(x) tapply(x, cluster.vec, sum))
	A	<- solve(crossprod(X))
	cls.vcov	<- A %*% crossprod(uj) %*% A	
	if(return.se){
	  return(sqrt(diag(cls.vcov))) 
	}else{
	  return(cls.vcov)
	}
}

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
						pre_dol_othchannel	= mean(dol_othchannel), 
						
						pre_trip_cvs_H1		= mean(trip_cvs[month<=6]), 
						pre_trip_othdrug_H1 	= mean(trip_othdrug[month<=6]), 
						pre_trip_othchannel_H1 = mean(trip_othchannel[month<=6]), 
						pre_dol_cvs_H1 		= mean(dol_cvs[month<=6]), 
						pre_dol_othdrug_H1		= mean(dol_othdrug[month<=6]), 
						pre_dol_othchannel_H1	= mean(dol_othchannel[month<=6]),
						pre_trip_cvs_H2		= mean(trip_cvs[month>6 & month<=12]), 
						pre_trip_othdrug_H2 	= mean(trip_othdrug[month>6 & month<=12]), 
						pre_trip_othchannel_H2 = mean(trip_othchannel[month>6 & month<=12]), 
						pre_dol_cvs_H2 		= mean(dol_cvs[month>6 & month<=12]), 
						pre_dol_othdrug_H2		= mean(dol_othdrug[month>6 & month<=12]), 
						pre_dol_othchannel_H2	= mean(dol_othchannel[month>6 & month<=12]),
						
						pre_trip_cvs_H3		= mean(trip_cvs[month>12 & month<=18]), 
						pre_trip_othdrug_H3 	= mean(trip_othdrug[month>12 & month<=18]), 
						pre_trip_othchannel_H3 = mean(trip_othchannel[month>12 & month<=18]), 
						pre_dol_cvs_H3 		= mean(dol_cvs[month>12 & month<=18]), 
						pre_dol_othdrug_H3		= mean(dol_othdrug[month>12 & month<=18]), 
						pre_dol_othchannel_H3	= mean(dol_othchannel[month>12 & month<=18]),
						pre_trip_cvs_H4		= mean(trip_cvs[month>18 & month < event.month]), 
						pre_trip_othdrug_H4 	= mean(trip_othdrug[month>18 & month < event.month]), 
						pre_trip_othchannel_H4 = mean(trip_othchannel[month>18 & month < event.month]), 
						pre_dol_cvs_H4 		= mean(dol_cvs[month>18 & month < event.month]), 
						pre_dol_othdrug_H4		= mean(dol_othdrug[month>18 & month < event.month]), 
						pre_dol_othchannel_H4	= mean(dol_othchannel[month>18 & month < event.month])
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
mydata$hhmonth	<- paste(mydata$household_code, mydata$month, sep="-")

# Construct treatment and control 
mydata$treat	<- with(mydata, 1*(smk == 1 & ban_ard == 0))
mypan$treat		<- with(mypan, 1*(smk == 1 & ban_ard == 0))
table(mydata$treat)
table(mypan$treat)
table(mypan$smk, mypan$frac_seg)
table(mypan$treat, mypan$frac_seg)

trips		<- merge(trips, mypan[,c("household_code", "treat")], by = "household_code", all.x = T)
purchases	<- merge(purchases, mypan[,c("household_code", "treat")], by = "household_code", all.x = T)

rm(list = c("tmp", "tmp1", "tmp2", "tmp3"))

#############################		
# Propensity score matching #		
#############################
# Aggregate over 3 month window around the event 
match.shtdat		<- data.table(subset(mydata, month >= event.month - 3 & month <= event.month + 2))
match.shtdat		<- match.shtdat[, list(	trip_cvs = sum(trip_cvs), trip_othdrug = sum(trip_othdrug), trip_othchannel = sum(trip_othchannel), 
											dol_cvs = sum(dol_cvs), dol_othdrug = sum(dol_othdrug), dol_othchannel = sum(dol_othchannel), 
											netdol_cvs = sum(netdol_cvs), netdol_othdrug = sum(netdol_othdrug), netdol_othchannel = sum(netdol_othchannel),
											dol_total = sum(dol_total), netdol = sum(netdol)), 
									by = list(treat, household_code, after, frac_seg, smk)]
match.shtdat 	<- match.shtdat[, drop:= 1*(length(after) < 2), by = list(household_code)]
table(match.shtdat$drop)									# 1 household did not both 2 period data
match.shtdat 	<- subset(match.shtdat, drop == 0)				
setkeyv(match.shtdat, c("household_code","after"))
match.shtdat	<- match.shtdat[,list(	trip_cvs = diff(trip_cvs), trip_othdrug = diff(trip_othdrug), trip_othchannel = diff(trip_othchannel), 
										dol_cvs = diff(dol_cvs), dol_othdrug = diff(dol_othdrug), dol_othchannel = diff(dol_othchannel), 
										netdol_cvs = diff(netdol_cvs), netdol_othdrug = diff(netdol_othdrug), netdol_othchannel = diff(netdol_othchannel),
										dol_total = diff(dol_total), netdol = diff(netdol)), 
								by = list(household_code, treat, frac_seg, smk)]
match.shtdat	<- data.frame(match.shtdat)
match.shtdat	<- match.shtdat[order(match.shtdat$household_code),]
mypan			<- mypan[order(mypan$household_code), ]
mypan1			<- subset(mypan, household_code %in% match.shtdat$household_code)
max(abs(mypan1$household_code - match.shtdat$household_code))

# Run logit propensity score
(dv.col 	<- setdiff(names(match.shtdat), c( "treat", "household_code","after")))
fml			<- treat ~ income + age + have_kids + employment + race + distance_cvs +
						pre_trip_cvs+pre_trip_othdrug +pre_trip_othchannel +pre_dol_cvs +pre_dol_othdrug +pre_dol_othchannel

# Different treatment construction						
sel1		<- list(rep(TRUE, nrow(match.shtdat)), 
					!(match.shtdat$frac_seg == "Zero" & match.shtdat$smk == 1), 
					!(match.shtdat$frac_seg %in% c("Zero", "S1") & match.shtdat$smk == 1))
sel2		<- list(rep(TRUE,nrow(mypan1)), 
					!(mypan1$frac_seg == "Zero" & mypan1$smk == 1), 
					!(mypan1$frac_seg %in% c("Zero", "S1") & mypan1$smk == 1))
sapply(sel1, function(x) sum(x))
sapply(sel2, function(x) sum(x))

# Set matching parameters
my.ratio	<- 1
my.commonspt		<- TRUE
my.caliper			<- NULL
numbt		<- 500

att.est.ls	<- vector("list", 3)
nn			<- matrix(NA, 3, 2, dimnames = list(NULL, c("Control", "Treated")))
blc			<- NULL
for(k in 1:3){
	psmod  		<- glm(fml,data = mypan1[sel2[[k]],], family=binomial )
	summary(psmod)
	pshat  		<- psmod$fitted
	Tr  		<- mypan1[sel2[[k]],"treat"]

	# Run matching for each DV 
	att.est		<- matrix(NA, length(dv.col), 4)
	dimnames(att.est)	<- list(dv.col, c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
	for(i in 1:length(dv.col)){
		# Matching without replacement 
		# Notice that without replacement, the funciton does not return Abadie-Imbenns se. 
		rr  	<- Match(Y=match.shtdat[sel1[[k]],dv.col[i]], Tr=Tr, X=pshat, M=my.ratio, 
						replace = FALSE, CommonSupport = my.commonspt, caliper = my.caliper)
		print(summary(rr))
		att.est[i,]	<- c(rr$est, rr$se.standard, rr$est/rr$se.standard, 2*pt(-abs(rr$est/rr$se.standard), df = rr$wnobs -1 ))
	}
	class(att.est)	<- "coeftest"
	att.est.ls[[k]]	<- att.est
	
	# Check matching balance 
	mb	<- MatchBalance(fml, data=mypan1[sel2[[k]],], match.out=rr, nboots=numbt)					
	tmp1<- sapply(mb$BeforeMatching, function(x) c(x$mean.Co,x$mean.Tr, x$sdiff.pooled/100, x$tt$p.value, 
											ifelse("ks" %in% names(x), x$ks$ks.boot.pvalue, x$tt$p.value) ))
	tmp2<- sapply(mb$AfterMatching, function(x) c(x$mean.Co,x$mean.Tr, x$sdiff.pooled/100, x$tt$p.value, 
											ifelse("ks" %in% names(x), x$ks$ks.boot.pvalue, x$tt$p.value) ))
	tmp	<- model.matrix(psmod)[,-1]		
	tmp.lab	<- c("Income", "Age", "Have kids", paste("Employment:", levels(mypan$employment)[-1]), paste("Race:", levels(mypan$race)[-1]), 
				"Distance to CVS", "No. trips to CVS/m.", "No. trips to other drug stores/m.", "No. trips to other channels/m.", 
				"Expenditure at CVS/m.", "Expenditure at other drug stores/m.", "Expenditure at other channels/m.")								
	cbind(colnames(tmp), tmp.lab)			
	dimnames(tmp1)	<- dimnames(tmp2)	<-	list(c("Mean Control", "Mean Treatment", "Std difference","t p-value", "KS bootstrap p-value"), tmp.lab)
	if(k == 1){
		blc	<- cbind(blc, t(tmp1), t(tmp2))
	}else{
		blc	<- cbind(blc, t(tmp2))
	}
	nn[k,]	<- c(length(unique(rr$index.control)), length(unique(rr$index.treated)) )
}												
cat("The number of unique households:\n"); print(nn); cat("\n")
cat("Balance check:\n"); print(round(blc,2)); cat("\n")
cat("Estimates of ATT after matching:\n"); 
for(k in 1:length(att.est.ls)){print(att.est.ls[[k]])}
cat("\n")

tmp		<- c(t(cbind(rbind(table(mypan1$treat), nn), matrix(NA, 4, 3))))
blc		<- rbind(blc, No.households = tmp)
stargazer(blc, type = "html", summary = FALSE, align = TRUE, no.space = TRUE, digits = 2,
          title = "Balance Check betweeen treatment and control group", 
          out = paste(plot.wd, "/tb_", out.file, "_balance_",Sys.Date(), ".html", sep=""))

stargazer(att.est.ls, type = "html", summary = FALSE, align = TRUE, no.space = TRUE, digits = 2,
          title = "Before-after difference between treatment and control group for the matched sample during 201406 - 201411",
		  add.lines = list(c("No. treated households", nn[,2])), 
          out = paste(plot.wd, "/tb_", out.file, "_att_",Sys.Date(), ".html", sep=""))

save.image(file = paste(plot.wd,"/", out.file, ".rdata", sep=""))

cat("This program is done.\n")
