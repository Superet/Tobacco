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
library(AER)

setwd("~/Documents/Research/Tobacco/processed_data")
plot.wd		<- "~/Desktop"
# setwd("U:/Users/ccv103/Documents/Research/tobacco/processed_data")
# setwd("/sscc/home/c/ccv103/Tobacco") 
# plot.wd		<- getwd()
out.file	<- "cvs_baseline_matchagg"
ww			<- 6.5
ar			<- .6

panelists 	<- read.csv("tob_CVS_pan.csv", header = T)
purchases	<- read.csv("tob_CVS_purchases.csv", header = T)
trips		<- read.csv("tob_CVS_trips.csv", header = T)
names(panelists)	<- tolower(names(panelists))
ma.policy	<- read.xlsx("MA_policy.xlsx", 1)

# Define the construction of treatment and control
# 1: Control (smokers who never shopped at CVS), Treament (smokers who are also CVS shoppers)
# 2: Control (smokers who shopped at CVS but did not buy cigarettes), Treament (smokers who also purchased cigarettes at CVS)
# 3: Control (smokers who shopped at CVS but did not buy cigarettes), Treament (smokers who spent more than 17% CVS spending on cigarettes)
treat.code	<- 2	
(out.file 	<- paste(out.file, treat.code, sep="")	)	

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
						q_cvs 	= sum(quantity*size*cvs*1*(channel_type == "Drug Store")/qunit, na.rm=T),
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
mydata$hhmonth	<- paste(mydata$household_code, mydata$month, sep="-")

# Construct treatment and control 
if(treat.code == 1){
	mydata$treat	<- with(mydata, 1*(frac_seg != "Never" & ban_ard == 0 ))		
	mypan$treat 	<- with(mypan, 1*(frac_seg != "Never" & ban_ard == 0 ))         # Define treatment dummy
}else if(treat.code == 2){
	mydata			<- subset(mydata, frac_seg != "Never")
	mypan			<- subset(mypan, frac_seg != "Never")
	mydata$treat	<- with(mydata, 1*(frac_seg != "Zero" & ban_ard == 0 ))
	mypan$treat		<- with(mypan, 1*(frac_seg != "Zero" & ban_ard == 0 ))
}else{
	mydata			<- subset(mydata, frac_seg %in% c("Zero", "S2"))
	mypan			<- subset(mypan, frac_seg %in% c("Zero", "S2"))
	mydata$treat	<- with(mydata, 1*(frac_seg != "Zero" & ban_ard == 0 ))
	mypan$treat		<- with(mypan, 1*(frac_seg != "Zero" & ban_ard == 0 ))	
}
table(mydata$treat)
table(mypan$treat)
table(mypan$treat, mypan$frac_seg)

trips		<- merge(trips, mypan[,c("household_code", "treat")], by = "household_code")
purchases	<- merge(purchases, mypan[,c("household_code", "treat")], by = "household_code")

#############################		
# Propensity score matching #		
#############################
# Aggregate over 3 month window around the event 
num.month			<- 3
match.shtdat		<- data.table(subset(mydata, month >= event.month - num.month & month <= event.month + num.month -1))
match.shtdat		<- match.shtdat[, list(	q = sum(q)/num.month, q_cvs = sum(q_cvs)/num.month, q_othdrug = sum(q_othdrug)/num.month, q_othchannel = sum(q_othchannel)/num.month, 
									trip_cvs = sum(trip_cvs)/num.month, trip_othdrug = sum(trip_othdrug)/num.month, trip_othchannel = sum(trip_othchannel)/num.month, 
									dol_cvs = sum(dol_cvs)/num.month, dol_othdrug = sum(dol_othdrug)/num.month, dol_othchannel = sum(dol_othchannel)/num.month, 
									netdol_cvs = sum(netdol_cvs)/num.month, netdol_othdrug = sum(netdol_othdrug)/num.month, netdol_othchannel = sum(netdol_othchannel)/num.month,
									dol_total = sum(dol_total)/num.month, netdol = sum(netdol)/num.month, 
									q_convenience = sum(q_convenience)/num.month, q_grocery = sum(q_grocery)/num.month, q_discount = sum(q_discount)/num.month, 
									q_service = sum(q_service)/num.month, q_gas = sum(q_gas)/num.month, q_tobacco = sum(q_tobacco)/num.month), 
									by = list(treat, household_code, after)]
match.shtdat 	<- match.shtdat[, drop:= 1*(length(after) < 2), by = list(household_code)]
table(match.shtdat$drop)									# 1 household did not both 2 period data
setkeyv(match.shtdat, c("household_code","after"))
match.shtdat	<- match.shtdat[,list(q = diff(q), q_cvs = diff(q_cvs), q_othdrug = diff(q_othdrug), q_othchannel = diff(q_othchannel), 
								trip_cvs = diff(trip_cvs), trip_othdrug = diff(trip_othdrug), trip_othchannel = diff(trip_othchannel), 
								dol_cvs = diff(dol_cvs), dol_othdrug = diff(dol_othdrug), dol_othchannel = diff(dol_othchannel), 
								netdol_cvs = diff(netdol_cvs), netdol_othdrug = diff(netdol_othdrug), netdol_othchannel = diff(netdol_othchannel),
								dol_total = diff(dol_total), netdol = diff(netdol), 
								q_convenience = diff(q_convenience), q_grocery = diff(q_grocery), q_discount = diff(q_discount), 
								q_service = diff(q_service), q_gas = diff(q_gas), q_tobacco = diff(q_tobacco)), 
								by = list(household_code, treat)]
match.shtdat	<- data.frame(match.shtdat)
match.shtdat	<- merge(match.shtdat, mypan, by = "household_code")
match.shtdat$dist_dummy	<- ifelse(match.shtdat$distance_cvs <= 2, 1, 0)
match.shtdat	<- match.shtdat[order(match.shtdat$household_code),]
mypan			<- mypan[order(mypan$household_code), ]
max(abs(mypan$household_code - match.shtdat$household_code))

# Run logit propensity score
my.ratio	<- 1
fml			<- treat ~ income + age + have_kids + employment + race + distance_cvs +
                pre_q + pre_trip_total + pre_trip_othchannel+ pre_dol_total + pre_dol_othchannel
psmod  		<- glm(fml,data = mypan, family=binomial )
summary(psmod)
pshat  		<- psmod$fitted
Tr  		<- mypan$treat

# Run matching for each DV 
# (dv.col 	<- setdiff(names(match.shtdat), c( "treat", "household_code","after", "distance_cvs", "dist_dummy")))
dv.col		<- c("q", "q_cvs", "q_othdrug", "q_othchannel", 
				"trip_cvs", "trip_othdrug", "trip_othchannel", "netdol_cvs", "netdol_othdrug","netdol_othchannel", 
				"q_convenience", "q_grocery", "q_discount", "q_service", "q_gas", "q_tobacco")
est.ols		<- est.mat	<- est.iv	<- est.fe	<- matrix(NA, length(dv.col), 4, 
															dimnames = list(dv.col, c("Estimate", "Std. Error", "t value", "Pr(>|t|)")))
my.commonspt		<- TRUE
my.caliper			<- NULL
for(i in 1:length(dv.col)){
	dv.fml			<- as.formula(paste(dv.col[i], "~ treat"))
	# OLS
	est.ols[i,]	<- coeftest(lm(dv.fml, data = match.shtdat))["treat",]
	
	# Matching without replacement 
	# Notice that without replacement, the funciton does not return Abadie-Imbenns se. 
	rr  	<- Match(Y=match.shtdat[,dv.col[i]], Tr=Tr, X=pshat, M=my.ratio, 
					replace = FALSE, CommonSupport = my.commonspt, caliper = my.caliper)
	print(summary(rr))
	est.mat[i,]	<- c(rr$est, rr$se.standard, rr$est/rr$se.standard, 2*pt(-abs(rr$est/rr$se.standard), df = rr$wnobs -1 ))
	
	# Distance as IV
	est.iv[i,]	<- coeftest(ivreg(dv.fml, instruments = ~ log(distance_cvs), data = match.shtdat))["treat",]
	
	# Fixed effect model with matched households
	tmpdat		<- subset(mydata, household_code %in% mypan[unlist(rr[c("index.treated", "index.control")]), "household_code"] &
									month >= event.month - num.month & month <= event.month + num.month -1)
	tmp		<- plm(as.formula(paste(dv.col[i], "~ treat + treat*after + after")), data = tmpdat, 
					index = c("household_code", "month"), model = "within")
	cls.v	<- Cls.se.fn(tmp, cluster.vec = tmpdat[,"household_code"], return.se = FALSE)
	est.fe[i,]	<- coeftest(tmp, vcov = cls.v)["treat:after",]
}
class(est.ols)	<- class(est.mat) <- class(est.iv) <- class(est.fe) <- "coeftest"
table(rr$weights)

# Check matching balance 
numbt	<- 500
mb	<- MatchBalance(fml, data=mypan, match.out=rr, nboots=numbt)					
tmp1<- sapply(mb$BeforeMatching, function(x) c(x$mean.Co,x$mean.Tr, x$sdiff.pooled/100, x$tt$p.value, 
										ifelse("ks" %in% names(x), x$ks$ks.boot.pvalue, x$tt$p.value) ))
tmp2<- sapply(mb$AfterMatching, function(x) c(x$mean.Co,x$mean.Tr, x$sdiff.pooled/100, x$tt$p.value, 
										ifelse("ks" %in% names(x), x$ks$ks.boot.pvalue, x$tt$p.value) ))
tmp	<- model.matrix(psmod)[,-1]		
tmp.lab	<- c("Income", "Age", "Have kids", paste("Employment:", levels(mypan$employment)[-1]), paste("Race:", levels(mypan$race)[-1]), 
			"Distance to CVS", "Cigarette consumption/m.", "No. total trips/m.", "No. trips to other channels/m.", 
			"Total expenditure/m.", "Expenditure at other channels/m.")								
cbind(colnames(tmp), tmp.lab)			
dimnames(tmp1)	<- dimnames(tmp2)	<-	list(c("Mean Control", "Mean Treatment", "Std difference","t p-value", "KS bootstrap p-value"), tmp.lab)
blc	<- cbind(t(tmp1), t(tmp2))
nn	<- setNames(c(length(unique(rr$index.control)), length(unique(rr$index.treated)) ), 
		c("Control w/o replacement", "Treated w/o replacement" ))
cat("The number of unique households:\n"); print(nn); cat("\n")
cat("Balance check before matching (1-4), after matchging without replacement (5-8):\n"); print(round(blc,2)); cat("\n")

# Print results 
stargazer(list(OLS = est.ols, Matching = est.mat, IV = est.iv, Panel = est.fe), type = "text", 
			column.labels = c("OLS", "Matching", "IV", "Panel"))

# ---------------- #
# Add covariates #
est1.ols		<- est1.mat	<- est1.iv	<- est1.fe	<- matrix(NA, length(dv.col), 4, 
															dimnames = list(dv.col, c("Estimate", "Std. Error", "t value", "Pr(>|t|)")))
my.commonspt		<- TRUE
my.caliper			<- NULL
sel			<- match.shtdat$household_code %in% mypan[unlist(rr[c("index.treated", "index.control")]), "household_code"]
table(mypan[sel,"frac_seg"])

for(i in 1:length(dv.col)){
	dv.fml			<- as.formula(paste(dv.col[i], "~ treat + income + age + have_kids + employment + race + distance_cvs +
	                pre_q + pre_trip_total + pre_trip_othchannel+ pre_dol_total + pre_dol_othchannel"))
	# OLS
	est1.ols[i,]	<- coeftest(lm(dv.fml, data = match.shtdat))["treat",]

	# Matching without replacement 
	est1.mat[i,]	<- coeftest(lm(dv.fml, data = match.shtdat[sel,]))["treat",]

	# Distance as IV
	est1.iv[i,]	<- coeftest(ivreg(dv.fml, instruments = ~ log(distance_cvs), data = match.shtdat))["treat",]

	# Fixed effect model with matched households
	tmpdat		<- subset(mydata, household_code %in% mypan[unlist(rr[c("index.treated", "index.control")]), "household_code"] &
									month >= event.month - num.month & month <= event.month + num.month -1)
	tmp		<- plm(as.formula(paste(dv.col[i], "~ treat + treat*after + after")), data = tmpdat, 
					index = c("household_code", "month"), model = "within")
	cls.v	<- Cls.se.fn(tmp, cluster.vec = tmpdat[,"household_code"], return.se = FALSE)
	est1.fe[i,]	<- coeftest(tmp, vcov = cls.v)["treat:after",]
}
class(est1.ols)	<- class(est1.mat) <- class(est1.iv) <- class(est1.fe) <- "coeftest"


# Print results 
stargazer(list(OLS = est1.ols, Matching = est1.mat, IV = est1.iv, Panel = est1.fe), type = "text", 
			column.labels = c("OLS", "Matching", "IV", "Panel"))

myline	<- paste(names(nn), " (", nn, ")", sep="", collapse = ",")
stargazer(list(blc, list(est.ols, est.mat, est.iv, est.fe), list(est1.ols, est1.mat, est1.iv, est1.fe)), 
		  type = "html", summary = FALSE, align = TRUE, no.space = TRUE, digits = 2,
          title = c("Balance Check among smokers", "Before-after difference between treatment and control group for the matched sample during 201406 - 201411", 
					"Before-after difference between treatment and control group for the matched sample during 201406 - 201411"),
		  notes = myline,
          out = paste(plot.wd, "/tb_", out.file, "_balance_",Sys.Date(), ".html", sep=""))
