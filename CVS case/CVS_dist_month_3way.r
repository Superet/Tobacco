library(reshape2)
library(ggplot2)
library(data.table)
library(lubridate)
library(stargazer)
library(zipcode)
library(plm)
library(lme4)
library(xlsx)

# setwd("~/Documents/Research/Tobacco/processed_data")
# plot.wd		<- "~/Desktop"
setwd("U:/Users/ccv103/Documents/Research/tobacco/processed_data")
# setwd("/sscc/home/c/ccv103/Tobacco") 
plot.wd		<- getwd()
cutoff		<- 2
(out.file	<- paste("cvs_did_month_3way_cut",cutoff,sep=""))

ww			<- 6.5
ar			<- .6

# NOTE: we have fewer smokers in the purchase data after we restrict to the several channels
panelists 	<- read.csv("tob_CVS_pan.csv", header = T)
purchases	<- read.csv("tob_CVS_purchases.csv", header = T)
trips		<- read.csv("tob_CVS_trips.csv", header = T)
names(panelists)	<- tolower(names(panelists))
ma.policy	<- read.xlsx("MA_policy.xlsx", 1)
sel.channel		<- c("Grocery", "Discount Store", "Drug Store", "Warehouse Club", "Dollar Store", "Convenience Store", 
					"Service Station", "All Other Stores", "Gas Mini Mart", "Tobacco Store", "Health Food Store")
trips			<- subset(trips, channel_type %in% sel.channel)			
purchases		<- subset(purchases, trip_code_uc %in% trips$trip_code_uc)		

# # Select a random sample
# sel		<- sample(unique(panelists$household_code), .1*length(unique(panelists$household_code)))
# panelists	<- subset(panelists, household_code %in% sel)
# trips 		<- subset(trips, household_code %in% sel)
# purchases	<- subset(purchases, household_code %in% sel )

############
# Function # 
############
Cls.se.fn <- function(model, cluster.vec){
# Var(beta) = (X'X)^(-1) [(eps*X)'(eps*X)](X'X)^(-1)
	X 	<- model.matrix(model)
	uj 	<- residuals(model)  * X
	uj 	<- apply(uj, 2, function(x) tapply(x, cluster.vec, sum))
	A	<- solve(crossprod(X))
	cls.vcov	<- A %*% crossprod(uj) %*% A	
	return(sqrt(diag(cls.vcov)))
}

#################
# Organize data # 
#################
# Add week
event.date	<- as.Date("2014-09-01", format = "%Y-%m-%d")		
event.month	<- month(event.date) + 12
cvs.ret	<- 4914		# retailer_code for CVS
qunit	<- 20		# 20 cigaretts per pack 
firstw	<- as.Date("2012-12-31", format = "%Y-%m-%d") 	# The first week in 2013
purchases$purchase_date	<- as.Date(as.character(purchases$purchase_date), format = "%Y-%m-%d")
purchases$week		<- ((as.numeric(purchases$purchase_date - firstw)) %/%7 + 1)* 7 + firstw - 1
purchases$year		<- year(purchases$purchase_date)
purchases$month		<- month(purchases$purchase_date)
purchases$month		<- ifelse(purchases$year == 2012, 1, ifelse(purchases$year == 2013, purchases$month, purchases$month + 12))
purchases			<- subset(purchases, year > 2012)

trips$purchase_date	<- as.Date(as.character(trips$purchase_date), format = "%Y-%m-%d")
trips$week			<- ((as.numeric(trips$purchase_date - firstw)) %/%7 + 1)* 7 + firstw - 1
trips$year			<- year(trips$purchase_date)
trips$month			<- month(trips$purchase_date)
trips$month			<- ifelse(trips$year == 2012, 1, ifelse(trips$year == 2013, trips$month, trips$month + 12))
endweek				<- c(min(purchases$week), max(purchases$week))
trips				<- subset(trips, year > 2012)

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
tmp	<- tmp[,list(nzip = length(unique(panelist_zip_code)), nd = length(unique(distance))), by = list(household_code)]
summary(tmp)
mean(tmp$nzip>1)
mypan	<- panelists[panelists$panel_year == 2014,]			# Use 2014 panelist profile
median(mypan$distance, na.rm = T)
mypan			<- subset(mypan, !is.na(distance))

mypan$cvs_in2	<- ifelse(mypan$distance <=2, 1, 0)
mypan$cvs_in5	<- ifelse(mypan$distance <=5, 1, 0)
mypan$cvs_in10	<- ifelse(mypan$distance <=10, 1, 0)
mypan$wgr_in2	<- ifelse(mypan$distance_wgr <=2, 1, 0)

# Classify light vs heavy smokers
tmp1		<- data.table(purchases)
setkeyv(tmp1, c("household_code", "purchase_date"))
tmp1		<- tmp1[, list(consum = sum(quantity*size/qunit, na.rm=T)/(as.numeric(max(purchase_date)-min(purchase_date)))*7), 
					by = list(household_code)]
summary(tmp1$consum)
tmp1$heavy 	<- 1*(tmp1$consum > 2.5)
tmp			<- setNames(tmp1$heavy, tmp1$household_code)
mypan$heavy 	<- tmp[as.character(mypan$household_code)]
sum(is.na(mypan$heavy))
length(purchases[purchases$household_code %in% mypan[is.na(mypan$heavy),"household_code"]])

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
summary(tmp)
median(tmp[cig_frac>0,cig_frac])		
tmp$frac_seg	<- ifelse(tmp$cig_frac ==0, "Zero", ifelse(tmp$cig_frac <= median(tmp[cig_frac>0,cig_frac]), "S1", "S2"))
table(tmp$frac_seg)
mypan	<- merge(mypan, tmp[,list(household_code, frac_seg)], by = "household_code", all.x=T)
sel		<- mypan[is.na(mypan$frac_seg),"household_code"]
sum(subset(trips, household_code %in% sel& purchase_date < event.date)[,"cvs"])
mypan[is.na(mypan$frac_seg), "frac_seg"]	<- "Never"
mypan$frac_seg	<- factor(mypan$frac_seg, levels = c("Never","Zero", "S1", "S2"))
mypan		<- subset(mypan, !is.na(heavy) & !is.na(wgr_in2))
summary(mypan)

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

# Actual cigarette purchases 
tmp2	<- data.table(purchases)
tmp2	<- tmp2[,list(	q = sum(quantity*size/qunit, na.rm=T), 
						q_othdrug = sum(quantity*size*(1-cvs)*1*(channel_type == "Drug Store")/qunit, na.rm=T), 
						q_othchannel = sum(quantity*size*1*(channel_type != "Drug Store")/qunit, na.rm=T)), 
				by = list(household_code, month)]
dim(tmp1); dim(tmp2)
mydata	<- merge(tmp1, tmp2, by = c("household_code", "month"), all.x = T)
dim(mydata)

# Trips and spending 
tmp1 	<- data.table(trips)
tmp1	<- tmp1[,list(total_spent = sum(total_spent)), by = list(household_code, month, purchase_date, channel_type, retailer_code, cvs)]
tmp1	<- tmp1[,list(	trip_cvs 		= length(purchase_date[cvs==1]), 
						trip_othdrug 	= length(purchase_date[channel_type == "Drug Store" & cvs ==0]),
						trip_othchannel = length(purchase_date[channel_type != "Drug Store"]),  
						dol_cvs 		= sum(total_spent*cvs, na.rm = T), 
						dol_othdrug		= sum(total_spent*(1-cvs)*1*(channel_type == "Drug Store"), na.rm = T), 
						dol_othchannel	= sum(total_spent*1*(channel_type != "Drug Store"), na.rm = T)
						), 
				by = list(household_code, month)]
dim(tmp1)		
summary(tmp1[,list(trip_cvs, trip_othdrug, trip_othchannel)])
mydata	<- merge(mydata, tmp1, by = c("household_code", "month"), all.x = T)
sel 	<- is.na(mydata)
mydata[sel]	<- 0
cat("Summary stats:\n"); print(summary(mydata[, -c(1:2)])); cat("\n")

# Create other control variables: city and month 
mydata$year		<- ifelse(mydata$month > 12, 2014, 2013)
mydata$month1	<- mydata$month %% 12
mydata$month1	<- ifelse(mydata$month1 == 0, 12, mydata$month1)
mydata$month1	<- factor(mydata$month1)
mydata			<- merge(mydata, mypan[,c("household_code", "panelist_zip_code","distance", "ban_ard","cvs_in2", 
											"wgr_in2", "heavy", "frac_seg")], 
						by = "household_code", all.x=T)
dim(mydata)
mydata$after	<- 1*(mydata$month >= event.month)
mydata$hhmonth	<- paste(mydata$household_code, mydata$month, sep="-")
mydata$treat	<- with(mydata, 1*(distance <= cutoff & ban_ard == 0))
table(mydata$treat)
table(mypan$distance<= cutoff& mypan$ban_ard == 0)
with(mypan, table(distance<= cutoff& ban_ard == 0, wgr_in2))
with(mypan, table(distance<= cutoff& ban_ard == 0, heavy))
with(mypan, table(distance<= cutoff& ban_ard == 0, frac_seg))

summary(mydata)

##################################
# DID regression of small window # 
##################################			
# Spcifications: 1 DV (cigarette quantity) * 2 IV specifications (2-way DID, 3-way DID)
# Restrict to a short window 
window.month	<- 4				# The analysis windown around the event
mydata1		<- subset(mydata, month >= event.month - window.month & month <= event.month + window.month -1 )	

# ----------------------- #
# Light vs. heavy smokers # 
fit.ls1	<- list(NULL)
fit.ls1[[1]]	<- plm(q ~ after + treat + heavy + after*treat + after*heavy + after*treat*heavy, data = mydata1, 
					index = c("household_code", "month"), model = "within") 
fit.ls1[[2]]	<- plm(q_othdrug ~ after + treat + heavy + after*treat + after*heavy + after*treat*heavy, data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls1[[3]]	<- plm(q_othchannel ~ after + treat + heavy + after*treat + after*heavy + after*treat*heavy, data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls1[[4]]	<- plm(trip_cvs ~ after + treat + heavy + after*treat + after*heavy + after*treat*heavy, data = mydata1, 
					index = c("household_code", "month"), model = "within") 
fit.ls1[[5]]	<- plm(trip_othdrug ~ after + treat + heavy + after*treat + after*heavy + after*treat*heavy, data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls1[[6]]	<- plm(trip_othchannel ~ after + treat + heavy + after*treat + after*heavy + after*treat*heavy, data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls1[[7]]	<- plm(dol_cvs ~ after + treat + heavy + after*treat + after*heavy + after*treat*heavy, data = mydata1, 
					index = c("household_code", "month"), model = "within") 
fit.ls1[[8]]	<- plm(dol_othdrug ~ after + treat + heavy + after*treat + after*heavy + after*treat*heavy, data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls1[[9]]	<- plm(dol_othchannel ~ after + treat + heavy + after*treat + after*heavy + after*treat*heavy, data = mydata1, 
					index = c("household_code", "month"), model = "within")
cls.se1		<- lapply(fit.ls1, function(x) Cls.se.fn(x, cluster.vec = mydata1$household_code))

myline 		<- list(c("Household",rep("FE", length(fit.ls1))))
stargazer(fit.ls1, type = "html", align = TRUE, title = "3-way DID regressions: treatment effect by heavy vs. light smokers", 
		keep = c("after", "treat", "heavy"), se = cls.se1,
		covariate.labels = c("After", "After*Treat", "After*Heavy", "After*Treat*Heavy"),  
		add.lines = myline, 
		no.space = TRUE, omit.stat = c("rsq", "adj.rsq", "f"), 
		notes = "S.E. clustered over households", 
		out = paste(plot.wd, "/tb_", out.file, "_heavy_",Sys.Date(), ".html", sep=""))	

fit.ls11	<- list(NULL)
fit.ls11[[1]]	<- plm(q ~ after + treat + heavy + after*treat + after*heavy + after*treat*heavy+month1, data = mydata, 
					index = c("household_code", "month"), model = "within") 
fit.ls11[[2]]	<- plm(q_othdrug ~ after + treat + heavy + after*treat + after*heavy + after*treat*heavy+month1, data = mydata, 
					index = c("household_code", "month"), model = "within")
fit.ls11[[3]]	<- plm(q_othchannel ~ after + treat + heavy + after*treat + after*heavy + after*treat*heavy+month1, data = mydata, 
					index = c("household_code", "month"), model = "within")
fit.ls11[[4]]	<- plm(trip_cvs ~ after + treat + heavy + after*treat + after*heavy + after*treat*heavy+month1, data = mydata, 
					index = c("household_code", "month"), model = "within") 
fit.ls11[[5]]	<- plm(trip_othdrug ~ after + treat + heavy + after*treat + after*heavy + after*treat*heavy+month1, data = mydata, 
					index = c("household_code", "month"), model = "within")
fit.ls11[[6]]	<- plm(trip_othchannel ~ after + treat + heavy + after*treat + after*heavy + after*treat*heavy+month1, data = mydata, 
					index = c("household_code", "month"), model = "within")
fit.ls11[[7]]	<- plm(dol_cvs ~ after + treat + heavy + after*treat + after*heavy + after*treat*heavy+month1, data = mydata, 
					index = c("household_code", "month"), model = "within") 
fit.ls11[[8]]	<- plm(dol_othdrug ~ after + treat + heavy + after*treat + after*heavy + after*treat*heavy+month1, data = mydata, 
					index = c("household_code", "month"), model = "within")
fit.ls11[[9]]	<- plm(dol_othchannel ~ after + treat + heavy + after*treat + after*heavy + after*treat*heavy+month1, data = mydata, 
					index = c("household_code", "month"), model = "within")
cls.se11		<- lapply(fit.ls11, function(x) Cls.se.fn(x, cluster.vec = mydata$household_code))

myline1			<- c(myline, list(c("Month",rep("FE", length(fit.ls11)))) )
stargazer(fit.ls11, type = "html", align = TRUE, title = "3-way DID regressions: treatment effect by heavy vs. light smokers", 
		keep = c("after", "treat", "heavy"), se = cls.se11,
		covariate.labels = c("After", "After*Treat", "After*Heavy", "After*Treat*Heavy"),  
		add.lines = myline1, 
		no.space = TRUE, omit.stat = c("rsq", "adj.rsq", "f"), 
		notes = "S.E. clustered over households", 
		out = paste(plot.wd, "/tb_", out.file, "_heavy_month_",Sys.Date(), ".html", sep=""))		

# -------------------# 		
# Close to Walgreens # 	
fit.ls2	<- list(NULL)
fit.ls2[[1]]	<- plm(q ~ after + treat + wgr_in2 + after*treat + after*wgr_in2 + after*treat*wgr_in2, data = mydata1, 
					index = c("household_code", "month"), model = "within") 
fit.ls2[[2]]	<- plm(q_othdrug ~ after + treat + wgr_in2 + after*treat + after*wgr_in2 + after*treat*wgr_in2, data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls2[[3]]	<- plm(q_othchannel ~ after + treat + wgr_in2 + after*treat + after*wgr_in2 + after*treat*wgr_in2, data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls2[[4]]	<- plm(trip_cvs ~ after + treat + wgr_in2 + after*treat + after*wgr_in2 + after*treat*wgr_in2, data = mydata1, 
					index = c("household_code", "month"), model = "within") 
fit.ls2[[5]]	<- plm(trip_othdrug ~ after + treat + wgr_in2 + after*treat + after*wgr_in2 + after*treat*wgr_in2, data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls2[[6]]	<- plm(trip_othchannel ~ after + treat + wgr_in2 + after*treat + after*wgr_in2 + after*treat*wgr_in2, data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls2[[7]]	<- plm(dol_cvs ~ after + treat + wgr_in2 + after*treat + after*wgr_in2 + after*treat*wgr_in2, data = mydata1, 
					index = c("household_code", "month"), model = "within") 
fit.ls2[[8]]	<- plm(dol_othdrug ~ after + treat + wgr_in2 + after*treat + after*wgr_in2 + after*treat*wgr_in2, data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls2[[9]]	<- plm(dol_othchannel ~ after + treat + wgr_in2 + after*treat + after*wgr_in2 + after*treat*wgr_in2, data = mydata1, 
					index = c("household_code", "month"), model = "within")
cls.se2		<- lapply(fit.ls2, function(x) Cls.se.fn(x, cluster.vec = mydata1$household_code) )

stargazer(fit.ls2, type = "html", align = TRUE, title = "3-way DID regressions: treatment effect by distance to Walgreens", 
		keep = c("after", "treat", "wgr_in2"), se = cls.se2,
		covariate.labels = c("After", "After*Treat", "After*CloseWgr", "After*Treat*CloseWgr"),  
		add.lines = myline, 
		no.space = TRUE, omit.stat = c("rsq", "adj.rsq", "f"), 
		notes = "S.E. clustered over households", 
		out = paste(plot.wd, "/tb_", out.file, "_wgr_",Sys.Date(), ".html", sep=""))
		
fit.ls21		<- list(NULL)
fit.ls21[[1]]	<- plm(q ~ after + treat + wgr_in2 + after*treat + after*wgr_in2 + after*treat*wgr_in2+month1, data = mydata, 
					index = c("household_code", "month"), model = "within") 
fit.ls21[[2]]	<- plm(q_othdrug ~ after + treat + wgr_in2 + after*treat + after*wgr_in2 + after*treat*wgr_in2+month1, data = mydata, 
					index = c("household_code", "month"), model = "within")
fit.ls21[[3]]	<- plm(q_othchannel ~ after + treat + wgr_in2 + after*treat + after*wgr_in2 + after*treat*wgr_in2+month1, data = mydata, 
					index = c("household_code", "month"), model = "within")
fit.ls21[[4]]	<- plm(trip_cvs ~ after + treat + wgr_in2 + after*treat + after*wgr_in2 + after*treat*wgr_in2+month1, data = mydata, 
					index = c("household_code", "month"), model = "within") 
fit.ls21[[5]]	<- plm(trip_othdrug ~ after + treat + wgr_in2 + after*treat + after*wgr_in2 + after*treat*wgr_in2+month1, data = mydata, 
					index = c("household_code", "month"), model = "within")
fit.ls21[[6]]	<- plm(trip_othchannel ~ after + treat + wgr_in2 + after*treat + after*wgr_in2 + after*treat*wgr_in2+month1, data = mydata, 
					index = c("household_code", "month"), model = "within")
fit.ls21[[7]]	<- plm(dol_cvs ~ after + treat + wgr_in2 + after*treat + after*wgr_in2 + after*treat*wgr_in2+month1, data = mydata, 
					index = c("household_code", "month"), model = "within") 
fit.ls21[[8]]	<- plm(dol_othdrug ~ after + treat + wgr_in2 + after*treat + after*wgr_in2 + after*treat*wgr_in2+month1, data = mydata, 
					index = c("household_code", "month"), model = "within")
fit.ls21[[9]]	<- plm(dol_othchannel ~ after + treat + wgr_in2 + after*treat + after*wgr_in2 + after*treat*wgr_in2+month1, data = mydata, 
					index = c("household_code", "month"), model = "within")
cls.se21		<- lapply(fit.ls21, function(x) Cls.se.fn(x, cluster.vec = mydata$household_code) )

stargazer(fit.ls21, type = "html", align = TRUE, title = "3-way DID regressions: treatment effect by distance to Walgreens", 
		keep = c("after", "treat", "wgr_in2"), se = cls.se21,
		covariate.labels = c("After", "After*Treat", "After*CloseWgr", "After*Treat*CloseWgr"),  
		add.lines = myline1, 
		no.space = TRUE, omit.stat = c("rsq", "adj.rsq", "f"), 
		notes = "S.E. clustered over households", 
		out = paste(plot.wd, "/tb_", out.file, "_wgr_month_",Sys.Date(), ".html", sep=""))		

# -------------------------------#		
# Fraction of cigarette spending # 		
fit.ls3	<- list(NULL)
fit.ls3[[1]]	<- plm(q ~ after + treat + frac_seg + after*treat + after*frac_seg + after*treat*frac_seg, data = mydata1, 
					index = c("household_code", "month"), model = "within") 
fit.ls3[[2]]	<- plm(q_othdrug ~ after + treat + frac_seg + after*treat + after*frac_seg + after*treat*frac_seg, data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls3[[3]]	<- plm(q_othchannel ~ after + treat + frac_seg + after*treat + after*frac_seg + after*treat*frac_seg, data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls3[[4]]	<- plm(trip_cvs ~ after + treat + frac_seg + after*treat + after*frac_seg + after*treat*frac_seg, data = mydata1, 
					index = c("household_code", "month"), model = "within") 
fit.ls3[[5]]	<- plm(trip_othdrug ~ after + treat + frac_seg + after*treat + after*frac_seg + after*treat*frac_seg, data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls3[[6]]	<- plm(trip_othchannel ~ after + treat + frac_seg + after*treat + after*frac_seg + after*treat*frac_seg, data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls3[[7]]	<- plm(dol_cvs ~ after + treat + frac_seg + after*treat + after*frac_seg + after*treat*frac_seg, data = mydata1, 
					index = c("household_code", "month"), model = "within") 
fit.ls3[[8]]	<- plm(dol_othdrug ~ after + treat + frac_seg + after*treat + after*frac_seg + after*treat*frac_seg, data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls3[[9]]	<- plm(dol_othchannel ~ after + treat + frac_seg + after*treat + after*frac_seg + after*treat*frac_seg, data = mydata1, 
					index = c("household_code", "month"), model = "within")
cls.se3		<- lapply(fit.ls3, function(x) Cls.se.fn(x, cluster.vec = mydata1$household_code))

stargazer(fit.ls3, type = "html", align = TRUE, title = "3-way DID regressions: treatment effect by fraction of cigarett spending out of CVS spending", 
		keep = c("after", "treat", "frac_seg"), se = cls.se3,
		covariate.labels = c("After", "After*Treat", paste("After*",levels(mydata$frac_seg)[-1], sep=""), paste("After*Treat*",levels(mydata$frac_seg)[-1], sep="")),  
		add.lines = myline,
		no.space = TRUE, omit.stat = c("rsq", "adj.rsq", "f"), 
		notes = "S.E. clustered over households", 
		out = paste(plot.wd, "/tb_", out.file, "_frac_seg_",Sys.Date(), ".html", sep=""))	
	
fit.ls31	<- list(NULL)
fit.ls31[[1]]	<- plm(q ~ after + treat + frac_seg + after*treat + after*frac_seg + after*treat*frac_seg+month1, data = mydata, 
					index = c("household_code", "month"), model = "within") 
fit.ls31[[2]]	<- plm(q_othdrug ~ after + treat + frac_seg + after*treat + after*frac_seg + after*treat*frac_seg+month1, data = mydata, 
					index = c("household_code", "month"), model = "within")
fit.ls31[[3]]	<- plm(q_othchannel ~ after + treat + frac_seg + after*treat + after*frac_seg + after*treat*frac_seg+month1, data = mydata, 
					index = c("household_code", "month"), model = "within")
fit.ls31[[4]]	<- plm(trip_cvs ~ after + treat + frac_seg + after*treat + after*frac_seg + after*treat*frac_seg+month1, data = mydata, 
					index = c("household_code", "month"), model = "within") 
fit.ls31[[5]]	<- plm(trip_othdrug ~ after + treat + frac_seg + after*treat + after*frac_seg + after*treat*frac_seg+month1, data = mydata, 
					index = c("household_code", "month"), model = "within")
fit.ls31[[6]]	<- plm(trip_othchannel ~ after + treat + frac_seg + after*treat + after*frac_seg + after*treat*frac_seg+month1, data = mydata, 
					index = c("household_code", "month"), model = "within")
fit.ls31[[7]]	<- plm(dol_cvs ~ after + treat + frac_seg + after*treat + after*frac_seg + after*treat*frac_seg+month1, data = mydata, 
					index = c("household_code", "month"), model = "within") 
fit.ls31[[8]]	<- plm(dol_othdrug ~ after + treat + frac_seg + after*treat + after*frac_seg + after*treat*frac_seg+month1, data = mydata, 
					index = c("household_code", "month"), model = "within")
fit.ls31[[9]]	<- plm(dol_othchannel ~ after + treat + frac_seg + after*treat + after*frac_seg + after*treat*frac_seg+month1, data = mydata, 
					index = c("household_code", "month"), model = "within")
cls.se31		<- lapply(fit.ls31, function(x) Cls.se.fn(x, cluster.vec = mydata$household_code))

stargazer(fit.ls31, type = "html", align = TRUE, title = "3-way DID regressions: treatment effect by fraction of cigarett spending out of CVS spending", 
		keep = c("after", "treat", "frac_seg"), se = cls.se31,
		covariate.labels = c("After", "After*Treat", paste("After*",levels(mydata$frac_seg)[-1], sep=""), paste("After*Treat*",levels(mydata$frac_seg)[-1], sep="")),  
		add.lines = myline1,
		no.space = TRUE, omit.stat = c("rsq", "adj.rsq", "f"), 
		notes = "S.E. clustered over households", 
		out = paste(plot.wd, "/tb_", out.file, "_frac_seg_month_",Sys.Date(), ".html", sep=""))	

save.image(file = paste(plot.wd, "/", out.file, "_", Sys.Date(),".rdata", sep=""))			

cat("This program is done.\n")
