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
(out.file	<- paste("cvs_did_month_testBoston_cut",cutoff,sep=""))

ww			<- 6.5
ar			<- .6

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
Cls.se.fn	<- function(model, cluster.vec, est.table = TRUE){
# This function returns the clustered standard error 
	G	<- length(unique(cluster.vec))		# Number of groups
	N	<- length(cluster.vec)				# Number of observations
	K	<- length(coef(model))				
	dfa	<- (G/(G - 1)) * (N - 1)/model$df.residual		# STATA-like adjusted degree of freedom
	cls.vcov	<- vcovHC(model, type = "HC0", cluster = "group")
	if(est.table){
		return(coeftest(model, vcov = cls.vcov))
	}else{
		se		<- sqrt(diag(cls.vcov))
		return(list(vcov = cls.vcov, se = se))
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
tmp	<- tmp[,list(nzip = length(unique(panelist_zip_code)), nd = length(unique(distance))), by = list(household_code)]
summary(tmp)
mean(tmp$nzip>1)
mypan	<- panelists[panelists$panel_year == 2014,]			# Use 2014 panelist profile
median(mypan$distance, na.rm = T)
mypan			<- subset(mypan, !is.na(distance))

mypan$cvs_in2	<- ifelse(mypan$distance <=2, 1, 0)
mypan$cvs_in3	<- ifelse(mypan$distance <=3, 1, 0)
mypan$cvs_in5	<- ifelse(mypan$distance <=5, 1, 0)
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

purchases	<- subset(purchases, household_code %in% mypan$household_code)
trips		<- subset(trips, household_code %in% mypan$household_code)
purchases	<- merge(purchases, mypan[,c("household_code", "ban_ard", "distance","cvs_in2", "cvs_in5", "wgr_in2", "heavy")], 
						by = "household_code", all.x=T)

###########################
# Descriptive comparision # 
###########################
# Compare per capita consumption trend by consumer group -- Whether the ban has implemented
ggtmp	<- data.table(purchases)			
ggtmp	<- ggtmp[!week %in% endweek, list(Total = sum(quantity*size/qunit, na.rm=T)/length(unique(household_code)), 
					CVS = sum(quantity*size*cvs/qunit, na.rm=T)/length(unique(household_code)), 
 					OtherDrug = sum(quantity*size*(1-cvs)*1*(channel_type=="Drug Store")/qunit, na.rm=T)/length(unique(household_code)), 
					OtherChannel = sum(quantity*size*(1-cvs)*1*(channel_type!="Drug Store")/qunit, na.rm=T)/length(unique(household_code))), 
				by = list(ban_ard,week)]
ggtmp	<- melt(ggtmp, id.vars = c("ban_ard", "week"))
ggtmp$ban_ard	<- factor(ggtmp$ban_ard, levels = c(1, 0), labels = c("Already implement ban", "Others"))

ggplot(ggtmp, aes(week, value, linetype = ban_ard)) + geom_line() + 
			geom_vline(xintercept = as.numeric(event.date), col = "red") + 
			facet_wrap(~variable, scales = "free") + 
			guides(linetype = guide_legend(title = "Distance to CVS")) + 
			theme(legend.position = "bottom") + 
			labs(y = "Volume (packs)", title = "Per capita consumption by consumer groups") 

################################
# Organize data for regression # 
################################
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
tmp1	<- tmp1[,list(	trip_cvs 		= 1*(sum(cvs)>0), 
						trip_othdrug 	= sum(channel_type == "Drug Store" & cvs ==0 ), 
						trip_othchannel = sum(channel_type != "Drug Store"), 
						dol_cvs 		= sum(total_spent*cvs, na.rm = T), 
						dol_othdrug		= sum(total_spent*(1-cvs)*1*(channel_type == "Drug Store"), na.rm = T), 
						dol_othchannel	= sum(total_spent*1*(channel_type != "Drug Store"), na.rm = T), 
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
cat("Summary stats:\n"); print(summary(mydata[, -c(1:2)])); cat("\n")

# Create other control variables: city and month 
mydata$year		<- ifelse(mydata$month > 12, 2014, 2013)
mydata$month1	<- mydata$month %% 12
mydata$month1	<- ifelse(mydata$month1 == 0, 12, mydata$month1)
mydata$month1	<- factor(mydata$month1)
mydata			<- merge(mydata, mypan[,c("household_code", "panelist_zip_code","distance", "ban_ard","cvs_in2", "cvs_in3","wgr_in2", "heavy")], 
						by = "household_code", all.x=T)
dim(mydata)
mydata$after	<- 1*(mydata$month >= event.month)
mydata$hhmonth	<- paste(mydata$household_code, mydata$month, sep="-")
mydata$treat	<- with(mydata, 1*(distance <= cutoff & ban_ard == 0))
table(mydata$treat)
table(mypan$distance<= cutoff& mypan$ban_ard == 0)

##################################
# DID regression of small window # 
##################################			
# Spcifications: 1 DV (cigarette quantity) * 2 IV specifications (2-way DID, 3-way DID)
# Restrict to a short window 
mydata1		<- subset(mydata, month >= event.month - 4 & month <= event.month + 3)						# Data with actual event 

# Drop the households who live in the counties that have already implemented the ban.
sel		<-	mydata1$ban_ard != 1
fit.ls0	<- list(NULL)
fit.ls0[[1]]	<- plm(q ~ after + treat + after*treat , data = mydata1[sel,], 
					index = c("household_code", "month"), model = "within") 
fit.ls0[[2]]	<- plm(q_othdrug ~ after + treat + after*treat , data = mydata1[sel,], 
					index = c("household_code", "month"), model = "within")
fit.ls0[[3]]	<- plm(q_othchannel ~ after + treat + after*treat , data = mydata1[sel,], 
					index = c("household_code", "month"), model = "within")
fit.ls0[[4]]	<- plm(trip_cvs ~ after + treat + after*treat , data = mydata1[sel,], 
					index = c("household_code", "month"), model = "within") 
fit.ls0[[5]]	<- plm(trip_othdrug ~ after + treat + after*treat , data = mydata1[sel,], 
					index = c("household_code", "month"), model = "within")
fit.ls0[[6]]	<- plm(trip_othchannel ~ after + treat + after*treat , data = mydata1[sel,], 
					index = c("household_code", "month"), model = "within")
fit.ls0[[7]]	<- plm(dol_cvs ~ after + treat + after*treat , data = mydata1[sel,], 
					index = c("household_code", "month"), model = "within") 
fit.ls0[[8]]	<- plm(dol_othdrug ~ after + treat + after*treat , data = mydata1[sel,], 
					index = c("household_code", "month"), model = "within")
fit.ls0[[9]]	<- plm(dol_othchannel ~ after + treat + after*treat , data = mydata1[sel,], 
					index = c("household_code", "month"), model = "within")
fit.ls0[[10]]	<- plm(dol_total ~ after + treat + after*treat , data = mydata1[sel,], 
					index = c("household_code", "month"), model = "within")	
fit.ls0[[11]]	<- plm(netdol_cvs ~ after + treat + after*treat , data = mydata1[sel,], 
					index = c("household_code", "month"), model = "within") 
fit.ls0[[12]]	<- plm(netdol_othdrug ~ after + treat + after*treat , data = mydata1[sel,], 
					index = c("household_code", "month"), model = "within")
fit.ls0[[13]]	<- plm(netdol_othchannel ~ after + treat + after*treat , data = mydata1[sel,], 
					index = c("household_code", "month"), model = "within")									 
cls.se0		<- lapply(fit.ls0, function(x) Cls.se.fn(x, cluster.vec = mydata1[sel,"household_code"], est.table = FALSE)$se)

stargazer(fit.ls0, type = "html", align = TRUE, title = "Drop the counties that have already implemented tobacco ban", 
		keep = c("after", "treat"), se = cls.se0,
		covariate.labels = c("After", "After*CloseDist"),  
		add.lines = list(c("Household",rep("FE", length(fit.ls0)))), 
		no.space = TRUE, omit.stat = c("rsq", "adj.rsq", "f"), 
		notes = "S.E. clustered over households", 
		out = paste(plot.wd, "/tb_", out.file, "_dropBoston_",Sys.Date(), ".html", sep=""))

# -----------------------------#
# Use Boston and SF as control #
fit.ls1	<- list(NULL)
fit.ls1[[1]]	<- plm(q ~ after + treat + after*treat , data = mydata1, 
					index = c("household_code", "month"), model = "within") 
fit.ls1[[2]]	<- plm(q_othdrug ~ after + treat + after*treat , data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls1[[3]]	<- plm(q_othchannel ~ after + treat + after*treat , data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls1[[4]]	<- plm(trip_cvs ~ after + treat + after*treat , data = mydata1, 
					index = c("household_code", "month"), model = "within") 
fit.ls1[[5]]	<- plm(trip_othdrug ~ after + treat + after*treat , data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls1[[6]]	<- plm(trip_othchannel ~ after + treat + after*treat , data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls1[[7]]	<- plm(dol_cvs ~ after + treat + after*treat , data = mydata1, 
					index = c("household_code", "month"), model = "within") 
fit.ls1[[8]]	<- plm(dol_othdrug ~ after + treat + after*treat , data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls1[[9]]	<- plm(dol_othchannel ~ after + treat + after*treat , data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls1[[10]]	<- plm(dol_total ~ after + treat + after*treat , data = mydata1, 
					index = c("household_code", "month"), model = "within")					
fit.ls1[[11]]	<- plm(netdol_cvs ~ after + treat + after*treat , data = mydata1, 
					index = c("household_code", "month"), model = "within") 
fit.ls1[[12]]	<- plm(netdol_othdrug ~ after + treat + after*treat , data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls1[[13]]	<- plm(netdol_othchannel ~ after + treat + after*treat , data = mydata1, 
					index = c("household_code", "month"), model = "within")									 
cls.se1		<- lapply(fit.ls1, function(x) Cls.se.fn(x, cluster.vec = mydata1$household_code, est.table = FALSE)$se)

stargazer(fit.ls1, type = "html", align = TRUE, title = "Use the counties that have implemented the ban as control", 
		keep = c("after", "treat"), se = cls.se1,
		covariate.labels = c("After", "After*CloseDist"),  
		add.lines = list(c("Household",rep("FE", length(fit.ls1)))), 
		no.space = TRUE, omit.stat = c("rsq", "adj.rsq", "f"), 
		notes = "S.E. clustered over households", 
		out = paste(plot.wd, "/tb_", out.file, "_",Sys.Date(), ".html", sep=""))	

# -----------------------------------------------#
# Use Boston and SF as control and control month #
fit.ls2	<- list(NULL)
fit.ls2[[1]]	<- plm(q ~ after + treat + after*treat + month1, data = mydata, 
					index = c("household_code", "month"), model = "within") 
fit.ls2[[2]]	<- plm(q_othdrug ~ after + treat + after*treat + month1, data = mydata, 
					index = c("household_code", "month"), model = "within")
fit.ls2[[3]]	<- plm(q_othchannel ~ after + treat + after*treat + month1 , data = mydata, 
					index = c("household_code", "month"), model = "within")
fit.ls2[[4]]	<- plm(trip_cvs ~ after + treat + after*treat + month1 , data = mydata, 
					index = c("household_code", "month"), model = "within") 
fit.ls2[[5]]	<- plm(trip_othdrug ~ after + treat + after*treat + month1 , data = mydata, 
					index = c("household_code", "month"), model = "within")
fit.ls2[[6]]	<- plm(trip_othchannel ~ after + treat + after*treat + month1 , data = mydata, 
					index = c("household_code", "month"), model = "within")
fit.ls2[[7]]	<- plm(dol_cvs ~ after + treat + after*treat + month1 , data = mydata, 
					index = c("household_code", "month"), model = "within") 
fit.ls2[[8]]	<- plm(dol_othdrug ~ after + treat + after*treat + month1 , data = mydata, 
					index = c("household_code", "month"), model = "within")
fit.ls2[[9]]	<- plm(dol_othchannel ~ after + treat + after*treat + month1 , data = mydata, 
					index = c("household_code", "month"), model = "within")
fit.ls2[[10]]	<- plm(dol_total ~ after + treat + after*treat + month1 , data = mydata, 
					index = c("household_code", "month"), model = "within")
fit.ls2[[11]]	<- plm(netdol_cvs ~ after + treat + after*treat + month1 , data = mydata, 
					index = c("household_code", "month"), model = "within") 
fit.ls2[[12]]	<- plm(netdol_othdrug ~ after + treat + after*treat + month1 , data = mydata, 
					index = c("household_code", "month"), model = "within")
fit.ls2[[13]]	<- plm(netdol_othchannel ~ after + treat + after*treat + month1 , data = mydata, 
					index = c("household_code", "month"), model = "within")
cls.se2		<- lapply(fit.ls2, function(x) Cls.se.fn(x, cluster.vec = mydata$household_code, est.table = FALSE)$se)

stargazer(fit.ls2, type = "html", align = TRUE, title = "Use the counties that have implemented the ban as control and control for month", 
		keep = c("after", "treat"), se = cls.se2,
		covariate.labels = c("After", "After*CloseDist"),  
		add.lines = list(c("Household",rep("FE", length(fit.ls2))), c("Month",rep("FE", 10))), 
		no.space = TRUE, omit.stat = c("rsq", "adj.rsq", "f"), 
		notes = "S.E. clustered over households", 
		out = paste(plot.wd, "/tb_", out.file, "_month_",Sys.Date(), ".html", sep=""))

# Quantity at other specific channels
fit.ls3		<- list(NULL)		
fit.ls3[[1]]	<- plm(q_convenience ~ after + treat + after*treat , data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls3[[2]]	<- plm(q_grocery ~ after + treat + after*treat , data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls3[[3]]	<- plm(q_service ~ after + treat + after*treat , data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls3[[4]]	<- plm(q_tobacco ~ after + treat + after*treat , data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls3[[5]]	<- plm(q_gas ~ after + treat + after*treat , data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls3[[6]]	<- plm(q_discount ~ after + treat + after*treat , data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls3[[7]]	<- plm(q_convenience ~ after + treat + after*treat + month1, data = mydata, 
					index = c("household_code", "month"), model = "within")
fit.ls3[[8]]	<- plm(q_grocery ~ after + treat + after*treat + month1, data = mydata, 
					index = c("household_code", "month"), model = "within")
fit.ls3[[9]]	<- plm(q_service ~ after + treat + after*treat + month1, data = mydata, 
					index = c("household_code", "month"), model = "within")
fit.ls3[[10]]	<- plm(q_tobacco ~ after + treat + after*treat + month1, data = mydata, 
					index = c("household_code", "month"), model = "within")
fit.ls3[[11]]	<- plm(q_gas ~ after + treat + after*treat + month1, data = mydata, 
					index = c("household_code", "month"), model = "within")
fit.ls3[[12]]	<- plm(q_discount ~ after + treat + after*treat + month1, data = mydata, 
					index = c("household_code", "month"), model = "within")					
cls.se3		<- c(lapply(fit.ls3[1:6], function(x) Cls.se.fn(x, cluster.vec = mydata1$household_code, est.table = FALSE)$se), 
				 lapply(fit.ls3[7:12], function(x) Cls.se.fn(x, cluster.vec = mydata$household_code, est.table = FALSE)$se) )

stargazer(fit.ls3, type = "html", align = TRUE, title = "Regressions of cigarette quantity at different channels", 
		keep = c("after", "treat"), se = cls.se3,
		covariate.labels = c("After", "After*Treatment"),  
		add.lines = list(c("Household",rep("FE", length(fit.ls3))), c("Month", rep("", 6), rep("FE", 6)), 
						c("Window", rep("8 m.", 6), rep("2 yr.", 6))), 
		no.space = TRUE, omit.stat = c("rsq", "adj.rsq", "f"), 
		notes = "S.E. clustered over households", 
		out = paste(plot.wd, "/tb_", out.file, "_channel_",Sys.Date(), ".html", sep=""))	


save.image(file = paste(plot.wd, "/", out.file, "_", Sys.Date(),".rdata", sep=""))			

cat("This program is done.\n")
