library(reshape2)
library(ggplot2)
library(data.table)
library(lubridate)
library(stargazer)
library(zipcode)
library(plm)
library(lme4)

# setwd("~/Documents/Research/Tobacco/processed_data")
# plot.wd		<- "~/Desktop"
setwd("U:/Users/ccv103/Desktop")
# setwd("/sscc/home/c/ccv103/Tobacco") 
plot.wd		<- getwd()
out.file	<- "cvs_did_month_robust"

ww			<- 6.5
ar			<- .6

panelists 	<- read.csv("tob_CVS_pan.csv", header = T)
purchases	<- read.csv("tob_CVS_purchases.csv", header = T)
trips		<- read.csv("tob_CVS_trips.csv", header = T)

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
tmp1	<- tmp1[,list(	trip_cvs 		= 1*(sum(cvs)>0), 
						trip_othdrug 	= sum(channel_type == "Drug Store" & cvs ==0 ), 
						trip_othchannel = sum(channel_type != "Drug Store"), 
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
mydata			<- merge(mydata, mypan[,c("household_code", "panelist_zip_code","distance", "cvs_in2", "cvs_in3", "cvs_in5", "wgr_in2", "heavy")], 
						by = "household_code", all.x=T)
dim(mydata)
mydata$panelist_zip_code	<- clean.zipcodes(mydata$panelist_zip_code)
data(zipcode)
mydata			<- merge(mydata, zipcode[,c("zip","city","state")], by.x = "panelist_zip_code", by.y = "zip")
mydata$scity	<- with(mydata, paste(state, city, sep="-"))
mydata$after	<- 1*(mydata$month >= event.month)
mydata$hhmonth	<- paste(mydata$household_code, mydata$month, sep="-")

##################################
# DID regression of small window # 
##################################			
# Spcifications: 1 DV (cigarette quantity) * 2 IV specifications (2-way DID, 3-way DID)
# Restrict to a short window 
mydata1		<- subset(mydata, month >= event.month - 4 & month <= event.month + 3)						# Data with actual event 

# Close distance: wtihin 3 miles 
fit.ls1	<- list(NULL)
fit.ls1[[1]]	<- plm(q ~ after + cvs_in3 + after*cvs_in3 , data = mydata1, 
					index = c("household_code", "month"), model = "within") 
fit.ls1[[2]]	<- plm(q_othdrug ~ after + cvs_in3 + after*cvs_in3 , data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls1[[3]]	<- plm(q_othchannel ~ after + cvs_in3 + after*cvs_in3 , data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls1[[4]]	<- plm(trip_cvs ~ after + cvs_in3 + after*cvs_in3 , data = mydata1, 
					index = c("household_code", "month"), model = "within") 
fit.ls1[[5]]	<- plm(trip_othdrug ~ after + cvs_in3 + after*cvs_in3 , data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls1[[6]]	<- plm(trip_othchannel ~ after + cvs_in3 + after*cvs_in3 , data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls1[[7]]	<- plm(dol_cvs ~ after + cvs_in3 + after*cvs_in3 , data = mydata1, 
					index = c("household_code", "month"), model = "within") 
fit.ls1[[8]]	<- plm(dol_othdrug ~ after + cvs_in3 + after*cvs_in3 , data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls1[[9]]	<- plm(dol_othchannel ~ after + cvs_in3 + after*cvs_in3 , data = mydata1, 
					index = c("household_code", "month"), model = "within")
cls.se1		<- lapply(fit.ls1, function(x) Cls.se.fn(x, cluster.vec = mydata1$household_code, est.table = FALSE)$se)

stargazer(fit.ls1, type = "html", align = TRUE, title = "DID regressions of monthly cigarette quantity, trips and expenditure", 
		keep = c("after", "cvs_in3"), se = cls.se1,
		covariate.labels = c("After", "After*CloseDist3"),  
		add.lines = list(c("Household",rep("FE", 9))), 
		no.space = TRUE, omit.stat = c("rsq", "adj.rsq", "f"), 
		notes = "S.E. clustered over households", 
		out = paste(plot.wd, "/tb_", out.file, "3_",Sys.Date(), ".html", sep=""))	


# Close distance: wtihin 3 miles 
fit.ls2	<- list(NULL)
fit.ls2[[1]]	<- plm(q ~ after + cvs_in5 + after*cvs_in5 , data = mydata1, 
					index = c("household_code", "month"), model = "within") 
fit.ls2[[2]]	<- plm(q_othdrug ~ after + cvs_in5 + after*cvs_in5 , data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls2[[3]]	<- plm(q_othchannel ~ after + cvs_in5 + after*cvs_in5 , data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls2[[4]]	<- plm(trip_cvs ~ after + cvs_in5 + after*cvs_in5 , data = mydata1, 
					index = c("household_code", "month"), model = "within") 
fit.ls2[[5]]	<- plm(trip_othdrug ~ after + cvs_in5 + after*cvs_in5 , data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls2[[6]]	<- plm(trip_othchannel ~ after + cvs_in5 + after*cvs_in5 , data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls2[[7]]	<- plm(dol_cvs ~ after + cvs_in5 + after*cvs_in5 , data = mydata1, 
					index = c("household_code", "month"), model = "within") 
fit.ls2[[8]]	<- plm(dol_othdrug ~ after + cvs_in5 + after*cvs_in5 , data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls2[[9]]	<- plm(dol_othchannel ~ after + cvs_in5 + after*cvs_in5 , data = mydata1, 
					index = c("household_code", "month"), model = "within")
cls.se2		<- lapply(fit.ls2, function(x) Cls.se.fn(x, cluster.vec = mydata1$household_code, est.table = FALSE)$se)

stargazer(fit.ls2, type = "html", align = TRUE, title = "DID regressions of monthly cigarette quantity, trips and expenditure", 
		keep = c("after", "cvs_in5"), se = cls.se2,
		covariate.labels = c("After", "After*CloseDist5"),  
		add.lines = list(c("Household",rep("FE", 9))), 
		no.space = TRUE, omit.stat = c("rsq", "adj.rsq", "f"), 
		notes = "S.E. clustered over households", 
		out = paste(plot.wd, "/tb_", out.file, "5_",Sys.Date(), ".html", sep=""))	

save.image(file = paste(plot.wd, "/", out.file, "_", Sys.Date(),".rdata", sep=""))			

cat("This program is done.\n")
