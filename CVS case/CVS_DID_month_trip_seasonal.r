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
out.file	<- "cvs_did_month_trip_ss"

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
event.date	<- as.Date("2014-09-01", format = "%Y-%m-%d")
event.month	<- month(event.date) + 12
cvs.ret	<- 4914		# retailer_code for CVS
qunit	<- 20		# 20 cigaretts per pack 
firstw	<- as.Date("2012-12-31", format = "%Y-%m-%d") 	# The first week in 2013
purchases$purchase_date	<- as.Date(as.character(purchases$purchase_date), format = "%Y-%m-%d")
purchases$week		<- ((as.numeric(purchases$purchase_date - firstw)) %/%7 + 1)* 7 + firstw - 1
purchases$year		<- year(purchases$purchase_date)
# purchases$month		<- as.Date(paste(purchases$year, month(purchases$purchase_date), "01", sep = "-"), format = "%Y-%m-%d")
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
median(panelists$distance)
tmp	<- data.table(panelists)
tmp	<- tmp[,list(nzip = length(unique(panelist_zip_code)), nd = length(unique(distance))), by = list(household_code)]
summary(tmp)
mean(tmp$nzip>1)
panelists	<- subset(panelists, !is.na(distance) & !is.na(distance_wgr))
purchases	<- subset(purchases, household_code %in% panelists$household_code)
trips		<- subset(trips, household_code %in% panelists$household_code)

panelists$cvs_in2	<- ifelse(panelists$distance <=2, 1, 0)
panelists$cvs_in5	<- ifelse(panelists$distance <=5, 1, 0)
panelists$cvs_in10	<- ifelse(panelists$distance <=10, 1, 0)
panelists$wgr_in2	<- ifelse(panelists$distance_wgr <=2, 1, 0)

# Classify light vs heavy smokers
tmp1		<- data.table(purchases)
setkeyv(tmp1, c("household_code", "purchase_date"))
tmp1		<- tmp1[, list(consum = sum(quantity*size/qunit, na.rm=T)/(as.numeric(max(purchase_date)-min(purchase_date)))*7), 
					by = list(household_code)]
summary(tmp1$consum)
tmp1$heavy 	<- 1*(tmp1$consum > 2.5)
tmp			<- setNames(tmp1$heavy, tmp1$household_code)
panelists$heavy 	<- tmp[as.character(panelists$household_code)]

# Merge household data to purchase and trip data 
purchases	<- merge(purchases, panelists[,c("household_code", "panel_year", "distance","cvs_in2", "cvs_in5", "cvs_in10", "wgr_in2", "heavy")], 
						by.x = c("household_code", "year"), by.y = c("household_code", "panel_year"), all.x=T)
trips	<- merge(trips, panelists[,c("household_code", "panel_year", "cvs_in2", "cvs_in5", "cvs_in10", "wgr_in2","heavy")], 
						by.x = c("household_code", "year"), by.y = c("household_code", "panel_year"), all.x=T)
purchases$cvs_c	<- factor(purchases$cvs, levels = c(1,0), labels = c("CVS", "Non-CVS"))


# -------------------------- #
# Fill in non-puchases months # 
# Complete month for each household 
tmp		<- data.table(purchases)
tmp		<- tmp[,list(start = min(month), end = max(month)), by = list(household_code)]
tmp		<- tmp[, n:= end-start]
tmp1	<- lapply(1:nrow(tmp), function(i) tmp[i,start] + c(0:tmp[i,n]))
names(tmp1)	<- tmp$household_code
tmp1	<- melt(tmp1)
names(tmp1)	<- c("month", "household_code")

# Actual cigarette purchases 
tmp2	<- data.table(purchases)
tmp2	<- tmp2[,list(	q = sum(quantity*size/qunit, na.rm=T), 
						q_othdrug = sum(quantity*size*(1-cvs)/qunit, na.rm=T), 
						q_othchannel = sum(quantity*size*1*(channel_type != "Drug Store")/qunit, na.rm=T)
						), 
				by = list(household_code, month)]
dim(tmp1); dim(tmp2)
mydata	<- merge(tmp1, tmp2, by = c("household_code", "month"), all.x = T)
dim(mydata)

# Trips
tmp1 	<- data.table(trips)
tmp1	<- tmp1[,list(	trip_cvs = 1*(sum(cvs)>0), 
						trip_othdrug = sum(channel_type == "Drug Store" & cvs ==0 ), 
						dol_cvs = sum(total_spent[cvs>0], na.rm = T), 
						dol_othdrug = sum(total_spent[channel_type == "Drug Store" & cvs ==0], na.rm=T)), 
				by = list(household_code, month)]
dim(tmp1)		
tmp1	<- tmp1[, trip_othdrug := 1*(trip_othdrug >0 )]		
mydata	<- merge(mydata, tmp1, by = c("household_code", "month"), all.x = T)
sel 	<- is.na(mydata)
mydata[sel]	<- 0
cat("Summary stats:\n"); print(summary(mydata[, -c(1:2)])); cat("\n")

# Create other control variables: city and month 
mydata$year		<- ifelse(mydata$month > 12, 2014, 2013)
mydata$month1	<- mydata$month %% 12
mydata$month1	<- ifelse(mydata$month1 == 0, 12, mydata$month1)
mydata$month1	<- factor(mydata$month1)
mydata			<- merge(mydata, panelists[,c("household_code", "panel_year", "panelist_zip_code","distance", "cvs_in2", "wgr_in2", "heavy")], 
						by.x = c("household_code", "year"), by.y = c("household_code", "panel_year"), all.x=T)
dim(mydata)
mydata$panelist_zip_code	<- clean.zipcodes(mydata$panelist_zip_code)
data(zipcode)
mydata			<- merge(mydata, zipcode[,c("zip","city","state")], by.x = "panelist_zip_code", by.y = "zip")
mydata$scity	<- with(mydata, paste(state, city, sep="-"))

mydata$after	<- 1*(mydata$month >= event.month)
mydata$hhmonth	<- paste(mydata$household_code, mydata$month, sep="-")

# Spcifications: 1 DV (cigarette quantity) * 2 IV specifications (2-way DID, 3-way DID)
# Restrict to a short window 
mydata1		<- subset(mydata, abs(month - event.month) <= 3)

############################
# DID regressions of trips #
############################
# 2-way DID # 
glm.ls1	<- list(NULL)
glm.ls1[[1]]	<- glmer(trip_cvs ~ after + cvs_in2 + after*cvs_in2  + (1|scity) , data = mydata1, family = binomial(link = "logit"))  
glm.ls1[[2]]	<- glmer(trip_othdrug ~ after + cvs_in2 + after*cvs_in2  + (1|scity) , data = mydata1, family = binomial(link = "logit"))  
glm.ls1[[3]]	<- glmer(trip_cvs ~ after + cvs_in2 + after*cvs_in2  + (1|household_code) , data = mydata1, family = binomial(link = "logit"))  
glm.ls1[[4]]	<- glmer(trip_othdrug ~ after + cvs_in2 + after*cvs_in2  + (1|household_code) , data = mydata1, family = binomial(link = "logit"))  

stargazer(glm.ls1, type = "html", align = TRUE, title = "DID logit regressions of monthly shopping incidence", 
			keep = c("after", "cvs_in2"),
			covariate.labels = c("After", "CloseDist", "After*CloseDist"),  
			add.lines = list(c("City", "RE","RE","",""), c("Household","","","RE","RE"), c("Month", "FE","FE","FE","FE")), 
			no.space = TRUE, omit.stat = c("rsq", "adj.rsq","f"), 
			out = paste(plot.wd, "/tb_", out.file, "_2way.html", sep="")	)			

# -------------------------------- #
# 3-way DID with Close to Walgreen #
glm.ls2	<- list(NULL)
glm.ls2[[1]]	<- glmer(trip_cvs ~ after + cvs_in2 + wgr_in2 + after*cvs_in2 + after*wgr_in2 + after*cvs_in2*wgr_in2  + (1|scity) , data = mydata1, family = binomial(link = "logit"))  
glm.ls2[[2]]	<- glmer(trip_othdrug ~ after + cvs_in2 + wgr_in2 + after*cvs_in2 + after*wgr_in2 + after*cvs_in2*wgr_in2  + (1|scity) , data = mydata1, family = binomial(link = "logit"))  
glm.ls2[[3]]	<- glmer(trip_cvs ~ after + cvs_in2 + wgr_in2 + after*cvs_in2 + after*wgr_in2 + after*cvs_in2*wgr_in2  + (1|household_code) , data = mydata1, family = binomial(link = "logit"))  
glm.ls2[[4]]	<- glmer(trip_othdrug ~ after + cvs_in2 + wgr_in2 + after*cvs_in2 + after*wgr_in2 + after*cvs_in2*wgr_in2  + (1|household_code) , data = mydata1, family = binomial(link = "logit"))  

stargazer(glm.ls2, type = "html", align = TRUE, title = "3-way DID logit regressions of monthly shopping incidence", 
			keep = c("after", "cvs_in2", "wgr_in2"),
			covariate.labels = c("After", "CloseDist", "CloseWgr", "After*CloseDist", 
								"After*CloseWgr", "CloseDist*CloseWgr", "After*CloseDist*CloseWgr"),  
			add.lines = list(c("City", "RE","RE","",""), c("Household","","","RE","RE"), c("Month", "FE","FE","FE","FE")), 
			no.space = TRUE, omit.stat = c("rsq", "adj.rsq","f"), 
			out = paste(plot.wd, "/tb_", out.file, "_3way_wgr.html", sep="")	)			

# -------------------------------------- #
# 3-way DID with heavy vs. light smokers #
glm.ls3	<- list(NULL)
glm.ls3[[1]]	<- glmer(trip_cvs ~ after + cvs_in2 + heavy + after*cvs_in2 + after*heavy + after*cvs_in2*heavy  + (1|scity) , data = mydata1, family = binomial(link = "logit"))  
glm.ls3[[2]]	<- glmer(trip_othdrug ~ after + cvs_in2 + heavy + after*cvs_in2 + after*heavy + after*cvs_in2*heavy  + (1|scity) , data = mydata1, family = binomial(link = "logit"))  
glm.ls3[[3]]	<- glmer(trip_cvs ~ after + cvs_in2 + heavy + after*cvs_in2 + after*heavy + after*cvs_in2*heavy  + (1|household_code) , data = mydata1, family = binomial(link = "logit"))  
glm.ls3[[4]]	<- glmer(trip_othdrug ~ after + cvs_in2 + heavy + after*cvs_in2 + after*heavy + after*cvs_in2*heavy  + (1|household_code) , data = mydata1, family = binomial(link = "logit"))  

stargazer(glm.ls3, type = "html", align = TRUE, title = "3-way DID logit regressions of monthly shopping incidence", 
			keep = c("after", "cvs_in2", "heavy"),
			covariate.labels = c("After", "CloseDist", "Heavy", "After*CloseDist", 
								"After*Heavy", "CloseDist*Heavy", "After*CloseDist*Heavy"),  
			add.lines = list(c("City", "RE","RE","",""), c("Household","","","RE","RE"), c("Month", "FE","FE","FE","FE")), 
			no.space = TRUE, omit.stat = c("rsq", "adj.rsq","f"), 
			out = paste(plot.wd, "/tb_", out.file, "_3way_heavy.html", sep="")	)			

#---------------#
# Longer window # 
# 2-way DID # 
glm.ls4	<- list(NULL)
glm.ls4[[1]]	<- glmer(trip_cvs ~ after + cvs_in2 + after*cvs_in2 + month1 + month1*cvs_in2 + (1|scity) , data = mydata, family = binomial(link = "logit"))  
glm.ls4[[2]]	<- glmer(trip_othdrug ~ after + cvs_in2 + after*cvs_in2 + month1 + month1*cvs_in2 + (1|scity) , data = mydata, family = binomial(link = "logit"))  
glm.ls4[[3]]	<- glmer(trip_cvs ~ after + cvs_in2 + after*cvs_in2 + month1 + month1*cvs_in2 + (1|household_code) , data = mydata, family = binomial(link = "logit"))  
glm.ls4[[4]]	<- glmer(trip_othdrug ~ after + cvs_in2 + after*cvs_in2 + month1 + month1*cvs_in2 + (1|household_code) , data = mydata, family = binomial(link = "logit"))  

stargazer(glm.ls4, type = "html", align = TRUE, title = "DID logit regressions of monthly shopping incidence", 
			keep = c("after", "cvs_in2"),
			covariate.labels = c("After", "CloseDist", "After*CloseDist"),  
			add.lines = list(c("City", "RE","RE","",""), c("Household","","","RE","RE"), c("Month", "FE","FE","FE","FE")), 
			no.space = TRUE, omit.stat = c("rsq", "adj.rsq","f"), 
			out = paste(plot.wd, "/tb_", out.file, "_long_2way.html", sep="")	)			

# -------------------------------- #
# 3-way DID with Close to Walgreen #
glm.ls5	<- list(NULL)
glm.ls5[[1]]	<- glmer(trip_cvs ~ after + cvs_in2 + wgr_in2 + after*cvs_in2 + after*wgr_in2 + after*cvs_in2*wgr_in2 + month1 + month1*cvs_in2 + (1|scity) , data = mydata, family = binomial(link = "logit"))  
glm.ls5[[2]]	<- glmer(trip_othdrug ~ after + cvs_in2 + wgr_in2 + after*cvs_in2 + after*wgr_in2 + after*cvs_in2*wgr_in2 + month1 + month1*cvs_in2 + (1|scity) , data = mydata, family = binomial(link = "logit"))  
glm.ls5[[3]]	<- glmer(trip_cvs ~ after + cvs_in2 + wgr_in2 + after*cvs_in2 + after*wgr_in2 + after*cvs_in2*wgr_in2 + month1 + month1*cvs_in2 + (1|household_code) , data = mydata, family = binomial(link = "logit"))  
glm.ls5[[4]]	<- glmer(trip_othdrug ~ after + cvs_in2 + wgr_in2 + after*cvs_in2 + after*wgr_in2 + after*cvs_in2*wgr_in2 + month1 + month1*cvs_in2 + (1|household_code) , data = mydata, family = binomial(link = "logit"))  

stargazer(glm.ls5, type = "html", align = TRUE, title = "3-way DID logit regressions of monthly shopping incidence", 
			keep = c("after", "cvs_in2", "wgr_in2"),
			covariate.labels = c("After", "CloseDist", "CloseWgr", "After*CloseDist", 
								"After*CloseWgr", "CloseDist*CloseWgr", "After*CloseDist*CloseWgr"),  
			add.lines = list(c("City", "RE","RE","",""), c("Household","","","RE","RE"), c("Month", "FE","FE","FE","FE")), 
			no.space = TRUE, omit.stat = c("rsq", "adj.rsq","f"), 
			out = paste(plot.wd, "/tb_", out.file, "_long_3way_wgr.html", sep="")	)			

# -------------------------------------- #
# 3-way DID with heavy vs. light smokers #
glm.ls6	<- list(NULL)
glm.ls6[[1]]	<- glmer(trip_cvs ~ after + cvs_in2 + heavy + after*cvs_in2 + after*heavy + after*cvs_in2*heavy + month1 + month1*cvs_in2 + (1|scity) , data = mydata, family = binomial(link = "logit"))  
glm.ls6[[2]]	<- glmer(trip_othdrug ~ after + cvs_in2 + heavy + after*cvs_in2 + after*heavy + after*cvs_in2*heavy + month1 + month1*cvs_in2 + (1|scity) , data = mydata, family = binomial(link = "logit"))  
glm.ls6[[3]]	<- glmer(trip_cvs ~ after + cvs_in2 + heavy + after*cvs_in2 + after*heavy + after*cvs_in2*heavy + month1 + month1*cvs_in2 + (1|household_code) , data = mydata, family = binomial(link = "logit"))  
glm.ls6[[4]]	<- glmer(trip_othdrug ~ after + cvs_in2 + heavy + after*cvs_in2 + after*heavy + after*cvs_in2*heavy + month1 + month1*cvs_in2 + (1|household_code) , data = mydata, family = binomial(link = "logit"))  

stargazer(glm.ls6, type = "html", align = TRUE, title = "3-way DID logit regressions of monthly shopping incidence", 
			keep = c("after", "cvs_in2", "heavy"),
			covariate.labels = c("After", "CloseDist", "Heavy", "After*CloseDist", 
								"After*Heavy", "CloseDist*Heavy", "After*CloseDist*Heavy"),  
			add.lines = list(c("City", "RE","RE","",""), c("Household","","","RE","RE"), c("Month", "FE","FE","FE","FE")), 
			no.space = TRUE, omit.stat = c("rsq", "adj.rsq","f"), 
			out = paste(plot.wd, "/tb_", out.file, "_long_3way_heavy.html", sep="")	)			

###############################
# DID regressions of spending # 
###############################
# --------------------- #
# 2-way DID on spending # 
fit.ls1	<- list(NULL)
fit.ls1[[1]]	<- plm(dol_cvs ~ after + cvs_in2 + after*cvs_in2 , data = mydata1, 
					index = c("scity", "hhmonth"), model = "within") 
fit.ls1[[2]]	<- plm(dol_othdrug ~ after + cvs_in2 + after*cvs_in2  , data = mydata1, 
					index = c("scity", "hhmonth"), model = "within")
fit.ls1[[3]]	<- plm(dol_cvs ~ after + cvs_in2 + after*cvs_in2  , data = mydata1, 
					index = c("household_code", "month"), model = "within")
fit.ls1[[4]]	<- plm(dol_othdrug ~ after + cvs_in2 + after*cvs_in2  , data = mydata1, 
					index = c("household_code", "month"), model = "within") 
cls.se		<- lapply(fit.ls1, function(x) Cls.se.fn(x, cluster.vec = mydata1$household_code, est.table = FALSE)$se)

stargazer(fit.ls1, type = "html", align = TRUE, title = "DID regressions of monthly spending", 
		keep = c("after", "cvs_in2"), se = cls.se,
		covariate.labels = c("After", "CloseDist", "After*CloseDist"),  
		add.lines = list(c("City FE", "FE","FE","",""), c("Household FE","","","FE","FE")), 
		no.space = TRUE, omit.stat = c("rsq", "adj.rsq","f"), 
		notes = "S.E. clustered over households", 
		out = paste(plot.wd, "/tb_", out.file, "_dol_2way.html", sep=""))	

# ------------------------------------------------------------------ #
# DID linear regressions of spending with 3-way DID (Close Walgreen) #
fit.ls2	<- list(NULL)
fit.ls2[[1]]	<- plm(dol_cvs ~ after + cvs_in2 + wgr_in2 + after*cvs_in2 + after*wgr_in2 + cvs_in2*wgr_in2 + after*cvs_in2*wgr_in2  , data = mydata1, 
					index = c("scity", "hhmonth"), model = "within") 
fit.ls2[[2]]	<- plm(dol_othdrug ~ after + cvs_in2 + wgr_in2 + after*cvs_in2 + after*wgr_in2 + cvs_in2*wgr_in2 + after*cvs_in2*wgr_in2  , data = mydata1, 
					index = c("scity", "hhmonth"), model = "within")
fit.ls2[[3]]	<- plm(dol_cvs ~ after + cvs_in2 + wgr_in2 + after*cvs_in2 + after*wgr_in2 + cvs_in2*wgr_in2 + after*cvs_in2*wgr_in2  , data = mydata1, 
					index = c("household_code", "month"), model = "within") 
fit.ls2[[4]]	<- plm(dol_othdrug ~ after + cvs_in2 + wgr_in2 + after*cvs_in2 + after*wgr_in2 + cvs_in2*wgr_in2 + after*cvs_in2*wgr_in2  , data = mydata1, 
					index = c("household_code", "month"), model = "within")

cls.se		<- lapply(fit.ls2, function(x) Cls.se.fn(x, cluster.vec = mydata1$household_code, est.table = FALSE)$se)

stargazer(fit.ls2, type = "html", align = TRUE, title = "3-way DID regressions of monthly spending", 
		keep = c("after", "cvs_in2", "wgr_in2"), se = cls.se,
		covariate.labels = c("After", "CloseDist", "CloseWgr", "After*CloseDist", 
							"After*CloseWgr", "CloseDist*CloseWgr", "After*CloseDist*CloseWgr"),  
		add.lines = list(c("City FE", "FE","FE","",""), c("Household FE","","","FE","FE")), 
		no.space = TRUE, omit.stat = c("rsq", "adj.rsq","f"), 
		notes = "S.E. clustered over households", 
		out = paste(plot.wd, "/tb_", out.file, "_dol_3way_wgr.html", sep=""))

# ------------------------------------------------------------------ #
# DID linear regressions of spending with 3-way DID (Close Walgreen) #
fit.ls3	<- list(NULL)
fit.ls3[[1]]	<- plm(dol_cvs ~ after + cvs_in2 + heavy + after*cvs_in2 + after*heavy + cvs_in2*heavy + after*cvs_in2*heavy  , data = mydata1, 
					index = c("scity", "hhmonth"), model = "within") 
fit.ls3[[2]]	<- plm(dol_othdrug ~ after + cvs_in2 + heavy + after*cvs_in2 + after*heavy + cvs_in2*heavy + after*cvs_in2*heavy  , data = mydata1, 
					index = c("scity", "hhmonth"), model = "within")
fit.ls3[[3]]	<- plm(dol_cvs ~ after + cvs_in2 + heavy + after*cvs_in2 + after*heavy + cvs_in2*heavy + after*cvs_in2*heavy  , data = mydata1, 
					index = c("household_code", "month"), model = "within") 
fit.ls3[[4]]	<- plm(dol_othdrug ~ after + cvs_in2 + heavy + after*cvs_in2 + after*heavy + cvs_in2*heavy + after*cvs_in2*heavy  , data = mydata1, 
					index = c("household_code", "month"), model = "within")

cls.se		<- lapply(fit.ls3, function(x) Cls.se.fn(x, cluster.vec = mydata1$household_code, est.table = FALSE)$se)

stargazer(fit.ls3, type = "html", align = TRUE, title = "3-way DID regressions of monthly spending", 
		keep = c("after", "cvs_in2", "heavy"), se = cls.se,
		covariate.labels = c("After", "CloseDist", "Heavy", "After*CloseDist", 
							"After*Heavy", "CloseDist*Heavy", "After*CloseDist*Heavy"),  
		add.lines = list(c("City FE", "FE","FE","",""), c("Household FE","","","FE","FE")), 
		no.space = TRUE, omit.stat = c("rsq", "adj.rsq","f"), 
		notes = "S.E. clustered over households", 
		out = paste(plot.wd, "/tb_", out.file, "_dol_3way_heavy.html", sep=""))

# Longer window #
# --------------------- #
# 2-way DID on spending # 
fit.ls4	<- list(NULL)
fit.ls4[[1]]	<- plm(dol_cvs ~ after + cvs_in2 + after*cvs_in2 + month1 + month1*cvs_in2, data = mydata, 
					index = c("scity", "hhmonth"), model = "within") 
fit.ls4[[2]]	<- plm(dol_othdrug ~ after + cvs_in2 + after*cvs_in2 + month1 + month1*cvs_in2 , data = mydata, 
					index = c("scity", "hhmonth"), model = "within")
fit.ls4[[3]]	<- plm(dol_cvs ~ after + cvs_in2 + after*cvs_in2 + month1 + month1*cvs_in2 , data = mydata, 
					index = c("household_code", "month"), model = "within")
fit.ls4[[4]]	<- plm(dol_othdrug ~ after + cvs_in2 + after*cvs_in2 + month1 + month1*cvs_in2 , data = mydata, 
					index = c("household_code", "month"), model = "within") 
cls.se		<- lapply(fit.ls4, function(x) Cls.se.fn(x, cluster.vec = mydata$household_code, est.table = FALSE)$se)

stargazer(fit.ls4, type = "html", align = TRUE, title = "DID regressions of monthly spending", 
		keep = c("after", "cvs_in2"), se = cls.se,
		covariate.labels = c("After", "CloseDist", "After*CloseDist"),  
		add.lines = list(c("City FE", "FE","FE","",""), c("Household FE","","","FE","FE"), 
						 c("Month FE", "FE","FE","FE","FE")), 
		no.space = TRUE, omit.stat = c("rsq", "adj.rsq","f"), 
		notes = "S.E. clustered over households", 
		out = paste(plot.wd, "/tb_", out.file, "_long_dol_2way.html", sep=""))	

# ------------------------------------------------------------------ #
# DID linear regressions of spending with 3-way DID (Close Walgreen) #
fit.ls5	<- list(NULL)
fit.ls5[[1]]	<- plm(dol_cvs ~ after + cvs_in2 + wgr_in2 + after*cvs_in2 + after*wgr_in2 + cvs_in2*wgr_in2 + after*cvs_in2*wgr_in2 + month1 + month1*cvs_in2 , data = mydata, 
					index = c("scity", "hhmonth"), model = "within") 
fit.ls5[[2]]	<- plm(dol_othdrug ~ after + cvs_in2 + wgr_in2 + after*cvs_in2 + after*wgr_in2 + cvs_in2*wgr_in2 + after*cvs_in2*wgr_in2 + month1 + month1*cvs_in2 , data = mydata, 
					index = c("scity", "hhmonth"), model = "within")
fit.ls5[[3]]	<- plm(dol_cvs ~ after + cvs_in2 + wgr_in2 + after*cvs_in2 + after*wgr_in2 + cvs_in2*wgr_in2 + after*cvs_in2*wgr_in2 + month1 + month1*cvs_in2 , data = mydata, 
					index = c("household_code", "month"), model = "within") 
fit.ls5[[4]]	<- plm(dol_othdrug ~ after + cvs_in2 + wgr_in2 + after*cvs_in2 + after*wgr_in2 + cvs_in2*wgr_in2 + after*cvs_in2*wgr_in2 + month1 + month1*cvs_in2 , data = mydata, 
					index = c("household_code", "month"), model = "within")

cls.se		<- lapply(fit.ls5, function(x) Cls.se.fn(x, cluster.vec = mydata$household_code, est.table = FALSE)$se)

stargazer(fit.ls5, type = "html", align = TRUE, title = "3-way DID regressions of monthly spending", 
		keep = c("after", "cvs_in2", "wgr_in2"), se = cls.se,
		covariate.labels = c("After", "CloseDist", "CloseWgr", "After*CloseDist", 
							"After*CloseWgr", "CloseDist*CloseWgr", "After*CloseDist*CloseWgr"),  
		add.lines = list(c("City FE", "FE","FE","",""), c("Household FE","","","FE","FE"), 
						 c("Month FE", "FE","FE","FE","FE")), 
		no.space = TRUE, omit.stat = c("rsq", "adj.rsq","f"), 
		notes = "S.E. clustered over households", 
		out = paste(plot.wd, "/tb_", out.file, "_long_dol_3way_wgr.html", sep=""))

# ------------------------------------------------------------------ #
# DID linear regressions of spending with 3-way DID (Close Walgreen) #
fit.ls6	<- list(NULL)
fit.ls6[[1]]	<- plm(dol_cvs ~ after + cvs_in2 + heavy + after*cvs_in2 + after*heavy + cvs_in2*heavy + after*cvs_in2*heavy + month1 + month1*cvs_in2 , data = mydata, 
					index = c("scity", "hhmonth"), model = "within") 
fit.ls6[[2]]	<- plm(dol_othdrug ~ after + cvs_in2 + heavy + after*cvs_in2 + after*heavy + cvs_in2*heavy + after*cvs_in2*heavy + month1 + month1*cvs_in2 , data = mydata, 
					index = c("scity", "hhmonth"), model = "within")
fit.ls6[[3]]	<- plm(dol_cvs ~ after + cvs_in2 + heavy + after*cvs_in2 + after*heavy + cvs_in2*heavy + after*cvs_in2*heavy + month1 + month1*cvs_in2 , data = mydata, 
					index = c("household_code", "month"), model = "within") 
fit.ls6[[4]]	<- plm(dol_othdrug ~ after + cvs_in2 + heavy + after*cvs_in2 + after*heavy + cvs_in2*heavy + after*cvs_in2*heavy + month1 + month1*cvs_in2 , data = mydata, 
					index = c("household_code", "month"), model = "within")

cls.se		<- lapply(fit.ls6, function(x) Cls.se.fn(x, cluster.vec = mydata$household_code, est.table = FALSE)$se)

stargazer(fit.ls6, type = "html", align = TRUE, title = "3-way DID regressions of monthly spending", 
		keep = c("after", "cvs_in2", "heavy"), se = cls.se,
		covariate.labels = c("After", "CloseDist", "Heavy", "After*CloseDist", 
							"After*Heavy", "CloseDist*Heavy", "After*CloseDist*Heavy"),  
		add.lines = list(c("City FE", "FE","FE","",""), c("Household FE","","","FE","FE"), 
						 c("Month FE", "FE","FE","FE","FE")), 
		no.space = TRUE, omit.stat = c("rsq", "adj.rsq","f"), 
		notes = "S.E. clustered over households", 
		out = paste(plot.wd, "/tb_", out.file, "_long_dol_3way_heavy.html", sep=""))


save.image(file = paste(plot.wd, "/", out.file, "_",Sys.Date(),".rdata", sep=""))			

cat("This program is done.\n")
		

