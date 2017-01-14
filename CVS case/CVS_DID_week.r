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
# setwd("U:/Users/ccv103/Desktop")
setwd("/sscc/home/c/ccv103/Tobacco") 
plot.wd		<- getwd()

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
cvs.ret	<- 4914		# retailer_code for CVS
qunit	<- 20		# 20 cigaretts per pack 
firstw	<- as.Date("2012-12-31", format = "%Y-%m-%d") 	# The first week in 2013
purchases$purchase_date	<- as.Date(as.character(purchases$purchase_date), format = "%Y-%m-%d")
purchases$week		<- ((as.numeric(purchases$purchase_date - firstw)) %/%7 + 1)* 7 + firstw - 1
purchases$year		<- year(purchases$purchase_date)

trips$purchase_date	<- as.Date(as.character(trips$purchase_date), format = "%Y-%m-%d")
trips$week			<- ((as.numeric(trips$purchase_date - firstw)) %/%7 + 1)* 7 + firstw - 1
trips$year			<- year(trips$purchase_date)
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

panelists$cvs_in2	<- ifelse(panelists$distance <=2, 1, 0)
panelists$cvs_in5	<- ifelse(panelists$distance <=5, 1, 0)
panelists$cvs_in10	<- ifelse(panelists$distance <=10, 1, 0)
purchases	<- merge(purchases, panelists[,c("household_code", "panel_year", "distance","cvs_in2", "cvs_in5", "cvs_in10")], 
						by.x = c("household_code", "year"), by.y = c("household_code", "panel_year"), all.x=T)
trips	<- merge(trips, panelists[,c("household_code", "panel_year", "cvs_in2", "cvs_in5", "cvs_in10")], 
						by.x = c("household_code", "year"), by.y = c("household_code", "panel_year"), all.x=T)						
purchases$cvs_c	<- factor(purchases$cvs, levels = c(1,0), labels = c("CVS", "Non-CVS"))

# -------------------------- #
# Fill in non-puchases weeks # 
# Complete week for each household 
tmp		<- data.table(purchases)
tmp		<- tmp[,list(start = min(week), end = max(week)), by = list(household_code)]
tmp		<- tmp[, n:= as.numeric(end-start)/7]
tmp1	<- lapply(1:nrow(tmp), function(i) tmp[i,start] + c(0:tmp[i,n])*7)
names(tmp1)	<- tmp$household_code
tmp1	<- melt(tmp1)
names(tmp1)	<- c("week", "household_code")

# Actual cigarette purchases 
tmp2	<- data.table(purchases)
tmp2	<- tmp2[,list(	q = sum(quantity*size/qunit, na.rm=T), 
						q_othdrug = sum(quantity*size*(1-cvs)/qunit, na.rm=T), 
						q_othchannel = sum(quantity*size*1*(channel_type != "Drug Store")/qunit, na.rm=T)), 
				by = list(household_code, week)]
dim(tmp1); dim(tmp2)
mydata	<- merge(tmp1, tmp2, by = c("household_code", "week"), all.x = T)
dim(mydata)

# Trips
tmp1 	<- data.table(trips)
tmp1	<- tmp1[,list(	trip_cvs = 1*(sum(cvs)>0), 
						trip_othdrug = sum(channel_type == "Drug Store" & cvs ==0 ) ), 
				by = list(household_code, week)]
dim(tmp1)		
tmp1	<- tmp1[, trip_othdrug := 1*(trip_othdrug >0 )]		
mydata	<- merge(mydata, tmp1, by = c("household_code", "week"), all.x = T)
sel 	<- is.na(mydata)
mydata[sel]	<- 0
cat("Summary stats:\n"); print(summary(mydata[, -c(1:2)])); cat("\n")

# Create other control variables: city and month 
mydata$year		<- year(mydata$week)
mydata$month	<- month(mydata$week) 
mydata			<- merge(mydata, panelists[,c("household_code", "panel_year", "panelist_zip_code","distance", "cvs_in2")], 
						by.x = c("household_code", "year"), by.y = c("household_code", "panel_year"), all.x=T)
dim(mydata)
mydata$panelist_zip_code	<- clean.zipcodes(mydata$panelist_zip_code)
data(zipcode)
mydata			<- merge(mydata, zipcode[,c("zip","city","state")], by.x = "panelist_zip_code", by.y = "zip")
mydata$scity	<- with(mydata, paste(state, city, sep="-"))

mydata$after	<- 1*(mydata$week >= event.date)
mydata$hhweek	<- with(mydata, paste(household_code, week, sep="-"))

###################
# Run regressions # 
###################
# DID linear regressions of Cigarette quantity in different places 
fit.ls	<- list(NULL)
fit.ls[[1]]	<- plm(q ~ after + cvs_in2 + after*cvs_in2 + factor(month), data = mydata, 
					index = c("scity", "hhweek"), model = "within") 
fit.ls[[2]]	<- plm(q_othdrug ~ after + cvs_in2 + after*cvs_in2 + factor(month), data = mydata, 
					index = c("scity", "hhweek"), model = "within")
fit.ls[[3]]	<- plm(q_othchannel ~ after + cvs_in2 + after*cvs_in2 + factor(month), data = mydata, 
					index = c("scity", "hhweek"), model = "within")
fit.ls[[4]]	<- plm(q ~ after + cvs_in2 + after*cvs_in2 + factor(month), data = mydata, 
					index = c("household_code", "week"), model = "within") 
fit.ls[[5]]	<- plm(q_othdrug ~ after + cvs_in2 + after*cvs_in2 + factor(month), data = mydata, 
					index = c("household_code", "week"), model = "within")
fit.ls[[6]]	<- plm(q_othchannel ~ after + cvs_in2 + after*cvs_in2 + factor(month), data = mydata, 
					index = c("household_code", "week"), model = "within")

cls.se		<- lapply(fit.ls[1:4], function(x) Cls.se.fn(x, cluster.vec = mydata$household_code, est.table = FALSE)$se)

stargazer(fit.ls, type = "html", align = TRUE, title = "DID regressions of weekly cigarette quantity", 
		keep = c("after", "cvs_in2"),
		covariate.labels = c("After", "CloseDist", "After*CloseDist"),  
		add.lines = list(c("City FE", "FE","FE","FE","","",""), c("Household FE","","","","FE","FE","FE")), 
		no.space = TRUE, omit.stat = c("rsq", "adj.rsq"), 
		out = paste(plot.wd, "/regout_lm_q.html", sep=""))					
		
# Trip visit 
glm.ls	<- list(NULL)
glm.ls[[1]]	<- glmer(trip_cvs ~ after + cvs_in2 + after*cvs_in2 + (1|scity) + (1|month), data = mydata, family = binomial(link = "logit"))  
glm.ls[[2]]	<- glmer(trip_othdrug ~ after + cvs_in2 + after*cvs_in2 + (1|scity) + (1|month), data = mydata, family = binomial(link = "logit"))  
glm.ls[[3]]	<- glmer(trip_cvs ~ after + cvs_in2 + after*cvs_in2 + (1|household_code) + (1|month), data = mydata, family = binomial(link = "logit"))  
glm.ls[[4]]	<- glmer(trip_othdrug ~ after + cvs_in2 + after*cvs_in2 + (1|household_code) + (1|month), data = mydata, family = binomial(link = "logit"))  

stargazer(glm.ls, type = "text", align = TRUE, title = "DID logit regressions of weekly shopping incidence", 
			keep = c("after", "cvs_in2"),
			covariate.labels = c("After", "CloseDist", "After*CloseDist"),  
			add.lines = list(c("City", "RE","RE","",""), c("Household","","","RE","RE")), 
			no.space = TRUE, omit.stat = c("rsq", "adj.rsq"), 
			out = paste(plot.wd, "/regout_glm_trip.html", sep="")	)

##################################
# DID regression of small window # 
##################################			
# Restrict to a short window 
mydata		<- subset(mydata, abs(week - event.date) <= 100)

# DID linear regressions of Cigarette quantity in different places 
fit.ls	<- list(NULL)
fit.ls[[1]]	<- plm(q ~ after + cvs_in2 + after*cvs_in2 , data = mydata, 
					index = c("scity", "hhweek"), model = "within") 
fit.ls[[2]]	<- plm(q_othdrug ~ after + cvs_in2 + after*cvs_in2 , data = mydata, 
					index = c("scity", "hhweek"), model = "within")
fit.ls[[3]]	<- plm(q_othchannel ~ after + cvs_in2 + after*cvs_in2 , data = mydata, 
					index = c("scity", "hhweek"), model = "within")
fit.ls[[4]]	<- plm(q ~ after + cvs_in2 + after*cvs_in2 , data = mydata, 
					index = c("household_code", "week"), model = "within") 
fit.ls[[5]]	<- plm(q_othdrug ~ after + cvs_in2 + after*cvs_in2 , data = mydata, 
					index = c("household_code", "week"), model = "within")
fit.ls[[6]]	<- plm(q_othchannel ~ after + cvs_in2 + after*cvs_in2 , data = mydata, 
					index = c("household_code", "week"), model = "within")

stargazer(fit.ls, type = "html", align = TRUE, title = "DID regressions of weekly cigarette quantity", 
		keep = c("after", "cvs_in2"),
		covariate.labels = c("After", "CloseDist", "After*CloseDist"),  
		add.lines = list(c("City FE", "FE","FE","FE","","",""), c("Household FE","","","","FE","FE","FE")), 
		no.space = TRUE, omit.stat = c("rsq", "adj.rsq"), 
		out = paste(plot.wd, "/regout_lm_q_nr.html", sep=""))					
		
# Trip visit 
glm.ls	<- list(NULL)
glm.ls[[1]]	<- glmer(trip_cvs ~ after + cvs_in2 + after*cvs_in2 + (1|scity) , data = mydata, family = binomial(link = "logit"))  
glm.ls[[2]]	<- glmer(trip_othdrug ~ after + cvs_in2 + after*cvs_in2 + (1|scity) , data = mydata, family = binomial(link = "logit"))  
glm.ls[[3]]	<- glmer(trip_cvs ~ after + cvs_in2 + after*cvs_in2 + (1|household_code) , data = mydata, family = binomial(link = "logit"))  
glm.ls[[4]]	<- glmer(trip_othdrug ~ after + cvs_in2 + after*cvs_in2 + (1|household_code) , data = mydata, family = binomial(link = "logit"))  

stargazer(glm.ls, type = "text", align = TRUE, title = "DID logit regressions of weekly shopping incidence", 
			keep = c("after", "cvs_in2"),
			covariate.labels = c("After", "CloseDist", "After*CloseDist"),  
			add.lines = list(c("City", "RE","RE","",""), c("Household","","","RE","RE")), 
			no.space = TRUE, omit.stat = c("rsq", "adj.rsq"), 
			out = paste(plot.wd, "/regout_glm_trip_nr.html", sep="")	)			
			
save.image(file = paste(plot.wd, "/cvs_didreg_narrow.rdata", sep=""))			

cat("This program is done.\n")
