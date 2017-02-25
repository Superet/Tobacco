library(reshape2)
library(ggplot2)
library(data.table)
library(lubridate)
library(stargazer)
library(zipcode)
library(plm)
library(lme4)
library(xlsx)
library(stargazer)
library(MatchIt)

# setwd("~/Documents/Research/Tobacco/processed_data")
# plot.wd		<- "~/Desktop"
setwd("U:/Users/ccv103/Documents/Research/tobacco/processed_data")
# setwd("/sscc/home/c/ccv103/Tobacco") 
plot.wd		<- getwd()
out.file	<- "cvs_did_month_testnsmk"

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
sel.channel		<- c("Grocery", "Discount Store", "Drug Store", "Warehouse Club", "Dollar Store", "Convenience Store", 
					"Service Station", "All Other Stores", "Gas Mini Mart", "Tobacco Store", "Health Food Store")
trips			<- subset(trips, channel_type %in% sel.channel)					
purchases		<- subset(purchases, trip_code_uc %in% trips$trip_code_uc)		

# # Select a random sample
# sel		<- sample(unique(panelists$household_code), .05*length(unique(panelists$household_code)))
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
mypan[is.na(mypan$heavy), "heavy"]	<- 0

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
names(mypan)[grep("distance", names(mypan))[1]] <- "distance_cvs"           # Change name to avoid confusion in later matching (distance is propensity score)
demo.col				<- c("distance_cvs", "income", "age", "have_kids", "employment", "race")
sel						<- sapply(demo.col, function(i) is.numeric(mypan[,i]))
summary(mypan[,demo.col[sel]])
lapply(demo.col[!sel], function(i) table(mypan[,i]))

purchases	<- subset(purchases, household_code %in% mypan$household_code)
trips		<- subset(trips, household_code %in% mypan$household_code)
trips		<- merge(trips, mypan[,c("household_code","cvs_in2","heavy","ban_ard")], by = "household_code", all.x = T)

############################
# Organize regression data # 
############################
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
tmp3	<- tmp3[,list(	cigdol 		= sum(total_price_paid - coupon_value, na.rm=T), 
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
tmp2	<- tmp2[month < event.month,]
tmp2	<- tmp2[,list(	pre_trip_cvs 		= mean(trip_cvs), 
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
bhv.col		<- c("pre_trip_cvs", "pre_trip_othdrug", "pre_trip_othchannel", "pre_dol_cvs", "pre_dol_othdrug", "pre_dol_othchannel")
bhv.col   <- paste(rep(bhv.col, 4), "_H", rep(1:4, each = 6), sep="")
sapply(bhv.col, function(i) sum(is.na(mypan[,i])))				
sel		<- apply(mypan[,bhv.col], 1, function(x) any(is.na(x)))
sum(sel)
mypan	<- mypan[!sel,]

# Create other control variables: city and month 
mydata$year		<- ifelse(mydata$month > 12, 2014, 2013)
mydata$month1	<- mydata$month %% 12
mydata$month1	<- ifelse(mydata$month1 == 0, 12, mydata$month1)
mydata$month1	<- factor(mydata$month1)
dim(mydata)
mydata			<- merge(mydata, mypan[,c("household_code", "panelist_zip_code","distance_cvs", "cvs_in2", "wgr_in2", "heavy", "smk", "ban_ard", demo.col)], 
						by = "household_code", all.x=T)
dim(mydata)
mydata$after	<- 1*(mydata$month >= event.month)
mydata$hhmonth	<- paste(mydata$household_code, mydata$month, sep="-")
mydata$treat	<- with(mydata, 1*(smk == 1 & ban_ard == 0))
mypan$treat		<- with(mypan, 1*(smk == 1 & ban_ard == 0))
trips$treat		<- with(trips, 1*(smk == 1 & ban_ard == 0))
table(mydata$treat)
table(mypan$smk == 1 & mypan$ban_ard == 0)

# Check if any missing values 
sapply(demo.col, function(i) sum(is.na(mydata[,i])))

rm(list = c("tmp", "tmp1", "tmp2", "tmp3"))

########################
# Descriptive analysis #
########################
# Demographic difference between smokers and nonsmokers 
sel		<- sapply(demo.col, function(i) is.numeric(mypan[,i]))
ggtab	<- NULL
for(i in c(demo.col[sel], bhv.col)){
	tmp		<- t.test(mypan[,i] ~ mypan$treat)
	tmp1	<- c(tmp$estimate, dif = diff(tmp$estimate), tmp$statistic, p = tmp$p.value)
	ggtab	<- rbind(ggtab, tmp1)
}
ggtab			<- rbind(c(table(mypan$treat), rep(NA, 3)), ggtab)
rownames(ggtab)	<- c("N", demo.col[sel], bhv.col)
colnames(ggtab)	<- c("Non-smokers", "Smokers", "Difference", "t stat", "P value")

ggtab1		<- NULL
for(i in demo.col[!sel]){
	tmp		<- table(mypan[,i], mypan$treat)
	tmp1	<- chisq.test(tmp)
	tmp 	<- apply(tmp, 2, function(x) x/sum(x))
	tmp1	<- rbind(cbind(tmp, tmp[,2]-tmp[,1], NA, NA), c(NA, NA, NA,tmp1$statistic, p = tmp1$p.value))
	ggtab1	<- rbind(ggtab1, tmp1)
}
colnames(ggtab1)	<- c("Non-smokers", "Smokers", "Difference", "t stat", "P value")
cat("Tests of demongraphic differences:\n"); print(round(rbind(ggtab, ggtab1), 2)); cat("\n")

stargazer(rbind(ggtab, ggtab1), type = "html", align = TRUE, no.space = TRUE, 
		title = "Tests of demographic difference", 
		out = paste(plot.wd, "/tb_", out.file, "_demotest_", Sys.Date(), ".html", sep=""))

#----------------#
# Plot the trips #
# Shopping trips per week per person #
ggtmp	<- data.table(subset(trips, cvs_in2 ==1 & !week %in% endweek))
ggtmp	<- ggtmp[,list(total_spent = sum(total_spent)), by = list(treat,household_code, month, week, purchase_date, channel_type, retailer_code, cvs)]
ggtmp	<- ggtmp[,list(	Total = length(purchase_date)/length(unique(household_code)), 
						CVS = length(purchase_date[cvs==1])/length(unique(household_code)), 
						OtherChannel = length(purchase_date[cvs==0 & channel_type != "Drug Store"])/length(unique(household_code)), 
						Grocery		= length(purchase_date[cvs==0 & channel_type == "Grocery"])/length(unique(household_code))), 
				by = list(treat, week)]
ggtmp	<- melt(ggtmp, id.vars = c("treat", "week"))		
ggtmp$treat	<- factor(ggtmp$treat, levels = c(1,0), labels = c("Smoker", "Non-smoker"))
p1 		<- ggplot(ggtmp, aes(week, value, linetype = treat)) + geom_line() + 
			geom_vline(xintercept = as.numeric(event.date), col = "red") + 
			facet_wrap(~variable, scales = "free") + 
			theme(legend.position = "bottom") + 
			labs(y = "Trips per person", title = "Shopping trips by consumer groups")

# Expenditure 
ggtmp	<- data.table(subset(trips, cvs_in2 ==1 & !week %in% endweek))
ggtmp	<- ggtmp[,list(	Total = sum(total_spent)/length(unique(household_code)), 
						CVS = sum(total_spent[cvs==1])/length(unique(household_code)), 
						OtherChannel = sum(total_spent[cvs==0 & channel_type != "Drug Store"])/length(unique(household_code)),  
						Grocery	= sum(total_spent[cvs==0 & channel_type == "Grocery"])/length(unique(household_code)) ), 
				by = list(treat, week)]
ggtmp	<- melt(ggtmp, id.vars = c("treat", "week"))		
ggtmp$treat	<- factor(ggtmp$treat, levels = c(1,0), labels = c("Smoker", "Non-smoker"))
p2		<- ggplot(ggtmp, aes(week, value, linetype = treat)) + geom_line() + 
			geom_vline(xintercept = as.numeric(event.date), col = "red") + 
			facet_wrap(~variable, scales = "free") + 
			theme(legend.position = "bottom") + 
			labs(y = "Expenditure per person", title = "Expenditure by consumer groups")

pdf(paste(plot.wd,"/fg_",out.file, "_trend_", Sys.Date(), ".pdf", sep=""), width = 7.5, height = 7.5*ar)			
print(p1)
print(p2)
dev.off()

#############################		
# Propensity score matching #		
#############################
my.ratio	<- 1
match.mod	<- matchit(treat ~ income + age + have_kids + employment + race + distance_cvs +
						pre_trip_cvs_H1+pre_trip_othdrug_H1 +pre_trip_othchannel_H1 +pre_dol_cvs_H1 +pre_dol_othdrug_H1 +pre_dol_othchannel_H1+
						pre_trip_cvs_H2+pre_trip_othdrug_H2 +pre_trip_othchannel_H2 +pre_dol_cvs_H2 +pre_dol_othdrug_H2 +pre_dol_othchannel_H2+ 
						pre_trip_cvs_H3+pre_trip_othdrug_H3 +pre_trip_othchannel_H3 +pre_dol_cvs_H3 +pre_dol_othdrug_H3 +pre_dol_othchannel_H3+
						pre_trip_cvs_H4+pre_trip_othdrug_H4 +pre_trip_othchannel_H4 +pre_dol_cvs_H4 +pre_dol_othdrug_H4 +pre_dol_othchannel_H4, 
				method = "nearest", data = mypan[,c("household_code","treat",demo.col,bhv.col)], ratio = my.ratio)
summary(match.mod)
match.pan	<- match.data(match.mod)
dim(match.pan)
table(match.pan$treat)

# Check demongraphic balance
blc		<- summary(match.mod)
cat("Summary of matching balance:\n"); print(blc); cat("\n")
sel 	<- c("Means Treated", "Means Control", "Mean Diff")
tmp		<- blc$sum.matched[,sel]
names(tmp)	<- paste("M_", names(tmp), sep="")
tmp		<- cbind(blc$sum.all[,sel], tmp)
tmp1	<- c(blc$nn[1,], NA, blc$nn[2,], NA)					# Number of controls and treatment
tmp		<- rbind(tmp1, tmp)
cat("The balance table:\n"); print(round(tmp,2)); cat("\n")
stargazer(tmp, type = "html", summary = FALSE, align = TRUE, no.space = TRUE, 
			title = "Balance Check",
			out = paste(plot.wd, "/tb_", out.file, "_balance_",Sys.Date(), ".html", sep=""))

# Check demongraphic balance with t test
sel		<- sapply(demo.col, function(i) is.numeric(mypan[,i]))
ggtab	<- NULL
for(i in c(demo.col[sel], bhv.col)){
	tmp		<- t.test(match.pan[,i] ~ match.pan$treat)
	tmp1	<- c(tmp$estimate, dif = diff(tmp$estimate), tmp$statistic, p = tmp$p.value)
	ggtab	<- rbind(ggtab, tmp1)
}
rownames(ggtab)	<- c(demo.col[sel], bhv.col)
colnames(ggtab)	<- c("Non-smokers", "Smokers", "Difference", "t stat", "P value")

ggtab1		<- NULL
for(i in demo.col[!sel]){
	tmp		<- table(match.pan[,i], match.pan$treat)
	tmp1	<- chisq.test(tmp)
	tmp 	<- apply(tmp, 2, function(x) x/sum(x))
	tmp1	<- rbind(cbind(tmp, tmp[,2]-tmp[,1], NA, NA), c(NA, NA, NA, tmp1$statistic, p = tmp1$p.value))
	ggtab1	<- rbind(ggtab1, tmp1)
}
colnames(ggtab1)	<- c("Non-smokers", "Smokers", "Difference", "t stat", "P value")
cat("Recheck demographic balance after matching:\n"); print(rbind(ggtab, ggtab1)); cat("\n")

# #----------------#
# Check the trend of the matched sample
# Shopping trips per week per person #
ggtmp	<- data.table(subset(trips, !week %in% endweek & household_code %in% match.pan$household_code))
ggtmp	<- ggtmp[,list(total_spent = sum(total_spent)), by = list(treat,household_code, month, week, purchase_date, channel_type, retailer_code, cvs)]
ggtmp	<- ggtmp[,list(	Total = length(purchase_date)/length(unique(household_code)), 
						CVS = length(purchase_date[cvs==1])/length(unique(household_code)), 
						OtherDrug = length(purchase_date[cvs==0 & channel_type == "Drug Store"])/length(unique(household_code)),
						OtherChannel = length(purchase_date[cvs==0 & channel_type != "Drug Store"])/length(unique(household_code)) 
						), 
				by = list(treat, week)]
ggtmp	<- melt(ggtmp, id.vars = c("treat", "week"))		
ggtmp$treat	<- factor(ggtmp$treat, levels = c(1,0), labels = c("Smoker", "Non-smoker"))
p1 		<- ggplot(ggtmp, aes(week, value, linetype = treat)) + geom_line() + 
			geom_vline(xintercept = as.numeric(event.date), col = "red") + 
			facet_wrap(~variable, scales = "free") + 
			theme(legend.position = "bottom") + 
			labs(y = "Trips per person", title = "Shopping trips by consumer groups")

# Expenditure 
ggtmp	<- data.table(subset(trips, !week %in% endweek & household_code %in% match.pan$household_code))
ggtmp	<- ggtmp[,list(	Total = sum(total_spent)/length(unique(household_code)), 
						CVS = sum(total_spent[cvs==1])/length(unique(household_code)), 
						OtherDrug = sum(total_spent[cvs==0 & channel_type == "Drug Store"])/length(unique(household_code)),
						OtherChannel = sum(total_spent[cvs==0 & channel_type != "Drug Store"])/length(unique(household_code))
						), 
				by = list(treat, week)]
ggtmp	<- melt(ggtmp, id.vars = c("treat", "week"))		
ggtmp$treat	<- factor(ggtmp$treat, levels = c(1,0), labels = c("Smoker", "Non-smoker"))
p2		<- ggplot(ggtmp, aes(week, value, linetype = treat)) + geom_line() + 
			geom_vline(xintercept = as.numeric(event.date), col = "red") + 
			facet_wrap(~variable, scales = "free") + 
			theme(legend.position = "bottom") + 
			labs(y = "Expenditure per person", title = "Expenditure by consumer groups")

pdf(paste(plot.wd,"/fg_",out.file, "_pmatch_trend_", Sys.Date(), ".pdf", sep=""), width = 7.5, height = 7.5*ar)			
print(p1)
print(p2)
dev.off()

# Run DID regressions 
mydata.match	<- subset(mydata, household_code %in% match.pan$household_code)
dim(mydata.match)
sel1		<- mydata.match$month >= event.month - 4 & mydata.match$month <= event.month + 3 & mydata.match$cvs_in2 == 1
sel2		<- mydata.match$cvs_in2 == 1
sel3		<- mydata.match$month >= event.month - 4 & mydata.match$month <= event.month + 3						# Small window

###############################################
# Run DID regressions with the matched sample # 
###############################################
# Trip incidences
trip.ls1	<- list(NULL)
trip.ls1[[1]]	<- glmer(trip_cvs ~ after + treat + after*treat+ (1|household_code), data = mydata.match[sel1,], 
                       family = poisson(link = "log")) 
trip.ls1[[2]]	<- glmer(trip_othdrug ~ after + treat + after*treat + (1|household_code), data = mydata.match[sel1,], 
                       family = poisson(link = "log"))
trip.ls1[[3]]	<- glmer(trip_othchannel ~ after + treat + after*treat + (1|household_code), data = mydata.match[sel1,], 
                       family = poisson(link = "log"))
trip.ls1[[4]]	<- glmer(trip_cvs ~ after + treat + after*treat + month1+ (1|household_code), data = mydata.match[sel2,], 
                       family = poisson(link = "log")) 
trip.ls1[[5]]	<- glmer(trip_othdrug ~ after + treat + after*treat + month1+ (1|household_code), data = mydata.match[sel2,], 
                       family = poisson(link = "log"))
trip.ls1[[6]]	<- glmer(trip_othchannel ~ after + treat + after*treat + month1+ (1|household_code), data = mydata.match[sel2,], 
                       family = poisson(link = "log"))
trip.ls1[[7]]	<- glmer(trip_cvs ~ after + treat + after*treat+ (1|household_code), data = mydata.match[sel3,], 
                       family = poisson(link = "log")) 
trip.ls1[[8]]	<- glmer(trip_othdrug ~ after + treat + after*treat + (1|household_code), data = mydata.match[sel3,], 
                       family = poisson(link = "log"))
trip.ls1[[9]]	<- glmer(trip_othchannel ~ after + treat + after*treat + (1|household_code), data = mydata.match[sel3,], 
                       family = poisson(link = "log"))
trip.ls1[[10]]	<- glmer(trip_cvs ~ after + treat + after*treat + month1+ (1|household_code), data = mydata.match, 
                        family = poisson(link = "log")) 
trip.ls1[[11]]	<- glmer(trip_othdrug ~ after + treat + after*treat + month1+ (1|household_code), data = mydata.match, 
                        family = poisson(link = "log"))
trip.ls1[[12]]	<- glmer(trip_othchannel ~ after + treat + after*treat + month1+ (1|household_code), data = mydata.match, 
                        family = poisson(link = "log"))

cls.se1		<- c(lapply(trip.ls1[1:3], function(x) Cls.se.fn(x, cluster.vec = mydata.match[sel1,"household_code"])), 
				 lapply(trip.ls1[4:6], function(x) Cls.se.fn(x, cluster.vec = mydata.match[sel2,"household_code"])), 
				 lapply(trip.ls1[7:9], function(x) Cls.se.fn(x, cluster.vec = mydata.match[sel3,"household_code"])), 
				 lapply(trip.ls1[10:12], function(x) Cls.se.fn(x, cluster.vec = mydata.match[,"household_code"])) )	

myline  <- list(c("Household",rep("RE", 12)), c("Month", rep(rep(c("", "FE"), each = 3), 2)), 
                c("Window", rep(rep(c("8 M.", "2 Yr"),each = 3), 2)), c("Distance", rep(c("< 2mi.", "Unrestricted"), each = 6)) )
stargazer(trip.ls1, type = "html", align = TRUE, title = "Regressions of monthly trip incidence on matched sample using non-smokers and SF and MA as control", 
		keep = c("after", "treat"), se = cls.se1, model.names = FALSE,
		covariate.labels = c("After", "Treat","After*Treat"),  
		add.lines = myline, 
		no.space = TRUE, omit.stat = c("rsq", "adj.rsq", "f","aic","bic","ll"), 
		notes = "S.E. clustered over households", 
		out = paste(plot.wd, "/tb_", out.file, "_pmatch_trip_", Sys.Date(), ".html", sep=""))

# ---------------------------------- #
# Expenditures at different channels # 
dol.ls1	<- list(NULL)
dol.ls1[[1]]	<- plm(dol_cvs ~ after + treat + after*treat , data = mydata.match[sel1,], 
					index = c("household_code", "month"), model = "within") 
dol.ls1[[2]]	<- plm(dol_othdrug ~ after + treat + after*treat , data = mydata.match[sel1,], 
					index = c("household_code", "month"), model = "within")
dol.ls1[[3]]	<- plm(dol_othchannel ~ after + treat + after*treat , data = mydata.match[sel1,], 
					index = c("household_code", "month"), model = "within")
dol.ls1[[4]]	<- plm(dol_cvs ~ after + treat + after*treat + month1, data = mydata.match[sel2,], 
					index = c("household_code", "month"), model = "within") 
dol.ls1[[5]]	<- plm(dol_othdrug ~ after + treat + after*treat + month1, data = mydata.match[sel2,], 
					index = c("household_code", "month"), model = "within")
dol.ls1[[6]]	<- plm(dol_othchannel ~ after + treat + after*treat + month1, data = mydata.match[sel2,], 
					index = c("household_code", "month"), model = "within")
dol.ls1[[7]]	<- plm(dol_cvs ~ after + treat + after*treat , data = mydata.match[sel3,], 
					index = c("household_code", "month"), model = "within") 
dol.ls1[[8]]	<- plm(dol_othdrug ~ after + treat + after*treat , data = mydata.match[sel3,], 
					index = c("household_code", "month"), model = "within")
dol.ls1[[9]]	<- plm(dol_othchannel ~ after + treat + after*treat , data = mydata.match[sel3,], 
					index = c("household_code", "month"), model = "within")
dol.ls1[[10]]	<- plm(dol_cvs ~ after + treat + after*treat + month1, data = mydata.match, 
					index = c("household_code", "month"), model = "within") 
dol.ls1[[11]]	<- plm(dol_othdrug ~ after + treat + after*treat + month1, data = mydata.match, 
					index = c("household_code", "month"), model = "within")
dol.ls1[[12]]	<- plm(dol_othchannel ~ after + treat + after*treat + month1, data = mydata.match, 
					index = c("household_code", "month"), model = "within")
cls.se2		<- c(lapply(dol.ls1[1:3], function(x) Cls.se.fn(x, cluster.vec = mydata.match[sel1,"household_code"])), 
				 lapply(dol.ls1[4:6], function(x) Cls.se.fn(x, cluster.vec = mydata.match[sel2,"household_code"])),
				 lapply(dol.ls1[7:9], function(x) Cls.se.fn(x, cluster.vec = mydata.match[sel3,"household_code"])), 
				 lapply(dol.ls1[10:12], function(x) Cls.se.fn(x, cluster.vec = mydata.match[,"household_code"])) )	

stargazer(dol.ls1, type = "html", align = TRUE, title = "Regressions of monthly expenditure on the matched sample using non-smokers and SF and MA as control", 
		keep = c("after", "treat"), se = cls.se2,
		covariate.labels = c("After", "After*Treat"),  
		add.lines = list(c("Household",rep("FE", 12)), c("Month", rep(rep(c("", "FE"), each = 3), 2)), 
						 c("Window", rep(rep(c("8 M.", "2 Yr"),each = 3), 2)), c("Distance", rep(c("< 2mi.", "Unrestricted"), each = 6)) ), 
		no.space = TRUE, omit.stat = c("rsq", "adj.rsq", "f"), 
		notes = "S.E. clustered over households", 
		out = paste(plot.wd, "/tb_", out.file, "_pmatch_dol", "_", Sys.Date(), ".html", sep=""))		

# -------------------------------------------#
# Look into expenditure at specific channels #
dol.ls2	<- list(NULL)
dol.ls2[[1]]	<- plm(dol_total ~ after + treat + after*treat , data = mydata.match[sel1,], 
					index = c("household_code", "month"), model = "within") 
dol.ls2[[2]]	<- plm(dol_grocery ~ after + treat + after*treat , data = mydata.match[sel1,], 
					index = c("household_code", "month"), model = "within")
dol.ls2[[3]]	<- plm(dol_discount ~ after + treat + after*treat , data = mydata.match[sel1,], 
					index = c("household_code", "month"), model = "within")
dol.ls2[[4]]	<- plm(dol_total ~ after + treat + after*treat + month1, data = mydata.match[sel2,], 
					index = c("household_code", "month"), model = "within") 
dol.ls2[[5]]	<- plm(dol_grocery ~ after + treat + after*treat + month1, data = mydata.match[sel2,], 
					index = c("household_code", "month"), model = "within")
dol.ls2[[6]]	<- plm(dol_discount ~ after + treat + after*treat + month1, data = mydata.match[sel2,], 
					index = c("household_code", "month"), model = "within")
dol.ls2[[7]]	<- plm(dol_total ~ after + treat + after*treat , data = mydata.match[sel3,], 
					index = c("household_code", "month"), model = "within") 
dol.ls2[[8]]	<- plm(dol_grocery ~ after + treat + after*treat , data = mydata.match[sel3,], 
					index = c("household_code", "month"), model = "within")
dol.ls2[[9]]	<- plm(dol_discount ~ after + treat + after*treat , data = mydata.match[sel3,], 
					index = c("household_code", "month"), model = "within")
dol.ls2[[10]]	<- plm(dol_total ~ after + treat + after*treat + month1, data = mydata.match, 
					index = c("household_code", "month"), model = "within") 
dol.ls2[[11]]	<- plm(dol_grocery ~ after + treat + after*treat + month1, data = mydata.match, 
					index = c("household_code", "month"), model = "within")
dol.ls2[[12]]	<- plm(dol_discount ~ after + treat + after*treat + month1, data = mydata.match, 
					index = c("household_code", "month"), model = "within")
cls.se3		<- c(lapply(dol.ls2[1:3], function(x) Cls.se.fn(x, cluster.vec = mydata.match[sel1,"household_code"])), 
				 lapply(dol.ls2[4:6], function(x) Cls.se.fn(x, cluster.vec = mydata.match[sel2,"household_code"])),
				 lapply(dol.ls2[7:9], function(x) Cls.se.fn(x, cluster.vec = mydata.match[sel3,"household_code"])), 
				 lapply(dol.ls2[10:12], function(x) Cls.se.fn(x, cluster.vec = mydata.match[,"household_code"])) )	

stargazer(dol.ls2, type = "html", align = TRUE, title = "Regressions of monthly expenditure on the matched sample using non-smokers and SF and MA as control", 
		keep = c("after", "treat"), se = cls.se3,
		covariate.labels = c("After", "After*Treatment"),  
		add.lines = list(c("Household",rep("FE", 12)), c("Month", rep(rep(c("", "FE"), each = 3), 2)), 
						 c("Window", rep(rep(c("8 M.", "2 Yr"),each = 3), 2)), c("Distance", rep(c("< 2mi.", "Unrestricted"), each = 6)) ), 
		no.space = TRUE, omit.stat = c("rsq", "adj.rsq", "f"), 
		notes = "S.E. clustered over households", 
		out = paste(plot.wd, "/tb_", out.file, "_pmatch_dolsep", "_", Sys.Date(), ".html", sep=""))

# ------------------------------------------ #
# Examine expenditure net cigarette spending #
dol.ls3	<- list(NULL)
dol.ls3[[1]]	<- plm(netdol_cvs ~ after + treat + after*treat , data = mydata.match[sel1,], 
					index = c("household_code", "month"), model = "within") 
dol.ls3[[2]]	<- plm(netdol_othdrug ~ after + treat + after*treat , data = mydata.match[sel1,], 
					index = c("household_code", "month"), model = "within")
dol.ls3[[3]]	<- plm(netdol_othchannel ~ after + treat + after*treat , data = mydata.match[sel1,], 
					index = c("household_code", "month"), model = "within")
dol.ls3[[4]]	<- plm(netdol_cvs ~ after + treat + after*treat + month1, data = mydata.match[sel2,], 
					index = c("household_code", "month"), model = "within") 
dol.ls3[[5]]	<- plm(netdol_othdrug ~ after + treat + after*treat + month1, data = mydata.match[sel2,], 
					index = c("household_code", "month"), model = "within")
dol.ls3[[6]]	<- plm(netdol_othchannel ~ after + treat + after*treat + month1, data = mydata.match[sel2,], 
					index = c("household_code", "month"), model = "within")
dol.ls3[[7]]	<- plm(netdol_cvs ~ after + treat + after*treat , data = mydata.match[sel3,], 
					index = c("household_code", "month"), model = "within") 
dol.ls3[[8]]	<- plm(netdol_othdrug ~ after + treat + after*treat , data = mydata.match[sel3,], 
					index = c("household_code", "month"), model = "within")
dol.ls3[[9]]	<- plm(netdol_othchannel ~ after + treat + after*treat , data = mydata.match[sel3,], 
					index = c("household_code", "month"), model = "within")
dol.ls3[[10]]	<- plm(netdol_cvs ~ after + treat + after*treat + month1, data = mydata.match, 
					index = c("household_code", "month"), model = "within") 
dol.ls3[[11]]	<- plm(netdol_othdrug ~ after + treat + after*treat + month1, data = mydata.match, 
					index = c("household_code", "month"), model = "within")
dol.ls3[[12]]	<- plm(netdol_othchannel ~ after + treat + after*treat + month1, data = mydata.match, 
					index = c("household_code", "month"), model = "within")
cls.se4		<- c(lapply(dol.ls3[1:3], function(x) Cls.se.fn(x, cluster.vec = mydata.match[sel1,"household_code"])), 
				 lapply(dol.ls3[4:6], function(x) Cls.se.fn(x, cluster.vec = mydata.match[sel2,"household_code"])),
				 lapply(dol.ls3[7:9], function(x) Cls.se.fn(x, cluster.vec = mydata.match[sel3,"household_code"])), 
				 lapply(dol.ls3[10:12], function(x) Cls.se.fn(x, cluster.vec = mydata.match[,"household_code"])) )	

stargazer(dol.ls3, type = "html", align = TRUE, title = "Regressions of monthly expenditure on the matched sample using non-smokers and SF and MA as control", 
		keep = c("after", "treat"), se = cls.se4,
		covariate.labels = c("After", "After*Treatment"),  
		add.lines = list(c("Household",rep("FE", 12)), c("Month", rep(rep(c("", "FE"), each = 3), 2)), 
						 c("Window", rep(rep(c("8 M.", "2 Yr"),each = 3), 2)), c("Distance", rep(c("< 2mi.", "Unrestricted"), each = 6)) ), 
		no.space = TRUE, omit.stat = c("rsq", "adj.rsq", "f"), 
		notes = "S.E. clustered over households", 
		out = paste(plot.wd, "/tb_", out.file, "_pmatch_netdol", "_", Sys.Date(), ".html", sep=""))		


save.image(file = paste(plot.wd, "/", out.file, "_", Sys.Date(),".rdata", sep=""))			

cat("This program is done.\n")
