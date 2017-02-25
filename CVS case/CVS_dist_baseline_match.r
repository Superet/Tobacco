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

# setwd("~/Documents/Research/Tobacco/processed_data")
# plot.wd		<- "~/Desktop"
setwd("U:/Users/ccv103/Documents/Research/tobacco/processed_data")
# setwd("/sscc/home/c/ccv103/Tobacco") 
plot.wd		<- getwd()
cutoff		<- 2
(out.file	<- paste("cvs_baseline_match_cut",cutoff,sep=""))

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
cat("Summary stats:\n"); print(summary(mydata[, -c(1:2)])); cat("\n")

# Calculate pre-event shopping behavior for each household
tmp2	<- tmp1[month < event.month,]
tmp2	<- tmp2[,list(	pre_trip_cvs 		= mean(trip_cvs), 
						pre_trip_othdrug 	= mean(trip_othdrug), 
						pre_trip_othchannel = mean(trip_othchannel), 
						pre_trip_total		= mean(trip_total),
						pre_dol_cvs 		= mean(dol_cvs), 
						pre_dol_othdrug		= mean(dol_othdrug), 
						pre_dol_othchannel	= mean(dol_othchannel), 
						pre_dol_total		= mean(dol_total)), by = list(household_code)]
mypan	<- merge(mypan, tmp2, by = "household_code", all.x = T)	
# bhv.col		<- c("pre_trip_cvs", "pre_trip_othdrug", "pre_trip_othchannel", "pre_dol_cvs", "pre_dol_othdrug", "pre_dol_othchannel")
bhv.col		<- c("pre_trip_total", "pre_trip_othchannel", "pre_dol_total", "pre_dol_othchannel")
sapply(bhv.col, function(i) sum(is.na(mypan[,i])))				
sel		<- apply(mypan[,bhv.col], 1, function(x) any(is.na(x)))
mypan	<- mypan[!sel,]

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
mypan$treat		<- with(mypan, 1 * (mypan$distance <= cutoff & ban_ard == 0))
table(mydata$treat)
table(mypan$treat)
trips		<- merge(trips, mypan[,c("household_code", "treat")], by = "household_code", all.x = T)
purchases	<- merge(purchases, mypan[,c("household_code", "treat")], by = "household_code", all.x = T)

#############################		
# Propensity score matching #		
#############################
my.ratio	<- 1
match.mod	<- matchit(treat ~ income + age + have_kids + employment + race + 
								pre_trip_total + pre_trip_othchannel+ pre_dol_total + pre_dol_othchannel, 
				method = "nearest", data = mypan[,c("household_code","treat",demo.col,bhv.col)], ratio = my.ratio, replace = T)
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

# Use t test
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
plots	<- list(NULL)
idx 	<- 1
ggtmp	<- data.table(subset(purchases, !week %in% endweek & household_code %in% match.pan$household_code))
ggtmp	<- ggtmp[, list(Total = sum(quantity*size/qunit, na.rm=T)/length(unique(household_code)), 
						CVS = sum(quantity*size*cvs/qunit, na.rm=T)/length(unique(household_code)), 
						OtherDrug = sum(quantity*size*(1-cvs)*1*(channel_type == "Drug Store")/qunit, na.rm=T)/length(unique(household_code)),
						OtherChannel = sum(quantity*size*1*(channel_type != "Drug Store")/qunit, na.rm=T)/length(unique(household_code))
						), by = list(treat, week)]
ggtmp	<- melt(ggtmp, id.vars = c("treat", "week"))		
ggtmp$treat	<- factor(ggtmp$treat, levels = c(1,0), labels = c("Treatment", "Control"))
plots[[idx]] 	<- ggplot(ggtmp, aes(week, value, linetype = treat)) + geom_line() + 
					geom_vline(xintercept = as.numeric(event.date), col = "red") + 
					facet_wrap(~variable, scales = "free") + 
					theme(legend.position = "bottom") + 
					labs(y = "Packs per person", title = "Cigarette consumption by consumer groups")
idx	<- idx + 1
			
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
ggtmp$treat	<- factor(ggtmp$treat, levels = c(1,0), labels = c("Treatment", "Control"))
plots[[idx]] 		<- ggplot(ggtmp, aes(week, value, linetype = treat)) + geom_line() + 
			geom_vline(xintercept = as.numeric(event.date), col = "red") + 
			facet_wrap(~variable, scales = "free") + 
			theme(legend.position = "bottom") + 
			labs(y = "Trips per person", title = "Shopping trips by consumer groups")
idx		<- idx + 1

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
plots[[idx]]		<- ggplot(ggtmp, aes(week, value, linetype = treat)) + geom_line() + 
			geom_vline(xintercept = as.numeric(event.date), col = "red") + 
			facet_wrap(~variable, scales = "free") + 
			theme(legend.position = "bottom") + 
			labs(y = "Expenditure per person", title = "Expenditure by consumer groups")

pdf(paste(plot.wd,"/fg_",out.file, "_pmatch_trend_", Sys.Date(), ".pdf", sep=""), width = 7.5, height = 7.5*ar)			
for(i in 1:length(plots)){
	print(plots[[i]])
}
dev.off()

##################################
# DID regression of small window # 
##################################			
# Restrict to a short window 
mydata.match	<- subset(mydata, household_code %in% match.pan$household_code)
dim(mydata.match)
sel1		<- mydata.match$month >= event.month - 4 & mydata.match$month <= event.month + 3

# -----------------------------#
# Use a narrow window #
fit.ls1	<- list(NULL)
fit.ls1[[1]]	<- plm(q ~ after + treat + after*treat , data = mydata.match[sel1,], 
					index = c("household_code", "month"), model = "within") 
fit.ls1[[2]]	<- plm(q_othdrug ~ after + treat + after*treat , data = mydata.match[sel1,], 
					index = c("household_code", "month"), model = "within")
fit.ls1[[3]]	<- plm(q_othchannel ~ after + treat + after*treat , data = mydata.match[sel1,], 
					index = c("household_code", "month"), model = "within")
fit.ls1[[4]]	<- glmer(trip_cvs ~ after + treat + after*treat + (1|household_code), data = mydata.match[sel1,], family = poisson(link = "log"))
fit.ls1[[5]]	<- glmer(trip_othdrug ~ after + treat + after*treat + (1|household_code), data = mydata.match[sel1,], family = poisson(link = "log"))
fit.ls1[[6]]	<- glmer(trip_othchannel ~ after + treat + after*treat + (1|household_code), data = mydata.match[sel1,], family = poisson(link = "log"))
fit.ls1[[7]]	<- plm(dol_cvs ~ after + treat + after*treat , data = mydata.match[sel1,], 
					index = c("household_code", "month"), model = "within") 
fit.ls1[[8]]	<- plm(dol_othdrug ~ after + treat + after*treat , data = mydata.match[sel1,], 
					index = c("household_code", "month"), model = "within")
fit.ls1[[9]]	<- plm(dol_othchannel ~ after + treat + after*treat , data = mydata.match[sel1,], 
					index = c("household_code", "month"), model = "within")
fit.ls1[[10]]	<- plm(dol_total ~ after + treat + after*treat , data = mydata.match[sel1,], 
					index = c("household_code", "month"), model = "within")					
fit.ls1[[11]]	<- plm(netdol_cvs ~ after + treat + after*treat , data = mydata.match[sel1,], 
					index = c("household_code", "month"), model = "within") 
fit.ls1[[12]]	<- plm(netdol_othdrug ~ after + treat + after*treat , data = mydata.match[sel1,], 
					index = c("household_code", "month"), model = "within")
fit.ls1[[13]]	<- plm(netdol_othchannel ~ after + treat + after*treat , data = mydata.match[sel1,], 
					index = c("household_code", "month"), model = "within")									 
cls.se1		<- lapply(fit.ls1, function(x) Cls.se.fn(x, cluster.vec = mydata.match[sel1,"household_code"]))

myline 		<- list(c("Model", rep("Linear", 3), rep("Poisson", 3), rep("Linear", length(fit.ls1) - 6)), 
					c("Household",rep("FE", 3), rep("RE", 3), rep("FE", length(fit.ls1) - 6) ))
stargazer(fit.ls1, type = "html", align = TRUE, title = "DID regressions for matched smokers using 8-month data", 
		keep = c("after", "treat"), se = cls.se1, model.names = FALSE, 
		covariate.labels = c("After", "Treat","After*Treat"),  
		add.lines = myline, 
		no.space = TRUE, omit.stat = c("rsq", "adj.rsq", "f"), 
		notes = "S.E. clustered over households", 
		out = paste(plot.wd, "/tb_", out.file, "_",Sys.Date(), ".html", sep=""))	

# -----------------------------------------------#
# Use a long window
fit.ls2	<- list(NULL)
fit.ls2[[1]]	<- plm(q ~ after + treat + after*treat + month1, data = mydata.match, 
					index = c("household_code", "month"), model = "within") 
fit.ls2[[2]]	<- plm(q_othdrug ~ after + treat + after*treat + month1, data = mydata.match, 
					index = c("household_code", "month"), model = "within")
fit.ls2[[3]]	<- plm(q_othchannel ~ after + treat + after*treat + month1 , data = mydata.match, 
					index = c("household_code", "month"), model = "within")
fit.ls2[[4]]	<- glmer(trip_cvs ~ after + treat + after*treat + month1+ (1|household_code), data = mydata.match, family = poisson(link = "log"))
fit.ls2[[5]]	<- glmer(trip_othdrug ~ after + treat + after*treat + month1 + (1|household_code), data = mydata.match, family = poisson(link = "log"))
fit.ls2[[6]]	<- glmer(trip_othchannel ~ after + treat + after*treat + month1 + (1|household_code), data = mydata.match, family = poisson(link = "log"))
fit.ls2[[7]]	<- plm(dol_cvs ~ after + treat + after*treat + month1 , data = mydata.match, 
					index = c("household_code", "month"), model = "within") 
fit.ls2[[8]]	<- plm(dol_othdrug ~ after + treat + after*treat + month1 , data = mydata.match, 
					index = c("household_code", "month"), model = "within")
fit.ls2[[9]]	<- plm(dol_othchannel ~ after + treat + after*treat + month1 , data = mydata.match, 
					index = c("household_code", "month"), model = "within")
fit.ls2[[10]]	<- plm(dol_total ~ after + treat + after*treat + month1 , data = mydata.match, 
					index = c("household_code", "month"), model = "within")
fit.ls2[[11]]	<- plm(netdol_cvs ~ after + treat + after*treat + month1 , data = mydata.match, 
					index = c("household_code", "month"), model = "within") 
fit.ls2[[12]]	<- plm(netdol_othdrug ~ after + treat + after*treat + month1 , data = mydata.match, 
					index = c("household_code", "month"), model = "within")
fit.ls2[[13]]	<- plm(netdol_othchannel ~ after + treat + after*treat + month1 , data = mydata.match, 
					index = c("household_code", "month"), model = "within")
cls.se2		<- lapply(fit.ls2, function(x) Cls.se.fn(x, cluster.vec = mydata.match$household_code))

myline2		<- c(myline, list(c("Month", rep("FE", length(fit.ls2)))))
stargazer(fit.ls2, type = "html", align = TRUE, title = "DID for matched smokers using 2-year data", 
		keep = c("after", "treat"), se = cls.se2, model.names = FALSE, 
		covariate.labels = c("After", "Treat","After*Treat"),  
		add.lines = myline2,
		no.space = TRUE, omit.stat = c("rsq", "adj.rsq", "f"), 
		notes = "S.E. clustered over households", 
		out = paste(plot.wd, "/tb_", out.file, "_month_",Sys.Date(), ".html", sep=""))

# Quantity at other specific channels
fit.ls3		<- list(NULL)		
fit.ls3[[1]]	<- plm(q_convenience ~ after + treat + after*treat , data = mydata.match[sel1,], 
					index = c("household_code", "month"), model = "within")
fit.ls3[[2]]	<- plm(q_grocery ~ after + treat + after*treat , data = mydata.match[sel1,], 
					index = c("household_code", "month"), model = "within")
fit.ls3[[3]]	<- plm(q_service ~ after + treat + after*treat , data = mydata.match[sel1,], 
					index = c("household_code", "month"), model = "within")
fit.ls3[[4]]	<- plm(q_tobacco ~ after + treat + after*treat , data = mydata.match[sel1,], 
					index = c("household_code", "month"), model = "within")
fit.ls3[[5]]	<- plm(q_gas ~ after + treat + after*treat , data = mydata.match[sel1,], 
					index = c("household_code", "month"), model = "within")
fit.ls3[[6]]	<- plm(q_discount ~ after + treat + after*treat , data = mydata.match[sel1,], 
					index = c("household_code", "month"), model = "within")
fit.ls3[[7]]	<- plm(q_convenience ~ after + treat + after*treat + month1, data = mydata.match, 
					index = c("household_code", "month"), model = "within")
fit.ls3[[8]]	<- plm(q_grocery ~ after + treat + after*treat + month1, data = mydata.match, 
					index = c("household_code", "month"), model = "within")
fit.ls3[[9]]	<- plm(q_service ~ after + treat + after*treat + month1, data = mydata.match, 
					index = c("household_code", "month"), model = "within")
fit.ls3[[10]]	<- plm(q_tobacco ~ after + treat + after*treat + month1, data = mydata.match, 
					index = c("household_code", "month"), model = "within")
fit.ls3[[11]]	<- plm(q_gas ~ after + treat + after*treat + month1, data = mydata.match, 
					index = c("household_code", "month"), model = "within")
fit.ls3[[12]]	<- plm(q_discount ~ after + treat + after*treat + month1, data = mydata.match, 
					index = c("household_code", "month"), model = "within")					
cls.se3		<- c(lapply(fit.ls3[1:6], function(x) Cls.se.fn(x, cluster.vec = mydata.match[sel1,"household_code"])), 
				 lapply(fit.ls3[7:12], function(x) Cls.se.fn(x, cluster.vec = mydata.match$household_code) ) )

stargazer(fit.ls3, type = "html", align = TRUE, title = "Regressions of cigarette quantity at different channels for matched smokers", 
		keep = c("after", "treat"), se = cls.se3,
		covariate.labels = c("After", "After*Treat"),  
		add.lines = list(c("Household",rep("FE", length(fit.ls3))), c("Month", rep("", 6), rep("FE", 6)), 
						c("Window", rep("8 m.", 6), rep("2 yr.", 6))), 
		no.space = TRUE, omit.stat = c("rsq", "adj.rsq", "f"), 
		notes = "S.E. clustered over households", 
		out = paste(plot.wd, "/tb_", out.file, "_channel_",Sys.Date(), ".html", sep=""))	

save.image(file = paste(plot.wd, "/", out.file, "_", Sys.Date(),".rdata", sep=""))			

cat("This program is done.\n")
