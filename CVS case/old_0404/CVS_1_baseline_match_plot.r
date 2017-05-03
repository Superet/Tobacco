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
library(gridExtra)

# setwd("~/Documents/Research/Tobacco/processed_data")
# plot.wd		<- "~/Desktop"
setwd("U:/Users/ccv103/Documents/Research/tobacco/processed_data")
# setwd("/sscc/home/c/ccv103/Tobacco") 
plot.wd		<- getwd()
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
table(mypan$frac_seg, mypan$ban_ard)
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

##############
# Plot trend #
##############
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

plots	<- list(NULL)
idx		<- 1
ggtmp0	<- data.table(mydata)
ggtmp0	<- ggtmp0[,nhh:=length(unique(household_code)), by = list(treat)]		
ggtmp0	<- ggtmp0[,list(q = sum(q/nhh), q_cvs= sum(q_cvs/nhh), q_othdrug = sum(q_othdrug/nhh), q_othchannel = sum(q_othchannel/nhh),
						trip_cvs = sum(trip_cvs/nhh), trip_othdrug = sum(trip_othdrug/nhh), trip_othchannel = sum(trip_othchannel/nhh),
						netdol_cvs = sum(netdol_cvs/nhh), netdol_othdrug = sum(netdol/nhh), netdol_othchannel = sum(netdol_othchannel/nhh)
						), 
					by = list(treat, month)]
ggtmp0$month	<- as.Date(paste(ifelse(ggtmp0$month <= 12, 2013, 2014), ifelse(ggtmp0$month %% 12 == 0, 12, ggtmp0$month%%12), "01", sep="-"), format = "%Y-%m-%d")					
ggtmp0$treat 	<- factor(ggtmp0$treat, levels = c(1, 0), labels = c("Treated", "Control"))	
(tmplab			<- setNames(c("Total", "Cigarettes at CVS","Cigarettes at other drug stores", "Cigarettes at other channel", 
							  "Trip to CVS", "Trip to other drug stores", "Trip to other channel", 
							  "Exp. other basket items \n at CVS", "Exp. other basket items \n at other drug stores", "Exp. other basket items \n at other channel" 
							), names(ggtmp0)[-c(1:2)]))
blankPlot <- ggplot()+geom_blank(aes(1,1))+
				theme(	plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
   						panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), 
   						axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank() )

# Cigarett quantity
ggtmp	<- melt(ggtmp0[,c("treat","month",grep("q", names(ggtmp0), value = T)), with = FALSE], id.vars = c("treat", "month"))
ggtmp$variable	<- factor(ggtmp$variable, levels = grep("q", names(ggtmp0), value = T), 
						labels = tmplab[grep("q", names(ggtmp0), value = T)])

plots[[idx]] <- ggplot(subset(ggtmp, variable == tmplab["q_cvs"]), aes(month, value, linetype = treat)) + geom_line() + 
						geom_vline(xintercept = as.numeric(event.date), col = "red") + 
						facet_wrap(~variable, scales = "free") + 
						guides(linetype = guide_legend(title = "")) + 
						# theme(legend.position = "bottom") + 
						labs(y = "Volume (packs)")
legend <- get_legend(plots[[idx]])												
idx			<- idx + 1	

plots[[idx]]<- ggplot(subset(ggtmp, variable == tmplab["q"]), aes(month, value, linetype = treat)) + geom_line() + 
						geom_vline(xintercept = as.numeric(event.date), col = "red") + 
						facet_wrap(~variable, scales = "free") + 
						guides(linetype = guide_legend(title = "")) + 
						theme(legend.position = "none") + 
						labs(y = "Volume (packs)")
idx			<- idx + 1						
plots[[idx]]<- ggplot(subset(ggtmp, variable %in% tmplab[c("q_othdrug", "q_othchannel")]), aes(month, value, linetype = treat)) + geom_line() + 
						geom_vline(xintercept = as.numeric(event.date), col = "red") + 
						facet_wrap(~variable, scales = "free") + 
						guides(linetype = guide_legend(title = "")) + 
						theme(legend.position = "none") + 
						labs(y = "Volume (packs)") 
idx <- idx + 1	

# Shopping trips
ggtmp	<- melt(ggtmp0[,c("treat","month",grep("trip", names(ggtmp0), value = T)), with = FALSE], id.vars = c("treat", "month"))
ggtmp$variable	<- factor(ggtmp$variable, levels = grep("trip", names(ggtmp0), value = T), 
						labels = tmplab[grep("trip", names(ggtmp0), value = T)])

plots[[idx]] <- ggplot(subset(ggtmp, variable == tmplab["trip_cvs"]), aes(month, value, linetype = treat)) + geom_line() + 
						geom_vline(xintercept = as.numeric(event.date), col = "red") + 
						facet_wrap(~variable, scales = "free") + 
						guides(linetype = guide_legend(title = "")) + 
						theme(legend.position = "none") + 	
						labs(y = "No. trips")
idx	<- idx + 1						
plots[[idx]]	<- ggplot(subset(ggtmp, variable %in% tmplab[c("trip_othdrug", "trip_othchannel")]), aes(month, value, linetype = treat)) + geom_line() + 
						geom_vline(xintercept = as.numeric(event.date), col = "red") + 
						facet_wrap(~variable, scales = "free") + 
						guides(linetype = guide_legend(title = "")) + 
						theme(legend.position = "none") + 
						labs(y = "No. trips") 
idx	<- idx + 1

# Expenditure 
ggtmp	<- melt(ggtmp0[,c("treat","month",grep("netdol", names(ggtmp0), value = T)), with = FALSE], id.vars = c("treat", "month"))
ggtmp$variable	<- factor(ggtmp$variable, levels = grep("netdol", names(ggtmp0), value = T), 
						labels = tmplab[grep("netdol", names(ggtmp0), value = T)])

plots[[idx]] <- ggplot(subset(ggtmp, variable == tmplab["netdol_cvs"]), aes(month, value, linetype = treat)) + geom_line() + 
						geom_vline(xintercept = as.numeric(event.date), col = "red") + 
						facet_wrap(~variable, scales = "free") + 
						guides(linetype = guide_legend(title = "")) + 
						theme(legend.position = "none") +
						labs(y = "Expenditure on \nother basket items")
idx		<- idx + 1						
plots[[idx]]	<- ggplot(subset(ggtmp, variable %in% tmplab[c("netdol_othdrug", "netdol_othchannel")]), aes(month, value, linetype = treat)) + geom_line() + 
						geom_vline(xintercept = as.numeric(event.date), col = "red") + 
						facet_wrap(~variable, scales = "free") + 
						guides(linetype = guide_legend(title = "")) + 
						theme(legend.position = "none") + 
						labs(y = "Expenditure on \nother basket items") 
idx	<- idx + 1

pdf(paste(plot.wd, "/fg_", out.file,"_trend.pdf", sep=""), width = 5.5, height = 4)
print(plots[[1]])
print(grid.arrange(blankPlot, plots[[2]], legend, plots[[3]], nrow = 2, layout_matrix = rbind(c(1,2,2,3), rep(4, 4)) ) )
print(grid.arrange(blankPlot, plots[[4]], legend, plots[[5]], nrow = 2, layout_matrix = rbind(c(1,2,2,3), rep(4, 4)) ) )
print(grid.arrange(blankPlot, plots[[6]], legend, plots[[7]], nrow = 2, layout_matrix = rbind(c(1,2,2,3), rep(4, 4)) ) )
dev.off()

#########################
# Household differences # 
#########################
sel.col	<- c("income", "age", "distance_cvs", 
               "pre_q", "pre_trip_total", "pre_trip_othchannel", "pre_dol_total", "pre_dol_othchannel")
(col.lab	<- setNames(c("Income", "Age", "Distance to CVS", "Cigarette consumption", "No. total trips", "No. trips to other channels", 
					   "Total expenditure", "Expenditure at other channels"), 
					sel.col))

# T-test 
tmp.tab	<- NULL
for(i in 1:length(sel.col)){
	tmp	<- t.test(as.formula(paste(sel.col[i], "~ treat")), data = mypan)
	tmp.tab	<- rbind(tmp.tab, unlist(tmp[c("estimate", "p.value")]))
}
dimnames(tmp.tab)	<- list(col.lab, c("Control","Treated", "p value"))
cat("T test of difference between treated and control:\n"); print(round(tmp.tab,2)); cat("\n")
stargazer(tmp.tab, type = "html", summary = F, out = paste(plot.wd,"/tb_", out.file, "_ttest.html", sep=""))

# QQ plot
sel	<- mypan$treat == 1
par(mfrow = c(2,4))
for(i in 1:length(sel.col)){
	rg	<- range(mypan[,sel.col[i]])
	qqplot(mypan[sel,sel.col[i]], mypan[!sel,sel.col[i]], xlim = rg, ylim = rg,xlab = "Treated", ylab = "Control", main = col.lab[i]) 
}

