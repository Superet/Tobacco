library(reshape2)
library(ggplot2)
library(data.table)
library(lubridate)
library(scales)
library(stargazer)
library(xlsx)
library(gridExtra)

# setwd("U:/Users/ccv103/Desktop")
setwd("~/Documents/Research/Tobacco/processed_data")
plot.wd		<- "~/Desktop"
ww			<- 6.5
ar			<- .6

panelists 	<- read.csv("tob_CVS_pan.csv", header = T)
purchases	<- read.csv("tob_CVS_purchases.csv", header = T)
trips		<- read.csv("tob_CVS_trips.csv", header = T)
ma.policy	<- read.xlsx("MA_policy.xlsx", 1)
sel.channel		<- c("Grocery", "Discount Store", "Drug Store", "Warehouse Club", "Dollar Store", "Convenience Store", 
					"Service Station", "All Other Stores", "Gas Mini Mart", "Tobacco Store", "Health Food Store")
trips			<- subset(trips, channel_type %in% sel.channel)			
purchases		<- subset(purchases, trip_code_uc %in% trips$trip_code_uc)		
names(panelists)	<- tolower(names(panelists))

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

trips$purchase_date	<- as.Date(as.character(trips$purchase_date), format = "%Y-%m-%d")
trips$week			<- ((as.numeric(trips$purchase_date - firstw)) %/%7 + 1)* 7 + firstw - 1
trips$year			<- year(trips$purchase_date)
trips$month			<- month(trips$purchase_date)
trips$month			<- ifelse(trips$year == 2012, 1, ifelse(trips$year == 2013, trips$month, trips$month + 12))
endweek				<- c(min(purchases$week), max(purchases$week))

# Mark CVS
trips$cvs	<- ifelse(trips$retailer_code == cvs.ret, 1, 0)
purchases	<- merge(purchases, trips[,c("trip_code_uc", "cvs", "channel_type", "retailer_code")], by = "trip_code_uc", all.x=T)

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
mypan$treat		<- with(mypan, 1*(cvs_in2 ==1 & ban_ard == 0))

# Classify light vs heavy smokers
tmp1		<- data.table(purchases)
setkeyv(tmp1, c("household_code", "purchase_date"))
tmp1		<- tmp1[, list(consum = sum(quantity*size/qunit, na.rm=T)/(as.numeric(max(purchase_date)-min(purchase_date)))*7), 
					by = list(household_code)]
summary(tmp1$consum)
tmp1$heavy 	<- 1*(tmp1$consum > 2.5)
tmp			<- setNames(tmp1$heavy, tmp1$household_code)
mypan$heavy 	<- tmp[as.character(mypan$household_code)]

# Fraction of cigaratte purchases at CVS
tmp1		<- data.table(subset(purchases, purchase_date < event.date))
tmp1		<- tmp1[,list(cvs = sum(quantity*size*cvs), total = sum(quantity*size)), 
							by = list(household_code)]
tmp1		<- tmp1[,cvs_share := cvs/total]
hist(tmp1$cvs_share, breaks = 100)

# Distribution of the fraction of cigarette spending conditional on CVS visit
tmp2	<- data.table(subset(purchases, cvs==1 & purchase_date < event.date))
tmp2	<- tmp2[,list(total_price_paid = sum(total_price_paid - coupon_value)), by = list(trip_code_uc)]
tmp2	<- merge(trips[trips$cvs==1 & trips$purchase_date < event.date, ], tmp2, by = "trip_code_uc", all.x = T)
tmp2[is.na(tmp2$total_price_paid), "total_price_paid"]	<- 0 
tmp2	<- data.table(tmp2)
tmp2	<- tmp2[, cig_frac := total_price_paid/total_spent]
p1		<- ggplot(tmp2, aes(cig_frac)) + geom_histogram() + 
				labs(title = "Fraction of cigarette spending conditional on CVS visit across all CVS trips") 
p2 		<- ggplot(subset(tmp2, cig_frac>0), aes(cig_frac)) + geom_histogram() + 
				labs(title = "Fraction of cigarette spending conditional on CVS visit and cigarette purchases") 
grid.arrange(p1, p2)

# Fraction of cigarette spending for each household 
tmp 	<- tmp2[,list(household_code, total_price_paid, total_spent, purchase_date)]
tmp		<- tmp[,list(cig_frac = sum(total_price_paid)/sum(total_spent), 
					 cig_frac_cond = sum(total_price_paid[total_price_paid>0])/sum(total_spent[total_price_paid>0]) ),
					by = list(household_code)]
tmp[is.na(tmp)]	<- 0
summary(tmp)
ggtmp	<- melt(tmp, id.vars = "household_code")
ggtmp$variable <- factor(ggtmp$variable, levels = c("cig_frac", "cig_frac_cond"), 
					labels = c("Fraction of cigarette spending\n out of total CVS spending", 
					"Fraction of cigarette spending\n conditional on cigarette purchases at CVS"))
ggplot(ggtmp, aes(value)) + geom_histogram() + 
		facet_wrap(~variable) 
median(tmp[cig_frac>0,cig_frac])		
tmp$frac_seg	<- ifelse(tmp$cig_frac ==0, "Zero", ifelse(tmp$cig_frac <= median(tmp[cig_frac>0,cig_frac]), "S1", "S2"))
table(tmp$frac_seg)
mypan	<- merge(mypan, tmp[,list(household_code, frac_seg)], by = "household_code", all.x=T)

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
trips		<- merge(trips, mypan[,c("household_code", "ban_ard", "distance","cvs_in2", "cvs_in5", "wgr_in2", "heavy", "treat","frac_seg")], 
						by = "household_code", all.x=T)
purchases	<- merge(purchases, mypan[,c("household_code", "ban_ard", "distance","cvs_in2", "cvs_in5", "wgr_in2", "heavy", "treat","frac_seg")], 
						by = "household_code", all.x=T)						

# Calculate pre-event shopping behavior for each household
tmp1	<- data.table(purchases)
tmp1	<- tmp1[, list(q = sum(quantity*size/qunit, na.rm=T)), by = list(household_code, month)]
tmp2 	<- data.table(trips)
tmp2	<- tmp2[,list(	trip_cvs 		= 1*(sum(cvs)>0), 
						trip_othdrug 	= sum(channel_type == "Drug Store" & cvs ==0 ), 
						trip_othchannel = sum(channel_type != "Drug Store"), 
						trip_grocery	= sum(channel_type == "Grocery"), 
						trip_discount	= sum(channel_type == "Discount Store"), 
						trip_convenience= sum(channel_type == "Convenience Store"), 
						trip_service	= sum(channel_type == "Service Station"), 
						trip_gas		= sum(channel_type == "Gas Mini Mart"), 
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
tmp2	<- merge(tmp2, tmp1, by = c("household_code", "month"), all.x = T)				
tmp2[is.na(tmp2)]	<- 0
tmp2	<- tmp2[month < event.month,]
tmp2	<- tmp2[,list(	pre_q 				= mean(q),
						pre_trip_cvs 		= mean(trip_cvs), 
						pre_trip_othdrug 	= mean(trip_othdrug), 
						pre_trip_othchannel = mean(trip_othchannel), 
						pre_dol_cvs 		= mean(dol_cvs), 
						pre_dol_othdrug		= mean(dol_othdrug), 
						pre_dol_othchannel	= mean(dol_othchannel)), by = list(household_code)]
mypan	<- merge(mypan, tmp2, by = "household_code", all.x = T)	
bhv.col		<- c("pre_q", "pre_trip_cvs", "pre_trip_othdrug", "pre_trip_othchannel", "pre_dol_cvs", "pre_dol_othdrug", "pre_dol_othchannel")
sapply(bhv.col, function(i) sum(is.na(mypan[,i])))				
sel		<- apply(mypan[,bhv.col], 1, function(x) any(is.na(x)))
mypan	<- mypan[!sel,]

# Check the tenure of households 
tmp		<- data.table(trips)
tmp		<- tmp[,list(n = length(unique(week)), cvs_in2 = mean(cvs_in2, na.rm=T)), by = list(household_code)]
tmp		<- tmp[cvs_in2 %in% c(0,1),]
ggplot(tmp, aes(n, fill = factor(cvs_in2), alpha = .5)) + geom_histogram(aes(y = ..density..), position = "identity")

#################
# Summary stats # 
#################
cat("Number of households in the data:", length(unique(panelists$household_code)))

# For each household, we look at their weekly cigarrette consumption level 
tmp1	<- data.table(purchases)
setkeyv(tmp1, c("household_code", "purchase_date"))
tmp2	<- tmp1[, month := month(purchase_date)]
tmp2	<- tmp1[, list(nstore = length(unique(retailer_code))), by = list(household_code, year, month)]
tmp2	<- tmp2[, list(nstore = as.numeric(mean(nstore))), by = list(household_code)]
# tmp1	<- tmp1[, list(consum = sum(quantity*size/qunit, na.rm=T)/length(unique(week))), by = list(household_code)]
tmp1	<- tmp1[, list(consum = sum(quantity*size/qunit, na.rm=T)/(as.numeric(max(purchase_date)-min(purchase_date)))*7, 
						interpurchase = mean(as.numeric(diff(unique(purchase_date)))) ), 
					by = list(household_code)]
tmp1	<- merge(tmp1, tmp2, by = "household_code", all.x = T)					

# We also look at their weekly trips
tmp2	<- data.table(trips)
tmp2	<- tmp2[, list(	ntrip = length(purchase_date)/(as.numeric(max(purchase_date)-min(purchase_date)))*7, 
						ncvs = length(purchase_date[cvs==1])/(as.numeric(max(purchase_date)-min(purchase_date)))*7, 
						nother_drug = length(purchase_date[cvs==0 & channel_type == "Drug Store"])/(as.numeric(max(purchase_date)-min(purchase_date)))*7 ), 
				by = list(household_code)]
tmp		<- merge(tmp1, tmp2, by = "household_code")

# Combine with the average distance to CVS over year; 
tmp1	<- data.table(panelists)
tmp1	<- tmp1[,list(distance = mean(distance)), by = list(household_code)]
tmp		<- merge(tmp, tmp1, by = "household_code")
cat("Summary statistics across households:\n"); print(summary(tmp)); cat("\n")
stargazer(tmp[,list(consum, interpurchase, ntrip, ncvs, nother_drug,distance)], type = "html", 
			title = "Summary statistics of household cigarette consumption, weekly trips, and distance to CVS", 
			covariate.labels = c("Consumption (packs)", "Inter-purchase days", "No. stores of cigarett trips/month", 
								"No. trips", "Trips to CVS", "Trips to other drug stores", "Distance to CVS (mi.)"), 
			summary.stat = c("n", "min", "median", "mean", "p25", "p75", "max", "sd"), 
			out = paste(plot.wd, "/cvs_sumstat.html", sep=""))			 						

# --------------------------------------------------# 
# Cigarrette market share by retailers / by channel #
tmp		<- data.table(purchases)
tmp		<- tmp[,list(q = sum(quantity*size/qunit, na.rm=T)), by = list(channel_type)]
tmp		<- tmp[order(tmp$q, decreasing = T),]
tmp1	<- as.character(tmp[1:10,channel_type])
tmp$channel	<- ifelse(tmp$channel_type %in% tmp1, as.character(tmp$channel_type), "All other")
tmp		<- tmp[,list(q = sum(q)), by = list(channel)]
tmp		<- tmp[,mkt.share := q/sum(q)]
tmp$channel.lab	<- factor(tmp$channel, levels = tmp$channel, 
	labels = paste(as.character(tmp$channel), "(", round(tmp$mkt.share*100),"%)", sep=""))

pdf(paste(plot.wd, "/fg_pie_mktshare.pdf", sep=""), width = 6, height = 5)
ggplot(tmp, aes(x = 1, mkt.share, fill = channel.lab)) + geom_bar(stat = "identity", width = 1)+
		coord_polar(theta = "y") + 
		guides(fill = guide_legend(title = "")) + 
		labs(y = "Market share of channels in 2013 - 2014")
dev.off()		

# Cigarrett sales within drug stores
sel		<- purchases$channel_type == "Drug Store" & year(purchases$purchase_date) == 2013
tmp		<- data.table(purchases[sel,])
tmp		<- tmp[,list(q = sum(quantity*size/qunit, na.rm=T)), by = list(cvs)]
tmp		<- tmp[,mkt.share := q/sum(q)]
cat("Market share within drug stores in 2013:\n"); print(tmp); cat("\n")

###############################################
# Summary stats between treatment and control #
###############################################
# Demographic differences by treatment 
sel		<- sapply(demo.col, function(i) is.numeric(mypan[,i]))
ggtab	<- NULL
for(i in c(demo.col[sel], bhv.col)){
	tmp		<- t.test(mypan[,i] ~ mypan$treat)
	tmp1	<- c(tmp$estimate, dif = diff(tmp$estimate), tmp$statistic, p = tmp$p.value)
	ggtab	<- rbind(ggtab, tmp1)
}
ggtab			<- rbind(c(table(mypan$treat), rep(NA, 3)), ggtab)
rownames(ggtab)	<- c("N", demo.col[sel], bhv.col)
colnames(ggtab)	<- c("Treatment", "Control", "Difference", "t stat", "P value")

ggtab1		<- NULL
for(i in demo.col[!sel]){
	tmp		<- table(mypan[,i], mypan$treat)
	tmp1	<- chisq.test(tmp)
	tmp 	<- apply(tmp, 2, function(x) x/sum(x))
	tmp1	<- rbind(cbind(tmp, tmp[,2]-tmp[,1], NA, NA), c(NA, NA, NA,tmp1$statistic, p = tmp1$p.value))
	ggtab1	<- rbind(ggtab1, tmp1)
}
colnames(ggtab1)	<- c("Treatment", "Control", "Difference", "t stat", "P value")
cat("Tests of demongraphic differences:\n"); print(rbind(ggtab, ggtab1)); cat("\n")

########################################
# Overall trend of cigarette purchases #
########################################
# Trend of total cigarette sales in CVS, non-CVS drug stores, and all other retailers. 
ggtmp	<- data.table(subset(purchases, !week %in% endweek))
ggtmp	<- ggtmp[,list(Total = sum(quantity*size/qunit, na.rm=T), 
						CVS = sum(quantity*size*cvs/qunit, na.rm=T), 
	 					OtherDrug = sum(quantity*size*(1-cvs)*1*(channel_type=="Drug Store")/qunit, na.rm=T), 
						OtherChannel = sum(quantity*size*(1-cvs)*1*(channel_type!="Drug Store")/qunit, na.rm=T)), 
				by = list(week)]
ggtmp1	<- melt(ggtmp, id.vars = "week")	

pdf(paste(plot.wd, "/fg_overall_trend.pdf", sep=""), width = ww, height = ww*ar)
print(ggplot(ggtmp1, aes(x = week, value)) + geom_line() + facet_wrap(~variable, scales = "free") + 
			labs(y = "Volume (packs)", title = "Trend of cigarette weekly sales")	)

# Trend of market share 
ggtmp	<- ggtmp[,':='(CVS = CVS/Total, OtherDrug = OtherDrug/Total, OtherChannel = OtherChannel/Total )]		
ggtmp1	<- melt(ggtmp, id.vars = "week")
print(ggplot(subset(ggtmp1, variable != "Total"), aes(x = week, value)) + geom_line() + facet_wrap(~variable, scales = "free") + 
			scale_y_continuous(labels = percent) + 
			labs(y = "Market share", title = "Trend of market share")	)
dev.off()			

# ---------------------------------------------------------------------- #
# Compare per capita consumption trend by consumer group -- Close to CVS#			
ggtmp	<- data.table(purchases)			
ggtmp	<- ggtmp[!week %in% endweek, list(Total = sum(quantity*size/qunit, na.rm=T)/length(unique(household_code)), 
					CVS = sum(quantity*size*cvs/qunit, na.rm=T)/length(unique(household_code)), 
 					OtherDrug = sum(quantity*size*(1-cvs)*1*(channel_type=="Drug Store")/qunit, na.rm=T)/length(unique(household_code)), 
					OtherChannel = sum(quantity*size*(1-cvs)*1*(channel_type!="Drug Store")/qunit, na.rm=T)/length(unique(household_code))), 
				by = list(cvs_in2,week)]
ggtmp	<- melt(ggtmp, id.vars = c("cvs_in2", "week"))
ggtmp$cvs_in2	<- factor(ggtmp$cvs_in2, levels = c(1, 0), labels = c("Less than 2 mi.", "Greater than 2 mi."))
pdf(paste(plot.wd, "/fg_percap_trend_dist.pdf", sep=""), width = ww, height = ww*ar)
ggplot(ggtmp, aes(week, value, linetype = cvs_in2)) + geom_line() + 
			geom_vline(xintercept = as.numeric(event.date), col = "red") + 
			facet_wrap(~variable, scales = "free") + 
			guides(linetype = guide_legend(title = "Distance to CVS")) + 
			theme(legend.position = "bottom") + 
			labs(y = "Volume (packs)", title = "Per capita consumption by consumer groups") 
dev.off()		

# cigarette spending 
ggtmp	<- data.table(purchases)			
ggtmp	<- ggtmp[!week %in% endweek, list(Total = sum(total_price_paid, na.rm=T)/length(unique(household_code)), 
					CVS = sum(total_price_paid*cvs, na.rm=T)/length(unique(household_code)), 
 					OtherDrug = sum(total_price_paid*(1-cvs)*1*(channel_type=="Drug Store"), na.rm=T)/length(unique(household_code)), 
					OtherChannel = sum(total_price_paid*(1-cvs)*1*(channel_type!="Drug Store"), na.rm=T)/length(unique(household_code))), 
				by = list(cvs_in2,week)]
ggtmp	<- melt(ggtmp, id.vars = c("cvs_in2", "week"))
ggtmp$cvs_in2	<- factor(ggtmp$cvs_in2, levels = c(1, 0), labels = c("Less than 2 mi.", "Greater than 2 mi."))

ggplot(ggtmp, aes(week, value, linetype = cvs_in2)) + geom_line() + 
			geom_vline(xintercept = as.numeric(event.date), col = "red") + 
			facet_wrap(~variable, scales = "free") + 
			guides(linetype = guide_legend(title = "Distance to CVS")) + 
			theme(legend.position = "bottom") + 
			labs(y = "Spending ($)", title = "Per capita consumption by consumer groups") 

# ---------------------------------------------------------------------------- #
# Compare per capita consumption trend by consumer group -- Close to Walgreens #			
ggtmp	<- data.table(purchases)			
ggtmp	<- ggtmp[!week %in% endweek, list(Total = sum(quantity*size/qunit, na.rm=T)/length(unique(household_code)), 
					CVS = sum(quantity*size*cvs/qunit, na.rm=T)/length(unique(household_code)), 
 					OtherDrug = sum(quantity*size*(1-cvs)*1*(channel_type=="Drug Store")/qunit, na.rm=T)/length(unique(household_code)), 
					OtherChannel = sum(quantity*size*(1-cvs)*1*(channel_type!="Drug Store")/qunit, na.rm=T)/length(unique(household_code))), 
				by = list(cvs_in2, wgr_in2,week)]
ggtmp	<- melt(ggtmp, id.vars = c("cvs_in2", "wgr_in2", "week"))
ggtmp$wgr_in2	<- factor(ggtmp$wgr_in2, levels = c(1, 0), labels = c("Less than 2 mi.", "Greater than 2 mi."))
ggtmp$cvs_in2	<- factor(ggtmp$cvs_in2, levels = c(1, 0), labels = c("Less than 2 mi.", "Greater than 2 mi."))

pdf(paste(plot.wd, "/fg_percap_trend_distwgr.pdf", sep=""), width = ww, height = ww*ar)
ggplot(subset(ggtmp, !is.na(cvs_in2) & !is.na(wgr_in2)), 
			aes(week, value, linetype = cvs_in2, color = wgr_in2)) + geom_line() + 
			geom_vline(xintercept = as.numeric(event.date), col = "red") + 
			facet_wrap(~variable, scales = "free") + 
			guides(linetype = guide_legend(title = "Distance to CVS"), color = guide_legend(title = "Distance to Walgreens")) + 
			theme(legend.position = "bottom") + 
			labs(y = "Volume (packs)", title = "Per capita consumption by consumer groups") 
dev.off()		

# ---------------------------------------------------------------------------------- #
# Compare per capita consumption trend by consumer group -- light vs. heavy smokers #			
ggtmp	<- data.table(purchases)			
ggtmp	<- ggtmp[!week %in% endweek, list(Total = sum(quantity*size/qunit, na.rm=T)/length(unique(household_code)), 
					CVS = sum(quantity*size*cvs/qunit, na.rm=T)/length(unique(household_code)), 
 					OtherDrug = sum(quantity*size*(1-cvs)*1*(channel_type=="Drug Store")/qunit, na.rm=T)/length(unique(household_code)), 
					OtherChannel = sum(quantity*size*(1-cvs)*1*(channel_type!="Drug Store")/qunit, na.rm=T)/length(unique(household_code))), 
				by = list(cvs_in2, heavy,week)]
ggtmp	<- melt(ggtmp, id.vars = c("cvs_in2", "heavy", "week"))
ggtmp$heavy	<- factor(ggtmp$heavy, levels = c(1, 0), labels = c("Heavy", "Light"))
ggtmp$cvs_in2	<- factor(ggtmp$cvs_in2, levels = c(1, 0), labels = c("Less than 2 mi.", "Greater than 2 mi."))

pdf(paste(plot.wd, "/fg_percap_trend_heavy.pdf", sep=""), width = ww, height = ww*ar)
ggplot(subset(ggtmp, !is.na(cvs_in2) & !is.na(heavy)), aes(week, value, linetype = cvs_in2, color = heavy)) + geom_line() + 
			geom_vline(xintercept = as.numeric(event.date), col = "red") + 
			facet_wrap(~variable, scales = "free") + 
			guides(linetype = guide_legend(title = "Distance to CVS"), color = guide_legend(title = "Smokers")) + 
			theme(legend.position = "bottom") + 
			labs(y = "Volume (packs)", title = "Per capita consumption by consumer groups") 
dev.off()		


# --------------------------------------------------------------------------------------- #
# Compare per capita consumption trend by consumer group -- segment of cigarette spending #			
ggtmp	<- data.table(purchases)			
ggtmp	<- ggtmp[!week %in% endweek, list(Total = sum(quantity*size/qunit, na.rm=T)/length(unique(household_code)), 
					CVS = sum(quantity*size*cvs/qunit, na.rm=T)/length(unique(household_code)), 
 					OtherDrug = sum(quantity*size*(1-cvs)*1*(channel_type=="Drug Store")/qunit, na.rm=T)/length(unique(household_code)), 
					OtherChannel = sum(quantity*size*(1-cvs)*1*(channel_type!="Drug Store")/qunit, na.rm=T)/length(unique(household_code))), 
				by = list(cvs_in2, frac_seg,week)]
ggtmp	<- melt(ggtmp, id.vars = c("cvs_in2", "frac_seg", "week"))
ggtmp$cvs_in2	<- factor(ggtmp$cvs_in2, levels = c(1, 0), labels = c("Less than 2 mi.", "Greater than 2 mi."))

pdf(paste(plot.wd, "/fg_percap_trend_fracseg.pdf", sep=""), width = ww, height = ww*ar)
ggplot(subset(ggtmp, !is.na(cvs_in2) & !is.na(frac_seg)), aes(week, value, linetype = cvs_in2, color = frac_seg)) + geom_line() + 
			geom_vline(xintercept = as.numeric(event.date), col = "red") + 
			facet_wrap(~variable, scales = "free") + 
			guides(linetype = guide_legend(title = "Distance to CVS"), color = guide_legend(title = "Smokers")) + 
			theme(legend.position = "bottom") + 
			labs(y = "Volume (packs)", title = "Per capita consumption by consumer groups") 
dev.off()		

###########################
# Trend of traffic to CVS # 
###########################
ggtmp	<- data.table(subset(trips, retailer_code == cvs.ret))
ggtmp	<- ggtmp[!week %in% endweek,list(n = length(purchase_date)), by = list(week)]
ggplot(ggtmp, aes(week, n)) + geom_line()

# Trend between households who live far vs close to CVS
ggtmp	<- data.table(subset(trips, !week %in% endweek & !is.na(cvs_in5)))
ggtmp	<- ggtmp[,list(	Total = length(purchase_date),
						CVS = length(purchase_date[cvs==1]), OtherDrug = length(purchase_date[cvs==0&channel_type=="Drug Store"]), 
						OtherChannel = length(purchase_date[cvs==0 & channel_type != "Drug Store"])), 
				by = list(cvs_in2, week)]
ggtmp	<- melt(ggtmp, id.vars = c("cvs_in2", "week"))		
ggtmp$cvs_in2	<- factor(ggtmp$cvs_in2, levels = c(1, 0), labels = c("Dist w/ 2miles", "Dist ge 2miles"))
		
ggplot(ggtmp, aes(week, value, linetype = cvs_in2)) + geom_line() + 
			geom_vline(xintercept = as.numeric(event.date), col = "red") + 
			facet_wrap(~variable, scales = "free") + 
			labs(y = "Traffic", title = "Total store traffic by consumer groups") 

# -----------------------------------#
# Shopping trips per week per person #
ggtmp	<- data.table(subset(trips, !week %in% endweek & !is.na(cvs_in5)))
ggtmp	<- ggtmp[,list(	Total = length(purchase_date)/length(unique(household_code)), 
						CVS = length(purchase_date[cvs==1])/length(unique(household_code)), 
						OtherDrug = length(purchase_date[cvs==0&channel_type=="Drug Store"])/length(unique(household_code)), 
						OtherChannel = length(purchase_date[cvs==0 & channel_type != "Drug Store"])/length(unique(household_code))), 
				by = list(cvs_in2, week)]
ggtmp	<- melt(ggtmp, id.vars = c("cvs_in2", "week"))		
ggtmp$cvs_in2	<- factor(ggtmp$cvs_in2, levels = c(1, 0), labels = c("Less than 2 mi.", "Greater than 2 mi."))

pdf(paste(plot.wd, "/fg_trips_dist.pdf", sep=""), width = ww, height = ww*ar)	
ggplot(ggtmp, aes(week, value, linetype = cvs_in2)) + geom_line() + 
			geom_vline(xintercept = as.numeric(event.date), col = "red") + 
			facet_wrap(~variable, scales = "free") + 
			guides(linetype = guide_legend(title = "Distance to CVS")) + 
			theme(legend.position = "bottom") + 
			labs(y = "Trips per person", title = "Shopping trips by consumer groups")
dev.off()			

# --------------------------------------------------------------- #
# Compare shopping trips by customer groups -- Close to Walgreens # 
ggtmp	<- data.table(subset(trips, !week %in% endweek & !is.na(cvs_in5)))
ggtmp	<- ggtmp[,list(	Total = length(purchase_date)/length(unique(household_code)), 
						CVS = length(purchase_date[cvs==1])/length(unique(household_code)), 
						OtherDrug = length(purchase_date[cvs==0&channel_type=="Drug Store"])/length(unique(household_code)), 
						OtherChannel = length(purchase_date[cvs==0 & channel_type != "Drug Store"])/length(unique(household_code))), 
				by = list(cvs_in2, wgr_in2, week)]
ggtmp	<- melt(ggtmp, id.vars = c("cvs_in2", "wgr_in2", "week"))		
ggtmp$wgr_in2	<- factor(ggtmp$wgr_in2, levels = c(1, 0), labels = c("Less than 2 mi.", "Greater than 2 mi."))
ggtmp$cvs_in2	<- factor(ggtmp$cvs_in2, levels = c(1, 0), labels = c("Less than 2 mi.", "Greater than 2 mi."))

pdf(paste(plot.wd, "/fg_trips_distwgr.pdf", sep=""), width = ww, height = ww*ar)	
ggplot(subset(ggtmp, !is.na(cvs_in2) & !is.na(wgr_in2)), aes(week, value, linetype = cvs_in2, color = wgr_in2)) + geom_line() + 
			geom_vline(xintercept = as.numeric(event.date), col = "red") + 
			facet_wrap(~variable, scales = "free") + 
			guides(linetype = guide_legend(title = "Distance to CVS"), color = guide_legend(title = "Distance to Walgreen")) + 
			theme(legend.position = "bottom") + 
			labs(y = "Trips per person", title = "Shopping trips by consumer groups")
dev.off()

# ------------------------------------------------------------------- #
# Compare shopping trips by customer groups -- heavy vs light smokers # 
ggtmp	<- data.table(subset(trips, !week %in% endweek & !is.na(cvs_in5)))
ggtmp	<- ggtmp[,list(	Total = length(purchase_date)/length(unique(household_code)), 
						CVS = length(purchase_date[cvs==1])/length(unique(household_code)), 
						OtherDrug = length(purchase_date[cvs==0&channel_type=="Drug Store"])/length(unique(household_code)), 
						OtherChannel = length(purchase_date[cvs==0 & channel_type != "Drug Store"])/length(unique(household_code))), 
				by = list(cvs_in2, heavy, week)]
ggtmp	<- melt(ggtmp, id.vars = c("cvs_in2", "heavy", "week"))		
ggtmp$heavy	<- factor(ggtmp$heavy, levels = c(1, 0), labels = c("Heavy", "Light"))
ggtmp$cvs_in2	<- factor(ggtmp$cvs_in2, levels = c(1, 0), labels = c("Less than 2 mi.", "Greater than 2 mi."))

pdf(paste(plot.wd, "/fg_trips_heavy.pdf", sep=""), width = ww, height = ww*ar)	
ggplot(subset(ggtmp, !is.na(cvs_in2)), aes(week, value, linetype = cvs_in2, color = heavy)) + geom_line() + 
			geom_vline(xintercept = as.numeric(event.date), col = "red") + 
			facet_wrap(~variable, scales = "free") + 
			guides(linetype = guide_legend(title = "Distance to CVS"), color = guide_legend(title = "Smokers")) + 
			theme(legend.position = "bottom") + 
			labs(y = "Trips per person", title = "Shopping trips by consumer groups")
dev.off()

# ---------------------------------------------------------- #
# Compare dollar spending by customer groups -- Close to CVS #
ggtmp	<- data.table(subset(trips, !week %in% endweek & !is.na(cvs_in5)))
ggtmp	<- ggtmp[,list(	Total = sum(total_spent)/length(unique(household_code)), 
						CVS = sum(total_spent[cvs==1])/length(unique(household_code)), 
						OtherDrug = sum(total_spent[cvs==0&channel_type=="Drug Store"])/length(unique(household_code)), 
						OtherChannel = sum(total_spent[cvs==0 & channel_type != "Drug Store"])/length(unique(household_code))), 
				by = list(cvs_in2, week)]
ggtmp	<- melt(ggtmp, id.vars = c("cvs_in2", "week"))		
ggtmp$cvs_in2	<- factor(ggtmp$cvs_in2, levels = c(1, 0), labels = c("Less than 2 mi.", "Greater than 2 mi."))

pdf(paste(plot.wd, "/fg_dol_dist.pdf", sep=""), width = ww, height = ww*ar)	
ggplot(ggtmp, aes(week, value, linetype = cvs_in2)) + geom_line() + 
			geom_vline(xintercept = as.numeric(event.date), col = "red") + 
			facet_wrap(~variable, scales = "free") + 
			guides(linetype = guide_legend(title = "Distance to CVS")) + 
			theme(legend.position = "bottom") + 
			labs(y = "Spending per person", title = "Spending by consumer groups")
dev.off()			

# -------------------------------------------------------------------- #
# Compare dollar spending by customer groups -- heavy vs light smokers #
ggtmp	<- data.table(subset(trips, !week %in% endweek & !is.na(cvs_in5)))
ggtmp	<- ggtmp[,list(	Total = sum(total_spent)/length(unique(household_code)), 
						CVS = sum(total_spent[cvs==1])/length(unique(household_code)), 
						OtherDrug = sum(total_spent[cvs==0&channel_type=="Drug Store"])/length(unique(household_code)), 
						OtherChannel = sum(total_spent[cvs==0 & channel_type != "Drug Store"])/length(unique(household_code))), 
				by = list(cvs_in2, heavy, week)]
ggtmp	<- melt(ggtmp, id.vars = c("cvs_in2", "heavy", "week"))		
ggtmp$heavy	<- factor(ggtmp$heavy, levels = c(1, 0), labels = c("Heavy", "Light"))
ggtmp$cvs_in2	<- factor(ggtmp$cvs_in2, levels = c(1, 0), labels = c("Less than 2 mi.", "Greater than 2 mi."))

pdf(paste(plot.wd, "/fg_dol_heavy.pdf", sep=""), width = ww, height = ww*ar)	
ggplot(subset(ggtmp, !is.na(cvs_in2)), aes(week, value, linetype = cvs_in2, color = heavy)) + geom_line() + 
			geom_vline(xintercept = as.numeric(event.date), col = "red") + 
			facet_wrap(~variable, scales = "free") + 
			guides(linetype = guide_legend(title = "Distance to CVS"), color = guide_legend(title = "Smokers")) + 
			theme(legend.position = "bottom") + 
			labs(y = "Spending per person", title = "Spending by consumer groups")
dev.off() 

# --------------------------------------------------------#
# Fraction of cigarret spending out of the total spending #
tmp		<- data.table(purchases)
tmp		<- tmp[,list(total_price_paid = sum(total_price_paid)), by = list(trip_code_uc)]
ggtmp	<- merge(trips, tmp, by = c("trip_code_uc"), all = T)
ggtmp$total_price_paid	<- ifelse(is.na(ggtmp$total_price_paid), 0, ggtmp$total_price_paid)

ggtmp1	<- data.table(ggtmp)
ggtmp1	<- ggtmp1[,list(Total 	= sum(total_price_paid)/sum(total_spent), 
						CVS 	= sum(total_price_paid*1*(cvs==1))/sum(total_spent*1*(cvs==1)), 
						OtherDrug  = sum(total_price_paid*1*(cvs==0&channel_type=="Drug Store"))/sum(total_spent*1*(cvs==0&channel_type=="Drug Store")),
						OtherChannel  = sum(total_price_paid*1*(cvs==0&channel_type!="Drug Store"))/sum(total_spent*1*(cvs==0&channel_type!="Drug Store")) 
						), by = list(treat, heavy,household_code, week)]
ggtmp2	<- melt(ggtmp1, id.vars = c("treat", "heavy", "household_code", "week"))
# ggplot(ggtmp2, aes(value)) + geom_histogram(aes(y=..density..), binwidth = .01) + 
# 		facet_wrap(~ variable, scales = "free") + xlim(c(0, .5)) + 
# 		labs(title = "Histogram of fraction of cigarette spending out of basket in differnt channels")
ggplot(subset(ggtmp2, value > 0), aes(value)) + geom_histogram(aes(y=..density..), binwidth = .01) + 
		facet_wrap(~ variable, scales = "free") + 
		labs(title = "Histogram of fraction of cigarette spending conditional on cigarette purchases")

# Trend of fraction of cigarette spending
ggtmp2	<- ggtmp1[,list(Total = mean(Total, na.rm=T), CVS = mean(CVS, na.rm=T), 
						OtherDrug = mean(OtherDrug, na.rm=T), OtherChannel = mean(OtherChannel, na.rm=T)), 
					by = list(treat, week)]						
ggtmp2	<- melt(ggtmp2, id.vars = c("week", "treat"))
ggtmp2$treat	<- factor(ggtmp2$treat, levels = c(1, 0), labels = c("Less than 2 mi.", "Greater than 2 mi."))

ggplot(ggtmp2, aes(week, value, linetype = treat)) + geom_line() + 
			geom_vline(xintercept = as.numeric(event.date), col = "red") + 
			facet_wrap(~variable, scales = "free") + 
			guides(linetype = guide_legend(title = "Distance to CVS")) + 
			theme(legend.position = "bottom") + 
			labs(y = "Fraction of cigarette spending", title = "Fraction of cigarette spending")
