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
out.file <- "cvs_sumstat"

panelists 	<- read.csv("tob_CVS_pan.csv", header = T)
purchases	<- read.csv("tob_CVS_purchases.csv", header = T)
trips		<- read.csv("tob_CVS_trips.csv", header = T)
ma.policy	<- read.xlsx("MA_policy.xlsx", 1)
unique(trips$channel_type)	
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
cnty				<- c("Berkeley","Daly City","Healdsburg","Hollister","Marin","Richmond","San Francisco","Santa Clara", "Sonoma" )
panelists$ban_ard	<- with(panelists, 1*((statecode=="MA" & countynm %in% ma.policy$countynm)| (statecode=="CA" & countynm %in% cnty)))
sort(unique(panelists[panelists$ban_ard==1,"countynm"]))
table(panelists$ban_ard)

# Classify households distance to CVS
tmp				<- data.table(panelists)
tmp				<- tmp[,list(nzip = length(unique(panelist_zip_code)), nd = length(unique(distance_cvs))), by = list(household_code)]
summary(tmp); mean(tmp$nzip>1)
mypan			<- panelists[panelists$panel_year == 2014,]			# Use 2014 panelist profile
median(mypan$distance_cvs)
mypan			<- subset(mypan, !is.na(distance_cvs))
mypan$cvs_in2	<- ifelse(mypan$distance_cvs <=2, 1, 0)
mypan$cvs_in3	<- ifelse(mypan$distance_cvs <=3, 1, 0)
mypan$wgr_in2	<- ifelse(mypan$distance_walgreens <=2, 1, 0)

# Classify light vs heavy smokers
tmp1		<- data.table(purchases)
setkeyv(tmp1, c("household_code", "purchase_date"))
tmp1		<- tmp1[, list(consum = sum(quantity*size/qunit, na.rm=T)/(as.numeric(max(purchase_date)-min(purchase_date)))*7), 
					by = list(household_code)]
summary(tmp1$consum)       
median(tmp1$consum, na.rm=T)
tmp1$heavy 	<- 1*(tmp1$consum > median(tmp1$consum, na.rm=T) )
tmp			<- setNames(tmp1$heavy, tmp1$household_code)
mypan$heavy 	<- tmp[as.character(mypan$household_code)]

# Fraction of cigaratte purchases at CVS
tmp1		<- data.table(subset(purchases, purchase_date < event.date))            # Use pre-event purchases 
tmp1		<- tmp1[,list(cvs = sum(quantity*size*cvs), total = sum(quantity*size)), 
							by = list(household_code)]
tmp1		<- tmp1[,cvs_share := cvs/total]
hist(tmp1$cvs_share, breaks = 50)

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
tmp 		<- tmp2[,list(household_code, total_price_paid, total_spent, purchase_date)]
tmp			<- tmp[,list(cig_frac = sum(total_price_paid)/sum(total_spent), 
					 cig_frac_cond = sum(total_price_paid[total_price_paid>0])/sum(total_spent[total_price_paid>0]) ),
					by = list(household_code)]
tmp[is.na(tmp)]	<- 0
summary(tmp)
ggtmp		<- melt(tmp, id.vars = "household_code")
ggtmp$variable 	<- factor(ggtmp$variable, levels = c("cig_frac", "cig_frac_cond"), 
					labels = c("Fraction of cigarette spending\n out of total CVS spending", 
					"Fraction of cigarette spending\n conditional on cigarette purchases at CVS"))
ggplot(ggtmp, aes(value)) + geom_histogram() + facet_wrap(~variable) 
median(tmp[cig_frac>0,cig_frac])		
tmp$frac_seg	<- ifelse(tmp$cig_frac ==0, "Zero", ifelse(tmp$cig_frac <= median(tmp[cig_frac>0,cig_frac]), "S1", "S2"))
table(tmp$frac_seg)
mypan			<- merge(mypan, tmp[,list(household_code, frac_seg, cig_frac)], by = "household_code", all.x=T)
mypan[is.na(mypan$frac_seg), "frac_seg"]	<- "Never"
mypan$frac_seg	<- factor(mypan$frac_seg, levels = c("Never", "Zero", "S1", "S2"))
mypan$treat 	<- with(mypan, 1*(frac_seg != "Never" & ban_ard == 0 ))         # Define treatment dummy
table(mypan$frac_seg)
table(mypan$treat)

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

# Calculate pre-event shopping behavior for each household
tmp		<- data.table(trips)
tmp		<- tmp[,list(start = min(month), end = max(month)), by = list(household_code)]
tmp		<- tmp[, n:= end-start]
tmp1	<- lapply(1:nrow(tmp), function(i) tmp[i,start] + c(0:tmp[i,n]))
names(tmp1)	<- tmp$household_code
tmp1	<- melt(tmp1)
names(tmp1)	<- c("month", "household_code")

tmp2	<- data.table(purchases)
tmp2	<- tmp2[, list(q = sum(quantity*size/qunit, na.rm=T), q_cvs = sum(quantity[cvs==1]*size[cvs==1]/qunit, na.rm=T), 
                    nstore = length(unique(retailer_code)), nchannel = length(unique(channel_type)), 
                    cig_dol = sum(total_price_paid - coupon_value), cig_dol_cvs = sum(total_price_paid[cvs==1] - coupon_value[cvs==1]) ), 
             by = list(household_code, month)]
dim(tmp1); dim(tmp2)
tmp1	<- merge(tmp1, tmp2, by = c("household_code", "month"), all.x=T)
dim(tmp1)

tmp2 	<- data.table(trips)
tmp2	<- tmp2[,list(total_spent = sum(total_spent)), by = list(household_code, month, purchase_date, channel_type, retailer_code, cvs)]
tmp2	<- tmp2[,list(	
			            trip_cvs 		= length(purchase_date[cvs==1]), 
			            trip_othdrug 	= length(purchase_date[channel_type == "Drug Store" & cvs ==0]), 
			            trip_othchannel = length(purchase_date[channel_type != "Drug Store"]), 
						trip_total  	= length(purchase_date),
						dol_cvs 		= sum(total_spent*cvs, na.rm = T), 
						dol_othdrug		= sum(total_spent*(1-cvs)*1*(channel_type == "Drug Store"), na.rm = T), 
						dol_othchannel	= sum(total_spent*1*(channel_type != "Drug Store"), na.rm = T), 
						dol_total		= sum(total_spent)
						), 
				by = list(household_code, month)]
dim(tmp1); dim(tmp2)
tmp2	<- data.table(merge(tmp1, tmp2, by = c("household_code", "month"), all.x = T))				
tmp2[is.na(tmp2)]	<- 0
tmp2	<- tmp2[month < event.month,]
tmp2	<- tmp2[,list(	pre_q 				= mean(q),
			            pre_qshr_cvs   		= ifelse(sum(q)==0, 0, sum(q_cvs)/sum(q)),
			            pre_cig_dol    		= mean(cig_dol), 
			            pre_cig_dol_cvs 	= mean(cig_dol_cvs),
			            nstore   			= mean(nstore), 
			            nchannel 			= mean(nchannel),
						pre_trip_cvs 		= mean(trip_cvs), 
						pre_trip_othdrug 	= mean(trip_othdrug), 
						pre_trip_othchannel = mean(trip_othchannel),
						pre_trip_total		=mean(trip_total), 
						pre_dol_cvs 		= mean(dol_cvs), 
						pre_dol_othdrug		= mean(dol_othdrug), 
						pre_dol_othchannel	= mean(dol_othchannel)), 
			by = list(household_code)]
mypan	<- merge(mypan, tmp2, by = "household_code", all.x = T)	
summary(mypan)

# Check if any households have missing pre-event metrics
bhv.col		<- c("pre_q", "pre_cig_dol","nstore", "nchannel", "pre_qshr_cvs", "pre_cig_dol_cvs",
              "pre_trip_cvs", "pre_trip_othdrug", "pre_trip_othchannel", "pre_dol_cvs", "pre_dol_othdrug", "pre_dol_othchannel")
sapply(c(demo.col,bhv.col, "heavy"), function(i) sum(is.na(mypan[,i])))				
sel			<- apply(mypan[,c(demo.col,bhv.col, "heavy")], 1, function(x) any(is.na(x)))
sum(sel)

# Check outliers 
summary(mypan$pre_q)
quantile(mypan$pre_q, .95)
mean(mypan$pre_q > 50); sum(mypan$pre_q > 50)
mypan <- subset(mypan, pre_q <= 50)

# Merge panelist and trips/purchases
purchases	<- subset(purchases, household_code %in% mypan$household_code)
trips		<- subset(trips, household_code %in% mypan$household_code)
trips		<- merge(trips, mypan[,c("household_code", "ban_ard", "distance_cvs","cvs_in2", "heavy", "treat","frac_seg")], 
						by = "household_code", all.x=T)
purchases	<- merge(purchases, mypan[,c("household_code", "ban_ard", "distance_cvs","cvs_in2", "wgr_in2", "heavy", "treat","frac_seg")], 
						by = "household_code", all.x=T)						

#################
# Summary stats # 
#################
cat("Number of households in the data:", length(unique(panelists$household_code)))
cat("Number of households in treatment and control group\n"); print(table(mypan$treat)); cat("\n")

# For each household, we look at their weekly cigarrette consumption level 
# We use the entire data to report summary stats, and an alternative is to report pre-event level. 
sel  <- list(c("distance_cvs", "pre_q", "pre_cig_dol","nstore", "nchannel", "pre_qshr_cvs", "pre_cig_dol_cvs"), 
             c("pre_trip_cvs", "pre_trip_othdrug", "pre_trip_othchannel"), 
             c("pre_dol_cvs", "pre_dol_othdrug", "pre_dol_othchannel"))
names(sel)<- c("Monthly cigarette purchases", "Monthly trips", "Monthly expenditure")
sel.lab <- list(c("Distance to CVS (mi.)", "Quantity (pack)", "Cigarette spending", "No. stores of cigarette purchases", 
                  "No. channels of cigarette purchases", "CVS share of cigarette quantity", "Cigarette spending at CVS"), 
                c("No. trips to CVS", "No. trips to other drug stores", "No. trips to other channels"), 
                c("Expenditure at CVS", "Expenditure at other drug stores", "Expenditure at other channels") )
cat("Summary statistics across households during 201301 - 201408:\n"); print(summary(mypan[,unlist(sel)])); cat("\n")

stargazer(mypan[,unlist(sel)], type = "html", 
      title = "Summary statistics of cigarette purchases and shopping trips across households during January 2013 - August 2014",
			covariate.labels = unlist(sel.lab), 
			digits = 2, 
			summary.stat = c("n", "mean", "sd", "min", "p25", "median", "p75", "max"), 
			out = paste(plot.wd, "/tb_", out.file,".html", sep=""))			 						

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

#################################################################
# Overall differences between CVS shoppers and non-CVS shoppers # 
#################################################################
# Differences in demographics and shopping behavior before the event
selcol  <- c("distance_cvs", "distance_walgreens", demo.col, bhv.col)
sel		<- sapply(selcol, function(i) is.numeric(mypan[,i]))
ggtab	<- NULL
for(i in selcol[sel]){
  tmp	<- t.test(mypan[,i] ~ mypan$treat)
  tmp1	<- c(tmp$estimate, dif = diff(tmp$estimate), tmp$statistic, p = tmp$p.value)
  ggtab	<- rbind(ggtab, tmp1)
}
ggtab			<- rbind(c(table(mypan$treat), rep(NA, 3)), ggtab)
rownames(ggtab)	<- c("N", selcol[sel])
colnames(ggtab)	<- c("Control", "Treatment", "Difference", "t stat", "P value")

# Chi-square test for categorical variables 
ggtab1		<- NULL
for(i in selcol[!sel]){
  tmp	<- table(mypan[,i], mypan$treat)
  tmp1	<- chisq.test(tmp)
  tmp 	<- apply(tmp, 2, function(x) x/sum(x))
  tmp1	<- rbind(cbind(tmp, tmp[,2]-tmp[,1], NA, NA), c(NA, NA, NA,tmp1$statistic, p = tmp1$p.value))
  ggtab1	<- rbind(ggtab1, tmp1)
}
colnames(ggtab1)	<- colnames(ggtab)
ggtab <- rbind(ggtab, ggtab1)
cat("Tests of demongraphic differences:\n"); print(round(ggtab, 2)); cat("\n")

# --------------------------------------------------- #
# Overall before-after differences for the two groups #
tmp1 <- data.table(subset(purchases, month >= event.month - 3 & month <= event.month + 2))
tmp1$after  <- 1*(tmp1$month >= event.month)
tmp1  <- tmp1[, list(q = sum(quantity*size/qunit, na.rm=T), 
                     q_cvs  = sum(quantity*size*cvs/qunit, na.rm=T),
                     q_othdrug = sum(quantity*size*(1-cvs)*1*(channel_type == "Drug Store")/qunit, na.rm=T), 
                     q_othchannel = sum(quantity*size*1*(channel_type != "Drug Store")/qunit, na.rm=T) 
                     ), 
              by = list(treat, household_code, after)]

tmp2 <- data.table(subset(trips,  month >= event.month - 3 & month <= event.month + 2))
tmp2	<- tmp2[,list(total_spent = sum(total_spent)), by = list(treat, household_code, month, purchase_date, channel_type, retailer_code, cvs)]
tmp2$after <-  1*(tmp2$month >= event.month)
tmp2	<- tmp2[,list(trip_cvs 		= length(purchase_date[cvs==1]), 
                    trip_othdrug 	= length(purchase_date[channel_type == "Drug Store" & cvs ==0]), 
                    trip_othchannel = length(purchase_date[channel_type != "Drug Store"]), 
                    dol_cvs 		= sum(total_spent*cvs, na.rm = T), 
                    dol_othdrug		= sum(total_spent*(1-cvs)*1*(channel_type == "Drug Store"), na.rm = T), 
                    dol_othchannel	= sum(total_spent*1*(channel_type != "Drug Store"), na.rm = T), 
                    dol_total		= sum(total_spent)
				), 
				by = list(treat, household_code, after)]
tmp <- merge(tmp1, tmp2, by = c( "treat", "household_code","after"), all.y = TRUE)
tmp <- tmp[, drop:= 1*(length(after) < 2), by = list(household_code)]
table(tmp$drop)									# 1 household did not both 2 period data
tmp <- subset(tmp, drop == 0)				
setkeyv(tmp, c( "treat", "household_code","after"))

# Calculate before-after difference for each household 
selcol <- setdiff(names(tmp), c( "treat", "household_code","after","drop"))
sel <- tmp$after == 1
m.bf <- as.matrix(tmp[!sel,selcol, with=FALSE])
m.af <- as.matrix(tmp[sel,selcol, with=FALSE])
m.bf[is.na(m.bf)] <- 0
m.af[is.na(m.af)] <- 0
dim(m.bf); dim(m.af)
d   <- m.af - m.bf
treat.idx <- unique(tmp, by = c("treat", "household_code"))$treat

# T-test of 1st difference
ggtab1	<- NULL
for(i in selcol){
  tmp		<- t.test(d[,i] ~ treat.idx)
  tmp1	<- c(tmp$estimate, dif = diff(tmp$estimate), tmp$statistic, p = tmp$p.value)
  ggtab1	<- rbind(ggtab1, tmp1)
}
dimnames(ggtab1) <- list(selcol, c("Control", "Treatment", "Difference", "t stat", "P value"))
cat("Before-after difference between treatment and control group during 201406 - 201411\n"); print(ggtab1); cat("\n")

stargazer(list(ggtab, ggtab1), type = "html", align = TRUE, no.space = TRUE, 
          title = c("Test difference in demographics and pre-event shopping behavior", 
                    "Average before-after difference between treatment and control group during 201406 - 201411"), 
          notes = c("The treatment group includes CVS shoppers and the households in MA and SF."), 
          out = paste(plot.wd, "/tb_", out.file,"_difftest.html", sep=""))

rm(list = c("d", "tmp", "m.af", "m.bf", "sel", "selcol", "tmp1", "tmp2", "treat.idx"))

########################################
# Overall trend of cigarette purchases #
########################################
# Trend of total cigarette sales in CVS, non-CVS drug stores, and all other retailers. 
ggtmp	<- data.table(subset(purchases, !week %in% endweek))
ggtmp	<- ggtmp[,list(Total 		= sum(quantity*size/qunit, na.rm=T), 
						CVS 		= sum(quantity*size*cvs/qunit, na.rm=T), 
	 					OtherDrug 	= sum(quantity*size*(1-cvs)*1*(channel_type=="Drug Store")/qunit, na.rm=T), 
						OtherChannel= sum(quantity*size*(1-cvs)*1*(channel_type!="Drug Store")/qunit, na.rm=T)), 
				by = list(week)]
ggtmp1	<- melt(ggtmp, id.vars = "week")	

pdf(paste(plot.wd, "/fg_", out.file, "_overall_trend.pdf", sep=""), width = ww, height = ww*ar)
print(ggplot(ggtmp1, aes(x = week, value)) + geom_line() + facet_wrap(~variable, scales = "free") + 
			labs(y = "Volume (packs)", title = "Trend of cigarette weekly sales")	)

# Trend of market share 
ggtmp	<- ggtmp[,':='(CVS = CVS/Total, OtherDrug = OtherDrug/Total, OtherChannel = OtherChannel/Total )]		
ggtmp1	<- melt(ggtmp, id.vars = "week")
print(ggplot(subset(ggtmp1, variable != "Total"), aes(x = week, value)) + geom_line() + facet_wrap(~variable, scales = "free") + 
			scale_y_continuous(labels = percent) + 
			labs(y = "Market share", title = "Trend of market share")	)
dev.off()			

# ------------------------------------------------------------------------------------------- #
# Compare per capita consumption trend by consumer group -- CVS shoppers vs. Non-CVS shoppers #
plots 	<- list(NULL)
idx  	<- 1
mycol	<- c("#d0d1e6", "#d0d1e6", "#74a9cf", "#023858")
mycol2	<- mycol[c(1,4)]

ggtmp	<- data.table(purchases)			
ggtmp	<- ggtmp[,nhh := length(unique(household_code)), by = list(treat)]
# ggtmp	<- ggtmp[!week %in% endweek, list(Total = sum(quantity*size/qunit, na.rm=T)/length(unique(household_code)), 
# 					CVS = sum(quantity*size*cvs/qunit, na.rm=T)/length(unique(household_code)), 
#  					OtherDrug = sum(quantity*size*(1-cvs)*1*(channel_type=="Drug Store")/qunit, na.rm=T)/length(unique(household_code)), 
# 					OtherChannel = sum(quantity*size*(1-cvs)*1*(channel_type!="Drug Store")/qunit, na.rm=T)/length(unique(household_code))), 
# 				by = list(treat,week)]
ggtmp	<- ggtmp[!week %in% endweek, list(Total = sum(quantity*size/qunit/nhh, na.rm=T), 
					CVS = sum(quantity*size*cvs/qunit/nhh, na.rm=T), 
 					OtherDrug = sum(quantity*size*(1-cvs)*1*(channel_type=="Drug Store")/qunit/nhh, na.rm=T), 
					OtherChannel = sum(quantity*size*(1-cvs)*1*(channel_type!="Drug Store")/qunit/nhh, na.rm=T)), 
				by = list(treat,week)]
ggtmp	<- melt(ggtmp, id.vars = c("treat", "week"))
ggtmp$treat <- factor(ggtmp$treat, levels = c(1, 0), labels = c("Treatment", "Control"))
plots[[idx]] <- ggplot(ggtmp, aes(week, value, linetype = treat)) + geom_line() + 
						geom_vline(xintercept = as.numeric(event.date), col = "red") + 
						facet_wrap(~variable, scales = "free") + 
						guides(linetype = guide_legend(title = "")) + 
						theme(legend.position = "bottom") + 
						labs(y = "Volume (packs)", title = "Per capita cigarette consumption by consumer groups") 
idx <- idx + 1	

# Compare per capita consumption trend by consumer group -- fraction of cigarette spending out of CVS spending # 
ggtmp	<- data.table(purchases)	
ggtmp	<- ggtmp[,nhh:=length(unique(household_code)), by = list(frac_seg)]		
ggtmp	<- ggtmp[!week %in% endweek, list(Total = sum(quantity*size/qunit/nhh, na.rm=T), 
					CVS = sum(quantity*size*cvs/qunit/nhh, na.rm=T), 
 					OtherDrug = sum(quantity*size*(1-cvs)*1*(channel_type=="Drug Store")/qunit/nhh, na.rm=T), 
					OtherChannel = sum(quantity*size*(1-cvs)*1*(channel_type!="Drug Store")/qunit/nhh, na.rm=T)), 
				by = list(frac_seg,week)]
ggtmp	<- melt(ggtmp, id.vars = c("frac_seg", "week"))
ggtmp$frac_seg 	<- factor(ggtmp$frac_seg, levels = rev(levels(ggtmp$frac_seg)))	
plots[[idx]] <- ggplot(ggtmp, aes(week, value, color = frac_seg, linetype = frac_seg)) + geom_line() + 
						geom_vline(xintercept = as.numeric(event.date), col = "red") + 
						scale_color_grey() + 
						facet_wrap(~variable, scales = "free") + 
						guides(linetype = guide_legend(title = "Fraction of cigarette spending\n out of total CVS spending"), 
							   color = guide_legend(title = "Fraction of cigarette spending\n out of total CVS spending")) + 
						theme(legend.position = "bottom") + 
						labs(y = "Volume (packs)", title = "Per capita cigarette consumption by consumer groups") 
idx <- idx + 1	

# ----------------------------------------------------------------------------- #
# Compare trend of trips by consumer group -- CVS shoppers vs. Non-CVS shoppers #
ggtmp	<- data.table(subset(trips, !week %in% endweek))
ggtmp	<- ggtmp[,list(total_spent = sum(total_spent)), by = list(treat, household_code, month, week, purchase_date, channel_type, retailer_code, cvs)]
ggtmp	<- ggtmp[,nhh := length(unique(household_code)), by = list(treat)]
ggtmp	<- ggtmp[,list(	Total = length(purchase_date)/nhh[1], 
						CVS = length(purchase_date[cvs==1])/nhh[1], 
						OtherDrug = length(purchase_date[cvs==0&channel_type=="Drug Store"])/nhh[1], 
						OtherChannel = length(purchase_date[cvs==0 & channel_type != "Drug Store"])/nhh[1]), 
				by = list(treat, week)]
ggtmp	<- melt(ggtmp, id.vars = c("treat", "week"))		
ggtmp$treat <- factor(ggtmp$treat, levels = c(1, 0), labels = c("Treatment", "Control"))
plots[[idx]]<- ggplot(ggtmp, aes(week, value, linetype = treat)) + geom_line() + 
					geom_vline(xintercept = as.numeric(event.date), col = "red") + 
					facet_wrap(~variable, scales = "free") + 
					guides(linetype = guide_legend(title = "")) + 
					theme(legend.position = "bottom") + 
					labs(y = "Trips per person", title = "Shopping trips by consumer groups")		
idx 	<- idx + 1

# Trend of trips by consumer group -- fraction of cigarette spending out of CVS spending # 
ggtmp	<- data.table(subset(trips, !week %in% endweek))
ggtmp	<- ggtmp[,list(total_spent = sum(total_spent)), by = list(frac_seg, household_code, month, week, purchase_date, channel_type, retailer_code, cvs)]
ggtmp	<- ggtmp[,nhh := length(unique(household_code)), by = list(frac_seg)]
ggtmp	<- ggtmp[,list(	Total = length(purchase_date)/nhh[1], 
						CVS = length(purchase_date[cvs==1])/nhh[1], 
						OtherDrug = length(purchase_date[cvs==0&channel_type=="Drug Store"])/nhh[1], 
						OtherChannel = length(purchase_date[cvs==0 & channel_type != "Drug Store"])/nhh[1]), 
				by = list(frac_seg, week)]
ggtmp	<- melt(ggtmp, id.vars = c("frac_seg", "week"))	
ggtmp$frac_seg 	<- factor(ggtmp$frac_seg, levels = rev(levels(ggtmp$frac_seg)))	
plots[[idx]] <- ggplot(ggtmp, aes(week, value, color = frac_seg, linetype = frac_seg)) + geom_line() + 
					geom_vline(xintercept = as.numeric(event.date), col = "red") + 
					scale_color_grey() + 
					facet_wrap(~variable, scales = "free") + 
					guides(linetype = guide_legend(title = "Fraction of cigarette spending\n out of total CVS spending"), 
						   color = guide_legend(title = "Fraction of cigarette spending\n out of total CVS spending")) + 
					theme(legend.position = "bottom") + 
					labs(y = "Trips per person", title = "Shopping trips by consumer groups")		
idx <- idx + 1

# ----------------------------------------------------------------------------------- #
# Compare trend of expenditure by consumer group -- CVS shoppers vs. Non-CVS shoppers #
ggtmp	<- data.table(subset(trips, !week %in% endweek))
ggtmp	<- ggtmp[,list(total_spent = sum(total_spent)), by = list(treat, household_code, month, week, purchase_date, channel_type, retailer_code, cvs)]
ggtmp	<- ggtmp[,nhh := length(unique(household_code)), by = list(treat)]
ggtmp	<- ggtmp[,list(	Total = sum(total_spent)/nhh[1], 
						CVS = sum(total_spent[cvs==1])/nhh[1], 
						OtherDrug = sum(total_spent[cvs==0&channel_type=="Drug Store"])/nhh[1], 
						OtherChannel = sum(total_spent[cvs==0 & channel_type != "Drug Store"])/nhh[1]), 
				by = list(treat, week)]
ggtmp	<- melt(ggtmp, id.vars = c("treat", "week"))		
ggtmp$treat <- factor(ggtmp$treat, levels = c(1, 0), labels = c("Treatment", "Control"))
plots[[idx]]<- ggplot(ggtmp, aes(week, value, linetype = treat)) + geom_line() + 
					geom_vline(xintercept = as.numeric(event.date), col = "red") + 
					facet_wrap(~variable, scales = "free") + 
					guides(linetype = guide_legend(title = "")) + 
					theme(legend.position = "bottom") + 
					labs(y = "Weekly expenditure", title = "Shopping trips by consumer groups")		
idx 	<- idx + 1

# Trend of expenditure by consumer group -- fraction of cigarette spending out of CVS spending # 
ggtmp	<- data.table(subset(trips, !week %in% endweek))
ggtmp	<- ggtmp[,list(total_spent = sum(total_spent)), by = list(frac_seg, household_code, month, week, purchase_date, channel_type, retailer_code, cvs)]
ggtmp	<- ggtmp[,nhh := length(unique(household_code)), by = list(frac_seg)]
ggtmp	<- ggtmp[,list(	Total = sum(total_spent)/nhh[1], 
						CVS = sum(total_spent[cvs==1])/nhh[1], 
						OtherDrug = sum(total_spent[cvs==0&channel_type=="Drug Store"])/nhh[1], 
						OtherChannel = sum(total_spent[cvs==0 & channel_type != "Drug Store"])/nhh[1]), 
				by = list(frac_seg, week)]
ggtmp	<- melt(ggtmp, id.vars = c("frac_seg", "week"))		
ggtmp$frac_seg 	<- factor(ggtmp$frac_seg, levels = rev(levels(ggtmp$frac_seg)))	
plots[[idx]] <- ggplot(ggtmp, aes(week, value, color = frac_seg, linetype = frac_seg)) + geom_line() + 
					geom_vline(xintercept = as.numeric(event.date), col = "red") + 
					scale_color_grey() + 
					facet_wrap(~variable, scales = "free") + 
					guides(linetype = guide_legend(title = "Fraction of cigarette spending\n out of total CVS spending"), 
						   color = guide_legend(title = "Fraction of cigarette spending\n out of total CVS spending")) + 
					theme(legend.position = "bottom") + 
					labs(y = "Trips per person", title = "Shopping trips by consumer groups")	

pdf(paste(plot.wd, "/fg_",out.file, "_trend.pdf", sep=""), width = ww, height = ww*ar)
for(i in 1:length(plots)){
	print(plots[[i]])
}
dev.off()
