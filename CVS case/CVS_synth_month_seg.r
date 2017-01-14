library(reshape2)
library(ggplot2)
library(data.table)
library(lubridate)
library(stargazer)
library(zipcode)
library(Synth)

# setwd("~/Documents/Research/Tobacco/processed_data")
# plot.wd		<- "~/Desktop"
setwd("U:/Users/ccv103/Desktop")
# setwd("/sscc/home/c/ccv103/Tobacco") 
plot.wd		<- getwd()
out.file	<- "cvs_syn_month_seg"

ww			<- 6.5
ar			<- .6

panelists 	<- read.csv("tob_CVS_pan.csv", header = T)
purchases	<- read.csv("tob_CVS_purchases.csv", header = T)
trips		<- read.csv("tob_CVS_trips.csv", header = T)
nsmk.pan 	<- read.csv("tob_CVS_nonsmk_pan.csv", header = T)
nsmk.trips	<- read.csv("tob_CVS_nonsmk_trips.csv", header = T)

panelists$smk	<- 1
nsmk.pan$smk	<- 0
trips$smk		<- 1
nsmk.trips$smk	<- 0
panelists		<- rbind(panelists, nsmk.pan)
trips			<- rbind(trips, nsmk.trips)

# # Select a random sample
# sel		<- sample(unique(panelists$household_code), .2*length(unique(panelists$household_code)))
# panelists	<- subset(panelists, household_code %in% sel)
# trips 		<- subset(trips, household_code %in% sel)
# purchases	<- subset(purchases, household_code %in% sel )

#############
# Functions # 
#############
synth.fn	<- function(focal.seg, prior.period, match.var, dv, fit = TRUE){	
	# Representative customer in the focal segment 
	tmp1		<- data.table(subset(mydata, segment == focal.seg & treat == 1 & month %in% my.window))
	tmp1		<- tmp1[,list(	q = mean(q), q_othdrug = mean(q_othdrug), q_othchannel = mean(q_othchannel), 
								trip_cvs = mean(trip_cvs), trip_othdrug = mean(trip_othdrug), trip_othchannel = mean(trip_othchannel), 
								dol_cvs = mean(dol_cvs), dol_othdrug = mean(dol_othdrug), dol_othchannel = mean(dol_othchannel)), 
						by = list(month)]
	tmp1		<- data.frame(household_code = 1, tmp1)	
	foc.data	<- rbind(tmp1, subset(mydata, segment == focal.seg & treat == 0 & month %in% my.window)[, c("household_code", "month", match.var)])
	
	# Identify the id number of focal treatment and control 
	focal.contrl	<- setdiff(unique(foc.data$household_code), 1)
	cat("In the segment of", focal.seg, ", the number of controsl is", length(focal.contrl), ".\n")	
	
	# Organize syntheti control data 
	synth.data	<- dataprep(foo = foc.data, 
							predictors = match.var, 
							dependent = dv, 
							unit.variable = "household_code", time.variable = "month", 
							treatment.identifier = 1, controls.identifier = focal.contrl, 
							time.predictors.prior = prior.period, time.optimize.ssr = prior.period, 
							time.plot = sort(unique(foc.data$month)))
	if(fit){
		syn.out		<- synth(synth.data)	
		out			<- list(data = synth.data, synout = syn.out)
	}else{
		out			<- synth.data
	}						
	
	return(out)
}

placebo.test	<- function(focal.seg, prior.period, match.var, dv.vec, select.hh = NULL){
	# Representative customer in the focal segment 
	tmp1		<- data.table(subset(mydata, segment == focal.seg & treat == 1 & month %in% my.window))
	tmp1		<- tmp1[,list(	q = mean(q), q_othdrug = mean(q_othdrug), q_othchannel = mean(q_othchannel), 
								trip_cvs = mean(trip_cvs), trip_othdrug = mean(trip_othdrug), trip_othchannel = mean(trip_othchannel), 
								dol_cvs = mean(dol_cvs), dol_othdrug = mean(dol_othdrug), dol_othchannel = mean(dol_othchannel)), 
						by = list(month)]
	tmp1		<- data.frame(household_code = 1, tmp1)	
	foc.data	<- rbind(tmp1, subset(mydata, segment == focal.seg & treat == 0 & month %in% my.window)[, c("household_code", "month", match.var)])
	if(!is.null(select.hh)){
		foc.data	<- subset(foc.data, household_code %in% select.hh)
	}
	
	# Identify the id number of focal treatment and control 
	focal.contrl	<- setdiff(unique(foc.data$household_code), 1)
	cat("In the segment of", focal.seg, ", the number of controsl is", length(focal.contrl), ".\n")	
	
	gap.data		<- data.frame()
	for(i in 1:length(focal.contrl)){
		synth.data	<- dataprep(foo = foc.data, 
								predictors = match.var, 
								dependent = dv.vec[1], 
								unit.variable = "household_code", time.variable = "month", 
								treatment.identifier = focal.contrl[i], controls.identifier = focal.contrl[-i], 
								time.predictors.prior = prior.period, time.optimize.ssr = prior.period, 
								time.plot = sort(unique(foc.data$month)))
		syn.out		<- synth(synth.data)	
				
		# Compute gaps 
		for(j in 1:length(dv.vec)){
			if(j == 1){
				tmpdata <- synth.data
			}else{
				tmpdata	<- dataprep(foo = foc.data, 
										predictors = match.var, 
										dependent = dv.vec[j], 
										unit.variable = "household_code", time.variable = "month", 
										treatment.identifier = focal.contrl[i], controls.identifier = focal.contrl[-i], 
										time.predictors.prior = prior.period, time.optimize.ssr = prior.period, 
										time.plot = sort(unique(foc.data$month)))
			}	
			# Compute gaps after setting pre-treatment gaps = 0
			ggtmp		<- data.frame(	month = as.vector(tmpdata$tag$time.plot), 
										treated = tmpdata$Y1plot, 
										synthetic = tmpdata$Y0plot %*% syn.out$solution.w, 
										segment = focal.seg, DV = dv.vec[j], counterid = focal.contrl[i])									
			names(ggtmp)<- c("month", "Treated", "Synthetic", "Segment", "DV", "counterid")
			gap.data	<- rbind(gap.data, tmp)
		}
	}
	return(gap.data)
}


#################
# Organize data # 
#################
# Add week
event.date	<- as.Date("2014-09-01", format = "%Y-%m-%d")			# Placebo event 
event.month	<- month(event.date) + 12
cvs.ret	<- 4914		# retailer_code for CVS
qunit	<- 20		# 20 cigaretts per pack 
prior.period 	<- 2:(event.month - 1)
(my.window		<- c(prior.period, event.month + 0:3))

# Convert time variables
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

# ----------------------------#
# Organize household segments # 
# Classify households distance to CVS
median(panelists$distance)
tmp	<- data.table(panelists)
tmp	<- tmp[,list(nzip = length(unique(panelist_zip_code)), nd = length(unique(distance))), by = list(household_code)]
summary(tmp)
mean(tmp$nzip>1)
panelists	<- subset(panelists, !is.na(distance) & !is.na(distance_wgr) & household_code %in% tmp[nzip==1,household_code])
purchases	<- subset(purchases, household_code %in% panelists$household_code)
trips		<- subset(trips, household_code %in% panelists$household_code)
panelists$cvs_in2	<- ifelse(panelists$distance <=2, 1, 0)
panelists$wgr_in2	<- ifelse(panelists$distance_wgr <=2, 1, 0)

# Classify light vs heavy smokers
tmp1		<- data.table(purchases)
setkeyv(tmp1, c("household_code", "purchase_date"))
tmp1		<- tmp1[, list(consum = sum(quantity*size/qunit, na.rm=T)/(as.numeric(max(purchase_date)-min(purchase_date)))*7), 
					by = list(household_code)]
summary(tmp1$consum)
tmp1$heavy 	<- 1*(tmp1$consum > 2.5)
# tmp			<- setNames(tmp1$heavy, tmp1$household_code)
# panelists$heavy 	<- tmp[as.character(panelists$household_code)]
dim(panelists)
panelists 	<- merge(panelists, tmp1, by = "household_code", all.x = T)

# Define demographic-based segments
tmp 	<- data.table(panelists)
tmp		<- unique(tmp, by = "household_code")
(tmp1	<- quantile(tmp$household_income, c(0:3)/3))					# Income segments
tmp1	<- cut(tmp$household_income, tmp1, include.lowest = TRUE, labels = c("LowInc", "MedInc", "HighInc"))
tmp2	<- factor(ifelse(tmp$household_size == 1, "Single", 
						ifelse(tmp$household_size == 2, "Two", ifelse(tmp$household_size ==3, "Three", "Four+"))), 
					levels = c("Single", "Two", "Three", "Four+"))
tmp3 	<- factor(ifelse(tmp$age_and_presence_of_children == 9, "NoKids", "HaveKids"), levels = c("NoKids", "HaveKids"))
tmp$segment <- paste(tmp1, tmp2, tmp3, sep = "-")
tmplevel	<- paste(	rep(levels(tmp1), each = length(levels(tmp2))*length(levels(tmp3)) ), 
												  	rep(rep(levels(tmp2), each = length(levels(tmp3))), length(levels(tmp1))), levels(tmp3),   
													sep = "-")
tmplevel	<- setdiff(tmplevel, paste(levels(tmp1), "Single", "HaveKids", sep="-"))													
tmp$segment	<- factor(tmp$segment, levels = tmplevel)
dim(panelists)
panelists	<- merge(panelists, tmp[,list(household_code, segment)], by = "household_code", all.x = T)
dim(panelists)

# Merge household data to purchase and trip data 
length(intersect(panelists$household_code, nsmk.pan$household_code))
purchases	<- merge(purchases, panelists[,c("household_code", "panel_year", "distance","cvs_in2", "heavy", "segment")], 
						by.x = c("household_code", "year"), by.y = c("household_code", "panel_year"), all.x=T)
trips	<- merge(subset(trips, year> 2012), panelists[,c("household_code", "panel_year", "cvs_in2", "heavy", "segment")], 
						by.x = c("household_code", "year"), by.y = c("household_code", "panel_year"), all.x=T)

# -------------------------- #
# Fill in non-puchases months # 
# Complete month for each household 
tmp		<- data.table(trips)
tmp		<- tmp[,list(start = min(month), end = max(month)), by = list(household_code)]
tmp		<- tmp[, n:= end-start]
dim(tmp)
tmp		<- subset(tmp, start <= min(my.window) & end >= max(my.window))		# Subset the panelists to a balanced panel 
dim(tmp)

tmp1	<- lapply(1:nrow(tmp), function(i) tmp[i,start] + c(0:tmp[i,n]))
names(tmp1)	<- tmp$household_code
tmp1	<- melt(tmp1)
names(tmp1)	<- c("month", "household_code")
tmp1$household_code	<- as.numeric(tmp1$household_code)

# Trips and spending 
tmp2 	<- data.table(trips)
tmp2	<- tmp2[,list(	trip_cvs 		= 1*(sum(cvs)>0), 
						trip_othdrug 	= sum(channel_type == "Drug Store" & cvs ==0 ), 
						trip_othchannel = sum(channel_type != "Drug Store"), 
						dol_cvs 		= sum(total_spent*cvs, na.rm = T), 
						dol_othdrug		= sum(total_spent*(1-cvs)*1*(channel_type == "Drug Store"), na.rm = T), 
						dol_othchannel	= sum(total_spent*1*(channel_type != "Drug Store"), na.rm = T)
						), 
				by = list(household_code, month)]
dim(tmp2)		
summary(tmp2[,list(trip_cvs, trip_othdrug, trip_othchannel)])
mydata	<- merge(tmp1, tmp2, by = c("household_code", "month"), all.x = T)

# Actual cigarette purchases 
tmp2	<- data.table(purchases)
tmp2	<- tmp2[,list(	q = sum(quantity*size/qunit, na.rm=T), 
						q_othdrug = sum(quantity*size*(1-cvs)/qunit, na.rm=T), 
						q_othchannel = sum(quantity*size*1*(channel_type != "Drug Store")/qunit, na.rm=T)), 
				by = list(household_code, month)]
mydata	<- merge(mydata, tmp2, by = c("household_code", "month"), all.x = T)
sel 	<- is.na(mydata)
mydata[sel]	<- 0
cat("Summary stats:\n"); print(summary(mydata[, -c(1:2)])); cat("\n")

# Create other control variables: city and month 
mydata$year		<- ifelse(mydata$month > 12, 2014, 2013)
mydata$month1	<- mydata$month %% 12
mydata$month1	<- ifelse(mydata$month1 == 0, 12, mydata$month1)
mydata$month1	<- factor(mydata$month1)

tmp				<- panelists[,c("household_code", "panel_year", "panelist_zip_code","distance", "cvs_in2", "wgr_in2", "heavy","smk", "segment")]
tmp$panelist_zip_code	<- clean.zipcodes(tmp$panelist_zip_code)
data(zipcode)
tmp			<- merge(tmp, zipcode[,c("zip","city","state")], by.x = "panelist_zip_code", by.y = "zip")

mydata			<- merge(mydata, tmp, 
						by.x = c("household_code", "year"), by.y = c("household_code", "panel_year"), all.x=T)
dim(mydata)

mydata$after	<- 1*(mydata$month >= event.month)
mydata$hhmonth	<- paste(mydata$household_code, mydata$month, sep="-")
mydata$treat	<- with(mydata, 1*(cvs_in2 ==1 & smk == 1))
tmp				<- unique(data.table(mydata), by = "household_code")
table(tmp$segment, tmp$treat)

#########################
# Run Synthetic control # 
#########################
# Set parameters 
match.var	<- c("q", "q_othdrug", "q_othchannel", 
					"trip_cvs", "trip_othdrug", "trip_othchannel", "dol_cvs", "dol_othdrug", "dol_othchannel")
dv.vec 		<- c("q", "trip_cvs", "trip_othdrug", "dol_cvs", "dol_othdrug")
dv.lab		<- c("Total cigarette purchases (packs)", "Trips to CVS", "Trips to other drug stores", "Expenditure at CVS", "Expenditure at other drug stores")
seg.unq		<- levels(mydata$segment)

path.data	<- data.frame(NULL)
for(i in 1:length(seg.unq)){
	synout <- synth.fn(focal.seg = seg.unq[i], prior.period = prior.period, match.var, dv = dv.vec[1])
	
	# Print out synth out table
	syn.tab		<- synth.tab(dataprep.res = synout$data, synth.res = synout$synout)		
	print(syn.tab)
	
	for(j in 1:length(dv.vec)){
		if(j == 1){
			tmpdata <- synout$data
		}else{
			tmpdata	<- synth.fn(focal.seg = seg.unq[i], prior.period = prior.period, match.var, dv = dv.vec[j], fit = FALSE)
		}	
		# Compute gaps after setting pre-treatment gaps = 0
		ggtmp		<- data.frame(	month = as.vector(tmpdata$tag$time.plot), 
									treated = tmpdata$Y1plot, 
									synthetic = tmpdata$Y0plot %*% synout$synout$solution.w, 
									segment = seg.unq[i], DV = dv.vec[j])									
		names(ggtmp)<- c("month", "Treated", "Synthetic", "Segment", "DV")
		path.data	<- rbind(path.data, ggtmp)							
	}
}

ggtmp	<- melt(path.data, id.vars = c("Segment", "DV", "month"))
ggtmp$month1	<- as.Date(paste(2012+ceiling(ggtmp$month/12), ifelse(ggtmp$month%%12==0,12,ggtmp$month%%12), "01", sep="-"), 
						format = "%Y-%m-%d")
plots	<- list(NULL)

for(i in 1:length(dv.vec)){
	plots[[i]]	<- ggplot(subset(ggtmp, DV == dv.vec[i]), aes(month1, value, linetype = variable)) + geom_line() + 
			geom_vline(aes(xintercept = as.numeric(event.date)), color = "red", size = .25) + 
			facet_wrap(~ Segment) + 
			guides(linetype = guide_legend(title = "")) +  
			labs(x = "Month", y = dv.lab[i], titles = "Synthetic control comparison by demographic segments\n (Income - family size - Kids)") + 
			theme(axis.text.x = element_text(angle = 30))
}

pdf(paste(plot.wd, "/fg_", out.file, "_path_", Sys.Date(), ".pdf", sep=""), width = 10, height = 8)
for(i in 1:length(plots)){
	print(plots[[i]])
}
dev.off()

# # ---------------- #
# # Run placebo test # 
# set.seed(666)
# nsmp	<- 50		 # The number of counterfactural sample
# select.hh	<- lapply(seg.unq, function(x) sample(unique(mydata[mydata$treat ==0 & mydata$segment == x,"household_code"]), nsmp))
# 
# placebo.data	<- data.frame(NULL)
# for(i in 1:length(seg.unq)){
# 	tmp	<- placebo.test(focal.seg = seg.unq[i], prior.period, match.var, dv.vec, select.hh = select.hh[[i]])
# 	placebo.data <- rbind(placebo.data, tmp)
# }
# 
# ggtmp	<- rbind(cbind(path.data, placebo = 0), cbind(placebo.data, placebo = 1))
# ggtmp	<- melt(ggtmp, id.vars = c("placebo", "Segment", "DV", "month"))
# plots	<- list(NULL)
# for(i in 1:length(dv.vec)){
# 	plots[[i]]	<- ggplot(subset(ggtmp, DV == dv.vec[i]), aes(month, value, linetype = variable, color = factor(placebo))) + geom_line() + 
# 			geom_vline(aes(xintercept = event.month), color = "red", size = .25) + 
# 			facet_wrap(~ Segment)
# }

save.image(file = paste(plot.wd, "/", out.file, "_", Sys.Date(), ".rdata", sep=""))

cat("This program is done.\n")

