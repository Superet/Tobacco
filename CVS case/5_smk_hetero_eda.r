library(reshape2)
library(ggplot2)
library(data.table)
library(lubridate)
library(stargazer)
library(rpart)

setwd("~/Documents/Research/Tobacco/processed_data")
plot.wd		<- "~/Desktop"
# setwd("U:/Users/ccv103/Documents/Research/tobacco/processed_data")
# setwd("/sscc/home/c/ccv103/Tobacco") 
# plot.wd		<- getwd()
out.file	<- "cvs_smk_heteda"
ww			<- 8
ar			<- .6

load("cvs_smk.rdata")
# load('cvs_smk_bipt2_id.rdata')

# Define the construction of treatment and control
# 1: Control (smokers who never shopped at CVS), Treament (smokers who are also CVS shoppers)
# 2: Control (smokers who shopped at CVS but did not buy cigarettes), Treament (smokers who also purchased cigarettes at CVS)
# 3: Control (smokers who shopped at CVS but did not buy cigarettes), Treament (smokers who spent more than 17% CVS spending on cigarettes)
treat.code	<- 2	
(out.file 	<- paste(out.file, treat.code, sep="")	)	
smk.pan$treat	<- smk.pan[,paste("treat", treat.code, sep="")]
smk.trips$treat	<- smk.trips[,paste("treat",treat.code,sep="")]
# sink(paste(plot.wd, "/log_", out.file, ".txt", sep=""), append = FALSE)

# # Select a random sample
# sel		<- sample(unique(panelists$household_code), .1*length(unique(panelists$household_code)))
# panelists	<- subset(panelists, household_code %in% sel)
# trips 		<- subset(trips, household_code %in% sel)
# purchases	<- subset(purchases, household_code %in% sel )

# Set event date
event.date	<- as.Date("2014-09-01", format = "%Y-%m-%d")			# Placebo event 
event.month	<- month(event.date) + 12
cvs.ret	<- 4914		# retailer_code for CVS
qunit	<- 20		# 20 cigaretts per pack

#################
# Organize data # 		
#################
# 
tmp		<- data.table(subset(smk.trips, month < event.month))
tmp.netdol	<- tmp[,list(	netdol 				= mean(netdol), 
						netdol_cvs			= mean(netdol_cvs), 
						netdol_othdrug		= mean(netdol_othdrug), 
						netdol_othchannel	= mean(netdol_othchannel)), 
				by = list(household_code)]
summary(tmp.netdol)				

# Only use the households with defined treatment variable
mypan			<- subset(smk.pan, !is.na(treat))
mypan			<- merge(mypan, tmp.netdol, by = "household_code", all.x = T)
mypan			<- mypan[order(mypan$household_code),]
# mypan$distance_cvs	<- log(mypan$distance_cvs)
# mypan$pre_q		<- log(mypan$pre_q)
# mypan$pre_trip_total	<- log(mypan$pre_trip_total)
# mypan$pre_trip_othchannel	<- log(mypan$pre_trip_othchannel)
# mypan$pre_dol_othchannel	<- log(mypan$pre_dol_total)
mypan$s2		<- with(mypan, 1*(treat==1 & frac_seg == "S2"))						# Cigdol_cvs/dol_cvs
mypan$cls_wgr	<- with(mypan, 1*(distance_walgreens <= 1.5))
tmp				<- data.table(subset(smk.trips, month < event.month))
tmp				<- tmp[,list(cvs_shr = sum(cigdol_cvs)/sum(cigdol)), by= list(household_code)]		# cigdol_cvs/cigdol
mypan			<- merge(mypan, tmp, by = "household_code", all.x=T)
mypan$large_cvs	<- 1*(mypan$cvs_shr > .1)											
mypan$dist_dummy<- ifelse(mypan$distance_cvs <= 2, 1, 0)
mypan$cvs_shopper<- ifelse(mypan$netdol_cvs > 10, 1, 0)
summary(mypan$age)

table(mypan$treat, mypan$s2)
table(mypan$treat, mypan$large_cvs)
table(mypan$treat, mypan$heavy)
table(mypan$treat, mypan$cls_wgr)
table(mypan$treat, mypan$cvs_shopper)

# Aggregate over 3 month window around the event 
num.month			<- 3
match.col			<- c("income", "age", "have_kids", "employment", "race", "distance_cvs",
                		 "pre_q", "pre_trip_total", "pre_trip_othchannel", "pre_dol_total", "pre_dol_othchannel")
match.shtdat		<- data.table(subset(smk.trips, month >= event.month - num.month & month <= event.month + num.month -1 & !is.na(treat)))
match.shtdat		<- match.shtdat[, list(	q = sum(q)/num.month, q_cvs = sum(q_cvs)/num.month, q_othdrug = sum(q_othdrug)/num.month, q_othchannel = sum(q_othchannel)/num.month, 
									trip_cvs = sum(trip_cvs)/num.month, trip_othdrug = sum(trip_othdrug)/num.month, trip_othchannel = sum(trip_othchannel)/num.month, 
									dol_cvs = sum(dol_cvs)/num.month, dol_othdrug = sum(dol_othdrug)/num.month, dol_othchannel = sum(dol_othchannel)/num.month, 
									netdol_cvs = sum(netdol_cvs)/num.month, netdol_othdrug = sum(netdol_othdrug)/num.month, netdol_othchannel = sum(netdol_othchannel)/num.month,
									dol_total = sum(dol_total)/num.month, netdol = sum(netdol)/num.month, 
									q_convenience = sum(q_convenience)/num.month, q_grocery = sum(q_grocery)/num.month, q_discount = sum(q_discount)/num.month, 
									q_service = sum(q_service)/num.month, q_gas = sum(q_gas)/num.month, q_tobacco = sum(q_tobacco)/num.month), 
									by = list(household_code, after)]
match.shtdat 	<- match.shtdat[, drop:= 1*(length(after) < 2), by = list(household_code)]
table(match.shtdat$drop)									# 1 household did not both 2 period data
dim(match.shtdat)
match.shtdat	<- merge(match.shtdat, mypan[,c("household_code", "treat", "frac_seg", "s2", "large_cvs","heavy", "cls_wgr", "dist_dummy","cvs_shopper", match.col)], 
						by = "household_code", all.x = T)
dim(match.shtdat)
match.shtdat	<- match.shtdat[order(match.shtdat$household_code),]
max(abs(mypan$household_code - match.shtdat$household_code))
setkeyv(match.shtdat, c("household_code","after"))
match.shtdat$after	<- factor(match.shtdat$after, levels = 0:1, labels = c("before", "after"))

########################################
# Before-after scatter plot by segment # 
########################################
bf_af_plot	<- function(df, seg.var, dv, comb.treat = TRUE, seg.var.lab = NULL){
	n		<- length(dv)
	if(is.null(seg.var.lab)){
		seg.var.lab	<- seg.var
	}
	ggtmp	<- data.frame(NULL)
	for(i in 1:n){
		tmp		<- data.frame(df)[,c("household_code", "after", "treat", seg.var, dv[i])]
		names(tmp)	<- c("household_code", "after", "treat","segment", "dv")
		if(comb.treat){
			tmp$segment	<- paste(ifelse(tmp$treat==1, "treated", "control"), tmp$segment, sep="-")
		}else{
			tmp$segment	<- factor(tmp$segment)
		}
		tmp		<- dcast(tmp, household_code + segment ~ after, value.var = "dv")
		ggtmp	<- rbind(ggtmp, cbind(tmp, Var = dv[i]))
	}

	p		<- ggplot(ggtmp, aes(before, after, col = segment, alpha = .5)) + geom_point(position = "jitter", size = .5) + 
					geom_abline(intercept = 0, slope = 1, linetype = 2, size = .5) + 
					facet_wrap(~ Var, scales = "free") +
					guides(color = guide_legend(title = seg.var.lab), alpha = FALSE)
	return(p)
}

seg.var 	<- c("heavy", "cvs_shopper", "large_cvs", "cls_wgr")
seg.var.lab	<- c("Heavy smoker", "Large CVS spender", "Large cig spending\n share at CVS", "Close to Walgreens")
dv			<- c("q", "q_othdrug", "q_othchannel", 
				"trip_cvs", "trip_othdrug", "trip_othchannel", "netdol_cvs", "netdol_othdrug", "netdol_othchannel")
				
# First check treated and control 
print(bf_af_plot(match.shtdat, "treat", dv))

# Plot the before-after plot by segment 				
pdf(paste(plot.wd, "/fg_", out.file, "bef_aft_2grp.pdf", sep=""), width = ww, height = ww)
for(i in 1:length(seg.var)){
	print(bf_af_plot(match.shtdat, seg.var[i], dv, FALSE, seg.var.lab[i]))
}
dev.off()

pdf(paste(plot.wd, "/fg_", out.file, "bef_aft_4grp.pdf", sep=""), width = ww, height = ww)
for(i in 1:length(seg.var)){
	print(bf_af_plot(match.shtdat, seg.var[i], dv, TRUE, seg.var.lab[i]))
}
dev.off()

####################
# Mining predictor # 
####################
library(randomForest)
selcol	<- c("household_code", "after", "q", "q_cvs", "q_othdrug", "q_othchannel", 
				"trip_cvs", "trip_othdrug", "trip_othchannel", "netdol_cvs", "netdol_othdrug", "netdol_othchannel")
mydat	<- match.shtdat[,selcol, with=FALSE]
setkeyv(mydat, c("household_code", "after"))
mydat	<- mydat[,list(q = diff(q), q_cvs = diff(q_cvs), q_othdrug = diff(q_othdrug), q_othchannel = diff(q_othchannel), 
								trip_cvs = diff(trip_cvs), trip_othdrug = diff(trip_othdrug), trip_othchannel = diff(trip_othchannel), 
								netdol_cvs = diff(netdol_cvs), netdol_othdrug = diff(netdol_othdrug), netdol_othchannel = diff(netdol_othchannel)
								), 
								by = list(household_code)]
selcol	<- c("treat", "distance_cvs", "distance_walgreens", "household_size", "employment", 
				"age_and_presence_of_children", "race", "income", "age", 
				"pre_q", "pre_trip_cvs", "pre_trip_othdrug","pre_trip_othchannel", "pre_trip_total", "pre_dol_cvs",                  
				# "pre_dol_othdrug", "pre_dol_othchannel", "pre_dol_total")
				"pre_netdol_othdrug", "pre_netdol_othchannel", "pre_netdol_total")
				
X		<- mypan[,selcol]
sum(is.na(X))
max(abs(mypan$household_code - mydat$household_code))
mydat	<- data.frame(mydat)

rf.ls	<- vector("list", length = length(dv))
for(i in 1:length(dv)){
	tmpdat	<- data.frame(delta = mydat[,dv[i],], X)
	rf <- randomForest(delta ~., data = tmpdat, importance=TRUE)
	print(rf)
	rf.ls[[i]]	<- rf
}

pdf(paste(plot.wd, "/fg_rf_varImp.pdf", sep=""), width = 8, height = 8*.6)
for(i in 1:length(dv)){
	print(varImpPlot(rf.ls[[i]], main = paste("Importance plot for DV - ", dv[i], sep="")))
}
#partialPlot(rf, tmpdat, "pre_trip_cvs")
dev.off()

