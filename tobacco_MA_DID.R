library(data.table)
library(ggplot2)
library(reshape2)
library(plm)
library(lme4)
library(r2excel)
library(xlsx)
library(lmtest)
library(stargazer)

# setwd("~/Documents/Research/Tobacco/processed_data")
setwd("//tsclient/Resear1/Tobacco/processed_data")
mycounty 	<- read.csv("county_treatment.csv", header = T)
store.pharm	<- read.csv("pharmacy_store.csv", header = T)
ma.policy	<- read.xlsx("MA_policy.xlsx", 1)

setwd("E:/Users/ccv103/Desktop")
load("sales_ma.rdata")
plot.wd	<- getwd()

# Set plotting parameters
# plot.wd	<- "~/Desktop"
ww		<- 12
ar		<- .66
make_plot	<- FALSE

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
# Convert variables of date format
sales_MA			<- subset(sales_MA, channel_code != "L")		# Ignore liqure stores
sales_MA$week_end	<- as.Date(as.character(sales_MA$week_end), format = "%Y-%m-%d")
sales_MA$upcv		<- with(sales_MA, paste(upc, upc_ver_uc, sep= "-"))
mycounty	<- subset(mycounty, state == "MA")
mycounty$effect_date	<- as.Date(as.character(mycounty$effect_date), format = "%m/%d/%y")
mycounty$control_date	<- as.Date(as.character(mycounty$control_date), format = "%m/%d/%y")

# Market-level aggregation
pack.size	<- 20
tmp			<- sort(table(sales_MA$brand_descr), decreasing = T)
top.brd		<- names(tmp)[1]

# Organize market-level data 
table(sales_MA$channel_code)/nrow(sales_MA)				# Examine the frequence of channel 
tmp			<- subset(mycounty, treatment == 1)
mncp 		<- tmp$municipality							# Unique cases
event_date 	<- tmp$effect_date							# Event date

# --------------------------------- #
# Quantity at the county-week level #
mkt_sale	<- data.frame(NULL)
selcol		<- c("municipality", "effect_date", "fips_county_descr", "treatment", "control_date")
for(i in 1:length(mncp)){
	tmp		<- as.character(mycounty[mycounty$municipality == mncp[i], "fips_county_descr"])
	tmpdat	<- subset(sales_MA, fips_county_descr %in% tmp)
	tmpdat	<- data.table(tmpdat)
	tmpdat	<- tmpdat[,list(quantity = sum(units*size)/pack.size, 
							price = sum(price * units )/sum(units * size) *pack.size, 
							top.share = sum(units*size*1*(brand_descr == top.brd))/sum(units*size) ),
						by = list(fips_county_descr, week_end)]
	tmpdat	<- merge(data.frame(tmpdat), mycounty[mycounty$municipality == mncp[i], selcol], by = "fips_county_descr", all.x = T)					
	mkt_sale	<- rbind(mkt_sale, tmpdat)
}
mkt_sale$treatment 	<- factor(mkt_sale$treatment, levels = c(1, 0))

#-------------------------------------- #
# Quantity at county-channel-week level #
channel_data	<- data.frame(NULL)
selcol		<- c("municipality", "effect_date", "fips_county_descr", "treatment", "control_date")
for(i in 1:length(mncp)){
	tmp		<- as.character(mycounty[mycounty$municipality == mncp[i], "fips_county_descr"])
	tmpdat	<- subset(sales_MA, fips_county_descr %in% tmp)		
	tmpdat	<- data.table(tmpdat)
	tmpdat	<- tmpdat[,list(quantity = sum(units*size)/pack.size, 
							price = sum(price * units )/sum(units * size) *pack.size),
						by = list(fips_county_descr, channel_code, week_end)]
	tmpdat	<- merge(data.frame(tmpdat), mycounty[mycounty$municipality == mncp[i], selcol], by = "fips_county_descr", all.x = T)					
	channel_data	<- rbind(channel_data, tmpdat)
}
channel_data$municp_county_channel	<- with(channel_data, paste(municipality, fips_county_descr, channel_code, sep ="-"))
channel_data$post	<- 1 * (channel_data$week_end > channel_data$effect_date)

#------------ #
# Price data 
price.data	<- data.frame(NULL)
selcol		<- c("municipality", "effect_date", "fips_county_descr", "treatment", "control_date")
for(i in 1:length(mncp)){
	tmp		<- as.character(mycounty[mycounty$municipality == mncp[i], "fips_county_descr"])
	tmpdat	<- subset(sales_MA, fips_county_descr %in% tmp)
	tmpdat	<- data.table(tmpdat)
	tmpdat	<- tmpdat[,list(price = mean(price) ),
						by = list(fips_county_descr, week_end, upcv)]
	tmpdat	<- merge(data.frame(tmpdat), mycounty[mycounty$municipality == mncp[i], selcol], by = "fips_county_descr", all.x = T)					
	price.data	<- rbind(price.data, tmpdat)
}

###########################
# Regressions of quantity #
###########################
my.wind			<- 182		 # We restrict to 6 months around the event 

#-----------------------#
# Markte-level quantity #
mkt_sale$post	<- 1 * (mkt_sale$week_end > mkt_sale$effect_date)
mkt_sale$municp_county	<- with(mkt_sale, paste(municipality, fips_county_descr, sep="-"))
mkt_sale$treatment	<- as.numeric(as.character(mkt_sale$treatment))
mydata			<- subset(mkt_sale, abs(week_end - effect_date) <= my.wind)
mydata$post_treat <- mydata$treatment * mydata$post
mydata$municipality	<- factor(mydata$municipality, levels = as.character(mncp))
mydata			<- pdata.frame(mydata, index = c("municp_county", "week_end"))

mkt.reg			<- setNames(vector("list", 6), c("DID", "HET_DID", "DID_WEEKFE", "HET_DID_WEEKFE", "DID_MIX", "DID_MIX_WEEK"))
mkt.reg[[1]]	<- plm(log(quantity) ~ post_treat + treatment + post, data = mydata, model = "pooling" )
mkt.reg[[2]]	<- plm(log(quantity) ~ municipality:post_treat + municipality:treatment + municipality:post, data = mydata, model = "pooling" )
mkt.reg[[3]]	<- plm(log(quantity) ~ post_treat + treatment, data = mydata, effect = "time", model = "within") 
mkt.reg[[4]]	<- update(mkt.reg[[3]], ~ municipality:post_treat + municipality:treatment)
mkt.reg[[5]]	<- lmer(log(quantity) ~ post_treat + treatment + post + (0 + treatment + post + post_treat|municipality), data = mydata )
mkt.reg[[6]]	<- lmer(log(quantity) ~ post_treat + treatment + (0 + treatment +post_treat|municipality) + (1|week_end), data = mydata )

# Compute cluster standard error
cls.se		<- lapply(mkt.reg[1:4], function(x) Cls.se.fn(x, cluster.vec = mydata$municp_county, est.table = FALSE)$se)
												 						
# Organize regression results
stargazer(mkt.reg, type = "html", align = TRUE, title = "Category quantity sales DID regressions at county-week level", 
		keep = c("post_treat"), 
		covariate.labels = c("Treatment*Post", as.character(mncp)),
		se	= c(cls.se, NULL, NULL), 
		omit.stat = c("f","ll"), no.space = TRUE, 
		notes = "First 4 report clusted se.",
		add.lines = list(c("Week","","","FE","FE","","RE"), c("Municipality","","","","","RC","RC")), 
		out = paste(plot.wd, "/tobacco_ma_rms_mkt_quant.html", sep=""))

#-------------------------------#
# Market-channel level quantity #
mydata 		<- subset(channel_data, abs(week_end - effect_date) <= my.wind)
mydata$post_treat <- mydata$treatment * mydata$post
mydata		<- pdata.frame(mydata, c("municp_county_channel", "week_end"))

channel.reg			<- setNames(vector("list", 6), c("DID", "HET_DID", "DID_WEEKFE", "HET_DID_WEEKFE", "DID_MIX", "DID_MIX_WEEK"))
channel.reg[[1]]	<- plm(log(quantity) ~ post_treat + treatment + post + channel_code, data = mydata, model = "pooling" )
channel.reg[[2]]	<- plm(log(quantity) ~ channel_code:post_treat + channel_code:treatment + channel_code:post , data = mydata, model = "pooling" )
channel.reg[[3]]	<- plm(log(quantity) ~ post_treat + treatment + channel_code, data = mydata, effect = "time", model = "within") 
channel.reg[[4]]	<- update(channel.reg[[3]], ~ channel_code:post_treat + channel_code:treatment + channel_code)
channel.reg[[5]]	<- lmer(log(quantity) ~ post_treat + treatment + post + (0 + treatment + post + post_treat|channel_code), data = mydata )
channel.reg[[6]]	<- lmer(log(quantity) ~ post_treat + treatment + (0 + treatment +post_treat|channel_code) + (1|week_end), data = mydata )

# Compute cluster standard error
cls.se		<- lapply(channel.reg[1:4], function(x) Cls.se.fn(x, cluster.vec = mydata$municp_county_channel, est.table = FALSE)$se)
												 						
# Organize regression results
stargazer(channel.reg, type = "html", align = TRUE, title = "Category quantity sales DID regressions at county-channel-week level", 
		keep = c("post_treat"), 
		covariate.labels = c("Treatment*Post", "Convenience", "Drug", "Food", "Mass"),
		se	= c(cls.se, NULL, NULL), 
		omit.stat = c("f","ll"), no.space = TRUE, 
		notes = "First 4 report clusted se.",
		add.lines = list(c("Week","","","FE","FE","","RE"), c("Municipality","","","","","RC","RC")), 
		out = paste(plot.wd, "/tobacco_ma_rms_channel_quant.html", sep=""))

#---------------------------- #
# Non-pharmacy level quantity #
npharm_data	<- data.frame(NULL)
selcol		<- c("municipality", "effect_date", "fips_county_descr", "treatment", "control_date")
for(i in 1:length(mncp)){
	tmp		<- as.character(mycounty[mycounty$municipality == mncp[i], "fips_county_descr"])
	tmpdat	<- subset(sales_MA, fips_county_descr %in% tmp & pharmacy == 0)		
	tmpdat	<- data.table(tmpdat)
	tmpdat	<- tmpdat[,list(quantity = sum(units*size)/pack.size, 
							price = sum(price * units )/sum(units * size) *pack.size),
						by = list(fips_county_descr, week_end)]
	tmpdat	<- merge(data.frame(tmpdat), mycounty[mycounty$municipality == mncp[i], selcol], by = "fips_county_descr", all.x = T)					
	npharm_data	<- rbind(npharm_data, tmpdat)
}
npharm_data$municp_county	<- with(npharm_data, paste(municipality, fips_county_descr, sep ="-"))
npharm_data$post	<- 1 * (npharm_data$week_end > npharm_data$effect_date)
mydata				<- subset(npharm_data, abs(week_end - effect_date) <= my.wind)
mydata$post_treat 	<- mydata$treatment * mydata$post
mydata$municipality	<- factor(mydata$municipality, levels = as.character(mncp))
mydata				<- pdata.frame(mydata, index = c("municp_county","week_end"))

npharm.reg			<- setNames(vector("list", 6), c("DID", "HET_DID", "DID_WEEKFE", "HET_DID_WEEKFE","DID_MIX", "DID_MIX_WEEK"))
npharm.reg[[1]]	<- plm(log(quantity) ~ post_treat + treatment + post, data = mydata, model = "pooling" )
npharm.reg[[2]]	<- plm(log(quantity) ~ municipality:post_treat + municipality:treatment + municipality:post, data = mydata, model = "pooling" )
npharm.reg[[3]]	<- plm(log(quantity) ~ post_treat + treatment, data = mydata, effect = "time", model = "within") 
npharm.reg[[4]]	<- update(npharm.reg[[3]], ~ municipality:post_treat + municipality:treatment)
npharm.reg[[5]]	<- lmer(log(quantity) ~ post_treat + treatment + post + (0 + treatment + post + post_treat|municipality), data = mydata )
npharm.reg[[6]]	<- lmer(log(quantity) ~ post_treat + treatment + (0 + treatment +post_treat|municipality) + (1|week_end), data = mydata )

# Compute cluster standard error
cls.se		<- lapply(npharm.reg[1:4], function(x) Cls.se.fn(x, cluster.vec = mydata$municp_county, est.table = FALSE)$se)
												 						
# Organize regression results
stargazer(npharm.reg, type = "html", align = TRUE, title = "Non-pharmacy quantity sales DID regressions at county-week level", 
		keep = c("post_treat"), 
		covariate.labels = c("Treatment*Post", as.character(mncp)),
		se	= c(cls.se, NULL, NULL), 
		omit.stat = c("f","ll"), no.space = TRUE, 
		notes = "First 4 report clusted se.",
		add.lines = list(c("Week","","","FE","FE","","RE"), c("Municipality","","","","","RC","RC")), 
		out = paste(plot.wd, "/tobacco_ma_rms_nonpharm_quant.html", sep=""))

#####################
# Price regressions # 
#####################
#-----------------------#
# Markte-level quantity #
mydata				<- subset(price.data, abs(week_end - effect_date) <= my.wind)
mydata$post			<- 1 * (mydata$week_end > mydata$effect_date)
mydata$municp_county<- with(mydata, paste(municipality, fips_county_descr, sep="*"))
mydata$treatment	<- as.numeric(as.character(mydata$treatment))
mydata$post_treat 	<- mydata$treatment * mydata$post
mydata$municp_county_week	<- with(mydata, paste(municp_county, week_end, sep="*"))
mydata$municipality	<- factor(mydata$municipality, levels = as.character(mncp))
mydata				<- pdata.frame(mydata, index = c("upcv", "municp_county_week"))

price.reg			<- setNames(vector("list", 6), c("DID", "HET_DID", "DID_WEEKFE", "HET_DID_WEEKFE", "DID_MIX", "DID_MIX_WEEK"))
price.reg[[1]]	<- plm(log(price) ~ post_treat + treatment + post, data = mydata, model = "within")
price.reg[[2]]	<- plm(log(price) ~ municipality:post_treat + municipality:treatment + municipality:post, 
						data = mydata, model = "within" )
price.reg[[3]]	<- plm(log(price) ~ post_treat + treatment + factor(week_end), data = mydata, model = "within") 
price.reg[[4]]	<- update(price.reg[[3]], ~ municipality:post_treat + municipality:treatment)
price.reg[[5]]	<- lmer(log(price) ~ post_treat + treatment + post + (0 + treatment + post_treat|municipality) + (1|upcv), 
						data = mydata )
price.reg[[6]]	<- lmer(log(price) ~ post_treat + treatment + (0 + treatment +post_treat|municipality) + (1|week_end) + (1|upcv), 
						data = mydata )

# Compute cluster standard error
cls.se		<- lapply(price.reg[1:4], function(x) Cls.se.fn(x, cluster.vec = mydata$upcv, est.table = FALSE)$se)

# Organize regression results
stargazer(price.reg, type = "html", align = TRUE, title = "Price DID regressions at upc-county-week level", 
		keep = c("post_treat"), 
		covariate.labels = c("Treatment*Post", as.character(mncp)),
		se	= c(cls.se, NULL, NULL), 
		omit.stat = c("f","ll"), no.space = TRUE, 
		notes = "First 4 report clusted se.",
		add.lines = list(c("Week","","","FE","FE","","RE"), c("Municipality","","","","","RC","RC"), 
						 c("UPC","FE","FE","FE","FE","RE","RE")), 
		out = paste(plot.wd, "/tobacco_ma_rms_price.html", sep=""))

