library(data.table)
library(ggplot2)
library(reshape2)
library(plm)
library(lme4)
library(lmtest)
library(stargazer)

# setwd("~/Documents/Research/Tobacco/processed_data")
# cnty		<- read.csv("supplements/county_charcs.csv")
# setwd("//tsclient/Resear1/Tobacco/processed_data")
setwd("/sscc/home/c/ccv103/Exercise")
ma.policy	<- read.csv("MA_policy_sub.csv")
tax			<- read.csv("state_cigarettes_tax.csv")
cnty		<- read.csv("county_charcs.csv")

# setwd("E:/Users/ccv103/Desktop")
load("sales_ma_ext.rdata")
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
# List the control counties in neighboring states
tmp	<- unique(subset(sales_MA, fips_state_descr != "MA"), by = c("fips_state_descr", "fips_county_descr"))
tmp[order(fips_state_descr),list(fips_state_descr, fips_county_descr)]
# outside.county	<- tmp$fips_county_descr
# Only focus on the counties that have touching borders with MA
outside.county		<- c("LITCHFIELD", "HARTFORD", "TOLLAND", "WINDHAM", 
						"CHESHIRE", "HILLSBOROUGH", "ROCKINGHAM", 
						"PROVIDENCE", "NEWPORT"
						)		

# Drop the data from the counties that don't have borders with MA
sales_MA 	<- subset(sales_MA, !(fips_state_descr != "MA" & fips_county_descr %in% setdiff(tmp$fips_county_descr, outside.county)))
sales_MA	<- sales_MA[,setdiff(names(sales_MA), c("upc_descr", "brand_descr")), with = FALSE]

# Convert variables of date format
ma.policy$eff_date	<- as.Date(as.character(ma.policy$eff_date), format = "%Y-%m-%d")
ma.policy$county	<- as.character(ma.policy$county)
ma.policy$ctr_county	<- as.character(ma.policy$ctr_county)
ma.policy$municipality	<- factor(ma.policy$municipality, 
							levels = c("Boston", "Worcester", "New Bedford,No.Attleboro", "Springfield", "Pittsfield,Lee,Lenox,Stockbridge"), 
									labels = c("Boston", "Worcester", "NB.NA.", "Springfield", "PLLS"))
tax$state			<- as.character(tax$state)
tax$start_date		<- as.Date(as.character(tax$start_date), format = "%m/%d/%y")									
tax$end_date		<- as.Date(as.character(tax$end_date), format = "%m/%d/%y")

sales_MA			<- subset(sales_MA, channel_code != "L")		# Ignore liqure stores
sales_MA$week_end	<- as.Date(as.character(sales_MA$week_end), format = "%Y-%m-%d")
sales_MA$upcv		<- with(sales_MA, paste(upc, upc_ver_uc, sep= "-"))
pack.size			<- 20
my.wind				<- 182		 # We restrict to 6 months around the event 

# Fix UPC weight
setkeyv(sales_MA, c("fips_county_descr","upcv", "week_end"))
sales_MA	<- sales_MA[, upc.wt:= units[1]*size[1], by = list(fips_county_descr, upcv)]

# Organize tax table
taxtbl			<- unique(sales_MA, by = c("fips_state_descr", "week_end"))[,list(fips_state_descr, week_end)]
taxtbl			<- data.frame(subset(taxtbl, fips_state_descr != ""))
taxtbl$tax		<- NA
sel.state	<- unique(tax$state)
for(i in 1:length(sel.state)){
	sel1	<- which(tax$state == sel.state[i])
	for(j in sel1){
		sel2	<- taxtbl$fips_state_descr == sel.state[i] & 
					taxtbl$week_end >= tax[j, "start_date"] & taxtbl$week_end <= tax[j, "end_date"]
		if(sum(sel2) > 0){
			taxtbl[sel2, "tax"]	<- tax[j,"cigar_tax"]
		}
	}
}
taxtbl		<- data.table(taxtbl, key = c("fips_state_descr", "week_end"))

# County-level characteristics
names(cnty)	<- c("fips_state_descr", "fips_county_descr", "pop")
cnty		<- data.table(cnty, key = c("fips_state_descr", "fips_county_descr"))
cnty		<- cnty[, pop:= pop/1000000]

# --------------------------------- #
# Quantity at the county-week level #
tmp_data	<- sales_MA[,list(quantity = sum(units*size)/pack.size, 
							price = sum(price/size * upc.wt )/sum(upc.wt) *pack.size ),
					by = list(fips_state_descr, fips_county_descr, week_end)]
setkeyv(tmp_data, c("fips_county_descr", "week_end"))					
for(i in 1:nrow(ma.policy)){
	tmp.ctl	<- unlist(strsplit(as.character(ma.policy[i,"ctr_county"]), ","))
	tmp		<- subset(tmp_data, fips_county_descr %in% c(ma.policy[i,"county"], tmp.ctl, outside.county) & 
								abs(week_end - ma.policy[i,"eff_date"]) < my.wind)
	tmp		<- tmp[, ':='(municipality = ma.policy[i,"municipality"] , effect_date = ma.policy[i,"eff_date"],
						treatment = ifelse(fips_county_descr == ma.policy[i,"county"], 1, 0), 
						 retail_aff = ifelse(fips_county_descr == ma.policy[i,"county"], ma.policy[i,"retail_aff"], 0)) ]
	if(i == 1){
		mkt_data	<- tmp
	}else{
		mkt_data	<- unique(rbind(mkt_data, tmp), by = c("fips_county_descr", "week_end"))
	}
}
# Check duplicates 
sum(duplicated(mkt_data, by = c("fips_county_descr", "week_end")))
mkt_data 	<- merge(mkt_data, taxtbl, by = c("fips_state_descr", "week_end"), all.x = T )
mkt_data	<- merge(mkt_data, cnty, by= c("fips_state_descr", "fips_county_descr"), all.x = T)

#-------------------------------------- #
# Quantity at county-channel-week level #
tmp_data	<- sales_MA[,list(quantity = sum(units*size)/pack.size, 
						price = sum(price/size * upc.wt )/sum(upc.wt) *pack.size ),
					by = list(fips_state_descr, fips_county_descr, channel_code, week_end)]
setkeyv(tmp_data, c("fips_county_descr", "channel_code", "week_end"))					
for(i in 1:nrow(ma.policy)){
	tmp.ctl	<- unlist(strsplit(as.character(ma.policy[i,"ctr_county"]), ","))
	tmp		<- subset(tmp_data, fips_county_descr %in% c(ma.policy[i,"county"], tmp.ctl, outside.county) & 
								abs(week_end - ma.policy[i,"eff_date"]) < my.wind)
	tmp		<- tmp[, ':='(municipality = ma.policy[i,"municipality"], effect_date = ma.policy[i,"eff_date"], 
						treatment = ifelse(fips_county_descr == ma.policy[i,"county"], 1, 0),
						retail_aff = ifelse(fips_county_descr == ma.policy[i,"county"], ma.policy[i,"retail_aff"], 0) ) ]
	if(i == 1){
		channel_data	<- tmp
	}else{
		channel_data	<- unique(rbind(channel_data, tmp), by = c("fips_county_descr", "channel_code", "week_end"))
	}
}
# Check duplicates 
sum(duplicated(channel_data, by = c("fips_county_descr", "channel_code", "week_end")))
channel_data 	<- merge(channel_data, taxtbl, by = c("fips_state_descr", "week_end"), all.x = T )
channel_data	<- merge(channel_data, cnty, by= c("fips_state_descr", "fips_county_descr"), all.x = T)

#------------ #
# Price data 
# NOTE: we could use store-weight to compute average price
tmp_data	<- sales_MA[,list(price = sum(price *units)/sum(units*size) * pack.size ),
					by = list(fips_state_descr, fips_county_descr, week_end, upcv)]
summary(tmp_data$price)					
quantile(tmp_data$price, c(.01, .99))

# Drop some unreasonable observations. 
tmp_data	<- subset(tmp_data, price >= 2 & price <= 10)

setkeyv(tmp_data, c("fips_county_descr", "week_end", "upcv"))					
for(i in 1:nrow(ma.policy)){
	tmp.ctl	<- unlist(strsplit(as.character(ma.policy[i,"ctr_county"]), ","))
	tmp		<- subset(tmp_data, fips_county_descr %in% c(ma.policy[i,"county"], tmp.ctl, outside.county) & 
								abs(week_end - ma.policy[i,"eff_date"]) < my.wind)
	tmp		<- tmp[, ':='(municipality = ma.policy[i,"municipality"], effect_date = ma.policy[i,"eff_date"], 
						treatment = ifelse(fips_county_descr == ma.policy[i,"county"], 1, 0),
						retail_aff = ifelse(fips_county_descr == ma.policy[i,"county"], ma.policy[i,"retail_aff"], 0) ) ]
	if(i == 1){
		price_data	<- tmp
	}else{
		price_data	<- unique(rbind(price_data, tmp), by = c("fips_county_descr", "week_end", "upcv"))
	}
}
# Check duplicates 
sum(duplicated(price_data, by = c("fips_county_descr", "week_end", "upcv")))
price_data 	<- merge(price_data, taxtbl, by = c("fips_state_descr", "week_end"), all.x = T )
price_data	<- merge(price_data, cnty, by= c("fips_state_descr", "fips_county_descr"), all.x = T)

rm(list = grep("tmp",ls(), value = TRUE))

###########################
# Regressions of quantity #
###########################
mydata	<- mkt_data
mydata	<- mydata[,':='(post = 1*(week_end >= effect_date))]
mydata	<- mydata[,':='(post_treat = treatment * post, retail_aff = ifelse(post==1, retail_aff, 0))]
mydata	<- pdata.frame(mydata, index = c("fips_county_descr", "week_end"))

mkt.reg			<- setNames(vector("list", 6), c("DID", "HET_DID", "DID_WEEKFE", "HET_DID_WEEKFE", "DID_MIX", "DID_MIX_WEEK"))
mkt.reg[[1]]	<- plm(log(quantity) ~ retail_aff + treatment + post + tax + pop, data = mydata, model = "pooling" )
mkt.reg[[2]]	<- plm(log(quantity) ~ municipality:retail_aff + municipality:treatment + municipality:post + tax + pop, data = mydata, model = "pooling" )
mkt.reg[[3]]	<- plm(log(quantity) ~ retail_aff + treatment + tax, data = mydata, effect = "time", model = "within") 
mkt.reg[[4]]	<- plm(log(quantity) ~ municipality:retail_aff + municipality:treatment + tax + pop, data = mydata, effect = "time", model = "within") 
mkt.reg[[5]]	<- lmer(log(quantity) ~ retail_aff + treatment + post + tax + pop + (1 + treatment + post + retail_aff|municipality), data = mydata )
mkt.reg[[6]]	<- lmer(log(quantity) ~ retail_aff + treatment + tax + pop + (1 + treatment + retail_aff|municipality) + (1|week_end), data = mydata )

# Compute cluster standard error
cls.se		<- lapply(mkt.reg[1:4], function(x) Cls.se.fn(x, cluster.vec = mydata$municp_county, est.table = FALSE)$se)
												 						
# Organize regression results
stargazer(mkt.reg, type = "html", align = TRUE, title = "Category quantity sales DID regressions at county-week level", 
		keep = c("retail_aff", "tax"), 
		covariate.labels = c("AffectRetail", "Tax", paste("AffectRetail*", as.character(ma.policy$municipality), sep ="") ),
		se	= c(cls.se, NULL, NULL), 
		omit.stat = c("f","ll"), no.space = TRUE, 
		notes = "First 4 models report clusteredse.",
		add.lines = list(c("Week","","","FE","FE","","RC"), c("Municipality","","","","","RC","RC")), 
		out = paste(plot.wd, "/tobacco_ma_rms_mkt_quant_retail.html", sep=""))

#-------------------------------#
# Market-channel level quantity #
mydata 	<- channel_data
mydata	<- mydata[,':='(post = 1*(week_end >= effect_date), county_channel = paste(fips_county_descr, channel_code, sep="*"))]
mydata	<- mydata[,':='(post_treat = treatment * post, retail_aff = ifelse(post==1, retail_aff, 0))]
mydata	<- mydata[,quantity := quantity/1000]			# Rescale quantity level 
mydata	<- pdata.frame(mydata, c("county_channel", "week_end"))

channel.reg			<- setNames(vector("list", 6), c("DID", "HET_DID", "DID_WEEKFE", "HET_DID_WEEKFE", "DID_MIX", "DID_MIX_WEEK"))
channel.reg[[1]]	<- plm(quantity ~ retail_aff + treatment + post + tax + pop + channel_code, data = mydata, model = "pooling" )
channel.reg[[2]]	<- plm(quantity ~ channel_code:retail_aff + channel_code:treatment + channel_code:post + tax + pop, data = mydata, model = "pooling" )
channel.reg[[3]]	<- plm(quantity ~ retail_aff + treatment + tax + pop + channel_code, data = mydata, effect = "time", model = "within") 
channel.reg[[4]]	<- plm(quantity ~ channel_code:retail_aff + channel_code:treatment + tax + pop + channel_code, data = mydata, effect = "time", model = "within") 
channel.reg[[5]]	<- lmer(quantity ~ retail_aff + treatment + post + tax + pop + (1 + treatment + post + retail_aff|channel_code), data = mydata )
channel.reg[[6]]	<- lmer(quantity ~ retail_aff + treatment + tax + pop + (1 + treatment + retail_aff|channel_code) + (1|week_end), data = mydata )

# Compute cluster standard error
cls.se		<- lapply(channel.reg[1:4], function(x) Cls.se.fn(x, cluster.vec = mydata$municp_county_channel, est.table = FALSE)$se)

# Organize regression results
stargazer(channel.reg, type = "html", align = TRUE, title = "Category quantity sales DID regressions at county-channel-week level", 
		keep = c("retail_aff", "tax"), 
		covariate.labels = c("AffectRetail", "Tax", paste("AffectRetail*", c("Convenience", "Drug", "Food", "Mass"), sep="") ),
		se	= c(cls.se, NULL, NULL), 
		omit.stat = c("f","ll"), no.space = TRUE, 
		notes = "First 4 models report clusteredse.",
		add.lines = list(c("Week","","","FE","FE","","RC"), c("Municipality","","","","","RC","RC")), 
		out = paste(plot.wd, "/tobacco_ma_rms_channel_quant_retail.html", sep=""))

#---------------------------- #
# Non-pharmacy level quantity #
tmp_data	<- sales_MA[pharmacy2 == 0,list(quantity = sum(units*size)/pack.size, 
							price = sum(price/size * upc.wt )/sum(upc.wt) *pack.size ),
					by = list(fips_state_descr, fips_county_descr, week_end)]
setkeyv(tmp_data, c("fips_county_descr", "week_end"))					
for(i in 1:nrow(ma.policy)){
	tmp.ctl	<- unlist(strsplit(as.character(ma.policy[i,"ctr_county"]), ","))
	tmp		<- subset(tmp_data, fips_county_descr %in% c(ma.policy[i,"county"], tmp.ctl, outside.county) & 
								abs(week_end - ma.policy[i,"eff_date"]) < my.wind)
	tmp		<- tmp[, ':='(municipality = ma.policy[i,"municipality"] , effect_date = ma.policy[i,"eff_date"],
						treatment = ifelse(fips_county_descr == ma.policy[i,"county"], 1, 0), 
						 retail_aff = ifelse(fips_county_descr == ma.policy[i,"county"], ma.policy[i,"retail_aff"], 0)) ]
	if(i == 1){
		npharm.data	<- tmp
	}else{
		npharm.data	<- unique(rbind(npharm.data, tmp), by = c("fips_county_descr", "week_end"))
	}
}
# Check duplicates 
sum(duplicated(npharm.data, by = c("fips_county_descr", "week_end")))
npharm.data	<- merge(npharm.data, taxtbl, by = c("fips_state_descr", "week_end"), all.x = T )
npharm.data	<- merge(npharm.data, cnty, by= c("fips_state_descr", "fips_county_descr"), all.x = T)

mydata		<- npharm.data
mydata		<- mydata[,':='(post = 1*(week_end >= effect_date))]
mydata		<- mydata[,':='(post_treat = treatment * post, retail_aff = ifelse(post==1, retail_aff, 0))]
mydata		<- pdata.frame(mydata, index = c("fips_county_descr", "week_end"))

npharm.reg			<- setNames(vector("list", 6), c("DID", "HET_DID", "DID_WEEKFE", "HET_DID_WEEKFE","DID_MIX", "DID_MIX_WEEK"))
npharm.reg[[1]]	<- plm(log(quantity) ~ retail_aff + treatment + post+ tax + pop, data = mydata, model = "pooling" )
npharm.reg[[2]]	<- plm(log(quantity) ~ municipality:retail_aff + municipality:treatment + municipality:post + tax + pop, data = mydata, model = "pooling" )
npharm.reg[[3]]	<- plm(log(quantity) ~ retail_aff + treatment + tax + pop, data = mydata, effect = "time", model = "within") 
npharm.reg[[4]]	<- plm(log(quantity) ~ municipality:retail_aff + municipality:treatment + tax + pop, data = mydata, effect = "time", model = "within")
npharm.reg[[5]]	<- lmer(log(quantity) ~ retail_aff + treatment + post + tax + pop+ (1 + treatment + post + retail_aff|municipality), data = mydata )
npharm.reg[[6]]	<- lmer(log(quantity) ~ retail_aff + treatment + tax + pop+ (1 + treatment + retail_aff|municipality) + (1|week_end), data = mydata )

# Compute cluster standard error
cls.se		<- lapply(npharm.reg[1:4], function(x) Cls.se.fn(x, cluster.vec = mydata$municp_county, est.table = FALSE)$se)

# Organize regression results
stargazer(npharm.reg, type = "html", align = TRUE, title = "Non-pharmacy quantity sales DID regressions at county-week level", 
		keep = c("retail_aff", "tax"), 
		covariate.labels = c("AffectRetail", "Tax", paste("AffectRetail*", as.character(ma.policy$municipality), sep="")),
		se	= c(cls.se, NULL, NULL), 
		omit.stat = c("f","ll"), no.space = TRUE, 
		notes = "First 4 report clusteredse.",
		add.lines = list(c("Week","","","FE","FE","","RC"), c("Municipality","","","","","RC","RC")), 
		out = paste(plot.wd, "/tobacco_ma_rms_nonpharm_quant_retail.html", sep=""))

####################
# Price regression # 
####################
mydata		<- price_data
mydata		<- mydata[,':='(county_week = paste(fips_county_descr, week_end, sep = "*"), post = 1*(week_end >= effect_date))]
mydata		<- mydata[,':='(post_treat = treatment * post, retail_aff = ifelse(post==1, retail_aff, 0))]
mydata		<- pdata.frame(mydata, index = c("upcv", "county_week"))

price.reg			<- setNames(vector("list", 6), c("DID", "HET_DID", "DID_WEEKFE", "HET_DID_WEEKFE", "DID_MIX", "DID_MIX_WEEK"))
price.reg[[1]]	<- plm(log(price) ~ retail_aff + treatment + post+ tax + pop, data = mydata, model = "within")
price.reg[[2]]	<- plm(log(price) ~ municipality:retail_aff + municipality:treatment + municipality:post+ tax + pop, 
						data = mydata, model = "within" )
price.reg[[3]]	<- plm(log(price) ~ retail_aff + treatment + tax + pop+ factor(week_end), data = mydata, model = "within") 
price.reg[[4]]	<- plm(log(price) ~ municipality:retail_aff + municipality:treatment + tax + pop+ factor(week_end), data = mydata, model = "within") 
price.reg[[5]]	<- lmer(log(price) ~ retail_aff + treatment + post + tax + pop+ (1 + treatment + retail_aff|municipality) + (1|upcv), 
						data = mydata )
price.reg[[6]]	<- lmer(log(price) ~ retail_aff + treatment + tax + pop+ (1 + treatment +retail_aff|municipality) + (1|week_end) + (1|upcv), 
						data = mydata )

# Compute cluster standard error
cls.se		<- lapply(price.reg[1:4], function(x) Cls.se.fn(x, cluster.vec = mydata$upcv, est.table = FALSE)$se)

# Organize regression results
stargazer(price.reg, type = "html", align = TRUE, title = "Price DID regressions at upc-county-week level", 
		keep = c("retail_aff", "tax"), 
		covariate.labels = c("AffectRetail", "Tax", paste("AffectRetail*", as.character(ma.policy$municipality), sep="")),
		se	= c(cls.se, NULL, NULL), 
		omit.stat = c("f","ll"), no.space = TRUE, 
		notes = "First 4 report clustered se.",
		add.lines = list(c("Week","","","FE","FE","","RC"), c("Municipality","","","","","RC","RC"), 
						 c("UPC","FE","FE","FE","FE","RE","RE")), 
		out = paste(plot.wd, "/tobacco_ma_rms_price_retail.html", sep=""))



