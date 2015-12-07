library(data.table)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(gridExtra)
library(plm)
library(lme4)
library(r2excel)
library(Synth)

setwd("~/Documents/Research/Tobacco/processed_data")
mycounty 	<- read.csv("county_treatment.csv", header = T)
ma.policy	<- read.xlsx("MA_policy.xlsx", 1)
names(ma.policy)	<- tolower(names(ma.policy))
load("sales_ma.rdata")

# Set plotting parameters
plot.wd	<- "~/Desktop"
ww		<- 6.5
ar		<- .66
make_plot	<- TRUE

############################################
# Organize data and check control counties # 
############################################
# Convert variables of date format
sales_MA			<- subset(sales_MA, channel_code != "L")		# Ignore liqure stores
sales_MA$week_end	<- as.Date(as.character(sales_MA$week_end), format = "%Y-%m-%d")
sales_MA$upcv		<- with(sales_MA, paste(upc, upc_ver_uc, sep= "-"))

mycounty	<- subset(mycounty, state == "MA")
mycounty$effect_date	<- as.Date(as.character(mycounty$effect_date), format = "%m/%d/%y")
mycounty$control_date	<- as.Date(as.character(mycounty$control_date), format = "%m/%d/%y")
mycounty$municipality	<- as.character(mycounty$municipality)
mycounty$fips_county_descr	<- as.character(mycounty$fips_county_descr)

ma.policy$municipality	<- as.character(ma.policy$municipality)
ma.policy$county		<- as.character(ma.policy$county)

# Focal municipality
mncp	<- subset(mycounty, treatment  == 1)

# Unique county sets with the earliest policy implementation date
my.pool	<- data.table(ma.policy)
setkeyv(my.pool, c("county","eff_date"))
my.pool	<- my.pool[,list(first = eff_date[1]), by = list(county)]
my.pool	<- my.pool[order(my.pool$first),]

# Check control county candidates for each focal municipality
my.window 	<- 182
treat.ctl	<- data.frame()
for(i in 1:nrow(mncp)){
	cat("-----------------------------------\n")
	cat("Focal municipality:", mncp[i,"municipality"],"\n")
	tmp.eff		<- mncp[i,"effect_date"]
	tmp.next	<- subset(ma.policy, eff_date > tmp.eff & municipality != as.character(mncp[i,"municipality"]))
	sel			<- which.min(tmp.next$eff_date)
	print(tmp.next[sel,])
	cat("Time difference with the next one =", tmp.next[sel,"eff_date"] - tmp.eff, "\n")
	
	# Select control candidates
	sel			<- my.pool$first >= tmp.eff + my.window
	cat("Control candidates:", as.character(my.pool[sel,county]),"\n")
	tmp			<- data.frame(my.pool[sel,], treatment = 0)
	tmp			<- cbind(tmp, mncp[i,c("municipality","effect_date")])
	treat.ctl	<- rbind(treat.ctl, tmp)
}
tmp		<- data.frame(municipality = mncp$municipality, effect_date = mncp$effect_date, county = mncp$fips_county_descr, treatment = 1)
treat.ctl	<- merge(treat.ctl, tmp, by = c("municipality", "effect_date", "county","treatment"), all = T)

########################
# Construct panel data #
########################
# The variables that we want to match: price, category quantity, number of stores, channel quantity
pack.size	<- 20
tmp			<- sort(table(sales_MA$brand_descr), decreasing = T)
top.brd		<- names(tmp)[1]

# Set weight for price computation
sales_MA	<- data.table(sales_MA)
setkeyv(sales_MA, c("fips_county_descr","upcv", "week_end"))
sales_MA	<- sales_MA[, upc.wt:= units[1]*size[1], by = list(upcv)]

mkt_sale	<- data.frame(NULL)
selcol		<- c("municipality", "effect_date", "fips_county_descr", "treatment", "control_date")
for(i in 1:length(mncp)){
	tmp		<- subset(treat.ctl, municipality == mncp[i,"municipality"])
	tmpdat	<- subset(sales_MA, fips_county_descr %in% tmp$county)
	tmpdat	<- tmpdat[,list(quantity	= sum(units*size)/pack.size, 
							price 		= sum(price/size * upc.wt )/sum(upc.wt) *pack.size ,
							n_store		= length(unique(store_code_uc)), 
							conv_quant	= sum(units*size*1*(channel_code == "C"))/pack.size,
							drug_quant 	= sum(units*size*1*(channel_code == "D"))/pack.size, 
							mass_quant	= sum(units*size*1*(channel_code == "M"))/pack.size, 
							food_quant	= sum(units*size*1*(channel_code == "F"))/pack.size ),
						by = list(fips_county_descr, week_end)]
	tmpdat		<- cbind(municipality = mncp[i,"municipality"], effect_date = mncp[i,"effect_date"], tmpdat)
	tmpdat		<- tmpdat[, treatment:= ifelse(fips_county_descr == mncp[i,"fips_county_descr"], 1, 0)]
	mkt_sale	<- rbind(mkt_sale, tmpdat)
}

#######################################
# Run synthetic control for each case # 
#######################################
synth.fn	<- function(focal.m, focal.county, effct_date){
	# Subset data set for focal municipality
	mydata	<- data.frame(subset(mkt_sale, municipality == focal.m & effect_date - week_end <= my.window & week_end - effect_date <= my.window))
	mydata$county_num	<- as.numeric(mydata$fips_county_descr)
	mydata$week_num		<- as.numeric(mydata$week_end)
	
	# Identify the id number of focal treatment and control 
	focal.treat		<- unique(mydata[mydata$fips_county_descr == focal.county,"county_num"])
	focal.contrl	<- setdiff(unique(mydata$county_num), focal.treat)
	cat("In the case of", focal.m, ", the number of controsl is", length(focal.contrl), ".\n")	
	tmp.time		<- as.numeric(unique(mydata$effect_date))
	tmp.prior		<- seq(min(mydata$week_num), max(mydata[mydata$week_end < mydata$effect_date, "week_num"]), 7)
	mydata$fips_county_descr	<- as.character(mydata$fips_county_descr)
	synth.data	<- dataprep(foo = mydata, predictors = c("price","quantity","n_store", "drug_quant", "food_quant"), dependent = "quantity", 
							unit.variable = "county_num", time.variable = "week_num", 
							treatment.identifier = focal.treat, controls.identifier = focal.contrl, 
							time.predictors.prior = tmp.prior, time.optimize.ssr = tmp.prior, 
							unit.names.variable = "fips_county_descr", time.plot = sort(unique(mydata$week_num)))

	syn.out		<- synth(synth.data)	
	out			<- list(data = synth.data, synout = syn.out)
	return(out)
}

placebo.test	<- function(focal.m, focal.county, effect_date){
	# Subset data for focal municipality
	mydata	<- data.frame(subset(mkt_sale, municipality == focal.m & effect_date - week_end <= my.window & week_end - effect_date <= my.window))
	mydata$county_num	<- as.numeric(mydata$fips_county_descr)
	mydata$week_num		<- as.numeric(mydata$week_end)
	
	# Identify the id number of focal treatment and control
	focal.treat		<- unique(mydata[mydata$fips_county_descr == focal.county,"county_num"])
	focal.contrl	<- setdiff(unique(mydata$county_num), focal.treat)
	allcounty		<- levels(mkt_sale$fips_county_descr)
	cat("In the case of", focal.m, ", the number of controsl is", length(focal.contrl), ".\n")	
	tmp.time	<- as.numeric(unique(mydata$effect_date))
	tmp.prior	<- seq(min(mydata$week_num), max(mydata[mydata$week_end < mydata$effect_date, "week_num"]), 7)
	mydata$fips_county_descr	<- as.character(mydata$fips_county_descr)
	
	syn.data.ls		<- setNames(vector("list", length(focal.contrl)), focal.contrl)
	syn.out.ls		<- setNames(vector("list", length(focal.contrl)), focal.contrl)
	gap.data		<- data.frame()
	for(i in 1:length(focal.contrl)){
		synth.data	<- dataprep(foo = mydata, predictors = c("price","quantity","n_store", "drug_quant", "food_quant"), dependent = "quantity", 
								unit.variable = "county_num", time.variable = "week_num", 
								treatment.identifier = focal.contrl[i], controls.identifier = focal.contrl[-i], 
								time.predictors.prior = tmp.prior, time.optimize.ssr = tmp.prior, 
								unit.names.variable = "fips_county_descr", time.plot = sort(unique(mydata$week_num)))
		syn.out		<- synth(synth.data)	
		syn.data.ls[[i]]	<- synth.data
		syn.out.ls[[i]]		<- syn.out
		
		# Compute gaps 
		gaps		<- synth.data$Y1plot - synth.data$Y0plot %*% syn.out$solution.w
		sel			<- as.Date(as.numeric(rownames(gaps)), origin = "1970-01-01") < effect_date
		pre.avg		<- mean(gaps[sel])
		gaps		<- gaps - pre.avg
		tmp			<- data.frame(PlaCounty = allcounty[focal.contrl[i]], 
								  Week = as.Date(as.numeric(rownames(gaps)), origin = "1970-01-01"), 
								  Gaps = gaps, effect_date = effect_date)	
		names(tmp)	<- c("PlaCounty", "Week", "Gaps", "effect_date")
		gap.data	<- rbind(gap.data, tmp)
	}
	return(list(data = syn.data.ls, synout = syn.out.ls, gaps = gap.data))
}

# Loop over municipality
for(i in 1:nrow(mncp)){
	cat("-----------------------------------\n")
	cat("Focal municipality:", mncp[i,"municipality"],"\n")
	synout		<- synth.fn(focal.m = mncp[i,"municipality"], focal.county = mncp[i,"fips_county_descr"], 
					effct_date=mncp[i, "effect_date"])
	testout		<- placebo.test(focal.m = mncp[i,"municipality"], focal.county = mncp[i,"fips_county_descr"], 
					effect_date=mncp[i, "effect_date"])
	
	# Print out synth out table
	syn.tab		<- synth.tab(dataprep.res = synout$data, synth.res = synout$synout)		
	print(syn.tab)
	
	# Compute gaps after setting pre-treatment gaps = 0
	gaps		<- synout$data$Y1plot - synout$data$Y0plot %*% synout$synout$solution.w
	sel			<- as.Date(as.numeric(rownames(gaps)), origin = "1970-01-01") < mncp[i,"effect_date"]
	pre.avg		<- mean(gaps[sel])
	gaps		<- gaps - pre.avg
	ggtmp		<- data.frame(Week = as.Date(as.numeric(rownames(gaps)), origin = "1970-01-01"), Gaps = gaps, 
								effect_date = mncp[i,"effect_date"])
	names(ggtmp)	<- c("Week", "Gaps", "effect_date")
	
	# Placebo gaps
	ggtmp1		<- rbind(cbind(PlaCounty = "Boston", ggtmp, placebo = 0), cbind(testout$gaps, placebo = 1))
	
	# Make plots 
	pdf(paste(plot.wd, "/graph_synth_", mncp[i,"municipality"], ".pdf", sep = ""), width = ww, height = ww*ar)
	# Path plot
	print(path.plot(dataprep.res = synout$data, synth.res = synout$synout, 
						Main = paste(mncp[i,"municipality"], "\n Path plot", sep=""), 
						tr.intake = as.numeric(mncp[i,"effect_date"]), Ylab = "Quantity", Xlab = "Week"))
	# Gap plot
	print(ggplot(ggtmp, aes(Week, Gaps)) + geom_line() + 
			geom_vline(aes(xintercept = as.numeric(effect_date)), size = .25, linetype = 2) + 
			labs(title = paste("Gap plot for ", mncp[i,"municipality"], sep=""))+ 
			theme_bw()
			)
	# Placebo gap plot
	print(ggplot(ggtmp1, aes(Week, Gaps)) + geom_line(aes(group = PlaCounty, col = factor(placebo)), alpha = .9) + 
			geom_vline(aes(xintercept = as.numeric(effect_date)), size = .25, linetype = 2) + 
			scale_colour_manual(values = c("black", "grey70")) + 
			labs(title = paste("Placebo test for ", mncp[i,"municipality"], sep="")) + 
			guides(color = FALSE) + 
			theme_bw()
			)
	dev.off()
}

