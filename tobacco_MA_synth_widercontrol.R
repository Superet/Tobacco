library(data.table)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(gridExtra)
library(Synth)

# setwd("~/Documents/Research/Tobacco/processed_data")
# cnty		<- read.csv("supplements/county_charcs.csv")
setwd("//tsclient/Resear1/Tobacco/processed_data")
ma.policy	<- read.csv("MA_policy_sub.csv")
tax			<- read.csv("state_cigarettes_tax.csv")
cnty		<- read.csv("county_charcs.csv")

setwd("E:/Users/ccv103/Desktop")
load("sales_ma_ext.rdata")
plot.wd	<- getwd()

# Set plotting parameters
# plot.wd	<- "~/Desktop"
ww		<- 12
ar		<- .66
make_plot	<- FALSE

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

########################
# Construct panel data #
########################
# The variables that we want to match: price, category quantity, number of stores, channel quantity
pack.size	<- 20
my.window	<- 182		 # We restrict to 6 months around the event

tmp_data	<- sales_MA[,list(quantity = sum(units*size)/pack.size, 
							price = sum(price/size * upc.wt )/sum(upc.wt) *pack.size, 
							n_store		= length(unique(store_code_uc)), 
							conv_quant	= sum(units*size*1*(channel_code == "C"))/pack.size,
							drug_quant 	= sum(units*size*1*(channel_code == "D"))/pack.size, 
							mass_quant	= sum(units*size*1*(channel_code == "M"))/pack.size, 
							food_quant	= sum(units*size*1*(channel_code == "F"))/pack.size ),
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
		mkt_sale	<- tmp
	}else{
		mkt_sale	<- rbind(mkt_data, tmp)
	}
}
sum(duplicated(mkt_sale, by = c("fips_county_descr", "week_end")))
mkt_sale 	<- merge(mkt_sale, taxtbl, by = c("fips_state_descr", "week_end"), all.x = T )
mkt_sale	<- merge(mkt_sale, cnty, by= c("fips_state_descr", "fips_county_descr"), all.x = T)

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
	synth.data	<- dataprep(foo = mydata, 
							predictors = c("price","quantity","n_store", "pop", "tax", "drug_quant", "food_quant", "mass_quant"), 
							dependent = "quantity", 
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
		synth.data	<- dataprep(foo = mydata, 
								predictors = c("price","quantity","n_store", "pop", "tax", "drug_quant", "food_quant", "mass_quant"), 
								dependent = "quantity", 
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
plots	<- list(NULL)
idx		<- 1
for(i in 1:nrow(ma.policy)){
	cat("-----------------------------------\n")
	cat("Focal municipality:", ma.policy[i,"municipality"],"\n")
	synout		<- synth.fn(focal.m = ma.policy[i,"municipality"], focal.county = ma.policy[i,"county"], 
					effct_date=ma.policy[i, "eff_date"])
	testout		<- placebo.test(focal.m = ma.policy[i,"municipality"], focal.county = ma.policy[i,"county"], 
					effect_date=ma.policy[i, "eff_date"])
	
	# Print out synth out table
	syn.tab		<- synth.tab(dataprep.res = synout$data, synth.res = synout$synout)		
	print(syn.tab)
	
	# Compute gaps after setting pre-treatment gaps = 0
	gaps		<- synout$data$Y1plot - synout$data$Y0plot %*% synout$synout$solution.w
	sel			<- as.Date(as.numeric(rownames(gaps)), origin = "1970-01-01") < ma.policy[i,"eff_date"]
	pre.avg		<- mean(gaps[sel])
	gaps		<- gaps - pre.avg
	ggtmp		<- data.frame(Week = as.Date(as.numeric(rownames(gaps)), origin = "1970-01-01"), Gaps = gaps, 
								effect_date = ma.policy[i,"eff_date"])
	names(ggtmp)	<- c("Week", "Gaps", "effect_date")
	
	# Placebo gaps
	ggtmp1		<- rbind(cbind(PlaCounty = "Boston", ggtmp, placebo = 0), cbind(testout$gaps, placebo = 1))
	
	# Path plot
	# Make plots 
	pdf(paste(plot.wd, "/graph_synth_", ma.policy[i,"municipality"], ".pdf", sep = ""), width = ww, height = ww*ar)
	print(path.plot(dataprep.res = synout$data, synth.res = synout$synout, 
						Main = paste(ma.policy[i,"municipality"], "\n Path plot", sep=""), 
						tr.intake = as.numeric(ma.policy[i,"effect_date"]), Ylab = "Quantity", Xlab = "Week"))
	dev.off()

	# # Gap plot
	# print(ggplot(ggtmp, aes(Week, Gaps)) + geom_line() + 
	# 		geom_vline(aes(xintercept = as.numeric(effect_date)), size = .25, linetype = 2) + 
	# 		labs(title = paste("Gap plot for ", ma.policy[i,"municipality"], sep=""))+ 
	# 		theme_bw()
	# 		)
	# Placebo gap plot
	plots[[idx]]	<- ggplot(ggtmp1, aes(Week, Gaps)) + geom_line(aes(group = PlaCounty, col = factor(placebo)), alpha = .9) + 
			geom_vline(aes(xintercept = as.numeric(effect_date)), size = .25, linetype = 2) + 
			scale_colour_manual(values = c("black", "grey70")) + 
			labs(title = paste("Placebo test for ", ma.policy[i,"municipality"], sep="")) + 
			guides(color = FALSE) + 
			theme_bw() + 
			theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
	idx <- idx + 1
}

pdf(paste(plot.wd, "/graph_synth_placebo.pdf", sep = ""), width = ww, height = ww*ar)
for(i in 1:length(plots)){
	print(plots[[i]])
}
dev.off()
