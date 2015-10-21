library(data.table)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(gridExtra)
library(plm)
library(lme4)
library(r2excel)

setwd("~/Documents/Research/Tobacco/processed_data")
source("../Exercise/outreg function.R")
mycounty 	<- read.csv("county_treatment.csv", header = T)
store.pharm	<- read.csv("pharmacy_store.csv", header = T)
load("sales_ma.rdata")

# Set plotting parameters
plot.wd	<- "~/Desktop"
ww		<- 12
ar		<- .66
out.xls	<- "tobacco_MA_rms.xlsx"
wb		<- createWorkbook(type = "xlsx")
sht1	<- createSheet(wb, sheetName = "quantity")

############
# Function # 
############
getEst	<- function(fit, excl.var = NULL){
	out	<- try(coef(summary(fit)))
	if(class(out) == "try-error"){
		b	<- coef(fit)
		se	<- diag(vcov(fit))
		if(!is.null(selvar)){
			sel	<- do.call(c, lapply(excl.var, function(x) grep(x, names(b))))
			sel	<- setdiff(1:length(b), sel)
			b	<- b[sel]
			se	<- se[sel]
		}
		se	<- sqrt(se)
		out	<- cbind(b, se)
		colnames(out)	<- c("Estimate", "Std. Error")
	}
	return(out)
}

#################
# Organize data # 
#################
# Convert variables of date format
sales_MA			<- subset(sales_MA, channel_code != "L")		# Ignore liqure stores
sales_MA$week_end	<- as.Date(as.character(sales_MA$week_end), format = "%Y-%m-%d")
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
channel.lab	<- c("All", "Drug", "DFM", "Convenience")		

mkt_sale	<- data.frame(NULL)
selcol		<- c("municipality", "effect_date", "fips_county_descr", "treatment", "control_date")
for(i in 1:length(mncp)){
	for(j in 1:length(channel.lab)){
		tmp		<- as.character(mycounty[mycounty$municipality == mncp[i], "fips_county_descr"])
		tmpdat	<- subset(sales_MA, fips_county_descr %in% tmp)
		if(j == 2){
			tmpdat	<- subset(tmpdat, channel_code == "D")
		}else if(j == 3){
			tmpdat	<- subset(tmpdat, channel_code %in% c("D", "F", "M"))
		}else if(j == 4){
			tmpdat	<- subset(tmpdat, channel_code == "C")
		}
		tmpdat	<- data.table(tmpdat)
		tmpdat	<- tmpdat[,list(quantity = sum(units*size)/pack.size, 
								price = sum(price * units )/sum(units * size) *pack.size, 
								top.share = sum(units*size*1*(brand_descr == top.brd))/sum(units*size) ),
							by = list(fips_county_descr, week_end)]
		tmpdat	<- merge(data.frame(tmpdat), mycounty[mycounty$municipality == mncp[i], selcol], by = "fips_county_descr", all.x = T)					
		tmpdat$channel	<- channel.lab[j]
		mkt_sale	<- rbind(mkt_sale, tmpdat)
	}
}
mkt_sale$treatment 	<- factor(mkt_sale$treatment, levels = c(1, 0))
mkt_sale$channel	<- factor(mkt_sale$channel, levels = channel.lab)



mydata	<- subset(mkt_sale, channel == "All" & municipality == "Boston")
mydata$county_num	<- as.numeric(mydata$fips_county_descr)
mydata$week_num		<- as.numeric(mydata$week_end)
tmp.treat	<- unique(mydata[mydata$fips_county_descr == "SUFFOLK","county_num"])
tmp.contrl	<- setdiff(unique(mydata$county_num), tmp.treat)
tmp.time	<- as.numeric(unique(mydata$effect_date))
tmp.prior	<- seq(min(mydata$week_num), max(mydata[mydata$week_end < mydata$effect_date, "week_num"]), 7)
mydata$fips_county_descr	<- as.character(mydata$fips_county_descr)
synth.data	<- dataprep(foo = mydata, predictors = c("price","top.share"), dependent = "quantity", 
						unit.variable = "county_num", time.variable = "week_num", 
						treatment.identifier = tmp.treat, controls.identifier = tmp.contrl, 
						time.predictors.prior = tmp.prior, time.optimize.ssr = tmp.prior, 
						unit.names.variable = "fips_county_descr", time.plot = sort(unique(mydata$week_num)))
						
syn.out		<- synth(synth.data)	
round(syn.out$solution.w, 2)
syn.out$solution.v
syn.tab		<- synth.tab(dataprep.res = synth.data, synth.res = syn.out)		
print(syn.tab)
gaps		<- synth.data$Y1plot - synth.data$Y0plot %*% syn.out$solution.w

gaps<- dataprep.out$Y1plot-(
        dataprep.out$Y0plot%*%synth.out$solution.w
        ) ; gaps
path.plot(dataprep.res = synth.data, synth.res = syn.out)
gaps.plot(dataprep.res = synth.data, synth.res = syn.out)
