library(data.table)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(gridExtra)
library(plm)
library(lme4)
library(r2excel)
library(zipcode)
library(xlsx)

setwd("/Users/chaoqunchen/Documents/Research/Tobacco/processed_data")
source("../Exercise/outreg function.R")
load("cigar_HMS_MA.rdata")
county	<- read.csv("county_treatment.csv", header = T)
store.pharm	<- read.csv("pharmacy_store.csv", header = T)
ma.policy	<- read.xlsx("MA_policy.xlsx", 1)
data(zipcode)

# Set plotting parameters 
plot.wd	<- "~/Desktop"
ww		<- 6.5
ar		<- .6
out.xls	<- paste(getwd(), "/hms_ma.xlsx", sep="")
wb		<- createWorkbook(type = "xlsx")
sht1	<- createSheet(wb, sheetName = "HMS")

############
# Function # 
############
getEst	<- function(fit, excl.var = NULL){
	out	<- try(coef(summary(fit)))
	if(class(out) == "try-error"){
		b	<- coef(fit)
		se	<- diag(vcov(fit))
		if(!is.null(excl.var)){
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
county$enact_date		<- as.Date(as.character(county$enact_date), format = "%m/%d/%y")
county$effect_date		<- as.Date(as.character(county$effect_date), format = "%m/%d/%y")
county$control_date		<- as.Date(as.character(county$control_date), format = "%m/%d/%y")
purchases$purchase_date	<- as.Date(as.character(purchases$purchase_date), format = "%Y-%m-%d")
purchases$year			<- year(purchases$purchase_date)

# Restrict to MA
my.state	<- "MA"
pan.ma	<- subset(panelists, fips_state_descr == my.state)
pch.ma	<- subset(purchases, household_code %in% pan.ma$household_code)
pch.ma$channel_type	<- as.character(pch.ma$channel_type)
pan.ma$fips_county_descr	<- as.character(pan.ma$fips_county_descr)
pack.size	<- 20

# Events in MA
mncp	<- subset(ma.policy, RETAIL_AFF > 0 & EFF_DATE < as.Date("2013-06-30", format = "%Y-%m-%d"))
names(mncp)	<- tolower(names(mncp))
mncp$municipality	<- as.character(mncp$municipality)
ord		<- order(mncp$eff_date)
mncp	<- mncp[ord,]
tmp 	<- as.numeric(diff(mncp$eff_date))
cat("The gaps between policy changes:\n"); print(summary(tmp)); cat("\n")

# Merge zipcode to determine the residence cities
pan.ma$zip	<- clean.zipcodes(pan.ma$panelist_zip_code)
pan.ma		<- merge(pan.ma, zipcode[,c("zip", "city")], by = "zip", all.x = T)
tmp1 		<- table(pan.ma$city)

# Drop the treatment cities where we don't observe any households
mncp		<- subset(mncp, municipality %in% pan.ma$city)
cat("Number of households in the treated city:\n"); print(tmp1[as.character(mncp$municipality)]); cat("\n")

# Merge data
pch.ma	<- merge(pch.ma, pan.ma[,c("household_code","panel_year","fips_county_descr","city")], 
				by.x = c("household_code", "year"), by.y = c("household_code", "panel_year"), all.x = T)
pch.ma	<- merge(pch.ma, prod[,c("upcv","multi","size1_amount")], by = "upcv", all.x = T)

# Construct experiment data for each policy change
my.window	<- 180			# We consider purchases within half a year around the event date
ma.plc	<- data.frame()
sel.idx	<- NULL
for(i in 1:nrow(mncp)){
	treatment.dt	<- subset(pch.ma, city == mncp[i,"municipality"] & abs(purchase_date - mncp[i,"eff_date"]) <= my.window)
	if(nrow(treatment.dt) == 0){
		sel.idx	<- c(sel.idx, i)
		next
	}
	treatment.dt$treatment	<- 1
	next.date		<- mncp[i,"eff_date"] + my.window
	sel				<- mncp$eff_date < next.date
	excl.city		<- mncp[sel,"municipality"]
	control.dt		<- subset(pch.ma, !city %in% excl.city & abs(purchase_date - mncp[i,"eff_date"]) <= my.window)
	control.dt$treatment	<- 0
	tmp				<- rbind(treatment.dt, control.dt)
	tmp				<- cbind(tmp, mncp[i,c("municipality", "eff_date")])
	ma.plc			<- rbind(ma.plc, tmp)
}
cat("The municipalies where we don't have households:\n"); print(mncp[sel.idx,]); cat("\n")
mncp	<- mncp[-sel.idx,]

# NOTE: there might be duplicated household-purchase_date in the control group
tmp		<- data.table(ma.plc)
id		<- with(tmp, paste(municipality, treatment,household_code, purchase_date, upcv, sep="*"))
sel		<- duplicated(id)
tmp		<- tmp[!sel,]
tmp		<- tmp[,list(first = min(purchase_date), last = max(purchase_date), n = max(purchase_date) - min(purchase_date)), 
				by = list(municipality, eff_date, treatment, household_code)]
sum(tmp$n == 0)				
tmp		<- subset(tmp, n > 0)
tmp1	<- tmp[,list(n_control = sum(treatment ==0), n_treat = sum(treatment == 1)), by = list(municipality)]
cat("With repeated controls, we have:\n"); print(tmp1); cat("\n")

# No treatment left in Oxford, hence delete this case
tmp		<- subset(tmp, municipality != "Oxford")
mncp	<- subset(mncp, municipality != "Oxford")
ma.plc	<- subset(ma.plc, municipality != "Oxford")

# Check the time difference between cases for each household
AssignGrp	<- function(eff_date, first, last){
	n	<- length(eff_date)
	if(n == 1){
		return(1)
	}
	out	<- rep(0, n)
	i	<- 1
	cum.gap	<- 0
	while(i <= n){
		gap.last	<- ifelse(i == 1, NA, eff_date[i] - eff_date[i-1])
		gap.next	<- ifelse(i == n, NA, eff_date[i+1] - eff_date[i])
		if(min(c(gap.last, gap.next), na.rm =T ) > 2*my.window){
			out[i]	<- 1
		}else{
			if(cum.gap == 0){
				out[i] <- 1
				cum.gap	<- cum.gap + ifelse(is.null(gap.next), 0, ifelse(gap.next ==0, 1, gap.next))
			}
			if(cum.gap > 2 * my.window){
				out[i]	<- 1
				cum.gap <- 0
			}
		}
		i <- i + 1
	}
	if(sum(out) == 1){
		out		<- rep(0, n)
		n_stay	<- as.numeric(last - first)
		out[which.max(n_stay)] <- 1
	}
	return(out)
}

setkeyv(tmp, c("household_code","eff_date") )
tmp		<- tmp[, keep:= AssignGrp(eff_date, first, last), by = list(household_code)]
ma.plc	<- merge(ma.plc, data.frame(tmp)[,c("household_code", "treatment", "municipality","keep")], 
				by = c("household_code", "treatment", "municipality"), all.x = T)
unq.maplc	<- subset(ma.plc, keep = 1)

# Summarize the size of treatment vs. control
trt.ctl		<- data.table(unq.maplc)
trt.ctl		<- trt.ctl[,list(household_code = unique(household_code)), by = list(municipality, treatment)]
trt.ctl		<- trt.ctl[,list(n_control = sum(treatment ==0), n_treat = sum(treatment == 1)), by = list(municipality)]
cat("The size of treatment vs. control for each municipality:\n"); print(trt.ctl); cat("\n")

# Export to excel
xlsx.addHeader(wb, sht1, value = "The size of treatment vs. control for each municipality.", level = 2)
xlsx.addTable(wb, sht1, trt.ctl)
xlsx.addLineBreak(sht1, 1)

#-------------------------------#
# Construct household-week data #
hh.week	<- data.table(unq.maplc)
hh.week	<- hh.week[, ':='(week = paste(year, format(purchase_date, "%U"), sep="-"), 
						effect_week = paste(year(eff_date), format(eff_date, "%U"), sep="-")) ]
hh.week	<- hh.week[, ':='( week_num = as.numeric(factor(week, levels = sort(unique(hh.week$week)))), 
						effect_week_num = as.numeric(factor(effect_week, levels = sort(unique(hh.week$week)))) )]
hh.week	<- hh.week[, list(quantity 	= sum(units*size1_amount*multi)/pack.size, 
						  price 	= mean(total_price_paid/(units*size1_amount*multi)) * pack.size), 
			by = list(municipality, effect_week_num, city, treatment, household_code, week_num)]

# Fill-in non-purchase weeks 
tmp		<- hh.week[,list(week_num = c(min(week_num):max(week_num))), 
					by = list(municipality, effect_week_num, city, treatment, household_code)]
hh.week	<- merge(data.frame(hh.week), data.frame(tmp), 
				by = c("municipality", "effect_week_num", "city", "treatment", "household_code", "week_num"), all.y = TRUE)
sel		<- is.na(hh.week$quantity)
table(sel)
hh.week[sel,"quantity"]	<- 0
hh.week$post		<- 1 * (hh.week$week_num >= hh.week$effect_week_num)					
hh.week$post_treat	<- with(hh.week, post*treatment)

# Check the ratio of treatment vs. control 
tmp1	<- data.table(hh.week)
tmp1	<- tmp1[,list(nobs = length(week_num)), by = list(treatment, household_code)]
table(tmp1$treatment)

# Plot the aggregated quantity 
ggtmp	<- data.table(hh.week)
ggtmp$week	<- with(ggtmp, week_num - effect_week_num)
ggtmp	<- ggtmp[,list(quantity = sum(quantity), nhh = length(unique(household_code))), by = list(municipality, treatment, week)]
ggtmp	<- ggtmp[,quant_per_hh := quantity/nhh]
ggtmp$municipality	<- factor(ggtmp$municipality, levels = mncp$municipality)
ggplot(ggtmp, aes(week, quant_per_hh, linetype = factor(treatment))) + 
		geom_line() + facet_wrap(~ municipality, scales = "free_y") + 
		labs(y = "Average packs per household")
		
# DID regressions
fit.ls	<- setNames(vector("list", 3), c("Quantity", "Incidence", "Price"))
fit.ls[[1]]	<- lm(quantity ~ post_treat + treatment + post, data = hh.week)
fit.ls[[2]]	<- glm(1*(quantity>0) ~ post_treat + treatment + post, data = hh.week, family = "binomial")
fit.ls[[3]]	<- lm(price ~ post_treat + treatment + post, data = subset(hh.week, !is.na(price)))

est.ls	<- lapply(fit.ls, function(x) coef(summary(x)))
tmp.tab	<- model_outreg(est.ls,	p.given = FALSE, head.name = c("Estimate", "Std. Error","Pr(>|t|)"), 
						varord = c("post_treat","treatment","post","(Intercept)"))
cat("Regression estimates:\n"); print(tmp.tab); cat("\n")

# Export to excel 
xlsx.addHeader(wb, sht1, value = "DID regressions at household-week level.", level = 2)
xlsx.addTable(wb, sht1, tmp.tab)
xlsx.addLineBreak(sht1, 1)

# Save xlsx results 
saveWorkbook(wb, out.xls)

