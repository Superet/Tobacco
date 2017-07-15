library(reshape2)
library(ggplot2)
library(data.table)
library(stargazer)
library(plm)
library(lme4)
library(MatchIt)
library(lmtest)
library(Matching)

setwd("~/Documents/Research/Tobacco/processed_data")
plot.wd		<- "~/Desktop"
# setwd("U:/Users/ccv103/Documents/Research/tobacco/processed_data")
# setwd("/sscc/home/c/ccv103/Tobacco") 
# plot.wd		<- getwd()
out.file	<- "cvs_nonsmk"
ww			<- 6.5
ar			<- .6
match.code	<- 1
num.month	<- 20					# Time window used for analysis
(out.file	<- paste(out.file, match.code, "_wd", num.month, sep=""))

load("cvs_nonsmk.rdata")
load(paste("cvs_nonsmk_mch", match.code, "_id.rdata", sep=""))

treat.code	<- 2	
nonsmk.pan$treat	<- nonsmk.pan[,paste("treat", treat.code, sep="")]
nonsmk.trips$treat	<- nonsmk.trips[,paste("treat",treat.code,sep="")]

# Set event date
event.date	<- as.Date("2014-09-01", format = "%Y-%m-%d")			# Placebo event 
event.month	<- month(event.date) + 12
cvs.ret	<- 4914		# retailer_code for CVS
qunit	<- 20		# 20 cigaretts per pack
# sink(paste(plot.wd,"/log_", out.file, ".txt", sep=""), append = FALSE)

############
# Function # 
############
Cls.se.fn <- function(model, cluster.vec, return.se = TRUE){
# Var(beta) = (X'X)^(-1) [(eps*X)'(eps*X)](X'X)^(-1)
	X 	<- model.matrix(model)
	uj 	<- residuals(model)  * X
	uj 	<- apply(uj, 2, function(x) tapply(x, cluster.vec, sum))
	A	<- solve(crossprod(X))
	cls.vcov	<- A %*% crossprod(uj) %*% A	
	if(return.se){
	  return(sqrt(diag(cls.vcov))) 
	}else{
	  return(cls.vcov)
	}
}

#######################
# Run DID regressions # 		
#######################
# Only use the households with defined treatment variable
table(nonsmk.pan$smk, nonsmk.pan$frac_seg)						
mypan			<- subset(nonsmk.pan, !is.na(treat) )
mypan			<- mypan[order(mypan$household_code),]
table(mypan$smk, mypan$frac_seg)
table(mypan$treat, mypan$frac_seg)

match.col			<- c("income", "age", "have_kids", "employment", "race", "distance_cvs",
                		  "pre_trip_cvs", "pre_trip_othdrug", "pre_trip_othchannel", "pre_dol_cvs", "pre_dol_othdrug", "pre_dol_othchannel")
if(match.code == 1){
	sel			<- grep("pre", match.col)				
	match.col[sel]	<- paste(match.col[sel], 1, sep="")
}else if(match.code == 2){
	sel			<- grep("pre", match.col)				
	match.col	<- c(match.col[-sel], paste(match.col[sel], rep(1:6, each = length(sel)), sep=""))
}				
match.col

sel.month			<- seq(event.month- num.month, event.month + num.month -1)
if( any(sel.month %%12 ==0 )){
	sel0			<- which(sel.month %% 12 == 0)
	sel.month		<- sel.month[-sel0]
	sel1			<- sel0[sel0 < num.month]
	sel2			<- sel0[sel0 >= num.month]
	if(length(sel1) > 0){sel.month <- c(sel.month, min(sel.month) - 1:length(sel1))}
	if(length(sel2) > 0){sel.month <- c(sel.month, max(sel.month) + 1:length(sel2))}
}
sel.month
match.shtdat		<- data.table(subset(nonsmk.trips, month %in% sel.month & !is.na(treat)))
table(match.shtdat$month)

# Aggregate over the assigned time window around the event 
match.shtdat		<- match.shtdat[, list(	trip_cvs = sum(trip_cvs)/num.month, trip_othdrug = sum(trip_othdrug)/num.month, trip_othchannel = sum(trip_othchannel)/num.month, 
											dol_cvs = sum(dol_cvs)/num.month, dol_othdrug = sum(dol_othdrug)/num.month, dol_othchannel = sum(dol_othchannel)/num.month, 
											netdol_cvs = sum(netdol_cvs)/num.month, netdol_othdrug = sum(netdol_othdrug)/num.month, netdol_othchannel = sum(netdol_othchannel)/num.month,
											dol_total = sum(dol_total)/num.month, netdol = sum(netdol)/num.month), 
									by = list(treat, household_code, after, frac_seg, smk)]
match.shtdat 	<- match.shtdat[, drop:= 1*(length(after) < 2), by = list(household_code)]
table(match.shtdat$drop)									# 1 household did not both 2 period data

# Take before-after difference
setkeyv(match.shtdat, c("household_code","after"))
base.lv	<- data.frame(match.shtdat[after==0 & treat==1,])
match.shtdat	<- match.shtdat[,list(	trip_cvs = diff(trip_cvs), trip_othdrug = diff(trip_othdrug), trip_othchannel = diff(trip_othchannel), 
										dol_cvs = diff(dol_cvs), dol_othdrug = diff(dol_othdrug), dol_othchannel = diff(dol_othchannel), 
										netdol_cvs = diff(netdol_cvs), netdol_othdrug = diff(netdol_othdrug), netdol_othchannel = diff(netdol_othchannel),
										dol_total = diff(dol_total), netdol = diff(netdol)), 
								by = list(household_code, treat, frac_seg, smk)]
match.shtdat	<- data.frame(match.shtdat)
match.shtdat	<- merge(match.shtdat, nonsmk.pan[,c("household_code", match.col)], by = "household_code", all.x = T)
match.shtdat	<- match.shtdat[order(match.shtdat$household_code),]
max(abs(mypan$household_code - match.shtdat$household_code))

dv.col		<- c("trip_cvs", "trip_othdrug", "trip_othchannel", 
				 "netdol_cvs", "netdol_othdrug", "netdol_othchannel", "dol_total", "netdol")
base.lv			<- apply(as.matrix(base.lv[,dv.col]), 2, mean, na.rm = T)
cat("Pre-treatment baseline for the treatd smokers:\n"); print(round(base.lv, 3)); cat("\n")

###############################
# Run regressions for each DV #
###############################
dv.col		<- c("trip_cvs", "trip_othdrug", "trip_othchannel", 
				 "netdol_cvs", "netdol_othdrug", "netdol_othchannel", "dol_total", "netdol")
dv.lab		<- c("Trip-CVS", "Trip-Other drug stores", "Trip-Other channel", 
				 "NetExp-CVS", "NetExp-Other drug stores", "NetExp-Other channel", "Total expenditure", "Net expenditure")
cbind(dv.col, dv.lab)				
tmpdat		<- subset(nonsmk.trips, household_code %in% match.ps & month %in% sel.month )
tmpdat		<- merge(tmpdat, mypan[,c("household_code", setdiff(match.col,names(tmpdat)))], by = "household_code", all.x = T)																
sel			<- match.shtdat$household_code %in% match.ps
sel.bp		<- match.shtdat$household_code %in% match.bipart
table(match.shtdat[sel,"treat"])
max(abs(sort(unique(tmpdat$household_code)) - mypan[sel, "household_code"]))

est.bipart	<- est.ols		<- est.mat	<- est.fe	<- matrix(NA, length(dv.col), 4, 
											dimnames = list(dv.col, c("Estimate", "Std. Error", "t value", "Pr(>|t|)")))
for(i in 1:length(dv.col)){
	dv.fml			<- as.formula(paste(dv.col[i], "~ treat"))
	# dv.fml			<- as.formula(paste(dv.col[i], "~ treat + income + age + have_kids + employment + race + distance_cvs +
	#                  		pre_trip_cvs+pre_trip_othdrug +pre_trip_othchannel +pre_dol_cvs +pre_dol_othdrug +pre_dol_othchannel"))
	# OLS
	est.ols[i,]	<- coeftest(lm(dv.fml, data = match.shtdat))["treat",]
		
	# Matching without replacement 
	est.mat[i,]	<- coeftest(lm(dv.fml, data = match.shtdat[sel,]))["treat",]
	
	# Fixed effect model with matched households
	tmp		<- plm(as.formula(paste(dv.col[i], "~ treat + treat*after + after")), data = tmpdat, 
					index = c("household_code", "month"), model = "within")
		
	cls.v	<- Cls.se.fn(tmp, cluster.vec = tmpdat[,"household_code"], return.se = FALSE)
	est.fe[i,]	<- coeftest(tmp, vcov = cls.v)["treat:after",]	
	
	# Bipart matching 
	est.bipart[i,]	<- coeftest(lm(dv.fml, data = match.shtdat[sel.bp,]))["treat",]
}
class(est.bipart)	<- class(est.ols)	<- class(est.mat) <- class(est.fe) <- c("coeftest", "matrix")

# Print results 
stargazer(list(OLS = est.ols, Propensity = est.mat, Bipart = est.bipart, Panel = est.fe), type = "text", 
			column.labels = c("OLS", "Propensity", "Bipartite", "Panel"))

nn1		<- rbind(table(mypan$treat), table(mypan[sel,"treat"]), table(mypan[sel.bp,"treat"]))			
stargazer(list(OLS = est.ols, Propensity = est.mat, Bipart = est.bipart), type = "latex",
 			digits = 2, no.space = FALSE, align = FALSE,
			title = "Regression with non-smokers wtihout covariates",
			column.labels = c("OLS", "Propensity", "Bipartite"),
			covariate.labels = dv.lab,
			add.lines = list(c("Control", rep(nn1[,"0"], 2)),c("Treated", rep(nn1[,"1"], 2)), 
								c("Covariates", rep("Yes", 4))) 
			)						

# Percentage change relative to pre-treatment level
tmp		<- cbind(OLS = est.ols[,"Estimate"], Propensity = est.mat[,"Estimate"], Bipart = est.bipart[,"Estimate"], Panel = est.fe[,"Estimate"])
tmp		<- tmp/base.lv[dv.col]
stargazer(tmp, type = "text", summary = FALSE, digits = 2, align = TRUE,
		column.labels = c("OLS", "Propensity", "Bipartite", "Panel"))			
			
# ---------------- #
# Add covariates #
est1.bipart	<- est1.ols	<- est1.mat	<- est1.fe	<- matrix(NA, length(dv.col), 4, 
													dimnames = list(dv.col, c("Estimate", "Std. Error", "t value", "Pr(>|t|)")))

for(i in 1:length(dv.col)){
	dv.fml			<- as.formula(paste(dv.col[i], "~ treat +", paste(match.col, collapse = "+")))
	# OLS
	est1.ols[i,]	<- coeftest(lm(dv.fml, data = match.shtdat))["treat",]

	# Matching without replacement 
	est1.mat[i,]	<- coeftest(lm(dv.fml, data = match.shtdat[sel,]))["treat",]

	# Fixed effect model with matched households
	tmp		<- plm(as.formula(paste(dv.col[i], "~ treat + treat*after + after")), data = tmpdat, 
					index = c("household_code", "month"), model = "within")
	cls.v	<- Cls.se.fn(tmp, cluster.vec = tmpdat[,"household_code"], return.se = FALSE)
	est1.fe[i,]	<- coeftest(tmp, vcov = cls.v)["treat:after",]
	
	# Bipart matching 
	est1.bipart[i,]	<- coeftest(lm(dv.fml, data = match.shtdat[sel.bp,]))["treat",]
}
class(est1.bipart)	<- class(est1.ols)	<- class(est1.mat) <- class(est1.fe) <- c("coeftest","matrix")

# Print results 
stargazer(list(OLS = est1.ols, Propensity = est1.mat, Bipart = est1.bipart, Panel = est1.fe), type = "text", 
			column.labels = c("OLS", "Propensity", "Bipartite", "Panel"))

nn1		<- rbind(table(mypan$treat), table(mypan[sel,"treat"]), table(mypan[sel.bp,"treat"]), table(mypan[sel,"treat"]))			
stargazer(list(OLS = est1.ols, Propensity = est1.mat, Bipart = est1.bipart, est1.fe), type = "latex", 
			no.space = FALSE, align = FALSE, digits = 2, 
			title = "Regression with non-smokers wtih covariates using non-smoker data",
			column.labels = c("OLS", "Propensity", "Bipartite", "Panel"),
			covariate.labels = dv.lab,
			add.lines = list(c("Control", nn1[,"0"]),c("Treated", nn1[,"1"]), 
								c("Covariates", rep("Yes", 3), "No"), 
								c("FE", rep("No", 3), "Yes") ) 
			)

# Percentage change relative to pre-treatment level
tmp		<- cbind(OLS = est.ols1[,"Estimate"], Propensity = est.mat1[,"Estimate"], Bipart = est.bipart1[,"Estimate"], Panel = est.fe1[,"Estimate"])
tmp		<- tmp/base.lv[dv.col]
stargazer(tmp, type = "text", summary = FALSE, digits = 2, align = TRUE,
		column.labels = c("OLS", "Propensity", "Bipartite", "Panel"))			

			
# sink()

stargazer(list(est.ols, est.mat, est.fe, est.bipart,est1.ols, est1.mat, est1.fe, est1.bipart), 
		  type = "html", summary = FALSE, align = TRUE, no.space = TRUE, digits = 2,
		  column.labels = rep(c("OLS", "Propensity", "Panel", "Bipartite"), 2),
		  covariate.labels = dv.lab,
          title = c("Before-after difference between treatment and control group for the matched sample during 201406 - 201411"),
		  add.lines = list(c("Control", rep(nn1[,"0"], 2)),c("Treated", rep(nn1[,"1"], 2)), 
							c("Covariates", rep("No", 4), rep("Yes", 4))), 
          out = paste(plot.wd, "/tb_", out.file, "_reg_",Sys.Date(), ".html", sep=""))
			
cat("This program is done.\n")
