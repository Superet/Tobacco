library(reshape2)
library(ggplot2)
library(data.table)
library(lubridate)
library(stargazer)
library(plm)
library(lme4)
library(MatchIt)
library(lmtest)
library(Matching)
library(AER)

setwd("~/Documents/Research/Tobacco/processed_data")
plot.wd		<- "~/Desktop"
# setwd("U:/Users/ccv103/Documents/Research/tobacco/processed_data")
# setwd("/sscc/home/c/ccv103/Tobacco") 
# plot.wd		<- getwd()
out.file	<- "cvs_smk"
ww			<- 6.5
ar			<- .6
num.month	<- 20					# Time window used for analysis

# Match.code
# 0: Pre-treatment variables are defined using Jan 2013 - Aug 2014
# 1: Pre-treatment variables are defined using June 2014 - Aug 2014
# 2: Match 3-month period trend 
match.code	<- 2	
(out.file	<- paste(out.file, match.code, "_wd", num.month, sep=""))

load("cvs_smk.rdata")
load(paste('Results/CVS case/cvs_smk_mch', match.code,'_id.rdata', sep=""))

# Define the construction of treatment and control
# 1: Control (smokers who never shopped at CVS), Treament (smokers who are also CVS shoppers)
# 2: Control (smokers who shopped at CVS but did not buy cigarettes), Treament (smokers who also purchased cigarettes at CVS)
# 3: Control (smokers who shopped at CVS but did not buy cigarettes), Treament (smokers who spent more than 17% CVS spending on cigarettes)
treat.code	<- 2	
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
mypan			<- subset(smk.pan, !is.na(treat))
mypan			<- mypan[order(mypan$household_code),]
# mypan$distance_cvs	<- log(mypan$distance_cvs)
# mypan$pre_q		<- log(mypan$pre_q)
# mypan$pre_trip_total	<- log(mypan$pre_trip_total)
# mypan$pre_trip_othchannel	<- log(mypan$pre_trip_othchannel)
# mypan$pre_dol_othchannel	<- log(mypan$pre_dol_othchannel)
# mypan$pre_dol_total	<- log(mypan$pre_dol_total)

# Aggregate over 3 month window around the event 
match.col			<- c("income", "age", "have_kids", "employment", "race", "distance_cvs",
                		 "pre_q", "pre_trip_total", "pre_trip_othchannel", "pre_dol_total", "pre_dol_othchannel")
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
match.shtdat		<- data.table(subset(smk.trips, month %in% sel.month & !is.na(treat)))
match.shtdat		<- match.shtdat[, list(	q = sum(q)/num.month, q_cvs = sum(q_cvs)/num.month, q_othdrug = sum(q_othdrug)/num.month, q_othchannel = sum(q_othchannel)/num.month, 
									trip_cvs = sum(trip_cvs)/num.month, trip_othdrug = sum(trip_othdrug)/num.month, trip_othchannel = sum(trip_othchannel)/num.month, 
									dol_cvs = sum(dol_cvs)/num.month, dol_othdrug = sum(dol_othdrug)/num.month, dol_othchannel = sum(dol_othchannel)/num.month, 
									netdol_cvs = sum(netdol_cvs)/num.month, netdol_othdrug = sum(netdol_othdrug)/num.month, netdol_othchannel = sum(netdol_othchannel)/num.month,
									dol_total = sum(dol_total)/num.month, netdol = sum(netdol)/num.month, 
									q_convenience = sum(q_convenience)/num.month, q_grocery = sum(q_grocery)/num.month, q_discount = sum(q_discount)/num.month, 
									q_service = sum(q_service+q_gas)/num.month, q_tobacco = sum(q_tobacco)/num.month), 
									by = list(household_code, after)]
match.shtdat 	<- match.shtdat[, drop:= 1*(length(after) < 2), by = list(household_code)]
table(match.shtdat$drop)									# 1 household did not both 2 period data

# Take before-after difference
base.lv	<- match.shtdat[after==0,]
setkeyv(match.shtdat, c("household_code","after"))
match.shtdat	<- match.shtdat[,list(q = diff(q), q_cvs = diff(q_cvs), q_othdrug = diff(q_othdrug), q_othchannel = diff(q_othchannel), 
								trip_cvs = diff(trip_cvs), trip_othdrug = diff(trip_othdrug), trip_othchannel = diff(trip_othchannel), 
								dol_cvs = diff(dol_cvs), dol_othdrug = diff(dol_othdrug), dol_othchannel = diff(dol_othchannel), 
								netdol_cvs = diff(netdol_cvs), netdol_othdrug = diff(netdol_othdrug), netdol_othchannel = diff(netdol_othchannel),
								dol_total = diff(dol_total), netdol = diff(netdol), 
								q_convenience = diff(q_convenience), q_grocery = diff(q_grocery), q_discount = diff(q_discount), 
								q_service = diff(q_service), q_tobacco = diff(q_tobacco)), 
								by = list(household_code)]
match.shtdat	<- data.frame(match.shtdat)
dim(match.shtdat)
match.shtdat	<- merge(match.shtdat, smk.pan[,c("household_code", "treat", "frac_seg",match.col)], by = "household_code", all.x = T)
dim(match.shtdat)
match.shtdat	<- match.shtdat[order(match.shtdat$household_code),]
max(abs(mypan$household_code - match.shtdat$household_code))
base.lv			<- merge(base.lv, smk.pan[,c("household_code", "treat")], by = "household_code", all.x = T)
base.lv			<- base.lv[treat==1,]
base.lv			<- apply(as.matrix(base.lv), 2, mean)
cat("Pre-treatment baseline for the treatd smokers:\n"); print(round(base.lv, 3)); cat("\n")

############################### 
# Run regressions for each DV #
############################### 
dv.col		<- c("q", "q_cvs", "q_othdrug", "q_othchannel", 
				"q_convenience", "q_grocery", "q_discount", "q_service", "q_tobacco",
				"trip_cvs", "trip_othdrug", "trip_othchannel", "netdol_cvs", "netdol_othdrug","netdol_othchannel"
				)
dv.lab		<- c(paste("Cigar-",c("Total", "CVS", "Other drugstores", "Other channel"), sep=""), 
				paste("Cigar-",c("Convenience", "Grocery", "Discount stores", "Sevice station", "Tabacco stores"), sep="") , 
				paste("Trip-",c("CVS", "Other drugstores", "Other channels"), sep=""), 
				paste("NetExp-",c("CVS", "Other drugstores", "Other channels"), sep="")
				)	
cbind(dv.col, dv.lab)		

sel			<- match.shtdat$household_code %in% match.ps
sel.bp		<- match.shtdat$household_code %in% match.bipart
table(match.shtdat[sel,"frac_seg"])
tmpdat		<- subset(smk.trips, household_code %in% match.ps & month %in% sel.month)
tmpdat		<- merge(tmpdat, smk.pan[,c("household_code", setdiff(match.col,names(tmpdat)))], by = "household_code", all.x = T)																


est.bipart	<- est.ols		<- est.mat	<- est.fe	<- matrix(NA, length(dv.col), 4, 
															dimnames = list(dv.col, c("Estimate", "Std. Error", "t value", "Pr(>|t|)")))

for(i in 1:length(dv.col)){
	dv.fml			<- as.formula(paste(dv.col[i], "~ treat"))
	# OLS
	est.ols[i,]	<- coeftest(lm(dv.fml, data = match.shtdat))["treat",]
	
	# Matching without replacement 
	est.mat[i,]	<- coeftest(lm(dv.fml, data = match.shtdat[sel,]))["treat",]
	
	# Fixed effect model with matched households
	if(num.month > 12){
		tmp		<- plm(as.formula(paste(dv.col[i], "~ treat + treat*after + after + month1")), data = tmpdat, 
						index = c("household_code", "month"), model = "within")		
	}else{
		tmp		<- plm(as.formula(paste(dv.col[i], "~ treat + treat*after + after")), data = tmpdat, 
						index = c("household_code", "month"), model = "within")		
	}
	cls.v	<- Cls.se.fn(tmp, cluster.vec = tmpdat[,"household_code"], return.se = FALSE)
	est.fe[i,]	<- coeftest(tmp, vcov = cls.v)["treat:after",]
	
	# Bipart matching 
	est.bipart[i,]	<- coeftest(lm(dv.fml, data = match.shtdat[sel.bp,]))["treat",]
}
class(est.bipart)	<- class(est.ols)	<- class(est.mat) <- class(est.fe) <- c("coeftest", "matrix")

# Print results 
stargazer(list(OLS = est.ols, Propensity = est.mat, Bipart = est.bipart, Panel = est.fe), type = "text", 
			column.labels = c("OLS", "Propensity", "Bipartite", "Panel"))

(nn1		<- rbind(table(smk.pan$treat), table(mypan[sel,"treat"]), table(mypan[sel.bp,"treat"])))
stargazer(list(est.ols, est.mat, est.bipart), 
		  type = "latex", summary = FALSE, align = FALSE, no.space = FALSE, digits = 2,
		  column.labels = rep(c("OLS", "Propensity", "Bipartite"), 2),
		  covariate.labels = dv.lab,
          title = c("Balance Check among smokers", "Before-after difference between treatment and control group for the matched sample during 201406 - 201411"),
		  add.lines = list(c("Control", rep(nn1[,"0"], 2)),c("Treated", rep(nn1[,"1"], 2)), 
							c("Covariates", rep("No", 4))) )

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
	if(num.month > 12){
		tmp		<- plm(as.formula(paste(dv.col[i], "~ treat + treat*after + after + month1")), data = tmpdat, 
						index = c("household_code", "month"), model = "within")		
	}else{
		tmp		<- plm(as.formula(paste(dv.col[i], "~ treat + treat*after + after")), data = tmpdat, 
						index = c("household_code", "month"), model = "within")		
	}
	cls.v	<- Cls.se.fn(tmp, cluster.vec = tmpdat[,"household_code"], return.se = FALSE)
	est1.fe[i,]	<- coeftest(tmp, vcov = cls.v)["treat:after",]
	
	# Bipart matching 
	est1.bipart[i,]	<- coeftest(lm(dv.fml, data = match.shtdat[sel.bp,]))["treat",]
}
class(est1.bipart)	<- class(est1.ols)	<- class(est1.mat) <- class(est1.fe) <- c("coeftest", "matrix")

# Print results 
stargazer(list(OLS = est1.ols, Propensity = est1.mat, Bipart = est1.bipart, Panel = est1.fe), type = "text", 
			column.labels = c("OLS", "Propensity", "Bipartite", "Panel"))

# Percentage change relative to pre-treatment level
tmp		<- cbind(OLS = est1.ols[,"Estimate"], Propensity = est1.mat[,"Estimate"], Bipart = est1.bipart[,"Estimate"], Panel = est1.fe[,"Estimate"])
tmp		<- tmp/base.lv[dv.col]
stargazer(tmp, type = "text", summary = FALSE, digits = 2, align = TRUE,
		column.labels = c("OLS", "Propensity", "Bipartite", "Panel"))			

nn1		<- rbind(table(mypan$treat), table(mypan[sel,"treat"]), table(mypan[sel.bp,"treat"]), table(mypan[sel,"treat"]))
stargazer(list(est1.ols, est1.mat, est1.bipart, est1.fe), 
		  type = "latex", summary = FALSE, align = FALSE, no.space = FALSE, digits = 2,
		  column.labels = c("OLS", "Propensity", "Bipartite", "Panel"),
		  covariate.labels = dv.lab,
          title = "Before-after difference between treatment and control group for the matched sample during 201406 - 201411", 
		  add.lines = list(c("Control", nn1[,"0"]),c("Treated", nn1[,"1"]), 
							c("Covariates", rep("Yes", 3), "No"), 
							c("FE", rep("No", 3), "Yes") ) )
			
# sink()

nn1		<- rbind(table(mypan$treat), table(mypan[sel,"treat"]), table(mypan[sel,"treat"]), table(mypan[sel.bp,"treat"]))
stargazer(list(est.ols, est.mat, est.fe, est.bipart, est1.ols, est1.mat, est1.fe, est1.bipart), 
		  type = "html", summary = FALSE, align = FALSE, no.space = FALSE, digits = 2,
		  column.labels = rep(c("OLS", "Propensity", "Panel", "Bipartite"), 2),
          title = c("Balance Check among smokers", "Before-after difference between treatment and control group for the matched sample during 201406 - 201411"),
		  add.lines = list(c("Control", rep(nn1[,"0"], 2)),c("Treated", rep(nn1[,"1"], 2)), 
							c("Covariates", rep("No", 4), rep("Yes", 4))), 
          out = paste(plot.wd, "/tb_", out.file, "_reg_",Sys.Date(), ".html", sep=""))
