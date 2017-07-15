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
out.file	<- "cvs_sl_rbst"
ww			<- 6.5
ar			<- .6
match.method	<- "ps"			# "genetic"
out.file	<- paste(out.file, match.method, sep="_")

load("cvs_smk.rdata")

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

################################################
# Treatment and control from the main analysis #
################################################
mypan			<- subset(smk.pan, !is.na(treat))
mypan			<- mypan[order(mypan$household_code),]
# mypan$distance_cvs	<- log(mypan$distance_cvs)
# mypan$pre_q		<- log(mypan$pre_q)
# mypan$pre_trip_total	<- log(mypan$pre_trip_total)
# mypan$pre_trip_othchannel	<- log(mypan$pre_trip_othchannel)
# mypan$pre_dol_othchannel	<- log(mypan$pre_dol_total)

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

# Take before-after difference
setkeyv(match.shtdat, c("household_code","after"))
match.shtdat	<- match.shtdat[,list(q = diff(q), q_cvs = diff(q_cvs), q_othdrug = diff(q_othdrug), q_othchannel = diff(q_othchannel), 
								trip_cvs = diff(trip_cvs), trip_othdrug = diff(trip_othdrug), trip_othchannel = diff(trip_othchannel), 
								dol_cvs = diff(dol_cvs), dol_othdrug = diff(dol_othdrug), dol_othchannel = diff(dol_othchannel), 
								netdol_cvs = diff(netdol_cvs), netdol_othdrug = diff(netdol_othdrug), netdol_othchannel = diff(netdol_othchannel),
								dol_total = diff(dol_total), netdol = diff(netdol), 
								q_convenience = diff(q_convenience), q_grocery = diff(q_grocery), q_discount = diff(q_discount), 
								q_service = diff(q_service), q_gas = diff(q_gas), q_tobacco = diff(q_tobacco)), 
								by = list(household_code)]
match.shtdat	<- data.frame(match.shtdat)
dim(match.shtdat)
match.shtdat	<- merge(match.shtdat, smk.pan[,c("household_code", "treat", "frac_seg",match.col)], by = "household_code", all.x = T)
dim(match.shtdat)
match.shtdat$dist_dummy	<- ifelse(match.shtdat$distance_cvs <= 2, 1, 0)
match.shtdat	<- match.shtdat[order(match.shtdat$household_code),]
max(abs(mypan$household_code - match.shtdat$household_code))

# Run logit propensity score
my.ratio		<- 1
my.caliper		<- NULL			#.25
(fml			<- as.formula(paste("treat ~", paste(match.col, collapse = "+"))))
psmod  			<- glm(fml,data = mypan, family=binomial )
summary(psmod)
pshat  			<- psmod$fitted
Tr  			<- mypan$treat
X <- model.matrix(update(fml, .~. -1), data= mypan)
if(match.method == "genetic"){
	genout 			<- GenMatch(Tr=Tr, X=X, M=1, pop.size=16, max.generations=10, wait.generations=1, caliper = my.caliper)
	rr	<- Match(Y=match.shtdat$q, Tr=Tr, X=X, Weight.matrix=genout, replace = FALSE, caliper = my.caliper)
}else{
	rr  			<- Match(Y=match.shtdat$q, Tr=Tr, X=pshat, M=my.ratio, replace = FALSE, caliper = my.caliper)
}
print(summary(rr))
cat("Check the treatment variable with index from matching:\n")
table(Tr[rr$index.treated]); table(Tr[rr$index.control])
main.hh	<- mypan[c(rr$index.control, rr$index.treated),c("household_code","treat")]

dv.col		<- c("q", "q_cvs", "q_othdrug", "q_othchannel", 
				"trip_cvs", "trip_othdrug", "trip_othchannel", "netdol_cvs", "netdol_othdrug","netdol_othchannel")
dv.lab		<- c(	paste("Cigar-",c("Total", "CVS", "Other drugstores", "Other channel"), sep=""),
					paste("Trip-",c("CVS", "Other drugstores", "Other channels"), sep=""), 
					paste("NetExp-",c("CVS", "Other drugstores", "Other channels"), sep=""))
cbind(dv.col, dv.lab)					

#############################################
# Robustness: differential holiday effect ? # 
#############################################
# Placebo test: if the event occurs in September 2013
# Only use the households with defined treatment variable
placebo.month	<- 9			# September 2013
match.shtdat		<- data.table(subset(smk.trips, month >= placebo.month - num.month & month <= placebo.month + num.month -1 
									& household_code %in% main.hh$household_code))
match.shtdat		<- match.shtdat[,after:= 1*(month >= placebo.month)]
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

# Take before-after difference
setkeyv(match.shtdat, c("household_code","after"))
match.shtdat	<- match.shtdat[,list(q = diff(q), q_cvs = diff(q_cvs), q_othdrug = diff(q_othdrug), q_othchannel = diff(q_othchannel), 
								trip_cvs = diff(trip_cvs), trip_othdrug = diff(trip_othdrug), trip_othchannel = diff(trip_othchannel), 
								dol_cvs = diff(dol_cvs), dol_othdrug = diff(dol_othdrug), dol_othchannel = diff(dol_othchannel), 
								netdol_cvs = diff(netdol_cvs), netdol_othdrug = diff(netdol_othdrug), netdol_othchannel = diff(netdol_othchannel),
								dol_total = diff(dol_total), netdol = diff(netdol), 
								q_convenience = diff(q_convenience), q_grocery = diff(q_grocery), q_discount = diff(q_discount), 
								q_service = diff(q_service), q_gas = diff(q_gas), q_tobacco = diff(q_tobacco)), 
								by = list(household_code)]
match.shtdat	<- data.frame(match.shtdat)
dim(match.shtdat)
match.shtdat	<- merge(match.shtdat, smk.pan[,c("household_code", "treat", "frac_seg",match.col)], by = "household_code", all.x = T)
dim(match.shtdat)

nn		<- table(main.hh$treat)
est1	<- matrix(NA, length(dv.col), 4, dimnames = list(dv.col, c("Estimate", "Std. Error", "t value", "Pr(>|t|)")))
for(i in 1:length(dv.col)){
	dv.fml			<- as.formula(paste(dv.col[i], "~ treat +", paste(match.col, collapse = "+")))
	est1[i,]		<- coeftest(lm(dv.fml, data = match.shtdat))["treat",]
}
cat("The results of placebo test, suppose the event were in September 2013:\n"); print(round(est1, 2)); cat("\n")

# -------------------------------------------------#
# Control month effect with FE models with 2 years #
tmpdat	<- subset(smk.trips, household_code %in% main.hh$household_code)
est2	<- matrix(NA, length(dv.col), 4, dimnames = list(dv.col, c("Estimate", "Std. Error", "t value", "Pr(>|t|)")))
for(i in 1:length(dv.col)){
	dv.fml			<- as.formula(paste(dv.col[i], "~ treat + treat*after + after+month1"))
	tmp				<- plm(dv.fml, data = tmpdat, index = c("household_code", "month"), model = "within")
	cls.v			<- Cls.se.fn(tmp, cluster.vec = tmpdat[,"household_code"], return.se = FALSE)
	est2[i,]		<- coeftest(tmp, vcov = cls.v)["treat:after",]
}
cat("The results of including month FE with 2-year data:\n"); print(round(est2, 2)); cat("\n")
nn		<- rbind(nn, table(main.hh$treat))

##############################
# Changes after announcement # 
##############################
anc.month	<- 	14		# February 2014
num.month1	<- 2
match.shtdat		<- data.table(subset(smk.trips, month %in% c(11, 13, 14, 15)
									& household_code %in% main.hh$household_code))
match.shtdat		<- match.shtdat[,after:= 1*(month >= anc.month)]
match.shtdat		<- match.shtdat[, list(	q = sum(q)/num.month1, q_cvs = sum(q_cvs)/num.month1, q_othdrug = sum(q_othdrug)/num.month1, q_othchannel = sum(q_othchannel)/num.month1, 
									trip_cvs = sum(trip_cvs)/num.month1, trip_othdrug = sum(trip_othdrug)/num.month1, trip_othchannel = sum(trip_othchannel)/num.month1, 
									dol_cvs = sum(dol_cvs)/num.month1, dol_othdrug = sum(dol_othdrug)/num.month1, dol_othchannel = sum(dol_othchannel)/num.month1, 
									netdol_cvs = sum(netdol_cvs)/num.month1, netdol_othdrug = sum(netdol_othdrug)/num.month1, netdol_othchannel = sum(netdol_othchannel)/num.month1,
									dol_total = sum(dol_total)/num.month1, netdol = sum(netdol)/num.month1, 
									q_convenience = sum(q_convenience)/num.month1, q_grocery = sum(q_grocery)/num.month1, q_discount = sum(q_discount)/num.month1, 
									q_service = sum(q_service)/num.month1, q_gas = sum(q_gas)/num.month1, q_tobacco = sum(q_tobacco)/num.month1), 
									by = list(household_code, after)]
match.shtdat 	<- match.shtdat[, drop:= 1*(length(after) < 2), by = list(household_code)]
table(match.shtdat$drop)									# 1 household did not both 2 period data

# Take before-after difference
setkeyv(match.shtdat, c("household_code","after"))
match.shtdat	<- match.shtdat[,list(q = diff(q), q_cvs = diff(q_cvs), q_othdrug = diff(q_othdrug), q_othchannel = diff(q_othchannel), 
								trip_cvs = diff(trip_cvs), trip_othdrug = diff(trip_othdrug), trip_othchannel = diff(trip_othchannel), 
								dol_cvs = diff(dol_cvs), dol_othdrug = diff(dol_othdrug), dol_othchannel = diff(dol_othchannel), 
								netdol_cvs = diff(netdol_cvs), netdol_othdrug = diff(netdol_othdrug), netdol_othchannel = diff(netdol_othchannel),
								dol_total = diff(dol_total), netdol = diff(netdol), 
								q_convenience = diff(q_convenience), q_grocery = diff(q_grocery), q_discount = diff(q_discount), 
								q_service = diff(q_service), q_gas = diff(q_gas), q_tobacco = diff(q_tobacco)), 
								by = list(household_code)]
match.shtdat	<- data.frame(match.shtdat)
dim(match.shtdat)
match.shtdat	<- merge(match.shtdat, smk.pan[,c("household_code", "treat", "frac_seg",match.col)], by = "household_code", all.x = T)
dim(match.shtdat)

est3	<- matrix(NA, length(dv.col), 4, dimnames = list(dv.col, c("Estimate", "Std. Error", "t value", "Pr(>|t|)")))
for(i in 1:length(dv.col)){
	dv.fml			<- as.formula(paste(dv.col[i], "~ treat +", paste(match.col, collapse = "+")))
	est3[i,]		<- coeftest(lm(dv.fml, data = match.shtdat))["treat",]
}
cat("The results of impacts of announcement in Feb 2014:\n"); print(round(est3, 2)); cat("\n")

nn		<- rbind(nn, table(main.hh$treat))

##########################################
# Potential effects on all CVS shoppers? #
##########################################
# Control: all the smokers who are not CVS shoppers 
# Treated: all CVS shoppers
# Use treat1 as treatment definition
mypan			<- smk.pan
mypan			<- mypan[order(mypan$household_code),]
# mypan$distance_cvs	<- log(mypan$distance_cvs)
# mypan$pre_q		<- log(mypan$pre_q)
# mypan$pre_trip_total	<- log(mypan$pre_trip_total)
# mypan$pre_trip_othchannel	<- log(mypan$pre_trip_othchannel)
# mypan$pre_dol_othchannel	<- log(mypan$pre_dol_total)

match.shtdat		<- data.table(subset(smk.trips, month >= event.month - num.month & month <= event.month + num.month -1 ))
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

# Take before-after difference
setkeyv(match.shtdat, c("household_code","after"))
match.shtdat	<- match.shtdat[,list(q = diff(q), q_cvs = diff(q_cvs), q_othdrug = diff(q_othdrug), q_othchannel = diff(q_othchannel), 
								trip_cvs = diff(trip_cvs), trip_othdrug = diff(trip_othdrug), trip_othchannel = diff(trip_othchannel), 
								dol_cvs = diff(dol_cvs), dol_othdrug = diff(dol_othdrug), dol_othchannel = diff(dol_othchannel), 
								netdol_cvs = diff(netdol_cvs), netdol_othdrug = diff(netdol_othdrug), netdol_othchannel = diff(netdol_othchannel),
								dol_total = diff(dol_total), netdol = diff(netdol), 
								q_convenience = diff(q_convenience), q_grocery = diff(q_grocery), q_discount = diff(q_discount), 
								q_service = diff(q_service), q_gas = diff(q_gas), q_tobacco = diff(q_tobacco)), 
								by = list(household_code)]
match.shtdat	<- data.frame(match.shtdat)
dim(match.shtdat)
match.shtdat	<- merge(match.shtdat, smk.pan[,c("household_code", "treat1", "frac_seg",match.col)], by = "household_code", all.x = T)
dim(match.shtdat)
match.shtdat$dist_dummy	<- ifelse(match.shtdat$distance_cvs <= 2, 1, 0)
match.shtdat	<- match.shtdat[order(match.shtdat$household_code),]
max(abs(mypan$household_code - match.shtdat$household_code))

# Run logit propensity score
# NOTICE: all smokers in the areas with "ban_ard=1" are CVS shoppers
table(smk.pan$treat1)				
table(smk.pan$treat1, smk.pan$treat2)
sel		<- smk.pan$treat1 == 0 & smk.pan$treat2 == 0 & !is.na(smk.pan$treat2)
mean(smk.pan[sel,"ban_ard"])

psmod1  			<- glm(update(fml, treat1 ~ .),data = mypan, family=binomial )
summary(psmod1)
pshat  			<- psmod1$fitted
Tr  			<- mypan$treat1
X <- model.matrix(update(fml, .~. -1), data= mypan)
if(match.method == "genetic"){
	genout 			<- GenMatch(Tr=Tr, X=X, M=1, pop.size=16, max.generations=10, wait.generations=1, caliper = my.caliper)
	rr	<- Match(Y=match.shtdat$q, Tr=Tr, X=X, Weight.matrix=genout, replace = FALSE, caliper = my.caliper)
}else{
	rr  			<- Match(Y=match.shtdat$q, Tr=Tr, X=pshat, M=my.ratio, replace = FALSE, caliper = my.caliper)
}
print(summary(rr))
cat("Check the treatment variable with index from matching:\n")
table(Tr[rr$index.treated]); table(Tr[rr$index.control])

est4	<- matrix(NA, length(dv.col), 4, dimnames = list(dv.col, c("Estimate", "Std. Error", "t value", "Pr(>|t|)")))
sel		<- match.shtdat$household_code %in% mypan[c(rr$index.control, rr$index.treated),"household_code"]
for(i in 1:length(dv.col)){
	dv.fml			<- as.formula(paste(dv.col[i], "~ treat1"))
	est4[i,]		<- coeftest(lm(dv.fml, data = match.shtdat[sel,]))["treat1",]
}
cat("The results of treatment effects using treat1:\n"); print(round(est4, 2)); cat("\n")
nn		<- rbind(nn, table(mypan[c(rr$index.control, rr$index.treated),"treat1"]))

#################################
# Placebo test with non-smokers # 
#################################
load("cvs_nonsmk.rdata")

mypan		<- subset(nonsmk.pan, smk == 0)
summary(mypan$distance_cvs)
mypan$treat	<- 1*(mypan$distance_cvs <= 1)
table(mypan$treat)
match.col2			<- c("income", "age", "have_kids", "employment", "race",  
						"pre_trip_cvs", "pre_trip_othdrug", "pre_trip_othchannel", "pre_dol_cvs", "pre_dol_othdrug", "pre_dol_othchannel")
dv.col2				<- 	c("trip_cvs", "trip_othdrug", "trip_othchannel", "netdol_cvs", "netdol_othdrug", "netdol_othchannel")
dv.lab2				<- c(	paste("Trip-",c("CVS", "Other drugstores", "Other channels"), sep=""), 
							paste("NetExp-",c("CVS", "Other drugstores", "Other channels"), sep=""))
cbind(dv.col2, dv.lab2)							

match.shtdat		<- data.table(subset(nonsmk.trips, month >= event.month - num.month & month <= event.month + num.month -1 & household_code %in% mypan$household_code))
match.shtdat		<- match.shtdat[, list(	
									trip_cvs = sum(trip_cvs)/num.month, trip_othdrug = sum(trip_othdrug)/num.month, trip_othchannel = sum(trip_othchannel)/num.month, 
									dol_cvs = sum(dol_cvs)/num.month, dol_othdrug = sum(dol_othdrug)/num.month, dol_othchannel = sum(dol_othchannel)/num.month, 
									netdol_cvs = sum(netdol_cvs)/num.month, netdol_othdrug = sum(netdol_othdrug)/num.month, netdol_othchannel = sum(netdol_othchannel)/num.month,
									dol_total = sum(dol_total)/num.month, netdol = sum(netdol)/num.month), 
									by = list(household_code, after)]
match.shtdat 	<- match.shtdat[, drop:= 1*(length(after) < 2), by = list(household_code)]
table(match.shtdat$drop)									# 1 household did not both 2 period data

# Take before-after difference
setkeyv(match.shtdat, c("household_code","after"))
match.shtdat	<- match.shtdat[,list(
								trip_cvs = diff(trip_cvs), trip_othdrug = diff(trip_othdrug), trip_othchannel = diff(trip_othchannel), 
								dol_cvs = diff(dol_cvs), dol_othdrug = diff(dol_othdrug), dol_othchannel = diff(dol_othchannel), 
								netdol_cvs = diff(netdol_cvs), netdol_othdrug = diff(netdol_othdrug), netdol_othchannel = diff(netdol_othchannel),
								dol_total = diff(dol_total), netdol = diff(netdol)), 
								by = list(household_code)]
match.shtdat	<- data.frame(match.shtdat)
dim(match.shtdat)
match.shtdat	<- merge(match.shtdat, mypan[,c("household_code", "treat", match.col2)], by = "household_code", all.x=T)

# Run logit propensity score
(fml2			<- as.formula(paste("treat~", paste(match.col2, collapse="+"))))
psmod2  		<- glm(fml2, data = mypan, family=binomial )
summary(psmod1)
pshat  			<- psmod2$fitted
Tr  			<- mypan$treat
X <- model.matrix(update(fml2, .~. -1), data= mypan)
if(match.method == "genetic"){
	genout 			<- GenMatch(Tr=Tr, X=X, M=1, pop.size=16, max.generations=10, wait.generations=1, caliper = my.caliper)
	rr	<- Match(Y=match.shtdat$q, Tr=Tr, X=X, Weight.matrix=genout, replace = FALSE, caliper = my.caliper)
}else{
	rr  			<- Match(Y=match.shtdat$trip_cvs, Tr=Tr, X=pshat, M=my.ratio, replace = FALSE, caliper = my.caliper)
}
print(summary(rr))
cat("Check the treatment variable with index from matching:\n")
table(Tr[rr$index.treated]); table(Tr[rr$index.control])

sel		<- match.shtdat$household_code %in% mypan[c(rr$index.control, rr$index.treated),"household_code"]
est5	<- matrix(NA, length(dv.col2), 4, dimnames = list(dv.col2, c("Estimate", "Std. Error", "t value", "Pr(>|t|)")))
for(i in 1:length(dv.col2)){
	dv.fml			<- as.formula(paste(dv.col2[i], "~ treat"))
	est5[i,]		<- coeftest(lm(dv.fml, data = match.shtdat[sel,]))["treat",]
}
cat("The results of treatment effects among non-smokers:\n"); print(round(est5, 2)); cat("\n")
nn		<- rbind(nn, table(mypan[c(rr$index.control, rr$index.treated),"treat"]))

#####################
# Summarize results # 
#####################
class(est1)	<- class(est2)	<- class(est3)	<- class(est4)	<- class(est5)	<- "coeftest"

myline		<- list(c("Treated", paste(nn[,"1"], c("CVS+cigar", "CVS+cigar",  "CVS+cigar","Smokers+CVS", "Non-smokers close to CVS"))), 
					c("Control", paste(nn[,"0"], c("CVS-cigar", "CVS-cigar", "CVS-cigar","Smokers-CVS", "Non-smokers far from CVS")))
					)
stargazer(list(est1, est2, est3, est4, est5), type = "text", 
		column.labels = c("Placebo test 2013", "Month FE", "Announcement","Treatment1", "Non-smoker placebo test"), 
		add.lines = myline)

stargazer(list(est1, est2, est3, est4, est5), 
		  type = "latex", align = FALSE, no.space = FALSE, digits = 2,
		  column.labels = c("Placebo test 2013", "Month FE", "Announcement","Treatment1", "Non-smoker placebo test"),
		  covariate.labels = dv.lab,
          title = "Robustness Check",
		  add.lines = myline)
		
# sink()

stargazer(list(est1, est2, est3, est4), 
		  type = "html", align = FALSE, no.space = FALSE, digits = 2,
		  column.labels = c("Placebo test 2013", "Month FE", "Treatment1", "Non-smoker placebo test"),
          title = "Robustness Check", label = "tab:robust",
		  add.lines = myline,
	      covariate.labels = dv.lab,
          out = paste(plot.wd, "/tb_", out.file, "_",Sys.Date(), ".html", sep=""))

cat("This program is done.\n")
