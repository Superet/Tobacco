library(reshape2)
library(ggplot2)
library(data.table)
library(lubridate)
library(stargazer)
library(zipcode)
library(plm)
library(lme4)
library(xlsx)
library(MatchIt)
library(lmtest)
library(Matching)

load('~/Downloads/matched_treat2.RData')

event.date	<- as.Date("2014-09-01", format = "%Y-%m-%d")			# Placebo event 
event.month	<- month(event.date) + 12

# My old results 
# Aggregate over 3 month window around the event 
match.shtdat		<- data.table(subset(smk.trips, household_code %in% smk.pan$household_code & month >= event.month - 3 & month <= event.month + 2))
match.shtdat		<- match.shtdat[, list(	q = sum(q), q_cvs = sum(q) - sum(q_othdrug) - sum(q_othchannel), q_othdrug = sum(q_othdrug), q_othchannel = sum(q_othchannel), 
									trip_cvs = sum(trip_cvs), trip_othdrug = sum(trip_othdrug), trip_othchannel = sum(trip_othchannel), 
									dol_cvs = sum(dol_cvs), dol_othdrug = sum(dol_othdrug), dol_othchannel = sum(dol_othchannel), 
									netdol_cvs = sum(netdol_cvs), netdol_othdrug = sum(netdol_othdrug), netdol_othchannel = sum(netdol_othchannel),
									dol_total = sum(dol_total), netdol = sum(netdol)), 
									by = list(treat2, household_code, after)]
match.shtdat 	<- match.shtdat[, drop:= 1*(length(after) < 2), by = list(household_code)]
table(match.shtdat$drop)									# 1 household did not both 2 period data
match.shtdat 	<- subset(match.shtdat, drop == 0)				
setkeyv(match.shtdat, c("household_code","after"))
match.shtdat	<- match.shtdat[,list(q = diff(q), q_cvs = diff(q_cvs), q_othdrug = diff(q_othdrug), q_othchannel = diff(q_othchannel), 
								trip_cvs = diff(trip_cvs), trip_othdrug = diff(trip_othdrug), trip_othchannel = diff(trip_othchannel), 
								dol_cvs = diff(dol_cvs), dol_othdrug = diff(dol_othdrug), dol_othchannel = diff(dol_othchannel), 
								netdol_cvs = diff(netdol_cvs), netdol_othdrug = diff(netdol_othdrug), netdol_othchannel = diff(netdol_othchannel),
								dol_total = diff(dol_total), netdol = diff(netdol)), 
								by = list(household_code, treat2)]
match.shtdat	<- data.frame(match.shtdat)
for(i in colnames(match.shtdat)[-c(1:2)]){
	match.shtdat[,i]	<- match.shtdat[,i]/3
}
match.shtdat	<- match.shtdat[order(match.shtdat$household_code),]
mypan1			<- smk.pan[order(smk.pan$household_code), ]
mypan1			<- subset(mypan1, household_code %in% match.shtdat$household_code)
max(abs(mypan1$household_code - match.shtdat$household_code))

# Run logit propensity score
my.ratio	<- 1
fml			<- treat2 ~ income + age + have_kids + employment + race + distance_cvs +
                pre_q + pre_trip_total + pre_trip_othchannel+ pre_dol_total + pre_dol_othchannel
psmod  		<- glm(fml,data = mypan1, family=binomial )
summary(psmod)
pshat  		<- psmod$fitted
Tr  		<- mypan1$treat2

# Run matching for each DV 
(dv.col 	<- setdiff(names(match.shtdat), c( "treat2", "household_code","after")))
att.est		<- att.est1	<- matrix(NA, length(dv.col), 4)
dimnames(att.est)	<- dimnames(att.est1) <- list(dv.col, c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
my.commonspt		<- TRUE
my.caliper			<- NULL
for(i in 1:length(dv.col)){
	# Matching without replacement 
	# Notice that without replacement, the funciton does not return Abadie-Imbenns se. 
	rr  	<- Match(Y=match.shtdat[,dv.col[i]], Tr=Tr, X=pshat, M=my.ratio, 
					replace = FALSE, CommonSupport = my.commonspt, caliper = my.caliper)
	print(summary(rr))
	att.est[i,]	<- c(rr$est, rr$se.standard, rr$est/rr$se.standard, 2*pt(-abs(rr$est/rr$se.standard), df = rr$wnobs -1 ))
	
	# Matching with replacement 
	rr1  	<- Match(Y=match.shtdat[,dv.col[i]], Tr=Tr, X=pshat, M=my.ratio, 
					replace = TRUE, CommonSupport = my.commonspt, caliper = my.caliper)
	print(summary(rr1))
	att.est1[i,]	<- c(rr1$est, rr1$se, rr1$est/rr1$se, 2*pt(-abs(rr1$est/rr1$se), df = rr1$wnobs-1 ))	
}
class(att.est)	<- class(att.est1) <- "coeftest"
table(rr$weights); table(rr1$weights)


# Aggregate over 3 month window around the event 
match.shtdat		<- subset(smk.trips, household_code %in% smk.pan[c(c_id, t_id),"household_code"])
match.shtdat		<- data.table(subset(match.shtdat, month >= event.month - 3 & month <= event.month + 2))
table(match.shtdat$month)
match.shtdat		<- match.shtdat[, list(	q = sum(q), q_cvs = sum(q) - sum(q_othdrug) - sum(q_othchannel), q_othdrug = sum(q_othdrug), q_othchannel = sum(q_othchannel), 
									trip_cvs = sum(trip_cvs), trip_othdrug = sum(trip_othdrug), trip_othchannel = sum(trip_othchannel), 
									dol_cvs = sum(dol_cvs), dol_othdrug = sum(dol_othdrug), dol_othchannel = sum(dol_othchannel), 
									netdol_cvs = sum(netdol_cvs), netdol_othdrug = sum(netdol_othdrug), netdol_othchannel = sum(netdol_othchannel),
									dol_total = sum(dol_total), netdol = sum(netdol)), 
									by = list(treat2, household_code, after)]
match.shtdat 	<- match.shtdat[, drop:= 1*(length(after) < 2), by = list(household_code)]
table(match.shtdat$drop)
setkeyv(match.shtdat, c("household_code","after"))
match.shtdat	<- match.shtdat[,list(q = diff(q), q_cvs = diff(q_cvs), q_othdrug = diff(q_othdrug), q_othchannel = diff(q_othchannel), 
								trip_cvs = diff(trip_cvs), trip_othdrug = diff(trip_othdrug), trip_othchannel = diff(trip_othchannel), 
								dol_cvs = diff(dol_cvs), dol_othdrug = diff(dol_othdrug), dol_othchannel = diff(dol_othchannel), 
								netdol_cvs = diff(netdol_cvs), netdol_othdrug = diff(netdol_othdrug), netdol_othchannel = diff(netdol_othchannel),
								dol_total = diff(dol_total), netdol = diff(netdol)), 
								by = list(household_code, treat2)]
match.shtdat	<- data.frame(match.shtdat)
match.shtdat	<- match.shtdat[order(match.shtdat$household_code),]
for(i in colnames(match.shtdat)[-c(1:2)]){
	match.shtdat[,i]	<- match.shtdat[,i]/3
}

# T-test
c_idx	<- sapply(c_id, function(i) which(match.shtdat$household_code == smk.pan[i,"household_code"]))
t_idx	<- sapply(t_id, function(i) which(match.shtdat$household_code == smk.pan[i,"household_code"]))
max(abs(match.shtdat[c_idx,"household_code"] - smk.pan[c_id, "household_code"]))
dv.col	<- colnames(match.shtdat)[-c(1:2)]
ggtab	<- ggtab1	<- NULL
for(i in dv.col){
	tmp		<- t.test(match.shtdat[,i] ~ match.shtdat$treat2)
	tmp1	<- c(dif = diff(tmp$estimate), t = -tmp$statistic, p = tmp$p.value)
	ggtab	<- rbind(ggtab, tmp1)
	
	# Paired t-test
	tmp		<- t.test(match.shtdat[t_idx,i], match.shtdat[c_idx,i], paired = T)
	tmp1	<- c(dif = tmp$estimate, t = tmp$statistic, p = tmp$p.value)
	ggtab1	<- rbind(ggtab1, tmp1)
}
ggtab	<- cbind(ggtab[,1], ggtab[,1]/ggtab[,2], ggtab[,c(2,3)])
ggtab1	<- cbind(ggtab1[,1], ggtab1[,1]/ggtab1[,2], ggtab1[,c(2,3)])
dimnames(ggtab)	<- dimnames(ggtab1)	<- list(dv.col, c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
class(ggtab)	<- "coeftest"
class(ggtab1)	<- "coeftest"

stargazer(list(Mean = ggtab, Paired = ggtab1, att.est, att.est1), type = "text", no.space = T, dep.var.caption = "", 
		column.labels = c("Group", "Paired", "Old w/o replacement", "Old w/.replacement"))

stargazer(list(Mean = ggtab, Paired = ggtab1, att.est, att.est1), type = "html", no.space = T, dep.var.caption = "", 
		column.labels = c("Group", "Paired", "Old w/o replacement", "Old w/.replacement"),
		out = "~/Desktop/matching.html")





