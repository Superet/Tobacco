library(reshape2)
library(ggplot2)
library(gridExtra)
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
out.file	<- "cvs_sl_smk_het"
ww			<- 6.5
ar			<- .6
match.method	<- "ps"			# "genetic"
out.file	<- paste(out.file, match.method, sep="_")
match.code	<- 1

load("cvs_smk.rdata")
load(paste("cvs_smk_mch", match.code, "_id.rdata",sep=""))

# Define the construction of treatment and control
# 1: Control (smokers who never shopped at CVS), Treament (smokers who are also CVS shoppers)
# 2: Control (smokers who shopped at CVS but did not buy cigarettes), Treament (smokers who also purchased cigarettes at CVS)
# 3: Control (smokers who shopped at CVS but did not buy cigarettes), Treament (smokers who spent more than 17% CVS spending on cigarettes)
treat.code	<- 2	
(out.file 	<- paste(out.file, treat.code, sep="")	)	
smk.pan$treat	<- smk.pan[,paste("treat", treat.code, sep="")]
smk.trips$treat	<- smk.trips[,paste("treat",treat.code,sep="")]

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
tmp		<- data.table(subset(smk.trips, month < event.month))
tmp.netdol	<- tmp[,list(	netdol 				= mean(netdol), 
						netdol_cvs			= mean(netdol_cvs), 
						netdol_othdrug		= mean(netdol_othdrug), 
						netdol_othchannel	= mean(netdol_othchannel)), 
				by = list(household_code)]
summary(tmp.netdol)

# Only use the households with defined treatment variable
mypan			<- subset(smk.pan, !is.na(treat))
mypan			<- merge(mypan, tmp.netdol, by = "household_code", all.x = T)
mypan			<- mypan[order(mypan$household_code),]
# mypan$distance_cvs	<- log(mypan$distance_cvs)
# mypan$pre_q		<- log(mypan$pre_q)
# mypan$pre_trip_total	<- log(mypan$pre_trip_total)
# mypan$pre_trip_othchannel	<- log(mypan$pre_trip_othchannel)
# mypan$pre_dol_othchannel	<- log(mypan$pre_dol_total)
mypan$s2		<- with(mypan, 1*(treat==1 & frac_seg == "S2"))						# Cigdol_cvs/dol_cvs
mypan$cls_wgr	<- with(mypan, 1*(distance_walgreens <= 1.5))
tmp				<- data.table(subset(smk.trips, month < event.month))
tmp				<- tmp[,list(cvs_shr = sum(cigdol_cvs)/sum(cigdol)), by= list(household_code)]		# cigdol_cvs/cigdol
mypan			<- merge(mypan, tmp, by = "household_code", all.x=T)
mypan$large_cvs	<- 1*(mypan$cvs_shr > .1)	
mypan$cvs_shopper<- ifelse(mypan$netdol_cvs > 10, 1, 0)										

table(mypan$treat, mypan$heavy)
table(mypan$treat, mypan$cvs_shopper)
# table(mypan$treat, mypan$s2)
# table(mypan$treat, mypan$large_cvs)
table(mypan$treat, mypan$cls_wgr)

# See the difference 
selcol			<- c("distance_cvs", "pre_q", "pre_trip_cvs", "pre_trip_othdrug", "pre_trip_othchannel", "pre_trip_total", 
 					"pre_dol_cvs", "pre_dol_othdrug", "pre_dol_othchannel", "pre_dol_total")
grp.col	<- c("heavy", "cvs_shopper", "cls_wgr")
grp.lab	<- c("Heavy smokers", "Ligh smokers", 
			 "Large CVS spenders", "Samll CVS spenders",
			 "Near Walgreens", "Far from Walgreens")
tmp				<- matrix(NA, length(selcol), 3*length(grp.col), dimnames = list(selcol, c(rbind(matrix(grp.lab, 2, length(grp.col)), ""))))
sel1			<- list(mypan$heavy 	== 1, 
						mypan$cvs_shopper== 1,
						mypan$cls_wgr	== 1)
for(i in selcol){
	for(j in 1:length(sel1)){
		tmp1	<- t.test(mypan[sel1[[j]], i], mypan[!sel1[[j]], i])
		tmp[i,((j-1)*3+1:3)]	<- c(tmp1$estimate, tmp1$p.value)
	}
}
cat("Difference between groups:\n"); 
for(j in 1:length(sel1)){ print(round(tmp[,((j-1)*3+1:3)],2));cat("\n") }

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
match.shtdat	<- merge(match.shtdat, mypan[,c("household_code", "treat", "frac_seg", "s2", "large_cvs","heavy", "cls_wgr", "cvs_shopper", match.col)], 
						by = "household_code", all.x = T)
dim(match.shtdat)
match.shtdat$dist_dummy	<- ifelse(match.shtdat$distance_cvs <= 2, 1, 0)
match.shtdat	<- match.shtdat[order(match.shtdat$household_code),]
max(abs(mypan$household_code - match.shtdat$household_code))

# # Run logit propensity score
# my.ratio		<- 1
# my.caliper		<- NULL		#.25
# (fml			<- as.formula(paste("treat ~", paste(match.col, collapse = "+"))))
# psmod  			<- glm(fml,data = mypan, family=binomial )
# summary(psmod)
# pshat  			<- psmod$fitted
# Tr  			<- mypan$treat
# X <- model.matrix(update(fml, .~. -1), data= mypan)
# if(match.method == "genetic"){
# 	genout 			<- GenMatch(Tr=Tr, X=X, M=1, pop.size=16, max.generations=10, wait.generations=1, caliper = my.caliper)
# 	rr	<- Match(Y=match.shtdat$q, Tr=Tr, X=X, Weight.matrix=genout, replace = FALSE, caliper = my.caliper)
# }else{
# 	rr  			<- Match(Y=match.shtdat$q, Tr=Tr, X=pshat, M=my.ratio, replace = FALSE, caliper = my.caliper)
# }
# print(summary(rr))
# cat("Check the treatment variable with index from matching:\n")
# table(Tr[rr$index.treated]); table(Tr[rr$index.control])

# --------------------------- #
# Run regressions for each DV #
dv.col		<- c("q", "q_cvs", "q_othdrug", "q_othchannel", 
				"trip_cvs", "trip_othdrug", "trip_othchannel", "netdol_cvs", "netdol_othdrug","netdol_othchannel")
dv.lab		<- c(	paste("Cigar-",c("Total", "CVS", "Other drug stores", "Other channels"), sep=""),
					paste("Trip-",c("CVS", "Other drug stores", "Other channels"), sep=""), 
					paste("NetExp-",c("CVS", "Other drug stores", "Other channels"), sep=""))
cbind(dv.col, dv.lab)					
				
est.cvsshopper	<- est.heavy	<- est.wgr	<- matrix(NA, length(dv.col)*2, 4, 
												dimnames = list(NULL, c("Estimate", "Std. Error", "t value", "Pr(>|t|)")))
cov.cvsshopper	<- cov.heavy	<- cov.wgr	<- matrix(NA, length(dv.col), 2, dimnames = list(dv.col, c(0, 1)))

sel			<- match.shtdat$household_code %in% c(match.ps)

for(i in 1:length(dv.col)){
	selidx			<- 1:2 + (i-1)*2
	(dv.fml			<- as.formula(paste(dv.col[i], "~ treat + heavy + treat*heavy +", paste(match.col, collapse = "+"))))
	
	tmp					<- lm(dv.fml, data = match.shtdat[sel,])
	v					<- vcov(tmp)[c("treat","treat:heavy"),c("treat","treat:heavy")]
	est.heavy[selidx,]		<- coeftest(tmp)[c("treat","treat:heavy"),]
	cov.heavy[i,]			<- c(v[1], sum(v))
	
	tmp					<- lm(update(dv.fml, .~.-heavy-treat*heavy+cvs_shopper + treat*cvs_shopper), data = match.shtdat[sel,])
	v					<- vcov(tmp)[c("treat","cvs_shopper:treat"),c("treat","cvs_shopper:treat")]
	est.cvsshopper[selidx,]<- coeftest(tmp)[c("treat","cvs_shopper:treat"),]
	cov.cvsshopper[i,]	<- c(v[1], sum(v))
	
	tmp					<- lm(update(dv.fml, .~.-heavy-treat*heavy+cls_wgr+treat*cls_wgr), data = match.shtdat[sel,])
	v					<- vcov(tmp)[c("treat","cls_wgr:treat"),c("treat","cls_wgr:treat")]
	est.wgr[selidx,]	<- coeftest(tmp)[c("treat","cls_wgr:treat"),]
	cov.wgr[i,]			<- c(v[1], sum(v))
}
df	<- lm(dv.fml, data = match.shtdat[sel,])$df.residual

# Print results 
sel 	<- seq(2, nrow(est.heavy), 2)
tmp1	<- list(est.heavy[-sel,], est.heavy[sel,])
tmp1	<- lapply(tmp1, function(x) {rownames(x) <- dv.col; class(x) <- c("coeftest", "matrix"); x})
names(tmp1)	<- c("Treat", "Treat*Heavy")
stargazer(tmp1, type = "text", column.labels = c("Treat", "Treat*Heavy"))

tmp2	<- list(est.cvsshopper[-sel,], est.cvsshopper[sel,])
tmp2	<- lapply(tmp2, function(x) {rownames(x) <- dv.col; class(x) <- c("coeftest", "matrix"); x})
names(tmp2)	<- c("Treat", "Treat*LargeCVS_Share")
stargazer(tmp2, type = "text", column.labels = c("Treat", "Treat*CVS_shopper"))


tmp3	<- list(est.wgr[-sel,], est.wgr[sel,])
tmp3	<- lapply(tmp3, function(x) {rownames(x) <- dv.col; class(x) <- c("coeftest", "matrix"); x})
names(tmp3)	<- c("Treat", "Treat*CloseWalgreens")
stargazer(tmp3, type = "text", column.labels = c("Treat", "Treat*CloseWalgreens"))

# stargazer(list(tmp1, tmp2, tmp3), type = "html", 
# 		align = TRUE, no.space = TRUE, digits = 2,	
# 		title = "Heterogeneous effects ", 
# 		covariate.labels = dv.lab,
# 		column.labels = c(names(tmp1), names(tmp2), names(tmp3)), 
# 		out = paste(plot.wd, "/tb_", out.file, "_",Sys.Date(), ".html", sep=""))

# grp.lab	<- c("Cigarette-driven CVS shoppers", "Non cigarette-driven CVS shoppers", 
# 			 "Heavy smokers", "Ligh smokers", 
# 			 "Near Walgreens", "Far from Walgreens", 
# 			 "Large share of\ncigarette wallet at CVS", "Small share of\ncigarette wallet at CVS")
tmp1	<- c(est.heavy[,"Estimate"], est.cvsshopper[,"Estimate"], est.wgr[,"Estimate"])
sel		<- seq(2, length(tmp1), 2)
tmp1[sel]	<- tmp1[-sel] + tmp1[sel]
tmp2	<- c(t(rbind( cov.heavy, cov.cvsshopper, cov.wgr)))
sel		<- sapply(1:length(grp.col), function(x) rep(c(2*x, 2*x-1), length(dv.lab)))
ggtmp	<- data.frame(	Model 	= rep(c("Consumption", "CVS shopper", "Walgreens"), each = nrow(est.heavy)), 
						Group 	= grp.lab[c(sel)], 
						DV 		= rep(rep(dv.lab, each = 2), length(grp.col)), 
						Estimate = tmp1, se = sqrt(tmp2))
ggtmp$Channel	<- sub(".*-", "", ggtmp$DV)
ggtmp$Channel	<- factor(ggtmp$Channel, levels = c("CVS", "Other drug stores", "Other channels", "Total"))
ggtmp$Vargrp	<- factor(sub("-.*", "", ggtmp$DV), levels = c("Cigar", "Trip", "NetExp"), 
							labels = c("Cigarette purchases", "No. of trips", "Exp. on other categories"))
ggtmp$Group		<- factor(ggtmp$Group, levels = grp.lab)

sel.mod	<- unique(ggtmp$Model)
# col.v	<- c("#3182bd", "#deebf7")
col.v	<- c("#08519c", "#bdd7e7")

pdf(paste(plot.wd,"/fg_", out.file, ".pdf",sep=""), width = ww, height = ww*ar)
for(i in 1:length(sel.mod)){
	print(
		ggplot(subset(ggtmp, Model == sel.mod[i]), aes(Channel, Estimate, fill = Group)) + 						
				geom_bar(stat = "identity", width = .5, position=position_dodge(width=0.5)) + 
				geom_errorbar(aes(ymin=Estimate - 1.96*se, ymax=Estimate + 1.96 * se, group = Group), 
								position=position_dodge(width=0.5), size = .3, width = .2) + 
				facet_wrap(~ Vargrp, scales = "free") + 
				scale_fill_manual(name = "", values = col.v) + 
				theme_bw() + 
				# guides(fill = guide_legend(title = "")) + 
				# theme(axis.text = element_text(size = rel(0.7)), axis.title = element_text(size = rel(0.7)), 
				theme(
						axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1), 
					legend.position = "bottom")
		)
}
dev.off()							
