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
out.file	<- "cvs_sl_smk"
ww			<- 5
ww1			<- 6
ar			<- .6
match.code	<- 1
# out.file	<- paste(out.file, match.method, sep="_")

load("cvs_smk.rdata")
load(paste("cvs_smk_mch", match.code, "_id.rdata", sep=""))

# Define the construction of treatment and control
# 1: Control (smokers who never shopped at CVS), Treament (smokers who are also CVS shoppers)
# 2: Control (smokers who shopped at CVS but did not buy cigarettes), Treament (smokers who also purchased cigarettes at CVS)
# 3: Control (smokers who shopped at CVS but did not buy cigarettes), Treament (smokers who spent more than 17% CVS spending on cigarettes)
treat.code	<- 2	
(out.file 	<- paste(out.file, treat.code, sep="")	)	
smk.pan$treat	<- smk.pan[,paste("treat", treat.code, sep="")]
smk.trips$treat	<- smk.trips[,paste("treat",treat.code,sep="")]
# sink(paste(plot.wd, "/log_", out.file, ".txt", sep=""), append = FALSE)

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

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

#######################
# Run DID regressions # 		
#######################
# Only use the households with defined treatment variable
mypan			<- subset(smk.pan, !is.na(treat))
mypan			<- mypan[order(mypan$household_code),]
mypan$log_distance_cvs	<- log(mypan$distance_cvs)
mypan$log_pre_q		<- log(mypan$pre_q)
mypan$log_pre_trip_total	<- log(mypan$pre_trip_total)
mypan$log_pre_trip_othchannel	<- log(mypan$pre_trip_othchannel)
mypan$log_pre_dol_othchannel	<- log(mypan$pre_dol_othchannel)
mypan$log_pre_dol_total	<- log(mypan$pre_dol_total)

# Aggregate over 3 month window around the event 
num.month			<- 3
match.col			<- c("income", "age", "have_kids", "employment", "race", "log_distance_cvs",
                		 "log_pre_q", "log_pre_trip_total", "log_pre_trip_othchannel", "log_pre_dol_total", "log_pre_dol_othchannel")
(fml			<- as.formula(paste("treat ~", paste(match.col, collapse = "+"))))
match.shtdat		<- data.table(subset(smk.trips, month >= event.month - num.month & month <= event.month + num.month -1 & !is.na(treat)))
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
match.shtdat	<- merge(match.shtdat, mypan[,c("household_code", "treat", "frac_seg",match.col)], by = "household_code", all.x = T)
dim(match.shtdat)
match.shtdat$dist_dummy	<- ifelse(match.shtdat$log_distance_cvs <= log(2), 1, 0)
match.shtdat	<- match.shtdat[order(match.shtdat$household_code),]
max(abs(mypan$household_code - match.shtdat$household_code))
# 
# # Run logit propensity score
# my.ratio		<- 1
# my.caliper		<- NULL			#.25
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

# ---------------------- #
# Check matching balance #
numbt	<- 500
# mb	<- MatchBalance(fml, data=mypan, match.out=rr, nboots=numbt)
tmp1<- MatchBalance(fml, data=mypan, nboots=numbt)																				
tmp1<- sapply(tmp1$BeforeMatching, function(x) c(x$mean.Co,x$mean.Tr, x$sdiff.pooled/100, x$tt$p.value, 
										ifelse("ks" %in% names(x), x$ks$ks.boot.pvalue, x$tt$p.value) ))
tmp2<- MatchBalance(fml, data=subset(mypan, household_code %in% c(match.ps)), nboots=numbt)										
tmp2<- sapply(tmp2$BeforeMatching, function(x) c(x$mean.Co,x$mean.Tr, x$sdiff.pooled/100, x$tt$p.value, 
										ifelse("ks" %in% names(x), x$ks$ks.boot.pvalue, x$tt$p.value) ))
tmp3<- MatchBalance(fml, data=subset(mypan, household_code %in% c(match.bipart)), nboots=numbt)															
tmp3<- sapply(tmp3$BeforeMatching, function(x) c(x$mean.Co,x$mean.Tr, x$sdiff.pooled/100, x$tt$p.value, 
										ifelse("ks" %in% names(x), x$ks$ks.boot.pvalue, x$tt$p.value) ))

tmp	<- model.matrix(glm(fml,data = mypan, family=binomial ))[,-1]		
tmp.lab	<- c("Income", "Age", "Have kids", paste("Employment -", levels(mypan$employment)[-1]), paste("Race -", levels(mypan$race)[-1]), 
			"log(Distance to CVS)", "log(Cigarette consumption)", "log(No. total trips)", "log(No. trips to other channels)", 
			"log(Total expenditure)", "log(Expenditure at other channels)")								
cbind(colnames(tmp), tmp.lab)			
dimnames(tmp1)	<- dimnames(tmp2)	<-	dimnames(tmp3) <-list(c("Mean Control", "Mean Treatment", "Std difference","t p-value", "KS bootstrap p-value"), tmp.lab)
blc	<- cbind(t(tmp1), t(tmp2), t(tmp3))
nn	<- setNames(c(length(unique(match.ps[,"control"])), length(unique(match.ps[,"treat"])) ), 
		c("Control", "Treated" ))
cat("The number of unique households:\n"); print(nn); cat("\n")
cat("Balance check before matching (1-4), propensity score (5-8), bipart matching (9-12):\n"); print(round(blc,2)); cat("\n")

# ------- #
# QQ plot #
selcol	<- c("distance_cvs","pre_q", "pre_trip_total", "pre_trip_othchannel", "pre_dol_total", "pre_dol_othchannel")
col.tab	<- c("Distance to CVS", "Cigarette quantity", "No. total trips", "No. trips to\n other channels", "Total expenditure", 
			"Expenditure at\n other channels")
# selcol	<- paste("log_",selcol,sep="")			
# col.tab	<- paste("log(",col.tab,")", sep="")	
tmpx		<- exp(tmp[,paste("log_",selcol,sep="")])
colnames(tmpx)	<- selcol
X			<- cbind(tmp,tmpx)
cbind(selcol, col.tab)
sel0	<- mypan$treat == 1									# Index of the treated smokers
sel1	<- mypan$treat == 0									# Index of control 
sel2	<- mypan$household_code %in% match.ps[,"control"]		# Index of control from propensity
sel3	<- mypan$household_code %in% match.bipart[,"treat"] 	# Index of treated from bipartite
sel4	<- mypan$household_code %in% match.bipart[,"control"]	# Index of control from bipartite
ggtmp	<- data.frame(NULL)
for(i in selcol){
	tmp1	<- qqplot(X[sel0,i], X[sel1,i], plot.it = FALSE)
	tmp2	<- qqplot(X[sel0,i], X[sel2,i], plot.it = FALSE)
	tmp3	<- qqplot(X[sel3,i], X[sel4,i], plot.it = FALSE)
	tmp		<- rbind(	data.frame(Var = i, Matching = "Before matching", Treated = tmp1$x, Control = tmp1$y), 
						data.frame(Var = i, Matching = "Propensity", Treated = tmp2$x, Control = tmp2$y), 
						data.frame(Var = i, Matching = "Bipartite", Treated = tmp3$x, Control = tmp3$y) )
	ggtmp	<- rbind(ggtmp, tmp)	
}
ggtmp$variable <- factor(ggtmp$Var, levels = selcol, labels = col.tab)

plots	<- list(NULL)
col.v	<- c("#000000", "#E69F00", "#56B4E9")
for(i in 1:length(selcol)){
	plots[[i]]	<- ggplot(subset(ggtmp, Var == selcol[i]), aes(Treated, Control, color = Matching, shape = Matching)) + 
						geom_point(size = rel(.75), alpha = .7) + 	
						geom_abline(intercept = 0, slope = 1, color = "red", linetype = 2, size = .25) + 
						xlim(range(X[,selcol[i]])) + ylim(range(X[,selcol[i]])) + 
						# scale_colour_grey() + 
						scale_color_manual(name = "", values = col.v) +
						facet_wrap(~ variable, scales = "free") + 
						theme_bw()	+ 
						theme(legend.position = "bottom") + 
						guides(alpha = FALSE, shape = guide_legend(title=""), color = guide_legend(override.aes= list(alpha = 1))) 
}
legend 		<- get_legend(plots[[1]])												
blankPlot	<- ggplot() +geom_blank(aes(1,1))+
					theme(	plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
	   						panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), 
	   						axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank() )

pdf(paste(plot.wd, "/fg_", out.file,"_qq_single.pdf",sep = ""), width = 4, height = 4.2)
print(plots[[2]])		
dev.off()

plots		<- lapply(plots, function(x) x+theme(legend.position = "none"))
hh		<- ww1*ar
pdf(paste(plot.wd, "/fg_", out.file,"_qq.pdf",sep = ""), width = ww1, height = ww1*ar)
grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], blankPlot, legend,blankPlot, ncol = 3, 
			heights = unit(c(hh*3/7,hh*3/7,hh/7), c("in", "in", "in")) )		
dev.off()		

###############################
# Run regressions for each DV #
###############################
dv.col		<- c("q", "q_cvs", "q_othdrug", "q_othchannel", 
				"q_convenience", "q_grocery", "q_discount", "q_service", "q_tobacco",
				"trip_cvs", "trip_othdrug", "trip_othchannel", "netdol_cvs", "netdol_othdrug","netdol_othchannel"
				)
dv.lab		<- c(paste("Cigar-",c("Total", "CVS", "Other drugstores", "Other channels"), sep=""), 
				paste("Cigar-",c("Convenience", "Grocery", "Discount stores", "Sevice station", "Tabacco stores"), sep="") , 
				paste("Trip-",c("CVS", "Other drugstores", "Other channels"), sep=""), 
				paste("NetExp-",c("CVS", "Other drugstores", "Other channels"), sep="")
				)	
cbind(dv.col, dv.lab)							

est.bipart	<- est.ols		<- est.mat	<- est.iv	<- est.fe	<- matrix(NA, length(dv.col), 4, 
															dimnames = list(dv.col, c("Estimate", "Std. Error", "t value", "Pr(>|t|)")))

sel			<- match.shtdat$household_code %in% c(match.ps)
sel.bp		<- match.shtdat$household_code %in% c(match.bipart)
table(mypan[sel,"frac_seg"])
tmpdat		<- subset(smk.trips, household_code %in% c(match.ps) &
								month >= event.month - num.month & month <= event.month + num.month -1)
tmpdat		<- merge(tmpdat, mypan[,c("household_code", setdiff(match.col,names(tmpdat)))], by = "household_code", all.x = T)								

for(i in 1:length(dv.col)){
	dv.fml			<- as.formula(paste(dv.col[i], "~ treat"))
	# OLS
	est.ols[i,]	<- coeftest(lm(dv.fml, data = match.shtdat))["treat",]
	
	# Matching without replacement 
	est.mat[i,]	<- coeftest(lm(dv.fml, data = match.shtdat[sel,]))["treat",]
	
	# Distance as IV
	est.iv[i,]	<- coeftest(ivreg(dv.fml, instruments = ~ log_distance_cvs, data = match.shtdat))["treat",]
	
	# Fixed effect model with matched households
	tmp		<- plm(as.formula(paste(dv.col[i], "~ treat + treat*after + after")), data = tmpdat, 
					index = c("household_code", "month"), model = "within")
	cls.v	<- Cls.se.fn(tmp, cluster.vec = tmpdat[,"household_code"], return.se = FALSE)
	est.fe[i,]	<- coeftest(tmp, vcov = cls.v)["treat:after",]
	
	# Bipart matching 
	est.bipart[i,]	<- coeftest(lm(dv.fml, data = match.shtdat[sel.bp,]))["treat",]
}

# Print results 
class(est.bipart)	<- class(est.ols)	<- class(est.mat) <- class(est.iv) <- class(est.fe) <- c("coeftest", "matrix")
stargazer(list(OLS = est.ols, Matching = est.mat, IV = est.iv, Panel = est.fe, Bipart = est.bipart), type = "text", 
			column.labels = c("OLS", "Matching", "IV", "Panel", "Bipartite"))


# Plot estiamtes 
nn1		<- rbind(table(mypan$treat), table(mypan[sel,"treat"]), table(mypan[sel.bp,"treat"]))
ggtmp <- rbind(	data.frame(Model = "DID", Var = dv.lab, est.ols), 
				data.frame(Model = "DID on \nthe matched \n(propensity)", Var = dv.lab, est.mat), 
				data.frame(Model = "DID on \nthe matched \n(bipartite)", Var = dv.lab, est.bipart))
ggtmp$Channel	<- rep(sub(".*-", "", dv.lab), 3)
ggtmp$Channel	<- factor(ggtmp$Channel, levels = c("CVS", "Other drugstores", "Other channels", "Total", 
													"Sevice station", "Grocery", "Tabacco stores", "Convenience", "Discount stores"))
var.grp		<- list(1:4, 5:9, 10:12, 13:15)
lapply(var.grp, function(x) dv.lab[x])
ylab.grp	<- c(	"Estiamte of treatment effect on\n cigarette quantity (pack)", 
					"Estiamte of treatment effect on\n cigarette quantity (pack)", 
					"No. of shopping trips", 
					"Expenditure\n of other categories ($)") 

plots	<- list(NULL)
for(i in 1:length(var.grp)){
plots[[i]] 	<- ggplot(subset(ggtmp, Var %in% dv.lab[var.grp[[i]]]), aes(Model, Estimate))	+ 
					geom_bar(stat = "identity", width = .4) + 
					geom_errorbar(aes(ymin=Estimate - 1.96*Std..Error, ymax=Estimate + 1.96 * Std..Error), width=.2, color = "black") + 
					facet_wrap(~ Channel) + 
					ylab(ylab.grp[i]) +
					theme_bw()  + 
					theme(axis.text = element_text(size = rel(0.75)), axis.title = element_text(size = rel(0.85)))
}

pdf(paste(plot.wd,"/fg_", out.file, "_att.pdf", sep=""), width = 6.5, height = 6.5*ar)
# for(i in 1:length(plots)) {print(plots[[i]]) }
print(plots[[1]])
print(plots[[2]])
print(grid.arrange(plots[[3]], plots[[4]]))
dev.off()

# ---------------- #
# Add covariates #
est1.bipart	<- est1.ols	<- est1.mat	<- est1.iv	<- est1.fe	<- matrix(NA, length(dv.col), 4, 
															dimnames = list(dv.col, c("Estimate", "Std. Error", "t value", "Pr(>|t|)")))

for(i in 1:length(dv.col)){
	dv.fml			<- as.formula(paste(dv.col[i], "~ treat +", paste(match.col, collapse = "+")))
	# OLS
	est1.ols[i,]	<- coeftest(lm(dv.fml, data = match.shtdat))["treat",]

	# Matching without replacement 
	est1.mat[i,]	<- coeftest(lm(dv.fml, data = match.shtdat[sel,]))["treat",]

	# Distance as IV
	# NOTE: we have distance in the controls, use use dummy 
	est1.iv[i,]	<- coeftest(ivreg(dv.fml, instruments = ~ dist_dummy + income + age + have_kids + employment + race + log_distance_cvs +
											    log_pre_q + log_pre_trip_total + log_pre_trip_othchannel + log_pre_dol_total + log_pre_dol_othchannel, 
						data = match.shtdat))["treat",]

	# Fixed effect model with matched households
	tmp		<- plm(as.formula(paste(dv.col[i], "~ treat + treat*after + after")), data = tmpdat, 
					index = c("household_code", "month"), model = "within")
	cls.v	<- Cls.se.fn(tmp, cluster.vec = tmpdat[,"household_code"], return.se = FALSE)
	est1.fe[i,]	<- coeftest(tmp, vcov = cls.v)["treat:after",]
	
	# Bipart matching 
	est1.bipart[i,]	<- coeftest(lm(dv.fml, data = match.shtdat[sel.bp,]))["treat",]
}
class(est1.bipart)	<- class(est1.ols)	<- class(est1.mat) <- class(est1.iv) <- class(est1.fe) <- c("coeftest", "matrix")

# Print results 
stargazer(list(OLS = est1.ols, Matching = est1.mat, IV = est1.iv, Panel = est1.fe, Bipart = est1.bipart), type = "text", 
			column.labels = c("OLS", "Matching", "IV", "Panel", "Bipartite"))

# Plot estiamtes 
nn1		<- rbind(table(mypan$treat), table(mypan[sel,"treat"]), table(mypan[sel.bp,"treat"]))
ggtmp <- rbind(	data.frame(Model = "DID", Var = dv.lab, est1.ols), 
				data.frame(Model = "DID on \nthe matched \n(propensity)", Var = dv.lab, est1.mat), 
				data.frame(Model = "DID on \nthe matched \n(bipartite)", Var = dv.lab, est1.bipart), 
				data.frame(Model = "DID with\n panel", Var = dv.lab, est1.fe))
ggtmp$Channel	<- rep(sub(".*-", "", dv.lab), 4)
ggtmp$Channel	<- factor(ggtmp$Channel, levels = c("CVS", "Other drugstores", "Other channels", "Total", 
													"Sevice station", "Grocery", "Tabacco stores", "Convenience", "Discount stores"))
var.grp		<- list(1:4, 10:12, 13:15)
lapply(var.grp, function(x) dv.lab[x])
ylab.grp	<- c(	"Estiamte of treatment effect on\n cigarette quantity (pack)", 
					"No. of shopping trips", 
					"Expenditure\n of other categories ($)") 

plots	<- list(NULL)
for(i in 1:length(var.grp)){
plots[[i]] 	<- ggplot(subset(ggtmp, Var %in% dv.lab[var.grp[[i]]]), aes(Model, Estimate))	+ 
					geom_bar(stat = "identity", width = .5) + 
					geom_errorbar(aes(ymin=Estimate - 1.96*Std..Error, ymax=Estimate + 1.96 * Std..Error), width=.2, color = "black") + 
					facet_wrap(~ Channel) + 
					ylab(ylab.grp[i]) +
					theme_bw()  + 
					theme(axis.text = element_text(size = rel(0.65)), 
						  # axis.text.x = element_text( angle = 30, vjust = 1, hjust = 1),
						  axis.title = element_text(size = rel(0.8)))
}

pdf(paste(plot.wd,"/fg_", out.file, "_att_ctr.pdf", sep=""), width = 7, height = 7*ar)
# for(i in 1:length(plots)) {print(plots[[i]]) }
print(plots[[1]])
print(grid.arrange(plots[[2]], plots[[3]]))
dev.off()

#####################
# Long-term effects #
#####################
# Aggregate over 3 month window around the event 
num.month			<- 20
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
table(match.shtdat$month)
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
match.shtdat	<- merge(match.shtdat, mypan[,c("household_code", "treat", "frac_seg",match.col)], by = "household_code", all.x = T)
dim(match.shtdat)
match.shtdat$dist_dummy	<- ifelse(match.shtdat$log_distance_cvs <= log(2), 1, 0)
match.shtdat	<- match.shtdat[order(match.shtdat$household_code),]
max(abs(mypan$household_code - match.shtdat$household_code))

estlong.bipart	<- estlong.ols		<- estlong.mat	<- estlong.iv	<- estlong.fe	<- matrix(NA, length(dv.col), 4, 
															dimnames = list(dv.col, c("Estimate", "Std. Error", "t value", "Pr(>|t|)")))

sel			<- match.shtdat$household_code %in% c(match.ps)
sel.bp		<- match.shtdat$household_code %in% c(match.bipart)
table(mypan[sel,"frac_seg"])
tmpdat		<- subset(smk.trips, household_code %in% c(match.ps) & month %in% sel.month)
tmpdat		<- merge(tmpdat, mypan[,c("household_code", setdiff(match.col,names(tmpdat)))], by = "household_code", all.x = T)								

for(i in 1:length(dv.col)){
	dv.fml			<- as.formula(paste(dv.col[i], "~ treat"))
	# OLS
	estlong.ols[i,]	<- coeftest(lm(dv.fml, data = match.shtdat))["treat",]
	
	# Matching without replacement 
	estlong.mat[i,]	<- coeftest(lm(dv.fml, data = match.shtdat[sel,]))["treat",]
	
	# Distance as IV
	estlong.iv[i,]	<- coeftest(ivreg(dv.fml, instruments = ~ log_distance_cvs, data = match.shtdat))["treat",]
	
	# Fixed effect model with matched households
	tmp		<- plm(as.formula(paste(dv.col[i], "~ treat + treat*after + after")), data = tmpdat, 
					index = c("household_code", "month"), model = "within")
	cls.v	<- Cls.se.fn(tmp, cluster.vec = tmpdat[,"household_code"], return.se = FALSE)
	estlong.fe[i,]	<- coeftest(tmp, vcov = cls.v)["treat:after",]
	
	# Bipart matching 
	estlong.bipart[i,]	<- coeftest(lm(dv.fml, data = match.shtdat[sel.bp,]))["treat",]
}

# Print results 
class(estlong.bipart)	<- class(estlong.ols)	<- class(estlong.mat) <- class(estlong.iv) <- class(estlong.fe) <- c("coeftest", "matrix")
stargazer(list(OLS = estlong.ols, Matching = estlong.mat, IV = estlong.iv, Panel = estlong.fe, Bipart = estlong.bipart), type = "text", 
			column.labels = c("OLS", "Matching", "IV", "Panel", "Bipartite"))


# Plot estiamtes 
nn1		<- rbind(table(mypan$treat), table(mypan[sel,"treat"]), table(mypan[sel.bp,"treat"]))
ggtmp <- rbind(	data.frame(Model = "DID", Var = dv.lab, estlong.ols), 
				data.frame(Model = "DID on \nthe matched \n(propensity)", Var = dv.lab, estlong.mat), 
				data.frame(Model = "DID on \nthe matched \n(bipartite)", Var = dv.lab, estlong.bipart))
ggtmp$Channel	<- rep(sub(".*-", "", dv.lab), 3)
ggtmp$Channel	<- factor(ggtmp$Channel, levels = c("CVS", "Other drugstores", "Other channels", "Total", 
													"Sevice station", "Grocery", "Tabacco stores", "Convenience", "Discount stores"))
var.grp		<- list(1:4, 5:9, 10:12, 13:15)
lapply(var.grp, function(x) dv.lab[x])
ylab.grp	<- c(	"Estiamte of treatment effect on\n cigarette quantity (pack)", 
					"Estiamte of treatment effect on\n cigarette quantity (pack)", 
					"No. of shopping trips", 
					"Expenditure\n of other categories ($)") 

plots	<- list(NULL)
for(i in 1:length(var.grp)){
plots[[i]] 	<- ggplot(subset(ggtmp, Var %in% dv.lab[var.grp[[i]]]), aes(Model, Estimate))	+ 
					geom_bar(stat = "identity", width = .4) + 
					geom_errorbar(aes(ymin=Estimate - 1.96*Std..Error, ymax=Estimate + 1.96 * Std..Error), width=.2, color = "black") + 
					facet_wrap(~ Channel) + 
					ylab(ylab.grp[i]) +
					theme_bw()  + 
					theme(axis.text = element_text(size = rel(0.75)), axis.title = element_text(size = rel(0.85)))
}

pdf(paste(plot.wd,"/fg_", out.file, "_att_long.pdf", sep=""), width = 6.5, height = 6.5*ar)
# for(i in 1:length(plots)) {print(plots[[i]]) }
print(plots[[1]])
print(plots[[2]])
print(grid.arrange(plots[[3]], plots[[4]]))
dev.off()



