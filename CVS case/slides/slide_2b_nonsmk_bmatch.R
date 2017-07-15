library(reshape2)
library(ggplot2)
library(data.table)
library(stargazer)
library(plm)
library(lme4)
library(MatchIt)
library(lmtest)
library(Matching)
library(gridExtra)

setwd("~/Documents/Research/Tobacco/processed_data")
plot.wd		<- "~/Desktop"
# setwd("U:/Users/ccv103/Documents/Research/tobacco/processed_data")
# setwd("/sscc/home/c/ccv103/Tobacco") 
# plot.wd		<- getwd()
out.file	<- "cvs_sl_nonsmk"
ww			<- 5
ww1			<- 6
ar			<- .6
match.code	<- 1

load("cvs_nonsmk.rdata")
load(paste("cvs_nonsmk_mch", match.code, "_id.rdata", sep=""))

treat.code	<- 2	
(out.file 	<- paste(out.file, treat.code, sep="")	)	
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
table(nonsmk.pan$smk, nonsmk.pan$frac_seg)						
mypan			<- subset(nonsmk.pan, !is.na(treat) )
mypan			<- mypan[order(mypan$household_code),]
table(mypan$smk, mypan$frac_seg)
table(mypan$treat, mypan$frac_seg)
match.col			<- c("income", "age", "have_kids", "employment", "race", "distance_cvs", 
						"pre_trip_cvs", "pre_trip_othdrug", "pre_trip_othchannel", 
						"pre_dol_cvs", "pre_dol_othdrug", "pre_dol_othchannel")						
mypan$log_distance_cvs	<- log(mypan$distance_cvs)
mypan$log_pre_trip_cvs	<- log(mypan$pre_trip_cvs)
mypan$log_pre_trip_othdrug	<- log(mypan$pre_trip_othdrug)
mypan$log_pre_trip_othchannel	<- log(mypan$pre_trip_othchannel)
mypan$log_pre_dol_cvs	<- log(mypan$pre_dol_cvs)
mypan$log_pre_dol_othdrug	<- log(mypan$pre_dol_othdrug)
mypan$log_pre_dol_othchannel	<- log(mypan$pre_dol_othchannel)
summary(mypan[,match.col])

# Aggregate over 3 month window around the event 
num.month			<- 3
match.shtdat		<- data.table(subset(nonsmk.trips, month >= event.month - num.month & month <= event.month + num.month -1 & 
										!is.na(treat) ))
match.shtdat		<- match.shtdat[, list(	trip_cvs = sum(trip_cvs)/num.month, trip_othdrug = sum(trip_othdrug)/num.month, trip_othchannel = sum(trip_othchannel)/num.month, 
											dol_cvs = sum(dol_cvs)/num.month, dol_othdrug = sum(dol_othdrug)/num.month, dol_othchannel = sum(dol_othchannel)/num.month, 
											netdol_cvs = sum(netdol_cvs)/num.month, netdol_othdrug = sum(netdol_othdrug)/num.month, netdol_othchannel = sum(netdol_othchannel)/num.month,
											dol_total = sum(dol_total)/num.month, netdol = sum(netdol)/num.month), 
									by = list(treat, household_code, after, frac_seg, smk)]
match.shtdat 	<- match.shtdat[, drop:= 1*(length(after) < 2), by = list(household_code)]
table(match.shtdat$drop)									# 1 household did not both 2 period data

# Take before-after difference
setkeyv(match.shtdat, c("household_code","after"))
match.shtdat	<- match.shtdat[,list(	trip_cvs = diff(trip_cvs), trip_othdrug = diff(trip_othdrug), trip_othchannel = diff(trip_othchannel), 
										dol_cvs = diff(dol_cvs), dol_othdrug = diff(dol_othdrug), dol_othchannel = diff(dol_othchannel), 
										netdol_cvs = diff(netdol_cvs), netdol_othdrug = diff(netdol_othdrug), netdol_othchannel = diff(netdol_othchannel),
										dol_total = diff(dol_total), netdol = diff(netdol)), 
								by = list(household_code, treat, frac_seg, smk)]
match.shtdat	<- data.frame(match.shtdat)
match.shtdat	<- merge(match.shtdat, mypan[,c("household_code", match.col)], by = "household_code", all.x = T)
match.shtdat	<- match.shtdat[order(match.shtdat$household_code),]
max(abs(mypan$household_code - match.shtdat$household_code))

# Run logit propensity score
# fml			<- treat ~ income + age + have_kids + employment + race + log_distance_cvs +
# 						pre_trip_cvs+pre_trip_othdrug +pre_trip_othchannel +log_pre_dol_cvs +pre_dol_othdrug +log_pre_dol_othchannel
(fml			<- as.formula(paste("treat ~", paste(match.col, collapse = "+"))))

# Set matching parameters
# my.ratio	<- 1
# my.commonspt<- TRUE
# my.caliper	<- NULL			#.25

# # Run logit propensity score
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

# Check matching balance 
numbt		<- 500
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
			"Distance to CVS", "No. trips to CVS", "No. trips to other drug stores", "No. trips to other channels", 
			"Expenditure at CVS", "Expenditure at other drug stores", "Expenditure at other channels")								
cbind(colnames(tmp), tmp.lab)			
dimnames(tmp1)	<- dimnames(tmp2)<-	dimnames(tmp3) <-	list(c("Mean Control", "Mean Treatment", "Std difference","t p-value", "KS bootstrap p-value"), tmp.lab)
blc		<-cbind(t(tmp1), t(tmp2), t(tmp3))
cat("Balance check before matching (1-4), propensity score (5-8), bipart matching (9-12):\n"); print(round(blc,2)); cat("\n")

# ------- #
# QQ plot #
selcol	<- c("distance_cvs","pre_trip_cvs", "pre_trip_othdrug", "pre_trip_othchannel", "pre_dol_cvs", "pre_dol_othdrug", "pre_dol_othchannel")
col.tab	<- c("Distance to CVS", "No. trips to CVS", "No. trips to\n other drug stores", "No. trips to\n other channels",
			"Expenditure at CVS", "Expenditure at\n other drug stores", "Expenditure at\nother channels")
# sel		<- which(!selcol %in% match.col)
# selcol[sel]	<- paste("log_",selcol[sel],sep="")			
# col.tab[sel]	<- paste("log(",col.tab[sel],")", sep="")			
cbind(selcol, col.tab)
X			<- tmp

sel0	<- mypan$treat == 1									# Index of the treated smokers
sel1	<- mypan$treat == 0									# Index of control 
sel2	<- mypan$household_code %in% match.ps[,"control"]		# Index of control from propensity
sel3	<- mypan$household_code %in% match.bipart[,"treat"]  	# Index of treated from bipartite
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
						geom_point(size = rel(.7), alpha = .5) + 	
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
plots		<- lapply(plots, function(x) x+theme(legend.position = "none"))
blankPlot	<- ggplot() +geom_blank(aes(1,1))+
					theme(	plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
	   						panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), 
	   						axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank() )

hh		<- ww1*ar
pdf(paste(plot.wd, "/fg_", out.file,"_qq.pdf",sep = ""), width = ww1, height = ww1*ar)
grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], blankPlot, legend,blankPlot, ncol = 3, 
			heights = unit(c(hh*3/7,hh*3/7,hh/7), c("in", "in", "in")) )
dev.off()		

###############################
# Run regressions for each DV #
###############################
dv.col		<- c("trip_cvs", "trip_othdrug", "trip_othchannel", 
				 "netdol_cvs", "netdol_othdrug", "netdol_othchannel")
dv.lab		<- c("Trip-CVS", "Trip-Other drug stores", "Trip-Other channels", 
				 "NetExp-CVS", "NetExp-Other drug stores", "NetExp-Other channels")
cbind(dv.col, dv.lab)				
tmpdat		<- subset(nonsmk.trips, household_code %in% c(match.ps) &
								month >= event.month - num.month & month <= event.month + num.month -1 )
tmpdat		<- merge(tmpdat, mypan[,c("household_code", setdiff(match.col,names(tmpdat)))], by = "household_code", all.x = T)																
sel			<- match.shtdat$household_code %in% c(match.ps)
sel.bp		<- match.shtdat$household_code %in% c(match.bipart)
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
stargazer(list(OLS = est.ols, Matching = est.mat, Panel = est.fe, Bipart = est.bipart), type = "text", 
			column.labels = c("OLS", "Matching", "Panel", "Bipartite"))

nn1		<- rbind(table(mypan$treat), table(mypan[sel,"treat"]), table(mypan[sel.bp,"treat"]))			
# stargazer(list(OLS = est.ols, Matching = est.mat, Bipart = est.bipart), type = "latex",
#  			digits = 2, no.space = FALSE, align = FALSE,
# 			title = "Regression with non-smokers wtihout covariates",
# 			column.labels = c("OLS", "Matching", "Bipartite"),
# 			covariate.labels = dv.lab,
# 			add.lines = list(c("Control", rep(nn1[,"0"], 2)),c("Treated", rep(nn1[,"1"], 2)), 
# 								c("Covariates", rep("Yes", 4))) 
# 			)						

# Plot estiamtes 
ggtmp <- rbind(	data.frame(Model = "DID", Var = dv.lab, est.ols), 
				data.frame(Model = "DID on the\nmatched \n(propensity)", Var = dv.lab, est.mat), 
				data.frame(Model = "DID on the\nmatched \n(bipartite)", Var = dv.lab, est.bipart))
ggtmp$Channel	<- rep(sub(".*-", "", dv.lab), 3)
ggtmp$Channel	<- factor(ggtmp$Channel, levels = c("CVS", "Other drug stores", "Other channels"))
var.grp		<- list(1:3, 4:6)
lapply(var.grp, function(x) dv.lab[x])
ylab.grp	<- c(	"No. of shopping trips", 
					"Expenditure\n of other categories ($)") 

plots	<- list(NULL)
for(i in 1:length(var.grp)){
plots[[i]] 	<- ggplot(subset(ggtmp, Var %in% dv.lab[var.grp[[i]]]), aes(Model, Estimate))	+ 
					geom_bar(stat = "identity", width = .5) + 
					geom_errorbar(aes(ymin=Estimate - 1.96*Std..Error, ymax=Estimate + 1.96 * Std..Error), width=.2, color = "black") + 
					facet_wrap(~ Channel) + 
					ylab(ylab.grp[i]) +
					theme_bw()  + 
					theme(axis.text = element_text(size = rel(0.7)), axis.title = element_text(size = rel(0.7)))
}

pdf(paste(plot.wd,"/fg_", out.file, "_att.pdf", sep=""), width = 6.5, height = 6.5*ar)
print(grid.arrange(plots[[1]], plots[[2]]))
dev.off()

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
stargazer(list(OLS = est1.ols, Matching = est1.mat, Panel = est1.fe, Bipart = est1.bipart), type = "text", 
			column.labels = c("OLS", "Matching", "Panel", "Bipartite"))
