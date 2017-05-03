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
match.method	<- "ps"			# "genetic"
out.file	<- paste(out.file, match.method, sep="_")

load("cvs_nonsmk.rdata")

treat.code	<- 2	
(out.file 	<- paste(out.file, treat.code, sep="")	)	
nonsmk.pan$treat	<- nonsmk.pan[,paste("treat", treat.code, sep="")]
nonsmk.trips$treat	<- nonsmk.trips[,paste("treat",treat.code,sep="")]

# Set event date
event.date	<- as.Date("2014-09-01", format = "%Y-%m-%d")			# Placebo event 
event.month	<- month(event.date) + 12
cvs.ret	<- 4914		# retailer_code for CVS
qunit	<- 20		# 20 cigaretts per pack
sink(paste("log_", out.file, ".txt", sep=""), append = FALSE)

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
mypan			<- subset(nonsmk.pan, !is.na(treat) & !(frac_seg == "Zero" & smk == 1))
mypan			<- mypan[order(mypan$household_code),]
table(mypan$smk, mypan$frac_seg)
table(mypan$treat, mypan$frac_seg)
match.col			<- c("income", "age", "have_kids", "employment", "race", "distance_cvs", 
						"pre_trip_cvs", "pre_trip_othdrug", "pre_trip_othchannel", "pre_dol_cvs", "pre_dol_othdrug", "pre_dol_othchannel")						
summary(mypan[,match.col])
mypan$distance_cvs	<- log(mypan$distance_cvs)
mypan$pre_dol_cvs	<- log(mypan$pre_dol_cvs)
mypan$pre_dol_othchannel	<- log(mypan$pre_dol_othchannel)

# Aggregate over 3 month window around the event 
num.month			<- 3
match.shtdat		<- data.table(subset(nonsmk.trips, month >= event.month - num.month & month <= event.month + num.month -1 & 
										!is.na(treat) & !(frac_seg == "Zero" & smk == 1) ))
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
match.shtdat	<- merge(match.shtdat, nonsmk.pan[,c("household_code", match.col)], by = "household_code", all.x = T)
match.shtdat	<- match.shtdat[order(match.shtdat$household_code),]
max(abs(mypan$household_code - match.shtdat$household_code))

# Run logit propensity score
fml			<- treat ~ income + age + have_kids + employment + race + distance_cvs +
						pre_trip_cvs+pre_trip_othdrug +pre_trip_othchannel +pre_dol_cvs +pre_dol_othdrug +pre_dol_othchannel

# Set matching parameters
my.ratio	<- 1
my.commonspt<- TRUE
my.caliper	<- .25
numbt		<- 500

# Run logit propensity score
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

# Check matching balance 
mb	<- MatchBalance(fml, data=mypan, match.out=rr, nboots=numbt)					
tmp1<- sapply(mb$BeforeMatching, function(x) c(x$mean.Co,x$mean.Tr, x$sdiff.pooled/100, x$tt$p.value, 
										ifelse("ks" %in% names(x), x$ks$ks.boot.pvalue, x$tt$p.value) ))
tmp2<- sapply(mb$AfterMatching, function(x) c(x$mean.Co,x$mean.Tr, x$sdiff.pooled/100, x$tt$p.value, 
										ifelse("ks" %in% names(x), x$ks$ks.boot.pvalue, x$tt$p.value) ))
tmp	<- model.matrix(psmod)[,-1]		
tmp.lab	<- c("Income", "Age", "Have kids", paste("Employment:", levels(mypan$employment)[-1]), paste("Race:", levels(mypan$race)[-1]), 
			"Distance to CVS", "No. trips to CVS/m.", "No. trips to other drug stores/m.", "No. trips to other channels/m.", 
			"Expenditure at CVS/m.", "Expenditure at other drug stores/m.", "Expenditure at other channels/m.")								
cbind(colnames(tmp), tmp.lab)			
dimnames(tmp1)	<- dimnames(tmp2)	<-	list(c("Mean Control", "Mean Treatment", "Std difference","t p-value", "KS bootstrap p-value"), tmp.lab)
blc		<-cbind(t(tmp1), t(tmp2))
nn		<- c(length(unique(rr$index.control)), length(unique(rr$index.treated)) )												
cat("The number of unique households:\n"); print(nn); cat("\n")
cat("Balance check:\n"); print(round(blc,2)); cat("\n")

# QQ plot
selcol	<- c("income", "age","distance_cvs","pre_trip_cvs", "pre_trip_othdrug", "pre_trip_othchannel", "pre_dol_cvs", "pre_dol_othdrug", "pre_dol_othchannel")
sel		<- c(rr$index.treated, rr$index.control)
pdf(paste("~/Desktop/fg_", out.file,"_qq.pdf",sep = ""), width = 10, height = 10)
par(mfrow = c(3,3))
for(i in selcol){
	rg	<- range(X[sel,i])
	qqplot(X[rr$index.treated,i], X[rr$index.control,i], xlim = rg, ylim = rg,xlab = "Treated", ylab = "Control", main = i) 
}
dev.off()

# --------------------------- #
# Run regressions for each DV #
dv.col		<- c("trip_cvs", "trip_othdrug", "trip_othchannel", "dol_cvs", "dol_othdrug", "dol_othchannel",     
				 "netdol_cvs", "netdol_othdrug", "netdol_othchannel", "dol_total", "netdol")
tmpdat		<- subset(nonsmk.trips, household_code %in% mypan[unlist(rr[c("index.treated", "index.control")]), "household_code"] &
								month >= event.month - num.month & month <= event.month + num.month -1 &
								!(frac_seg == "Zero" & smk == 1))
tmpdat		<- merge(tmpdat, mypan[,c("household_code", setdiff(match.col,names(tmpdat)))], by = "household_code", all.x = T)																
sel			<- match.shtdat$household_code %in% mypan[unlist(rr[c("index.treated", "index.control")]), "household_code"]
table(match.shtdat[sel,"treat"])
max(abs(sort(unique(tmpdat$household_code)) - mypan[sel, "household_code"]))

est.ols		<- est.mat	<- est.fe	<- matrix(NA, length(dv.col), 4, 
											dimnames = list(dv.col, c("Estimate", "Std. Error", "t value", "Pr(>|t|)")))
for(i in 1:length(dv.col)){
	# dv.fml			<- as.formula(paste(dv.col[i], "~ treat"))
	dv.fml			<- as.formula(paste(dv.col[i], "~ treat + income + age + have_kids + employment + race + distance_cvs +
	                 		pre_trip_cvs+pre_trip_othdrug +pre_trip_othchannel +pre_dol_cvs +pre_dol_othdrug +pre_dol_othchannel"))
	# OLS
	est.ols[i,]	<- coeftest(lm(dv.fml, data = match.shtdat))["treat",]
		
	# Matching without replacement 
	est.mat[i,]	<- coeftest(lm(dv.fml, data = match.shtdat[sel,]))["treat",]
	
	# Fixed effect model with matched households
	tmp		<- plm(as.formula(paste(dv.col[i], "~ treat + treat*after + after")), data = tmpdat, 
					index = c("household_code", "month"), model = "within")
		
	cls.v	<- Cls.se.fn(tmp, cluster.vec = tmpdat[,"household_code"], return.se = FALSE)
	est.fe[i,]	<- coeftest(tmp, vcov = cls.v)["treat:after",]	
}
class(est.ols)	<- class(est.mat) <- class(est.fe) <- "coeftest"

# Print results 
stargazer(list(OLS = est.ols, Matching = est.mat, Panel = est.fe), type = "text", 
			column.labels = c("OLS", "Matching", "Panel"))
						
# ---------------- #
# Add covariates #
est1.ols	<- est1.mat	<- est1.fe	<- matrix(NA, length(dv.col), 4, 
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
}
class(est1.ols)	<- class(est1.mat) <- class(est1.fe) <- "coeftest"

# Print results 
stargazer(list(OLS = est1.ols, Matching = est1.mat, Panel = est1.fe), type = "text", 
			column.labels = c("OLS", "Matching", "IV", "Panel"))

sink()

myline	<- paste(names(nn), " (", nn, ")", sep="", collapse = ",")
stargazer(list(blc, list(est.ols, est.mat, est.fe, est1.ols, est1.mat, est1.fe)), 
		  type = "html", summary = FALSE, align = TRUE, no.space = TRUE, digits = 2,
          title = c("Balance Check among smokers", "Before-after difference between treatment and control group for the matched sample during 201406 - 201411"),
		  notes = myline,
          out = paste(plot.wd, "/tb_", out.file, "_balance_",Sys.Date(), ".html", sep=""))
			
cat("This program is done.\n")
