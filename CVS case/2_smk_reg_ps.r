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
sink(paste(plot.wd, "/log_", out.file, ".txt", sep=""), append = FALSE)

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
mypan$distance_cvs	<- log(mypan$distance_cvs)
mypan$pre_q		<- log(mypan$pre_q)
mypan$pre_trip_total	<- log(mypan$pre_trip_total)
mypan$pre_trip_othchannel	<- log(mypan$pre_trip_othchannel)
mypan$pre_dol_othchannel	<- log(mypan$pre_dol_total)

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
my.caliper		<- .25
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
numbt	<- 500
mb	<- MatchBalance(fml, data=mypan, match.out=rr, nboots=numbt)					
tmp1<- sapply(mb$BeforeMatching, function(x) c(x$mean.Co,x$mean.Tr, x$sdiff.pooled/100, x$tt$p.value, 
										ifelse("ks" %in% names(x), x$ks$ks.boot.pvalue, x$tt$p.value) ))
tmp2<- sapply(mb$AfterMatching, function(x) c(x$mean.Co,x$mean.Tr, x$sdiff.pooled/100, x$tt$p.value, 
										ifelse("ks" %in% names(x), x$ks$ks.boot.pvalue, x$tt$p.value) ))
tmp	<- model.matrix(psmod)[,-1]		
tmp.lab	<- c("Income", "Age", "Have kids", paste("Employment:", levels(mypan$employment)[-1]), paste("Race:", levels(mypan$race)[-1]), 
			"Distance to CVS", "Cigarette consumption/m.", "No. total trips/m.", "No. trips to other channels/m.", 
			"Total expenditure/m.", "Expenditure at other channels/m.")								
cbind(colnames(tmp), tmp.lab)			
dimnames(tmp1)	<- dimnames(tmp2)	<-	list(c("Mean Control", "Mean Treatment", "Std difference","t p-value", "KS bootstrap p-value"), tmp.lab)
blc	<- cbind(t(tmp1), t(tmp2))
nn	<- setNames(c(length(unique(rr$index.control)), length(unique(rr$index.treated)) ), 
		c("Control w/o replacement", "Treated w/o replacement" ))
cat("The number of unique households:\n"); print(nn); cat("\n")
cat("Balance check before matching (1-4), after matchging without replacement (5-8):\n"); print(round(blc,2)); cat("\n")

# QQ plot
selcol	<- c("income", "age","distance_cvs","pre_q", "pre_trip_total", "pre_trip_othchannel", "pre_dol_total", "pre_dol_othchannel")
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
dv.col		<- c("q", "q_cvs", "q_othdrug", "q_othchannel", 
				"trip_cvs", "trip_othdrug", "trip_othchannel", "netdol_cvs", "netdol_othdrug","netdol_othchannel", 
				"q_convenience", "q_grocery", "q_discount", "q_service", "q_gas", "q_tobacco")
est.ols		<- est.mat	<- est.iv	<- est.fe	<- matrix(NA, length(dv.col), 4, 
															dimnames = list(dv.col, c("Estimate", "Std. Error", "t value", "Pr(>|t|)")))

sel			<- match.shtdat$household_code %in% mypan[unlist(rr[c("index.treated", "index.control")]), "household_code"]
table(mypan[sel,"frac_seg"])
tmpdat		<- subset(smk.trips, household_code %in% mypan[unlist(rr[c("index.treated", "index.control")]), "household_code"] &
								month >= event.month - num.month & month <= event.month + num.month -1)
tmpdat		<- merge(tmpdat, mypan[,c("household_code", setdiff(match.col,names(tmpdat)))], by = "household_code", all.x = T)								

for(i in 1:length(dv.col)){
	dv.fml			<- as.formula(paste(dv.col[i], "~ treat"))
	# OLS
	est.ols[i,]	<- coeftest(lm(dv.fml, data = match.shtdat))["treat",]
	
	# Matching without replacement 
	est.mat[i,]	<- coeftest(lm(dv.fml, data = match.shtdat[sel,]))["treat",]
	
	# Distance as IV
	est.iv[i,]	<- coeftest(ivreg(dv.fml, instruments = ~ distance_cvs, data = match.shtdat))["treat",]
	
	# Fixed effect model with matched households
	tmp		<- plm(as.formula(paste(dv.col[i], "~ treat + treat*after + after")), data = tmpdat, 
					index = c("household_code", "month"), model = "within")
	cls.v	<- Cls.se.fn(tmp, cluster.vec = tmpdat[,"household_code"], return.se = FALSE)
	est.fe[i,]	<- coeftest(tmp, vcov = cls.v)["treat:after",]
}
class(est.ols)	<- class(est.mat) <- class(est.iv) <- class(est.fe) <- "coeftest"

# Print results 
stargazer(list(OLS = est.ols, Matching = est.mat, IV = est.iv, Panel = est.fe), type = "text", 
			column.labels = c("OLS", "Matching", "IV", "Panel"))

# ---------------- #
# Add covariates #
est1.ols	<- est1.mat	<- est1.iv	<- est1.fe	<- matrix(NA, length(dv.col), 4, 
															dimnames = list(dv.col, c("Estimate", "Std. Error", "t value", "Pr(>|t|)")))
sel			<- match.shtdat$household_code %in% mypan[unlist(rr[c("index.treated", "index.control")]), "household_code"]

for(i in 1:length(dv.col)){
	dv.fml			<- as.formula(paste(dv.col[i], "~ treat +", paste(match.col, collapse = "+")))
	# OLS
	est1.ols[i,]	<- coeftest(lm(dv.fml, data = match.shtdat))["treat",]

	# Matching without replacement 
	est1.mat[i,]	<- coeftest(lm(dv.fml, data = match.shtdat[sel,]))["treat",]

	# Distance as IV
	# NOTE: we have distance in the controls, use use dummy 
	est1.iv[i,]	<- coeftest(ivreg(dv.fml, instruments = ~ dist_dummy + income + age + have_kids + employment + race + distance_cvs +
											    pre_q + pre_trip_total + pre_trip_othchannel + pre_dol_total + pre_dol_othchannel, 
						data = match.shtdat))["treat",]

	# Fixed effect model with matched households
	tmp		<- plm(as.formula(paste(dv.col[i], "~ treat + treat*after + after")), data = tmpdat, 
					index = c("household_code", "month"), model = "within")
	cls.v	<- Cls.se.fn(tmp, cluster.vec = tmpdat[,"household_code"], return.se = FALSE)
	est1.fe[i,]	<- coeftest(tmp, vcov = cls.v)["treat:after",]
}
class(est1.ols)	<- class(est1.mat) <- class(est1.iv) <- class(est1.fe) <- "coeftest"

# Print results 
stargazer(list(OLS = est1.ols, Matching = est1.mat, IV = est1.iv, Panel = est1.fe), type = "text", 
			column.labels = c("OLS", "Matching", "IV", "Panel"))
sink()

myline	<- paste(names(nn), " (", nn, ")", sep="", collapse = ",")
stargazer(list(blc, list(est.ols, est.mat, est.iv, est.fe, est1.ols, est1.mat, est1.iv, est1.fe)), 
		  type = "html", summary = FALSE, align = TRUE, no.space = TRUE, digits = 2,
          title = c("Balance Check among smokers", "Before-after difference between treatment and control group for the matched sample during 201406 - 201411"),
		  notes = myline,
          out = paste(plot.wd, "/tb_", out.file, "_balance_",Sys.Date(), ".html", sep=""))
