library(data.table)
library(designmatch)
library(dummies)
library(ggplot2)
library(MatchIt)
library(Matching)
library(stargazer)

### Load the data
# setwd("~/Documents/Research/Tobacco/processed_data")
# plot.wd		<- "~/Desktop"
setwd("U:/Users/ccv103/Documents/Research/tobacco/processed_data")
# setwd("/sscc/home/c/ccv103/Tobacco") 
plot.wd		<- getwd()
out.file	<- "cvs_smk_mch"
ww			<- 6.5
ar			<- .6

load("cvs_smk.rdata")

# Define the construction of treatment and control
# 1: Control (smokers who never shopped at CVS), Treament (smokers who are also CVS shoppers)
# 2: Control (smokers who shopped at CVS but did not buy cigarettes), Treament (smokers who also purchased cigarettes at CVS)
# 3: Control (smokers who shopped at CVS but did not buy cigarettes), Treament (smokers who spent more than 17% CVS spending on cigarettes)
treat.code	<- 2	

# Match.code
# 0: Pre-treatment variables are defined using Jan 2013 - Aug 2014
# 1: Pre-treatment variables are defined using June 2014 - Aug 2014
# 2: Match 3-month period trend 
match.code	<- 2	
(out.file 	<- paste(out.file, match.code, sep="")	)	

smk.pan$treat	<- smk.pan[,paste("treat", treat.code, sep="")]
smk.trips$treat	<- smk.trips[,paste("treat",treat.code,sep="")]
sink(paste(plot.wd, "/log_", out.file, ".txt", sep=""), append = FALSE)

# # Select a random sample
# sel		<- sample(unique(panelists$household_code), .1*length(unique(panelists$household_code)))
# panelists	<- subset(panelists, household_code %in% sel)
# trips 		<- subset(trips, household_code %in% sel)
# purchases	<- subset(purchases, household_code %in% sel )

######################
# Bipartite matching # 
######################
match.col	<- c("income", "age", "have_kids", "employment", "race", "distance_cvs",
                "pre_q", "pre_trip_total", "pre_trip_othchannel", "pre_dol_total", "pre_dol_othchannel")
if(match.code == 1){
	sel			<- grep("pre", match.col)				
	match.col[sel]	<- paste(match.col[sel], 1, sep="")
}else if(match.code == 2){
	sel			<- grep("pre", match.col)				
	match.col	<- c(match.col[-sel], paste(match.col[sel], rep(1:6, each = length(sel)), sep=""))
}				
match.col

# We only keep the households with defined treatment
smk.pan	<- subset(smk.pan, !is.na(treat))

# Check outliers
summary(smk.pan[,match.col])
# smk.pan	<- subset(smk.pan, distance_cvs <= 100)

table(smk.pan$treat)
smk.pan	<- smk.pan[order(-smk.pan$treat),]
t_ind 	<- as.matrix(smk.pan$treat)
cat("Table of treatment:\n"); print(table(t_ind)); cat("\n")

# -------------------- #
# Matrix of covariates #
smk.pan$income	<- factor(smk.pan$income)
(fml		<- as.formula(paste("treat ~", paste(match.col, collapse = "+"))))
X_mat		<- model.matrix(fml, data = smk.pan)[,-1]
dim(X_mat)
summary(X_mat)

## Distance matrix
dist_mat 	<- distmat(t_ind, X_mat)

## Subset matching weight
subset_weight <- median(dist_mat)

## Moment balance: constrain differences in means to be at most .05 standard deviations apart
mom_covs 	<- X_mat
mom_tols 	<- round(absstddif(mom_covs, t_ind, .05), 2)
mom 		<-  list(covs = mom_covs, tols = mom_tols)
colnames(X_mat)
near.col		<- c("age", "distance_cvs","pre_q","pre_trip_total","pre_dol_total")
if(match.code == 1){
	sel			<- grep("pre", near.col)				
	near.col[sel]	<- paste(near.col[sel], 1, sep="")
}else if(match.code == 2){
	sel			<- grep("pre", near.col)				
	near.col	<- c(near.col[-sel], paste(near.col[sel], rep(1:6, each = length(sel)), sep=""))
}
near.fine.col	<- NULL
near.exact.col	<- NULL
exact.col		<- c("raceAfrican American","have_kids")

## Near balance
near_covs 	<- cbind(X_mat[,near.col])
near_pairs 	<- c(20, 25, 2, 60, 350)
near_groups <- NULL
near 		<- list(covs = near_covs, pairs = near_pairs, groups = near_groups)
if(match.code == 1){
	near_pairs	<- c(20, 25, 2, 60, 350)
	near 		<- list(covs = near_covs, pairs = near_pairs, groups = near_groups)
}else if(match.code == 2){
	near_pairs	<- NULL
}
near 		<- list(covs = near_covs, pairs = near_pairs, groups = near_groups)
#near 		<- NULL

## Near-fine balance
#near_fine_covs <- cbind(X_mat[,"age"])
#near_fine_devs <- c(5)
#near_fine 	<- list(covs = near_fine_covs, devs = near_fine_devs)
near_fine 	<- NULL

## Near-exact matching
#near_exact_covs = cbind(black, hispanic)
#near_exact_devs = rep(5, 2)
#near_exact = list(covs = near_exact_covs, devs = near_exact_devs)
near_exact 	<- NULL

## Exact matching
exact_covs 	<- X_mat[,exact.col]
exact 		<- list(covs = exact_covs)
#exact = NULL

## Solver options
t_max 	<- 60*20
solver 	<- "gurobi"
approximate <- 1
solver 	<- list(name = solver, t_max = t_max, approximate = approximate, round_cplex = 0, trace_cplex = 0)

## Match        
out <- bmatch(t_ind = t_ind, dist_mat = dist_mat, subset_weight = subset_weight, mom = mom, 
				near = near, near_fine = near_fine, near_exact = near_exact, exact = exact, solver = solver)
cat("The matching finishes with",out$time/60,"min.\n")
cat("Number of matched treatment =", length(out$t_id), "\n"); 

## Indices of the treated units and matched controls
t_id <-	out$t_id  
c_id <- out$c_id	
g_id <- out$group_id
match.bipart	<- cbind(treat = smk.pan[t_id,"household_code"], control = smk.pan[c_id,"household_code"])

# Check covariant balance after matching
meantab(mom_covs, t_ind, t_id, c_id)
tmp		<- apply(X_mat, 2, function(x) max(x[t_id] - x[c_id]) )
cat("Max difference with paired units:\n"); print(tmp); cat("\n")

#############################
# Propensity score matching # 
#############################
# Run logit propensity score
my.ratio		<- 1
my.caliper		<- NULL			#.25
(fml			<- as.formula(paste("treat ~", paste(match.col, collapse = "+"))))
# psmod  			<- glm(fml,data = smk.pan, family=binomial )
psmod			<- glm(t_ind ~ X_mat, family = binomial)
summary(psmod)
pshat  			<- psmod$fitted

# Generic matching 
# genout 			<- GenMatch(Tr=t_ind, X=X_mat, M=1, pop.size=16, max.generations=10, wait.generations=1, caliper = my.caliper)
# rr	<- Match(Tr=t_ind, X=X_mat, Weight.matrix=genout, replace = FALSE, caliper = my.caliper)

rr  			<- Match(Tr=t_ind, X=pshat, M=my.ratio, replace = FALSE, caliper = my.caliper)
print(summary(rr))
cat("Check the treatment variable with index from matching:\n")
table(t_ind[rr$index.treated]); table(t_ind[rr$index.control])
match.ps			<- cbind(treat = smk.pan[rr$index.treated,"household_code"], control = smk.pan[rr$index.control,"household_code"])

##########################
# Check matching balance # 
##########################
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

numbt	<- 500
mb		<- MatchBalance(fml, data=smk.pan, match.out=rr, nboots=numbt)					
tmp1	<- sapply(mb$BeforeMatching, function(x) c(x$mean.Co,x$mean.Tr, x$sdiff.pooled/100, x$tt$p.value, 
										ifelse("ks" %in% names(x), x$ks$ks.boot.pvalue, x$tt$p.value) ))
tmp2	<- sapply(mb$AfterMatching, function(x) c(x$mean.Co,x$mean.Tr, x$sdiff.pooled/100, x$tt$p.value, 
										ifelse("ks" %in% names(x), x$ks$ks.boot.pvalue, x$tt$p.value) ))
tmp3	<- MatchBalance(fml, data=subset(smk.pan, household_code %in% c(match.bipart)), nboots=numbt)															
tmp3	<- sapply(tmp3$BeforeMatching, function(x) c(x$mean.Co,x$mean.Tr, x$sdiff.pooled/100, x$tt$p.value, 
										ifelse("ks" %in% names(x), x$ks$ks.boot.pvalue, x$tt$p.value) ))

tmp.lab	<- c(paste("Income -", levels(smk.pan$income)[-1]), "Age", "Have kids", paste("Employment -", levels(smk.pan$employment)[-1]), 
			paste("Race -", levels(smk.pan$race)[-1]), 
			"Distance to CVS", "Cigarette consumption", "No. total trips", "No. trips to other channels", 
			"Total expenditure", "Expenditure at other channels")								
if(match.code == 2){
	tmp.lab	<- c(paste("Income -", levels(smk.pan$income)[-1]), "Age", "Have kids", paste("Employment -", levels(smk.pan$employment)[-1]), 
				paste("Race -", levels(smk.pan$race)[-1]), 
				"Distance to CVS", 
				paste(c("Cigarette consumption", "No. total trips", "No. trips to other channels", 
				"Total expenditure", "Expenditure at other channels"), rep(1:6, each = 5), sep=""))
}			
cbind(colnames(X_mat), tmp.lab)			
dimnames(tmp1)	<- dimnames(tmp2)	<-	dimnames(tmp3) <-list(c("Mean Control", "Mean Treatment", "Std difference","t p-value", "KS bootstrap p-value"), tmp.lab)
blc		<- cbind(t(tmp1), t(tmp2), t(tmp3))
nn		<- setNames(c(length(unique(rr$index.control)), length(unique(rr$index.treated)) ), 
			c("Control", "Treated" ))
cat("The number of unique households:\n"); print(nn); cat("\n")
cat("Balance check before matching (1-4), after matchging without replacement (5-8):\n"); print(round(blc,2)); cat("\n")

stargazer(blc[,-grep("Std", colnames(blc))], summary = FALSE, type = "text", label = "tab:blc_smk", 
			title = "Balance Check Between Control Smokers and Treated Smokers", 
			digits = 2)

# ------- #
# QQ plot #
selcol	<- setdiff(match.col, c("income", "age", "have_kids", "employment", "race"))
col.tab	<- c("Distance to CVS", "Cigarette quantity", "No. total trips", "No. trips to\n other channels", "Total expenditure", 
			"Expenditure at\n other channels")
sel		<- which(!selcol %in% match.col)
selcol[sel]	<- paste("log_",selcol[sel],sep="")			
col.tab[sel]	<- paste("log(",col.tab[sel],")", sep="")			
if(match.code == 2){
	col.tab	<- c("Distance to CVS", paste(c("Cigarette quantity", "No. total trips", "No. trips to\n other channels", "Total expenditure", 
				"Expenditure at\n other channels"), rep(1:6, each = 5), sep=""))
}
cbind(selcol, col.tab)

sel0	<- smk.pan$treat == 1									# Index of the treated smokers
sel1	<- smk.pan$treat == 0									# Index of control 
sel2	<- rr$index.control										# Index of control from propensity
sel3	<- smk.pan$household_code %in% match.bipart[,"treat"] 	# Index of treated from bipartite
sel4	<- smk.pan$household_code %in% match.bipart[,"control"]	# Index of control from bipartite
ggtmp	<- data.frame(NULL)
for(i in selcol){
	tmp1	<- qqplot(X_mat[sel0,i], X_mat[sel1,i], plot.it = FALSE)
	tmp2	<- qqplot(X_mat[sel0,i], X_mat[sel2,i], plot.it = FALSE)
	tmp3	<- qqplot(X_mat[sel3,i], X_mat[sel4,i], plot.it = FALSE)
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
						xlim(range(X_mat[,selcol[i]])) + ylim(range(X_mat[,selcol[i]])) + 
						# scale_colour_grey() + 
						scale_color_manual(name = "", values = col.v) +
						facet_wrap(~ variable, scales = "free") + 
						theme_bw()	+ 
						theme(legend.position = "bottom") + 
						guides(alpha = FALSE, shape = guide_legend(title=""), color = guide_legend(override.aes= list(alpha = 1))) 
}


pdf(file = paste(plot.wd, "/fg_",out.file, "_balance.pdf", sep=""), width = 8, height = 6)
for(i in 1:length(plots)){
	print(plots[[i]])
}

# Compare distribution - Bipartite
sel		<- c("income", "age", "distance_cvs", grep("pre", match.col, value = T))
# 
# sel		<- c("income", "age","distance_cvs", "pre_q", "pre_trip_total", "pre_dol_total")
# if(match.code == 1){
# 	sel1<- grep("pre", sel)
# 	sel[sel1]	<- paste(sel[sel1], 1, sep="")
# }else if(match.code == 2){
# 	sel1<- grep("pre", sel)
# 	sel	<- c(sel[-sel1], paste(sel[sel1], rep(1:6, 3), sep=""))
# }
par(mfrow=c(2, 3))
for(i in sel){
	if(is.numeric(smk.pan[,i])){
		ecdfplot(smk.pan[,i], t_id, c_id, main_title = paste("Bipartite -", i, sep=""))
	}else{
		ecdfplot(as.numeric(as.character(smk.pan[,i])), t_id, c_id, main_title = paste("Bipartite -", i, sep=""))
	}
}

# Compare distribution - Propensity
par(mfrow=c(2, 3))
for(i in sel){
	if(is.numeric(smk.pan[,i])){
		ecdfplot(smk.pan[,i], rr$index.treated, rr$index.treated, main_title = paste("Propensity -", i, sep=""))
	}else{
		ecdfplot(as.numeric(as.character(smk.pan[,i])), rr$index.treated, rr$index.treated, main_title = paste("Propensity -", i, sep=""))
	}
}
dev.off()

sink()

save(match.bipart, match.ps, file = paste(plot.wd,"/",out.file, "_id.rdata", sep=""))
