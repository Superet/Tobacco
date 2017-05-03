library(data.table)
library(designmatch)
library(dummies)
library(ggplot2)

### Load the data
# setwd("~/Documents/Research/Tobacco/processed_data")
# plot.wd		<- "~/Desktop"
setwd("U:/Users/ccv103/Documents/Research/tobacco/processed_data")
# setwd("/sscc/home/c/ccv103/Tobacco") 
plot.wd		<- getwd()
out.file	<- "cvs_smk"
ww			<- 6.5
ar			<- .6
match.method<- "bipt"
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

######################
# Bipartite matching # 
######################
match.col	<- c("income", "age", "have_kids", "employment", "race", "distance_cvs",
                		 "pre_q", "pre_trip_total", "pre_trip_othchannel", "pre_dol_total", "pre_dol_othchannel")

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
near.fine.col	<- NULL
near.exact.col	<- NULL
exact.col		<- c("raceAfrican American","have_kids")

## Near balance
near_covs 	<- cbind(X_mat[,near.col])
near_pairs 	<- c(20, 25, 2, 60, 350)
near_groups <- NULL
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
matches <- cbind(t_id,c_id)
match.pan	<- cbind(treat = smk.pan[t_id,"household_code"], control = smk.pan[c_id,"household_code"])

# Check covariant balance after matching
meantab(mom_covs, t_ind, t_id, c_id)
tmp		<- apply(X_mat, 2, function(x) max(x[t_id] - x[c_id]) )
cat("Max difference with paired units:\n"); print(tmp); cat("\n")

# Compare distribution 
# (sel	<- setdiff(match.col, exact.col))
sel		<- c("income", "age","distance_cvs", "pre_q", "pre_trip_total", "pre_dol_total")
pdf(file = paste(plot.wd, "/fg_",out.file, "_balance.pdf", sep=""), width = 8, height = 6)
par(mfrow=c(2, 3))
for(i in sel){
	if(is.numeric(smk.pan[,i])){
		ecdfplot(smk.pan[,i], t_id, c_id, main_title = i)
	}else{
		ecdfplot(as.numeric(as.character(smk.pan[,i])), t_id, c_id, main_title = i)
	}
}

# Scatter plots of paired units
ggtmp	<- do.call(rbind, lapply(sel, function(i) data.frame(var = i, Treated = as.numeric(smk.pan[t_id,i]), Control = as.numeric(smk.pan[c_id, i]))))
print(ggplot(ggtmp, aes(Treated, Control)) + geom_point() + 
		geom_abline(intercept = 0, slope = 1, col = "red") + 
		facet_wrap(~ var, scales = "free") )
dev.off()

save(match.pan, file = paste(plot.wd,"/",out.file, "_id.rdata", sep=""))
