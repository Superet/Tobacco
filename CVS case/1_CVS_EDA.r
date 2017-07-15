library(reshape2)
library(ggplot2)
library(data.table)
library(lubridate)
library(scales)
library(stargazer)
library(gridExtra)

# setwd("U:/Users/ccv103/Desktop")
setwd("~/Documents/Research/Tobacco/processed_data")
plot.wd		<- "~/Desktop"
ww			<- 4.5
ar			<- .6
treat.code	<- 2
out.file <- "cvs_sumstat"

load("cvs_smk.rdata")

smk.pan$treat	<- smk.pan[,paste("treat", treat.code, sep="")]
smk.trips$treat	<- smk.trips[,paste("treat",treat.code,sep="")]

purchases	<- read.csv("tob_CVS_purchases.csv", header = T)
trips		<- read.csv("tob_CVS_trips.csv", header = T)

purchases	<- subset(purchases, household_code %in% smk.pan$household_code)
purchases$purchase_date	<- as.Date(as.character(purchases$purchase_date), format = "%Y-%m-%d")
purchases	<- merge(purchases, trips[,c("trip_code_uc", "channel_type", "retailer_code")], by = "trip_code_uc", all.x=T)
trips$purchase_date	<- as.Date(as.character(trips$purchase_date), format = "%Y-%m-%d")

#################
# Summary stats # 
#################
event.date	<- as.Date("2014-09-01", format = "%Y-%m-%d")
event.month	<- month(event.date) + 12
cvs.ret	<- 4914		# retailer_code for CVS
qunit	<- 20		# 20 cigaretts per pack 

cat("Number of households in the data:", length(unique(smk.pan$household_code)), "\n")
cat("Number of households who never shopped at CVS:", sum(is.na(smk.pan$treat)), "\n")
cat("Number of households in treatment and control group\n"); print(table(smk.pan$treat)); cat("\n")
cat("Percentage of households in each group:\n"); print(c(table(smk.pan$treat)/nrow(smk.pan), sum(is.na(smk.pan$treat))/nrow(smk.pan))); cat("\n")
table(smk.pan$treat, smk.pan$frac_seg)

# Check summary stats 
tmp		<- data.table(subset(smk.trips, month < event.month))
tmp		<- tmp[,n:= max(month) - min(month), by = list(household_code)]
tmp		<- tmp[,list(q = sum(q/n), cigdol = sum(cigdol/n),
 					q_cvs = sum(q_cvs/n), cigdol_cvs = sum(cigdol_cvs/n), 
					cigdol_cvs_frac = sum(cigdol_cvs)/sum(dol_cvs), 
					dol_cvs = sum(dol_cvs/n),trip_cvs = sum(trip_cvs/n), netdol_cvs = sum(netdol_cvs/n)), 
				by = list(household_code)]
tmp		<- merge(tmp, smk.pan[,c("household_code", "treat", "distance_cvs")], by = "household_code")				
summary(tmp)
by(tmp, tmp$treat, summary)
selcol	<- c("q", "q_cvs", "cigdol", "cigdol_cvs", "distance_cvs", "trip_cvs", "dol_cvs", "netdol_cvs", "cigdol_cvs_frac")			
col.lab	<- c("Cigarette quantity (packs)", "Cigarette quantity at CVS (packs)", 
			"Cigarette spending", "Cigarette spending at CVS", "Distance to CVS (mi)",
			"No. trips to CVS", "Total spending at CVS", "Spending on other items at CVS", 
			"Share of cigarette spending at CVS")
cbind(selcol, col.lab)	
setcolorder(tmp, c("household_code", "treat", selcol))
setnames(tmp, selcol, col.lab)		
tmp		<- tmp[,!"household_code", with = F]

stargazer(tmp, type = "text", digits = 2, 
			summary.stat = c("n", "mean", "sd", "min", "p25", "median", "p75","max"))
	
# Summary statistics by CVS segments
sel <- is.na(tmp$treat)
cat("Summary statistics for non CVS shoppers: \n")
stargazer(tmp[sel,], type = "text", digits = 2, 
			summary.stat = c("n", "mean", "sd", "min", "p25", "median", "p75","max"))

sel0 <- !is.na(tmp$treat) & tmp$treat == 0
cat("Summary statistics for CVS shoppers but did not buy cigarettes at CVS: \n")
stargazer(tmp[sel0,], type = "text", digits = 2, 
			summary.stat = c("n", "mean", "sd", "min", "p25", "median", "p75","max"))

sel1 <- !is.na(tmp$treat) & tmp$treat == 1
cat("Summary statistics for treated smokers: \n")
stargazer(tmp[sel1,], type = "text", digits = 2, 
			summary.stat = c("n", "mean", "sd", "min", "p25", "median", "p75","max"))			

# ----------------------------------------------------------# 
# Cigarrette market share by retailers / by channel in 2013 #
tmp		<- data.table(subset(purchases, year(purchase_date) == 2013))
tmp		<- tmp[,list(q = sum(quantity*size/qunit, na.rm=T)), by = list(channel_type)]
tmp		<- tmp[order(tmp$q, decreasing = T),]
tmp1	<- as.character(tmp[1:10,channel_type])
tmp$channel	<- ifelse(tmp$channel_type %in% tmp1, as.character(tmp$channel_type), "All other")
tmp		<- tmp[,list(q = sum(q)), by = list(channel)]
tmp		<- tmp[,mkt.share := q/sum(q)]
tmp$channel.lab	<- factor(tmp$channel, levels = tmp$channel, 
	labels = paste(as.character(tmp$channel), "(", round(tmp$mkt.share*100),"%)", sep=""))
cat("Market share by channel:\n"); print(tmp); cat("\n")

# Cigarrett sales within drug stores
sel		<- purchases$channel_type == "Drug Store" & year(purchases$purchase_date) == 2013
tmp		<- data.table(purchases[sel,])
tmp$cvs	<- 1*(tmp$retailer_code == cvs.ret)
tmp		<- tmp[,list(q = sum(quantity*size/qunit, na.rm=T)), by = list(cvs)]
tmp		<- tmp[,mkt.share := q/sum(q)]
cat("Market share within drug stores in 2013:\n"); print(tmp); cat("\n")

# Cigarette spending across trips #
tmp		<- subset(trips, purchase_date < event.date & retailer_code == cvs.ret)
tmp1	<- data.table(subset(purchases, purchase_date < event.date & retailer_code == cvs.ret))
tmp1	<- tmp1[,list(cigdol_cvs = sum(total_price_paid - coupon_value, na.rm=T)), by = list(trip_code_uc)]
tmp		<- merge(tmp, tmp1, by = "trip_code_uc", all.x = T)
tmp[is.na(tmp)]	<- 0
tmp$cig_frac_cvs	<- tmp$cigdol_cvs/tmp$total_spent
summary(tmp$cig_frac_cvs)
summary(tmp$cig_frac_cvs[tmp$cigdol_cvs>0])
sel1	<- tmp$cigdol_cvs > 0
ggplot(tmp, aes(total_spent, fill = factor(1*(cigdol_cvs > 0)), alpha = .7)) + geom_histogram(aes(y = ..density..), position = "identity")
quartz()
ggplot(tmp, aes(total_spent-cigdol_cvs, fill = factor(1*(cigdol_cvs > 0)), alpha = .7)) + geom_histogram(aes(y=..density..), position = "identity")


ggplot(tmp, aes(total_spent, fill = factor(1*(cigdol_cvs > 0)), alpha = .7)) + 
			geom_histogram(aes(y = ..density..), position = "identity") + 
			xlim(c(0, 200))
quartz()
ggplot(tmp, aes(total_spent-cigdol_cvs, fill = factor(1*(cigdol_cvs > 0)), alpha = .7)) + 
			geom_histogram(aes(y=..density..), position = "identity") + 
			xlim(c(0, 200))

######################
# Plot monthly trend #
######################
### NOTE: There are only 25 days in the last December

get_legend	<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

plots	<- list(NULL)
idx		<- 1
ggtmp0	<- data.table(subset(smk.trips, !is.na(treat) ))
ggtmp0	<- ggtmp0[,nhh:=length(unique(household_code)), by = list(treat)]		
ggtmp0	<- ggtmp0[,list(q_cvs= sum(q_cvs/nhh), q_othdrug = sum(q_othdrug/nhh), q_othchannel = sum(q_othchannel/nhh),q = sum(q/nhh), 
						trip_cvs = sum(trip_cvs/nhh), trip_othdrug = sum(trip_othdrug/nhh), trip_othchannel = sum(trip_othchannel/nhh),
						netdol_cvs = sum(netdol_cvs/nhh), netdol_othdrug = sum(netdol_othdrug/nhh), netdol_othchannel = sum(netdol_othchannel/nhh)
						), 
					by = list(treat, month)]
# ggtmp0$month	<- as.Date(paste(ifelse(ggtmp0$month <= 12, 2013, 2014), ifelse(ggtmp0$month %% 12 == 0, 12, ggtmp0$month%%12), "01", sep="-"), format = "%Y-%m-%d")					
ggtmp0$month	<- as.Date(paste(ceiling(ggtmp0$month/12)+2012, ifelse(ggtmp0$month %% 12 == 0, 12, ggtmp0$month%%12), "01", sep="-"), format = "%Y-%m-%d")					
ggtmp0$treat 	<- factor(ggtmp0$treat, levels = c(1, 0), labels = c("Treated", "Control"))	
(tmplab			<- setNames(c("Cigarettes at CVS", "Cigarettes at other drug stores", "Cigarettes at other channel", "Total", 
							  "Trip to CVS", "Trip to other drug stores", "Trip to other channel", 
							  "Exp. other basket items \n at CVS", "Exp. other basket items \n at other drug stores", "Exp. other basket items \n at other channel" 
							), names(ggtmp0)[-c(1:2)]))
blankPlot <- ggplot()+geom_blank(aes(1,1))+
				theme(	plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
   						panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), 
   						axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank() )

# Cigarett quantity
ggtmp	<- melt(ggtmp0[,c("treat","month",grep("q", names(ggtmp0), value = T)), with = FALSE], id.vars = c("treat", "month"))
ggtmp$variable	<- factor(ggtmp$variable, levels = grep("q", names(ggtmp0), value = T), 
						labels = tmplab[grep("q", names(ggtmp0), value = T)])

plots[[idx]] <- ggplot(subset(ggtmp, variable == tmplab["q_cvs"]), aes(month, value, linetype = treat)) + geom_line() + 
						geom_vline(xintercept = as.numeric(event.date), col = "red") + 
						facet_wrap(~variable, scales = "free") + 
						guides(linetype = guide_legend(title = "")) + 
						# theme(legend.position = "bottom") + 
						labs(y = "Volume (packs)")
legend <- get_legend(plots[[idx]])												
idx			<- idx + 1	

plots[[idx]]<- ggplot(subset(ggtmp, variable == tmplab["q"]), aes(month, value, linetype = treat)) + geom_line() + 
						geom_vline(xintercept = as.numeric(event.date), col = "red") + 
						facet_wrap(~variable, scales = "free") + 
						guides(linetype = guide_legend(title = "")) + 
						theme(legend.position = "none") + 
						labs(y = "Volume (packs)")
idx			<- idx + 1						
plots[[idx]]<- ggplot(subset(ggtmp, variable %in% tmplab[c("q_othdrug", "q_othchannel")]), aes(month, value, linetype = treat)) + geom_line() + 
						geom_vline(xintercept = as.numeric(event.date), col = "red") + 
						facet_wrap(~variable, scales = "free") + 
						guides(linetype = guide_legend(title = "")) + 
						theme(legend.position = "none") + 
						labs(y = "Volume (packs)") 
idx <- idx + 1	

# Shopping trips
ggtmp	<- melt(ggtmp0[,c("treat","month",grep("trip", names(ggtmp0), value = T)), with = FALSE], id.vars = c("treat", "month"))
ggtmp$variable	<- factor(ggtmp$variable, levels = grep("trip", names(ggtmp0), value = T), 
						labels = tmplab[grep("trip", names(ggtmp0), value = T)])

plots[[idx]] <- ggplot(subset(ggtmp, variable == tmplab["trip_cvs"]), aes(month, value, linetype = treat)) + geom_line() + 
						geom_vline(xintercept = as.numeric(event.date), col = "red") + 
						facet_wrap(~variable, scales = "free") + 
						guides(linetype = guide_legend(title = "")) + 
						theme(legend.position = "none") + 	
						labs(y = "No. trips")
idx	<- idx + 1						
plots[[idx]]	<- ggplot(subset(ggtmp, variable %in% tmplab[c("trip_othdrug", "trip_othchannel")]), aes(month, value, linetype = treat)) + geom_line() + 
						geom_vline(xintercept = as.numeric(event.date), col = "red") + 
						facet_wrap(~variable, scales = "free") + 
						guides(linetype = guide_legend(title = "")) + 
						theme(legend.position = "none") + 
						labs(y = "No. trips") 
idx	<- idx + 1

# Expenditure 
ggtmp	<- melt(ggtmp0[,c("treat","month",grep("netdol", names(ggtmp0), value = T)), with = FALSE], id.vars = c("treat", "month"))
ggtmp$variable	<- factor(ggtmp$variable, levels = grep("netdol", names(ggtmp0), value = T), 
						labels = tmplab[grep("netdol", names(ggtmp0), value = T)])

plots[[idx]] <- ggplot(subset(ggtmp, variable == tmplab["netdol_cvs"]), aes(month, value, linetype = treat)) + geom_line() + 
						geom_vline(xintercept = as.numeric(event.date), col = "red") + 
						facet_wrap(~variable, scales = "free") + 
						guides(linetype = guide_legend(title = "")) + 
						theme(legend.position = "none") +
						labs(y = "Expenditure on \nother basket items")
idx		<- idx + 1						
plots[[idx]]	<- ggplot(subset(ggtmp, variable %in% tmplab[c("netdol_othdrug", "netdol_othchannel")]), aes(month, value, linetype = treat)) + geom_line() + 
						geom_vline(xintercept = as.numeric(event.date), col = "red") + 
						facet_wrap(~variable, scales = "free") + 
						guides(linetype = guide_legend(title = "")) + 
						theme(legend.position = "none") + 
						labs(y = "Expenditure on \nother basket items") 
idx	<- idx + 1

pdf(paste(plot.wd, "/fg_", out.file,"_trend.pdf", sep=""), width = 5.5, height = 4)
print(plots[[1]])
print(grid.arrange(blankPlot, plots[[2]], legend, plots[[3]], nrow = 2, layout_matrix = rbind(c(1,2,2,3), rep(4, 4)) ) )
print(grid.arrange(blankPlot, plots[[4]], legend, plots[[5]], nrow = 2, layout_matrix = rbind(c(1,2,2,3), rep(4, 4)) ) )
print(grid.arrange(blankPlot, plots[[6]], legend, plots[[7]], nrow = 2, layout_matrix = rbind(c(1,2,2,3), rep(4, 4)) ) )
dev.off()

# --------------------- #
# Put all plots into one 
ggtmp	<- melt(ggtmp0[,c("treat","month",grep("q", names(ggtmp0), value = T)), with = FALSE], id.vars = c("treat", "month"))
ggtmp$variable	<- factor(ggtmp$variable, levels = grep("q", names(ggtmp0), value = T), 
						labels = tmplab[grep("q", names(ggtmp0), value = T)])
plots	<- ggplot(ggtmp, aes(month, value, linetype = treat)) + geom_line() + geom_point(size = .5) +
						geom_vline(xintercept = as.numeric(event.date), col = "red") + 
						facet_wrap(~variable, scales = "free") + 
						guides(linetype = guide_legend(title = "")) + 
						theme(legend.position = "bottom") + 
						labs(y = "Cigarette quantity (packs)")
legend <- get_legend(plots)												
						
# Shopping trips
ggtmp	<- melt(ggtmp0[,c("treat","month",grep("trip", names(ggtmp0), value = T)), with = FALSE], id.vars = c("treat", "month"))
ggtmp$variable	<- factor(ggtmp$variable, levels = grep("trip", names(ggtmp0), value = T), 
						labels = c("CVS", "Other drug stores", "Other channels"))
p1	<- ggplot(ggtmp, aes(month, value, linetype = treat)) + geom_line() + geom_point(size = .5) + 
						geom_vline(xintercept = as.numeric(event.date), col = "red") + 
						facet_wrap(~variable, scales = "free") + 
						guides(linetype = guide_legend(title = "")) + 
						theme(legend.position = "none", axis.text.x = element_text(size = rel(.8))) + 	
						labs(y = "No. trips")

ggtmp	<- melt(ggtmp0[,c("treat","month",grep("netdol", names(ggtmp0), value = T)), with = FALSE], id.vars = c("treat", "month"))
ggtmp$variable	<- factor(ggtmp$variable, levels = grep("netdol", names(ggtmp0), value = T), 
						labels = c("CVS", "Other drug stores", "Other channels"))
p2	<- ggplot(ggtmp, aes(month, value, linetype = treat)) + geom_line() + geom_point(size = .5) +
						geom_vline(xintercept = as.numeric(event.date), col = "red") + 
						facet_wrap(~variable, scales = "free") + 
						guides(linetype = guide_legend(title = "")) + 
						theme(legend.position = "none", axis.text.x = element_text(size = rel(.8))) + 	
						labs(y = "Expenditure on \nother basket items")

# lheight <- sum(legend$height)
ww1		<- 7.5
hh		<- 4.5
pdf(paste(plot.wd,"/fg_", out.file, "_trend_onepage.pdf",sep=""), width = ww1, height = hh)
print(plots )
print(grid.arrange(p1, p2, legend, heights = unit(c(hh*3/7,hh*3/7,hh/7), c("in", "in", "in")))
		)
dev.off()	

######################
# Plot weekly trend #
######################
firstw	<- as.Date("2012-12-31", format = "%Y-%m-%d") 	# The first week in 2013
trips$week	<- (as.numeric(trips$purchase_date - firstw) %/%7 + 1)* 7 + firstw - 1
purchases$week	<- (as.numeric(purchases$purchase_date - firstw) %/%7 + 1)* 7 + firstw - 1
trips$cvs	<- 1*(trips$retailer_code == cvs.ret)
purchases$cvs	<- 1*(purchases$retailer_code == cvs.ret)

tmp		<- data.table(trips)
tmp		<- tmp[,list(start = min(week), end = max(week)), 
					by = list(household_code)]
tmp		<- tmp[, n:= (end-start)/7]
tmp1	<- lapply(1:nrow(tmp), function(i) tmp[i,start] + c(0:tmp[i,n])*7)
names(tmp1)	<- tmp$household_code
tmp1	<- melt(tmp1)
names(tmp1)	<- c("week", "household_code")
tmp1$household_code	<- as.numeric(tmp1$household_code)

# Actual cigarette purchases 
tmp2	<- data.table(purchases)
tmp2	<- tmp2[,list(	q = sum(quantity*size/qunit, na.rm=T), 
						q_cvs 		= sum(quantity*size*cvs/qunit, na.rm=T),
						q_othdrug = sum(quantity*size*(1-cvs)*1*(channel_type == "Drug Store")/qunit, na.rm=T), 
						q_othchannel = sum(quantity*size*1*(channel_type != "Drug Store")/qunit, na.rm=T), 
						q_convenience = sum(quantity*size*1*(channel_type == "Convenience Store")/qunit, na.rm=T), 
						cigdol 		= sum(total_price_paid - coupon_value, na.rm=T), 
						cigdol_cvs	= sum((total_price_paid - coupon_value)*cvs, na.rm=T),
						cigdol_othdrug 	= sum((total_price_paid - coupon_value)*(1-cvs)*1*(channel_type == "Drug Store"), na.rm=T), 
						cigdol_othchannel= sum((total_price_paid - coupon_value)*1*(channel_type != "Drug Store"), na.rm=T) 
						),
				by = list(household_code, week)]
dim(tmp1); dim(tmp2)
tmpdat	<- merge(tmp1, tmp2, by = c("household_code", "week"), all.x = T)
dim(tmpdat)

# Trips and spending 
tmp1 	<- data.table(trips)
tmp1	<- tmp1[,list(total_spent = sum(total_spent)), by = list(household_code, week, purchase_date, channel_type, retailer_code, cvs)]
tmp1	<- tmp1[,list(	trip_cvs 		= length(purchase_date[cvs==1]), 
						trip_othdrug 	= length(purchase_date[channel_type == "Drug Store" & cvs ==0] ), 
						trip_othchannel = length(purchase_date[channel_type != "Drug Store"]), 
						trip_total		= length(purchase_date),
						dol_cvs 		= sum(total_spent*cvs, na.rm = T), 
						dol_othdrug		= sum(total_spent*(1-cvs)*1*(channel_type == "Drug Store"), na.rm = T), 
						dol_othchannel	= sum(total_spent*1*(channel_type != "Drug Store"), na.rm = T), 
						dol_total		= sum(total_spent)
						), 
				by = list(household_code, week)]
dim(tmp1)		
summary(tmp1[,list(trip_cvs, trip_othdrug, trip_othchannel)])
tmpdat	<- merge(tmpdat, tmp1, by = c("household_code", "week"), all.x = T)
sel 	<- is.na(tmpdat)
tmpdat[sel]	<- 0
tmpdat$netdol			<- with(tmpdat, dol_total - cigdol)
tmpdat$netdol_cvs		<- with(tmpdat, dol_cvs - cigdol_cvs)
tmpdat$netdol_othdrug	<- with(tmpdat, dol_othdrug - cigdol_othdrug)
tmpdat$netdol_othchannel<- with(tmpdat, dol_othchannel - cigdol_othchannel)
cat("Summary stats:\n"); print(summary(tmpdat)); cat("\n")

tmpdat	<- merge(tmpdat, smk.pan[,c("household_code", "treat")], by = "household_code", all.x = T)

plots	<- list(NULL)
idx		<- 1
ggtmp0	<- data.table(subset(tmpdat, !is.na(treat) ))
ggtmp0	<- ggtmp0[,nhh:=length(unique(household_code)), by = list(treat)]		
ggtmp0	<- ggtmp0[,list(q_cvs= sum(q_cvs/nhh), q_othdrug = sum(q_othdrug/nhh), q_othchannel = sum(q_othchannel/nhh),q = sum(q/nhh), 
						trip_cvs = sum(trip_cvs/nhh), trip_othdrug = sum(trip_othdrug/nhh), trip_othchannel = sum(trip_othchannel/nhh),
						netdol_cvs = sum(netdol_cvs/nhh), netdol_othdrug = sum(netdol_othdrug/nhh), netdol_othchannel = sum(netdol_othchannel/nhh)
						), 
					by = list(treat, week)]
ggtmp0$treat 	<- factor(ggtmp0$treat, levels = c(1, 0), labels = c("Treated", "Control"))	
(tmplab			<- setNames(c("Cigarettes at CVS", "Cigarettes at other drug stores", "Cigarettes at other channel", "Total", 
							  "Trip to CVS", "Trip to other drug stores", "Trip to other channel", 
							  "Exp. other basket items \n at CVS", "Exp. other basket items \n at other drug stores", "Exp. other basket items \n at other channel" 
							), names(ggtmp0)[-c(1:2)]))
blankPlot <- ggplot()+geom_blank(aes(1,1))+
				theme(	plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
   						panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), 
   						axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank() )

# --------------------- #
# Put all plots into one 
ggtmp	<- melt(ggtmp0[,c("treat","week",grep("q", names(ggtmp0), value = T)), with = FALSE], id.vars = c("treat", "week"))
ggtmp$variable	<- factor(ggtmp$variable, levels = grep("q", names(ggtmp0), value = T), 
						labels = tmplab[grep("q", names(ggtmp0), value = T)])
plots	<- ggplot(ggtmp, aes(week, value, linetype = treat)) + geom_line() + geom_point(size = .3) +
						geom_vline(xintercept = as.numeric(event.date), col = "red") + 
						facet_wrap(~variable, scales = "free") + 
						guides(linetype = guide_legend(title = "")) + 
						theme(legend.position = "bottom") + 
						labs(y = "Cigarette quantity (packs)")
legend <- get_legend(plots)												
						
# Shopping trips
ggtmp	<- melt(ggtmp0[,c("treat","week",grep("trip", names(ggtmp0), value = T)), with = FALSE], id.vars = c("treat", "week"))
ggtmp$variable	<- factor(ggtmp$variable, levels = grep("trip", names(ggtmp0), value = T), 
						labels = c("CVS", "Other drug stores", "Other channels"))
p1	<- ggplot(ggtmp, aes(week, value, linetype = treat)) + geom_line() + 
						geom_point(size = .3) + 
						geom_vline(xintercept = as.numeric(event.date), col = "red") + 
						facet_wrap(~variable, scales = "free") + 
						guides(linetype = guide_legend(title = "")) + 
						theme(legend.position = "none", axis.text.x = element_text(size = rel(.8))) + 	
						labs(y = "No. trips")

ggtmp	<- melt(ggtmp0[,c("treat","week",grep("netdol", names(ggtmp0), value = T)), with = FALSE], id.vars = c("treat", "week"))
ggtmp$variable	<- factor(ggtmp$variable, levels = grep("netdol", names(ggtmp0), value = T), 
						labels = c("CVS", "Other drug stores", "Other channels"))
p2	<- ggplot(ggtmp, aes(week, value, linetype = treat)) + geom_line() + 
						geom_point(size = .1) +
						geom_vline(xintercept = as.numeric(event.date), col = "red") + 
						facet_wrap(~variable, scales = "free") + 
						guides(linetype = guide_legend(title = "")) + 
						theme(legend.position = "none", axis.text.x = element_text(size = rel(.8))) + 	
						labs(y = "Expenditure on \nother basket items")

# lheight <- sum(legend$height)
ww1		<- 7.5
hh		<- 4.5
pdf(paste(plot.wd,"/fg_", out.file, "_wktrend.pdf",sep=""), width = ww1, height = hh)
print(plots )
print(grid.arrange(p1, p2, legend, heights = unit(c(hh*3/7,hh*3/7,hh/7), c("in", "in", "in")))
		)
dev.off()	
					
#########################
# Household differences # 
#########################
match.col	<- c("distance_cvs", "pre_q", "pre_trip_total", "pre_trip_othchannel", "pre_dol_total", "pre_dol_othchannel", 
				"income", "age", "have_kids", "employment")
X			<- model.matrix(as.formula(paste("~ treat+", paste(match.col, collapse = "+"), "-1")), data = smk.pan)
X			<- cbind(X, model.matrix(~ treat + race - 1, data = smk.pan)[,-1])
colnames(X)

# T-test 
tmp.tab	<- NULL
sel		<- X[,"treat"] == 1
for(i in 2:ncol(X)){
	# tmp	<- t.test(as.formula(paste(sel.col[i], "~ treat")), data = smk.pan)
	tmp		<- t.test(X[!sel,i], X[sel,i])
	tmp.tab	<- rbind(tmp.tab, unlist(tmp[c("estimate", "p.value")]))
}
dimnames(tmp.tab)	<- list(c("Distance to CVS", "Cigarette consumption", "No. total trips", "No. trips to other channels", 
								"Total expenditure", "Expenditure at other channels", 
						"Income", "Age", "Have kids", "One person unemployed", "One person employed", 
						"Two people only one employed", "Two people both employed", "Two people both unemployed", 
						"Race - White", "Race - African American", "Race - Asian", "Race - Other"),
					 c("Control","Treated", "p value"))
cat("T test of difference between treated and control:\n"); print(round(tmp.tab,2)); cat("\n")
stargazer(tmp.tab, type = "latex", summary = F, digits = 2, label = "lab:smk_diff", align = FALSE)

# QQ plot
sel.col	<- c("income", "age", "distance_cvs", 
               "pre_q", "pre_trip_total", "pre_trip_othchannel", "pre_dol_total", "pre_dol_othchannel")
(col.lab	<- setNames(c("Income", "Age", "Distance to CVS", "Cigarette consumption", "No. total trips", "No. trips to other channels", 
					   "Total expenditure", "Expenditure at other channels"), 
					sel.col))
sel1	<- smk.pan$treat == 1
sel2	<- smk.pan$treat == 0
par(mfrow = c(2,4))
for(i in 1:length(sel.col)){
	rg	<- range(smk.pan[sel1|sel2,sel.col[i]], na.rm= T)
	qqplot(smk.pan[sel1,sel.col[i]], smk.pan[sel2,sel.col[i]], xlim = rg, ylim = rg,xlab = "Treated", ylab = "Control", main = col.lab[i]) 
	abline(a = 0, b = 1, col = "red")
}

######################################
# Summary statistics from nonsmokers # 
######################################
load("cvs_nonsmk.rdata")
nonsmk.pan$treat	<- nonsmk.pan[,paste("treat", treat.code, sep="")]
nonsmk.trips$treat	<- nonsmk.trips[,paste("treat",treat.code,sep="")]

cat("Number of non-smokers =", sum(nonsmk.pan$smk ==0),"\n")
table(nonsmk.pan$treat)
table(nonsmk.pan$treat, nonsmk.pan$frac_seg)
table(nonsmk.pan$smk, nonsmk.pan$frac_seg)

# T-test 
match.col	<- c("distance_cvs", "pre_trip_cvs", "pre_trip_othdrug", "pre_trip_othchannel", "pre_dol_cvs", "pre_dol_othdrug", "pre_dol_othchannel", 
				"income", "age", "have_kids", "employment")
X			<- model.matrix(as.formula(paste("~ treat+", paste(match.col, collapse = "+"), "-1")), data = nonsmk.pan)
X			<- cbind(X, model.matrix(~ treat + race - 1, data = nonsmk.pan)[,-1])
colnames(X)

tmp.tab	<- NULL
sel		<- X[,"treat"] == 1
for(i in 2:ncol(X)){
	# tmp	<- t.test(as.formula(paste(sel.col[i], "~ treat")), data = nonsmk.pan)
	tmp		<- t.test(X[!sel,i], X[sel,i])
	tmp.tab	<- rbind(tmp.tab, unlist(tmp[c("estimate", "p.value")]))
}
dimnames(tmp.tab)	<- list(c("Distance to CVS", "No. trips to CVS", "No. trips to other drug stores", "No. trips to other channels", 
					   "Expenditure at CVS", "Expenditure at other drug stores", "Expenditure at other channels", 
							"Income", "Age", "Have kids", "One person unemployed", "One person employed", 
							"Two people only one employed", "Two people both employed", "Two people both unemployed", 
							"Race - White", "Race - African American", "Race - Asian", "Race - Other"), 
							c("Control","Treated", "p value"))
cat("T test of difference between treated and control:\n"); print(round(tmp.tab,2)); cat("\n")
stargazer(tmp.tab, type = "latex", summary = F, digits = 2, label = "lab:nonsmk_diff", align = FALSE, 
			title = "Difference between treated smokers and non-smokers")

# QQ plot
sel.col	<- c("income", "age", "distance_cvs", "pre_trip_cvs", "pre_trip_othdrug", "pre_trip_othchannel", "pre_dol_cvs", "pre_dol_othdrug", "pre_dol_othchannel")
(col.lab	<- setNames(c("Income", "Age", "Distance to CVS", "No. trips to CVS", "No. trips to other drug stores", "No. trips to other channels", 
					   "Expenditure at CVS", "Expenditure at other drug stores", "Expenditure at other channels"), 
					sel.col))
sel1	<- nonsmk.pan$treat == 1
sel2	<- nonsmk.pan$treat == 0
par(mfrow = c(3,3))
for(i in 1:length(sel.col)){
	rg	<- range(nonsmk.pan[sel1|sel2,sel.col[i]], na.rm=T)
	qqplot(nonsmk.pan[sel1,sel.col[i]], nonsmk.pan[sel2,sel.col[i]], xlim = rg, ylim = rg,xlab = "Treated", ylab = "Control", main = col.lab[i]) 
}


			