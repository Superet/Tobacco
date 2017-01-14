library(reshape2)
library(ggplot2)
library(data.table)
library(lubridate)
library(scales)
library(stargazer)

# setwd("U:/Users/ccv103/Desktop")
setwd("~/Documents/Research/Tobacco/processed_data")
plot.wd		<- "~/Desktop"
ww			<- 6.5
ar			<- .6

panelists 	<- read.csv("tob_CVS_pan.csv", header = T)
purchases	<- read.csv("tob_CVS_purchases.csv", header = T)
trips		<- read.csv("tob_CVS_trips.csv", header = T)

#################
# Organize data # 
#################
# Add week
event.date	<- as.Date("2014-09-01", format = "%Y-%m-%d")
cvs.ret	<- 4914		# retailer_code for CVS
qunit	<- 20		# 20 cigaretts per pack 
firstw	<- as.Date("2012-12-31", format = "%Y-%m-%d") 	# The first week in 2013
purchases$purchase_date	<- as.Date(as.character(purchases$purchase_date), format = "%Y-%m-%d")
purchases$week		<- ((as.numeric(purchases$purchase_date - firstw)) %/%7 + 1)* 7 + firstw - 1
purchases$year		<- year(purchases$purchase_date)
purchases$month		<- month(purchases$purchase_date)
purchases$month		<- ifelse(purchases$year == 2012, 1, ifelse(purchases$year == 2013, purchases$month, purchases$month + 12))

trips$purchase_date	<- as.Date(as.character(trips$purchase_date), format = "%Y-%m-%d")
trips$week			<- ((as.numeric(trips$purchase_date - firstw)) %/%7 + 1)* 7 + firstw - 1
trips$year			<- year(trips$purchase_date)
trips$month			<- month(trips$purchase_date)
trips$month			<- ifelse(trips$year == 2012, 1, ifelse(trips$year == 2013, trips$month, trips$month + 12))
endweek				<- c(min(purchases$week), max(purchases$week))

# Mark CVS
trips$cvs	<- ifelse(trips$retailer_code == cvs.ret, 1, 0)
purchases	<- merge(purchases, trips[,c("trip_code_uc", "cvs", "channel_type", "retailer_code")], by = "trip_code_uc", all.x=T)

# Classify households distance to CVS
sum(is.na(panelists$distance)); sum(is.na(panelists$distance_wgr))
hist(panelists$distance)
median(panelists$distance)
tmp	<- data.table(panelists)
tmp	<- tmp[,list(nzip = length(unique(panelist_zip_code)), nd = length(unique(distance))), by = list(household_code)]
summary(tmp)
mean(tmp$nzip>1)

# Distribution of distance to Walgreens
hist(panelists$distance_wgr, breaks = 100)
median(panelists$distance_wgr)
cor(panelists$distance, panelists$distance_wgr, use = "complete.obs")
plot(panelists$distance, panelists$distance_wgr)

panelists$cvs_in2	<- ifelse(panelists$distance <=2, 1, 0)
panelists$cvs_in5	<- ifelse(panelists$distance <=5, 1, 0)
panelists$cvs_in10	<- ifelse(panelists$distance <=10, 1, 0)
panelists$wgr_in2	<- ifelse(panelists$distance_wgr <=2, 1, 0)

# Classify light vs heavy smokers
tmp1		<- data.table(purchases)
setkeyv(tmp1, c("household_code", "purchase_date"))
tmp1		<- tmp1[, list(consum = sum(quantity*size/qunit, na.rm=T)/(as.numeric(max(purchase_date)-min(purchase_date)))*7), 
					by = list(household_code)]
summary(tmp1$consum)
tmp1$heavy 	<- 1*(tmp1$consum > 2.5)
tmp			<- setNames(tmp1$heavy, tmp1$household_code)
panelists$heavy 	<- tmp[as.character(panelists$household_code)]

# Merge household data to purchase and trip data 
purchases	<- merge(purchases, panelists[,c("household_code", "panel_year", "distance","cvs_in2", "cvs_in5", "cvs_in10", "wgr_in2", "heavy")], 
						by.x = c("household_code", "year"), by.y = c("household_code", "panel_year"), all.x=T)
trips	<- merge(trips, panelists[,c("household_code", "panel_year", "cvs_in2", "cvs_in5", "cvs_in10", "wgr_in2","heavy")], 
						by.x = c("household_code", "year"), by.y = c("household_code", "panel_year"), all.x=T)
						
purchases$cvs_c	<- factor(purchases$cvs, levels = c(1,0), labels = c("CVS", "Non-CVS"))

# Check the tenure of households 
tmp		<- data.table(trips)
tmp		<- tmp[,list(n = length(unique(week)), cvs_in2 = mean(cvs_in2, na.rm=T)), by = list(household_code)]
tmp		<- tmp[cvs_in2 %in% c(0,1),]
ggplot(tmp, aes(n, fill = factor(cvs_in2), alpha = .5)) + geom_histogram(aes(y = ..density..), position = "identity")

#################
# Summary stats # 
#################
cat("Number of households in the data:", length(unique(panelists$household_code)))

# For each household, we look at their weekly cigarrette consumption level 
tmp1	<- data.table(purchases)
setkeyv(tmp1, c("household_code", "purchase_date"))
tmp2	<- tmp1[, month := month(purchase_date)]
tmp2	<- tmp1[, list(nstore = length(unique(retailer_code))), by = list(household_code, year, month)]
tmp2	<- tmp2[, list(nstore = as.numeric(mean(nstore))), by = list(household_code)]
# tmp1	<- tmp1[, list(consum = sum(quantity*size/qunit, na.rm=T)/length(unique(week))), by = list(household_code)]
tmp1	<- tmp1[, list(consum = sum(quantity*size/qunit, na.rm=T)/(as.numeric(max(purchase_date)-min(purchase_date)))*7, 
						interpurchase = mean(as.numeric(diff(unique(purchase_date)))) ), 
					by = list(household_code)]
tmp1	<- merge(tmp1, tmp2, by = "household_code", all.x = T)					

# We also look at their weekly trips
tmp2	<- data.table(trips)
tmp2	<- tmp2[, list(	ntrip = length(purchase_date)/(as.numeric(max(purchase_date)-min(purchase_date)))*7, 
						ncvs = length(purchase_date[cvs==1])/(as.numeric(max(purchase_date)-min(purchase_date)))*7, 
						nother_drug = length(purchase_date[cvs==0 & channel_type == "Drug Store"])/(as.numeric(max(purchase_date)-min(purchase_date)))*7 ), 
				by = list(household_code)]
tmp		<- merge(tmp1, tmp2, by = "household_code")

# Combine with the average distance to CVS over year; 
tmp1	<- data.table(panelists)
tmp1	<- tmp1[,list(distance = mean(distance)), by = list(household_code)]
tmp		<- merge(tmp, tmp1, by = "household_code")
cat("Summary statistics across households:\n"); print(summary(tmp)); cat("\n")
stargazer(tmp[,list(consum, interpurchase, ntrip, ncvs, nother_drug,distance)], type = "html", 
			title = "Summary statistics of household cigarette consumption, weekly trips, and distance to CVS", 
			covariate.labels = c("Consumption (packs)", "Inter-purchase days", "No. stores of cigarett trips/month", 
								"No. trips", "Trips to CVS", "Trips to other drug stores", "Distance to CVS (mi.)"), 
			summary.stat = c("n", "min", "median", "mean", "p25", "p75", "max", "sd"), 
			out = paste(plot.wd, "/cvs_sumstat.html", sep=""))			

# --------------------------------------------------# 
# Cigarrette market share by retailers / by channel #
tmp		<- data.table(purchases)
tmp		<- tmp[,list(q = sum(quantity*size/qunit, na.rm=T)), by = list(channel_type)]
tmp		<- tmp[order(tmp$q, decreasing = T),]
tmp1	<- as.character(tmp[1:10,channel_type])
tmp$channel	<- ifelse(tmp$channel_type %in% tmp1, as.character(tmp$channel_type), "All other")
tmp		<- tmp[,list(q = sum(q)), by = list(channel)]
tmp		<- tmp[,mkt.share := q/sum(q)]
tmp$channel.lab	<- factor(tmp$channel, levels = tmp$channel, 
	labels = paste(as.character(tmp$channel), "(", round(tmp$mkt.share*100),"%)", sep=""))

pdf(paste(plot.wd, "/fg_pie_mktshare.pdf", sep=""), width = 6, height = 5)
ggplot(tmp, aes(x = 1, mkt.share, fill = channel.lab)) + geom_bar(stat = "identity", width = 1)+
		coord_polar(theta = "y") + 
		guides(fill = guide_legend(title = "")) + 
		labs(y = "Market share of channels in 2013 - 2014")
dev.off()		

# Cigarrett sales within drug stores
sel		<- purchases$channel_type == "Drug Store" & year(purchases$purchase_date) == 2013
tmp		<- data.table(purchases[sel,])
tmp		<- tmp[,list(q = sum(quantity*size/qunit, na.rm=T)), by = list(cvs)]
tmp		<- tmp[,mkt.share := q/sum(q)]
cat("Market share within drug stores in 2013:\n"); print(tmp); cat("\n")

########################################
# Overall trend of cigarette purchases #
########################################
# Trend of total cigarette sales in CVS, non-CVS drug stores, and all other retailers. 
ggtmp	<- data.table(subset(purchases, !week %in% endweek))
ggtmp	<- ggtmp[,list(Total = sum(quantity*size/qunit, na.rm=T), 
						CVS = sum(quantity*size*cvs/qunit, na.rm=T), 
	 					OtherDrug = sum(quantity*size*(1-cvs)*1*(channel_type=="Drug Store")/qunit, na.rm=T), 
						OtherChannel = sum(quantity*size*(1-cvs)*1*(channel_type!="Drug Store")/qunit, na.rm=T)), 
				by = list(week)]
ggtmp1	<- melt(ggtmp, id.vars = "week")	

pdf(paste(plot.wd, "/fg_overall_trend.pdf", sep=""), width = ww, height = ww*ar)
print(ggplot(ggtmp1, aes(x = week, value)) + geom_line() + facet_wrap(~variable, scales = "free") + 
			labs(y = "Volume (packs)", title = "Trend of cigarette weekly sales")	)

# Trend of market share 
ggtmp	<- ggtmp[,':='(CVS = CVS/Total, OtherDrug = OtherDrug/Total, OtherChannel = OtherChannel/Total )]		
ggtmp1	<- melt(ggtmp, id.vars = "week")
print(ggplot(subset(ggtmp1, variable != "Total"), aes(x = week, value)) + geom_line() + facet_wrap(~variable, scales = "free") + 
			scale_y_continuous(labels = percent) + 
			labs(y = "Market share", title = "Trend of market share")	)
dev.off()			

# ---------------------------------------------------------------------- #
# Compare per capita consumption trend by consumer group -- Close to CVS#			
ggtmp	<- data.table(purchases)			
ggtmp	<- ggtmp[!week %in% endweek, list(Total = sum(quantity*size/qunit, na.rm=T)/length(unique(household_code)), 
					CVS = sum(quantity*size*cvs/qunit, na.rm=T)/length(unique(household_code)), 
 					OtherDrug = sum(quantity*size*(1-cvs)*1*(channel_type=="Drug Store")/qunit, na.rm=T)/length(unique(household_code)), 
					OtherChannel = sum(quantity*size*(1-cvs)*1*(channel_type!="Drug Store")/qunit, na.rm=T)/length(unique(household_code))), 
				by = list(cvs_in2,week)]
ggtmp	<- melt(ggtmp, id.vars = c("cvs_in2", "week"))
ggtmp$cvs_in2	<- factor(ggtmp$cvs_in2, levels = c(1, 0), labels = c("Less than 2 mi.", "Greater than 2 mi."))
pdf(paste(plot.wd, "/fg_percap_trend_dist.pdf", sep=""), width = ww, height = ww*ar)
ggplot(ggtmp, aes(week, value, linetype = cvs_in2)) + geom_line() + 
			geom_vline(xintercept = as.numeric(event.date), col = "red") + 
			facet_wrap(~variable, scales = "free") + 
			guides(linetype = guide_legend(title = "Distance to CVS")) + 
			theme(legend.position = "bottom") + 
			labs(y = "Volume (packs)", title = "Per capita consumption by consumer groups") 
dev.off()		

# cigarette spending 
ggtmp	<- data.table(purchases)			
ggtmp	<- ggtmp[!week %in% endweek, list(Total = sum(total_price_paid, na.rm=T)/length(unique(household_code)), 
					CVS = sum(total_price_paid*cvs, na.rm=T)/length(unique(household_code)), 
 					OtherDrug = sum(total_price_paid*(1-cvs)*1*(channel_type=="Drug Store"), na.rm=T)/length(unique(household_code)), 
					OtherChannel = sum(total_price_paid*(1-cvs)*1*(channel_type!="Drug Store"), na.rm=T)/length(unique(household_code))), 
				by = list(cvs_in2,week)]
ggtmp	<- melt(ggtmp, id.vars = c("cvs_in2", "week"))
ggtmp$cvs_in2	<- factor(ggtmp$cvs_in2, levels = c(1, 0), labels = c("Less than 2 mi.", "Greater than 2 mi."))

ggplot(ggtmp, aes(week, value, linetype = cvs_in2)) + geom_line() + 
			geom_vline(xintercept = as.numeric(event.date), col = "red") + 
			facet_wrap(~variable, scales = "free") + 
			guides(linetype = guide_legend(title = "Distance to CVS")) + 
			theme(legend.position = "bottom") + 
			labs(y = "Spending ($)", title = "Per capita consumption by consumer groups") 


# -----------------------------------------------------------------#						
# Compare consumption trend by consumer group -- close to Walgreen #
ggtmp	<- data.table(purchases)			
ggtmp	<- ggtmp[!week %in% endweek, list(Total = sum(quantity*size/qunit, na.rm=T), 
					CVS = sum(quantity*size*cvs/qunit, na.rm=T), 
 					OtherDrug = sum(quantity*size*(1-cvs)*1*(channel_type=="Drug Store")/qunit, na.rm=T), 
					OtherChannel = sum(quantity*size*(1-cvs)*1*(channel_type!="Drug Store")/qunit, na.rm=T)), 
				by = list(cvs_in2, wgr_in2,week)]
ggtmp1	<- melt(ggtmp, id.vars = c("cvs_in2", "wgr_in2", "week"))
ggtmp1$wgr_in2	<- factor(ggtmp1$wgr_in2, levels = c(1, 0), labels = c("Less than 2 mi.", "Greater than 2 mi."))
ggtmp1$cvs_in2	<- factor(ggtmp1$cvs_in2, levels = c(1, 0), labels = c("Less than 2 mi.", "Greater than 2 mi."))

pdf(paste(plot.wd, "/fg_overall_trend_distwgr.pdf", sep=""), width = ww, height = ww*ar)
ggplot(subset(ggtmp1, !is.na(cvs_in2) & !is.na(wgr_in2)), aes(week, value, linetype = cvs_in2, color = wgr_in2)) + geom_line() + 
			geom_vline(xintercept = as.numeric(event.date), col = "red") + 
			facet_wrap(~variable, scales = "free") + 
			guides(linetype = guide_legend(title = "Distance to CVS"), color = guide_legend(title = "Distance to Walgreens")) + 
			theme(legend.position = "bottom") + 
			labs(y = "Volume (packs)", title = "Total cigarette sales by consumer groups") 
dev.off()			

# Compare the market share by consumer group
ggtmp	<- ggtmp[,':='(CVS = CVS/Total, OtherDrug = OtherDrug/Total, OtherChannel = OtherChannel/Total)]
ggtmp1	<- melt(ggtmp, id.vars = c("cvs_in2", "wgr_in2", "week"))
ggtmp1$wgr_in2	<- factor(ggtmp1$wgr_in2, levels = c(1, 0), labels = c("Less than 2 mi.", "Greater than 2 mi."))
ggtmp1$cvs_in2	<- factor(ggtmp1$cvs_in2, levels = c(1, 0), labels = c("Less than 2 mi.", "Greater than 2 mi."))
ggplot(subset(ggtmp1, variable != "Total" & !is.na(cvs_in2) & !is.na(wgr_in2)), 
			aes(week, value, linetype = cvs_in2, color = wgr_in2)) + geom_line() + 
			geom_vline(xintercept = as.numeric(event.date), col = "red") + 
			facet_wrap(~variable, scales = "free") + 
			guides(linetype = guide_legend(title = "Distance to Walgreens")) + 
			theme(legend.position = "bottom") + 
			scale_y_continuous(labels = percent) + 
			labs(y = "Market share", title = "Market share by consumer groups")			


# ---------------------------------------------------------------------------- #
# Compare per capita consumption trend by consumer group -- Close to Walgreens #			
ggtmp	<- data.table(purchases)			
ggtmp	<- ggtmp[!week %in% endweek, list(Total = sum(quantity*size/qunit, na.rm=T)/length(unique(household_code)), 
					CVS = sum(quantity*size*cvs/qunit, na.rm=T)/length(unique(household_code)), 
 					OtherDrug = sum(quantity*size*(1-cvs)*1*(channel_type=="Drug Store")/qunit, na.rm=T)/length(unique(household_code)), 
					OtherChannel = sum(quantity*size*(1-cvs)*1*(channel_type!="Drug Store")/qunit, na.rm=T)/length(unique(household_code))), 
				by = list(cvs_in2, wgr_in2,week)]
ggtmp	<- melt(ggtmp, id.vars = c("cvs_in2", "wgr_in2", "week"))
ggtmp$wgr_in2	<- factor(ggtmp$wgr_in2, levels = c(1, 0), labels = c("Less than 2 mi.", "Greater than 2 mi."))
ggtmp$cvs_in2	<- factor(ggtmp$cvs_in2, levels = c(1, 0), labels = c("Less than 2 mi.", "Greater than 2 mi."))

pdf(paste(plot.wd, "/fg_percap_trend_distwgr.pdf", sep=""), width = ww, height = ww*ar)
ggplot(subset(ggtmp, !is.na(cvs_in2) & !is.na(wgr_in2)), 
			aes(week, value, linetype = cvs_in2, color = wgr_in2)) + geom_line() + 
			geom_vline(xintercept = as.numeric(event.date), col = "red") + 
			facet_wrap(~variable, scales = "free") + 
			guides(linetype = guide_legend(title = "Distance to CVS"), color = guide_legend(title = "Distance to Walgreens")) + 
			theme(legend.position = "bottom") + 
			labs(y = "Volume (packs)", title = "Per capita consumption by consumer groups") 
dev.off()		

# ---------------------------------------------------------------------------------- #
# Compare per capita consumption trend by consumer group -- light vs. heavy smokers #			
ggtmp	<- data.table(purchases)			
ggtmp	<- ggtmp[!week %in% endweek, list(Total = sum(quantity*size/qunit, na.rm=T)/length(unique(household_code)), 
					CVS = sum(quantity*size*cvs/qunit, na.rm=T)/length(unique(household_code)), 
 					OtherDrug = sum(quantity*size*(1-cvs)*1*(channel_type=="Drug Store")/qunit, na.rm=T)/length(unique(household_code)), 
					OtherChannel = sum(quantity*size*(1-cvs)*1*(channel_type!="Drug Store")/qunit, na.rm=T)/length(unique(household_code))), 
				by = list(cvs_in2, heavy,week)]
ggtmp	<- melt(ggtmp, id.vars = c("cvs_in2", "heavy", "week"))
ggtmp$heavy	<- factor(ggtmp$heavy, levels = c(1, 0), labels = c("Heavy", "Light"))
ggtmp$cvs_in2	<- factor(ggtmp$cvs_in2, levels = c(1, 0), labels = c("Less than 2 mi.", "Greater than 2 mi."))

pdf(paste(plot.wd, "/fg_percap_trend_heavy.pdf", sep=""), width = ww, height = ww*ar)
ggplot(subset(ggtmp, !is.na(cvs_in2) & !is.na(heavy)), aes(week, value, linetype = cvs_in2, color = heavy)) + geom_line() + 
			geom_vline(xintercept = as.numeric(event.date), col = "red") + 
			facet_wrap(~variable, scales = "free") + 
			guides(linetype = guide_legend(title = "Distance to CVS"), color = guide_legend(title = "Smokers")) + 
			theme(legend.position = "bottom") + 
			labs(y = "Volume (packs)", title = "Per capita consumption by consumer groups") 
dev.off()		

###########################
# Trend of traffic to CVS # 
###########################
ggtmp	<- data.table(subset(trips, retailer_code == cvs.ret))
ggtmp	<- ggtmp[!week %in% endweek,list(n = length(purchase_date)), by = list(week)]
ggplot(ggtmp, aes(week, n)) + geom_line()

# Trend between households who live far vs close to CVS
ggtmp	<- data.table(subset(trips, !week %in% endweek & !is.na(cvs_in5)))
ggtmp	<- ggtmp[,list(	Total = length(purchase_date),
						CVS = length(purchase_date[cvs==1]), OtherDrug = length(purchase_date[cvs==0&channel_type=="Drug Store"]), 
						OtherChannel = length(purchase_date[cvs==0 & channel_type != "Drug Store"])), 
				by = list(cvs_in2, week)]
ggtmp	<- melt(ggtmp, id.vars = c("cvs_in2", "week"))		
ggtmp$cvs_in2	<- factor(ggtmp$cvs_in2, levels = c(1, 0), labels = c("Dist w/ 2miles", "Dist ge 2miles"))
		
ggplot(ggtmp, aes(week, value, linetype = cvs_in2)) + geom_line() + 
			geom_vline(xintercept = as.numeric(event.date), col = "red") + 
			facet_wrap(~variable, scales = "free") + 
			labs(y = "Traffic", title = "Total store traffic by consumer groups") 

# -----------------------------------#
# Shopping trips per week per person #
ggtmp	<- data.table(subset(trips, !week %in% endweek & !is.na(cvs_in5)))
ggtmp	<- ggtmp[,list(	Total = length(purchase_date)/length(unique(household_code)), 
						CVS = length(purchase_date[cvs==1])/length(unique(household_code)), 
						OtherDrug = length(purchase_date[cvs==0&channel_type=="Drug Store"])/length(unique(household_code)), 
						OtherChannel = length(purchase_date[cvs==0 & channel_type != "Drug Store"])/length(unique(household_code))), 
				by = list(cvs_in2, week)]
ggtmp	<- melt(ggtmp, id.vars = c("cvs_in2", "week"))		
ggtmp$cvs_in2	<- factor(ggtmp$cvs_in2, levels = c(1, 0), labels = c("Less than 2 mi.", "Greater than 2 mi."))

pdf(paste(plot.wd, "/fg_trips_dist.pdf", sep=""), width = ww, height = ww*ar)	
ggplot(ggtmp, aes(week, value, linetype = cvs_in2)) + geom_line() + 
			geom_vline(xintercept = as.numeric(event.date), col = "red") + 
			facet_wrap(~variable, scales = "free") + 
			guides(linetype = guide_legend(title = "Distance to CVS")) + 
			theme(legend.position = "bottom") + 
			labs(y = "Trips per person", title = "Shopping trips by consumer groups")
dev.off()			

# --------------------------------------------------------------- #
# Compare shopping trips by customer groups -- Close to Walgreens # 
ggtmp	<- data.table(subset(trips, !week %in% endweek & !is.na(cvs_in5)))
ggtmp	<- ggtmp[,list(	Total = length(purchase_date)/length(unique(household_code)), 
						CVS = length(purchase_date[cvs==1])/length(unique(household_code)), 
						OtherDrug = length(purchase_date[cvs==0&channel_type=="Drug Store"])/length(unique(household_code)), 
						OtherChannel = length(purchase_date[cvs==0 & channel_type != "Drug Store"])/length(unique(household_code))), 
				by = list(cvs_in2, wgr_in2, week)]
ggtmp	<- melt(ggtmp, id.vars = c("cvs_in2", "wgr_in2", "week"))		
ggtmp$wgr_in2	<- factor(ggtmp$wgr_in2, levels = c(1, 0), labels = c("Less than 2 mi.", "Greater than 2 mi."))
ggtmp$cvs_in2	<- factor(ggtmp$cvs_in2, levels = c(1, 0), labels = c("Less than 2 mi.", "Greater than 2 mi."))

pdf(paste(plot.wd, "/fg_trips_distwgr.pdf", sep=""), width = ww, height = ww*ar)	
ggplot(subset(ggtmp, !is.na(cvs_in2) & !is.na(wgr_in2)), aes(week, value, linetype = cvs_in2, color = wgr_in2)) + geom_line() + 
			geom_vline(xintercept = as.numeric(event.date), col = "red") + 
			facet_wrap(~variable, scales = "free") + 
			guides(linetype = guide_legend(title = "Distance to CVS"), color = guide_legend(title = "Distance to Walgreen")) + 
			theme(legend.position = "bottom") + 
			labs(y = "Trips per person", title = "Shopping trips by consumer groups")
dev.off()

# ------------------------------------------------------------------- #
# Compare shopping trips by customer groups -- heavy vs light smokers # 
ggtmp	<- data.table(subset(trips, !week %in% endweek & !is.na(cvs_in5)))
ggtmp	<- ggtmp[,list(	Total = length(purchase_date)/length(unique(household_code)), 
						CVS = length(purchase_date[cvs==1])/length(unique(household_code)), 
						OtherDrug = length(purchase_date[cvs==0&channel_type=="Drug Store"])/length(unique(household_code)), 
						OtherChannel = length(purchase_date[cvs==0 & channel_type != "Drug Store"])/length(unique(household_code))), 
				by = list(cvs_in2, heavy, week)]
ggtmp	<- melt(ggtmp, id.vars = c("cvs_in2", "heavy", "week"))		
ggtmp$heavy	<- factor(ggtmp$heavy, levels = c(1, 0), labels = c("Heavy", "Light"))
ggtmp$cvs_in2	<- factor(ggtmp$cvs_in2, levels = c(1, 0), labels = c("Less than 2 mi.", "Greater than 2 mi."))

pdf(paste(plot.wd, "/fg_trips_heavy.pdf", sep=""), width = ww, height = ww*ar)	
ggplot(subset(ggtmp, !is.na(cvs_in2)), aes(week, value, linetype = cvs_in2, color = heavy)) + geom_line() + 
			geom_vline(xintercept = as.numeric(event.date), col = "red") + 
			facet_wrap(~variable, scales = "free") + 
			guides(linetype = guide_legend(title = "Distance to CVS"), color = guide_legend(title = "Smokers")) + 
			theme(legend.position = "bottom") + 
			labs(y = "Trips per person", title = "Shopping trips by consumer groups")
dev.off()

# ---------------------------------------------------------- #
# Compare dollar spending by customer groups -- Close to CVS #
ggtmp	<- data.table(subset(trips, !week %in% endweek & !is.na(cvs_in5)))
ggtmp	<- ggtmp[,list(	Total = sum(total_spent)/length(unique(household_code)), 
						CVS = sum(total_spent[cvs==1])/length(unique(household_code)), 
						OtherDrug = sum(total_spent[cvs==0&channel_type=="Drug Store"])/length(unique(household_code)), 
						OtherChannel = sum(total_spent[cvs==0 & channel_type != "Drug Store"])/length(unique(household_code))), 
				by = list(cvs_in2, week)]
ggtmp	<- melt(ggtmp, id.vars = c("cvs_in2", "week"))		
ggtmp$cvs_in2	<- factor(ggtmp$cvs_in2, levels = c(1, 0), labels = c("Less than 2 mi.", "Greater than 2 mi."))

pdf(paste(plot.wd, "/fg_dol_dist.pdf", sep=""), width = ww, height = ww*ar)	
ggplot(ggtmp, aes(week, value, linetype = cvs_in2)) + geom_line() + 
			geom_vline(xintercept = as.numeric(event.date), col = "red") + 
			facet_wrap(~variable, scales = "free") + 
			guides(linetype = guide_legend(title = "Distance to CVS")) + 
			theme(legend.position = "bottom") + 
			labs(y = "Spending per person", title = "Spending by consumer groups")
dev.off()			

# -------------------------------------------------------------------- #
# Compare dollar spending by customer groups -- heavy vs light smokers #
ggtmp	<- data.table(subset(trips, !week %in% endweek & !is.na(cvs_in5)))
ggtmp	<- ggtmp[,list(	Total = sum(total_spent)/length(unique(household_code)), 
						CVS = sum(total_spent[cvs==1])/length(unique(household_code)), 
						OtherDrug = sum(total_spent[cvs==0&channel_type=="Drug Store"])/length(unique(household_code)), 
						OtherChannel = sum(total_spent[cvs==0 & channel_type != "Drug Store"])/length(unique(household_code))), 
				by = list(cvs_in2, heavy, week)]
ggtmp	<- melt(ggtmp, id.vars = c("cvs_in2", "heavy", "week"))		
ggtmp$heavy	<- factor(ggtmp$heavy, levels = c(1, 0), labels = c("Heavy", "Light"))
ggtmp$cvs_in2	<- factor(ggtmp$cvs_in2, levels = c(1, 0), labels = c("Less than 2 mi.", "Greater than 2 mi."))

pdf(paste(plot.wd, "/fg_dol_heavy.pdf", sep=""), width = ww, height = ww*ar)	
ggplot(subset(ggtmp, !is.na(cvs_in2)), aes(week, value, linetype = cvs_in2, color = heavy)) + geom_line() + 
			geom_vline(xintercept = as.numeric(event.date), col = "red") + 
			facet_wrap(~variable, scales = "free") + 
			guides(linetype = guide_legend(title = "Distance to CVS"), color = guide_legend(title = "Smokers")) + 
			theme(legend.position = "bottom") + 
			labs(y = "Spending per person", title = "Spending by consumer groups")
dev.off() 

# --------------------------------------------------------#
# Fraction of cigarret spending out of the total spending #
tmp		<- data.table(purchases)
tmp		<- tmp[,list(total_price_paid = sum(total_price_paid)), by = list(trip_code_uc)]
ggtmp	<- merge(trips, tmp, by = c("trip_code_uc"), all = T)
ggtmp$total_price_paid	<- ifelse(is.na(ggtmp$total_price_paid), 0, ggtmp$total_price_paid)

ggtmp1	<- data.table(ggtmp)
ggtmp1	<- ggtmp1[,list(Total 	= sum(total_price_paid)/sum(total_spent), 
						CVS 	= sum(total_price_paid*1*(cvs==1))/sum(total_spent*1*(cvs==1)), 
						OtherDrug  = sum(total_price_paid*1*(cvs==0&channel_type=="Drug Store"))/sum(total_spent*1*(cvs==0&channel_type=="Drug Store")),
						OtherChannel  = sum(total_price_paid*1*(cvs==0&channel_type!="Drug Store"))/sum(total_spent*1*(cvs==0&channel_type!="Drug Store")) 
						), by = list(cvs_in2, heavy,household_code, week)]
ggtmp2	<- melt(ggtmp1, id.vars = c("cvs_in2", "household_code", "week"))
ggplot(ggtmp2, aes(value)) + geom_histogram(aes(y=..density..), binwidth = .01) + 
		facet_wrap(~ variable, scales = "free") + xlim(c(0, .5))
ggplot(subset(ggtmp2, value > 0), aes(value)) + geom_histogram(aes(y=..density..), binwidth = .01) + 
		facet_wrap(~ variable, scales = "free") + 
		labs(title = "Histogram of fraction of cigarette spending conditional on cigarette purchases")

ggtmp2	<- ggtmp1[,list(Total = mean(Total, na.rm=T), CVS = mean(CVS, na.rm=T), 
						OtherDrug = mean(OtherDrug, na.rm=T), OtherChannel = mean(OtherChannel, na.rm=T)), 
					by = list(cvs_in2, week)]						
ggtmp2	<- melt(ggtmp2, id.vars = c("week", "cvs_in2"))
ggtmp2$cvs_in2	<- factor(ggtmp2$cvs_in2, levels = c(1, 0), labels = c("Less than 2 mi.", "Greater than 2 mi."))

ggplot(ggtmp2, aes(week, value, linetype = cvs_in2)) + geom_line() + 
			geom_vline(xintercept = as.numeric(event.date), col = "red") + 
			facet_wrap(~variable, scales = "free") + 
			guides(linetype = guide_legend(title = "Distance to CVS")) + 
			theme(legend.position = "bottom") + 
			labs(y = "Fraction of cigarette spending", title = "Fraction of cigarette spending")

ggtmp2	<- ggtmp1[,list(Total = mean(Total, na.rm=T), CVS = mean(CVS, na.rm=T), 
						OtherDrug = mean(OtherDrug, na.rm=T), OtherChannel = mean(OtherChannel, na.rm=T)), 
					by = list(cvs_in2, heavy,week)]						
ggtmp2	<- melt(ggtmp2, id.vars = c("week", "cvs_in2", "heavy"))
ggtmp2$cvs_in2	<- factor(ggtmp2$cvs_in2, levels = c(1, 0), labels = c("Less than 2 mi.", "Greater than 2 mi."))
ggtmp2$heavy	<- factor(ggtmp2$heavy, levels = c(1, 0), labels = c("Heavy", "Light"))

ggplot(ggtmp2, aes(week, value, linetype = cvs_in2, color = heavy)) + geom_line() + 
			geom_vline(xintercept = as.numeric(event.date), col = "red") + 
			facet_wrap(~variable, scales = "free") + 
			guides(linetype = guide_legend(title = "Distance to CVS")) + 
			theme(legend.position = "bottom") + 
			labs(y = "Fraction of cigarette spending", title = "Fraction of cigarette spending")
