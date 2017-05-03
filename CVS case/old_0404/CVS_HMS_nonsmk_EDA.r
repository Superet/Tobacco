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

panelists 	<- read.csv("tob_CVS_nonsmk_pan.csv", header = T)
trips		<- read.csv("tob_CVS_nonsmk_trips.csv", header = T)

#################
# Organize data # 
#################
# Add week
event.date	<- as.Date("2014-09-01", format = "%Y-%m-%d")
cvs.ret	<- 4914		# retailer_code for CVS
qunit	<- 20		# 20 cigaretts per pack 
firstw	<- as.Date("2012-12-31", format = "%Y-%m-%d") 	# The first week in 2013

trips$purchase_date	<- as.Date(as.character(trips$purchase_date), format = "%Y-%m-%d")
trips$week			<- ((as.numeric(trips$purchase_date - firstw)) %/%7 + 1)* 7 + firstw - 1
trips$year			<- year(trips$purchase_date)
trips$cvs			<- ifelse(trips$retailer_code == cvs.ret, 1, 0)					# Mark CVS

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

# Merge household data to purchase and trip data 
trips	<- merge(trips, panelists[,c("household_code", "panel_year", "cvs_in2", "cvs_in5", "cvs_in10", "wgr_in2")], 
						by.x = c("household_code", "year"), by.y = c("household_code", "panel_year"), all.x=T)
						
# Check the tenure of households 
tmp		<- data.table(trips)
tmp		<- tmp[,list(n = length(unique(week)), cvs_in2 = mean(cvs_in2, na.rm=T)), by = list(household_code)]
tmp		<- tmp[cvs_in2 %in% c(0,1),]
ggplot(tmp, aes(n, fill = factor(cvs_in2), alpha = .5)) + geom_histogram(aes(y = ..density..), position = "identity")


###########################
# Trend of traffic to CVS # 
###########################
# Trend between households who live far vs close to CVS
ggtmp	<- data.table(trips)
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
ggtmp	<- data.table(subset(trips, !is.na(cvs_in2)))
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
ggtmp	<- data.table(subset(trips, !is.na(cvs_in2)))
ggtmp	<- ggtmp[,list(	Total = length(purchase_date)/length(unique(household_code)), 
						CVS = length(purchase_date[cvs==1])/length(unique(household_code)), 
						OtherDrug = length(purchase_date[cvs==0&channel_type=="Drug Store"])/length(unique(household_code)), 
						OtherChannel = length(purchase_date[cvs==0 & channel_type != "Drug Store"])/length(unique(household_code))), 
				by = list(wgr_in2, week)]
ggtmp	<- melt(ggtmp, id.vars = c("wgr_in2", "week"))		
ggtmp$wgr_in2	<- factor(ggtmp$wgr_in2, levels = c(1, 0), labels = c("Less than 2 mi.", "Greater than 2 mi."))

pdf(paste(plot.wd, "/fg_trips_distwgr.pdf", sep=""), width = ww, height = ww*ar)	
ggplot(ggtmp, aes(week, value, linetype = wgr_in2)) + geom_line() + 
			geom_vline(xintercept = as.numeric(event.date), col = "red") + 
			facet_wrap(~variable, scales = "free") + 
			guides(linetype = guide_legend(title = "Distance to Walgreen")) + 
			theme(legend.position = "bottom") + 
			labs(y = "Trips per person", title = "Shopping trips by consumer groups")
dev.off()

# ---------------------------------------------------------- #
# Compare dollar spending by customer groups -- Close to CVS #
ggtmp	<- data.table(subset(trips, !is.na(cvs_in2)))
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
