library(data.table)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(gridExtra)

setwd("/Users/chaoqunchen/Documents/Research/Tobacco/processed_data")
load("cigar_hms.rdata")

# Set plotting parameters 
plot.wd	<- "~/Desktop"

# Convert variables of date format
county$enact_date		<- as.Date(as.character(county$enact_date), format = "%m/%d/%y")
county$effect_date		<- as.Date(as.character(county$effect_date), format = "%m/%d/%y")
purchases$purchase_date	<- as.Date(as.character(purchases$purchase_date), format = "%Y-%m-%d")
purchases$year			<- year(purchases$purchase_date)

#######################
# Policy change in SF # 
#######################
# Subset households in CA
my.state	<- "CA"
pan.ca	<- subset(panelists, fips_state_descr == my.state)
pch.ca	<- subset(purchases, household_code %in% pan.ca$household_code)
pch.ca$channel_type	<- as.character(pch.ca$channel_type)
pan.ca$fips_county_descr	<- as.character(pan.ca$fips_county_descr)

# Do we observe enough channel
sort(table(pch.ca$channel_type)/nrow(pch.ca))

# Check the time when households stay
length(unique(pan.ca$household_code))
u.pan.ca	<- unique(pan.ca[,c("household_code", "fips_state_descr", "fips_county_descr")])
dim(u.pan.ca)			# There are people who moved
table(u.pan.ca$fips_county_descr)

# Plot household tenure
tmp	<- data.table(pch.ca)
tmp	<- tmp[,list(start = min(purchase_date), end = max(purchase_date)), by = list(household_code)]
tmp	<- merge(tmp, u.pan.ca, by = "household_code")
tmp	<- tmp[,treatment := 1 * (fips_county_descr == "SAN FRANCISC")]
ggplot(tmp, aes(start, end, col = factor(treatment))) + geom_point(position = "jitter")

# Event in CA
sel		<- county$municipality == "San Francisco1" & county$treatment == 1
county[sel,]
my.treat	<- county[sel,"fips_county_descr"]
event.date	<- county[sel,"effect_date"]
my.window	<- 365			# The examine window period arond the event

# Plot monthly demand 
ggtmp	<- merge(pch.ca, pan.ca[,c("household_code","panel_year","fips_county_descr")], by.x = c("household_code", "year"), by.y = c("household_code", "panel_year"), all.x = T)
ggtmp	<- subset(ggtmp, abs(purchase_date - event.date) <= my.window )
ggtmp	<- merge(ggtmp, prod[,c("upcv","multi","size1_amount")], by = "upcv", all.x = T)
dim(ggtmp)
ggtmp	<- data.table(ggtmp)
ggtmp	<- ggtmp[, ':='(quantity=units*multi*size1_amount, month = month(purchase_date))]
ggtmp1	<- ggtmp[,list(quantity = sum(quantity), nhh = length(unique(household_code))), by = list(fips_county_descr, year, month)]
ggtmp1$ymonth	<- as.Date(paste(ggtmp1$year, ggtmp1$month, "01", sep = "-"), format = "%Y-%m-%d")
ggtmp1	<- subset(ggtmp1, !is.na(fips_county_descr))
ggtmp1$treatment	<- factor(1*(ggtmp1$fips_county_descr == "SAN FRANCISC"), levels = c(1,0))
ggtmp1$fips_county_descr	<- factor(ggtmp1$fips_county_descr, 
								levels = county[county$municipality == "San Francisco1","fips_county_descr"])

my.col	<- brewer.pal(length(levels(ggtmp1$fips_county_descr)),"Set1")

pdf(paste(plot.wd, "/graph_hms_ca.pdf", sep=""), width = 6.5, height = 6.5 * .6)
ggplot(ggtmp1, aes(ymonth, quantity, col = fips_county_descr)) + 
		geom_point(aes(size = nhh, alpha = .8)) + 
		geom_line(aes(linetype = treatment)) + 
		geom_vline(xintercept = as.numeric(event.date), size = .25) + 
		scale_color_manual(name = "County", values = my.col) + 
		guides(alpha = FALSE, size = guide_legend(title = "Num.HH")) + 
		labs(x = "Month", 
		title = "Policy change in San Francisco")
dev.off()		

#######################
# Policy change in MA #
#######################		
# Subset households in MA
my.state2	<- "MA"
pan.ma	<- subset(panelists, fips_state_descr == my.state2)
pch.ma	<- subset(purchases, household_code %in% pan.ma$household_code)
pch.ma$channel_type	<- as.character(pch.ma$channel_type)
pan.ma$fips_county_descr	<- as.character(pan.ma$fips_county_descr)

# Do we observe enough channel
sort(table(pch.ma$channel_type)/nrow(pch.ma))

# Check the time when households stay
length(unique(pan.ma$household_code))
u.pan.ma	<- unique(pan.ma[,c("household_code", "fips_state_descr", "fips_county_descr")])
dim(u.pan.ma)			# There are people who moved
table(u.pan.ma$fips_county_descr)

# Merge data
pch.ma	<- merge(pch.ma, pan.ma[,c("household_code","panel_year","fips_county_descr")], 
				by.x = c("household_code", "year"), by.y = c("household_code", "panel_year"), all.x = T)
pch.ma	<- merge(pch.ma, prod[,c("upcv","multi","size1_amount")], by = "upcv", all.x = T)

# Events in MA
sel		<- county$state == my.state2 & county$treatment == 1
county[sel,]
mncp	<- county[sel,]
my.window	<- 180

# Construct experiment-like data 
ma.plc	<- data.frame()
for(i in 1:nrow(mncp)){
	sel	<- county$municipality == mncp[i,"municipality"]
	tmp	<- subset(pch.ma, fips_county_descr %in% county[sel,"fips_county_descr"])	# Foucs on the relevant counties
	tmp	<- subset(tmp, abs(purchase_date - mncp[i,"effect_date"]) <= my.window)		# Focus on the window around the event date
	tmp	<- cbind(municipality = mncp[i,"municipality"], effect_date = mncp[i,"effect_date"],tmp)
	ma.plc	<- rbind(ma.plc, tmp)
}
ma.plc	<- merge(ma.plc, county[,c("municipality", "fips_county_descr","treatment", "control_date")], 
					by = c("municipality", "fips_county_descr"), all.x = T)

ggtmp	<- data.table(ma.plc)					
ggtmp	<- ggtmp[, ':='(quantity=units*multi*size1_amount, month = month(purchase_date))]
ggtmp	<- ggtmp[,list(quantity = sum(quantity), nhh = length(unique(household_code))), 
					by = list(municipality, treatment, fips_county_descr, year, month)]
ggtmp$ymonth	<- as.Date(paste(ggtmp$year, ggtmp$month, "01", sep = "-"), format = "%Y-%m-%d")
ggtmp$treatment	<- factor(ggtmp$treatment, levels = c(1, 0))
summary(ggtmp$nhh)

my.col	<- brewer.pal(4,"Set1")
my.size	<- c(5, 10, 15, 20)
plots	<- list(NULL)
for(i in 1:nrow(mncp)){
	ggtmp1		<- subset(ggtmp, municipality == mncp[i,"municipality"])
	sel			<- county$municipality == mncp[i,"municipality"]
	ggtmp1$fips_county_descr	<- factor(as.character(ggtmp1$fips_county_descr), 
										levels = county[sel, "fips_county_descr"])
	tmpn	<- length(unique(ggtmp1$fips_county_descr))
	plots[[i]]	<- ggplot(ggtmp1, aes(ymonth, quantity, col = fips_county_descr)) + 
					geom_point(aes(size = nhh, alpha = .8)) + 
					geom_line(aes(linetype = treatment)) + 
					geom_vline(xintercept = as.numeric(mncp[i, "effect_date"]), size = .25) + 
					scale_color_manual(name = "County", values = my.col[1:tmpn]) + 
					scale_size_area(breaks = my.size) +  
					guides(alpha = FALSE, size = guide_legend(title = "Num.HH")) + 
					labs(x = "Month", 
					title = paste("Policy change in ", mncp[i,"municipality"], "(", mncp[i,"effect_date"], ")", sep=""))
	print(plots[[i]])				
}

pdf(paste(plot.wd, "/graph_hms_ma.pdf", sep=""), width = 6.5, height = 6.5 * .6)
for(i in 1:length(plots)){
	print(plots[[i]])
}
dev.off()
