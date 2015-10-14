library(data.table)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(gridExtra)

setwd("~/Documents/Research/Tobacco/processed_data")
mycounty 	<- read.csv("county_treatment.csv", header = T)
load("sales_ma.rdata")

# Set plotting parameters
plot.wd	<- "~/Desktop"
ww		<- 12
ar		<- .66

# Convert variables of date format
sales_MA$week_end	<- as.Date(as.character(sales_MA$week_end), format = "%Y-%m-%d")
mycounty	<- subset(mycounty, state == "MA")
mycounty$effect_date	<- as.Date(as.character(mycounty$effect_date), format = "%m/%d/%y")
mycounty$control_date	<- as.Date(as.character(mycounty$control_date), format = "%m/%d/%y")

# Market-level aggregation
pack.size	<- 20
tmp			<- sort(table(sales_MA$brand_descr), decreasing = T)
top.brd		<- names(tmp)[1]

# Organize market-level data
table(sales_MA$channel_code)/nrow(sales_MA)				# Examine the frequence of channel 
tmp			<- subset(mycounty, treatment == 1)
mncp 		<- tmp$municipality							# Unique cases
event_date 	<- tmp$effect_date							# Event date
channel.lab	<- c("All", "Drug", "DFM", "Convenience")		

mkt_sale	<- data.frame(NULL)
selcol		<- c("municipality", "effect_date", "fips_county_descr", "treatment", "control_date")
for(i in 1:length(mncp)){
	for(j in 1:length(channel.lab)){
		tmp		<- as.character(mycounty[mycounty$municipality == mncp[i], "fips_county_descr"])
		tmpdat	<- subset(sales_MA, fips_county_descr %in% tmp)
		if(j == 2){
			tmpdat	<- subset(tmpdat, channel_code == "D")
		}else if(j == 3){
			tmpdat	<- subset(tmpdat, channel_code %in% c("D", "F", "M"))
		}else if(j == 4){
			tmpdat	<- subset(tmpdat, channel_code == "C")
		}
		tmpdat	<- data.table(tmpdat)
		tmpdat	<- tmpdat[,list(quantity = sum(units*size)/pack.size, 
								price = sum(price * units )/sum(units * size) *pack.size, 
								top.share = sum(units*size*1*(brand_descr == top.brd))/sum(units*size) ),
							by = list(fips_county_descr, week_end)]
		tmpdat	<- merge(data.frame(tmpdat), mycounty[mycounty$municipality == mncp[i], selcol], by = "fips_county_descr", all.x = T)					
		tmpdat$channel	<- channel.lab[j]
		mkt_sale	<- rbind(mkt_sale, tmpdat)
	}
}
mkt_sale$treatment 	<- factor(mkt_sale$treatment, levels = c(1, 0))
mkt_sale$channel	<- factor(mkt_sale$channel, levels = channel.lab)

selcol	<- c("quantity", "price", "top.share")
sellab	<- c("Quantity (packs)", "Price ($/pack)", "Marlboro share")
my.wind	<-	365										# Restrict to 1 year window around the event
my.col	<- brewer.pal(4,"Set1")
pdf(paste(plot.wd, "/graph_tabaco_rms_MA.pdf", sep=""), width = 10.5, height = 12) 
for(i in 1:length(mncp)){
	plots		<- list(NULL)
	for(j in 1:length(selcol)){
		ggtmp	<- subset(mkt_sale, municipality == mncp[i] & abs(week_end - effect_date) <= my.wind)
		ggtmp$fips_county_descr	<- factor(as.character(ggtmp$fips_county_descr), levels = mycounty[mycounty$municipality == mncp[i], "fips_county_descr"])
		tmpn	<- length(unique(ggtmp$fips_county_descr))
		plots[[j]]	<- ggplot(ggtmp,	aes_string(x = "week_end", y = selcol[j])) + 
			geom_line(aes(col = fips_county_descr, linetype = treatment)) + 
			geom_vline(xintercept = as.numeric(event_date[i]), size = .25) + 
			# geom_vline(aes(xintercept = as.numeric(control_date), col = fips_county_descr)) + 
			facet_grid(. ~ channel) + 
			scale_color_manual(name = "County", values = my.col[1:tmpn]) + 
			labs(x = "Week", y = sellab[j]) + 
				# title = paste(sellab[j]," for the policy case in ", mncp[i], " (", event_date[i], ")", sep="")) + 
			theme(axis.text.x = element_text(angle = 30))	
	}
	tmp 	<- ggplot_gtable(ggplot_build(plots[[1]]))
	leg 	<- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
	legend 	<- tmp$grobs[[leg]]
	print(grid.arrange(arrangeGrob(plots[[1]] + theme(legend.position="none"), 
							 plots[[2]] + theme(legend.position="none"), 
							 plots[[3]] + theme(legend.position="none"), 
							 nrow = 3) ,
							 legend, 
							widths=unit.c(unit(1, "npc") - sum(legend$width), sum(legend$width) ), nrow = 1, 
							main = paste("Policy case in ", mncp[i], " (", event_date[i], ")", sep=""))
			)
}
dev.off()

#------------------------------ #
# Plot price series for top UPC #
num.upc	<- 5
top.upc	<- data.table(sales_MA)
tmp.upc	<- top.upc[,list(quant = sum(units*size)), by = list(upc, brand_descr)]
ord		<- order(tmp.upc$quant, decreasing = T)
tmp.upc	<- tmp.upc[ord,]
tmp.upc	<- tmp.upc[1:num.upc,]

price.dat	<- data.frame(NULL)
selcol		<- c("municipality", "effect_date", "fips_county_descr", "treatment", "control_date")
for(i in 1:length(mncp)){
	for(j in 1:length(channel.lab)){
		tmp		<- as.character(mycounty[mycounty$municipality == mncp[i], "fips_county_descr"])
		tmpdat	<- subset(sales_MA, fips_county_descr %in% tmp & upc %in% tmp.upc$upc)
		if(j == 2){
			tmpdat	<- subset(tmpdat, channel_code == "D")
		}else if(j == 3){
			tmpdat	<- subset(tmpdat, channel_code %in% c("D", "F", "M"))
		}else if(j == 4){
			tmpdat	<- subset(tmpdat, channel_code == "C")
		}
		tmpdat	<- data.table(tmpdat)
		tmpdat	<- tmpdat[,list(price = mean(price) ),
							by = list(fips_county_descr, week_end, upc)]
		tmpdat	<- merge(data.frame(tmpdat), mycounty[mycounty$municipality == mncp[i], selcol], by = "fips_county_descr", all.x = T)					
		tmpdat$channel	<- channel.lab[j]
		price.dat	<- rbind(price.dat, tmpdat)
	}
}
price.dat$treatment 	<- factor(price.dat$treatment, levels = c(1, 0))
price.dat$channel	<- factor(price.dat$channel, levels = channel.lab)

pdf(paste(plot.wd, "/graph_tabaco_rms_MA_upcprice.pdf", sep=""), width = 10.5, height = 12) 
for(i in 1:length(mncp)){
	plots		<- list(NULL)
	ggtmp	<- subset(price.dat, municipality == mncp[i] & abs(week_end - effect_date) <= my.wind)
	ggtmp$fips_county_descr	<- factor(as.character(ggtmp$fips_county_descr), levels = mycounty[mycounty$municipality == mncp[i], "fips_county_descr"])
	tmpn	<- length(unique(ggtmp$fips_county_descr))
	print(ggplot(ggtmp, aes(week_end, price)) + 
		geom_line(aes(col = fips_county_descr, linetype = treatment)) + 
		geom_vline(xintercept = as.numeric(event_date[i]), size = .25) + 
		facet_grid(upc ~ channel, scales = "free_y") + 
		scale_color_manual(name = "County", values = my.col[1:tmpn]) + 
		labs(x = "Week", title = paste("policy change in ", mncp[i], " (", event_date[i], ")", sep="")) + 
		theme(axis.text.x = element_text(angle = 30))	
		)
}
dev.off()
