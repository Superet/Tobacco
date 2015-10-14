library(data.table)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(gridExtra)

# Set plotting parameters
plot.wd	<- "~/Desktop"
ww		<- 12
ar		<- .66

setwd("~/Documents/Research/Tobacco/processed_data")
mycounty 	<- read.csv("county_treatment.csv", header = T)
load("sales_ca.rdata")
sales_CA$week_end	<- as.Date(as.character(sales_CA$week_end), format = "%Y-%m-%d")

# Market-level aggregation
pack.size	<- 20
tmp			<- sort(table(sales_CA$brand_descr), decreasing = T)
top.brd		<- names(tmp)[1]
treat.time	<- as.Date(c("2008-10-01", "2010-11-01"), format = "%Y-%m-%d")
channel.lab	<- c("All", "NonDrug")

mkt_sale	<- data.frame()
for(i in 1:length(channel.lab)){
	tmp		<- data.table(sales_CA)
	if(i == 2){
		tmp	<- subset(tmp, channel_code != "D")
	}
	tmp		<- tmp[,list(	quantity = sum(units*size)/pack.size, 
							price = sum(price * units )/sum(units * size) *pack.size, 
							top.share = sum(units*size*1*(brand_descr == top.brd))/sum(units*size) ), 
							by = list(treatment, fips_county_descr, week_end)]
	tmp$channel	<- channel.lab[i]
	mkt_sale	<- rbind(mkt_sale, tmp)
}
mkt_sale$treatment 	<- factor(mkt_sale$treatment, levels = c(1, 0))

selcol	<- c("quantity", "price", "top.share")
sellab	<- c("Quantity (packs)", "Price ($/pack)", "Marlboro share")
tmpn	<- length(unique(mkt_sale$fips_county_descr))
my.col	<- brewer.pal(tmpn,"Set1")
mkt_sale$fips_county_descr	<- factor(mkt_sale$fips_county_descr, 
								levels = mycounty[mycounty$municipality == "San Francisco1","fips_county_descr"])

plots	<- list(NULL)
for(i in 1:length(selcol)){
	quartz()
	plots[[i]]	<- ggplot(mkt_sale, aes_string(x = "week_end", y = selcol[i])) + 
			geom_line(aes(col = fips_county_descr, linetype = treatment)) + 
			geom_vline(xintercept = as.numeric(treat.time), size = .25) + 
			facet_grid(. ~ channel) + 
			scale_color_manual(name = "County", values = my.col) + 
			labs(x = "Week", y = sellab[i]) + 
			theme(axis.text.x = element_text(angle = 30))
}
tmp 	<- ggplot_gtable(ggplot_build(plots[[1]]))
leg 	<- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
legend 	<- tmp$grobs[[leg]]

pdf(paste(plot.wd, "/graph_tabaco_rms_CA.pdf", sep=""), width = 10.5, height = 12) 
print(grid.arrange(arrangeGrob(plots[[1]] + theme(legend.position="none"), 
						 plots[[2]] + theme(legend.position="none"), 
						 plots[[3]] + theme(legend.position="none"), 
						 nrow = 3) ,
						 legend, 
						widths=unit.c(unit(1, "npc") - sum(legend$width), sum(legend$width) ), nrow = 1, 
						main = "Policy change in San Francisco")
		)
dev.off()

#------------------------------ #
# Plot price series for top UPC #
num.upc	<- 5
top.upc	<- data.table(sales_CA)
tmp.upc	<- top.upc[,list(quant = sum(units*size)), by = list(upc, brand_descr)]
ord		<- order(tmp.upc$quant, decreasing = T)
tmp.upc	<- tmp.upc[ord,]
tmp.upc	<- tmp.upc[1:num.upc,]

price.dat	<- data.frame()
for(i in 1:length(channel.lab)){
	tmp		<- data.table(subset(sales_CA, upc %in% tmp.upc$upc))
	if(i == 2){
		tmp	<- subset(tmp, channel_code != "D")
	}
	tmp		<- tmp[,list(	price = mean(price)), 
							by = list(treatment, fips_county_descr, upc, week_end)]
	tmp$channel	<- channel.lab[i]
	price.dat	<- rbind(price.dat, tmp)
}
price.dat$treatment 	<- factor(price.dat$treatment, levels = c(1, 0))

pdf(paste(plot.wd, "/graph_tabaco_rms_CA_upcprice.pdf", sep=""), width = 10.5, height = 12) 
print(ggplot(price.dat, aes(week_end, price)) + 
		geom_line(aes(col = fips_county_descr, linetype = treatment)) + 
		geom_vline(xintercept = as.numeric(treat.time), size = .25) + 
		facet_grid(upc ~ channel, scales = "free_y") + 
		scale_color_manual(name = "County", values = my.col) + 
		labs(x = "Week", y = sellab[i]) + 
		theme(axis.text.x = element_text(angle = 30))
		)
dev.off()
