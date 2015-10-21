library(data.table)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(gridExtra)
library(plm)

# Set plotting parameters
plot.wd	<- "~/Desktop"
ww		<- 12
ar		<- .66

setwd("~/Documents/Research/Tobacco/processed_data")
source("../Exercise/outreg function.R")
mycounty 	<- read.csv("county_treatment.csv", header = T)
load("sales_ca.rdata")
sales_CA$week_end	<- as.Date(as.character(sales_CA$week_end), format = "%Y-%m-%d")
sales_CA$treatment 	<- ifelse(sales_CA$fips_county_descr == "SAN FRANCISC", 1, 0)

# Market-level aggregation of quantity
pack.size	<- 20
tmp			<- sort(table(sales_CA$brand_descr), decreasing = T)
top.brd		<- names(tmp)[1]
event.date	<- as.Date(c("2008-10-01", "2010-11-01"), format = "%Y-%m-%d")
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
			geom_vline(xintercept = as.numeric(event.date), size = .25) + 
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
tmp.upc	<- top.upc[,list(quant = sum(units*size)), by = list(upc, upc_descr, brand_descr)]
ord		<- order(tmp.upc$quant, decreasing = T)
tmp.upc	<- tmp.upc[ord,]
tmp.upc	<- tmp.upc[1:num.upc,]

price.data	<- data.frame()
for(i in 1:length(channel.lab)){
	tmp		<- data.table(sales_CA)
	if(i == 2){
		tmp	<- subset(tmp, channel_code != "D")
	}
	tmp	<- tmp[,list(price = mean(price/size)*pack.size, price_vol = sum(units*price)/sum(units*size)*pack.size), 
				by = list(treatment, fips_county_descr, upc, week_end)]
	tmp$channel	<- channel.lab[i]
	price.data	<- rbind(price.data, tmp)
}
price.dat 	<- subset(price.data, upc %in% tmp.upc$upc)
price.dat$treatment 	<- factor(price.dat$treatment, levels = c(1, 0))

pdf(paste(plot.wd, "/graph_tabaco_rms_CA_upcprice.pdf", sep=""), width = 10.5, height = 12) 
print(ggplot(price.dat, aes(week_end, price)) + 
		geom_line(aes(col = fips_county_descr, linetype = treatment)) + 
		geom_vline(xintercept = as.numeric(event.date), size = .25) + 
		facet_grid(upc ~ channel, scales = "free_y") + 
		scale_color_manual(name = "County", values = my.col) + 
		labs(x = "Week", y = sellab[i]) + 
		theme(axis.text.x = element_text(angle = 30))
		)
dev.off()

###############
# Regressions # 
###############
# Category quantity 
mkt_sale	<- mkt_sale[,':='(post1 = 1*(week_end >= event.date[1]), post2 = 1*(week_end >= event.date[2]))]
quant.ls	<- setNames( vector("list", length = length(channel.lab)), channel.lab)

for(i in 1:length(channel.lab)){
	tmpfit	<- plm(quantity ~ treatment + treatment*post1 + treatment*post2, data = subset(mkt_sale, channel == channel.lab[i]), 
					index = c("fips_county_descr", "week_end"), effect = "time", model = "within")
	quant.ls[[i]]	<- coef(summary(tmpfit))
}
quant.est	<- model_outreg(quant.ls, p.given = TRUE, head.name = c("Estimate", "Std. Error", "Pr(>|t|)"))
cat("Treatment effect for quantity:\n"); print(quant.est); cat("\n")

# Price regressions
price.data	<- price.data[,':='(post1 = 1*(week_end >= event.date[1]), post2 = 1*(week_end >= event.date[2]), 
								upc_county = paste(upc, "-", fips_county_descr, sep=""), 
								county_week = paste(fips_county_descr, "-", week_end, sep=""))]

mywindow 	<- 365
price.ls	<- setNames(vector("list", length = 2 * length(channel.lab)), 
						paste(rep(c("08", "10"), each = 2), "-", rep(channel.lab, 2), sep ="") )
for(i in 1:2){
	for(j in 1:length(channel.lab)){
		idx		<- (i-1) * length(channel.lab) + j
		myfml	<- as.formula(paste("price ~ treatment + treatment*post", i, " + factor(week_end)", sep=""))
		tmpfit	<- plm(myfml,data = subset(price.data, abs(week_end - event.date[i]) <= mywindow & channel == channel.lab[j]), 
						index = c("upc", "county_week"), effect = "individual", model = "within")
		b			<- try(coef(summary(tmpfit)))
		if(class(b) != "try-error"){
			sel			<- setdiff(1:nrow(b), grep("factor(week_end)", rownames(b)))
			price.ls[[idx]]	<- b[sel,]
		}else{
			b		<- coef(reg.prc1)
			sel		<- setdiff(1:length(b), grep("week_end", names(b)))
			Vb		<- diag(vcov(reg.prc1))
			se		<- sqrt(Vb[sel])
			price.ls[[idx]]	<- data.frame(Estiamte = b[sel], Std.Error = se)
		}														
	}	
}

price.est	<- model_outreg(price.ls, p.given = FALSE, head.name = c("Estimate", "Std. Error"))
cat("Treatment effect for price:\n"); print(price.est); cat("\n")


