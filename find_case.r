# Among all the policy changes in MA, we select a few cases where it is possible to detect any changes from the county level. 

library(ggplot2)
library(xlsx)
library(reshape2)
library(data.table)
library(maps)
library(zipcode)

setwd("~/Documents/Research/Tobacco/processed_data")
policy	<- read.xlsx("MA_policy.xlsx", sheetIndex = 1)
names(policy)	<- tolower(names(policy))
plot.wd	<- "~/Desktop"

############################
# Combine and select cases #
############################
# Too see if we can combine some manicipality cases within the same county together
# We plot the time line of policy change
ggtmp	<- policy
ggtmp$pop_prop	<- with(ggtmp, pop/county_pop)
ggplot(ggtmp, aes(eff_date, pop_prop, col = county)) + geom_point(aes(size = retail_aff)) + 
	geom_text(aes(label = municipality), vjust = 1.5, size = 3) 

# Combine municipalites
comb.plc	<- data.table(policy)
setkeyv(comb.plc, c("county", "eff_date"))
comb.plc[,list(county, municipality, eff_date, retail_aff)]
comb.plc	<- comb.plc[, firstm:= 1*(eff_date == min(eff_date)), by = list(county)]

comb.window 		<- 31					# Group the municipalities that have policy changes within 30 days
comb.plc$case_id	<- 1
case.id	<- 0
focal.date	<- NA
for(i in 1:nrow(policy)){
	this.date	<- comb.plc[i,eff_date]
	if(comb.plc[i,firstm] == 1){
		focal.date 	<- NA
		case.id 	<- case.id + 1
	} 
	if(is.na(focal.date) ){
		comb.plc	<- comb.plc[i, case_id:= case.id]
		focal.date	<- this.date	
	}else{
		if(this.date- focal.date <= comb.window){
			comb.plc	<- comb.plc[i, case_id:= case.id]	
		}else{
			case.id 	<- case.id + 1
			focal.date	<- this.date
			comb.plc	<- comb.plc[i, case_id:= case.id]
		}
	}
}
comb.plc[,list(case_id, county, municipality, eff_date, retail_aff)]

# Compute the cumulative numbers
comb.plc	<- comb.plc[,list(municipality = paste(municipality, collapse= ","), retail_aff = sum(retail_aff), 
							eff_date = min(eff_date), eff_date2 = max(eff_date), pop = sum(pop), county_pop = unique(county_pop)), 
						by = list(county, case_id)]	
comb.plc	<- comb.plc[, pop_prop:= pop/county_pop]	
comb.plc[,list(case_id, county, municipality, eff_date, retail_aff)]
					
pdf(paste(plot.wd, "/graph_pop.pdf",sep=""), width = 7.5, height = 7.5 *.66)					
ggplot(comb.plc, aes(eff_date, pop_prop, col = county)) + geom_point(aes(size = retail_aff)) + 
	geom_text(aes(label = municipality), vjust = 1.5, size = 3) + 
	labs(x = "Effect date", y = "Ratio of municipality pop. to county pop.")
ggplot(comb.plc[pop_prop>= .2,], aes(eff_date, pop_prop, col = county)) + geom_point(aes(size = retail_aff)) + 
	geom_text(aes(label = municipality), vjust = 1.5, size = 3) + 
	labs(x = "Effect date", y = "Ratio of municipality pop. to county pop.")
dev.off()

# Select the cases that satisfy: 
# 1. Consist of more than 20% county population; 
# 2. The number of retailers affected is greater than 10. 
# 3. No major policy change in the same county around 6-month window. 
# 4. The effect date is prior to 2013-06
sub.plc	<- comb.plc[pop_prop >= .2 & retail_aff >= 10 & eff_date <= as.Date("2013-06-01", format = "%Y-%m-%d"),]
setkeyv(sub.plc, c("eff_date"))
sub.plc

# Double check two things: no policy changes in the same county within 6 months. 
# Plot the locations
data(zipcode)
tmp	<- unlist(strsplit(sub.plc[,municipality], ","))
tmp	<- subset(zipcode, state == "MA" & city %in% tmp)
tmp	<- data.table(tmp)
tmp	<- unique(tmp, by = "city")

pdf(paste(plot.wd, "/graph_map.pdf", sep=""), width = 7.5, height = 7.5 *.66)
map('county', 'massachusetts') 
text(tmp$longitude, tmp$latitude, tmp$city, cex = .8)
dev.off()

###############################
# Construct treatment-control #
###############################
my.window 	<- 182
sub.plc	<- data.frame(sub.plc)
sub.plc$ctr_county	<- NA
county	<- data.table(policy)
county	<- county[,list(first = min(eff_date)), by = list(county)]
for(i in 1:nrow(sub.plc)){
	tmp	<- subset(county, first >= sub.plc[i,"eff_date"] + my.window)
	sub.plc[i,"ctr_county"]	<- paste(as.character(tmp$county), collapse = ",")
}
sub.plc	<- sub.plc[,setdiff(names(sub.plc), c("case_id", "pop_prop"))]

write.csv(sub.plc, file = "MA_policy_sub.csv", row.names = FALSE)
