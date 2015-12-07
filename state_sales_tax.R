library(xlsx)

sel.state	<- c("Massachusetts", "New Hampshire", "Rhode Island", "Connecticut")

sales_tax	<- data.frame()
for(i in 2006:2013){
	dat1	<- read.xlsx("~/Documents/Research/Tobacco/Literature/Report/Tax/State Sales, Gasoline, Cigarette and Alcohol Taxes, 2000-2014.xlsx", 
						sheetName = as.character(i))
	names(dat1)		<- c("state", "sales_tax", "gas_tax", "cigar_tax", "spirit_tax", "wine_tax", "beer_tax")
	sel		<- grep("Alabama", dat1$state)
	dat1	<- dat1[-(1:sel),]
	dat1	<- dat1[1:50,]
	dat1$state	<- gsub("^\\s+|\\s+$", "", dat1$state)
	sel		<- dat1$state %in% sel.state
	
	# Correct zero tax rate for New Hampshire
	tmp		<- dat1[sel,c("state", "sales_tax")]
	tmp$sales_tax	<- as.numeric(as.character(tmp$sales_tax) )
	tmp[is.na(tmp$sales_tax),"sales_tax"]	<- 0
	
	# Keep the tax rate in the percentage unit
	if(round(tmp[1,"sales_tax"]) == 0 ){
		tmp$sales_tax	<- tmp$sales_tax * 100
	}
	
	# Stack data by year
	sales_tax	<- rbind(sales_tax, cbind(tmp, year = i))
	print(i)
}

# Check if sales tax vary by year
by(sales_tax$sales_tax, sales_tax$state, function(x) all(x == mean(x)))
by(sales_tax$sales_tax, sales_tax$state, unique)

write.csv(sales_tax, file = "~/Documents/Research/Tobacco/processed_data/supplements/sales_tax.csv")
