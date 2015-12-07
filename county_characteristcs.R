setwd("~/Documents/Research/Tobacco/processed_data/supplements")

data	<- read.csv("county_population.csv")

# Get the state and county name, which are matched to the Nielsen data 
tmp		<- do.call(rbind, strsplit(as.character(data$Geography), ","))
cnty	<- gsub(" County", "",tmp[,1])
cnty	<- toupper(cnty)
tmp1	<- setNames(c("MA", "CT", "NH", "RI"), 
			c(" Massachusetts", " Connecticut", " New Hampshire", " Rhode Island"))
state	<- tmp1[tmp[,2]]
pop_data<- data.frame(state = state, county = cnty, pop = data$Total)

# Export
write.csv(pop_data, file = "county_charcs.csv", row.names = FALSE)