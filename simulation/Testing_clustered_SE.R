# Testing clusted standard error

# Compare the clustered SE with R and STATA
# Stata output and data see: http://www.kellogg.northwestern.edu/faculty/petersen/htm/papers/se/test_data.htm


library(plm)
library(lmtest)
options(digits = 8)  # for more exact comparison with Stata's output

# get data and load as pdata.frame
url		<- "http://www.kellogg.northwestern.edu/faculty/petersen/htm/papers/se/test_data.txt"
p.df 	<- read.table(url)
names(p.df) <- c("firmid", "year", "x", "y")
p.df 	<- pdata.frame(p.df, index = c("firmid", "year"), drop.index = F, row.names = T)
head(p.df)

# fit pooled OLS
m1	<- lm(y ~ x, data = p.df)
# fit same model with plm (needed for clustering)
pm1 <- plm(y ~ x, data = p.df, model = "pooling")

coeftest(m1)
coeftest(m1, vcov = vcovHC(m1, type = "HC1"))						# White standard error

# compute Stata like df-adjustment
G <- length(unique(p.df$firmid))
N <- length(p.df$firmid)
dfa <- (G/(G - 1)) * (N - 1)/pm1$df.residual
# display with cluster VCE and df-adjustment
firm_c_vcov <- dfa * vcovHC(pm1, type = "HC0", cluster = "group", adjust = T)
coeftest(pm1, vcov = firm_c_vcov)

# compute Stata like df-adjustment
G <- length(unique(p.df$year))
N <- length(p.df$year)
dfa <- (G/(G - 1)) * (N - 1)/pm1$df.residual
 
# display with cluster VCE and df-adjustment
time_c_vcov <- dfa * vcovHC(pm1, type = "HC0", cluster = "time", adjust = T)
coeftest(pm1, vcov = time_c_vcov)

#====================================================================#
# STATA command 
webuse set http://www.kellogg.northwestern.edu/faculty/petersen/htm/papers/se
webuse test_data, clear
xtset firmid year
reg y x
reg y x, robust
reg y x, cluster(firmid)
reg y x, cluster(year)



