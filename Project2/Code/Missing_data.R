setwd("~/Documents/CU Denver/Fall_2017/Advanced_Data_Analysis/Project_2")
library(sas7bdat)

dat <- read.sas7bdat("vadata2.sas7bdat")

# I would like you to tell me the observed death rate from surgery at each hospital for the most recent 6 month period 
# (IE;sixmonth = 39)
# Also provide some measure of variation around the expected rate of death at each hospital

# Descriptive Statistics
# Note: R recognizes NaN as NA

# Remove individuals with procedure 2
dat[which(dat$proced==2),] <- NA

# People to convert: dat$hospcode<17 and dat$sixmonth==39
# Multiply the kg by 2.2 to convert them into lbs
dat$weight[which(dat$hospcode<17 & dat$sixmonth==39)] <- dat$weight[which(dat$hospcode<17 & dat$sixmonth==39)]*2.2

# Recalculate BMI
# compute BMI from the height and weight (the calculation is: weight (lbs)/height (in)^2 * 703) 
dat$bmi <- 703*(dat$weight/((dat$height)^2))

## Load VIM package for Visualization and Imputation of Missing Values
library(VIM)

# Reduce data
r_dat <- dat
r_dat[,c("weight","height")]<-NULL

# Show complete cases
comp <- r_dat[complete.cases(r_dat),]
nrow(comp)/nrow(r_dat)

# Show non complete cases
non_comp <- r_dat[!complete.cases(r_dat),]
nrow(non_comp)/nrow(r_dat)

# Visualize the missing data
aggr(r_dat, prop = F, numbers = T,combined=T,sortVars=T)
aggr(r_dat, prop = F, numbers = T,sortVars=T)
matrixplot(dat,interactive=T)

# Focus on: albumin, bmi, asa and proced
scattmatrixMiss(dat, interactive = F)

## Create data frame indicating missingness by 1
x <- as.data.frame(abs(is.na(r_dat)))
## Select columns with some (but not all) missing values
y <- x[,sapply(x, sd) > 0]

## Create a correlation matrix: Variables missing together have high correlation
cor(y)