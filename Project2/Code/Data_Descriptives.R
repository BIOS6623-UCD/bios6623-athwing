setwd("~/Documents/CU Denver/Fall_2017/Advanced_Data_Analysis/Project_2")
library(sas7bdat)

dat <- read.sas7bdat("vadata2.sas7bdat")

# I would like you to tell me the observed death rate from surgery at each hospital for the most recent 6 month period 
# (IE;sixmonth = 39)
# Also provide some measure of variation around the expected rate of death at each hospital

# Descriptive Statistics
# Note: R recognizes NaN as NA

# First look summary: ie; look at means, NA's etc.
# summary(dat)

# Look at distribution of hospitals:
# hist(dat$hospcode)

# Check how many hospitals have each procedure: 0 = valve, 1 = CABG, we don't want to include procedure 2
# table(dat$proced)
# Remove individuals with procedure 2
dat[which(dat$proced==2),] <- NA
# table(dat$proced)
# See distribution of procedure over hospital
# table(dat$hospcode[which(dat$proced==0)])
# table(dat$hospcode[which(dat$proced==1)])

# asa - patients start condition, 1:5, 1 good health -> 5 near death
# table(dat$asa)

# BMI is sufficient (can toss height and weight)
# Check for weight outliers
plot(dat$weight) # two distinct groups, should probably convert some
hist(dat$weight)
summary(dat$weight) # Missing 704 weight values
plot(dat$weight,dat$hospcode)
plot(dat$weight,dat$sixmonth)
# Convert height and weight for 39 month time points at hospital codes under 20?
plot(dat$weight[which(dat$hospcode<20)],dat$hospcode[which(dat$hospcode<20)])
plot(dat$weight[which(dat$hospcode<17)],dat$hospcode[which(dat$hospcode<17)])
plot(dat$weight[which(dat$hospcode<17)],dat$sixmonth[which(dat$hospcode<17)])

# People to convert: dat$hospcode<17 and dat$sixmonth==39
# Multiply the kg by 2.2 to convert them into lbs
dat$weight[which(dat$hospcode<17 & dat$sixmonth==39)] <- dat$weight[which(dat$hospcode<17 & dat$sixmonth==39)]*2.2

# Verify that you have corrected the weight issues
plot(dat$weight)

# Check for height outliers ** NONE
# plot(dat$height) # Nicely distributed
# summary(dat$height) # Missing 704 values
# Looks fine, no need to convert

# Recalculate BMI
# compute BMI from the height and weight (the calculation is: weight (lbs)/height (in)^2 * 703) 
dat$bmi <- 703*(dat$weight/((dat$height)^2))
# plot(dat$bmi) # Some possible outliers

# Email Nicole about these BMIs:
# dat$bmi[which(dat$bmi<15)]
# 14.06198 11.75813 14.07736 14.80945 12.53193 14.87330 13.81189 14.48363 13.45211

# albumin: ranges from 3.4 - 5.4 g/dL
# length(which(dat$albumin<3.4))/nrow(dat)
# length(which(dat$albumin>5.4))/nrow(dat)
# Email Nicole about the biologically plausible range

# death30 : thirty day mortality
# table(dat$death30)

## MISSING DATA
# use_Names <- c("hospcode","sixmonth","proced","asa","bmi","albumin","death30")
# mat <- matrix(data=NA,ncol = 2,nrow = 44)
# for(i in 1:44){
#   matrix[i,1] <- i
#   for(j in 1:length(use_Names)){
#     curName <- useNames[j]
#     num_nas <- summary(dat[,curName][which(dat$hospcode==j)])[7]
#     
#   }
# }

## Load VIM package for Visualization and Imputation of Missing Values
library(VIM)

# Reduce data
r_dat <- dat
r_dat[,c("weight","height")]<-NULL

## Show complete cases
r_dat[complete.cases(r_dat),]

# Show non complete cases
r_dat[!complete.cases(r_dat),]

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

# Make a table one
library(tableone)

# Notes:
# Done: Converted weight, recalculated BMI, removed procedure 2
# We want to come up with an expected death rate for each hospital
# So we use all six months to come up with expected value
# -> hosp, # died, # seen, expected death adjusted for procedure
# There should not be time trends
# Ex: Should be something low, under 5%, or can give it as how many died per 1000
# Pool data together to come up with expected rates
# We want hospital as a random effect (intercept?) Do we also include a fixed effect for hospital? What about interactions?
# Put together whole population, then given data for my specific hosptial given these procedures, these patients, what should I see?
# What you see (ie; mean), also median, IQR would be good
# observed column: only last month time point; expected column: all time points

# Model
library(lme4)

# Include a random intercept for hospital
# Model covariates: hospcode, proced, asa, bmi, albumin
lm.fit <- glmer(death30 ~ factor(hospcode) + proced + asa + bmi + albumin + (1|hospcode), data = dat, family = binomial())

# Make prediction at the hospital level by making hospcode as a fixed effect and as a factor
# predict(lm.fit for 39 month data)