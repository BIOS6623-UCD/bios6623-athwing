# We are interested in understanding how treatment response 2 years after initiating HAART 
# differs between subjects who report using hard drugs, such as heroine and cocaine, 
# at baseline and other subjects, who did not report hard drug use at baseline.   

setwd("~/Documents/CU Denver/Fall_2017/Advanced_Data_Analysis/Project_1")
dat <- read.table("hiv_6623_final.csv",header = T,sep = ",")

# hard_drugs = 0 (not used) 1 (used) NA (missing)
# treatment response: 
# lab measures -> VLOAD, LEU3n (CD4+ cell count)
# quality of life measures -> AGG_PHYS, AGG_MENT

# Reduce data to only years 0(baseline) and 2
dat <- dat[which(dat$years==2 | dat$years==0),]

# Separate out into baseline dataset and outcome dataset
baseline <- dat[which(dat$years==0),] # We have 715 baseline values
outcome <- dat[which(dat$years==2),]  # We have 506 outcome values

# Find missing users
missing <- which(!baseline$newid%in%outcome$newid) # ID's in baseline and not in outcome

# Separate into drug users and nondrug users at baseline
drug_users <- unique(dat$newid[which(dat$hard_drugs==1 & dat$years==0)])
non_users <- unique(dat$newid[which(dat$hard_drugs==0 & dat$years==0)])

# Add a column for hard drug use at baseline to outcome
outcome$use_at_baseline <- 0
outcome$use_at_baseline[outcome$newid%in%drug_users] <- 1

# Find ID's of users who used drugs at baseline to double check they are in fact drug users
outcome$newid[which(outcome$use_at_baseline==1)]

unique(outcome$newid[which(outcome$use_at_baseline==1)] %in% baseline$newid[which(baseline$hard_drugs==1)])
unique(outcome$newid[which(outcome$use_at_baseline==0)] %in% baseline$newid[which(baseline$hard_drugs==0)])

# Create a tableone
library(tableone)

## Look at descriptive statistics for categorical variables at year = two
catVars <- c("HASHV","HASHF","income","HBP","DIAB","LIV34","KID","FRP","FP","DYSLIP","SMOKE","DKGRP",
             "HEROPIATE","IDU","ADH","RACE","EDUCBAS")

# Stratify by hard drug use at baseline
catTableStrata <- CreateCatTable(vars = catVars,strata = "use_at_baseline",data = outcome,test=T,includeNA = T)
print(catTableStrata,showAllLevels = T)

## Look at descriptive statistics for continuous variables at year = two
contVars <- c("BMI","TCHOL","TRIG","LDL","CESD","age")

# Stratify by hard drug use at baseline
contTableStrata <- CreateContTable(vars = contVars,strata = "use_at_baseline",data = outcome,test=T)
print(contTableStrata,showAllLevels = T)

# Create table one
tableOne <- CreateTableOne(vars = c(contVars,catVars),data = outcome,factorVars = catVars,test = T)
tableOne_strata <- CreateTableOne(vars = c(contVars,catVars),strata = "use_at_baseline",data = outcome,
                                  factorVars = catVars,includeNA = T,test = T)
print(tableOne_strata,showAllLevels = T)


# Deal with NA values
# "HASHV","HASHF","income","HBP","DIAB","LIV34","KID","FRP","FP","DYSLIP","SMOKE","DKGRP",
# "HEROPIATE","IDU","ADH","RACE","EDUCBAS"
# BMI: 999 -> NA, -1 -> NA
# Race: blank -> NA
# EDUC: blank -> NA
# Income: 9 -> NA
# CESD: -1 -> NA
# SMOKE: blank -> NA
# DKGRP: blank -> NA
# IDU: blank -> NA
# HEROPIATE: blank -> NA, -9 -> NA
# HASHV: blank -> NA
# HASHF: blank -> NA
# KID: 9 -> NA
# LIV34: 9 -> NA
# FRP,FP: 9 -> NA
# HBP: 9 -> NA, -1 -> NA
# DYSLIP: 9 -> NA
# TRIG: blank -> NA
# LDL: blank -> NA
# DIAB: 9 -> NA
# ADH: blank -> NA












# Quick look at outcome distribution between drug_users and non_users
hist(outcome$AGG_MENT[outcome$use_at_baseline==0]) # non_users = clear
hist(outcome$AGG_MENT[outcome$use_at_baseline==1],add=T,col = rgb(1,0,0,0.5)) # drug_users = red

hist(outcome$AGG_PHYS[outcome$use_at_baseline==0]) # non_users = clear
hist(outcome$AGG_PHYS[outcome$use_at_baseline==1],add=T,col = rgb(1,0,0,0.5)) # drug_users = red

hist(outcome$VLOAD[outcome$use_at_baseline==0]) # non_users = clear
hist(outcome$VLOAD[outcome$use_at_baseline==1],add=T,col = rgb(1,0,0,0.5)) # drug_users = red

hist(outcome$LEU3N[outcome$use_at_baseline==0]) # non_users = clear
hist(outcome$LEU3N[outcome$use_at_baseline==1],add=T,col = rgb(1,0,0,0.5)) # drug_users = red















# don't need to worry about: X, ART, everART,hivpos

## Look at descriptive statistics for continuous
contVars <- c("age","BMI")
contTableOverall <- CreateContTable(vars = contVars, strata="hard_drugs",data = dat)

# Correlation between variables


