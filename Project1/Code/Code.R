# We are interested in understanding how treatment response 2 years after initiating HAART 
# differs between subjects who report using hard drugs, such as heroine and cocaine, 
# at baseline and other subjects, who did not report hard drug use at baseline.

# hard_drugs = 0 (not used) 1 (used) NA (missing)
# treatment response: 
# lab measures -> VLOAD (log transform), LEU3n (CD4+ cell count)
# quality of life measures -> AGG_PHYS, AGG_MENT

# We are interested in these covariates:
# Baseline value of the outcome -- VLOAD,LEU3n,AGG_PHYS,AGG_MENT
# Baseline age -- age

setwd("~/Documents/CU Denver/Fall_2017/Advanced_Data_Analysis/Project_1")
dat <- read.table("hiv_6623_final.csv",header = T,sep = ",")

# Reduce data to only years 0(baseline) and 2
dat <- dat[which(dat$years==2 | dat$years==0),]

# Separate out into baseline dataset and outcome dataset
baseline <- dat[which(dat$years==0),] # We have 715 baseline values
outcome <- dat[which(dat$years==2),]  # We have 506 outcome values

# Find missing users
missing <- which(!baseline$newid%in%outcome$newid) # ID's in baseline and not in outcome

# Baseline BMI -- BMI 999/-1: missing -> NA
baseline$BMI[which(baseline$BMI==-1 | baseline$BMI==999)] <- NA
summary(baseline$BMI)

# Race - NHW vs. Other -- NHW: 1 -> 1 other: 2-8 -> 0 Blank: missing
summary(baseline$RACE)
baseline$RACE[which(baseline$RACE!=1)] <- 2
table(baseline$RACE)

# Baseline marijuana use -- HASHV (Y/N) Blank: missing

# Baseline alcohol use - > DKGRP 13 drinks per week: 3 -> 1 vs. 13 or less: 0/1/2 -> 0 Blank: missing
baseline$DKGRP[which(baseline$DKGRP!=3)] <- 0
baseline$DKGRP[which(baseline$DKGRP==3)] <- 1
table(baseline$DKGRP)

# Baseline smoking - Current smokers: 3 -> 1 vs. never/former: 1/2 -> 0 Blank: missing
baseline$SMOKE[which(baseline$SMOKE!=3)] <- 0
baseline$SMOKE[which(baseline$SMOKE==3)] <- 1
table(baseline$SMOKE)

# Baseline income level - < $10,000: 1 -> 0 $10,000 - $40,000: 2/3/4 -> 1  >$40,000: 5/6/7 -> 2  9: missing -> NA
table(baseline$income)
baseline$income[which(baseline$income==9)] <- NA
baseline$income[which(baseline$income==1)] <- 0
baseline$income[which(baseline$income==2 | baseline$income==3 | baseline$income==4)] <- 1
baseline$income[which(baseline$income==5 | baseline$income==6 | baseline$income==7)] <- 2
table(baseline$income)

# Education -> HS: 3/4/5/6/7 -> 1  vs. HS or less: 1/2 -> 0 Blank: missing
table(baseline$EDUCBAS)
baseline$EDUCBAS[which(baseline$EDUCBAS==1 | baseline$EDUCBAS==2)] <- 0
baseline$EDUCBAS[which(baseline$EDUCBAS!=0)] <- 1
table(baseline$EDUCBAS)

# ART Adherence, >95%: 1/2 -> 1 vs. <95%: 3/4 -> 0 Blank: missing (from outcome)
table(outcome$ADH)
baseline$ADH[which(outcome$ADH==3 | outcome$ADH==4)] <- 0
baseline$ADH[which(outcome$ADH==1 | outcome$ADH== 2)] <- 1
table(baseline$ADH)
# missing is going to be those we don't have a measurement for

which(outcome$ADH==1 | outcome$ADH==2)
which(baseline$ADH==1)



# make a baseline vector
# if id is missing make it na
# else fill in values from outcome











# Separate into drug users and nondrug users at baseline
drug_users <- unique(dat$newid[which(dat$hard_drugs==1 & dat$years==0)])
non_users <- unique(dat$newid[which(dat$hard_drugs==0 & dat$years==0)])

# Add a column for hard drug use at baseline to outcome
# outcome$use_at_baseline <- 0
# outcome$use_at_baseline[outcome$newid%in%drug_users] <- 1

# Find ID's of users who used drugs at baseline to double check they are in fact drug users
# outcome$newid[which(outcome$use_at_baseline==1)]

# unique(outcome$newid[which(outcome$use_at_baseline==1)] %in% baseline$newid[which(baseline$hard_drugs==1)])
# unique(outcome$newid[which(outcome$use_at_baseline==0)] %in% baseline$newid[which(baseline$hard_drugs==0)])

## List of categorical variables
catVars <- c("HASHV","HASHF","income","HBP","DIAB","LIV34","KID","FRP","FP","DYSLIP","SMOKE","DKGRP",
             "HEROPIATE","IDU","ADH","RACE","EDUCBAS")

## List of continuous variables
contVars <- c("BMI","TCHOL","TRIG","LDL","CESD","age")

# Replace all 9, 999 and -1 with NA
missing_vals <- c(9,999,-1,-9)
for(i in 1:ncol(baseline)){
  if(!is.null(colnames(baseline)[i]%in%catVars)){
    baseline[,i][which(baseline[,i]%in%missing_vals)] <- NA
  }
  if(!is.null(colnames(baseline[i]%in%contVars))){
    baseline[,i][which(baseline[,i]<0)] <- NA
  }
}

# 9 is categorical, -1 for continuous


# Create a tableone
library(tableone)


# Stratify by hard drug use at baseline
catTableStrata <- CreateCatTable(vars = catVars,strata = "use_at_baseline",data = outcome,test=T,includeNA = T)
print(catTableStrata,showAllLevels = T)

# Stratify by hard drug use at baseline
contTableStrata <- CreateContTable(vars = contVars,strata = "use_at_baseline",data = outcome,test=T)
print(contTableStrata,showAllLevels = T)

# Create table one
tableOne <- CreateTableOne(vars = c(contVars,catVars),data = baseline,factorVars = catVars,test = T)
tableOne_strata <- CreateTableOne(vars = c(contVars,catVars),strata = "hard_drugs",data = baseline,
                                  factorVars = catVars,includeNA = T,test = T)
print(tableOne_strata,showAllLevels = F)

# Transform VLOAD on the log10 scale
# Email investigator about covariates to include

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


# Ask about covariates to include, missing vs declined to answer, insufficient data, etc.
# Cohort study: demographic differences between hard drug users and non drug users
# Age, BMI, race, socioeconomic -> educ_level or income, adherence

# Email for list of covariates controlled for or typically included
# Email Camille for the clinically significant covariates she is mostly interested in

# Partial F -> Pairwise



# Quick look at outcome distribution between drug_users and non_users
hist(baseline$AGG_MENT[baseline$hard_drugs==0],main="AGG_MENT outcome distribution",xlab = "AGG_MENT score") # non_users = clear
hist(baseline$AGG_MENT[baseline$hard_drugs==1],add=T,col = rgb(1,0,0,0.5)) # drug_users = red

hist(baseline$AGG_PHYS[baseline$hard_drugs==0],main="AGG_PHYS outcome distribution",xlab = "AGG_PHYS score") # non_users = clear
hist(baseline$AGG_PHYS[baseline$hard_drugs==1],add=T,col = rgb(1,0,0,0.5)) # drug_users = red

hist(baseline$VLOAD[baseline$hard_drugs==0]) # non_users = clear
hist(baseline$VLOAD[baseline$hard_drugs==1],add=T,col = rgb(1,0,0,0.5)) # drug_users = red

hist(baseline$LEU3N[baseline$hard_drugs==0],main="LEU3N distribution",xlab = "CD4+ cell count") # non_users = clear
hist(baseline$LEU3N[baseline$hard_drugs==1],add=T,col = rgb(1,0,0,0.5)) # drug_users = red

# Add adherence variable from year two to dataset


# Correlation between variables


