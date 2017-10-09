## THIS CODE IS PURELY TO CLEAN UP THE DATA SET BEFORE IMPORTING INTO SAS
## Excess code is commented out, if the code below is run, it will create a CSV file
## From which the NA's need to be changed to . ie; open Text Wrangler: Find NA, replace with .
## Then it will be ready to run in SAS

# setwd("~/Documents/CU Denver/Fall_2017/Advanced_Data_Analysis/Project_1")
dat <- read.table("hiv_6623_final.csv",header = T,sep = ",")

# Reduce data to only years 0(baseline) and 2
dat <- dat[which(dat$years==2 | dat$years==0),]

# Separate out into baseline dataset and outcome dataset
baseline <- dat[which(dat$years==0),] # We have 715 baseline values
outcome <- dat[which(dat$years==2),]  # We have 506 outcome values

# Find missing users
missing <- which(!baseline$newid%in%outcome$newid) # ID's in baseline and not in outcome

# Remove missing from baseline
baseline <- baseline[-c(missing),]

# Baseline BMI -- BMI 999/-1: missing -> NA
baseline$BMI[which(baseline$BMI==-1 | baseline$BMI==999)] <- NA
# summary(baseline$BMI)
# Remove BMI outlier
baseline$BMI[which(baseline$BMI > 500)] <- NA
# summary(baseline$BMI)
# plot(baseline$BMI)

# Race - NHW vs. Other -- NHW: 1 -> 1 other: 2-8 -> 0 Blank: missing
# table(baseline$RACE)
# summary(baseline$RACE)
baseline$RACE[which(baseline$RACE!=1)] <- 2
# table(baseline$RACE)

# Baseline marijuana use -- HASHV (Y/N) Blank: missing

# Baseline alcohol use - > DKGRP 13 drinks per week: 3 -> 1 vs. 13 or less: 0/1/2 -> 0 Blank: missing
# table(baseline$DKGRP)
baseline$DKGRP[which(baseline$DKGRP!=3)] <- 0
baseline$DKGRP[which(baseline$DKGRP==3)] <- 1
# table(baseline$DKGRP)

# Baseline smoking - Current smokers: 3 -> 1 vs. never/former: 1/2 -> 0 Blank: missing
# table(baseline$SMOKE)
baseline$SMOKE[which(baseline$SMOKE!=3)] <- 0
baseline$SMOKE[which(baseline$SMOKE==3)] <- 1
# table(baseline$SMOKE)

# Baseline income level - < $10,000: 1 -> 0 $10,000 - $40,000: 2/3/4 -> 1  >$40,000: 5/6/7 -> 2  9: missing -> NA
# table(baseline$income)
baseline$income[which(baseline$income==9)] <- NA
baseline$income[which(baseline$income==1)] <- 0
baseline$income[which(baseline$income==2 | baseline$income==3 | baseline$income==4)] <- 1
baseline$income[which(baseline$income==5 | baseline$income==6 | baseline$income==7)] <- 2
# table(baseline$income)

# Education -> HS: 4/5/6/7 -> 1  vs. HS or less: 1/2/3 -> 0 Blank: missing
# table(baseline$EDUCBAS)
baseline$EDUCBAS[which(baseline$EDUCBAS==1 | baseline$EDUCBAS==2 | baseline$EDUCBAS==3)] <- 0
baseline$EDUCBAS[which(baseline$EDUCBAS!=0)] <- 1
# table(baseline$EDUCBAS)

# ART Adherence, >95%: 1/2 -> 1 vs. <95%: 3/4 -> 0 Blank: missing (from outcome)
# table(outcome$ADH)
outcome$ADH[which(outcome$ADH==3 | outcome$ADH==4)] <- 0
outcome$ADH[which(outcome$ADH==1 | outcome$ADH==2)] <- 1
# table(outcome$ADH)
# Match up baseline and outcome IDs
baseline$ADH <- outcome$ADH
# table(baseline$ADH)

# Transform VLOAD on the log10 scale
# plot(baseline$VLOAD)
# plot(outcome$VLOAD)
baseline$VLOAD <- log10(baseline$VLOAD)
outcome$VLOAD <- log10(outcome$VLOAD)
# plot(baseline$VLOAD)
# plot(outcome$VLOAD)

HIV_data <- baseline[,c("X","newid","AGG_MENT","AGG_PHYS","HASHV","income","BMI","SMOKE","DKGRP","LEU3N","VLOAD","ADH",
                        "RACE","EDUCBAS","age","hard_drugs")]

# Now add outcomes and differences
HIV_data$AGG_MENTDIFF <- outcome$AGG_MENT - baseline$AGG_MENT
HIV_data$AGG_PHYSDIFF <- outcome$AGG_PHYS - baseline$AGG_PHYS
HIV_data$LEU3NDIFF <- outcome$LEU3N - baseline$LEU3N
HIV_data$VLOADDIFF <- outcome$VLOAD - baseline$VLOAD

HIV_data$AGG_MENT2 <- outcome$AGG_MENT
HIV_data$AGG_PHYS2 <- outcome$AGG_PHYS
HIV_data$LEU3N2 <- outcome$LEU3N
HIV_data$VLOAD2 <- outcome$VLOAD

write.table(HIV_data,"HIV_cleaned.csv",row.names = F,sep=",",quote = F)


# # Descriptive statistics
# # Create a table one
# library(tableone)
# 
# ## List of categorical variables
# catVars <- c("HASHV","income","SMOKE","DKGRP","ADH","RACE","EDUCBAS","hard_drugs")
# 
# ## List of continuous variables
# contVars <- c("BMI","age")
# 
# # Create table one
# tableOne <- CreateTableOne(vars = c(contVars,catVars),data = HIV_data,factorVars = catVars,test = T)
# tableOne_strata <- CreateTableOne(vars = c(contVars,catVars),strata = "hard_drugs",data = HIV_data,
#                                   factorVars = catVars,includeNA = T,test = T)
# print(tableOne_strata,showAllLevels = T)
# 
# # Some Plots
# plot(HIV_data$BMI)
# 
# # Quick look at outcome distribution between drug_users and non_users
# hist(HIV_data$AGG_MENTDIFF[baseline$hard_drugs==0],main="AGG_MENT outcome distribution",xlab = "AGG_MENT score") # non_users = clear
# hist(HIV_data$AGG_MENTDIFF[baseline$hard_drugs==1],add=T,col = rgb(1,0,0,0.5)) # drug_users = red
# 
# hist(HIV_data$AGG_PHYSDIFF[baseline$hard_drugs==0],main="AGG_PHYS outcome distribution",xlab = "AGG_PHYS score") # non_users = clear
# hist(HIV_data$AGG_PHYSDIFF[baseline$hard_drugs==1],add=T,col = rgb(1,0,0,0.5)) # drug_users = red
# 
# hist(HIV_data$VLOAD[baseline$hard_drugs==0]) # non_users = clear
# hist(HIV_data$VLOAD[baseline$hard_drugs==1],add=T,col = rgb(1,0,0,0.5)) # drug_users = red
# 
# hist(HIV_data$LEU3N[baseline$hard_drugs==0],main="LEU3N distribution",xlab = "CD4+ cell count") # non_users = clear
# hist(HIV_data$LEU3N[baseline$hard_drugs==1],add=T,col = rgb(1,0,0,0.5)) # drug_users = red
# 
