# Read in data given in the group github 
setwd("~/Documents/FALL2017/Advanced_Data_Analysis/Project_0")
dental_data <- read.table("Project0_dental_data.csv",header = T,sep=",")

## Data management

# There is one subject with a missing age, one with a missing smoking status and 27 with missing 1 year measurements
# those with missing 1 year measurements also have an NA for the _diff outcome variable.

# Are the NA's evenly distributed?
# Treatment group 6 has the largest amount of NAs
# NA_rows <- which(is.na(dental_data$pd1year))
# treat <- dental_data$trtgroup[NA_rows]
# table(treat)
# hist(treat,main = "NA Distribution Across Treatment Groups")

## Search for outliers by evalutaing each column of the data frame:

# Check that subjects are evenly distributed into treatment groups
# hist(dental_data$trtgroup)
# table(dental_data$trtgroup)

# Genders somewhat evenly distributed
# table(dental_data$sex)
# Evenly distributed throughout treatment groups
# table(dental_data$trtgroup[which(dental_data$sex==1)])
# table(dental_data$trtgroup[which(dental_data$sex==2)])

# Race not distributed evenly
# hist(dental_data$race)
# table(dental_data$race)
# Race is distributed evenly across treatment groups
# Evenly distributed throughout treatment groups
# table(dental_data$trtgroup[which(dental_data$race==1)])
# table(dental_data$trtgroup[which(dental_data$race==2)])
# table(dental_data$trtgroup[which(dental_data$race==4)])
# table(dental_data$trtgroup[which(dental_data$race==5)])

# Age has a roughly normal distribution
# hist(dental_data$age)

# Smokers/nonsmokers distribution across treatments
# table(dental_data$smoker)
# table(dental_data$trtgroup[which(dental_data$smoker==1)])
# table(dental_data$trtgroup[which(dental_data$smoker==0)])

# Look at distribution of sites
# table(dental_data$sites)
# qqplot(dental_data$id,dental_data$sites)

# Check summaries of measurements
# summary(dental_data$attachbase)
# summary(dental_data$attach1year)
# summary(dental_data$pdbase)
# summary(dental_data$pd1year)

# Use the difference between pre/post as the outcome
# Two outcomes <- attach_dif and pd_diff, difference in attachment and difference in 
dental_data$attach_diff <- dental_data$attach1year-dental_data$attachbase
dental_data$pd_diff <- dental_data$pd1year-dental_data$pdbase

# Visualize the difference with a boxplot
boxplot(attach_diff ~ factor(trtgroup), data = dental_data,xlab = "Treatment Group", ylab = "Average change in attachment loss")
boxplot(pd_diff ~ factor(trtgroup), data = dental_data,xlab = "Treatment Group", ylab = "Average change in pocket depth")

# Plot differences to potentially observe outliers
# hist(dental_data$attach_diff)
# plot(dental_data$attach_diff)
# hist(dental_data$pd_diff)
# plot(dental_data$pd_diff)

# Model

# Two control arms: don't group them
# Use two outcomes, one for pd and one for attach.

# I want to include the baseline measure as a covariate, as starting with less of a depth you can have a greater
# increase than starting at more shallow of a depth
# AIC backs this up

# Consider with and without baseline attachment measurement as a covariate
# attach_fit_NB <- lm(attach_diff ~ factor(trtgroup),data = dental_data)
attach_fit <- lm(attach_diff ~ factor(trtgroup) + attachbase,data = dental_data)
# AIC(attach_fit)
# AIC(attach_fit_NB)

# Model for the pd outcome
# Consider with and without baseline pocket depth measurement as a covariate
# pd_fit_NB <- lm(pd_diff ~ factor(trtgroup),data = dental_data)
pd_fit <- lm(pd_diff ~ factor(trtgroup) + pdbase,data = dental_data)
# AIC(pd_fit)
# AIC(pd_fit_NB)

# Plot the fits, examine the residuals
plot(attach_fit)
hist(attach_fit$residuals)
plot(pd_fit)
hist(pd_fit$residuals)

# Look at summaries of results
summary(attach_fit)
confint(attach_fit)
summary(pd_fit)
confint(pd_fit)
