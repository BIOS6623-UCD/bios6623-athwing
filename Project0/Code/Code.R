setwd("~/Documents/FALL2017/Advanced_Data_Analysis/Project_0")
dental_data <- read.table("Project0_dental_data.csv",header = T,sep=",")

## Data management
##
# There is one subject with a missing age, one with a missing smoking status and 27 with missing 1 year measurements
# those with missing 1 year measurements also have an NA for the _diff outcome variable.

## Search for outliers by evalutaing each column of the data frame:
# Subjects are evenly distributed into treatment groups
hist(dental_data$trtgroup)
table(dental_data$trtgroup)

# Genders somewhat evenly distributed
table(dental_data$sex)

# Race not distributed evenly
hist(dental_data$race)
table(dental_data$race)

# Age has a roughly normal distribution
hist(dental_data$age)

# Smokers/nonsmokers not even
table(dental_data$smoker)

# look at distribution of sites
table(dental_data$sites)
qqplot(dental_data$id,dental_data$sites)

summary(dental_data$attachbase)
summary(dental_data$attach1year)
summary(dental_data$pdbase)
summary(dental_data$pd1year)

# Are the NA's evenly distributed?
# Treatment group 6 has the largest amount of NAs
NA_rows <- which(is.na(dental_data$pd1year))
treat <- dental_data$trtgroup[NA_rows]
table(treat)
hist(treat)

# Use the difference between pre/post as the outcome
# Two outcomes <- attach_dif and pd_diff, difference in attachment and difference in 
dental_data$attach_diff <- dental_data$attach1year-dental_data$attachbase
dental_data$pd_diff <- dental_data$pd1year-dental_data$pdbase

# Two control arms: don't group them
# Use two outcomes, one for pd and one for attach.

# Model

# Consider including baseline attachment measurement as a covariate (attach_fit2)
attach_fit <- lm(attach_diff ~ factor(trtgroup) + sex + race + age + smoker + sites,data = dental_data)
attach_fit2 <- lm(attach_diff ~ factor(trtgroup) + sex + race + age + smoker + sites + attachbase,data = dental_data)

# Model for the pd outcome
# pd_fit2 considers including the baseline pd measurement as a covariate
pd_fit <- lm(pd_diff ~ trtgroup + sex + race + age + smoker + sites,data = dental_data)
pd_fit <- lm(pd_diff ~ trtgroup + sex + race + age + smoker + sites +pdbase,data = dental_data)

# Plot the fits, examine the residuals
plot(attach_fit)
hist(attach_fit$residuals)
plot(pd_fit)
hist(pd_fit$residuals)

