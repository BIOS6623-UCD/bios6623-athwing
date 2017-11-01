# This script should provide all of the code needed to replicate our results

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

# Remove albumin, weight and height from data set
dat <- dat[,c("hospcode","sixmonth","proced","asa","bmi","death30")]
# Keep only complete cases
dat <- na.omit(dat)

# Model
# Model covariates: hospcode, proced, asa, bmi
lm.fit <- glm(death30 ~ proced + asa + bmi, data = dat, family = binomial())

# Put last month data into new_dat
new_dat <- dat[which(dat$sixmonth==39),]

# Put predicted probabilities in their own column, type = "response" gives predicted probabilities
new_dat$predicted <- predict(lm.fit,type = "response",newdata = new_dat)

# Find expected death rate per hospital using tapply
expected <- tapply(new_dat$predicted, new_dat$hospcode, mean,simplify = T)
# Make sure no hospitals have missing expected death rates / NA if they do / keep in order
if(length(names(expected))<44){
  missing_hospital <- sum(seq(1:44)) - sum(as.integer(names(expected)))
  expected[(missing_hospital+1):44] <- expected[missing_hospital:43]
  expected[missing_hospital] <- NA
}
names(expected) <- seq(1:44)

# Find observed death rate per hospital
observed <- tapply(new_dat$death30, new_dat$hospcode, mean,simplify = T)
# Put NA in for any missing
if(length(names(observed))<44){
  missing_hospital <- sum(seq(1:44)) - sum(as.integer(names(observed)))
  observed[(missing_hospital+1):44] <- observed[missing_hospital:43]
  observed[missing_hospital] <- NA
}
names(observed) <- seq(1:44)

# Create final which will hold observed, expected, then those over/under the clinical threshold
final <- cbind(observed, expected)

# Bootstrap to find variation

# Actual 39 month values from original dataset
last_month <- dat[which(dat$sixmonth==39),]

# Create a matrix that has 44 rows(hospitals) and n columns (num of boostraps)
n <- 1000 # Set number of bootstraps
probs <- matrix(0,nrow = 44,ncol = n)

for(i in 1:n){
  print(i)
  # Create a new sample from everything
  new_sample <- dat[sample(seq(1:nrow(dat)),nrow(dat),replace = T),]
  
  # Fit model using new data sample to get covariates
  fit <- glm(death30 ~ proced + asa + bmi, data = new_sample, family = binomial())
  
  # Put predicted probabilities in their own column, type = "response" gives predicted probabilities
  # Important: newdata = last_month is actual 39 month values from the original datset
  # predicted with the model fit from the bootstrapped dataset
  last_month$predicted <- predict(fit,type = "response",newdata = last_month)
  
  # Find expected death rate per hospital using new predicted probabilites
  expected_bts <- tapply(last_month$predicted, last_month$hospcode, mean,simplify = T)
  # Account for any missing data, if a hospital is missing give it NA // Keep hospitals in order
  if(length(names(expected_bts))<44){
    missing_hospital <- sum(seq(1:44)) - sum(as.integer(names(expected_bts)))
    expected_bts[(missing_hospital+1):44] <- expected_bts[missing_hospital:43]
    expected_bts[missing_hospital] <- NA
  }
  names(expected_bts) <- seq(1:44)
  
  probs[,i] <- expected_bts
}

lower_bound <- apply(probs, 1, function(x) quantile(x, probs = .025, na.rm = T))
upper_bound <- apply(probs, 1, function(x) quantile(x, probs = .975, na.rm = T))
clinical <- rep(0,44)
conf_interval <- rep(0,44)
final <- as.data.frame(cbind(final,lower_bound,upper_bound,conf_interval,clinical))

# Whether or not it is above or below the 95% Confidence interval
final$conf_interval[which(final$observed>final$upper_bound)] <- 1
final$conf_interval[which(final$observed<final$lower_bound)] <- -1

# We are worried at the clinical level if obs/exp > 1.2
final$clinical[which((final$observed/final$expected)>1.2)] <- 1
final$clinical[which((final$observed/final$expected)<0.8)] <- -1

