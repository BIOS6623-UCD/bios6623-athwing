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


# Model
dat <- na.omit(dat)

# Include a random intercept for hospital
# Model covariates: hospcode, proced, asa, bmi, albumin

lm.fit <- glm(death30 ~ proced + asa + bmi + albumin, data = dat, family = binomial())

# Put predicted probabilities in their own column
# type = "response" gives the predicted probabilities
dat$predicted <- predict(lm.fit,type = "response")

# Loop through and group by hospital
hospital_expected <- matrix(NA,ncol = 1,nrow = 44)
for(i in 1:nrow(hospital_expected)){
  current_pred <- dat$predicted[which(dat$hospcode==i)]
  expected <- sum(current_pred)/length(current_pred)
  hospital_expected[i] <- expected
}

# Create vector of observed death rates at 39 month time point
new_dat <- dat[which(dat$sixmonth==39),]

hospital_observed <- matrix(NA,ncol = 1,nrow = 44)
for(i in 1:nrow(hospital_observed)){
  deaths <- new_dat$death30[which(new_dat$hospcode==i)]
  rate <- sum(deaths)/length(deaths)
  hospital_observed[i] <- rate
}

# Use bootstrapping to find confidence intervals
boot <- function(){
  
  for(i in 1:1000){
    new_sample <- sample(new_dat$predicted, size=length(new_dat$predicted), replace = T)
  }
  
}




