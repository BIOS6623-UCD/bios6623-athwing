# Function to search for change point (adapted from Camille)

cp.search_and_fit <- function(y, dataSet, cps = seq(0,6,.083)){
  id <- dataSet$id
  age <- dataSet$age
  ageonset <- dataSet$ageonset
  demind <- dataSet$demind
  SES <- dataSet$SES
  gender <- dataSet$gender
  
  # Place to store likelihoods from the CP search
  ll <-data.frame (changepoint=rep(NA,length(cps)), ll=rep(NA,length(cps)))
  
  # Search for the CP
  for (i in 1:length(cps)){
    cp <- cps[i]
    # tau is either time after changepoint or 0 if before changepoint, should stay 0 where demind = 0
    tau <- ifelse(demind==1 & (age>ageonset-cp), age - ageonset + cp, 0)
    cp.model <- lme(y ~ age + demind + age*demind + tau + gender + SES, random=~1|id, method='ML')
    ll[i,] <- c(cp,logLik(cp.model))
  }
  
  # Plot the likelihood
  plot(ll$changepoint, ll$ll, type='l', xlab='Change Point (months)', ylab='Log Likelihood')
  
  # Find the max
  cp<-ll[which(ll$ll==max(ll$ll)),'changepoint']
  print(cp)
  
  # Fit the final model
  tau <- ifelse(demind==1 & (age>ageonset-cp), age - ageonset + cp, 0)
  cp.model <- lme(y ~ age + demind + age*demind + tau + gender + SES, random=~1|id, method='ML')
  return(list(cp=cp, model=cp.model))
}

# Run the function on the dataset
cp.model.animals <- cp.search_and_fit(y = animals.pop_CC$animals, dataSet = animals.pop_CC)




# Clinically: What's happening with people's memory measures as they age?
# Two time periods: 
# If you're about to get diagnosed with MCI is there acceleration as compared to the normal aging process?
# How long before you get diagnosed do changes happen that are noticible as compared to the aging process?
# Looking at different information for each outcome.

# Age is in partial years
# Average age range should be 65-85 age of entry, followed for 7-8 years

# Adjust for SES and gender
# Adding correlation is secondary to getting main effects model correct





