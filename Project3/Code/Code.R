# Function to search for change point (adapted from Camille)

cp.search_and_fit <- function(y, dataSet, cps = seq(0,10,.1)){
  id <- dataSet$id
  age <- dataSet$age
  ageonset <- dataSet$ageonset
  demind <- dataSet$demind
  SES <- dataSet$SES
  gender <- dataSet$gender
  
  # Place to store likelihoods from the CP search
  ll <-data.frame (changepoint=rep(NA,length(cps)), ll=rep(NA,length(cps)))
  # Place to store estimates and standard errors from each CP model
  est.table <- data.frame(intercept=rep(NA,length(cps)),age=rep(NA,length(cps)),demind=rep(NA,length(cps)),tau=rep(NA,length(cps)),
                          gender=rep(NA,length(cps)),SES=rep(NA,length(cps)),agexdem=rep(NA,length(cps)))
  se.table <- data.frame(intercept=rep(NA,length(cps)),age=rep(NA,length(cps)),demind=rep(NA,length(cps)),tau=rep(NA,length(cps)),
                         gender=rep(NA,length(cps)),SES=rep(NA,length(cps)),agexdem=rep(NA,length(cps)))
  
  # Search for the CP
  for (i in 1:length(cps)){
    cp <- cps[i]
    # tau is either time after changepoint or 0 if before changepoint, should stay 0 where demind = 0
    tau <- ifelse(demind==1 & (age>ageonset-cp), age - ageonset + cp, 0)
    cp.model <- lme(y ~ age + demind + age*demind + tau + gender + SES, random=~1|id, method='ML')
    ll[i,] <- c(cp,logLik(cp.model))
    est.table[i,] <- summary(cp.model)$tTable[,1]
    se.table[i,] <- summary(cp.model)$tTable[,2]
  }
  
  # Plot the likelihood
  plot(ll$changepoint, ll$ll, type='l', xlab='Change Point (months)', ylab='Log Likelihood')
  
  # Find the max
  cp<-ll[which(ll$ll==max(ll$ll)),'changepoint']
  print(cp)
  
  # Compute the adjusted standard errors for each estimate
  # Place to store values for first component of Var for each coefficient
  var1.adjusted <- data.frame(intercept=rep(NA,length(cps)),age=rep(NA,length(cps)),demind=rep(NA,length(cps)),tau=rep(NA,length(cps)),
                         gender=rep(NA,length(cps)),SES=rep(NA,length(cps)),agexdem=rep(NA,length(cps)))
  # Place to store values for second component of Var for each coefficient
  var2.adjusted <- data.frame(intercept=rep(NA,length(cps)),age=rep(NA,length(cps)),demind=rep(NA,length(cps)),tau=rep(NA,length(cps)),
                              gender=rep(NA,length(cps)),SES=rep(NA,length(cps)),agexdem=rep(NA,length(cps)))
  # Compute the sum of all of the likelihoods, store as sumlogLik
  sumlogLik <- sum(ll$ll)
  for(i in 1:length(cps)){
    # Store the loglikelihood*(standard error^2 of each tau) as var1
    var1.adjusted[i,] <- (se.table[i,]**2)*ll$ll[i]
    # Store the loglikelihood*(beta-beta.hat) for each tau, where beta.hat is mean of all betas
    var2.adjusted[i,] <- (est.table[i,]-colMeans(est.table))^2*ll$ll[i]
  }
  # Find the first part of the variance equation
  # This is equivalent to sum(likelihood|tau*var|tau)/sum(likelihoods)
  v1 <- colSums(var1.adjusted)/sumlogLik
  # Find the second part of the variance equation
  # This is equivalent to sum(likelihood|tau*(beta|tau-beta.hat))/sum(likelihoods)
  v2 <- colSums(var2.adjusted)/sumlogLik
  adj.se <- sqrt(v1+v2)
  
  # Fit the final model
  tau <- ifelse(demind==1 & (age>ageonset-cp), age - ageonset + cp, 0)
  cp.model <- lme(y ~ age + demind + age*demind + tau + gender + SES, random=~1|id, method='ML')
  return(list(cp=cp, model=cp.model,ll.table=ll,adjusted.se = adj.se))
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





