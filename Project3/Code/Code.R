library(ggplot2)
library(tableone)
library(nlme)

# Read in data exactly as it is given to us
dat <- read.table("~/Documents/CU Denver/Fall_2017/Advanced_Data_Analysis/Project_3/Project3Data.csv",sep=",",header = T)

# Individual must have >=3 measurements for animal outcome to be included

# Individuals with enough data for outcome 3:
freq.animals <- as.data.frame(table(dat$id[which(!is.na(dat$animals))]))
ind.animals <- as.numeric(as.vector(freq.animals[which(freq.animals$Freq >=3),1]))
animals.pop <- dat[which(dat$id%in%ind.animals),!colnames(dat)%in%c("cdr","logmemI","logmemII","blockR")]
# Only keep the observations where you have an outcome
animals.pop_CC <- animals.pop[!is.na(animals.pop$animals),]

# Function to search for change point (adapted from Camille)
cp.search_and_fit <- function(y, dataSet, cps = seq(0,6,.08333)){
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
  
  # Find the max
  cp<-ll[which(ll$ll==max(ll$ll)),'changepoint']
  print(cp)
  
  # Plot the likelihood
  plot(ll$changepoint, ll$ll, type='l', xlab='Change Point (months)', ylab='Log Likelihood',
       main = paste("Change point = ",cp,sep=""))
  abline(v = cp, untf = FALSE, col= "red")

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
  
  # Find the confidence interval around the changepoint
  maxll <- max(ll$ll)
  range <- which(ll$ll>=(max(ll$ll)-(3.84/2)))
  lower_bound <- ll$changepoint[min(range)]
  upper_bound <- ll$changepoint[max(range)]
  
  # Fit the final model
  tau <- ifelse(demind==1 & (age>ageonset-cp), age - ageonset + cp, 0)
  cp.model <- lme(y ~ age + demind + age*demind + tau + gender + SES, random=~1|id, method='ML')
  return(list(cp=cp,cp.ci=c(lower_bound,upper_bound), model=cp.model,ll.table=ll,adjusted.se = adj.se))
}

# Run the function on the dataset
cp.model.animals <- cp.search_and_fit(y = animals.pop_CC$animals, dataSet = animals.pop_CC)

# Calculate the t-statistic: beta/se(be)
beta.est <- summary(cp.model.animals$model)$tTable[,1]
stand.err <- cp.model.animals$adjusted.se
t.val <- beta.est/stand.err

# Slope of dementia before change point
# Figure out if age is sig(with dementia)(before change): age+demind+ageXdemind
slope.before <- beta.est["age"]+beta.est["age:demind"]

# Slope of dementia after change point
# Figure out if age is sig(with dementia)(after change): age+demind+tau+ageXdemind
slope.after <- beta.est["age"]+beta.est["age:demind"]+beta.est["tau"]

# Confidence Intervals
ci.low <- beta.est - 1.96*(stand.err)
ci.high <- beta.est + 1.96*(stand.err)

# Plot
animals.pop_CC$predicted <- as.data.frame(cp.model.animals$model$fitted)$id
ggplot(data = animals.pop_CC, aes(x=age, y = predicted, group = id, col = factor(demind))) +  geom_line()

# Time to diagnosis
ggplot(data = animals.pop_CC, aes(x=age-ageonset, y = predicted, group = id, col = factor(demind))) +  geom_line() + 
  labs(x = "Time to diagnosis")
 




