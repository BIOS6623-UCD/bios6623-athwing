library(ggplot2)
library(gridExtra)
library(nlme)

dat <- read.table("~/Documents/CU Denver/Fall_2017/Advanced_Data_Analysis/Project_3/Project3Data.csv",sep=",",header = T)

# Create an outcome dataset for each outcome of interest, must have >=3 measurements per subject to be included
# Outcomes of interest: 1. logmemI 2. logmemII 3. animals 4. blockR

# Individuals with enough data for outcome 1:
freq.logmemI <- as.data.frame(table(dat$id[which(!is.na(dat$logmemI))]))
ind.logmemI <- as.numeric(as.vector(freq.logmemI[which(freq.logmemI$Freq >=3),1]))
logmemI.pop <- dat[which(dat$id%in%ind.logmemI),!colnames(dat)%in%c("cdr","logmemII","animals","blockR")]
# Plot trajectories per person for logmemI
logmemI.pop_CC <- logmemI.pop[!is.na(logmemI.pop$logmemI),]

# Individuals with enough data for outcome 2:
freq.logmemII <- as.data.frame(table(dat$id[which(!is.na(dat$logmemII))]))
ind.logmemII <- as.numeric(as.vector(freq.logmemII[which(freq.logmemII$Freq >=3),1]))
logmemII.pop <- dat[which(dat$id%in%ind.logmemII),!colnames(dat)%in%c("cdr","logmemI","animals","blockR")]
# Plot trajectories per person for logmemII
logmemII.pop_CC <- logmemII.pop[!is.na(logmemII.pop$logmemII),]

# Individuals with enough data for outcome 3:
freq.animals <- as.data.frame(table(dat$id[which(!is.na(dat$animals))]))
ind.animals <- as.numeric(as.vector(freq.animals[which(freq.animals$Freq >=3),1]))
animals.pop <- dat[which(dat$id%in%ind.animals),!colnames(dat)%in%c("cdr","logmemI","logmemII","blockR")]
# Plot trajectories per person for animals
animals.pop_CC <- animals.pop[!is.na(animals.pop$animals),]

# Individuals with enough data for outcome 4:
freq.blockR <- as.data.frame(table(dat$id[which(!is.na(dat$blockR))]))
ind.blockR <- as.numeric(as.vector(freq.blockR[which(freq.blockR$Freq >=3),1]))
blockR.pop <- dat[which(dat$id%in%ind.blockR),!colnames(dat)%in%c("cdr","logmemI","logmemII","animals")]
# Plot trajectories per person for blockR
blockR.pop_CC <- blockR.pop[!is.na(blockR.pop$blockR),]

# make demind a factor

# plotlI <- ggplot(data = logmemI.pop_CC, aes(x = age, y = logmemI, group = id,col = demind)) + geom_line() + theme(legend.position='none')
# plotlII <- ggplot(data = logmemII.pop_CC, aes(x = age, y = logmemII, group = id,col = demind)) + geom_line() + theme(legend.position='none')
# plotan <- ggplot(data = animals.pop_CC, aes(x = age, y = animals, group = id,col = demind)) + geom_line() + theme(legend.position='none')
# plotbr <-ggplot(data = blockR.pop_CC, aes(x = age, y = blockR, group = id,col = demind)) + geom_line() 

# grid.arrange(plotlI, plotlII, plotan, plotbr, nrow =2, ncol=2)

# Descriptive Statistics by outcome
plot(logmemI.pop$age,logmemI.pop$logmemI)
hist(logmemI.pop$logmemI)

plot(logmemII.pop$age,logmemII.pop$logmemII)
hist(logmemII.pop$logmemII)

plot(animals.pop$age,animals.pop$animals)
hist(animals.pop$animals)

plot(blockR.pop$age,blockR.pop$blockR)
hist(blockR.pop$blockR)

### Missing Data
summary(logmemI.pop)
summary(logmemII.pop)
summary(animals.pop)
summary(blockR.pop)

## How many people end up in each analysis
length(unique(logmemI.pop_CC$id))
length(unique(logmemII.pop_CC$id))
length(unique(animals.pop_CC$id))
length(unique(blockR.pop_CC$id))

# How many observations per person on average for each measure (Use complete cases)
mean(table(logmemI.pop_CC$id))
mean(table(logmemII.pop_CC$id))
mean(table(animals.pop_CC$id))
mean(table(blockR.pop_CC$id))

# Range in number of observations per person (Use complete cases)
max(table(logmemI.pop_CC$id))
min(table(logmemI.pop_CC$id))

max(table(logmemII.pop_CC$id))
min(table(logmemII.pop_CC$id))

max(table(animals.pop_CC$id))
min(table(animals.pop_CC$id))

max(table(blockR.pop_CC$id))
min(table(blockR.pop_CC$id))

# How long were people followed on average?
# Function to find years followed from a dataframe based on ids
yrs_followed <- function(df){
  ppl <- unique(df$id)
  min.age <- c();max.age <- c()
  for(i in 1:length(ppl)){
    min.age <- c(min.age,min(df$age[df$id==ppl[i]]))
    max.age <- c(max.age,max(df$age[df$id==ppl[i]]))
  }
  res <- list(min.age,max.age)
  names(res) <- c("min","max")
  res$diff <- res$max-res$min
  return(res)
}

# Complete cases
lmI.range_CC <- yrs_followed(logmemI.pop_CC)
mean(lmI.range_CC$diff)
lmII.range_CC <- yrs_followed(logmemII.pop_CC)
mean(lmII.range_CC$diff)
animals.range_CC <- yrs_followed(animals.pop_CC)
mean(animals.range_CC$diff)
blockR.range_CC <- yrs_followed(blockR.pop_CC)
mean(blockR.range_CC$diff)

# Non-complete cases
lmI.range_NC <- yrs_followed(logmemI.pop)
mean(lmI.range_NC$diff)
lmII.range_NC <- yrs_followed(logmemII.pop)
mean(lmII.range_NC$diff)
animals.range_NC <- yrs_followed(animals.pop)
mean(animals.range_NC$diff)
blockR.range_NC <- yrs_followed(blockR.pop)
mean(blockR.range_NC$diff)

# I'd like to know the average baseline values for the total population and split by dementia group.

# It might be nice to also know how many years after baseline dementia is being diagnosed to give an idea of how much data is available prior to the change point.




## I'd have to know more about SES from you to know if it should be categorized. Past analysis has used it continuously. Are there groups in the data?

 








# Table 1 stratified by demind

# variable is x-k (age is x, k is knot(negative) or 0, set age of onset for not as something huge)

## Difficulty is finding k

## Investigator questions:
#  A trajectory of onset

# Clinically: What's happening with people's memory measures as they age?
# Two time periods: 
# If you're about to get diagnosed with MCI is there acceleration as compared to the normal aging process?
# How long before you get diagnosed do changes happen that are noticible as compared to the aging process?
# Looking at different information for each outcome.

# Age is in partial years
# Average age range should be 65-85 age of entry, followed for 7-8 years

# How long they were followed for (time between first and last observation)
# # of subjects
# avg # of obs on each subject, SD
# avg time between measurements
# Avg characteristics of when you first saw people (avg values on 1st observation)

# Adjust for SES and gender
# Adding correlation is secondary to getting main effects model correct

# Slope changing around 4 years (before diagnosis?)


# Start fitting my models:












