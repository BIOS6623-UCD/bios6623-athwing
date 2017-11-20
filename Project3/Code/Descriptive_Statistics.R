library(ggplot2);library(gridExtra);library(nlme);library(tableone)

# Read in data exactly as it is given to us
dat <- read.table("~/Documents/CU Denver/Fall_2017/Advanced_Data_Analysis/Project_3/Project3Data.csv",sep=",",header = T)

# Create an outcome dataset for each outcome of interest, must have >=3 measurements per subject to be included
# Outcomes of interest: 1. logmemI 2. logmemII 3. animals 4. blockR

# Individuals with enough data for outcome 1:
freq.logmemI <- as.data.frame(table(dat$id[which(!is.na(dat$logmemI))]))
ind.logmemI <- as.numeric(as.vector(freq.logmemI[which(freq.logmemI$Freq >=3),1]))
logmemI.pop <- dat[which(dat$id%in%ind.logmemI),!colnames(dat)%in%c("cdr","logmemII","animals","blockR")]
# Create a complete cases dataset, of only the complete cases of logmemI
logmemI.pop_CC <- logmemI.pop[!is.na(logmemI.pop$logmemI),]

# Individuals with enough data for outcome 2:
freq.logmemII <- as.data.frame(table(dat$id[which(!is.na(dat$logmemII))]))
ind.logmemII <- as.numeric(as.vector(freq.logmemII[which(freq.logmemII$Freq >=3),1]))
logmemII.pop <- dat[which(dat$id%in%ind.logmemII),!colnames(dat)%in%c("cdr","logmemI","animals","blockR")]
# Create a complete cases dataset, of only the complete cases
logmemII.pop_CC <- logmemII.pop[!is.na(logmemII.pop$logmemII),]

# Individuals with enough data for outcome 3:
freq.animals <- as.data.frame(table(dat$id[which(!is.na(dat$animals))]))
ind.animals <- as.numeric(as.vector(freq.animals[which(freq.animals$Freq >=3),1]))
animals.pop <- dat[which(dat$id%in%ind.animals),!colnames(dat)%in%c("cdr","logmemI","logmemII","blockR")]
# Create a complete cases dataset, of only the complete cases
animals.pop_CC <- animals.pop[!is.na(animals.pop$animals),]

# Individuals with enough data for outcome 4:
freq.blockR <- as.data.frame(table(dat$id[which(!is.na(dat$blockR))]))
ind.blockR <- as.numeric(as.vector(freq.blockR[which(freq.blockR$Freq >=3),1]))
blockR.pop <- dat[which(dat$id%in%ind.blockR),!colnames(dat)%in%c("cdr","logmemI","logmemII","animals")]
# Create a complete cases dataset, of only the complete cases
blockR.pop_CC <- blockR.pop[!is.na(blockR.pop$blockR),]

# Descriptive Statistics (table one)
# Remove people removed from all four datasets
# People in logmemI are the same as in logmemII, don't need to do union of that
all_included <- union(ind.logmemI,ind.animals)
all_included <- union(all_included,ind.blockR)
tbOneDat <- dat[dat$id%in%all_included,]
# Keep only baseline values of each person
tbOneDat <- tbOneDat[match(unique(tbOneDat$id),tbOneDat$id),]

# Make a table one of average values at baseline, for the total population and split by dementia group
tblOne <- CreateTableOne(vars=c("gender","SES","age","blockR","animals","logmemI","logmemII"), data = tbOneDat, factorVars = c("gender"))
print(tblOne)
summary(tblOne)
# Avg characteristics split by dementia group
tblOneStratified <- CreateTableOne(vars=c("gender","SES","age","blockR","animals","logmemI","logmemII"), strata = "demind", data = tbOneDat, factorVars = c("gender"))
print(tblOneStratified)
summary(tblOneStratified)

# Calculate length of following/age descriptive statistics
# Interested in: How long they were followed for, avg # of obs for each subject, avg time between measurements

# How many people end up in each analysis
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

yrs_followed_all <- yrs_followed(dat[dat$id%in%all_included,])
mean(yrs_followed_all$min)
mean(yrs_followed_all$max)
mean(yrs_followed_all$diff)

# Complete cases
lmI.range_CC <- yrs_followed(logmemI.pop_CC)
mean(lmI.range_CC$diff)
lmII.range_CC <- yrs_followed(logmemII.pop_CC)
mean(lmII.range_CC$diff)
animals.range_CC <- yrs_followed(animals.pop_CC)
mean(animals.range_CC$diff)
blockR.range_CC <- yrs_followed(blockR.pop_CC)
mean(blockR.range_CC$diff)




# Spaghetti plots of the trajectory of the outcome, color coded by dementia group, ***make x-axis time to diagnosis***

plotlI <- ggplot(data = logmemI.pop_CC, aes(x = age, y = logmemI, group = id,col = as.factor(demind))) + geom_line() + theme(legend.position='none')
plotlII <- ggplot(data = logmemII.pop_CC, aes(x = age, y = logmemII, group = id,col = as.factor(demind))) + geom_line() + theme(legend.position='none')
plotan <- ggplot(data = animals.pop_CC, aes(x = age, y = animals, group = id,col = as.factor(demind))) + geom_line() + theme(legend.position='none')
plotbr <-ggplot(data = blockR.pop_CC, aes(x = age, y = blockR, group = id,col = as.factor(demind))) + geom_line() + theme(legend.position='none') 

grid.arrange(plotlI, plotlII, plotan, plotbr, nrow =2, ncol=2)

### Missing Data
summary(logmemI.pop)
summary(logmemII.pop)
summary(animals.pop)
summary(blockR.pop)

# Find avg values at baseline for each outcome (a little different for animal group)
lmI.first <- logmemI.pop_CC[match(unique(logmemI.pop_CC$id),logmemI.pop_CC$id),]
mean(lmI.first$logmemI)
lmII.first <- logmemII.pop_CC[match(unique(logmemII.pop_CC$id),logmemII.pop_CC$id),]
mean(lmII.first$logmemII)
animals.first <- animals.pop_CC[match(unique(animals.pop_CC$id),animals.pop_CC$id),]
mean(animals.first$animals)
blockR.first <- blockR.pop_CC[match(unique(blockR.pop_CC$id),blockR.pop_CC$id),]
mean(blockR.first$blockR)


# It might be nice to also know how many years after baseline, dementia is being diagnosed to give an idea of how much data is available prior to the change point.




## I'd have to know more about SES from you to know if it should be categorized. Past analysis has used it continuously. Are there groups in the data?

 



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

# Adjust for SES and gender
# Adding correlation is secondary to getting main effects model correct


# Start fitting my models and find changepoint (in Code.R)

