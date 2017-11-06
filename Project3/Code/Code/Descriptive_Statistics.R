dat <- read.table("~/Documents/CU Denver/Fall_2017/Advanced_Data_Analysis/Project_3/Project3Data.csv",sep=",",header = T)

# Create an outcome dataset for each outcome of interest, must have >=3 measurements per subject to be included
# Outcomes of interest: 1. logmemI 2. logmemII 3. animals 4. blockR

# Individuals with enough data for outcome 1:
freq.logmemI <- as.data.frame(table(dat$id[which(!is.na(dat$logmemI))]))
ind.logmemI <- as.numeric(as.vector(freq.logmemI[which(freq.logmemI$Freq >=3),1]))
logmemI.pop <- dat[which(dat$id%in%ind.logmemI),]

# Individuals with enough data for outcome 2:
freq.logmemII <- as.data.frame(table(dat$id[which(!is.na(dat$logmemII))]))
ind.logmemII <- as.numeric(as.vector(freq.logmemII[which(freq.logmemII$Freq >=3),1]))
logmemII.pop <- dat[which(dat$id%in%ind.logmemII),]

# Individuals with enough data for outcome 3:
freq.animal <- as.data.frame(table(dat$id[which(!is.na(dat$animal))]))
ind.animal <- as.numeric(as.vector(freq.animal[which(freq.animal$Freq >=3),1]))
animal.pop <- dat[which(dat$id%in%ind.animal),]

# Individuals with enough data for outcome 4:
freq.blockR <- as.data.frame(table(dat$id[which(!is.na(dat$blockR))]))
ind.blockR <- as.numeric(as.vector(freq.blockR[which(freq.blockR$Freq >=3),1]))
blockR.pop <- dat[which(dat$id%in%ind.blockR),]

## By Wednesday:
## Plot trajectories of measurements per subject

## Investigator questions:
#  A trajectory of onset

# Clinically: What's happening with people's memory measures as they age?
# Two time periods: 
# If you're about to get diagnosed with MCI is there acceleration as compared to the normal aging process?
# How long before you get diagnosed do changes happen that are noticible as compared to the aging process?
# Looking at different information for each outcome.

# Age is in partial years
# Average age range should be 65-85 age of entry, followed for 7-8 years


