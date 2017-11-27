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

# Descriptive Statistics (table one)

# Table one is values at the first time the person has an outcome
tableOneData <- animals.pop_CC[match(unique(animals.pop_CC$id),animals.pop_CC$id),]

# Make a table one of average values at baseline, for the total population and split by dementia group
table_one <- CreateTableOne(vars=c("gender","SES","age","animals"), data = tableOneData, factorVars = c("gender"))
print(table_one,showAllLevels = T)
# Avg characteristics split by dementia group
table_one_strat <- CreateTableOne(vars=c("gender","SES","age","animals"), strata = "demind", data = tableOneData, factorVars = c("gender"))
print(table_one_strat,showAllLevels = T)

# Interested in: How long they were followed for, avg # of obs for each subject, avg time between measurements
# How many people end up in the analysis
length(unique(animals.pop_CC$id))

# How many observations per person on average (Use complete cases)
mean(table(animals.pop_CC$id))

# Range in number of observations per person (Use complete cases)
max(table(animals.pop_CC$id))
min(table(animals.pop_CC$id))

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

# Look at range of ages in study
yrs_followed_all <- yrs_followed(animals.pop)
mean(yrs_followed_all$min)
mean(yrs_followed_all$max)
mean(yrs_followed_all$diff)

# Range of ages with observed outcome in study
yrs_followed_CC <- yrs_followed(animals.pop_CC)
mean(yrs_followed_CC$min)
mean(yrs_followed_CC$max)
mean(yrs_followed_CC$diff)

# Spaghetti plot of the trajectory of the outcome, color coded by dementia group, ***make x-axis time to diagnosis***
plot_animals <- ggplot(data = animals.pop_CC, aes(x = age, y = animals, group = id,col = as.factor(demind))) + geom_line()
plot_animals

# Plot of the trajectory of the outcome, x axis is time to diagnosis (only dementia group)
plot_ttd <- ggplot(data = animals.pop_CC[which(animals.pop_CC$demind==1),], aes(x = age-ageonset, y = animals, group = id)) + geom_line() + labs(x = "Time to diagnosis")
plot_ttd

# Missing Data
summary(animals.pop)


# Start fitting my models and find changepoint (in Code.R)

