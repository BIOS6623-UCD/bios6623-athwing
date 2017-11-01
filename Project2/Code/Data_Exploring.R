# This code is not needed to replicate results but rather was used to explore the data, create plots and table ones

setwd("~/Documents/CU Denver/Fall_2017/Advanced_Data_Analysis/Project_2")
library(sas7bdat)

dat <- read.sas7bdat("vadata2.sas7bdat")
full_dat <- dat[,c("hospcode","sixmonth","asa","proced","bmi","albumin","death30")]
album.group <- dat[which(!is.na(dat$albumin)),]

dat[which(dat$asa==2),] <- NA
dat$weight[which(dat$hospcode<17 & dat$sixmonth==39)] <- dat$weight[which(dat$hospcode<17 & dat$sixmonth==39)]*2.2
dat$bmi <- 703*(dat$weight/((dat$height)^2))

# Check to see whether including albumin gave sig different covariates
no.al <- glm(death30 ~ asa + proced + bmi, data = album.group, family = binomial())
yes.al <- glm(death30 ~ asa + proced + bmi + albumin, data = album.group, family = binomial())

dat <- dat[,c("hospcode","sixmonth","asa","proced","bmi","death30")]
dat <- na.omit(dat)

## Load VIM package for Visualization and Imputation of Missing Values
library(VIM)
aggr(full_dat, prop = F, numbers = T,combined=F,sortVars=T)

## Create data frame indicating missingness by 1
x <- as.data.frame(abs(is.na(dat)))
## Select columns with some (but not all) missing values
y <- x[,sapply(x, sd) > 0]
## Create a correlation matrix: Variables missing together have high correlation
cor(y)


# Final probs
final <- read.table("~/Documents/CU Denver/Fall_2017/Advanced_Data_Analysis/Project_2/final.txt",header = T)

plot(seq(1:44),final$observed,xlab="Hospitals",ylab="Death Rate",main="Expected vs Observed Death Rate",col=2,pch=1)
points(seq(1:44),final$expected,col=1,pch=1)
legend(0,.135,c("Obs","Exp"),col=c(2,1),pch=c(1,1))


# Create a table one
# TABLE ONE: VA population as a whole
# Summary statistics of avg bmi across hospitals, variation
# Either mean(sd) or median(25/75)
dat <- na.omit(dat)
bmi.means <- tapply(dat$bmi, dat$hospcode, mean,simplify = T)
mean(bmi.means)
sd(bmi.means)
table(dat$asa)
table(dat$proced)
table(dat$death30)

dat <- dat[which(dat$sixmonth==39),]

library(tableone)

t1<-CreateTableOne(vars = c("asa","proced","bmi","death30"),strata = "death30", data = dat,
               factorVars = c("asa","proced","death30"))
print(t1,showAllLevels = T)

# Observed death rate for hospital 30
observed.total <- tapply(dat$death30, dat$hospcode, mean,simplify = T)
observed.39 <- tapply(dat$death30[which(dat$sixmonth==39)], dat$hospcode[which(dat$sixmonth==39)], mean,simplify = T)
cbind(observed.total,observed.39,final$observed)[27:33,]


# Look at distribution of missing data variables across the outcomes
bmi_missing <- sum(dat$death30[which(is.na(dat$bmi))])/length(dat$death30[which(is.na(dat$bmi))])
bmi_not_missing <- sum(dat$death30[which(!is.na(dat$bmi))])/length(dat$death30[which(!is.na(dat$bmi))])

proced_missing <- sum(dat$death30[which(is.na(dat$proced))])/length(dat$death30[which(is.na(dat$proced))])
proced_not_missing <- sum(dat$death30[which(!is.na(dat$proced))])/length(dat$death30[which(!is.na(dat$proced))])

asa_missing <- sum(dat$death30[which(is.na(dat$asa))])/length(dat$death30[which(is.na(dat$asa))])
asa_not_missing <- sum(dat$death30[which(!is.na(dat$asa))])/length(dat$death30[which(!is.na(dat$asa))])

missing <- rbind(cbind(bmi_missing,bmi_not_missing),cbind(proced_missing,proced_not_missing),cbind(asa_missing,asa_not_missing))
colnames(missing) <- c("Missing","Not Missing")
rownames(missing) <- c("BMI","Procedure","ASA")
missing
