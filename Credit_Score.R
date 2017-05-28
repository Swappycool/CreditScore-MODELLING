install.packages("Information")
install.packages("varhandle")
install.packages("pROC")
install.packages("broom")

library(dplyr)
library(ggplot2)
library(Hmisc)
library(pastecs)
library(moments)
library(stats)
library(MASS)
library(GGally)
library(Information)
library(plotly)
library(varhandle)
library(woe)
library(Causata)
library(caret)
library(caTools)
library(dummies)
library(MASS)
library(car)
library(ROCR)
library(randomForest)
library(pROC)
library(ggthemes)
library(grid)
library(broom)
library(gridExtra)
library(data.table)

setwd("C:/Users/hp-g6/Documents/Capstone Project")
credit_bureau_data<-read.csv("Credit Bureau Data.csv")
View(credit_bureau_data)
str(credit_bureau_data)
summary(credit_bureau_data)

demographics_data<-read.csv("Demographic Data.csv")
View(demographics_data)
str(demographics_data)
summary(demographics_data)

### MISSING VALUE IDENTIFICATION

sapply(credit_bureau_data, function(x) sum(is.na(x)))
sapply(demographics_data, function(x) sum(is.na(x)))

### Removal of Duplicate observations of Application ID

demographics_unique<-demographics_data[!duplicated(demographics_data$Application.ID),]
cb_data_unique<-credit_bureau_data[!duplicated(credit_bureau_data$Application.ID),]

### MERGING the datasets containing unique Application ID and Performance Tag, into a 
### master file, for further analysis

master_file<-merge(demographics_unique, cb_data_unique, by= c("Application.ID", "Performance.Tag"))


### Again, Missing value Identification in the master file

sapply(master_file, function(x) sum(is.na(x)))


# The applicants having their Performance Tags missing in the main dataset (valid cases, as they are 
# based on the Credit bureau information) have been considered as the REJECTED candidates, for further analysis.

master_file_rejected<-master_file[which(is.na(master_file$Performance.Tag)==TRUE),]

# Since, there are missing values in the TARGET VARIABLE "PERFORMANCE TAG" , which are valid cases,
# as they are based on the Credit bureau information, hence removal of these observations is required,
# before moving onto further analysis. The no.of such missing observations are 1425, and hence can be
# removed(as they constitute a minimal part of the data). Following is the master_file, after removal,

master_file<- master_file[which(is.na(master_file$Performance.Tag)==FALSE),]


## Checking the no.of observations and attributes in the above prepared master_file

observations <- nrow(master_file) # 69867 observations
attributes <- ncol(master_file)   # 29 attributes


####________________________________UNIVARIATE ANALYSIS_________________________________________####

###---------------------------------For Continuous Variables-----------------------------------###

## For analysis of "Age" of the Applicant

# Histogram plot 
ggplot(master_file, aes(x=Age))+geom_histogram(fill="Red")+ ggtitle("Histogram of Age")+ xlab("Age") + ylab("No. of Customers") 


ggplot(master_file, aes(x=Age))+geom_histogram(aes(y=..density..),colour="black", fill="white")+geom_density(alpha=.2, fill="#FF6666")+ggtitle("Distribution of Age")+ xlab("Age") + ylab("Density") 

# Summary (mean=median=45)
summary(master_file$Age)

# Box plot & boxplot stats (20 outliers detected)
ggplot(master_file, aes(x=1, y=Age)) + geom_boxplot(outlier.colour = "Blue")  + xlab("Width of boxplot") + ylab("Age") + ggtitle("Age box-plot")
boxplot.stats(master_file$Age)

describe(master_file$Age)
stat.desc(master_file$Age)

# Stastical Information
IQR(master_file$Age, na.rm = TRUE)        # IQR =16
var(master_file$Age, na.rm = TRUE)        # variance =98.61
sd(master_file$Age, na.rm = TRUE)         # std. deviation = 9.93
skewness(master_file$Age, na.rm = TRUE)   # skewness = -0.0135
kurtosis(master_file$Age, na.rm = TRUE)   # kurtosis = 2.31


## For analysis of "Income" of the Applicant

# Histogram plot 
ggplot(master_file, aes(x=Income))+geom_histogram(fill="Red")+ ggtitle("Histogram of Income")+ xlab("Income") + ylab("No. of Customers") 

ggplot(master_file, aes(x=Income))+geom_histogram(aes(y=..density..),colour="black", fill="white")+geom_density(alpha=.2, fill="#FF6666")+ggtitle("Distribution of Income")+ xlab("Income") + ylab("Density") 

summary(master_file$Income)

ggplot(master_file, aes(x=1, y=Income)) + geom_boxplot(outlier.colour = "Blue")  + xlab("Width of boxplot") + ylab("Income") + ggtitle("Income box-plot")
boxplot.stats(master_file$Income)

describe(master_file$Income)
stat.desc(master_file$Income)

IQR(master_file$Income, na.rm = TRUE)
var(master_file$Income, na.rm = TRUE)
sd(master_file$Income, na.rm = TRUE)
skewness(master_file$Income, na.rm = TRUE)
kurtosis(master_file$Income, na.rm = TRUE)

# For the dependents of the applicant

ggplot(master_file, aes(x=No.of.dependents))+geom_histogram(fill="Red")+ ggtitle("Histogram of No.of.dependents")+ xlab("No.of.dependents") + ylab("No. of Customers") 

ggplot(master_file, aes(x=No.of.dependents))+geom_histogram(aes(y=..density..),colour="black", fill="white",bins=5)+geom_density(alpha=.2, fill="#FF6666")+ggtitle("Distribution of No.of.dependents")+ xlab("No.of.dependents") + ylab("Density") 

summary(master_file$No.of.dependents)


ggplot(master_file, aes(x=1, y=No.of.dependents)) + geom_boxplot(outlier.colour = "Blue")  + xlab("Width of boxplot") + ylab("No.of.dependents") + ggtitle("No.of.dependents box-plot")
boxplot.stats(master_file$No.of.dependents)

describe(master_file$No.of.dependents)
stat.desc(master_file$No.of.dependents)

IQR(master_file$No.of.dependents, na.rm = TRUE)
var(master_file$No.of.dependents, na.rm = TRUE)
sd(master_file$No.of.dependents, na.rm = TRUE)
skewness(master_file$No.of.dependents,na.rm = TRUE)
kurtosis(master_file$No.of.dependents,na.rm = TRUE)

# For the analysis of variable "No. of months in a current Company" of the applicant

ggplot(master_file, aes(x=No.of.months.in.current.company))+geom_histogram(fill="Red")+ ggtitle("Histogram of No.of.months.in.current.company")+ xlab("No.of.months.in.current.company") + ylab("No. of Customers") 

ggplot(master_file, aes(x=No.of.months.in.current.company))+geom_histogram(aes(y=..density..),colour="black", fill="white")+geom_density(alpha=.2, fill="#FF6666")+ggtitle("Distribution of No.of.months.in.current.company")+ xlab("No.of.months.in.current.company") + ylab("Density") 

summary(master_file$No.of.months.in.current.company)

ggplot(master_file, aes(x=1, y=No.of.months.in.current.company)) + geom_boxplot(outlier.colour = "Blue")  + xlab("Width of boxplot") + ylab("No.of.months.in.current.company") + ggtitle("No.of.months.in.current.company box-plot")
boxplot.stats(master_file$No.of.months.in.current.company)
quantile(master_file$No.of.months.in.current.company, probs=seq(0, 1,length = 101))

describe(master_file$No.of.months.in.current.company)
stat.desc(master_file$No.of.months.in.current.company)

IQR(master_file$No.of.months.in.current.company, na.rm = TRUE)
var(master_file$No.of.months.in.current.company, na.rm = TRUE)
sd(master_file$No.of.months.in.current.company, na.rm = TRUE)
skewness(master_file$No.of.months.in.current.company)
kurtosis(master_file$No.of.months.in.current.company, na.rm = TRUE)

# For the variable analysis of "No. of months in a current residence" of the applicant

 
ggplot(master_file, aes(x=No.of.months.in.current.residence))+geom_histogram(aes(y=..density..),colour="black", fill="white")+geom_density(alpha=.2, fill="#FF6666")+ggtitle("Distribution of No.of.months.in.current.residence")+ xlab("No.of.months.in.current.residence") + ylab("Density") 

summary(master_file$No.of.months.in.current.residence)

ggplot(master_file, aes(x=1, y=No.of.months.in.current.residence)) + geom_boxplot(outlier.colour = "Blue")  + xlab("Width of boxplot") + ylab("No.of.months.in.current.company") + ggtitle("No.of.months.in.current.residence box-plot")
boxplot.stats(master_file$No.of.months.in.current.residence)
quantile(master_file$No.of.months.in.current.residence, probs=seq(0, 1,length = 101))

describe(master_file$No.of.months.in.current.residence)
stat.desc(master_file$No.of.months.in.current.residence)

IQR(master_file$No.of.months.in.current.residence, na.rm = TRUE)
var(master_file$No.of.months.in.current.residence, na.rm = TRUE)
sd(master_file$No.of.months.in.current.residence, na.rm = TRUE)
skewness(master_file$No.of.months.in.current.residence)
kurtosis(master_file$No.of.months.in.current.residence, na.rm = TRUE)

# For analyzing the customers who have been 90DPD or worse in last 6 months 

ggplot(master_file, aes(x=No.of.times.90.DPD.or.worse.in.last.6.months))+geom_histogram(fill="Red", bins=5)+ ggtitle("Histogram of No.of.times.90.DPD.or.worse.in.last.6.months")+ xlab("No.of.times.90.DPD.or.worse.in.last.6.months") + ylab("No. of Customers") 


summary(master_file$No.of.times.90.DPD.or.worse.in.last.6.months)

ggplot(master_file, aes(x=1, y=No.of.times.90.DPD.or.worse.in.last.6.months)) + geom_boxplot(outlier.colour = "Blue")  + xlab("Width of boxplot") + ylab("No.of.times.90.DPD.or.worse.in.last.6.months") + ggtitle("No.of.times.90.DPD.or.worse.in.last.6.months box-plot")
boxplot.stats(master_file$No.of.times.90.DPD.or.worse.in.last.6.months)
quantile(master_file$No.of.times.90.DPD.or.worse.in.last.6.months, probs=seq(0, 1,length = 101))

describe(master_file$No.of.times.90.DPD.or.worse.in.last.6.months)
stat.desc(master_file$No.of.times.90.DPD.or.worse.in.last.6.months)

IQR(master_file$No.of.times.90.DPD.or.worse.in.last.6.months, na.rm = TRUE)
var(master_file$No.of.times.90.DPD.or.worse.in.last.6.months, na.rm = TRUE)
sd(master_file$No.of.times.90.DPD.or.worse.in.last.6.months, na.rm = TRUE)
skewness(master_file$No.of.times.90.DPD.or.worse.in.last.6.months, na.rm = TRUE)
kurtosis(master_file$No.of.times.90.DPD.or.worse.in.last.6.months, na.rm = TRUE)

# For analyzing the customers who have been 60DPD or worse in last 6 months 

ggplot(master_file, aes(x=No.of.times.60.DPD.or.worse.in.last.6.months))+geom_histogram(fill="Red", bins=5)+ ggtitle("Histogram of No.of.times.60.DPD.or.worse.in.last.6.months")+ xlab("No.of.times.60.DPD.or.worse.in.last.6.months") + ylab("No. of Customers") 

summary(master_file$No.of.times.60.DPD.or.worse.in.last.6.months)

ggplot(master_file, aes(x=1, y=No.of.times.60.DPD.or.worse.in.last.6.months)) + geom_boxplot(outlier.colour = "Blue")  + xlab("Width of boxplot") + ylab("No.of.times.60.DPD.or.worse.in.last.6.months") + ggtitle("No.of.times.60.DPD.or.worse.in.last.6.months box-plot")
boxplot.stats(master_file$No.of.times.60.DPD.or.worse.in.last.6.months)
quantile(master_file$No.of.times.60.DPD.or.worse.in.last.6.months, probs=seq(0, 1,length = 101))

describe(master_file$No.of.times.60.DPD.or.worse.in.last.6.months)
stat.desc(master_file$No.of.times.60.DPD.or.worse.in.last.6.months)

IQR(master_file$No.of.times.60.DPD.or.worse.in.last.6.months, na.rm = TRUE)
var(master_file$No.of.times.60.DPD.or.worse.in.last.6.months, na.rm = TRUE)
sd(master_file$No.of.times.60.DPD.or.worse.in.last.6.months, na.rm = TRUE)
skewness(master_file$No.of.times.60.DPD.or.worse.in.last.6.months, na.rm = TRUE)
kurtosis(master_file$No.of.times.60.DPD.or.worse.in.last.6.months, na.rm = TRUE)

# For analyzing the customers who have been 30DPD or worse in last 6 months 

ggplot(master_file, aes(x=No.of.times.30.DPD.or.worse.in.last.6.months))+geom_histogram(fill="Red", bins = 5)+ ggtitle("Histogram of No.of.times.30.DPD.or.worse.in.last.6.months")+ xlab("No.of.times.30.DPD.or.worse.in.last.6.months") + ylab("No. of Customers") 

summary(master_file$No.of.times.30.DPD.or.worse.in.last.6.months)

ggplot(master_file, aes(x=1, y=No.of.times.30.DPD.or.worse.in.last.6.months)) + geom_boxplot(outlier.colour = "Blue")  + xlab("Width of boxplot") + ylab("No.of.times.30.DPD.or.worse.in.last.6.months") + ggtitle("No.of.times.30.DPD.or.worse.in.last.6.months")
boxplot.stats(master_file$No.of.times.30.DPD.or.worse.in.last.6.months)
quantile(master_file$No.of.times.30.DPD.or.worse.in.last.6.months, probs=seq(0, 1,length = 101))

describe(master_file$No.of.times.30.DPD.or.worse.in.last.6.months)
stat.desc(master_file$No.of.times.30.DPD.or.worse.in.last.6.months)

IQR(master_file$No.of.times.30.DPD.or.worse.in.last.6.months, na.rm = TRUE)
var(master_file$No.of.times.30.DPD.or.worse.in.last.6.months, na.rm = TRUE)
sd(master_file$No.of.times.30.DPD.or.worse.in.last.6.months, na.rm = TRUE)
skewness(master_file$No.of.times.30.DPD.or.worse.in.last.6.months, na.rm = TRUE)
kurtosis(master_file$No.of.times.30.DPD.or.worse.in.last.6.months, na.rm = TRUE)

# For analyzing the customers who have been 90DPD or worse in last 12 months 

ggplot(master_file, aes(x=No.of.times.90.DPD.or.worse.in.last.12.months))+geom_histogram(fill="Red",bins = 5)+ ggtitle("Histogram of No.of.times.90.DPD.or.worse.in.last.12.months")+ xlab("No.of.times.90.DPD.or.worse.in.last.12.months") + ylab("No. of Customers") 

summary(master_file$No.of.times.90.DPD.or.worse.in.last.12.months)

ggplot(master_file, aes(x=1, y=No.of.times.90.DPD.or.worse.in.last.12.months)) + geom_boxplot(outlier.colour = "Blue")  + xlab("Width of boxplot") + ylab("No.of.times.90.DPD.or.worse.in.last.12.months") + ggtitle("No.of.times.90.DPD.or.worse.in.last.12.months box-plot")
boxplot.stats(master_file$No.of.times.90.DPD.or.worse.in.last.12.months)
quantile(master_file$No.of.times.90.DPD.or.worse.in.last.12.months, probs=seq(0, 1,length = 101))

describe(master_file$No.of.times.90.DPD.or.worse.in.last.12.months)
stat.desc(master_file$No.of.times.90.DPD.or.worse.in.last.12.months)

IQR(master_file$No.of.times.90.DPD.or.worse.in.last.12.months, na.rm = TRUE)
var(master_file$No.of.times.90.DPD.or.worse.in.last.12.months, na.rm = TRUE)
sd(master_file$No.of.times.90.DPD.or.worse.in.last.12.months, na.rm = TRUE)
skewness(master_file$No.of.times.90.DPD.or.worse.in.last.12.months, na.rm = TRUE)
kurtosis(master_file$No.of.times.90.DPD.or.worse.in.last.12.months, na.rm = TRUE)


# For analyzing the customers who have been 60DPD or worse in last 12 months 

ggplot(master_file, aes(x=No.of.times.60.DPD.or.worse.in.last.12.months))+geom_histogram(fill="Red", bins = 5)+ ggtitle("Histogram of No.of.times.60.DPD.or.worse.in.last.12.months")+ xlab("No.of.times.60.DPD.or.worse.in.last.12.months") + ylab("No. of Customers") 

summary(master_file$No.of.times.60.DPD.or.worse.in.last.12.months)

ggplot(master_file, aes(x=1, y=No.of.times.60.DPD.or.worse.in.last.12.months)) + geom_boxplot(outlier.colour = "Blue")  + xlab("Width of boxplot") + ylab("No.of.times.60.DPD.or.worse.in.last.12.months") + ggtitle("No.of.times.60.DPD.or.worse.in.last.12.months box-plot")
boxplot.stats(master_file$No.of.times.60.DPD.or.worse.in.last.12.months)
quantile(master_file$No.of.times.60.DPD.or.worse.in.last.12.months, probs=seq(0, 1,length = 101))

describe(master_file$No.of.times.60.DPD.or.worse.in.last.12.months)
stat.desc(master_file$No.of.times.60.DPD.or.worse.in.last.12.months)

IQR(master_file$No.of.times.60.DPD.or.worse.in.last.12.months, na.rm = TRUE)
var(master_file$No.of.times.60.DPD.or.worse.in.last.12.months, na.rm = TRUE)
sd(master_file$No.of.times.60.DPD.or.worse.in.last.12.months, na.rm = TRUE)
skewness(master_file$No.of.times.60.DPD.or.worse.in.last.12.months, na.rm = TRUE)
kurtosis(master_file$No.of.times.60.DPD.or.worse.in.last.12.months, na.rm = TRUE)


# For analyzing the customers who have been 30DPD or worse in last 12 months 

ggplot(master_file, aes(x=No.of.times.30.DPD.or.worse.in.last.12.months))+geom_histogram(fill="Red",bins = 5)+ ggtitle("Histogram of No.of.times.30.DPD.or.worse.in.last.12.months")+ xlab("No.of.times.30.DPD.or.worse.in.last.12.months") + ylab("No. of Customers") 

summary(master_file$No.of.times.30.DPD.or.worse.in.last.12.months)

ggplot(master_file, aes(x=1, y=No.of.times.30.DPD.or.worse.in.last.12.months)) + geom_boxplot(outlier.colour = "Blue")  + xlab("Width of boxplot") + ylab("No.of.times.30.DPD.or.worse.in.last.12.months") + ggtitle("No.of.times.30.DPD.or.worse.in.last.12.months box-plot")
boxplot.stats(master_file$No.of.times.30.DPD.or.worse.in.last.12.months)
quantile(master_file$No.of.times.30.DPD.or.worse.in.last.12.months, probs=seq(0, 1,length = 101))

describe(master_file$No.of.times.30.DPD.or.worse.in.last.12.months)
stat.desc(master_file$No.of.times.30.DPD.or.worse.in.last.12.months)

IQR(master_file$No.of.times.30.DPD.or.worse.in.last.12.months, na.rm = TRUE)
var(master_file$No.of.times.30.DPD.or.worse.in.last.12.months, na.rm = TRUE)
sd(master_file$No.of.times.30.DPD.or.worse.in.last.12.months, na.rm = TRUE)
skewness(master_file$No.of.times.30.DPD.or.worse.in.last.12.months, na.rm = TRUE)
kurtosis(master_file$No.of.times.30.DPD.or.worse.in.last.12.months, na.rm = TRUE)


# For the analysis of variable representing aveg. Credit Card utilization of the Applicant

ggplot(master_file, aes(x=Avgas.CC.Utilization.in.last.12.months))+geom_histogram(fill="Red")+ ggtitle("Histogram of Avgas.CC.Utilization.in.last.12.months")+ xlab("Avgas.CC.Utilization.in.last.12.months") + ylab("No. of Customers") 

ggplot(master_file, aes(x=Avgas.CC.Utilization.in.last.12.months))+geom_histogram(aes(y=..density..),colour="black", fill="white")+geom_density(alpha=.2, fill="#FF6666")+ggtitle("Distribution of Avgas.CC.Utilization.in.last.12.months")+ xlab("Avgas.CC.Utilization.in.last.12.months") + ylab("Density") 

summary(master_file$Avgas.CC.Utilization.in.last.12.months)

ggplot(master_file, aes(x=1, y=Avgas.CC.Utilization.in.last.12.months)) + geom_boxplot(outlier.colour = "Blue")  + xlab("Width of boxplot") + ylab("Avgas.CC.Utilization.in.last.12.months") + ggtitle("Avgas.CC.Utilization.in.last.12.months box-plot")
boxplot.stats(master_file$Avgas.CC.Utilization.in.last.12.months)
quantile(master_file$Avgas.CC.Utilization.in.last.12.months, probs=seq(0, 1,length = 101), na.rm = TRUE)

describe(master_file$Avgas.CC.Utilization.in.last.12.months)
stat.desc(master_file$Avgas.CC.Utilization.in.last.12.months)

IQR(master_file$Avgas.CC.Utilization.in.last.12.months,na.rm = TRUE)
var(master_file$Avgas.CC.Utilization.in.last.12.months,na.rm = TRUE)
sd(master_file$Avgas.CC.Utilization.in.last.12.months,na.rm = TRUE)
skewness(master_file$Avgas.CC.Utilization.in.last.12.months,na.rm = TRUE)
kurtosis(master_file$Avgas.CC.Utilization.in.last.12.months,na.rm = TRUE)


# For the analysis of variable representing No.of.trades.opened.in.last.6.months of the Applicant

ggplot(master_file, aes(x=No.of.trades.opened.in.last.6.months))+geom_histogram(fill="Red",bins = 5)+ ggtitle("Histogram of No.of.trades.opened.in.last.6.months")+ xlab("No.of.trades.opened.in.last.6.months") + ylab("No. of Customers") 

summary(master_file$No.of.trades.opened.in.last.6.months)

ggplot(master_file, aes(x=1, y=No.of.trades.opened.in.last.6.months)) + geom_boxplot(outlier.colour = "Blue")  + xlab("Width of boxplot") + ylab("No.of.trades.opened.in.last.6.months") + ggtitle("No.of.trades.opened.in.last.6.months box-plot")
boxplot.stats(master_file$No.of.trades.opened.in.last.6.months)
quantile(master_file$No.of.trades.opened.in.last.6.months, probs=seq(0, 1,length = 101))

describe(master_file$No.of.trades.opened.in.last.6.months)
stat.desc(master_file$No.of.trades.opened.in.last.6.months)

IQR(master_file$No.of.trades.opened.in.last.6.months, na.rm = TRUE)
var(master_file$No.of.trades.opened.in.last.6.months, na.rm = TRUE)
sd(master_file$No.of.trades.opened.in.last.6.months, na.rm = TRUE)
skewness(master_file$No.of.trades.opened.in.last.6.months, na.rm = TRUE)
kurtosis(master_file$No.of.trades.opened.in.last.6.months, na.rm = TRUE)


# For the analysis of variable representing No.of.trades.opened.in.last.12.months of the Applicant

ggplot(master_file, aes(x=No.of.trades.opened.in.last.12.months))+geom_histogram(fill="Red",bins = 5)+ ggtitle("Histogram of No.of.trades.opened.in.last.12.months")+ xlab("No.of.trades.opened.in.last.12.months") + ylab("No. of Customers") 

summary(master_file$No.of.trades.opened.in.last.12.months)

ggplot(master_file, aes(x=1, y=No.of.trades.opened.in.last.12.months)) + geom_boxplot(outlier.colour = "Blue")  + xlab("Width of boxplot") + ylab("No.of.trades.opened.in.last.12.months") + ggtitle("No.of.trades.opened.in.last.12.months box-plot")
boxplot.stats(master_file$No.of.trades.opened.in.last.12.months)
quantile(master_file$No.of.trades.opened.in.last.12.months, probs=seq(0, 1,length = 101))

describe(master_file$No.of.trades.opened.in.last.12.months)
stat.desc(master_file$No.of.trades.opened.in.last.12.months)

IQR(master_file$No.of.trades.opened.in.last.12.months, na.rm = TRUE)
var(master_file$No.of.trades.opened.in.last.12.months, na.rm = TRUE)
sd(master_file$No.of.trades.opened.in.last.12.months, na.rm = TRUE)
skewness(master_file$No.of.trades.opened.in.last.12.months, na.rm = TRUE)
kurtosis(master_file$No.of.trades.opened.in.last.12.months, na.rm = TRUE)

# For the analysis of variable representing No.of.PL.trades.opened.in.last.6.months of the Applicant

ggplot(master_file, aes(x=No.of.PL.trades.opened.in.last.6.months))+geom_histogram(fill="Red",bins = 5)+ ggtitle("Histogram of No.of.PL.trades.opened.in.last.6.months")+ xlab("No.of.PL.trades.opened.in.last.6.months") + ylab("No. of Customers") 

summary(master_file$No.of.PL.trades.opened.in.last.6.months)

ggplot(master_file, aes(x=1, y=No.of.PL.trades.opened.in.last.6.months)) + geom_boxplot(outlier.colour = "Blue")  + xlab("Width of boxplot") + ylab("No.of.PL.trades.opened.in.last.6.months") + ggtitle("No.of.PL.trades.opened.in.last.6.months box-plot")
boxplot.stats(master_file$No.of.PL.trades.opened.in.last.6.months)
quantile(master_file$No.of.PL.trades.opened.in.last.6.months, probs=seq(0, 1,length = 101))

describe(master_file$No.of.PL.trades.opened.in.last.6.months)
stat.desc(master_file$No.of.PL.trades.opened.in.last.6.months)

IQR(master_file$No.of.PL.trades.opened.in.last.6.months, na.rm = TRUE)
var(master_file$No.of.PL.trades.opened.in.last.6.months, na.rm = TRUE)
sd(master_file$No.of.PL.trades.opened.in.last.6.months, na.rm = TRUE)
skewness(master_file$No.of.PL.trades.opened.in.last.6.months, na.rm = TRUE)
kurtosis(master_file$No.of.PL.trades.opened.in.last.6.months, na.rm = TRUE)

# For the analysis of variable representing No.of.PL.trades.opened.in.last.12.months of the Applicant

ggplot(master_file, aes(x=No.of.PL.trades.opened.in.last.12.months))+geom_histogram(fill="Red",bins = 5)+ ggtitle("Histogram of No.of.PL.trades.opened.in.last.12.months")+ xlab("No.of.PL.trades.opened.in.last.12.months") + ylab("No. of Customers") 

summary(master_file$No.of.PL.trades.opened.in.last.12.months)

ggplot(master_file, aes(x=1, y=No.of.PL.trades.opened.in.last.12.months)) + geom_boxplot(outlier.colour = "Blue")  + xlab("Width of boxplot") + ylab("No.of.PL.trades.opened.in.last.12.months") + ggtitle("No.of.PL.trades.opened.in.last.12.months box-plot")
boxplot.stats(master_file$No.of.PL.trades.opened.in.last.12.months)
quantile(master_file$No.of.PL.trades.opened.in.last.12.months, probs=seq(0, 1,length = 101))

describe(master_file$No.of.PL.trades.opened.in.last.12.months)
stat.desc(master_file$No.of.PL.trades.opened.in.last.12.months)

IQR(master_file$No.of.PL.trades.opened.in.last.12.months, na.rm = TRUE)
var(master_file$No.of.PL.trades.opened.in.last.12.months, na.rm = TRUE)
sd(master_file$No.of.PL.trades.opened.in.last.12.months, na.rm = TRUE)
skewness(master_file$No.of.PL.trades.opened.in.last.12.months, na.rm = TRUE)
kurtosis(master_file$No.of.PL.trades.opened.in.last.12.months, na.rm = TRUE)

# For the analysis of variable representing No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. of the Applicant

ggplot(master_file, aes(x=No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.))+geom_histogram(fill="Red",bins = 5)+ ggtitle("Histogram of No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.")+ xlab("No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.") + ylab("No. of Customers") 

summary(master_file$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)

ggplot(master_file, aes(x=1, y=No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)) + geom_boxplot(outlier.colour = "Blue")  + xlab("Width of boxplot") + ylab("No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.") + ggtitle("No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. box-plot")
boxplot.stats(master_file$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
quantile(master_file$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., probs=seq(0, 1,length = 101))

describe(master_file$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
stat.desc(master_file$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)

IQR(master_file$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., na.rm = TRUE)
var(master_file$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., na.rm = TRUE)
sd(master_file$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., na.rm = TRUE)
skewness(master_file$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., na.rm = TRUE)
kurtosis(master_file$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., na.rm = TRUE)

# For the analysis of variable representing No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. of the Applicant

ggplot(master_file, aes(x=No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.))+geom_histogram(fill="Red",bins = 5)+ ggtitle("Histogram of No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.")+ xlab("No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.") + ylab("No. of Customers") 

summary(master_file$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)

ggplot(master_file, aes(x=1, y=No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)) + geom_boxplot(outlier.colour = "Blue")  + xlab("Width of boxplot") + ylab("No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.") + ggtitle("No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. box-plot")
boxplot.stats(master_file$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
quantile(master_file$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., probs=seq(0, 1,length = 101))

describe(master_file$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
stat.desc(master_file$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)

IQR(master_file$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., na.rm = TRUE)
var(master_file$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., na.rm = TRUE)
sd(master_file$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., na.rm = TRUE)
skewness(master_file$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., na.rm = TRUE)
kurtosis(master_file$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., na.rm = TRUE)

# For analysis of total no.of trades of the applicant

ggplot(master_file, aes(x=Total.No.of.Trades))+geom_histogram(fill="Red")+ ggtitle("Histogram of Total.No.of.Trades")+ xlab("Total.No.of.Trades") + ylab("No. of Customers") 

ggplot(master_file, aes(x=Total.No.of.Trades))+geom_histogram(aes(y=..density..),colour="black", fill="white")+geom_density(alpha=.2, fill="#FF6666")+ggtitle("Distribution of Total.No.of.Trades")+ xlab("Total.No.of.Trades") + ylab("Density") 

summary(master_file$Total.No.of.Trades )

ggplot(master_file, aes(x=1, y=Total.No.of.Trades)) + geom_boxplot(outlier.colour = "Blue")  + xlab("Width of boxplot") + ylab("Total.No.of.Trades") + ggtitle("Total.No.of.Trades box-plot")
boxplot.stats(master_file$Total.No.of.Trades )
quantile(master_file$Total.No.of.Trades , probs=seq(0, 1,length = 101), na.rm = TRUE)

describe(master_file$Total.No.of.Trades)
stat.desc(master_file$Total.No.of.Trades)

IQR(master_file$Total.No.of.Trades, na.rm = TRUE)
var(master_file$Total.No.of.Trades, na.rm = TRUE)
sd(master_file$Total.No.of.Trades, na.rm = TRUE)
skewness(master_file$Total.No.of.Trades, na.rm = TRUE)
kurtosis(master_file$Total.No.of.Trades, na.rm = TRUE)

# For analysis of outstanding balance variable of the applicant

ggplot(master_file, aes(x=Outstanding.Balance))+geom_histogram(fill="Red")+ ggtitle("Histogram of Outstanding.Balance")+ xlab("Outstanding.Balance") + ylab("No. of Customers") 

ggplot(master_file, aes(x=Outstanding.Balance))+geom_histogram(aes(y=..density..),colour="black", fill="white")+geom_density(alpha=.2, fill="#FF6666")+ggtitle("Distribution of Outstanding.Balance")+ xlab("Outstanding.Balance") + ylab("Density") 

summary(master_file$Outstanding.Balance )

ggplot(master_file, aes(x=1, y=Outstanding.Balance)) + geom_boxplot(outlier.colour = "Blue")  + xlab("Width of boxplot") + ylab("Outstanding.Balance") + ggtitle("Outstanding.Balance box-plot")
boxplot.stats(master_file$Outstanding.Balance)
quantile(master_file$Outstanding.Balance, probs=seq(0, 1,length = 101), na.rm = TRUE)

describe(master_file$Outstanding.Balance)
stat.desc(master_file$Outstanding.Balance)

IQR(master_file$Outstanding.Balance, na.rm = TRUE)
var(master_file$Outstanding.Balance, na.rm = TRUE)
sd(master_file$Outstanding.Balance, na.rm = TRUE)
skewness(master_file$Outstanding.Balance, na.rm = TRUE)
kurtosis(master_file$Outstanding.Balance, na.rm = TRUE)


###----------------------------------For Discrete Variables------------------------------------###

# Creation of a function to plot the distribution of applicants in categories of 
# different Categorical variables

plot_applicants <- function(cat_var, var_name)
  {
  count <- data.frame(table(cat_var))

  colnames(count) <- c(var_name,"No.of_Applicants")
  
  ggplot(count, aes(count[,1], No.of_Applicants, label = No.of_Applicants)) +geom_bar(stat="identity", color = "red",fill = "grey") + geom_text(size = 3, vjust = -0.5) +xlab(var_name)
  
  }

# For Gender of the applicant

plot_applicants(master_file$Gender, "Gender" )
table(master_file$Gender)

# For Marital Status of the applicant

plot_applicants(master_file$Marital.Status..at.the.time.of.application., "Marital Status at time of application" )
table(master_file$Marital.Status..at.the.time.of.application.)

# For Education of the applicant

plot_applicants(master_file$Education, "Education" )
table(master_file$Education)

# For Profession of the applicant

plot_applicants(master_file$Profession, "Profession" )

table(master_file$Profession)

# For Residence type of the applicant

plot_applicants(master_file$Type.of.residence, "Type.of.residence" )
table(master_file$Type.of.residence)


# For analyzing the customers who have opened trades in last 6 months 

plot_applicants(master_file$No.of.trades.opened.in.last.6.months, "No.of.trades.opened.in.last.6.months" )
table(master_file$No.of.trades.opened.in.last.6.months)

# For analyzing the customers who have opened trades in last 12 months 

plot_applicants(master_file$No.of.trades.opened.in.last.12.months, "No.of.trades.opened.in.last.12.months" )
table(master_file$No.of.trades.opened.in.last.12.months)

# For analyzing the customers who have opened PL trades in last 6 months 

plot_applicants(master_file$No.of.PL.trades.opened.in.last.6.months, "No.of.PL.trades.opened.in.last.6.months" )
table(master_file$No.of.PL.trades.opened.in.last.6.months)

# For analyzing the customers who have opened PL trades in last 12 months 

plot_applicants(master_file$No.of.PL.trades.opened.in.last.12.months, "No.of.PL.trades.opened.in.last.12.months" )
table(master_file$No.of.PL.trades.opened.in.last.12.months)

# For analyzing the customers who have inquired for loan in last 6 months 

plot_applicants(master_file$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans." )
table(master_file$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)

# For analyzing the customers who have inquired for loan in last 12 months 

plot_applicants(master_file$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans." )
table(master_file$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)

# For analyzing the customers who have a open home loan  

plot_applicants(master_file$Presence.of.open.home.loan, "Presence.of.open.home.loan Status" )
table(master_file$Presence.of.open.home.loan)

# For analyzing the customers who have an open auto loan 

plot_applicants(master_file$Presence.of.open.auto.loan, "Presence.of.open.auto.loan Status" )
table(master_file$Presence.of.open.auto.loan)

# For analyzing the Performance of the Customers (TARGET VARIABLE)

plot_applicants(master_file$Performance.Tag, "Performance_Tag" )
table(master_file$Performance.Tag)


####_________________________________MULTI-VARIATE ANALYSIS___________________________________####

###---------------------------------For Continuous Variables-----------------------------------###

## Subsetting the continous variables and renaming the attributes

master_file_continuous<-master_file[,c(3,7,11,12,13,14,15,16,17,18, 19,20,21,22,23,24,25, 27,28,6)]

names(master_file_continuous)<-c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19", "20")

# Following is the index for the renamed attributes

# [1] "Age"                                                            
# [2] "Income"                                                         
# [3] "No.of.months.in.current.residence"                              
# [4] "No.of.months.in.current.company"                                
# [5] "No.of.times.90.DPD.or.worse.in.last.6.months"                   
# [6] "No.of.times.60.DPD.or.worse.in.last.6.months"                   
# [7] "No.of.times.30.DPD.or.worse.in.last.6.months"                   
# [8] "No.of.times.90.DPD.or.worse.in.last.12.months"                  
# [9] "No.of.times.60.DPD.or.worse.in.last.12.months"                  
# [10] "No.of.times.30.DPD.or.worse.in.last.12.months"                  
# [11] "Avgas.CC.Utilization.in.last.12.months"                         
# [12] "No.of.trades.opened.in.last.6.months"                           
# [13] "No.of.trades.opened.in.last.12.months"                          
# [14] "No.of.PL.trades.opened.in.last.6.months"                        
# [15] "No.of.PL.trades.opened.in.last.12.months"                       
# [16] "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans." 
# [17] "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans."
# [18] "Outstanding.Balance"                                            
# [19] "Total.No.of.Trades"   
# [20] "No.of Dependents"

## Calculation of CORRELATION of all the pairs of continuous variables at the same time

credit_card_continous_matrix <- cor(master_file_continuous, use="complete.obs")

ggpairs(credit_card_continous_matrix, axisLabels = "internal")


## Bi-variate Distribution of all the continuous variables across different levels of Performance.Tag :

# Age vs. Performance Tag

plot_ly(master_file, x = ~Age, color = ~factor(Performance.Tag), type = "box")%>%
  layout(title = "Age boxplot across different Performance Levels", yaxis=list(title="Performance Levels"))

ggplot(master_file,aes(x=Age,colour=factor(Performance.Tag)))+geom_density()+ggtitle(" Age distribution across Performance levels") + xlab("Age") +labs(colour="Performance.Tag")

# Income vs. Performance Tag

plot_ly(master_file, x = ~Income, color = ~factor(Performance.Tag), type = "box")%>%
  layout(title = "Income boxplot across different Performance Levels", yaxis=list(title="Performance Levels"))

ggplot(master_file,aes(x=Income,colour=factor(Performance.Tag)))+geom_density()+ggtitle(" Income distribution across Performance levels") + xlab("Income") +labs(colour="Performance.Tag")

# No.of months in current residence vs. Performance Tag

plot_ly(master_file, x = ~No.of.months.in.current.residence, color = ~factor(Performance.Tag), type = "box")%>%
  layout(title = "No.of.months.in.current.residence boxplot across different Performance Levels", yaxis=list(title="Performance Levels"))

ggplot(master_file,aes(x=No.of.months.in.current.residence,colour=factor(Performance.Tag)))+geom_density()+ggtitle(" No.of.months.in.current.residence distribution across Performancelevels") + xlab("No.of.months.in.current.residence") +labs(colour="Performance.Tag")

# No.of months in current company vs. Performance Tag

plot_ly(master_file, x = ~No.of.months.in.current.company, color = ~factor(Performance.Tag), type = "box")%>%
  layout(title = "No.of.months.in.current.company boxplot across different Performance Levels", yaxis=list(title="Performance Levels"))

ggplot(master_file,aes(x=No.of.months.in.current.company,colour=factor(Performance.Tag)))+geom_density()+ggtitle(" No.of.months.in.current.company distribution across Performance levels") + xlab("No.of.months.in.current.company") +labs(colour="Performance.Tag")

# Average credit card utilisation by an applicant in last 12 months vs. Performance Tag

plot_ly(master_file, x = ~Avgas.CC.Utilization.in.last.12.months, color = ~factor(Performance.Tag), type = "box")%>%
  layout(title = "Avgas.CC.Utilization.in.last.12.months boxplot across different Performance Levels", yaxis=list(title="Performance Levels"))

ggplot(master_file,aes(x=Avgas.CC.Utilization.in.last.12.months ,colour=factor(Performance.Tag)))+geom_density()+ggtitle(" Density Distribution of Avgas.CC.Utilization.in.last.12.months across levels") + xlab("Avgas.CC.Utilization.in.last.12.months") +labs(colour="Performance.Tag")

# Total no.of trades by an applicant vs. Performance Tag

plot_ly(master_file, x = ~Total.No.of.Trades, color = ~factor(Performance.Tag), type = "box")%>%
  layout(title = "Total.No.of.Trades boxplot across different Performance Levels", yaxis=list(title="Performance Levels"))

ggplot(master_file,aes(x=Total.No.of.Trades ,colour=factor(Performance.Tag)))+geom_density()+ggtitle(" Density Distribution of Total.No.of.Trades across Performancelevels") + xlab("Total.No.of.Trades") +labs(colour="Performance.Tag")

# Outstanding Balance of an applicant vs. Performance Tag

plot_ly(master_file, x = ~Outstanding.Balance, color = ~factor(Performance.Tag), type = "box")%>%
  layout(title = "Outstanding.Balance boxplot across different Performance Levels", yaxis=list(title="Performance Levels"))

ggplot(master_file,aes(x=Outstanding.Balance ,colour=factor(Performance.Tag)))+geom_density()+ggtitle(" Density Distribution of Outstanding.Balance across Performance levels") + xlab("Outstanding.Balance") +labs(colour="Performance.Tag")


# No.of.times.90.DPD.or.worse.in.last.6.months vs. Performance.Tag

ggplot(master_file, aes(x =No.of.times.90.DPD.or.worse.in.last.6.months)) + geom_histogram(fill = "black",bins = 5)+facet_wrap(~Performance.Tag)+ggtitle("No.of.times.90.DPD.or.worse.in.last.6.months vs Performance of Applicants")+ylab("No.of Applicants")+xlab("No.of.times.90.DPD.or.worse.in.last.6.months ")

ggplot(master_file, aes(x =No.of.times.90.DPD.or.worse.in.last.6.months,fill=factor(Performance.Tag))) + geom_histogram(bins = 5)+ggtitle("No.of.times.90.DPD.or.worse.in.last.6.months vs Performance of Applicants")+ylab("No.of Applicants")+xlab("No.of.times.90.DPD.or.worse.in.last.6.months ")+labs(fill="Performance.Tag")

# No.of.times.60.DPD.or.worse.in.last.6.months vs. Performance.Tag

ggplot(master_file, aes(x =No.of.times.60.DPD.or.worse.in.last.6.months)) + geom_histogram(fill = "black",bins = 5)+facet_wrap(~Performance.Tag)+ggtitle("No.of.times.60.DPD.or.worse.in.last.6.months vs Performance of Applicants")+ylab("No.of Applicants")+xlab("No.of.times.60.DPD.or.worse.in.last.6.months ")

ggplot(master_file, aes(x =No.of.times.60.DPD.or.worse.in.last.6.months,fill=factor(Performance.Tag))) + geom_histogram(bins = 5)+ggtitle("No.of.times.60.DPD.or.worse.in.last.6.months vs Performance of Applicants")+ylab("No.of Applicants")+xlab("No.of.times.60.DPD.or.worse.in.last.6.months ")+labs(fill="Performance.Tag")

# No.of.times.30.DPD.or.worse.in.last.6.months vs. Performance.Tag

ggplot(master_file, aes(x =No.of.times.30.DPD.or.worse.in.last.6.months)) + geom_histogram(fill = "black",bins = 5)+facet_wrap(~Performance.Tag)+ggtitle("No.of.times.30.DPD.or.worse.in.last.6.months vs Performance of Applicants")+ylab("No.of Applicants")+xlab("No.of.times.30.DPD.or.worse.in.last.6.months ")

ggplot(master_file, aes(x =No.of.times.30.DPD.or.worse.in.last.6.months,fill=factor(Performance.Tag))) + geom_histogram(bins = 5)+ggtitle("No.of.times.30.DPD.or.worse.in.last.6.months vs Performance of Applicants")+ylab("No.of Applicants")+xlab("No.of.times.30.DPD.or.worse.in.last.6.months ")+labs(fill="Performance.Tag")

# No.of.times.90.DPD.or.worse.in.last.12.months vs. Performance.Tag

ggplot(master_file, aes(x =No.of.times.90.DPD.or.worse.in.last.12.months)) + geom_histogram(fill = "black",bins = 5)+facet_wrap(~Performance.Tag)+ggtitle("No.of.times.90.DPD.or.worse.in.last.12.months vs Performance of Applicants")+ylab("No.of Applicants")+xlab("No.of.times.90.DPD.or.worse.in.last.12.months ")

ggplot(master_file, aes(x =No.of.times.90.DPD.or.worse.in.last.12.months,fill=factor(Performance.Tag))) + geom_histogram(bins = 5)+ggtitle("No.of.times.90.DPD.or.worse.in.last.12.months vs Performance of Applicants")+ylab("No.of Applicants")+xlab("No.of.times.90.DPD.or.worse.in.last.12.months ")+labs(fill="Performance.Tag")

# No.of.times.60.DPD.or.worse.in.last.12.months vs. Performance.Tag

ggplot(master_file, aes(x =No.of.times.60.DPD.or.worse.in.last.12.months)) + geom_histogram(fill = "black",bins = 5)+facet_wrap(~Performance.Tag)+ggtitle("No.of.times.60.DPD.or.worse.in.last.12.months vs Performance of Applicants")+ylab("No.of Applicants")+xlab("No.of.times.60.DPD.or.worse.in.last.12.months ")

ggplot(master_file, aes(x =No.of.times.60.DPD.or.worse.in.last.12.months,fill=factor(Performance.Tag))) + geom_histogram(bins = 5)+ggtitle("No.of.times.60.DPD.or.worse.in.last.12.months vs Performance of Applicants")+ylab("No.of Applicants")+xlab("No.of.times.60.DPD.or.worse.in.last.12.months ")+labs(fill="Performance.Tag")

# No.of.times.30.DPD.or.worse.in.last.12.months vs. Performance.Tag

ggplot(master_file, aes(x =No.of.times.30.DPD.or.worse.in.last.12.months)) + geom_histogram(fill = "black",bins = 5)+facet_wrap(~Performance.Tag)+ggtitle("No.of.times.30.DPD.or.worse.in.last.12.months vs Performance of Applicants")+ylab("No.of Applicants")+xlab("No.of.times.30.DPD.or.worse.in.last.12.months ")

ggplot(master_file, aes(x =No.of.times.30.DPD.or.worse.in.last.12.months,fill=factor(Performance.Tag))) + geom_histogram(bins = 5)+ggtitle("No.of.times.30.DPD.or.worse.in.last.12.months vs Performance of Applicants")+ylab("No.of Applicants")+xlab("No.of.times.30.DPD.or.worse.in.last.12.months ")+labs(fill="Performance.Tag")


## Trivariate-variate Distribution of all the continuous variables across different levels of Performance.Tag :

# Age variation of different Gender at different Performance tag levels

ggplot(master_file, aes(x=Age,fill=Gender))+geom_histogram()+facet_wrap(~Performance.Tag)

# Income variation of different Gender at different Performance tag levels

ggplot(master_file, aes(x=Income,fill=Gender))+geom_histogram()+facet_wrap(~Performance.Tag,scales = "free")

# Outstanding.Balance variation of different Gender at different Performance tag levels

ggplot(master_file, aes(x=Outstanding.Balance,fill=Gender))+geom_histogram()+facet_wrap(~Performance.Tag,scales = "free")

# Income variation of applicants with different Profession at different Performance tag levels

ggplot(master_file, aes(x=Income,fill=Profession))+geom_histogram()+facet_wrap(~Performance.Tag,scales = "free")

# Outstanding.Balance variation of different Profession at different Performance tag levels

ggplot(master_file, aes(x=Outstanding.Balance,fill=Profession))+geom_histogram()+facet_wrap(~Performance.Tag,scales = "free")

# Total no.of trades variation of different Profession at different Performance tag levels

ggplot(master_file, aes(x=Total.No.of.Trades, fill=Profession))+geom_histogram()+facet_wrap(~Performance.Tag,scales = "free")


###---------------------------------For Discrete Variables-----------------------------------###


## Creation of a function to plot the Distribution of 
## all the categorical variables across different levels of Performance.Tag :

binary_plot<-function(cat_var,cat_var_name)
{
  
  a<-data.frame(table(cat_var,master_file$Performance.Tag))
  
  colnames(a)<-c(cat_var_name,"Performance_Levels", "No_of_Applicants")
  
  ggplot(a, aes(x = Performance_Levels,y=No_of_Applicants, fill= a[,1])) + geom_bar(stat="identity",position = "dodge",hjust=1)+geom_text(aes(label=No_of_Applicants),position=position_dodge(0.9), vjust=-0.5, size=3, fontface= "bold")+ggtitle(paste(cat_var_name,"vs Performance of Applicants"))+ylab("No.of Applicants")+xlab("Performance Levels")+labs(fill=cat_var_name)
  
  
}

# Gender vs. Performance.Tag

binary_plot(master_file$Gender, "Gender")

# Marital.Status..at.the.time.of.application.vs. Performance.Tag

binary_plot(master_file$Marital.Status..at.the.time.of.application., "Marital Status")

# Education  vs. Performance.Tag

binary_plot(master_file$Education , "Education")

# Profession  vs. Performance.Tag

binary_plot(master_file$Profession , "Profession")

# Type.of.residence vs. Performance.Tag

binary_plot(master_file$Type.of.residence , "Type.of.residence")

# Presence.of.open.home.loan vs. Performance.Tag

binary_plot(master_file$Presence.of.open.home.loan  , "Presence.of.open.home.loan ")

# Presence.of.open.auto.loan vs. Performance.Tag

binary_plot(master_file$Presence.of.open.auto.loan  , "Presence.of.open.auto.loan ")


####_________________________________MISSING VALUE TREATMENT___________________________________####


###---------------------------------For Demographic Continuous Attributes-----------------------------------###

## Variables with NA values
## 1) No. of Dependents(NA values= 3)


# For No.of Dependents variable having 3 values missing, the missing values have been 
#replaced with the median of the column, i.e., 15.

master_file$No.of.dependents[which(is.na(master_file$No.of.dependents)==TRUE)]<-median(master_file$No.of.dependents, na.rm = TRUE)


###---------------------------------For Demographic Discrete Variables-----------------------------------###

## Variables with blank values
## 1) Gender (blank values= 2)
## 2) Marital Status (blank values = 6)
## 3) Education (blank values = 119)
## 4) Profession (blank values = 14)
## 5) Type of Residence (blank values = 8)

# As the categorical variables were observed to have blank values, hence, these blank 
# values have been replaced with the MODE of the individual attributes

# 1) Gender Attribute (Mode= "M")

master_file$Gender[which(master_file$Gender=="")]<-"M" 

# 2) Marital Status Attribute (Mode= "Married")

master_file$Marital.Status..at.the.time.of.application.[which(master_file$Marital.Status..at.the.time.of.application.=="")]<-"Married"

# 3) Education Attribute (Mode= "Professional")

master_file$Education[which(master_file$Education=="")]<-"Professional"

# 4) Profession Attribute (Mode= "SAL")

master_file$Profession[which(master_file$Profession=="")]<-"SAL"

# 5) Type of Residence Attribute (Mode= "Rented")

master_file$Type.of.residence[which(master_file$Type.of.residence=="")]<-"Rented"


####___________________________________OUTLIER TREATMENT______________________________________####

###---------------------------For Demographic Continuous Variables-----------------------------###

## 1) Age
## 2) Income
## 2) No.of months in current company

# For Age variable, the outlier values have been replaced with median of the attribute, i.e., 45.

master_file$Age[which(master_file$Age<18)]=45

# For No.of months in current company variable, the outlier values have been capped with the maximum
# value of 98 , as observed in the boxplot statistics

master_file$No.of.months.in.current.company[which(master_file$No.of.months.in.current.company>98)]<-98

# For Income variable, the values of -0.5 & 0 were considerd as outliers (though they were not 
# depicted by the box-plot statistics.These values were replaced by the median of the Income attribute
# 

master_file$Income[which(master_file$Income<1)]<-median(master_file$Income, na.rm = TRUE)


####________________________________WOE & IV ANALYSIS_________________________________________####

## As, for calculation of WOE and IV the type conversion of attributes is required. Hence, the following 
## attributes are converted to FACTOR type

master_file$Presence.of.open.home.loan<-as.factor(master_file$Presence.of.open.home.loan)
master_file$Presence.of.open.auto.loan<-as.factor(master_file$Presence.of.open.auto.loan)

## Also, the binary target variable must be numeric in nature, while WOE calculation is done. Hence,
## the PERFORMANCE TAG attribute is converted to numeric type from integer type.

master_file$Performance.Tag<-as.numeric(master_file$Performance.Tag)

## The main dataset has been assigned to a new dataset with the name MASTER_NEW. This new dataset will
## contain the WOE values for every attribute.

master_new<-master_file


# ##-------Binning the AGE Attribute------------------


master_new$Age[which(master_new$Age<=30)]="(18,30]"
master_new$Age[which(master_new$Age>30 & master_new$Age<=40)]="(31,40]"
master_new$Age[which(master_new$Age>40 & master_new$Age<=50)]="(41,50]"
master_new$Age[which(master_new$Age>50 & master_new$Age<=60)]="(51,60]"
master_new$Age[which(master_new$Age>60 )]="(61,65]"


## In order to check WOE for the prepared bins for Age attribute

IV_age<-create_infotables(data=master_new[,-1], y="Performance.Tag", bins=10, parallel=FALSE)

IV_age$Tables$Age

plot_infotables(IV_age, "Age")

## Replacement of the bins of the Age attribute with their respective WOE values

master_new$Age[which(master_new$Age=="(18,30]")]= -0.034822022
master_new$Age[which(master_new$Age=="(31,40]")]=  0.053929007 
master_new$Age[which(master_new$Age=="(41,50]")]= -0.009195985
master_new$Age[which(master_new$Age=="(51,60]")]= -0.030906352
master_new$Age[which(master_new$Age=="(61,65]")]= -0.018204111


## For getting the validation that whether the binning done in the AGE attribute is proper or not,
## a logistic regression model using AGE attribute with the WOE values need to be checked for the
## slope of the attribute (it should be 1, if the binning algorithm followed is good)

master_new$Performance.Tag<-as.factor(master_new$Performance.Tag)
master_new$Age<-as.numeric(master_new$Age)
model_Age<-glm(formula=Performance.Tag~Age, data= master_new, family="binomial")
summary((model_Age)) # slope of the equation = 1


## -------Binning the INCOME Attribute---------

a<-iv.binning.simple(master_new, "Income", by= 1/6)

master_new$Income[which(master_new$Income<=10)]= "[1,10]"
master_new$Income[which(master_new$Income>10 & master_new$Income<=18)]= "(10,18]"
master_new$Income[which(master_new$Income>18 & master_new$Income<=27)]= "(18,27]"
master_new$Income[which(master_new$Income>27 & master_new$Income<=35)]= "(27,35]"
master_new$Income[which(master_new$Income>35 & master_new$Income<=44)]= "(35,44]"
master_new$Income[which(master_new$Income>44 & master_new$Income<=60)]= "(44,60]"


## In order to check WOE for the prepared bins for Income attribute

master_new$Performance.Tag<-unfactor(master_new$Performance.Tag)

IV_income<-create_infotables(data=master_new[,-1], y="Performance.Tag", bins=10, parallel=FALSE)

IV_income$Tables$Income
plot_infotables(IV_income,"Income")

## Replacement of the bins of the Income attribute with their respective WOE values

master_new$Income[which(master_new$Income=="[1,10]")] <-  0.29488902
master_new$Income[which(master_new$Income=="(10,18]")]<- 0.05672875
master_new$Income[which(master_new$Income=="(18,27]")]<- 0.04940877
master_new$Income[which(master_new$Income=="(27,35]")]<- -0.03093510
master_new$Income[which(master_new$Income=="(35,44]")]<- -0.2431116
master_new$Income[which(master_new$Income=="(44,60]")]<- -0.26283400


## For getting the validation that whether the binning done in the Income attribute is proper or not,
## a logistic regression model using Income attribute with the WOE values need to be checked for the
## slope of the attribute (it should be 1, if the binning algorithm followed is good)

master_new$Performance.Tag<-as.factor(master_new$Performance.Tag)
master_new$Income<-as.numeric(master_new$Income)

model_Income<-glm(formula=Performance.Tag~Income, data= master_new, family="binomial")

summary((model_Income)) # slope of the equation = 1


## -------Binning the No.of.dependents Attribute---------

m<-iv.binning.simple(master_new, "No.of.dependents", by= 1/4)

master_new$No.of.dependents[which(master_new$No.of.dependents<=2)]= "[1,2]"
master_new$No.of.dependents[which(master_new$No.of.dependents>2 & master_new$No.of.dependents<=3)]= "(2,3]"
master_new$No.of.dependents[which(master_new$No.of.dependents>3 & master_new$No.of.dependents<=4)]= "(3,4]"
master_new$No.of.dependents[which(master_new$No.of.dependents>4 & master_new$No.of.dependents<=5)]= "(4,5]"

## In order to check WOE for the prepared bins for No.of.dependents attribute

master_new$Performance.Tag<-unfactor(master_new$Performance.Tag)

IV_No.of.dependents<-create_infotables(data=master_new[,-1], y="Performance.Tag", bins=10, parallel=FALSE)

IV_No.of.dependents$Tables$No.of.dependents
plot_infotables(IV_No.of.dependents,"No.of.dependents")

## Replacement of the bins of the No.of.dependents attribute with their respective WOE values

master_new$No.of.dependents[which(master_new$No.of.dependents=="[1,2]")] <-  -0.02058310
master_new$No.of.dependents[which(master_new$No.of.dependents=="(2,3]")]<- 0.05395479
master_new$No.of.dependents[which(master_new$No.of.dependents=="(3,4]")]<- -0.02520439
master_new$No.of.dependents[which(master_new$No.of.dependents=="(4,5]")]<- 0.00439087

## For getting the validation that whether the binning done in the No.of.dependents attribute is proper or not,
## a logistic regression model using No.of.dependents attribute with the WOE values need to be checked for the
## slope of the attribute (it should be 1, if the binning algorithm followed is good)

master_new$Performance.Tag<-as.factor(master_new$Performance.Tag)
master_new$No.of.dependents<-as.numeric(master_new$No.of.dependents)

model_No.of.dependents<-glm(formula=Performance.Tag~No.of.dependents, data= master_new, family="binomial")

summary((model_No.of.dependents)) # slope of the equation = 1


## -------Binning the No. OF MONTHS.IN.CURRENT.RESIDENCE Attribute---------

b<-iv.binning.simple(master_new, "No.of.months.in.current.residence", by= 1/2)


master_new$No.of.months.in.current.residence[which(master_new$No.of.months.in.current.residence>=6 &  master_new$No.of.months.in.current.residence<=10)]<-"[6,10]"
master_new$No.of.months.in.current.residence[which(master_new$No.of.months.in.current.residence>10)]<-"[10,126]"


## In order to check WOE for the prepared bins for NO. OF MONTHS IN CURRENT RESIDENCE attribute

master_new$Performance.Tag<-unfactor(master_new$Performance.Tag)

IV_residence<-create_infotables(data=master_new[,-1], y="Performance.Tag", bins=2, parallel=FALSE)

IV_residence$Tables$No.of.months.in.current.residence
plot_infotables(IV_residence,"No.of.months.in.current.residence")

## Replacement of the bins of the NO. OF MONTHS IN CURRENT RESIDENCE attribute with their respective WOE values

master_new$No.of.months.in.current.residence[which(master_new$No.of.months.in.current.residence=="[6,10]" )]<- 0.2067408
master_new$No.of.months.in.current.residence[which(master_new$No.of.months.in.current.residence=="[10,126]")]<- -0.2528746

## For getting the validation that whether the binning done in the NO. OF MONTHS IN CURRENT RESIDENCE
## attribute is proper or not,a logistic regression model using NO. OF MONTHS IN CURRENT RESIDENCE
## attribute with the WOE values need to be checked for the slope of the attribute (it should 
## be 1, if the binning algorithm followed is good)

master_new$Performance.Tag<-as.factor(master_new$Performance.Tag)
master_new$No.of.months.in.current.residence<-as.numeric(master_new$No.of.months.in.current.residence)

model_residence<-glm(formula=Performance.Tag~No.of.months.in.current.residence, data= master_new, family="binomial")
summary((model_residence)) # slope of the equation = -1


## -------Binning the NO. OF MONTHS IN CURRENT COMPANY Attribute---------

c<-iv.binning.simple(master_new, "No.of.months.in.current.company", by= 1/5)

master_new$No.of.months.in.current.company[which(master_new$No.of.months.in.current.company>=3 &  master_new$No.of.months.in.current.company<=13)]<-"[3,13]"
master_new$No.of.months.in.current.company[which(master_new$No.of.months.in.current.company>13 &  master_new$No.of.months.in.current.company<=27)]<-"(13,27]"
master_new$No.of.months.in.current.company[which(master_new$No.of.months.in.current.company>27 &  master_new$No.of.months.in.current.company<=41)]<-"(27,41]"
master_new$No.of.months.in.current.company[which(master_new$No.of.months.in.current.company>41 &  master_new$No.of.months.in.current.company<=54)]<-"(41,54]"
master_new$No.of.months.in.current.company[which(master_new$No.of.months.in.current.company>54 &  master_new$No.of.months.in.current.company<=98)]<-"(54,98]"


## In order to check WOE for the prepared bins for NO. OF MONTHS IN CURRENT COMPANY attribute

master_new$Performance.Tag<-unfactor(master_new$Performance.Tag)

IV_company<-create_infotables(data=master_new[,-1], y="Performance.Tag", bins=5, parallel=FALSE)

IV_company$Tables$No.of.months.in.current.company
plot_infotables(IV_company, "No.of.months.in.current.company")

## Replacement of the bins of the NO. OF MONTHS IN CURRENT COMPANY attribute with their respective WOE values

master_new$No.of.months.in.current.company[which(master_new$No.of.months.in.current.company=="[3,13]" )]<-  0.14114927
master_new$No.of.months.in.current.company[which(master_new$No.of.months.in.current.company=="(13,27]")]<- 0.10209682
master_new$No.of.months.in.current.company[which(master_new$No.of.months.in.current.company=="(27,41]")]<- -0.02315598
master_new$No.of.months.in.current.company[which(master_new$No.of.months.in.current.company=="(41,54]")]<-  -0.19666823
master_new$No.of.months.in.current.company[which(master_new$No.of.months.in.current.company=="(54,98]")]<- -0.07001794 

## For getting the validation that whether the binning done in the NO. OF MONTHS IN CURRENT COMPANY
## attribute is proper or not, a logistic regression model using NO. OF MONTHS IN CURRENT COMPANY
## attribute with the WOE values need to be checked for the slope of the attribute (it should 
## be 1, if the binning algorithm followed is good)

master_new$Performance.Tag<-as.factor(master_new$Performance.Tag)
master_new$No.of.months.in.current.company<-as.numeric(master_new$No.of.months.in.current.company)

model_company<-glm(formula=Performance.Tag~No.of.months.in.current.company, data= master_new, family="binomial")
summary((model_company)) # slope of the equation = 1


## -------Binning the No.of.times.90.DPD.or.worse.in.last.6.months Attribute---------

nrow(subset(master_new, master_new$No.of.times.90.DPD.or.worse.in.last.6.months<=0))
nrow(subset(master_new, master_new$No.of.times.90.DPD.or.worse.in.last.6.months>0))

master_new$No.of.times.90.DPD.or.worse.in.last.6.months[which(master_new$No.of.times.90.DPD.or.worse.in.last.6.months==0)]<-"Not 90DPD"
master_new$No.of.times.90.DPD.or.worse.in.last.6.months[which(master_new$No.of.times.90.DPD.or.worse.in.last.6.months>=1 & master_new$No.of.times.90.DPD.or.worse.in.last.6.months!="Not 90DPD")] <- "On 90DPD"

## In order to check WOE for the prepared bins for No.of.times.90.DPD.or.worse.in.last.6.months attribute

master_new$Performance.Tag<-unfactor(master_new$Performance.Tag)

IV_90DPD6months<-create_infotables(data=master_new[,-1], y="Performance.Tag", bins=5, parallel=FALSE)

IV_90DPD6months$Tables$No.of.times.90.DPD.or.worse.in.last.6.months
plot_infotables(IV_90DPD6months, "No.of.times.90.DPD.or.worse.in.last.6.months")

## Replacement of the bins of the No.of.times.90.DPD.or.worse.in.last.6.months attribute with their respective WOE values

master_new$No.of.times.90.DPD.or.worse.in.last.6.months[which(master_new$No.of.times.90.DPD.or.worse.in.last.6.months=="Not 90DPD" )]<-  -0.2606781
master_new$No.of.times.90.DPD.or.worse.in.last.6.months[which(master_new$No.of.times.90.DPD.or.worse.in.last.6.months=="On 90DPD")]<- 0.6224550

## For getting the validation that whether the binning done in the No.of.times.90.DPD.or.worse.in.last.6.months
## attribute is proper or not,a logistic regression model using No.of.times.90.DPD.or.worse.in.last.6.months
## attribute with the WOE values need to be checked for the slope of the attribute (it should 
## be 1, if the binning algorithm followed is good)

master_new$Performance.Tag<-as.factor(master_new$Performance.Tag)
master_new$No.of.times.90.DPD.or.worse.in.last.6.months<-as.numeric(master_new$No.of.times.90.DPD.or.worse.in.last.6.months)

model_90DPD6<-glm(formula=Performance.Tag~No.of.times.90.DPD.or.worse.in.last.6.months, data= master_new, family="binomial")
summary((model_90DPD6)) # slope of the equation = 1


## -------Binning the No.of.times.60.DPD.or.worse.in.last.6.months Attribute---------

d<-iv.binning.simple(master_new, "No.of.times.60.DPD.or.worse.in.last.6.months", by= 1/2)

nrow(subset(master_new, master_new$No.of.times.60.DPD.or.worse.in.last.6.months<=0))
nrow(subset(master_new, master_new$No.of.times.60.DPD.or.worse.in.last.6.months>0))

master_new$No.of.times.60.DPD.or.worse.in.last.6.months[which(master_new$No.of.times.60.DPD.or.worse.in.last.6.months==0)]<-"Not 60DPD"
master_new$No.of.times.60.DPD.or.worse.in.last.6.months[which(master_new$No.of.times.60.DPD.or.worse.in.last.6.months>=1 & master_new$No.of.times.60.DPD.or.worse.in.last.6.months!="Not 60DPD")] <- "On 60DPD"

## In order to check WOE for the prepared bins for No.of.times.60.DPD.or.worse.in.last.6.months attribute

master_new$Performance.Tag<-unfactor(master_new$Performance.Tag)

IV_60DPD6months<-create_infotables(data=master_new[,-1], y="Performance.Tag", bins=5, parallel=FALSE)

IV_60DPD6months$Tables$No.of.times.60.DPD.or.worse.in.last.6.months
plot_infotables(IV_60DPD6months, "No.of.times.60.DPD.or.worse.in.last.6.months")

## Replacement of the bins of the No.of.times.60.DPD.or.worse.in.last.6.months attribute with their respective WOE values

master_new$No.of.times.60.DPD.or.worse.in.last.6.months[which(master_new$No.of.times.60.DPD.or.worse.in.last.6.months=="Not 60DPD" )]<-  -0.3363664
master_new$No.of.times.60.DPD.or.worse.in.last.6.months[which(master_new$No.of.times.60.DPD.or.worse.in.last.6.months=="On 60DPD")]<- 0.6225361

## For getting the validation that whether the binning done in the No.of.times.60.DPD.or.worse.in.last.6.months
## attribute is proper or not,a logistic regression model using No.of.times.60.DPD.or.worse.in.last.6.months
## attribute with the WOE values need to be checked for the slope of the attribute (it should 
## be 1, if the binning algorithm followed is good)

master_new$Performance.Tag<-as.factor(master_new$Performance.Tag)
master_new$No.of.times.60.DPD.or.worse.in.last.6.months<-as.numeric(master_new$No.of.times.60.DPD.or.worse.in.last.6.months)

model_60DPD6<-glm(formula=Performance.Tag~No.of.times.60.DPD.or.worse.in.last.6.months, data= master_new, family="binomial")
summary((model_60DPD6)) # slope of the equation = 1


## -------Binning the No.of.times.30.DPD.or.worse.in.last.6.months Attribute---------

d<-iv.binning.simple(master_new, "No.of.times.30.DPD.or.worse.in.last.6.months", by= 1/2)

nrow(subset(master_new, master_new$No.of.times.30.DPD.or.worse.in.last.6.months<=0))
nrow(subset(master_new, master_new$No.of.times.30.DPD.or.worse.in.last.6.months>0))

master_new$No.of.times.30.DPD.or.worse.in.last.6.months[which(master_new$No.of.times.30.DPD.or.worse.in.last.6.months==0)]<-"Not 30DPD"
master_new$No.of.times.30.DPD.or.worse.in.last.6.months[which(master_new$No.of.times.30.DPD.or.worse.in.last.6.months>=1 & master_new$No.of.times.30.DPD.or.worse.in.last.6.months!="Not 30DPD")] <- "On 30DPD"

## In order to check WOE for the prepared bins for No.of.times.60.DPD.or.worse.in.last.6.months attribute

master_new$Performance.Tag<-unfactor(master_new$Performance.Tag)

IV_30DPD6months<-create_infotables(data=master_new[,-1], y="Performance.Tag", bins=5, parallel=FALSE)

IV_30DPD6months$Tables$No.of.times.30.DPD.or.worse.in.last.6.months
plot_infotables(IV_30DPD6months, "No.of.times.30.DPD.or.worse.in.last.6.months")

## Replacement of the bins of the No.of.times.60.DPD.or.worse.in.last.6.months attribute with their respective WOE values

master_new$No.of.times.30.DPD.or.worse.in.last.6.months[which(master_new$No.of.times.30.DPD.or.worse.in.last.6.months=="Not 30DPD" )]<-  -0.3867918
master_new$No.of.times.30.DPD.or.worse.in.last.6.months[which(master_new$No.of.times.30.DPD.or.worse.in.last.6.months=="On 30DPD")]<-  0.6171842

## For getting the validation that whether the binning done in the No.of.times.60.DPD.or.worse.in.last.6.months
## attribute is proper or not,a logistic regression model using No.of.times.60.DPD.or.worse.in.last.6.months
## attribute with the WOE values need to be checked for the slope of the attribute (it should 
## be 1, if the binning algorithm followed is good)

master_new$Performance.Tag<-as.factor(master_new$Performance.Tag)
master_new$No.of.times.30.DPD.or.worse.in.last.6.months<-as.numeric(master_new$No.of.times.30.DPD.or.worse.in.last.6.months)

model_30DPD6<-glm(formula=Performance.Tag~No.of.times.30.DPD.or.worse.in.last.6.months, data= master_new, family="binomial")
summary((model_30DPD6)) # slope of the equation = 1


## -------Binning the No.of.times.90.DPD.or.worse.in.last.12.months Attribute---------

d<-iv.binning.simple(master_new, "No.of.times.90.DPD.or.worse.in.last.12.months", by= 1/2)

nrow(subset(master_new, master_new$No.of.times.90.DPD.or.worse.in.last.12.months<=0))
nrow(subset(master_new, master_new$No.of.times.90.DPD.or.worse.in.last.12.months>0))

master_new$No.of.times.90.DPD.or.worse.in.last.12.months[which(master_new$No.of.times.90.DPD.or.worse.in.last.12.months==0)]<-"Not 90DPD in year"
master_new$No.of.times.90.DPD.or.worse.in.last.12.months[which(master_new$No.of.times.90.DPD.or.worse.in.last.12.months>=1 & master_new$No.of.times.90.DPD.or.worse.in.last.12.months!="Not 90DPD in year")] <- "On 90DPD in year"

## In order to check WOE for the prepared bins for No.of.times.90.DPD.or.worse.in.last.12.months attribute

master_new$Performance.Tag<-unfactor(master_new$Performance.Tag)

IV_90DPD12months<-create_infotables(data=master_new[,-1], y="Performance.Tag", bins=5, parallel=FALSE)

IV_90DPD12months$Tables$No.of.times.90.DPD.or.worse.in.last.12.months
plot_infotables(IV_90DPD12months, "No.of.times.90.DPD.or.worse.in.last.12.months")

## Replacement of the bins of the No.of.times.90.DPD.or.worse.in.last.12.months attribute with their respective WOE values

master_new$No.of.times.90.DPD.or.worse.in.last.12.months[which(master_new$No.of.times.90.DPD.or.worse.in.last.12.months=="Not 90DPD in year" )]<-  -0.3566331
master_new$No.of.times.90.DPD.or.worse.in.last.12.months[which(master_new$No.of.times.90.DPD.or.worse.in.last.12.months=="On 90DPD in year")]<- 0.5983464

## For getting the validation that whether the binning done in the No.of.times.90.DPD.or.worse.in.last.12.months
## attribute is proper or not,a logistic regression model using No.of.times.90.DPD.or.worse.in.last.12.months
## attribute with the WOE values need to be checked for the slope of the attribute (it should 
## be 1, if the binning algorithm followed is good)

master_new$Performance.Tag<-as.factor(master_new$Performance.Tag)
master_new$No.of.times.90.DPD.or.worse.in.last.12.months<-as.numeric(master_new$No.of.times.90.DPD.or.worse.in.last.12.months)

model_90DPD12<-glm(formula=Performance.Tag~No.of.times.90.DPD.or.worse.in.last.12.months, data= master_new, family="binomial")
summary((model_90DPD12)) # slope of the equation = 1


## -------Binning the No.of.times.60.DPD.or.worse.in.last.12.months Attribute---------

d<-iv.binning.simple(master_new, "No.of.times.60.DPD.or.worse.in.last.12.months", by= 1/2)

nrow(subset(master_new, master_new$No.of.times.60.DPD.or.worse.in.last.12.months<=0))
nrow(subset(master_new, master_new$No.of.times.60.DPD.or.worse.in.last.12.months>0))

master_new$No.of.times.60.DPD.or.worse.in.last.12.months[which(master_new$No.of.times.60.DPD.or.worse.in.last.12.months==0)]<-"Not 60DPD in year"
master_new$No.of.times.60.DPD.or.worse.in.last.12.months[which(master_new$No.of.times.60.DPD.or.worse.in.last.12.months>=1 & master_new$No.of.times.60.DPD.or.worse.in.last.12.months!="Not 60DPD in year")] <- "On 60DPD in year"

## In order to check WOE for the prepared bins for No.of.times.60.DPD.or.worse.in.last.12.months attribute

master_new$Performance.Tag<-unfactor(master_new$Performance.Tag)

IV_60DPD12months<-create_infotables(data=master_new[,-1], y="Performance.Tag", bins=5, parallel=FALSE)

IV_60DPD12months$Tables$No.of.times.60.DPD.or.worse.in.last.12.months
plot_infotables(IV_60DPD12months, "No.of.times.60.DPD.or.worse.in.last.12.months")

## Replacement of the bins of the No.of.times.60.DPD.or.worse.in.last.12.months attribute with their respective WOE values

master_new$No.of.times.60.DPD.or.worse.in.last.12.months[which(master_new$No.of.times.60.DPD.or.worse.in.last.12.months=="Not 60DPD in year" )]<-  -0.3519211
master_new$No.of.times.60.DPD.or.worse.in.last.12.months[which(master_new$No.of.times.60.DPD.or.worse.in.last.12.months=="On 60DPD in year")]<- 0.4627493

## For getting the validation that whether the binning done in the No.of.times.90.DPD.or.worse.in.last.6.months
## attribute is proper or not,a logistic regression model using No.of.times.90.DPD.or.worse.in.last.6.months
## attribute with the WOE values need to be checked for the slope of the attribute (it should 
## be 1, if the binning algorithm followed is good)

master_new$Performance.Tag<-as.factor(master_new$Performance.Tag)
master_new$No.of.times.60.DPD.or.worse.in.last.12.months<-as.numeric(master_new$No.of.times.60.DPD.or.worse.in.last.12.months)

model_60DPD12<-glm(formula=Performance.Tag~No.of.times.60.DPD.or.worse.in.last.12.months, data= master_new, family="binomial")
summary((model_60DPD12)) # slope of the equation = 1


## -------Binning the No.of.times.30.DPD.or.worse.in.last.12.months Attribute---------

d<-iv.binning.simple(master_new, "No.of.times.30.DPD.or.worse.in.last.12.months", by= 1/2)

nrow(subset(master_new, master_new$No.of.times.30.DPD.or.worse.in.last.12.months<=0))
nrow(subset(master_new, master_new$No.of.times.30.DPD.or.worse.in.last.12.months>0))

master_new$No.of.times.30.DPD.or.worse.in.last.12.months[which(master_new$No.of.times.30.DPD.or.worse.in.last.12.months==0)]<-"Not 30DPD in year"
master_new$No.of.times.30.DPD.or.worse.in.last.12.months[which(master_new$No.of.times.30.DPD.or.worse.in.last.12.months>=1 & master_new$No.of.times.30.DPD.or.worse.in.last.12.months!="Not 30DPD in year")] <- "On 30DPD in year"

## In order to check WOE for the prepared bins for No.of.times.30.DPD.or.worse.in.last.12.months attribute

master_new$Performance.Tag<-unfactor(master_new$Performance.Tag)

IV_30DPD12months<-create_infotables(data=master_new[,-1], y="Performance.Tag", bins=5, parallel=FALSE)

IV_30DPD12months$Tables$No.of.times.30.DPD.or.worse.in.last.12.months
plot_infotables(IV_30DPD12months, "No.of.times.30.DPD.or.worse.in.last.12.months")

## Replacement of the bins of the No.of.times.30.DPD.or.worse.in.last.12.months attribute with their respective WOE values

master_new$No.of.times.30.DPD.or.worse.in.last.12.months[which(master_new$No.of.times.30.DPD.or.worse.in.last.12.months=="Not 30DPD in year" )]<-  -0.3763960
master_new$No.of.times.30.DPD.or.worse.in.last.12.months[which(master_new$No.of.times.30.DPD.or.worse.in.last.12.months=="On 30DPD in year")]<- 0.4600653

## For getting the validation that whether the binning done in the No.of.times.30.DPD.or.worse.in.last.12.months
## attribute is proper or not,a logistic regression model using No.of.times.30.DPD.or.worse.in.last.12.months
## attribute with the WOE values need to be checked for the slope of the attribute (it should 
## be 1, if the binning algorithm followed is good)

master_new$Performance.Tag<-as.factor(master_new$Performance.Tag)
master_new$No.of.times.30.DPD.or.worse.in.last.12.months<-as.numeric(master_new$No.of.times.30.DPD.or.worse.in.last.12.months)

model_30DPD12<-glm(formula=Performance.Tag~No.of.times.30.DPD.or.worse.in.last.12.months, data= master_new, family="binomial")
summary((model_30DPD12)) # slope of the equation = 1


## -------Binning the Avgas.CC.Utilization.in.last.12.months Attribute---------

d<-iv.binning.simple((subset(master_new, is.na(master_new$Avgas.CC.Utilization.in.last.12.months)==FALSE)), "Avgas.CC.Utilization.in.last.12.months", by= 1/9)

nrow(subset(master_new, master_new$Avgas.CC.Utilization.in.last.12.months<=5))
nrow(subset(master_new, master_new$Avgas.CC.Utilization.in.last.12.months>5 & master_new$Avgas.CC.Utilization.in.last.12.months<=7))
nrow(subset(master_new, master_new$Avgas.CC.Utilization.in.last.12.months>7 & master_new$Avgas.CC.Utilization.in.last.12.months<=10))
nrow(subset(master_new, master_new$Avgas.CC.Utilization.in.last.12.months>10 & master_new$Avgas.CC.Utilization.in.last.12.months<=12))
nrow(subset(master_new, master_new$Avgas.CC.Utilization.in.last.12.months>12 & master_new$Avgas.CC.Utilization.in.last.12.months<=16))
nrow(subset(master_new, master_new$Avgas.CC.Utilization.in.last.12.months>16 & master_new$Avgas.CC.Utilization.in.last.12.months<=25))
nrow(subset(master_new, master_new$Avgas.CC.Utilization.in.last.12.months>25 & master_new$Avgas.CC.Utilization.in.last.12.months<=42))
nrow(subset(master_new, master_new$Avgas.CC.Utilization.in.last.12.months>42 & master_new$Avgas.CC.Utilization.in.last.12.months<=57))
nrow(subset(master_new, master_new$Avgas.CC.Utilization.in.last.12.months>57 & master_new$Avgas.CC.Utilization.in.last.12.months<=85))
nrow(subset(master_new, master_new$Avgas.CC.Utilization.in.last.12.months>85 & master_new$Avgas.CC.Utilization.in.last.12.months<=113))
sum(is.na(master_new$Avgas.CC.Utilization.in.last.12.months))

master_new$Avgas.CC.Utilization.in.last.12.months<-as.factor(master_new$Avgas.CC.Utilization.in.last.12.months)

levels(master_new$Avgas.CC.Utilization.in.last.12.months)[1:6]<-"[0,5]"
levels(master_new$Avgas.CC.Utilization.in.last.12.months)[2:3]<-"(5,7]"
levels(master_new$Avgas.CC.Utilization.in.last.12.months)[3:5]<-"(7,10]"
levels(master_new$Avgas.CC.Utilization.in.last.12.months)[4:5]<-"(10,12]"
levels(master_new$Avgas.CC.Utilization.in.last.12.months)[5:8]<-"(12,16]"
levels(master_new$Avgas.CC.Utilization.in.last.12.months)[6:14]<-"(16,25]"
levels(master_new$Avgas.CC.Utilization.in.last.12.months)[7:23]<-"(25,42]"
levels(master_new$Avgas.CC.Utilization.in.last.12.months)[8:22]<-"(42,57]"
levels(master_new$Avgas.CC.Utilization.in.last.12.months)[9:36]<-"(57,85]"
levels(master_new$Avgas.CC.Utilization.in.last.12.months)[10:37]<-"(85,113]"

## In order to check WOE for the prepared bins for Avgas.CC.Utilization.in.last.12.months attribute

master_new$Performance.Tag<-unfactor(master_new$Performance.Tag)

IV_AvgCCutil<-create_infotables(data=master_new[,-1], y="Performance.Tag", bins=5, parallel=FALSE)

IV_AvgCCutil$Tables$Avgas.CC.Utilization.in.last.12.months
plot_infotables(IV_AvgCCutil, "Avgas.CC.Utilization.in.last.12.months")

## Replacement of the bins of the Avgas.CC.Utilization.in.last.12.months attribute with their respective WOE values

master_new$Avgas.CC.Utilization.in.last.12.months<-unfactor(master_new$Avgas.CC.Utilization.in.last.12.months)

master_new$Avgas.CC.Utilization.in.last.12.months[which(master_new$Avgas.CC.Utilization.in.last.12.months=="[0,5]" )]<- -0.8721315
master_new$Avgas.CC.Utilization.in.last.12.months[which(master_new$Avgas.CC.Utilization.in.last.12.months=="(5,7]")]<-  -0.7784605
master_new$Avgas.CC.Utilization.in.last.12.months[which(master_new$Avgas.CC.Utilization.in.last.12.months=="(7,10]")]<- -0.7575516
master_new$Avgas.CC.Utilization.in.last.12.months[which(master_new$Avgas.CC.Utilization.in.last.12.months=="(10,12]")]<- -0.5005034
master_new$Avgas.CC.Utilization.in.last.12.months[which(master_new$Avgas.CC.Utilization.in.last.12.months=="(12,16]")]<- -0.4170280
master_new$Avgas.CC.Utilization.in.last.12.months[which(master_new$Avgas.CC.Utilization.in.last.12.months=="(16,25]")]<- 0.2078188
master_new$Avgas.CC.Utilization.in.last.12.months[which(master_new$Avgas.CC.Utilization.in.last.12.months=="(25,42]")]<- 0.5327389
master_new$Avgas.CC.Utilization.in.last.12.months[which(master_new$Avgas.CC.Utilization.in.last.12.months=="(42,57]")]<- 0.5920429
master_new$Avgas.CC.Utilization.in.last.12.months[which(master_new$Avgas.CC.Utilization.in.last.12.months=="(57,85]")]<- 0.5718882
master_new$Avgas.CC.Utilization.in.last.12.months[which(master_new$Avgas.CC.Utilization.in.last.12.months=="(85,113]")]<- 0.2441514
master_new$Avgas.CC.Utilization.in.last.12.months[which(is.na(master_new$Avgas.CC.Utilization.in.last.12.months)==TRUE)]<- 0.1114737


## For getting the validation that whether the binning done in the Avgas.CC.Utilization.in.last.12.months
## attribute is proper or not,a logistic regression model using Avgas.CC.Utilization.in.last.12.months
## attribute with the WOE values need to be checked for the slope of the attribute (it should 
## be 1, if the binning algorithm followed is good)

master_new$Performance.Tag<-as.factor(master_new$Performance.Tag)

master_new$Avgas.CC.Utilization.in.last.12.months<-as.numeric(master_new$Avgas.CC.Utilization.in.last.12.months)

model_AvgCCutil<-glm(formula=Performance.Tag~Avgas.CC.Utilization.in.last.12.months, data= master_new, family="binomial")
summary((model_AvgCCutil)) # slope of the equation = 1


## -------Binning the No.of.trades.opened.in.last.6.months Attribute---------

e<-iv.binning.simple((subset(master_new, is.na(master_new$No.of.trades.opened.in.last.6.months)==FALSE)), "No.of.trades.opened.in.last.6.months", by= 1/4)

nrow(subset(master_new, master_new$No.of.trades.opened.in.last.6.months<=1))
nrow(subset(master_new, master_new$No.of.trades.opened.in.last.6.months>1 & master_new$No.of.trades.opened.in.last.6.months<=2))
nrow(subset(master_new, master_new$No.of.trades.opened.in.last.6.months>2 & master_new$No.of.trades.opened.in.last.6.months<=3))
nrow(subset(master_new, master_new$No.of.trades.opened.in.last.6.months>3 & master_new$No.of.trades.opened.in.last.6.months<=6))
nrow(subset(master_new, master_new$No.of.trades.opened.in.last.6.months>6 & master_new$No.of.trades.opened.in.last.6.months<=12))
sum(is.na(master_new$No.of.trades.opened.in.last.6.months))

master_new$No.of.trades.opened.in.last.6.months<-as.factor(master_new$No.of.trades.opened.in.last.6.months)


levels(master_new$No.of.trades.opened.in.last.6.months)[1:2]<-"[0,1]"
levels(master_new$No.of.trades.opened.in.last.6.months)[2]<-"(1,2]"
levels(master_new$No.of.trades.opened.in.last.6.months)[3]<-"(2,3]"
levels(master_new$No.of.trades.opened.in.last.6.months)[4:6]<-"(3,6]"
levels(master_new$No.of.trades.opened.in.last.6.months)[5:10]<-"(6,12]"


## In order to check WOE for the prepared bins for No.of.trades.opened.in.last.6.months attribute

master_new$Performance.Tag<-unfactor(master_new$Performance.Tag)

IV_trades6<-create_infotables(data=master_new[,-1], y="Performance.Tag", bins=5, parallel=FALSE)

IV_trades6$Tables$No.of.trades.opened.in.last.6.months
plot_infotables(IV_trades6, "No.of.trades.opened.in.last.6.months")

## Replacement of the bins of the No.of.trades.opened.in.last.6.months attribute with their respective WOE values

master_new$No.of.trades.opened.in.last.6.months<-unfactor(master_new$No.of.trades.opened.in.last.6.months)

master_new$No.of.trades.opened.in.last.6.months[which(master_new$No.of.trades.opened.in.last.6.months=="[0,1]" )]<-  -0.54323458
master_new$No.of.trades.opened.in.last.6.months[which(master_new$No.of.trades.opened.in.last.6.months=="(1,2]" )]<-  0.23286102
master_new$No.of.trades.opened.in.last.6.months[which(master_new$No.of.trades.opened.in.last.6.months=="(2,3]" )]<-  0.43512392
master_new$No.of.trades.opened.in.last.6.months[which(master_new$No.of.trades.opened.in.last.6.months=="(3,6]" )]<-  0.39841223
master_new$No.of.trades.opened.in.last.6.months[which(master_new$No.of.trades.opened.in.last.6.months=="(6,12]" )]<-  -0.07220746
master_new$No.of.trades.opened.in.last.6.months[which(is.na(master_new$No.of.trades.opened.in.last.6.months)==TRUE )]<-  0.00000000

## For getting the validation that whether the binning done in the No.of.trades.opened.in.last.6.months
## attribute is proper or not,a logistic regression model using No.of.trades.opened.in.last.6.months
## attribute with the WOE values need to be checked for the slope of the attribute (it should 
## be 1, if the binning algorithm followed is good)

master_new$Performance.Tag<-as.factor(master_new$Performance.Tag)
master_new$No.of.trades.opened.in.last.6.months<-as.numeric(master_new$No.of.trades.opened.in.last.6.months)

model_trades6<-glm(formula=Performance.Tag~No.of.trades.opened.in.last.6.months, data= master_new, family="binomial")
summary((model_trades6)) # slope of the equation = 1


## -------Binning the No.of.trades.opened.in.last.12.months Attribute---------

f<-iv.binning.simple(master_new, "No.of.trades.opened.in.last.12.months", by= 1/5)

nrow(subset(master_new, master_new$No.of.trades.opened.in.last.12.months<=1))
nrow(subset(master_new, master_new$No.of.trades.opened.in.last.12.months>1 & master_new$No.of.trades.opened.in.last.12.months<=3))
nrow(subset(master_new, master_new$No.of.trades.opened.in.last.12.months>3 & master_new$No.of.trades.opened.in.last.12.months<=6))
nrow(subset(master_new, master_new$No.of.trades.opened.in.last.12.months>6 & master_new$No.of.trades.opened.in.last.12.months<=9))
nrow(subset(master_new, master_new$No.of.trades.opened.in.last.12.months>9 & master_new$No.of.trades.opened.in.last.12.months<=17))
nrow(subset(master_new, master_new$No.of.trades.opened.in.last.12.months>17 & master_new$No.of.trades.opened.in.last.12.months<=28))

master_new$No.of.trades.opened.in.last.12.months<-as.factor(master_new$No.of.trades.opened.in.last.12.months)


levels(master_new$No.of.trades.opened.in.last.12.months)[1:2]<-"[0,1]"
levels(master_new$No.of.trades.opened.in.last.12.months)[2:3]<-"(1,3]"
levels(master_new$No.of.trades.opened.in.last.12.months)[3:5]<-"(3,6]"
levels(master_new$No.of.trades.opened.in.last.12.months)[4:6]<-"(6,9]"
levels(master_new$No.of.trades.opened.in.last.12.months)[5:12]<-"(9,17]"
levels(master_new$No.of.trades.opened.in.last.12.months)[6:16]<-"(17,28]"


## In order to check WOE for the prepared bins for No.of.trades.opened.in.last.12.months attribute

master_new$Performance.Tag<-unfactor(master_new$Performance.Tag)

IV_trades12<-create_infotables(data=master_new[,-1], y="Performance.Tag", bins=5, parallel=FALSE)

IV_trades12$Tables$No.of.trades.opened.in.last.12.months
plot_infotables(IV_trades12, "No.of.trades.opened.in.last.12.months")

## Replacement of the bins of the No.of.trades.opened.in.last.12.months attribute with their respective WOE values

master_new$No.of.trades.opened.in.last.12.months<-unfactor(master_new$No.of.trades.opened.in.last.12.months)

master_new$No.of.trades.opened.in.last.12.months[which(master_new$No.of.trades.opened.in.last.12.months=="[0,1]" )]<- -0.89395337
master_new$No.of.trades.opened.in.last.12.months[which(master_new$No.of.trades.opened.in.last.12.months=="(1,3]")]<- -0.46736230
master_new$No.of.trades.opened.in.last.12.months[which(master_new$No.of.trades.opened.in.last.12.months=="(3,6]")]<- 0.21899083
master_new$No.of.trades.opened.in.last.12.months[which(master_new$No.of.trades.opened.in.last.12.months=="(6,9]")]<- 0.53390532
master_new$No.of.trades.opened.in.last.12.months[which(master_new$No.of.trades.opened.in.last.12.months=="(9,17]")]<- 0.31364026
master_new$No.of.trades.opened.in.last.12.months[which(master_new$No.of.trades.opened.in.last.12.months=="(17,28]")]<- -0.07962968

## For getting the validation that whether the binning done in the No.of.trades.opened.in.last.12.months
## attribute is proper or not,a logistic regression model using No.of.trades.opened.in.last.12.months
## attribute with the WOE values need to be checked for the slope of the attribute (it should 
## be 1, if the binning algorithm followed is good)

master_new$Performance.Tag<-as.factor(master_new$Performance.Tag)
master_new$No.of.trades.opened.in.last.12.months<-as.numeric(master_new$No.of.trades.opened.in.last.12.months)

model_trades12<-glm(formula=Performance.Tag~No.of.trades.opened.in.last.12.months, data= master_new, family="binomial")
summary((model_trades12)) # slope of the equation = 1


## -------Binning the No.of.PL.trades.opened.in.last.6.months Attribute---------

g<-iv.binning.simple(master_new, "No.of.PL.trades.opened.in.last.6.months", by= 1/2)

nrow(subset(master_new, master_new$No.of.PL.trades.opened.in.last.6.months<=1))
nrow(subset(master_new, master_new$No.of.PL.trades.opened.in.last.6.months>1 & master_new$No.of.PL.trades.opened.in.last.6.months<=5))
nrow(subset(master_new, master_new$No.of.PL.trades.opened.in.last.6.months>5 & master_new$No.of.PL.trades.opened.in.last.6.months<=6))

master_new$No.of.PL.trades.opened.in.last.6.months<-as.factor(master_new$No.of.PL.trades.opened.in.last.6.months)

levels(master_new$No.of.PL.trades.opened.in.last.6.months)[1:2]<-"[0,1]"
levels(master_new$No.of.PL.trades.opened.in.last.6.months)[2:5]<-"(1,5]"
levels(master_new$No.of.PL.trades.opened.in.last.6.months)[3]<-"(5,6]"

## In order to check WOE for the prepared bins for No.of.PL.trades.opened.in.last.6.months attribute

master_new$Performance.Tag<-unfactor(master_new$Performance.Tag)

IV_PLtrades6<-create_infotables(data=master_new[,-1], y="Performance.Tag", bins=5, parallel=FALSE)

IV_PLtrades6$Tables$No.of.PL.trades.opened.in.last.6.months
plot_infotables(IV_PLtrades6, "No.of.PL.trades.opened.in.last.6.months")

## Replacement of the bins of the No.of.PL.trades.opened.in.last.6.months attribute with their respective WOE values

master_new$No.of.PL.trades.opened.in.last.6.months<-unfactor(master_new$No.of.PL.trades.opened.in.last.6.months)

master_new$No.of.PL.trades.opened.in.last.6.months[which(master_new$No.of.PL.trades.opened.in.last.6.months=="[0,1]" )]<- -0.3146596
master_new$No.of.PL.trades.opened.in.last.6.months[which(master_new$No.of.PL.trades.opened.in.last.6.months=="(1,5]")]<- 0.4077547
master_new$No.of.PL.trades.opened.in.last.6.months[which(master_new$No.of.PL.trades.opened.in.last.6.months=="(5,6]")]<- -0.4608088

## For getting the validation that whether the binning done in the No.of.PL.trades.opened.in.last.6.months
## attribute is proper or not,a logistic regression model using No.of.PL.trades.opened.in.last.6.months
## attribute with the WOE values need to be checked for the slope of the attribute (it should 
## be 1, if the binning algorithm followed is good)

master_new$Performance.Tag<-as.factor(master_new$Performance.Tag)
master_new$No.of.PL.trades.opened.in.last.6.months<-as.numeric(master_new$No.of.PL.trades.opened.in.last.6.months)

model_PLtrades6<-glm(formula=Performance.Tag~No.of.PL.trades.opened.in.last.6.months, data= master_new, family="binomial")
summary((model_PLtrades6)) # slope of the equation = 1


## -------Binning the No.of.PL.trades.opened.in.last.12.months Attribute---------

h<-iv.binning.simple(master_new, "No.of.PL.trades.opened.in.last.12.months", by= 1/2)

nrow(subset(master_new, master_new$No.of.PL.trades.opened.in.last.12.months<=2))
nrow(subset(master_new, master_new$No.of.PL.trades.opened.in.last.12.months>2 & master_new$No.of.PL.trades.opened.in.last.12.months<=10))
nrow(subset(master_new, master_new$No.of.PL.trades.opened.in.last.12.months>10 & master_new$No.of.PL.trades.opened.in.last.12.months<=12))

master_new$No.of.PL.trades.opened.in.last.12.months<-as.factor(master_new$No.of.PL.trades.opened.in.last.12.months)

levels(master_new$No.of.PL.trades.opened.in.last.12.months)[1:3]<-"[0,2]"
levels(master_new$No.of.PL.trades.opened.in.last.12.months)[2:9]<-"(2,10]"
levels(master_new$No.of.PL.trades.opened.in.last.12.months)[3:4]<-"(10,12]"

## In order to check WOE for the prepared bins for No.of.PL.trades.opened.in.last.12.months attribute

master_new$Performance.Tag<-unfactor(master_new$Performance.Tag)

IV_PLtrades12<-create_infotables(data=master_new[,-1], y="Performance.Tag", bins=5, parallel=FALSE)

IV_PLtrades12$Tables$No.of.PL.trades.opened.in.last.12.months
plot_infotables(IV_PLtrades12, "No.of.PL.trades.opened.in.last.12.months")

## Replacement of the bins of the No.of.PL.trades.opened.in.last.12.months attribute with their respective WOE values

master_new$No.of.PL.trades.opened.in.last.12.months<-unfactor(master_new$No.of.PL.trades.opened.in.last.12.months)

master_new$No.of.PL.trades.opened.in.last.12.months[which(master_new$No.of.PL.trades.opened.in.last.12.months=="[0,2]" )]<- -0.4531209
master_new$No.of.PL.trades.opened.in.last.12.months[which(master_new$No.of.PL.trades.opened.in.last.12.months=="(2,10]")]<- 0.3960410
master_new$No.of.PL.trades.opened.in.last.12.months[which(master_new$No.of.PL.trades.opened.in.last.12.months=="(10,12]")]<- 0.2323384

## For getting the validation that whether the binning done in the No.of.PL.trades.opened.in.last.12.months
## attribute is proper or not,a logistic regression model using No.of.PL.trades.opened.in.last.12.months
## attribute with the WOE values need to be checked for the slope of the attribute (it should 
## be 1, if the binning algorithm followed is good)

master_new$Performance.Tag<-as.factor(master_new$Performance.Tag)
master_new$No.of.PL.trades.opened.in.last.12.months<-as.numeric(master_new$No.of.PL.trades.opened.in.last.12.months)

model_PLtrades12<-glm(formula=Performance.Tag~No.of.PL.trades.opened.in.last.12.months, data= master_new, family="binomial")
summary((model_PLtrades12)) # slope of the equation = 1


## -------Binning the No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. Attribute---------

i<-iv.binning.simple(master_new, "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.", by= 1/2)

nrow(subset(master_new, master_new$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.<=1))
nrow(subset(master_new, master_new$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.>1 & master_new$No.of.PL.trades.opened.in.last.12.months<=7))
nrow(subset(master_new, master_new$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.>7 & master_new$No.of.PL.trades.opened.in.last.12.months<=10))

master_new$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.<-as.factor(master_new$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)

levels(master_new$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)[1:2]<-"[0,1]"
levels(master_new$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)[2:7]<-"(1,7]"
levels(master_new$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)[3:5]<-"(7,10]"

## In order to check WOE for the prepared bins for No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. attribute

master_new$Performance.Tag<-unfactor(master_new$Performance.Tag)

IV_Inquiriesin6<-create_infotables(data=master_new[,-1], y="Performance.Tag", bins=5, parallel=FALSE)

IV_Inquiriesin6$Tables$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
plot_infotables(IV_Inquiriesin6, "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.")

## Replacement of the bins of the No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. attribute with their respective WOE values

master_new$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.<-unfactor(master_new$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)

master_new$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.[which(master_new$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.=="[0,1]" )]<- -0.3200583
master_new$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.[which(master_new$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.=="(1,7]")]<-  0.3101983
master_new$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.[which(master_new$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.=="(7,10]")]<- -0.1914758

## For getting the validation that whether the binning done in the No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
## attribute is proper or not,a logistic regression model using No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
## attribute with the WOE values need to be checked for the slope of the attribute (it should 
## be 1, if the binning algorithm followed is good)

master_new$Performance.Tag<-as.factor(master_new$Performance.Tag)
master_new$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.<-as.numeric(master_new$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)

model_Inquiriesin6<-glm(formula=Performance.Tag~No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., data= master_new, family="binomial")
summary((model_Inquiriesin6)) # slope of the equation = 1


## -------Binning the No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. Attribute---------

j<-iv.binning.simple(master_new, "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.", by= 1/3)

nrow(subset(master_new, master_new$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.<=1))
nrow(subset(master_new, master_new$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.>1 & master_new$No.of.PL.trades.opened.in.last.12.months<=4))
nrow(subset(master_new, master_new$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.>4 & master_new$No.of.PL.trades.opened.in.last.12.months<=12))
nrow(subset(master_new, master_new$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.>12 & master_new$No.of.PL.trades.opened.in.last.12.months<=20))

master_new$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.<-as.factor(master_new$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)

levels(master_new$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)[1:2]<-"[0,1]"
levels(master_new$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)[2:4]<-"(1,4]"
levels(master_new$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)[3:10]<-"(4,12]"
levels(master_new$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)[4:11]<-"(12,20]"

## In order to check WOE for the prepared bins for No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. attribute

master_new$Performance.Tag<-unfactor(master_new$Performance.Tag)

IV_Inquiriesin12<-create_infotables(data=master_new[,-1], y="Performance.Tag", bins=5, parallel=FALSE)

IV_Inquiriesin12$Tables$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.
plot_infotables(IV_Inquiriesin12, "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.")

## Replacement of the bins of the No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. attribute with their respective WOE values

master_new$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.<-unfactor(master_new$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)

master_new$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.[which(master_new$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.=="[0,1]" )]<- -0.8282838
master_new$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.[which(master_new$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.=="(1,4]")]<-  0.1827363
master_new$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.[which(master_new$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.=="(4,12]")]<- 0.4136663
master_new$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.[which(master_new$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.=="(12,20]")]<- -0.1807082

## For getting the validation that whether the binning done in the No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.
## attribute is proper or not,a logistic regression model using No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.
## attribute with the WOE values need to be checked for the slope of the attribute (it should 
## be 1, if the binning algorithm followed is good)

master_new$Performance.Tag<-as.factor(master_new$Performance.Tag)
master_new$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.<-as.numeric(master_new$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)

model_Inquiriesin12<-glm(formula=Performance.Tag~No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., data= master_new, family="binomial")
summary((model_Inquiriesin12)) # slope of the equation = 1


## -------Binning the Outstanding.Balance Attribute---------

k<-iv.binning.simple(subset(master_new, is.na(master_new$Outstanding.Balance)==FALSE), "Outstanding.Balance", by= 1/8)


nrow(subset(master_new, master_new$Outstanding.Balance<=1.05e+04))
nrow(subset(master_new, master_new$Outstanding.Balance>1.05e+04 & master_new$Outstanding.Balance<=2.13e+05))
nrow(subset(master_new, master_new$Outstanding.Balance>2.13e+05 & master_new$Outstanding.Balance<=5.69e+05))
nrow(subset(master_new, master_new$Outstanding.Balance>5.69e+05 & master_new$Outstanding.Balance<=7.78e+05))
nrow(subset(master_new, master_new$Outstanding.Balance>7.78e+05 & master_new$Outstanding.Balance<=1.02e+06))
nrow(subset(master_new, master_new$Outstanding.Balance>1.02e+06 & master_new$Outstanding.Balance<=2.93e+06))
nrow(subset(master_new, master_new$Outstanding.Balance>2.93e+06 & master_new$Outstanding.Balance<=3.12e+06))
nrow(subset(master_new, master_new$Outstanding.Balance>3.12e+06 & master_new$Outstanding.Balance<=5.22e+06))

master_new$Outstanding.Balance<-as.factor(master_new$Outstanding.Balance)


levels(master_new$Outstanding.Balance)[1:5779]<-"[0,1.05e+04]" 
levels(master_new$Outstanding.Balance)[2:7627]<-"(1.05e+04,2.13e+05]"
levels(master_new$Outstanding.Balance)[3:8535]<-"(2.13e+05,5.69e+05]"
levels(master_new$Outstanding.Balance)[4:8344]<-"(5.69e+05,7.78e+05]"
levels(master_new$Outstanding.Balance)[5:8433]<-"(7.78e+05,1.02e+06]"
levels(master_new$Outstanding.Balance)[6:8482]<-"(1.02e+06,2.93e+06]"
levels(master_new$Outstanding.Balance)[7:8133]<-"(2.93e+06,3.12e+06]"
levels(master_new$Outstanding.Balance)[8:8639]<-"(3.12e+06,5.22e+06]"


## In order to check WOE for the prepared bins for Outstanding.Balance attribute

master_new$Performance.Tag<-unfactor(master_new1$Performance.Tag)

IV_outbal<-create_infotables(data=master_new[,-1], y="Performance.Tag", bins=5, parallel=FALSE)

IV_outbal$Tables$Outstanding.Balance
plot_infotables(IV_outbal, "Outstanding.Balance")

## Replacement of the bins of the Outstanding.Balance attribute with their respective WOE values

master_new$Outstanding.Balance<-unfactor(master_new$Outstanding.Balance)

master_new$Outstanding.Balance[which(master_new$Outstanding.Balance=="[0,1.05e+04]" )]<- -0.8308386
master_new$Outstanding.Balance[which(master_new$Outstanding.Balance=="(1.05e+04,2.13e+05]")]<-  -0.6279279
master_new$Outstanding.Balance[which(master_new$Outstanding.Balance=="(2.13e+05,5.69e+05]")]<- 0.2505438
master_new$Outstanding.Balance[which(master_new$Outstanding.Balance=="(5.69e+05,7.78e+05]")]<- 0.4024312
master_new$Outstanding.Balance[which(master_new$Outstanding.Balance=="(7.78e+05,1.02e+06]")]<- 0.3781004
master_new$Outstanding.Balance[which(master_new$Outstanding.Balance=="(1.02e+06,2.93e+06]")]<- 0.2720899
master_new$Outstanding.Balance[which(master_new$Outstanding.Balance=="(2.93e+06,3.12e+06]")]<- -1.0101426
master_new$Outstanding.Balance[which(master_new$Outstanding.Balance=="(3.12e+06,5.22e+06]")]<- 0.2070019
master_new$Outstanding.Balance[which(is.na(master_new$Outstanding.Balance)==TRUE)]<-  -0.3737974

## For getting the validation that whether the binning done in the Outstanding.Balance
## attribute is proper or not,a logistic regression model using Outstanding.Balance
## attribute with the WOE values need to be checked for the slope of the attribute (it should 
## be 1, if the binning algorithm followed is good)

master_new$Performance.Tag<-as.factor(master_new$Performance.Tag)
master_new$Outstanding.Balance<-as.numeric(master_new$Outstanding.Balance)

model_outbal<-glm(formula=Performance.Tag~Outstanding.Balance, data= master_new, family="binomial")
summary((model_outbal)) # slope of the equation = 1


## -------Binning the Total.No.of.Trades Attribute---------

l<-iv.binning.simple(master_new, "Total.No.of.Trades", by= 1/9)

nrow(subset(master_new, master_new$Total.No.of.Trades<=2))
nrow(subset(master_new, master_new$Total.No.of.Trades>2 & master_new$Total.No.of.Trades<=3))
nrow(subset(master_new, master_new$Total.No.of.Trades>3 & master_new$Total.No.of.Trades<=4))
nrow(subset(master_new, master_new$Total.No.of.Trades>4 & master_new$Total.No.of.Trades<=5))
nrow(subset(master_new, master_new$Total.No.of.Trades>5 & master_new$Total.No.of.Trades<=6))
nrow(subset(master_new, master_new$Total.No.of.Trades>6 & master_new$Total.No.of.Trades<=7))
nrow(subset(master_new, master_new$Total.No.of.Trades>7 & master_new$Total.No.of.Trades<=9))
nrow(subset(master_new, master_new$Total.No.of.Trades>9 & master_new$Total.No.of.Trades<=11))
nrow(subset(master_new, master_new$Total.No.of.Trades>11 & master_new$Total.No.of.Trades<=18))
nrow(subset(master_new, master_new$Total.No.of.Trades>18 & master_new$Total.No.of.Trades<=44))

master_new$Total.No.of.Trades<-as.factor(master_new$Total.No.of.Trades)

levels(master_new$Total.No.of.Trades)[1:3]<-"[0,2]"
levels(master_new$Total.No.of.Trades)[2]<-"(2,3]"
levels(master_new$Total.No.of.Trades)[3]<-"(3,4]"
levels(master_new$Total.No.of.Trades)[4]<-"(4,5]"
levels(master_new$Total.No.of.Trades)[5]<-"(5,6]"
levels(master_new$Total.No.of.Trades)[6]<-"(6,7]"
levels(master_new$Total.No.of.Trades)[7:8]<-"(7,9]"
levels(master_new$Total.No.of.Trades)[8:9]<-"(9,11]"
levels(master_new$Total.No.of.Trades)[9:15]<-"(11,18]"
levels(master_new$Total.No.of.Trades)[10:35]<-"(18,44]"

## In order to check WOE for the prepared bins for Total.No.of.Trades attribute

master_new$Performance.Tag<-unfactor(master_new$Performance.Tag)

IV_totaltrades<-create_infotables(data=master_new[,-1], y="Performance.Tag", bins=5, parallel=FALSE)

IV_totaltrades$Tables$Total.No.of.Trades
plot_infotables(IV_totaltrades, "Total.No.of.Trades")

## Replacement of the bins of the Total.No.of.Trades attribute with their respective WOE values

master_new$Total.No.of.Trades<-unfactor(master_new$Total.No.of.Trades)

master_new$Total.No.of.Trades[which(master_new$Total.No.of.Trades=="[0,2]" )]<- -0.87778148
master_new$Total.No.of.Trades[which(master_new$Total.No.of.Trades=="(2,3]")]<-  -0.70202474
master_new$Total.No.of.Trades[which(master_new$Total.No.of.Trades=="(3,4]")]<- -0.44785257
master_new$Total.No.of.Trades[which(master_new$Total.No.of.Trades=="(4,5]")]<- -0.04880056
master_new$Total.No.of.Trades[which(master_new$Total.No.of.Trades=="(5,6]")]<- 0.12930127
master_new$Total.No.of.Trades[which(master_new$Total.No.of.Trades=="(6,7]")]<- 0.29649292
master_new$Total.No.of.Trades[which(master_new$Total.No.of.Trades=="(7,9]")]<- 0.51261494
master_new$Total.No.of.Trades[which(master_new$Total.No.of.Trades=="(9,11]")]<- 0.48924883
master_new$Total.No.of.Trades[which(master_new$Total.No.of.Trades=="(11,18]")]<- 0.46477123
master_new$Total.No.of.Trades[which(master_new$Total.No.of.Trades=="(18,44]")]<- -0.08113362

## For getting the validation that whether the binning done in the Total.No.of.Trades
## attribute is proper or not,a logistic regression model using Total.No.of.Trades
## attribute with the WOE values need to be checked for the slope of the attribute (it should 
## be 1, if the binning algorithm followed is good)

master_new$Performance.Tag<-as.factor(master_new$Performance.Tag)
master_new$Total.No.of.Trades<-as.numeric(master_new$Total.No.of.Trades)

model_totaltrades<-glm(formula=Performance.Tag~Total.No.of.Trades, data= master_new, family="binomial")
summary((model_totaltrades)) # slope of the equation = 1


## -------WOE of Gender Attribute---------


## In order to check WOE for the categories of Gender attribute

master_new$Performance.Tag<-unfactor(master_new$Performance.Tag)

IV_Gender<-create_infotables(data=master_new[,-1], y="Performance.Tag", bins=5, parallel=FALSE)

IV_Gender$Tables$Gender
plot_infotables(IV_Gender, "Gender")

## Replacement of the categories of the Gender attribute with their respective WOE values

master_new$Gender<-as.character(master_new$Gender)

master_new$Gender[which(master_new$Gender=="F" )]<-  0.0321743
master_new$Gender[which(master_new$Gender=="M")]<-  -0.0101473

## For getting the validation that whether the categories in the Gender
## attribute is proper or not,a logistic regression model using Gender
## attribute with the WOE values need to be checked for the slope of the attribute (it should 
## be 1, if the binning algorithm followed is good)

master_new$Performance.Tag<-as.factor(master_new$Performance.Tag)
master_new$Gender<-as.numeric(master_new$Gender)

model_Gender<-glm(formula=Performance.Tag~Gender, data= master_new, family="binomial")
summary((model_Gender)) # slope of the equation = 1


## -------WOE of Marital.Status..at.the.time.of.application. Attribute---------


## In order to check WOE for the categories of Marital.Status..at.the.time.of.application. attribute

master_new$Performance.Tag<-unfactor(master_new$Performance.Tag)

IV_Maritalstatus<-create_infotables(data=master_new[,-1], y="Performance.Tag", bins=5, parallel=FALSE)

IV_Maritalstatus$Tables$Marital.Status..at.the.time.of.application.
plot_infotables(IV_Maritalstatus, "Marital.Status..at.the.time.of.application.")

## Replacement of the categories of the Marital.Status..at.the.time.of.application. attribute with their respective WOE values

master_new$Marital.Status..at.the.time.of.application.<-as.character(master_new$Marital.Status..at.the.time.of.application.)

master_new$Marital.Status..at.the.time.of.application.[which(master_new$Marital.Status..at.the.time.of.application.=="Married" )]<-  -0.004092434
master_new$Marital.Status..at.the.time.of.application.[which(master_new$Marital.Status..at.the.time.of.application.=="Single")]<-  0.023326708

## For getting the validation that whether the categories in the Marital.Status..at.the.time.of.application.
## attribute is proper or not,a logistic regression model using Marital.Status..at.the.time.of.application.
## attribute with the WOE values need to be checked for the slope of the attribute (it should 
## be 1, if the binning algorithm followed is good)

master_new$Performance.Tag<-as.factor(master_new$Performance.Tag)
master_new$Marital.Status..at.the.time.of.application.<-as.numeric(master_new$Marital.Status..at.the.time.of.application.)

model_Maritalstatus<-glm(formula=Performance.Tag~Marital.Status..at.the.time.of.application., data= master_new, family="binomial")
summary((model_Maritalstatus)) # slope of the equation = 1


## -------WOE of Education Attribute---------


## In order to check WOE for the categories of Education attribute

master_new$Performance.Tag<-unfactor(master_new$Performance.Tag)

IV_Education<-create_infotables(data=master_new[,-1], y="Performance.Tag", bins=5, parallel=FALSE)

IV_Education$Tables$Education
plot_infotables(IV_Education, "Education")

## Replacement of the categories of the Education attribute with their respective WOE values

master_new$Education<-as.character(master_new$Education)

master_new$Education[which(master_new$Education=="Bachelor" )]<-  0.017313988
master_new$Education[which(master_new$Education=="Masters")]<-  0.007948702
master_new$Education[which(master_new$Education=="Others")]<-  0.492621513
master_new$Education[which(master_new$Education=="Phd")]<-  -0.029511963
master_new$Education[which(master_new$Education=="Professional")]<-  -0.017820977

## For getting the validation that whether the categories in the Education
## attribute is proper or not,a logistic regression model using Education
## attribute with the WOE values need to be checked for the slope of the attribute (it should 
## be 1, if the binning algorithm followed is good)

master_new$Performance.Tag<-as.factor(master_new$Performance.Tag)
master_new$Education<-as.numeric(master_new$Education)

model_Education<-glm(formula=Performance.Tag~Education, data= master_new, family="binomial")
summary((model_Education)) # slope of the equation = 1


## -------WOE of Profession Attribute---------


## In order to check WOE for the categories of Profession attribute

master_new$Performance.Tag<-unfactor(master_new$Performance.Tag)

IV_Profession<-create_infotables(data=master_new[,-1], y="Performance.Tag", bins=5, parallel=FALSE)

IV_Profession$Tables$Profession
plot_infotables(IV_Profession, "Profession")

## Replacement of the categories of the Profession attribute with their respective WOE values

master_new$Profession<-as.character(master_new$Profession)

master_new$Profession[which(master_new$Profession=="SAL" )]<-  -0.02840853
master_new$Profession[which(master_new$Profession=="SE")]<-  0.09142405
master_new$Profession[which(master_new$Profession=="SE_PROF")]<-  -0.01329769

## For getting the validation that whether the categories in the Profession
## attribute is proper or not,a logistic regression model using Profession
## attribute with the WOE values need to be checked for the slope of the attribute (it should 
## be 1, if the binning algorithm followed is good)

master_new$Performance.Tag<-as.factor(master_new$Performance.Tag)
master_new$Profession<-as.numeric(master_new$Profession)

model_Profession<-glm(formula=Performance.Tag~Profession, data= master_new, family="binomial")
summary((model_Profession)) # slope of the equation = 1


## -------WOE of Type.of.residence Attribute---------


## In order to check WOE for the categories of Type.of.residence attribute

master_new$Performance.Tag<-unfactor(master_new$Performance.Tag)

IV_Type.of.residence<-create_infotables(data=master_new[,-1], y="Performance.Tag", bins=5, parallel=FALSE)

IV_Type.of.residence$Tables$Type.of.residence
plot_infotables(IV_Type.of.residence, "Type.of.residence")

## Replacement of the categories of the Type.of.residence attribute with their respective WOE values

master_new$Type.of.residence<-as.character(master_new$Type.of.residence)

master_new$Type.of.residence[which(master_new$Type.of.residence=="Company provided" )]<-  0.080146599
master_new$Type.of.residence[which(master_new$Type.of.residence=="Living with Parents")]<-  0.067530440
master_new$Type.of.residence[which(master_new$Type.of.residence=="Others")]<-  -0.530542104
master_new$Type.of.residence[which(master_new$Type.of.residence=="Owned")]<-  0.004148595
master_new$Type.of.residence[which(master_new$Type.of.residence=="Rented")]<-  -0.004453727

## For getting the validation that whether the categories in the Type.of.residence
## attribute is proper or not,a logistic regression model using Type.of.residence
## attribute with the WOE values need to be checked for the slope of the attribute (it should 
## be 1, if the binning algorithm followed is good)

master_new$Performance.Tag<-as.factor(master_new$Performance.Tag)
master_new$Type.of.residence<-as.numeric(master_new$Type.of.residence)

model_Type.of.residence<-glm(formula=Performance.Tag~Type.of.residence, data= master_new, family="binomial")
summary((model_Type.of.residence)) # slope of the equation = 1


## -------WOE of Presence.of.open.home.loan Attribute---------


## In order to check WOE for the categories of Presence.of.open.home.loan attribute

master_new$Performance.Tag<-unfactor(master_new$Performance.Tag)

IV_Presence.of.open.home.loan<-create_infotables(data=master_new[,-1], y="Performance.Tag", bins=5, parallel=FALSE)

IV_Presence.of.open.home.loan$Tables$Presence.of.open.home.loan
plot_infotables(IV_Presence.of.open.home.loan, "Presence.of.open.home.loan")

## Replacement of the categories of the Presence.of.open.home.loan attribute with their respective WOE values

master_new$Presence.of.open.home.loan<-as.character(master_new$Presence.of.open.home.loan)

master_new$Presence.of.open.home.loan[which(master_new$Presence.of.open.home.loan=="0" )]<-  0.07370543
master_new$Presence.of.open.home.loan[which(master_new$Presence.of.open.home.loan=="1")]<-  -0.23665793
master_new$Presence.of.open.home.loan[which(is.na(master_new$Presence.of.open.home.loan)==TRUE)]<-  -0.37379739

## For getting the validation that whether the categories in the Presence.of.open.home.loan
## attribute is proper or not,a logistic regression model using Presence.of.open.home.loan
## attribute with the WOE values need to be checked for the slope of the attribute (it should 
## be 1, if the binning algorithm followed is good)

master_new$Performance.Tag<-as.factor(master_new$Performance.Tag)
master_new$Presence.of.open.home.loan<-as.numeric(master_new$Presence.of.open.home.loan)

model_Presence.of.open.home.loan<-glm(formula=Performance.Tag~Presence.of.open.home.loan, data= master_new, family="binomial")
summary((model_Presence.of.open.home.loan)) # slope of the equation = 1


## -------WOE of Presence.of.open.auto.loan Attribute---------


## In order to check WOE for the categories of Presence.of.open.auto.loan attribute

master_new$Performance.Tag<-unfactor(master_new$Performance.Tag)

IV_Presence.of.open.auto.loan<-create_infotables(data=master_new[,-1], y="Performance.Tag", bins=5, parallel=FALSE)

IV_Presence.of.open.auto.loan$Tables$Presence.of.open.auto.loan
plot_infotables(IV_Presence.of.open.auto.loan, "Presence.of.open.auto.loan")

## Replacement of the categories of the Presence.of.open.auto.loan attribute with their respective WOE values

master_new$Presence.of.open.auto.loan<-as.character(master_new$Presence.of.open.auto.loan)

master_new$Presence.of.open.auto.loan[which(master_new$Presence.of.open.auto.loan=="0" )]<-  0.01198467
master_new$Presence.of.open.auto.loan[which(master_new$Presence.of.open.auto.loan=="1")]<-  -0.13836752

## For getting the validation that whether the categories in the Presence.of.open.auto.loan
## attribute is proper or not,a logistic regression model using Presence.of.open.auto.loan
## attribute with the WOE values need to be checked for the slope of the attribute (it should 
## be 1, if the binning algorithm followed is good)

master_new$Performance.Tag<-as.factor(master_new$Performance.Tag)
master_new$Presence.of.open.auto.loan<-as.numeric(master_new$Presence.of.open.auto.loan)

model_Presence.of.open.auto.loan<-glm(formula=Performance.Tag~Presence.of.open.auto.loan, data= master_new, family="binomial")
summary((model_Presence.of.open.auto.loan)) # slope of the equation = 1


# ##-------------------IV Calculation------------------

master_new$Performance.Tag<-unfactor(master_new$Performance.Tag)

IV_main<-create_infotables(data=master_new[,-1], y="Performance.Tag", parallel = FALSE)

IV_main$Summary

## Following is the ordered list of the attributes on the basis of their decreasing IV values,
## and their predictiveness efficiency based on the thumb rule for IV analysis.

## "Attribute no."                              "Attribute"                "IV"       "Variable 
##                                                                                  Predictiveness"

## 17                          Avgas.CC.Utilization.in.last.12.months  3.231979e-01    Strong 
## 19                           No.of.trades.opened.in.last.12.months  2.514371e-01    Medium 
## 25                                             Outstanding.Balance  2.462796e-01    Medium 
## 23 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.  2.381895e-01    Medium 
## 26                                              Total.No.of.Trades  2.376002e-01    Medium 
## 13                    No.of.times.30.DPD.or.worse.in.last.6.months  2.340875e-01    Medium
## 14                   No.of.times.90.DPD.or.worse.in.last.12.months  2.096783e-01    Medium
## 12                    No.of.times.60.DPD.or.worse.in.last.6.months  2.058259e-01    Medium
## 18                            No.of.trades.opened.in.last.6.months  1.827825e-01    Medium
## 21                        No.of.PL.trades.opened.in.last.12.months  1.766843e-01    Medium
## 16                   No.of.times.30.DPD.or.worse.in.last.12.months  1.707107e-01    Medium
## 15                   No.of.times.60.DPD.or.worse.in.last.12.months  1.606770e-01    Medium
## 11                    No.of.times.90.DPD.or.worse.in.last.6.months  1.601060e-01    Medium
## 20                         No.of.PL.trades.opened.in.last.6.months  1.273828e-01    Medium
## 22  No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.  9.731971e-02    Weak
## 9                                No.of.months.in.current.residence  5.205295e-02    Weak
## 5                                                           Income  3.813776e-02    Weak
## 24                                      Presence.of.open.home.loan  1.761939e-02    Very Weak
## 10                                 No.of.months.in.current.company  1.442268e-02    Very Weak
## 7                                                       Profession  2.230788e-03    Very Weak
## 27                                      Presence.of.open.auto.loan  1.658061e-03    Very Weak
## 1                                                              Age  1.184605e-03    Very Weak
## 4                                                 No.of.dependents  9.617601e-04    Very Weak
## 8                                                Type.of.residence  9.208511e-04    Very Weak
## 6                                                        Education  7.816743e-04    Very Weak
## 2                                                           Gender  3.264734e-04    Very Weak
## 3                      Marital.Status..at.the.time.of.application.  9.546226e-05    Very Weak


### ------------------------- PREPARATION of REJECTED APPLICANT DATA----------------

##-------Binning the AGE Attribute------------------


master_file_rejected$Age[which(master_file_rejected$Age<=30)]="(18,30]"
master_file_rejected$Age[which(master_file_rejected$Age>30 & master_file_rejected$Age<=40)]="(31,40]"
master_file_rejected$Age[which(master_file_rejected$Age>40 & master_file_rejected$Age<=50)]="(41,50]"
master_file_rejected$Age[which(master_file_rejected$Age>50 & master_file_rejected$Age<=60)]="(51,60]"
master_file_rejected$Age[which(master_file_rejected$Age>60 )]="(61,65]"

## Replacement of the bins of the Age attribute with their respective WOE values

master_file_rejected$Age[which(master_file_rejected$Age=="(18,30]")]= -0.034822022
master_file_rejected$Age[which(master_file_rejected$Age=="(31,40]")]=  0.053929007 
master_file_rejected$Age[which(master_file_rejected$Age=="(41,50]")]= -0.009195985
master_file_rejected$Age[which(master_file_rejected$Age=="(51,60]")]= -0.030906352
master_file_rejected$Age[which(master_file_rejected$Age=="(61,65]")]= -0.018204111

master_file_rejected$Age<-as.numeric(master_file_rejected$Age)


##-------Binning the INCOME Attribute---------


master_file_rejected$Income[which(master_file_rejected$Income<=10)]= "[1,10]"
master_file_rejected$Income[which(master_file_rejected$Income>10 & master_file_rejected$Income<=18)]= "(10,18]"
master_file_rejected$Income[which(master_file_rejected$Income>18 & master_file_rejected$Income<=27)]= "(18,27]"
master_file_rejected$Income[which(master_file_rejected$Income>27 & master_file_rejected$Income<=35)]= "(27,35]"
master_file_rejected$Income[which(master_file_rejected$Income>35 & master_file_rejected$Income<=44)]= "(35,44]"
master_file_rejected$Income[which(master_file_rejected$Income>44 & master_file_rejected$Income<=60)]= "(44,60]"


master_file_rejected$Income[which(master_file_rejected$Income=="[1,10]")] <-  0.29488902
master_file_rejected$Income[which(master_file_rejected$Income=="(10,18]")]<- 0.05672875
master_file_rejected$Income[which(master_file_rejected$Income=="(18,27]")]<- 0.04940877
master_file_rejected$Income[which(master_file_rejected$Income=="(27,35]")]<- -0.03093510
master_file_rejected$Income[which(master_file_rejected$Income=="(35,44]")]<- -0.2431116
master_file_rejected$Income[which(master_file_rejected$Income=="(44,60]")]<- -0.26283400


master_new$Income<-as.numeric(master_new$Income)


##-------Binning the No.of.dependents Attribute---------

master_file_rejected$No.of.dependents[which(master_file_rejected$No.of.dependents<=2)]= "[1,2]"
master_file_rejected$No.of.dependents[which(master_file_rejected$No.of.dependents>2 & master_file_rejected$No.of.dependents<=3)]= "(2,3]"
master_file_rejected$No.of.dependents[which(master_file_rejected$No.of.dependents>3 & master_file_rejected$No.of.dependents<=4)]= "(3,4]"
master_file_rejected$No.of.dependents[which(master_file_rejected$No.of.dependents>4 & master_file_rejected$No.of.dependents<=5)]= "(4,5]"


master_file_rejected$No.of.dependents[which(master_file_rejected$No.of.dependents=="[1,2]")] <-  -0.02058310
master_file_rejected$No.of.dependents[which(master_file_rejected$No.of.dependents=="(2,3]")]<- 0.05395479
master_file_rejected$No.of.dependents[which(master_file_rejected$No.of.dependents=="(3,4]")]<- -0.02520439
master_file_rejected$No.of.dependents[which(master_file_rejected$No.of.dependents=="(4,5]")]<- 0.00439087

master_file_rejected$No.of.dependents<-as.numeric(master_file_rejected$No.of.dependents)


##-------Binning the No. OF MONTHS.IN.CURRENT.RESIDENCE Attribute---------


master_file_rejected$No.of.months.in.current.residence[which(master_file_rejected$No.of.months.in.current.residence>=6 &  master_file_rejected$No.of.months.in.current.residence<=10)]<-"[6,10]"
master_file_rejected$No.of.months.in.current.residence[which(master_file_rejected$No.of.months.in.current.residence>10)]<-"[10,126]"


master_file_rejected$No.of.months.in.current.residence[which(master_file_rejected$No.of.months.in.current.residence=="[6,10]" )]<- 0.2067408
master_file_rejected$No.of.months.in.current.residence[which(master_file_rejected$No.of.months.in.current.residence=="[10,126]")]<- -0.2528746

master_file_rejected$No.of.months.in.current.residence<-as.numeric(master_file_rejected$No.of.months.in.current.residence)


##-------Binning the NO. OF MONTHS IN CURRENT COMPANY Attribute---------


master_file_rejected$No.of.months.in.current.company[which(master_file_rejected$No.of.months.in.current.company>=3 &  master_file_rejected$No.of.months.in.current.company<=13)]<-"[3,13]"
master_file_rejected$No.of.months.in.current.company[which(master_file_rejected$No.of.months.in.current.company>13 &  master_file_rejected$No.of.months.in.current.company<=27)]<-"(13,27]"
master_file_rejected$No.of.months.in.current.company[which(master_file_rejected$No.of.months.in.current.company>27 &  master_file_rejected$No.of.months.in.current.company<=41)]<-"(27,41]"
master_file_rejected$No.of.months.in.current.company[which(master_file_rejected$No.of.months.in.current.company>41 &  master_file_rejected$No.of.months.in.current.company<=54)]<-"(41,54]"
master_file_rejected$No.of.months.in.current.company[which(master_file_rejected$No.of.months.in.current.company>54 &  master_file_rejected$No.of.months.in.current.company<=98)]<-"(54,98]"


master_file_rejected$No.of.months.in.current.company[which(master_file_rejected$No.of.months.in.current.company=="[3,13]" )]<-  0.14114927
master_file_rejected$No.of.months.in.current.company[which(master_file_rejected$No.of.months.in.current.company=="(13,27]")]<- 0.10209682
master_file_rejected$No.of.months.in.current.company[which(master_file_rejected$No.of.months.in.current.company=="(27,41]")]<- -0.02315598
master_file_rejected$No.of.months.in.current.company[which(master_file_rejected$No.of.months.in.current.company=="(41,54]")]<-  -0.19666823
master_file_rejected$No.of.months.in.current.company[which(master_file_rejected$No.of.months.in.current.company=="(54,98]")]<- -0.07001794 

master_file_rejected$No.of.months.in.current.company<-as.numeric(master_file_rejected$No.of.months.in.current.company)


##-------Binning the No.of.times.90.DPD.or.worse.in.last.6.months Attribute---------

master_file_rejected$No.of.times.90.DPD.or.worse.in.last.6.months[which(master_file_rejected$No.of.times.90.DPD.or.worse.in.last.6.months==0)]<-"Not 90DPD"
master_file_rejected$No.of.times.90.DPD.or.worse.in.last.6.months[which(master_file_rejected$No.of.times.90.DPD.or.worse.in.last.6.months>=1 & master_file_rejected$No.of.times.90.DPD.or.worse.in.last.6.months!="Not 90DPD")] <- "On 90DPD"

master_file_rejected$No.of.times.90.DPD.or.worse.in.last.6.months[which(master_file_rejected$No.of.times.90.DPD.or.worse.in.last.6.months=="Not 90DPD" )]<-  -0.2606781
master_file_rejected$No.of.times.90.DPD.or.worse.in.last.6.months[which(master_file_rejected$No.of.times.90.DPD.or.worse.in.last.6.months=="On 90DPD")]<- 0.6224550

master_file_rejected$No.of.times.90.DPD.or.worse.in.last.6.months<-as.numeric(master_file_rejected$No.of.times.90.DPD.or.worse.in.last.6.months)


##-------Binning the No.of.times.60.DPD.or.worse.in.last.6.months Attribute---------


master_file_rejected$No.of.times.60.DPD.or.worse.in.last.6.months[which(master_file_rejected$No.of.times.60.DPD.or.worse.in.last.6.months==0)]<-"Not 60DPD"
master_file_rejected$No.of.times.60.DPD.or.worse.in.last.6.months[which(master_file_rejected$No.of.times.60.DPD.or.worse.in.last.6.months>=1 & master_file_rejected$No.of.times.60.DPD.or.worse.in.last.6.months!="Not 60DPD")] <- "On 60DPD"


master_file_rejected$No.of.times.60.DPD.or.worse.in.last.6.months[which(master_file_rejected$No.of.times.60.DPD.or.worse.in.last.6.months=="Not 60DPD" )]<-  -0.3363664
master_file_rejected$No.of.times.60.DPD.or.worse.in.last.6.months[which(master_file_rejected$No.of.times.60.DPD.or.worse.in.last.6.months=="On 60DPD")]<- 0.6225361

master_file_rejected$No.of.times.60.DPD.or.worse.in.last.6.months<-as.numeric(master_file_rejected$No.of.times.60.DPD.or.worse.in.last.6.months)


##-------Binning the No.of.times.30.DPD.or.worse.in.last.6.months Attribute---------

master_file_rejected$No.of.times.30.DPD.or.worse.in.last.6.months[which(master_file_rejected$No.of.times.30.DPD.or.worse.in.last.6.months==0)]<-"Not 30DPD"
master_file_rejected$No.of.times.30.DPD.or.worse.in.last.6.months[which(master_file_rejected$No.of.times.30.DPD.or.worse.in.last.6.months>=1 & master_file_rejected$No.of.times.30.DPD.or.worse.in.last.6.months!="Not 30DPD")] <- "On 30DPD"


master_file_rejected$No.of.times.30.DPD.or.worse.in.last.6.months[which(master_file_rejected$No.of.times.30.DPD.or.worse.in.last.6.months=="Not 30DPD" )]<-  -0.3867918
master_file_rejected$No.of.times.30.DPD.or.worse.in.last.6.months[which(master_file_rejected$No.of.times.30.DPD.or.worse.in.last.6.months=="On 30DPD")]<-  0.6171842

master_file_rejected$No.of.times.30.DPD.or.worse.in.last.6.months<-as.numeric(master_file_rejected$No.of.times.30.DPD.or.worse.in.last.6.months)


##-------Binning the No.of.times.90.DPD.or.worse.in.last.12.months Attribute---------


master_file_rejected$No.of.times.90.DPD.or.worse.in.last.12.months[which(master_file_rejected$No.of.times.90.DPD.or.worse.in.last.12.months==0)]<-"Not 90DPD in year"
master_file_rejected$No.of.times.90.DPD.or.worse.in.last.12.months[which(master_file_rejected$No.of.times.90.DPD.or.worse.in.last.12.months>=1 & master_file_rejected$No.of.times.90.DPD.or.worse.in.last.12.months!="Not 90DPD in year")] <- "On 90DPD in year"


master_file_rejected$No.of.times.90.DPD.or.worse.in.last.12.months[which(master_file_rejected$No.of.times.90.DPD.or.worse.in.last.12.months=="Not 90DPD in year" )]<-  -0.3566331
master_file_rejected$No.of.times.90.DPD.or.worse.in.last.12.months[which(master_file_rejected$No.of.times.90.DPD.or.worse.in.last.12.months=="On 90DPD in year")]<- 0.5983464

master_file_rejected$No.of.times.90.DPD.or.worse.in.last.12.months<-as.numeric(master_file_rejected$No.of.times.90.DPD.or.worse.in.last.12.months)


##-------Binning the No.of.times.60.DPD.or.worse.in.last.12.months Attribute---------

master_file_rejected$No.of.times.60.DPD.or.worse.in.last.12.months[which(master_file_rejected$No.of.times.60.DPD.or.worse.in.last.12.months==0)]<-"Not 60DPD in year"
master_file_rejected$No.of.times.60.DPD.or.worse.in.last.12.months[which(master_file_rejected$No.of.times.60.DPD.or.worse.in.last.12.months>=1 & master_file_rejected$No.of.times.60.DPD.or.worse.in.last.12.months!="Not 60DPD in year")] <- "On 60DPD in year"


master_file_rejected$No.of.times.60.DPD.or.worse.in.last.12.months[which(master_file_rejected$No.of.times.60.DPD.or.worse.in.last.12.months=="Not 60DPD in year" )]<-  -0.3519211
master_file_rejected$No.of.times.60.DPD.or.worse.in.last.12.months[which(master_file_rejected$No.of.times.60.DPD.or.worse.in.last.12.months=="On 60DPD in year")]<- 0.4627493

master_file_rejected$No.of.times.60.DPD.or.worse.in.last.12.months<-as.numeric(master_file_rejected$No.of.times.60.DPD.or.worse.in.last.12.months)


##------Binning the No.of.times.30.DPD.or.worse.in.last.12.months Attribute---------

master_file_rejected$No.of.times.30.DPD.or.worse.in.last.12.months[which(master_file_rejected$No.of.times.30.DPD.or.worse.in.last.12.months==0)]<-"Not 30DPD in year"
master_file_rejected$No.of.times.30.DPD.or.worse.in.last.12.months[which(master_file_rejected$No.of.times.30.DPD.or.worse.in.last.12.months>=1 & master_file_rejected$No.of.times.30.DPD.or.worse.in.last.12.months!="Not 30DPD in year")] <- "On 30DPD in year"


master_file_rejected$No.of.times.30.DPD.or.worse.in.last.12.months[which(master_file_rejected$No.of.times.30.DPD.or.worse.in.last.12.months=="Not 30DPD in year" )]<-  -0.3763960
master_file_rejected$No.of.times.30.DPD.or.worse.in.last.12.months[which(master_file_rejected$No.of.times.30.DPD.or.worse.in.last.12.months=="On 30DPD in year")]<- 0.4600653

master_file_rejected$No.of.times.30.DPD.or.worse.in.last.12.months<-as.numeric(master_file_rejected$No.of.times.30.DPD.or.worse.in.last.12.months)


##-------Binning the Avgas.CC.Utilization.in.last.12.months Attribute---------


master_file_rejected$Avgas.CC.Utilization.in.last.12.months<-as.factor(master_file_rejected$Avgas.CC.Utilization.in.last.12.months)

levels(master_file_rejected$Avgas.CC.Utilization.in.last.12.months)[1:2]<-"[0,5]"
levels(master_file_rejected$Avgas.CC.Utilization.in.last.12.months)[2:3]<-"(5,7]"
levels(master_file_rejected$Avgas.CC.Utilization.in.last.12.months)[3:5]<-"(7,10]"
levels(master_file_rejected$Avgas.CC.Utilization.in.last.12.months)[4:5]<-"(10,12]"
levels(master_file_rejected$Avgas.CC.Utilization.in.last.12.months)[5:8]<-"(12,16]"
levels(master_file_rejected$Avgas.CC.Utilization.in.last.12.months)[6:14]<-"(16,25]"
levels(master_file_rejected$Avgas.CC.Utilization.in.last.12.months)[7:23]<-"(25,42]"
levels(master_file_rejected$Avgas.CC.Utilization.in.last.12.months)[8:22]<-"(42,57]"
levels(master_file_rejected$Avgas.CC.Utilization.in.last.12.months)[9:36]<-"(57,85]"
levels(master_file_rejected$Avgas.CC.Utilization.in.last.12.months)[10:23]<-"(85,113]"

master_file_rejected$Avgas.CC.Utilization.in.last.12.months<-unfactor(master_file_rejected$Avgas.CC.Utilization.in.last.12.months)

master_file_rejected$Avgas.CC.Utilization.in.last.12.months[which(master_file_rejected$Avgas.CC.Utilization.in.last.12.months=="[0,5]" )]<- -0.8721315
master_file_rejected$Avgas.CC.Utilization.in.last.12.months[which(master_file_rejected$Avgas.CC.Utilization.in.last.12.months=="(5,7]")]<-  -0.7784605
master_file_rejected$Avgas.CC.Utilization.in.last.12.months[which(master_file_rejected$Avgas.CC.Utilization.in.last.12.months=="(7,10]")]<- -0.7575516
master_file_rejected$Avgas.CC.Utilization.in.last.12.months[which(master_file_rejected$Avgas.CC.Utilization.in.last.12.months=="(10,12]")]<- -0.5005034
master_file_rejected$Avgas.CC.Utilization.in.last.12.months[which(master_file_rejected$Avgas.CC.Utilization.in.last.12.months=="(12,16]")]<- -0.4170280
master_file_rejected$Avgas.CC.Utilization.in.last.12.months[which(master_file_rejected$Avgas.CC.Utilization.in.last.12.months=="(16,25]")]<- 0.2078188
master_file_rejected$Avgas.CC.Utilization.in.last.12.months[which(master_file_rejected$Avgas.CC.Utilization.in.last.12.months=="(25,42]")]<- 0.5327389
master_file_rejected$Avgas.CC.Utilization.in.last.12.months[which(master_file_rejected$Avgas.CC.Utilization.in.last.12.months=="(42,57]")]<- 0.5920429
master_file_rejected$Avgas.CC.Utilization.in.last.12.months[which(master_file_rejected$Avgas.CC.Utilization.in.last.12.months=="(57,85]")]<- 0.5718882
master_file_rejected$Avgas.CC.Utilization.in.last.12.months[which(master_file_rejected$Avgas.CC.Utilization.in.last.12.months=="(85,113]")]<- 0.2441514
master_file_rejected$Avgas.CC.Utilization.in.last.12.months[which(is.na(master_file_rejected$Avgas.CC.Utilization.in.last.12.months)==TRUE)]<- 0.1114737


master_file_rejected$Avgas.CC.Utilization.in.last.12.months<-as.numeric(master_file_rejected$Avgas.CC.Utilization.in.last.12.months)


##-------Binning the No.of.trades.opened.in.last.6.months Attribute---------


master_file_rejected$No.of.trades.opened.in.last.6.months<-as.factor(master_file_rejected$No.of.trades.opened.in.last.6.months)


levels(master_file_rejected$No.of.trades.opened.in.last.6.months)[1:2]<-"[0,1]"
levels(master_file_rejected$No.of.trades.opened.in.last.6.months)[2]<-"(1,2]"
levels(master_file_rejected$No.of.trades.opened.in.last.6.months)[3]<-"(2,3]"
levels(master_file_rejected$No.of.trades.opened.in.last.6.months)[4:6]<-"(3,6]"

master_file_rejected$No.of.trades.opened.in.last.6.months<-unfactor(master_file_rejected$No.of.trades.opened.in.last.6.months)

master_file_rejected$No.of.trades.opened.in.last.6.months[which(master_file_rejected$No.of.trades.opened.in.last.6.months=="[0,1]" )]<-  -0.54323458
master_file_rejected$No.of.trades.opened.in.last.6.months[which(master_file_rejected$No.of.trades.opened.in.last.6.months=="(1,2]" )]<-  0.23286102
master_file_rejected$No.of.trades.opened.in.last.6.months[which(master_file_rejected$No.of.trades.opened.in.last.6.months=="(2,3]" )]<-  0.43512392
master_file_rejected$No.of.trades.opened.in.last.6.months[which(master_file_rejected$No.of.trades.opened.in.last.6.months=="(3,6]" )]<-  0.39841223
master_file_rejected$No.of.trades.opened.in.last.6.months[which(master_file_rejected$No.of.trades.opened.in.last.6.months=="(6,12]" )]<-  -0.07220746
master_file_rejected$No.of.trades.opened.in.last.6.months[which(is.na(master_file_rejected$No.of.trades.opened.in.last.6.months)==TRUE )]<-  0.00000000

master_file_rejected$No.of.trades.opened.in.last.6.months<-as.numeric(master_file_rejected$No.of.trades.opened.in.last.6.months)


##-------Binning the No.of.trades.opened.in.last.12.months Attribute---------


master_file_rejected$No.of.trades.opened.in.last.12.months<-as.factor(master_file_rejected$No.of.trades.opened.in.last.12.months)


levels(master_file_rejected$No.of.trades.opened.in.last.12.months)[1:2]<-"[0,1]"
levels(master_file_rejected$No.of.trades.opened.in.last.12.months)[2:3]<-"(1,3]"
levels(master_file_rejected$No.of.trades.opened.in.last.12.months)[3:5]<-"(3,6]"
levels(master_file_rejected$No.of.trades.opened.in.last.12.months)[4:6]<-"(6,9]"
levels(master_file_rejected$No.of.trades.opened.in.last.12.months)[5:9]<-"(9,14]"


master_file_rejected$No.of.trades.opened.in.last.12.months<-unfactor(master_file_rejected$No.of.trades.opened.in.last.12.months)

master_file_rejected$No.of.trades.opened.in.last.12.months[which(master_file_rejected$No.of.trades.opened.in.last.12.months=="[0,1]" )]<- -0.89395337
master_file_rejected$No.of.trades.opened.in.last.12.months[which(master_file_rejected$No.of.trades.opened.in.last.12.months=="(1,3]")]<- -0.46736230
master_file_rejected$No.of.trades.opened.in.last.12.months[which(master_file_rejected$No.of.trades.opened.in.last.12.months=="(3,6]")]<- 0.21899083
master_file_rejected$No.of.trades.opened.in.last.12.months[which(master_file_rejected$No.of.trades.opened.in.last.12.months=="(6,9]")]<- 0.53390532
master_file_rejected$No.of.trades.opened.in.last.12.months[which(master_file_rejected$No.of.trades.opened.in.last.12.months=="(9,14]")]<- 0.31364026

master_file_rejected$No.of.trades.opened.in.last.12.months<-as.numeric(master_file_rejected$No.of.trades.opened.in.last.12.months)


##-------Binning the No.of.PL.trades.opened.in.last.6.months Attribute---------

master_file_rejected$No.of.PL.trades.opened.in.last.6.months<-as.factor(master_file_rejected$No.of.PL.trades.opened.in.last.6.months)

levels(master_file_rejected$No.of.PL.trades.opened.in.last.6.months)[1:2]<-"[0,1]"
levels(master_file_rejected$No.of.PL.trades.opened.in.last.6.months)[2:4]<-"(1,5]"


master_file_rejected$No.of.PL.trades.opened.in.last.6.months<-unfactor(master_file_rejected$No.of.PL.trades.opened.in.last.6.months)

master_file_rejected$No.of.PL.trades.opened.in.last.6.months[which(master_file_rejected$No.of.PL.trades.opened.in.last.6.months=="[0,1]" )]<- -0.3146596
master_file_rejected$No.of.PL.trades.opened.in.last.6.months[which(master_file_rejected$No.of.PL.trades.opened.in.last.6.months=="(1,5]")]<- 0.4077547

master_file_rejected$No.of.PL.trades.opened.in.last.6.months<-as.numeric(master_file_rejected$No.of.PL.trades.opened.in.last.6.months)


##-------Binning the No.of.PL.trades.opened.in.last.12.months Attribute---------

master_file_rejected$No.of.PL.trades.opened.in.last.12.months<-as.factor(master_file_rejected$No.of.PL.trades.opened.in.last.12.months)

levels(master_file_rejected$No.of.PL.trades.opened.in.last.12.months)[1:3]<-"[0,2]"
levels(master_file_rejected$No.of.PL.trades.opened.in.last.12.months)[2:7]<-"(2,10]"

master_file_rejected$No.of.PL.trades.opened.in.last.12.months<-unfactor(master_file_rejected$No.of.PL.trades.opened.in.last.12.months)

master_file_rejected$No.of.PL.trades.opened.in.last.12.months[which(master_file_rejected$No.of.PL.trades.opened.in.last.12.months=="[0,2]" )]<- -0.4531209
master_file_rejected$No.of.PL.trades.opened.in.last.12.months[which(master_file_rejected$No.of.PL.trades.opened.in.last.12.months=="(2,10]")]<- 0.3960410

master_file_rejected$No.of.PL.trades.opened.in.last.12.months<-as.numeric(master_file_rejected$No.of.PL.trades.opened.in.last.12.months)


##-------Binning the No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. Attribute---------


master_file_rejected$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.<-as.factor(master_file_rejected$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)

levels(master_file_rejected$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)[1:2]<-"[0,1]"
levels(master_file_rejected$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)[2:4]<-"(1,4]"


master_file_rejected$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.<-unfactor(master_file_rejected$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)

master_file_rejected$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.[which(master_file_rejected$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.=="[0,1]" )]<- -0.3200583
master_file_rejected$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.[which(master_file_rejected$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.=="(1,4]")]<-  0.3101983

master_file_rejected$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.<-as.numeric(master_file_rejected$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)


##-------Binning the No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. Attribute---------

master_file_rejected$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.<-as.factor(master_file_rejected$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)

levels(master_file_rejected$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)[1:2]<-"[0,1]"
levels(master_file_rejected$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)[2:4]<-"(1,4]"
levels(master_file_rejected$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)[3:6]<-"(4,12]"


master_file_rejected$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.<-unfactor(master_file_rejected$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)

master_file_rejected$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.[which(master_file_rejected$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.=="[0,1]" )]<- -0.8282838
master_file_rejected$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.[which(master_file_rejected$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.=="(1,4]")]<-  0.1827363
master_file_rejected$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.[which(master_file_rejected$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.=="(4,12]")]<- 0.4136663

master_file_rejected$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.<-as.numeric(master_file_rejected$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)


##-------Binning the Outstanding.Balance Attribute---------


master_file_rejected$Outstanding.Balance<-as.factor(master_file_rejected$Outstanding.Balance)


levels(master_file_rejected$Outstanding.Balance)[1:27]<-"(1.05e+04,2.13e+05]"
levels(master_file_rejected$Outstanding.Balance)[2:257]<-"(2.13e+05,5.69e+05]"
levels(master_file_rejected$Outstanding.Balance)[3:376]<-"(5.69e+05,7.78e+05]"
levels(master_file_rejected$Outstanding.Balance)[4:359]<-"(7.78e+05,1.02e+06]"
levels(master_file_rejected$Outstanding.Balance)[5:276]<-"(1.02e+06,2.93e+06]"
levels(master_file_rejected$Outstanding.Balance)[6:24]<-"(2.93e+06,3.12e+06]"
levels(master_file_rejected$Outstanding.Balance)[7:123]<-"(3.12e+06,5.22e+06]"


master_file_rejected$Outstanding.Balance<-unfactor(master_file_rejected$Outstanding.Balance)

master_file_rejected$Outstanding.Balance[which(master_file_rejected$Outstanding.Balance=="(1.05e+04,2.13e+05]")]<-  -0.6279279
master_file_rejected$Outstanding.Balance[which(master_file_rejected$Outstanding.Balance=="(2.13e+05,5.69e+05]")]<- 0.2505438
master_file_rejected$Outstanding.Balance[which(master_file_rejected$Outstanding.Balance=="(5.69e+05,7.78e+05]")]<- 0.4024312
master_file_rejected$Outstanding.Balance[which(master_file_rejected$Outstanding.Balance=="(7.78e+05,1.02e+06]")]<- 0.3781004
master_file_rejected$Outstanding.Balance[which(master_file_rejected$Outstanding.Balance=="(1.02e+06,2.93e+06]")]<- 0.2720899
master_file_rejected$Outstanding.Balance[which(master_file_rejected$Outstanding.Balance=="(2.93e+06,3.12e+06]")]<- -1.0101426
master_file_rejected$Outstanding.Balance[which(master_file_rejected$Outstanding.Balance=="(3.12e+06,5.22e+06]")]<- 0.2070019
master_file_rejected$Outstanding.Balance[which(is.na(master_file_rejected$Outstanding.Balance)==TRUE)]<-  -0.3737974

master_file_rejected$Outstanding.Balance<-as.numeric(master_file_rejected$Outstanding.Balance)


##------Binning the Total.No.of.Trades Attribute---------


master_file_rejected$Total.No.of.Trades<-as.factor(master_file_rejected$Total.No.of.Trades)

levels(master_file_rejected$Total.No.of.Trades)[1:2]<-"[0,2]"
levels(master_file_rejected$Total.No.of.Trades)[2]<-"(2,3]"
levels(master_file_rejected$Total.No.of.Trades)[3]<-"(3,4]"
levels(master_file_rejected$Total.No.of.Trades)[4]<-"(4,5]"
levels(master_file_rejected$Total.No.of.Trades)[5]<-"(5,6]"
levels(master_file_rejected$Total.No.of.Trades)[6]<-"(6,7]"
levels(master_file_rejected$Total.No.of.Trades)[7:8]<-"(7,9]"
levels(master_file_rejected$Total.No.of.Trades)[8:9]<-"(9,11]"
levels(master_file_rejected$Total.No.of.Trades)[9:13]<-"(11,18]"

master_file_rejected$Total.No.of.Trades<-unfactor(master_file_rejected$Total.No.of.Trades)

master_file_rejected$Total.No.of.Trades[which(master_file_rejected$Total.No.of.Trades=="[0,2]" )]<- -0.87778148
master_file_rejected$Total.No.of.Trades[which(master_file_rejected$Total.No.of.Trades=="(2,3]")]<-  -0.70202474
master_file_rejected$Total.No.of.Trades[which(master_file_rejected$Total.No.of.Trades=="(3,4]")]<- -0.44785257
master_file_rejected$Total.No.of.Trades[which(master_file_rejected$Total.No.of.Trades=="(4,5]")]<- -0.04880056
master_file_rejected$Total.No.of.Trades[which(master_file_rejected$Total.No.of.Trades=="(5,6]")]<- 0.12930127
master_file_rejected$Total.No.of.Trades[which(master_file_rejected$Total.No.of.Trades=="(6,7]")]<- 0.29649292
master_file_rejected$Total.No.of.Trades[which(master_file_rejected$Total.No.of.Trades=="(7,9]")]<- 0.51261494
master_file_rejected$Total.No.of.Trades[which(master_file_rejected$Total.No.of.Trades=="(9,11]")]<- 0.48924883
master_file_rejected$Total.No.of.Trades[which(master_file_rejected$Total.No.of.Trades=="(11,18]")]<- 0.46477123

master_file_rejected$Total.No.of.Trades<-as.numeric(master_file_rejected$Total.No.of.Trades)


##-------WOE of Gender Attribute---------


master_file_rejected$Gender<-as.character(master_file_rejected$Gender)

master_file_rejected$Gender[which(master_file_rejected$Gender=="F" )]<-  0.0321743
master_file_rejected$Gender[which(master_file_rejected$Gender=="M")]<-  -0.0101473

master_file_rejected$Gender<-as.numeric(master_file_rejected$Gender)


##-------WOE of Marital.Status..at.the.time.of.application. Attribute---------


master_file_rejected$Marital.Status..at.the.time.of.application.<-as.character(master_file_rejected$Marital.Status..at.the.time.of.application.)

master_file_rejected$Marital.Status..at.the.time.of.application.[which(master_file_rejected$Marital.Status..at.the.time.of.application.=="Married" )]<-  -0.004092434
master_file_rejected$Marital.Status..at.the.time.of.application.[which(master_file_rejected$Marital.Status..at.the.time.of.application.=="Single")]<-  0.023326708

master_file_rejected$Marital.Status..at.the.time.of.application.<-as.numeric(master_file_rejected$Marital.Status..at.the.time.of.application.)


##-------WOE of Education Attribute---------

master_file_rejected$Education<-as.character(master_file_rejected$Education)

master_file_rejected$Education[which(master_file_rejected$Education=="Bachelor" )]<-  0.017313988
master_file_rejected$Education[which(master_file_rejected$Education=="Masters")]<-  0.007948702
master_file_rejected$Education[which(master_file_rejected$Education=="Others")]<-  0.492621513
master_file_rejected$Education[which(master_file_rejected$Education=="Phd")]<-  -0.029511963
master_file_rejected$Education[which(master_file_rejected$Education=="Professional")]<-  -0.017820977
master_file_rejected$Education[which(is.na(master_file_rejected$Education)==TRUE)]<- -0.017820977

master_file_rejected$Education<-as.numeric(master_file_rejected$Education)


##-------WOE of Profession Attribute---------


master_file_rejected$Profession<-as.character(master_file_rejected$Profession)

master_file_rejected$Profession[which(master_file_rejected$Profession=="SAL" )]<-  -0.02840853
master_file_rejected$Profession[which(master_file_rejected$Profession=="SE")]<-  0.09142405
master_file_rejected$Profession[which(master_file_rejected$Profession=="SE_PROF")]<-  -0.01329769
master_file_rejected$Profession[which(is.na(master_file_rejected$Profession)==TRUE)]<- -0.02840853

master_file_rejected$Profession<-as.numeric(master_file_rejected$Profession)


##-------WOE of Type.of.residence Attribute---------


master_file_rejected$Type.of.residence<-as.character(master_file_rejected$Type.of.residence)

master_file_rejected$Type.of.residence[which(master_file_rejected$Type.of.residence=="Company provided" )]<-  0.080146599
master_file_rejected$Type.of.residence[which(master_file_rejected$Type.of.residence=="Living with Parents")]<-  0.067530440
master_file_rejected$Type.of.residence[which(master_file_rejected$Type.of.residence=="Others")]<-  -0.530542104
master_file_rejected$Type.of.residence[which(master_file_rejected$Type.of.residence=="Owned")]<-  0.004148595
master_file_rejected$Type.of.residence[which(master_file_rejected$Type.of.residence=="Rented")]<-  -0.004453727

master_file_rejected$Type.of.residence<-as.numeric(master_file_rejected$Type.of.residence)


##-------WOE of Presence.of.open.home.loan Attribute---------


master_file_rejected$Presence.of.open.home.loan<-as.character(master_file_rejected$Presence.of.open.home.loan)

master_file_rejected$Presence.of.open.home.loan[which(master_file_rejected$Presence.of.open.home.loan=="0" )]<-  0.07370543
master_file_rejected$Presence.of.open.home.loan[which(master_file_rejected$Presence.of.open.home.loan=="1")]<-  -0.23665793
master_file_rejected$Presence.of.open.home.loan[which(is.na(master_file_rejected$Presence.of.open.home.loan)==TRUE)]<-  -0.37379739

master_file_rejected$Presence.of.open.home.loan<-as.numeric(master_file_rejected$Presence.of.open.home.loan)


##-------WOE of Presence.of.open.auto.loan Attribute---------


master_file_rejected$Presence.of.open.auto.loan<-as.character(master_file_rejected$Presence.of.open.auto.loan)

master_file_rejected$Presence.of.open.auto.loan[which(master_file_rejected$Presence.of.open.auto.loan=="0" )]<-  0.01198467
master_file_rejected$Presence.of.open.auto.loan[which(master_file_rejected$Presence.of.open.auto.loan=="1")]<-  -0.13836752

master_file_rejected$Presence.of.open.auto.loan<-as.numeric(master_file_rejected$Presence.of.open.auto.loan)


####____________________________________MODEL BUILDING & EVALUATION_______________________________####

## As per the demand of the project, different models have to be built using following data:

## (1) Only Demographic Data
## (2) Both Demographic & Credit Bureau Data

## Hence, now the datset prepared above needs to be divide into 2 different sets;

## One file containing only the DEMOGRAPHIC DATA

master_file_demographic_WOE<-master_new[,c(1:12)]

## and the second main file having both the DEMOGRAPHIC and CREDIT BUREAU DATA, which is same as the 
## file MASTER_NEW

## Now, before, building the model, the TARGET ATTRIBUTE, i.e., PERFORMANCE.TAG, in both the datasets
## need to be converted into FACTOR type.

master_file_demographic_WOE$Performance.Tag<-as.factor(master_file_demographic_WOE$Performance.Tag)
master_new$Performance.Tag<-as.factor(master_new$Performance.Tag)


### ------------ LOGISTIC REGRESSION MODEL Using DEMOGRAPHIC DATASET-------------------


## splitting into train and test data

set.seed(1)

split_indices_demographic <- sample.split(master_file_demographic_WOE$Performance.Tag, SplitRatio = 0.70)

train_demographic <- master_file_demographic_WOE[split_indices_demographic, ]

test_demographic <- master_file_demographic_WOE[!split_indices_demographic, ]

## Initial model including all the predictor variables,

model_1_demog<-glm(Performance.Tag~., family = "binomial", data = train_demographic[,-1])
summary(model_1_demog)

## Using stepwise algorithm for removing insignificant variables 

step_model_demog<-stepAIC(model_1_demog, direction = "both")

## Following are the variables remaning after the stepwise regression method:

## "No.of.dependentsIncome", "Education", "Profession", "Type.of.residence", 
## "No.of.months.in.current.residence", "No.of.months.in.current.company"

## Model building using the above attributes obtained thorught he stepAIC model :

# 1st model with all the remaining variables

model_demog_1<-glm(formula= Performance.Tag~No.of.dependents + Income + Education + 
                     Profession + Type.of.residence + No.of.months.in.current.residence + 
                     No.of.months.in.current.company, family = "binomial", data = train_demographic[,-1])

summary(model_demog_1)
vif(model_demog_1)

# Since, all the attributes have VIF<2, hence, the removal of attribues is now based on their
# significance level. Hence, removing the attribute "Type.of.residence" (having Pr value =0.3073),
# and developing the new model,

model_demog_2<-glm(formula= Performance.Tag~No.of.dependents + Income + Education + 
                     Profession + No.of.months.in.current.residence + 
                     No.of.months.in.current.company, family = "binomial", data = train_demographic[,-1])

summary(model_demog_2)
vif(model_demog_2)

# Further, removing "Education" attribute (Pr value=0.4130), the new model will be,

model_demog_3<-glm(formula= Performance.Tag~No.of.dependents + Income + 
                     Profession + No.of.months.in.current.residence + 
                     No.of.months.in.current.company, family = "binomial", data = train_demographic[,-1])

summary(model_demog_3)
vif(model_demog_3)


# Further, removing "No.of.dependents" attribute (Pr value=  0.0257 *), the new model will be,

model_demog_4<-glm(formula= Performance.Tag~Income + Profession + No.of.months.in.current.residence + 
                     No.of.months.in.current.company, family = "binomial", data = train_demographic[,-1])

summary(model_demog_4)
vif(model_demog_4)

# Finally, removing "Profession" attribute (Pr value= 0.0274 *), the new model will be,

model_demog_5<-glm(formula= Performance.Tag~Income  + No.of.months.in.current.residence + 
                     No.of.months.in.current.company, family = "binomial", data = train_demographic[,-1])

summary(model_demog_5)
vif(model_demog_5)

# Since, the attributes now remaining in the model "model_demog_5" are all significant and all of them
# have VIF<2, this model is the best model developed using the Demographic Dataset.

best_demog_model<- model_demog_5

### --------------------  Model EVALUATION-------------------


## TRAIN DATA

# Predicting the probabilities of Performance in the TRAIN dataset

train_demographic$pred_perf_prob_train<- predict(best_demog_model, type = "response")
summary(pred_perf_prob_train)

ggplot(train_demographic, aes(x=pred_perf_prob_train, color=Performance.Tag))+geom_density( size = 1 )+
  ggtitle( "Demographic Train Set's Predicted Probabilities" ) + 
  scale_color_economist( name = "Performance.Tag", labels = c( "Defaulter(1)", "Non-Defaulter(0)" ) ) + 
  theme_economist()

# Calculating C-Index for the model, using TRAIN data

rcorr.cens(train_demographic$pred_perf_prob_train, train_demographic$Performance.Tag) # C-Index= 5.89

# Calculating the AUC & visualising the ROC curve for the model using TRAIN data

pred_demog_train= prediction(train_demographic$pred_perf_prob_train,train_demographic$Performance.Tag)
perf_demog_train= performance(pred_demog_train, "tpr", "fpr")

plot(perf_demog_train, main="ROC Curve for Logistic Regression for Demographic Data", col=2, lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

AUC_demog_train=performance(pred_demog_train,"auc")
AUC_demog_train # AUC = 0.589

# Calculating the KS-statistic for the model using TRAIN data

ks_table_train_demog <- attr(perf_demog_train, "y.values")[[1]] - (attr(perf_demog_train, "x.values")[[1]])
ks_train_demog = max(ks_table_train_demog)

ks_train_demog # KS- Statistic = 0.1417


# Calculating the CUTOFF value of probability, where a balanced value of Specificity
# & Sensitivity can be obtained with a better accuracy.

ROC_Values_demog_train<-roc(train_demographic$Performance.Tag, train_demographic$pred_perf_prob_train)

cutoff_demog_train<-coords(ROC_Values_demog_train, "best", ret = "threshold") 
cutoff_demog_train # cutoff_demog_train = 0.04286598

pred_perf_demog_train <- factor(ifelse(train_demographic$pred_perf_prob_train >= cutoff_demog_train, 1, 0)) 

conf_demog_train<-confusionMatrix(pred_perf_demog_train, train_demographic$Performance.Tag, positive = "1")

conf_demog_train # Sensitivity : 0.56374 , Specificity : 0.57794 , Accuracy : 0.5773 


## TEST DATA

# Predicting the probabilities of Performance in the test dataset

test_demographic$pred_perf_prob_test<- predict(best_demog_model, newdata = test_demographic[,-1], type = "response")
summary(test_demographic$pred_perf_prob_test)

ggplot(test_demographic, aes(x=pred_perf_prob_test, color=Performance.Tag))+geom_density( size = 1 )+
  ggtitle( "Demographic Test Set's Predicted Probabilities" ) + 
  scale_color_economist( name = "Performance.Tag", labels = c( "Defaulter(1)", "Non-Defaulter(0)" ) ) + 
  theme_economist()

# Calculating C-Index for the model, using test data

rcorr.cens(test_demographic$pred_perf_prob_test, test_demographic$Performance.Tag) # C-Index= 5.80

# Calculating the AUC & visualising the ROC curve for the model using test data

pred_demog_test= prediction(test_demographic$pred_perf_prob_test,test_demographic$Performance.Tag)
perf_demog_test= performance(pred_demog_test, "tpr", "fpr")

plot(perf_demog_test, main="ROC Curve for Logistic Regression for Demographic Test Data", col=2, lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

AUC_demog_test=performance(pred_demog_test,"auc")
AUC_demog_test # AUC = 0.5802


# Calculating the KS-statistic for the model using test data

ks_table_test_demog <- attr(perf_demog_test, "y.values")[[1]] - (attr(perf_demog_test, "x.values")[[1]])
ks_test_demog = max(ks_table_test_demog) # KS- Statistic = 0.1313204


# Calculating the CUTOFF value of probability, where a balanced value of Specificity
# & Sensitivity can be obtained with a better accuracy.

ROC_Values_demog_test<-roc(test_demographic$Performance.Tag, test_demographic$pred_perf_prob_test)

cutoff_demog_test<-coords(ROC_Values_demog_test, "best", ret = "threshold") 
cutoff_demog_test # cutoff_demog_test = 0.04286598

pred_perf_demog_test <- factor(ifelse(test_demographic$pred_perf_prob_test >= cutoff_demog_test, 1, 0)) 

conf_demog_test<-confusionMatrix(pred_perf_demog_test, test_demographic$Performance.Tag, positive = "1")

conf_demog_test # Sensitivity : 0.55317 , Specificity : 0.57815 , Accuracy : 0.5771 


### ------------ LOGISTIC REGRESSION MODEL Using MASTER DATASET (using both Demographic and Credit Bureau data)-------------------


## splitting into train and test data

set.seed(100)

split_indices_master <- sample.split(master_new$Performance.Tag, SplitRatio = 0.70)

train_master<- master_new[split_indices_master, ]

test_master <- master_new[!split_indices_master, ]

## Initial model including all the predictor variables,

model_1_master<-glm(Performance.Tag~., family = "binomial", data = train_master[,-1])
summary(model_1_master)

## Using stepwise algorithm for removing insignificant variables 

step_model_master<-stepAIC(model_1_master, direction = "both")


## Following are the variables remaning after the stepwise regression method:

## "No.of.dependentsIncome", "Education", "Profession", "Type.of.residence", 
## "No.of.months.in.current.residence", "No.of.months.in.current.company"

## Model building using the above attributes obtained thorught he stepAIC model :

# 1st model with all the remaining variables

model_master_1<-glm(formula = Performance.Tag ~ Type.of.residence + No.of.months.in.current.residence + 
                      No.of.months.in.current.company + No.of.times.90.DPD.or.worse.in.last.6.months + 
                      No.of.times.60.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                      Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.6.months + 
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                      Outstanding.Balance, family = "binomial", data = train_master[,-1])

summary(model_master_1)
vif(model_master_1)

# The attributes No.of.times.90.DPD.or.worse.in.last.6.months and No.of.times.60.DPD.
# or.worse.in.last.6.months have VIF>4. There might exist a strong multicollinearity between both of them.

cor(master_new$No.of.times.90.DPD.or.worse.in.last.6.months, master_new$No.of.times.60.DPD.or.worse.in.last.6.months)
# Correlation=0.895

# Also, the attribute No.of.times.90.DPD.or.worse.in.last.6.months has very low significance(Pr value= 0.130459)
# Hence, the next model is prepared after the removal of this attribute.

model_master_2<-glm(formula = Performance.Tag ~ Type.of.residence + No.of.months.in.current.residence + 
                      No.of.months.in.current.company + 
                      No.of.times.60.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                      Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.6.months + 
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                      Outstanding.Balance, family = "binomial", data = train_master[,-1])

summary(model_master_2)
vif(model_master_2)

# Further, removing "No.of.times.90.DPD.or.worse.in.last.12.months" attribute (VIF= 3.334587, 
# Pr value=0.04761 *), the new model will be,

model_master_3<-glm(formula = Performance.Tag ~ Type.of.residence + No.of.months.in.current.residence + 
                      No.of.months.in.current.company + 
                      No.of.times.60.DPD.or.worse.in.last.6.months + 
                      Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.6.months + 
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                      Outstanding.Balance, family = "binomial", data = train_master[,-1])

summary(model_master_3)
vif(model_master_3)


# Further, removing "Outstanding.Balance" attribute (VIF= 2.224936, Pr value=  0.01421*), the new model will be,

model_master_4<-glm(formula = Performance.Tag ~ Type.of.residence + No.of.months.in.current.residence + 
                      No.of.months.in.current.company + 
                      No.of.times.60.DPD.or.worse.in.last.6.months + 
                      Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.6.months + 
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., family = "binomial", data = train_master[,-1])

summary(model_master_4)
vif(model_master_4)


# Further, removing "Type.of.residence" attribute (Pr value= 0.137264 ), although
# the VIF of Avg CC utilisation attribute was moer than 2 (it has not been removed, as it appeared to
# be highly significant predictor, both as indicated by IV analysis as well as Pr value),
# the new model will be,

model_master_5<-glm(formula = Performance.Tag ~ No.of.months.in.current.residence +
                      No.of.months.in.current.company + 
                      No.of.times.60.DPD.or.worse.in.last.6.months + 
                      Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.6.months + 
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., family = "binomial", data = train_master[,-1])

summary(model_master_5)
vif(model_master_5)

# Further, removing "No.of.months.in.current.company" attribute (Pr value= 0.137264 ), although
# the VIF of Avg CC utilisation attribute was moer than 2 (it has not been removed, as it appeared to
# be highly significant predictor, both as indicated by IV analysis as well as Pr value),
# the new model will be,

model_master_6<-glm(formula = Performance.Tag ~ No.of.months.in.current.residence +
                      No.of.times.60.DPD.or.worse.in.last.6.months + 
                      Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.6.months + 
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., family = "binomial", data = train_master[,-1])

summary(model_master_6)
vif(model_master_6)

# Further, removing "No.of.months.in.current.residence" attribute (Pr value= 0.137264 ), although
# the VIF of Avg CC utilisation attribute was moer than 2 (it has not been removed, as it appeared to
# be highly significant predictor, both as indicated by IV analysis as well as Pr value),
# the new model will be,

model_master_7<-glm(formula = Performance.Tag ~ No.of.times.60.DPD.or.worse.in.last.6.months + 
                      Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.6.months + 
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., family = "binomial", data = train_master[,-1])

summary(model_master_7)
vif(model_master_7)

# Since, the attributes now remaining in the model "model_master_7" are all significant and all of them
# have VIF<2, this model is the best model developed using the Master Dataset.

best_master_model<- model_master_7

### ------------- Model EVALUATION -----------------------

## TRAIN DATA

# Predicting the probabilities of Performance in the train dataset

train_master$pred_perf_prob_master_train<- predict(best_master_model, type = "response")

summary(train_master$pred_perf_prob_master_train)

ggplot(train_master, aes(x=pred_perf_prob_master_train, color=Performance.Tag))+geom_density( size = 1 )+
  ggtitle( "Master Train Set's Predicted Probabilities" ) + 
  scale_color_economist( name = "Performance.Tag", labels = c( "Defaulter(1)", "Non-Defaulter(0)" ) ) + 
  theme_economist()

# Calculating C-Index for the model, using train data

rcorr.cens(train_master$pred_perf_prob_master_train, train_master$Performance.Tag) # C-Index = 6.737

# Calculating the AUC & visualising the ROC curve for the model using train data

pred_master_train= prediction(train_master$pred_perf_prob_master_train,train_master$Performance.Tag)
perf_master_train= performance(pred_master_train, "tpr", "fpr")

plot(perf_master_train, main="ROC Curve for Logistic Regression for Master Train Data", col=2, lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

AUC_master_train=performance(pred_master_train,"auc")

AUC_master_train # AUC= 0.6737

# Calculating the KS-statistic for the model using train data

ks_table_train_master <- attr(perf_master_train, "y.values")[[1]] - (attr(perf_master_train, "x.values")[[1]])

ks_train = max(ks_table_train_master) # KS-statistic = 0.2669

# Calculating the CUTOFF value of probability, where a balanced value of Specificity
# & Sensitivity can be obtained with a better accuracy.

ROC_Values_train<-roc(train_master$Performance.Tag, train_master$pred_perf_prob_master_train)

cutoff_master_train<-coords(ROC_Values_train, "best", ret = "threshold") # cutoff_master = 0.03898094

pred_perf_master_train <- factor(ifelse(train_master$pred_perf_prob_master_train >= cutoff_master_train, 1, 0)) 

conf_master_train<-confusionMatrix(pred_perf_master_train, train_master$Performance.Tag, positive = "1")

# Specificity = 0.56208, Sensitivity = 0.70480,  Accuracy = 0.5681 is obtained, on applying
# the best model (developed using the master dataset) on the test data.


## TEST DATA

# Predicting the probabilities of Performance in the test dataset

test_master$pred_perf_prob_master<- predict(best_master_model, newdata = test_master[,-1], type = "response")

summary(test_master$pred_perf_prob_master)

ggplot(test_master, aes(x=pred_perf_prob_master, color=Performance.Tag))+geom_density( size = 1 )+
  ggtitle( "Master Test Set's Predicted Probabilities" ) + 
  scale_color_economist( name = "Performance.Tag", labels = c( "Defaulter(1)", "Non-Defaulter(0)" ) ) + 
  theme_economist()

# Calculating C-Index for the model, using test data

rcorr.cens(test_master$pred_perf_prob_master, test_master$Performance.Tag) # C-Index = 6.6954

# Calculating the AUC & visualising the ROC curve for the model using test data

pred_master= prediction(test_master$pred_perf_prob_master,test_master$Performance.Tag)
perf_master= performance(pred_master, "tpr", "fpr")

plot(perf_master, main="ROC Curve for Logistic Regression for Master Test Data", col=2, lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

AUC_master=performance(pred_master,"auc")

AUC_master # AUC= 0.6695

# Calculating the KS-statistic for the model using test data

ks_table_test_master <- attr(perf_master, "y.values")[[1]] - (attr(perf_master, "x.values")[[1]])

ks_test = max(ks_table_test_master) # KS-statistic = 0.2834

# Calculating the CUTOFF value of probability, where a balanced value of Specificity
# & Sensitivity can be obtained with a better accuracy.

ROC_Values<-roc(test_master$Performance.Tag, test_master$pred_perf_prob_master)

cutoff_master<-coords(ROC_Values, "best", ret = "threshold") # cutoff_master = 0.039127

pred_perf_master <- factor(ifelse(test_master$pred_perf_prob_master >= cutoff_master, 1, 0)) 

conf_master<-confusionMatrix(pred_perf_master, test_master$Performance.Tag, positive = "1")

# Specificity = 0.57073, Sensitivity = 0.71267,  Accuracy = 0.5767 is obtained, on applying
# the best model (developed using the master dataset) on the test data.


### ------------ RANDOM FOREST MODEL Using MASTER DATASET (using both Demographic and Credit Bureau data) --------------------

## Calculating the best optimal "mtry" value, to be used in the randomForest() function

mtry <- tuneRF(train_master[,c(-1,-2)],train_master$Performance.Tag, ntreeTry=200,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]

print(mtry)
print(best.m)

## Building the model

fit_master= randomForest(Performance.Tag~.,data=train_master[,c(-1,-30)], mtry=4, importance=TRUE,
                         ntree=200)
summary(fit_master)

print(fit_master) # OOB error = 4.22%

important_attr <- fit_master$importance

### ------------- MODEL EVALUATION --------------

## Calculating the predicted probabilites, on the test dataset

pred_prob_rf_test<- predict(fit_master, test_master[,c(-1,-30)], type="prob")

summary(pred_prob_rf_test)

# Calculating the CUTOFF value of probability, where a balanced value of Specificity
# & Sensitivity can be obtained with a better accuracy.

ROC_Values_rf<-roc(test_master$Performance.Tag,pred_prob_rf_test[,2])

cutoff_master_rf<-coords(ROC_Values_rf, "best", ret = "threshold") # cutoff_master = 0.0225

pred_perf_master_rf_test <- factor(ifelse(pred_prob_rf_test[,2]>= cutoff_master_rf, 1, 0)) 

conf_master_rf_test<-confusionMatrix(pred_perf_master_rf_test, test_master$Performance.Tag, positive = "1")

# Specificity = 0.77715, Sensitivity = 0.44117,  Accuracy = 0.4553 is obtained, on applying
# the best model (developed using the master dataset) on the test data.


####____________________________________APPLICATION SCORECARD_____________________________________####


### Clearly, it can be seen that on EVALUATION of the 3 Models prepared above, the best model
### to be used for developing the Application Scorecard, is the "Logistic Regression Model"
### build using the "Master file (combined file of Credit Bureau & demographic data)".


###------------ Calculation of APPLICATION SCORES for MASTER FILE (APPROVED CANDIDATES)

## Creating a new dataframe for developing the SCORECARD, which contains only the Application
## ID(s) & Performance Tags, from the main master data(the one with WOE values)

master_data_scorecard=master_new[,c(1,2)]

## Further, probabilities of Default for the applicants is calculated using the main master 
## data, and stored in the newly created dataframe. The model used for prediction is the 
## LOGISTIC REGRESSION MODEL prepared on the MASTER TRAINING DATA.

master_data_scorecard$pred_prob_default=predict(best_master_model, newdata = master_new[,-1], type="response")

ggplot(master_data_scorecard, aes(x=pred_prob_default, color=Performance.Tag))+geom_density( size = 1 )+
  ggtitle( "Master Dataset Predicted Probabilities of Default" ) + 
  scale_color_economist( name = "Performance.Tag", labels = c( "Defaulter(1)", "Non-Defaulter(0)" ) ) + 
  theme_economist()


## Then, these predicted probabilites of default are converted into probabilites of being
## not Default fr specific customer (As for scorecard calculation, odd of Goods need to be 
## calculated)

master_data_scorecard$pred_prob_good= 1 - master_data_scorecard$pred_prob_default

## Now, the odd of goods is calculated and saved as separate column. Also, the log(odds_good)
## is also calculated.

master_data_scorecard$odd_goods=(master_data_scorecard$pred_prob_good)/(1-master_data_scorecard$pred_prob_good)
master_data_scorecard$log_odds<-log(master_data_scorecard$odd_goods)

## Finally, the SCORES of every customer are calculated using the following formula

master_data_scorecard$scores<- (400-(28.8539*log(10)))+(28.8539*master_data_scorecard$log_odds)

ggplot(master_data_scorecard, aes(x=scores, color=Performance.Tag))+geom_density(size=1)+
  ggtitle( "Master data Application Scores" ) +
  scale_color_economist( name = "Performance.Tag", labels = c( "Defaulter(1)", "Non-Defaulter(0)" ) ) +
  theme_economist()

## Calculate the Application Scores of the Candidates, having predicted probabilties
## of default greater than the CUTOFF predicted probability of default

x=master_data_scorecard$scores[which(master_data_scorecard$pred_prob_default>=cutoff_master)]
summary(x)

## On analyzing the the above application scores and the Density Plot of APPLICATION
## SCORES , it can be deduced that the score of "425.9" can be used as the THRESHOLD/CUTOFF
## APPLICATION SCORE.


###--------Calculation of APPLICATION SCORES for MASTER FILE (REJECTED CANDIDATES)-----
###-------- To VALIDATE the DECIDED CUTOFF APPLICATION SCORE----------------------------

## Creating a new dataframe for developing the SCORECARD, which contains only the Application
## ID(s) & Performance Tags, from the rejected master data(the one with WOE values)

master_data_reject_scorecard=master_file_rejected[,c(1,2)]

## Further, probabilities of Default for the applicants is calculated using the main master 
## data, and stored in the newly created dataframe. The model used for prediction is the 
## LOGISTIC REGRESSION MODEL prepared on the MASTER TRAINING DATA.

master_data_reject_scorecard$pred_prob_default=predict(best_master_model, newdata = master_file_rejected[,-1], type="response")

## Then, these predicted probabilites of default are converted into probabilites of being
## not Default fr specific customer (As for scorecard calculation, odd of Goods need to be 
## calculated)

master_data_reject_scorecard$pred_prob_good= 1 - master_data_reject_scorecard$pred_prob_default

## Now, the odd of goods is calculated and saved as separate column. Also, the log(odds_good)
## is also calculated.

master_data_reject_scorecard$odd_goods=(master_data_reject_scorecard$pred_prob_good)/(1-master_data_reject_scorecard$pred_prob_good)
master_data_reject_scorecard$log_odds<-log(master_data_reject_scorecard$odd_goods)

## Finally, the SCORES of every customer are calculated using the following formula

master_data_reject_scorecard$scores<- (400-(28.8539*log(10)))+(28.8539*master_data_reject_scorecard$log_odds)

ggplot(master_data_reject_scorecard, aes(x=scores))+geom_histogram(bins=6)+ggtitle ("Histogram of Apllication Scores for Rejected Applicants")

## Calculate the no.of applicants in the reject list, below the CUTOFF APPLICATION
## SCORE

defaulters= nrow(subset(master_data_reject_scorecard, master_data_reject_scorecard$scores<425.9))

## The value of defaulters (1346)in the reject list shows that the selected value of 
## CUTOFF APPLICATION SCORE is a VALID VALUE, as it proves that 94.46 % of the initially
## Rejected Customers are actually Defaulters, and thus, have been correctly rejected.

## Hence, a Cut-off of "425.9" will be used for further business decisions. 


###___________________________FINANCIAL BENEFIT ANALYSIS____________________________### ---------

## Now, on predicting the Performance of the applicants in the main master file
## (onehaving all the approved candidates) using the Model,

master_data_scorecard$pred_perf_main<- factor(ifelse(master_data_scorecard$pred_prob_default >= cutoff_master, 1, 0)) 

plot_applicants(master_data_scorecard$pred_perf_main, "Performance_Tag" )

conf_main<-confusionMatrix(pred_perf_main, master_data_scorecard$Performance.Tag, positive = "1")

conf_main

# No. of Defaulters in the master data (correctly Predicted- TP) = 2077

# No.of Non-Defaulters in the master data (correctly Predicted - TN)= 37882

# No. of Defaulters in the master data (Wrongly Predicted - FP) = 29038 (False Positives)

# No.of Non-Defaulters in the master data (Wrongly Predicted - FN)= 870 (False Negatives)

# Total customers predicted as defaulters = 31114

# Total customer predicted as non-defaulters = 38481


## Now, merge the Application scorecard dataset with the master_file, and create a new
## dataset, with the Application ID, Actual & Predicted Performance Tags, and 
## the Outstanding Balance

master_profit_analysis <- merge(master_data_scorecard, master_file, by= c(1,2))

master_profit_analysis <- master_profit_analysis[,c(1,2,7,9,34)]

## Removing the observations with NA value in the OUTSTANDING BALANCE Attribute
 
master_profit_analysis=subset(master_profit_analysis, is.na(master_profit_analysis$Outstanding.Balance)==FALSE)

# After, this step, 

# No. of Defaulters in the master_profit_analysis data (correctly Predicted- TP) = 2077

# No.of Non-Defaulters in the master_profit_analysis data (correctly Predicted - TN)= 37619

# No. of Defaulters in the master_profit_analysis data (Wrongly Predicted - FP) = 29037 (False Positives)

# No.of Non-Defaulters in the master_profit_analysis data (Wrongly Predicted - FN)= 862 (False Negatives)

# Total customers predicted as defaulters = 31114

# Total customer predicted as non-defaulters = 38481


## Calculate the Outstanding Balance of the Applicants without any model.

outbal_actual <- aggregate(as.numeric(Outstanding.Balance)~Performance.Tag,data=n, FUN=sum)
colnames(outbal_actual) <- c("Performance_Actual", "Total_Outstanding_Balance")

## Calculate the Outstanding Balance of the Applicants after the prediction by the model.

outbal_pred <- aggregate(as.numeric(Outstanding.Balance)~pred_perf_main,data=n, FUN=sum)
colnames(outbal_pred) <- c("Performance_Predicted", "Total_Outstanding_Balance")

## Assuming that the Non-Defaulters pay their entire Outstanding Balance, along with 
## some interest say 10 units per customer, and the Defaulters do not pay the Outstanding
## Balance.

PROFIT_without_model = outbal_actual$Total_Outstanding_Balance[1]-outbal_actual$Total_Outstanding_Balance[2]

per_correctly_pred_goods = 37619/38481
per_correctly_pred_defaulters = 2077/31114 

PROFIT_using_model_correct_decision = (per_correctly_pred_goods * outbal_pred$Total_Outstanding_Balance[1])-(per_correctly_pred_defaulters * outbal_pred$Total_Outstanding_Balance[2])

Opportunity_Cost <- (1- per_correctly_pred_defaulters) * outbal_pred$Total_Outstanding_Balance[2]

## Percentage of Profit that can be redeemed using the model,

ratio = PROFIT_using_model_correct_decision / PROFIT_without_model # 54.64 %
 


