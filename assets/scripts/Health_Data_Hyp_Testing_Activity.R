## =============================================================================
# Unit 7: Health_Data_Activities and Hypothesis Testing
## =============================================================================
View(Health_Data)
summary(Health_Data)
sapply(Health_Data, class)

## =============================================================================
# Mean, Median, Mode for sbp, dbp and income
## =============================================================================
library(DescTools)

#Data characteristic of Systolic BP (sbp) 
mean(Health_Data$sbp)
median(Health_Data$sbp)
Mode(Health_Data$sbp)

#Data characteristic - Diastolic BP (dbp)
mean(Health_Data$dbp)
median(Health_Data$dbp)
Mode(Health_Data$dbp)

#Data characteristic - Monthly income (income)
mean(Health_Data$income)
median(Health_Data$income)
Mode(Health_Data$income)

#Five-figure summary - Monthly income (income)
summary(Health_Data$income)

#Histogram and Boxplot - Monthly income (income)
hist(Health_Data$income)    #shape of distribution
boxplot(Health_Data$income, horizontal = TRUE) #median, spread, outliers

# =============================================================================
#Hypothesis testing
## =============================================================================
#Inspecting data type

str(Health_Data$sbp)
str(Health_Data$pepticulcer)

boxplot(sbp ~ pepticulcer, data = Health_Data) #median, spread, outliers

## =============================================================================
# Normality Testing - shapiro.test
## =============================================================================
shapiro.test(Health_Data$sbp)

## =============================================================================
# Hypothesis Testing: Independent Two-Sample t-Test
# H0: Mean systolic blood pressure (sbp) is the same for patients with and without peptic ulcer condition.
# H1: Mean systolic blood pressure (sbp) differs between patients with and without peptic ulcer condition
## =============================================================================

t.test(sbp~pepticulcer,data = Health_Data)

