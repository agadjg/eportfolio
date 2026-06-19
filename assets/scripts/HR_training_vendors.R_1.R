## =============================================================================
# HR Training Vendors Statistical Analysis
## =============================================================================
#Load packages 
library(haven)
library(tidyr)
library(DescTools)

#Load data
HR_vendor_scores <- read_csv("~/vendor_scores.csv")

## =============================================================================
#Data Exploration (Central Tendency and Dispersion)
## =============================================================================
summary(vendor_scores)
str(vendor_scores)

#Vendor 1
sd(vendor_scores$Vendor_1)
IQR(vendor_scores$Vendor_1)

#Vendor 2
sd(vendor_scores$Vendor_2)
IQR(vendor_scores$Vendor_2)

#Vendor 3
sd(vendor_scores$Vendor_3)
IQR(vendor_scores$Vendor_3)

## =============================================================================
#Data Visualisation
## =============================================================================
boxplot(vendor_scores)

## =============================================================================
# Normality Testing
## =============================================================================
shapiro.test(vendor_scores$Vendor_1)
shapiro.test(vendor_scores$Vendor_2)
shapiro.test(vendor_scores$Vendor_3)

## =============================================================================
## One-way ANOVA test
# H0: Mean efficiency of a vendor is the same across the training groups 
# H1: Mean efficiency differs at least in one of  the training groups
## =============================================================================

#Long format conversion prior to test
library(tidyr)
vendor_scores_long <- pivot_longer(
  vendor_scores,
  cols = everything(),
  names_to = "Vendors",
  values_to = "Score"
)

vendor_scores_long

#ANOVA Test
aov(score ~ vendor,don data = Health_Data)
summary(aov(sbp ~ occupation, data = Health_Data))

