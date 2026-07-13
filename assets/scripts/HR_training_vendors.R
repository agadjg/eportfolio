## =============================================================================
# HR Training Vendors Statistical Analysis
## =============================================================================
#Load data
library(readr)
vendor_scores <- read.csv("vendor_scores.csv")

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
boxplot(vendor_scores,
        main = "Distribution of Vendors scores")

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
aov(Score ~ Vendors, data = vendor_scores_long)
summary(aov(Score ~ Vendors, data = vendor_scores_long))

## =============================================================================
## ## Post-Hoc Comparison - TukeyHSD
## =============================================================================

aggregate(Score ~ Vendors, data = vendor_scores_long, mean)
TukeyHSD(aov(Score ~ Vendors, data = vendor_scores_long))



