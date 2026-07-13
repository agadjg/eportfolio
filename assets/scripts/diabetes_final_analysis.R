## =============================================================================
## DIABETES ANALYSIS - FINAL ASSIGNEMENT - NUMERICAL ANALYSIS
## =============================================================================
# Load packages 
library(tidyverse)

# Import CSV
diabetes <- read.csv("diabetes.csv")

## =============================================================================
## DATA EXPLORATION
## =============================================================================
View(diabetes)
str(diabetes)
class(diabetes)
summary(diabetes)

# Checking for missing variables
sum(is.na(diabetes))
colSums(is.na(diabetes))

# Checking for 0 values
sum(diabetes$Pregnancies == 0, na.rm = TRUE)
sum(diabetes$Glucose == 0, na.rm = TRUE)
sum(diabetes$BloodPressure == 0, na.rm = TRUE)
sum(diabetes$SkinThickness == 0, na.rm = TRUE)
sum(diabetes$Insulin == 0, na.rm = TRUE)
sum(diabetes$BMI == 0, na.rm = TRUE)
sum(diabetes$DiabetesPedigreeFunction == 0, na.rm = TRUE)
sum(diabetes$Age == 0, na.rm = TRUE)

# Encoding missing values to NA
diabetes$Glucose[diabetes$Glucose == 0] <- NA
diabetes$BloodPressure[diabetes$BloodPressure == 0] <- NA
diabetes$SkinThickness[diabetes$SkinThickness == 0] <- NA
diabetes$Insulin[diabetes$Insulin == 0] <- NA
diabetes$BMI[diabetes$BMI == 0] <- NA
colSums(is.na(diabetes))

#-------------------------------------------------------------------------------
# Converting "Outcome" to categories ("No diabetes", "Diabetes") 
diabetes$Outcome <- factor(diabetes$Outcome, levels = c(0, 1), 
                           labels = c("No Diabetes", "Diabetes"))
str(diabetes$Outcome)

# Share of people diagnosed with diabetes 
diabetes_freq <- table(diabetes$Outcome)
diabetes_pct <- round(diabetes_freq/sum(diabetes_freq)*100,1)

cbind(Frequency = diabetes_freq, 
      Percentage = diabetes_pct)

# Visualisation: Diabetes Status
pie(diabetes_freq, labels = paste0(names(diabetes_freq), " (", diabetes_pct, "%)"),
    col = c("white", "steelblue"),
    main = "Diabetes Status")

#-------------------------------------------------------------------------------
# Aggregating pregnancies under "PregnancyStatus" with "Non pregnant", "Pregnant" labels
diabetes$PregnancyStatus <- ifelse(
 diabetes$Pregnancies == 0, "Non pregnant","Pregnant")

# Converting "PregnancyStatus" variable into categorical variable 
diabetes$PregnancyStatus <- factor(diabetes$PregnancyStatus,
                                  levels = c("Non pregnant", "Pregnant"))

# Frequency table for Pregnancy Status
table(diabetes$PregnancyStatus)
pregnant_freq <- table(diabetes$PregnancyStatus)
pregnant_pct <- round(pregnant_freq/sum(pregnant_freq)*100,1)

cbind(Frequency = pregnant_freq, Percentage = pregnant_pct)

# Visualisation: Pregnancy Status
barplot(pregnant_freq,
        main = "Pregnancy Status") 

pie(pregnant_freq,
    labels = paste0(names(pregnant_freq), " (", pregnant_pct, "%)"),
    col = c("white", "steelblue"),
    main = "Pregnancy Status")

## =============================================================================
# Measures of Central Tendency and Dispersion
## =============================================================================
library(DescTools) #mode function

#Age
summary(diabetes$Age)
Mode(diabetes$Age)
sd(diabetes$Age)
IQR(diabetes$Age)

#BMI
summary(diabetes$BMI, na.rm = TRUE)
Mode(diabetes$BMI, na.rm = TRUE)
sd(diabetes$BMI, na.rm = TRUE)
IQR(diabetes$BMI, na.rm = TRUE)

#Glucose level
summary(diabetes$Glucose, na.rm = TRUE)
Mode(diabetes$Glucose, na.rm = TRUE)
sd(diabetes$Glucose, na.rm = TRUE)
IQR(diabetes$Glucose, na.rm = TRUE)

#Blood Pressure
summary(diabetes$BloodPressure, na.rm = TRUE)
Mode(diabetes$BloodPressure, na.rm = TRUE)
sd(diabetes$BloodPressure, na.rm = TRUE)
IQR(diabetes$BloodPressure, na.rm = TRUE)

#Pregnant Cases
PregnantCases <- subset(diabetes, Pregnancies >0)
summary(PregnantCases$Pregnancies)
Mode(PregnantCases$Pregnancies)
sd(PregnantCases$Pregnancies)
IQR(PregnantCases$Pregnancies)

## =============================================================================
# Visualisation of Age, BMI, Glucose, Blood Pressure, Pregnant Cases
## =============================================================================

#Boxplot: Age
boxplot(diabetes$Age, main = "Age")

#Histogram: Age
ggplot(diabetes, aes(x = Age))+ 
  geom_histogram(binwidth = 5,
                 fill = "lightgrey",
                 colour = "black")+
  scale_x_continuous(breaks = seq(20, 90, by = 10))+
  labs(title = "Distribution of Age", 
       x = "Age (years)", 
       y = "Frequency")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"))

#Histogram + Density curve: Age
ggplot(diabetes, aes(x = Age))+ 
  geom_histogram(aes(y= after_stat(density)),
                 binwidth = 4,
                 fill = "lightgrey",
                 colour = "black")+
  geom_density(colour = "orange", linewidth = 0.7)+
  labs(title = "Distribution of Age", 
       x = "Age (years)", 
       y = "Density")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 17, face = "bold"),
        axis.text.y = element_text(size = 17, face = "bold"),
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        plot.title = element_text(size = 20, face = "bold"))

# ------------------------------------------------------------------------------
# Boxplot: BMI
boxplot(diabetes$BMI, main = "BMI")

#Histogram + Density curve: BMI
ggplot(diabetes, aes(x = BMI))+ 
  geom_histogram(aes(y= after_stat(density)),
                 binwidth = 2,
                 fill = "lightgrey",
                 colour = "black", 
                 na.rm = TRUE)+
  geom_density(colour = "orange", linewidth = 0.7, na.rm = TRUE)+
  labs(title = "Distributiun of Body Mass Index", 
       x = "BMI = weight in kg/(height in m)²", 
       y = "Density")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 20, face = "bold"))

# ------------------------------------------------------------------------------
# Boxplot: Glucose level
boxplot(diabetes$Glucose, main = "Glucose")

#Histogram + Density curve: Glucose level
ggplot(diabetes, aes(x = Glucose))+ 
  geom_histogram(aes(y= after_stat(density)),
                 binwidth = 10,
                 fill = "lightgrey",
                 colour = "black", 
                 na.rm = TRUE)+
  geom_density(colour = "orange", linewidth = 0.7, na.rm = TRUE)+
  labs(title = "Distribution of Glucose level", 
       x = "Glucose Concentration", 
       y = "Density")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 20, face = "bold"))

# ------------------------------------------------------------------------------
# Boxplot: BloodPressure
boxplot(diabetes$BloodPressure, main = "Blood Pressure")

#Histogram + Density curve: Blood Pressure
ggplot(diabetes, aes(x = BloodPressure))+ 
  geom_histogram(aes(y= after_stat(density)),
                 binwidth = 4,
                 fill = "lightgrey",
                 colour = "black", 
                 na.rm = TRUE)+
  geom_density(colour = "orange", linewidth = 0.7, na.rm = TRUE)+
  labs(title = "Distribution of Blood Pressure", 
       x = "Blood Pressure", 
       y = "Density")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 20, face = "bold"))

# ------------------------------------------------------------------------------
#Boxplot: PregnantCases
boxplot(PregnantCases$Pregnancies, main = "Pregnancies No.")

#Histogram + Density curve: Pregnancy Counts
ggplot(subset(diabetes, Pregnancies > 0 ), 
       aes(x = Pregnancies)) + 
  geom_histogram(
    aes(y= after_stat(density)), 
    binwidth = 1,
    fill = "lightgrey",
    colour = "black")+
  geom_density(colour = "orange", linewidth = 0.7)+
  scale_x_continuous(breaks = seq(1, max(diabetes$Pregnancies), by = 1))+
  labs(title = "Distributiun of Pregnancy Count", 
       x = "Number of Pregnancies", 
       y = "Density")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 20, face = "bold"))

#-------------------------------------------------------------------------------
# Outliers
boxplot.stats(diabetes$Pregnancies)$out
boxplot.stats(diabetes$Age)$out
boxplot.stats(diabetes$BMI)$out
boxplot.stats(diabetes$BloodPressure)$out
boxplot.stats(diabetes$Glucose)$out

## =============================================================================
# Diabetes Rate across Age Groups including Visualisation
## =============================================================================

# Age Groups creation with correspond age category label 
diabetes$AgeGroup <- ifelse(
  diabetes$Age <= 29, "<30",
  ifelse(
    diabetes$Age <= 39, "30-39",
    ifelse(
      diabetes$Age <= 49, "40-49",
      ifelse(
        diabetes$Age <= 81, "50+"))))

# Converting AgeGroup variable into categorical variable                 
diabetes$AgeGroup <- factor(diabetes$AgeGroup, levels = c("<30", "30-39", "40-49", "50+"))

# Frequency table for AgeGroup
table(diabetes$AgeGroup)

# Diabetes and Non-diabetes rate across age groups
outcome_freq_age_tb <- table(diabetes$AgeGroup, diabetes$Outcome)
outcome_pct_age_tb <- round(prop.table(outcome_freq_age_tb, 1)*100, 1)

# Diabetes rate ONLY across age groups
diabetes_rate_age_tb <- outcome_pct_age_tb[, "Diabetes"] 

# Diabetes rate ONLY across age groups converted into a data frame
diabetes_rate_age_df <- data.frame(AgeGroup = rownames(outcome_pct_age_tb),
                               DiabetesPercentage = diabetes_rate_age_tb) 

# ------------------------------------------------------------------------------
# Visualisation: Diabetes rate ONLY across age groups, 
ggplot(diabetes_rate_age_df, aes(x = AgeGroup, y = DiabetesPercentage)) + 
  geom_col(fill = "lightgrey", colour = "darkgrey") +
  geom_text(aes(label = paste0(DiabetesPercentage, "%")), size = 7, vjust = 2, colour = "black")+
  ylim(0, 60)+
  labs(
    title = "Diabetes Rate by Age Groups", 
       x = "Age Group", 
       y = "Diabetes (%)") +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 17, face = "bold"),
        axis.text.y = element_text(size = 17, face = "bold"),
        axis.title.x = element_text(size = 17, margin = margin(t = 5)),
        axis.title.y = element_text(size = 17),
        plot.title = element_text(size = 20, face = "bold"))
  

## =============================================================================
# Diabetes Rate across BMI Groups including Visualisation
## =============================================================================

# BMI Groups creation with correspond BMI category label according to CDC(2024)
# (https://www.cdc.gov/bmi/adult-calculator/bmi-categories.html) 

diabetes$BMIGroup <- ifelse(
  diabetes$BMI <= 18.49, "underweight",
  ifelse(
    diabetes$BMI <= 24.9, "normal",
    ifelse(
      diabetes$BMI <= 29.9, "overweight",
      ifelse(
        diabetes$BMI <= 67.1, "obese"))))

# Converting BMIGroup variable into categorical variable                 
diabetes$BMIGroup <- factor(diabetes$BMIGroup, 
                            levels = c("underweight", "normal", "overweight", "obese"))

# Frequency table for BMIGroup
table(diabetes$BMIGroup)

# Diabetes and Non-diabetes rate across BMI groups
outcome_freq_bmi_tb <- table(diabetes$BMIGroup, diabetes$Outcome)
outcome_pct_bmi_tb <- round(prop.table(outcome_freq_bmi_tb, 1)*100, 1)

# Diabetes rate ONLY across BMI groups
diabetes_rate_bmi_tb <- outcome_pct_bmi_tb[, "Diabetes"] 

# Diabetes rate ONLY across BMI groups converted into a data frame
diabetes_rate_bmi_df <- data.frame(BMIGroup = rownames(outcome_pct_bmi_tb),
                                   DiabetesPercentage = diabetes_rate_bmi_tb) 

# ------------------------------------------------------------------------------
# Visualisation: Diabetes rate ONLY across BMI groups, 
ggplot(diabetes_rate_bmi_df, aes(x = reorder(BMIGroup, -DiabetesPercentage), 
                                 y = DiabetesPercentage)) + 
  geom_col(fill = "lightgrey", colour = "darkgrey") +
  scale_x_discrete(limits = c("underweight", "normal", "overweight", "obese")) +
  geom_text(aes(label = ifelse(DiabetesPercentage == 0, "", paste0(DiabetesPercentage, "%"))), size = 7, vjust = 2, colour = "black")+
  ylim(0, 50)+
  labs(
    title = "Diabetes Rate by BMI Groups", 
    x = "BMI Group", 
    y = "Diabetes (%)") +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 17, face = "bold"),
        axis.text.y = element_text(size = 17, face = "bold"),
        axis.title.x = element_text(size = 17, margin = margin(t = 5)),
        axis.title.y = element_text(size = 17),
        plot.title = element_text(size = 20, face = "bold"))


## =============================================================================
# Diabetes Rate by number of Pregnancies including Visualisation
## =============================================================================

# Pregnancy Groups creation with correspond number of pregnancies  
diabetes$PregnancyCount <- ifelse(
  diabetes$Pregnancies <= 0, "0",
  ifelse(
    diabetes$Pregnancies <= 2, "1-2",
    ifelse(
      diabetes$Pregnancies <= 4, "3-4",
      ifelse(
        diabetes$Pregnancies <= 17, "5+"))))

# Converting Pregnancy Count variable into categorical variable                 
diabetes$PregnancyCount <- factor(diabetes$PregnancyCount, 
                                  levels = c("0", "1-2", "3-4", "5+"))

# Frequency table for Pregnancy Count
table(diabetes$PregnancyCount)

# Diabetes and Non-diabetes rate across Pregnancy Group
outcome_freq_preg_tb <- table(diabetes$PregnancyCount, diabetes$Outcome)
outcome_pct_preg_tb <- round(prop.table(outcome_freq_preg_tb, 1)*100, 1)

# Diabetes rate ONLY across Pregnancy Group
diabetes_rate_preg_tb <- outcome_pct_preg_tb[, "Diabetes"] 

# Diabetes rate ONLY across BMI groups converted into a data frame
diabetes_rate_preg_df <- data.frame(PregnancyCount = rownames(outcome_pct_preg_tb),
                                   DiabetesPercentage = diabetes_rate_preg_tb) 

# ------------------------------------------------------------------------------
# Visualisation: Diabetes rateacross Pregnant and Non-Pregnant groups 
ggplot(diabetes_rate_preg_df,
       aes(x = PregnancyCount, y = DiabetesPercentage)) + 
  geom_col(fill = "lightgrey", colour = "darkgrey") +
  geom_text(aes(label = paste0(DiabetesPercentage, "%")), size = 7, vjust = 2, colour = "black")+
  ylim(0, 50)+
  labs(
    title = "Diabetes Rate by Number of Pregnancies", 
    x = "Pregnancy Count", 
    y = "Diabetes (%)") +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 17, face = "bold"),
        axis.text.y = element_text(size = 17, face = "bold"),
        axis.title.x = element_text(size = 17, margin = margin(t = 5)),
        axis.title.y = element_text(size = 17),
        plot.title = element_text(size = 20, face = "bold"))

write.csv(diabetes, "diabetes_cleaned.csv",
          row.names = FALSE
)

# =============================================================================
# Hypothesis testing 1: Glucose ~ Outcome (Diabetes and No-Diabetes)
## =============================================================================
# Data characteristics
# Glucose level characteristic across Diabetes and No Diabetes
aggregate(Glucose ~ Outcome, na.rm = TRUE, data = diabetes, median)
aggregate(Glucose ~ Outcome, na.rm = TRUE, data = diabetes, mean)
aggregate(Glucose ~ Outcome, na.rm = TRUE, data = diabetes, IQR)
aggregate(Glucose ~ Outcome, na.rm = TRUE, data = diabetes, Mode)
aggregate(Glucose ~ Outcome, na.rm = TRUE, data = diabetes, sd)

# Glucose sample size across Diabetes and No Diabetes
table(diabetes$Outcome[!is.na(diabetes$Glucose)])

#Outliers
boxplot.stats(diabetes$Glucose[diabetes$Outcome == "Diabetes"])$out
boxplot.stats(diabetes$Glucose[diabetes$Outcome == "No Diabetes"])$out

# ------------------------------------------------------------------------------
# Visualisation
# Boxplot: Glucose distribution across Diabetes and No-Diabetes 
boxplot(Glucose ~ Outcome, data = diabetes, 
        main = "Distribution of Glucose by Outcome")

# Q-Q-Plot: Glucose distribution by Diabetes and No-Diabetes 
ggplot(diabetes, aes(sample = Glucose)) +
  stat_qq() +
  stat_qq_line(colour = "red") + 
  facet_wrap(~ Outcome) +
  labs(title = "Distribution of Pregnancies by Diabetes and No-Diabetes") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 15, face = "bold"))

# Histogram: Glucose distribution and density curve by Diabetes and No-Diabetes
ggplot(diabetes, aes(x = Glucose)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 5,
                 fill = "lightgrey", 
                 colour = "black",
                 na.rm = TRUE) +
  geom_density(colour = "orange", na.rm = TRUE) +
  facet_wrap(~Outcome) + 
  labs(title = "Distribution of Glucose Levels across Diabetes and No-Diabetes") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 9, face = "bold"),
      axis.text.y = element_text(size = 9, face = "bold"),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      strip.text = element_text(size = 12),
      plot.title = element_text(size = 15, face = "bold"))

## =============================================================================
# Normality Assumption (shapiro.test)
## =============================================================================
# Glucose
shapiro.test(diabetes$Glucose[diabetes$Outcome == "Diabetes"])
shapiro.test(diabetes$Glucose[diabetes$Outcome == "No Diabetes"])

## =============================================================================
# Hypothesis Testing: Mann-Whitney U test (Wilcoxon test) 
# H0: The rank sum of Glucose level is the same for Diabetes and No Diabetes.
# H1: The rank sum of Glucose level differs among Diabetes and No Diabetes.
## =============================================================================
wilcox.test(Glucose ~ Outcome, data = diabetes)

# ==============================================================================
# Hypothesis testing 2: Pregnancies ~ Outcome (Diabetes and No Diabetes)
## =============================================================================
#Data characteristics

#No of Pregnancies across Diabetes and No Diabetes
aggregate(Pregnancies ~ Outcome, data = diabetes, median)
aggregate(Pregnancies ~ Outcome, data = diabetes, mean)
aggregate(Pregnancies ~ Outcome, data = diabetes, IQR)
aggregate(Pregnancies ~ Outcome, data = diabetes, Mode)
aggregate(Pregnancies ~ Outcome, data = diabetes, sd)

#Pregnancies No. sample size across Diabetes and No Diabetes
table(diabetes$Outcome[!is.na(diabetes$Pregnancies)])

#Outliers
boxplot.stats(diabetes$Pregnancies[diabetes$Outcome == "Diabetes"])$out
boxplot.stats(diabetes$Pregnancies[diabetes$Outcome == "No Diabetes"])$out

#-------------------------------------------------------------------------------
#Visualisation
#Boxplot: Pregnancies distribution by Diabetes and No-Diabetes 
boxplot(Pregnancies ~ Outcome, data = diabetes, 
        main = "Distribution of Pregnancies by Outcome") 

#Q-Q-Plot:Pregnancies distribution by Diabetes and No-Diabetes 
ggplot(diabetes, aes(sample = Pregnancies)) +
  stat_qq() +
  stat_qq_line(colour = "red") + 
  facet_wrap(~ Outcome) +
  labs(title = "Distribution of Pregnancies by Diabetes and No-Diabetes") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 15, face = "bold"))

#Histogram: Pregnancies distribution and density curve by Diabetes and No-Diabetes
ggplot(diabetes, aes(x = Pregnancies)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 1,
                 fill = "lightgrey", 
                 colour = "black") +
  geom_density(colour = "orange") +
  facet_wrap(~Outcome) + 
  labs(title = "Distribution of Pregnancies across Diabetes and No-Diabetes") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 15, face = "bold"))

## =============================================================================
# Normality Assumption (shapiro.test)
## =============================================================================
# Pregnancies
shapiro.test(diabetes$Pregnancies[diabetes$Outcome == "Diabetes"])
shapiro.test(diabetes$Pregnancies[diabetes$Outcome == "No Diabetes"])

## =============================================================================
# Hypothesis Testing: Wilcoxon test
# H0: There is no difference in the rank sums of pregnancies between diabetics and non-diabetics.
# H1: There is difference in the rank sums of pregnancies between diabetics and non-diabetics.
## =============================================================================
wilcox.test(Pregnancies ~ Outcome, data = diabetes)


# ==============================================================================
# Hypothesis Testing 3 (Glucose ~ Age Groups)
## =============================================================================

#Data characteristics
#Glucose level
aggregate(Glucose ~ AgeGroup, data = diabetes, median)
aggregate(Glucose ~ AgeGroup, data = diabetes, mean)
aggregate(Glucose ~ AgeGroup, data = diabetes, IQR)
aggregate(Glucose ~ AgeGroup, data = diabetes, Mode)
aggregate(Glucose ~ AgeGroup, data = diabetes, sd)

#Glucose sample size across Age Groups
table(diabetes$AgeGroup[!is.na(diabetes$Glucose)])

#Outliers
boxplot.stats(diabetes$Glucose[diabetes$AgeGroup == "<30"])$out
boxplot.stats(diabetes$Glucose[diabetes$AgeGroup == "30-39"])$out
boxplot.stats(diabetes$Glucose[diabetes$AgeGroup == "40-49"])$out
boxplot.stats(diabetes$Glucose[diabetes$AgeGroup == "50+"])$out

#-------------------------------------------------------------------------------
#Visualisation 
#Glucose distribution by Age Groups 
boxplot(Glucose ~ AgeGroup, data = diabetes, 
        main = "Distribution of Glucose Levels across Age Groups")

#Q-Q-Plot: Distribution of Glucose Levels across Age Groups
ggplot(diabetes, aes(sample = Glucose)) +
  stat_qq() +
  stat_qq_line(colour = "red") + 
  facet_wrap(~ AgeGroup) +
labs(title = "Distribution of Glucose Levels across Age Groups") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 15, face = "bold"))

# Histogram: Glucose distribution and density curve by Age Group
ggplot(diabetes, aes(x = Glucose)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 5,
                 fill = "lightgrey", 
                 colour = "black", 
                 na.rm = TRUE) +
  geom_density(colour = "orange", na.rm = TRUE) +
  facet_wrap(~AgeGroup) + 
  labs(title = "Distribution of Glucose Levels across Age Groups") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 15, face = "bold"))

## =============================================================================
# Normality Assumption (shapiro.test)
## =============================================================================
#Glucose
shapiro.test(diabetes$Glucose[diabetes$AgeGroup == "<30"])
shapiro.test(diabetes$Glucose[diabetes$AgeGroup == "30-39"])
shapiro.test(diabetes$Glucose[diabetes$AgeGroup == "40-49"])
shapiro.test(diabetes$Glucose[diabetes$AgeGroup == "50+"])

## =============================================================================
## Kruskal-Wallis test
## H0: There is no difference in the rank sums of glucose level across different age groups 
## H1: There is a difference in the rank sums of glucose level across different age groups
## =============================================================================
kruskal.test(Glucose ~ AgeGroup, data = diabetes)

## =============================================================================
## Post hoc Dunn's Pairwise Comparison
## =============================================================================
library(FSA)
dunnTest(Glucose ~ AgeGroup, data = diabetes)

# ==============================================================================
# Hypothesis Testing 4: Association between Diabetes outcome and Age Groups
#H0: There is no association between Diabetes outcome and Age groups. The variables are independent
#H1: There is an association between Diabetes outcome and Age groups.
## =============================================================================

#Investigate variables types
str(diabetes$Outcome)
str(diabetes$AgeGroup)
str(diabetes$BMIGroup)

#Visualise the relationship between Diabetes outcome and Age Groups - Contingency table for AgeGroup
AgeGroup_tb <- table(diabetes[, c("AgeGroup", "Outcome")], dnn = c("AgeGroup", "Outcome")) # dnn adds the variable names
AgeGroup_tb <- addmargins(AgeGroup_tb) # addmargins adds the row and column totals
AgeGroup_tb

# Calculating the Diabetes outcome percentages by Age Group
AgeGroup_pct_tb <- round(prop.table(table(diabetes$AgeGroup, diabetes$Outcome), margin = 1) * 100, 1)

# AgeGroup_pct converted into a data frame
AgeGroup_pct_df <- data.frame(AgeGroup_pct_tb)
names(AgeGroup_pct_df) <- c("AgeGroup", "Outcome", "Percentage")

#-------------------------------------------------------------------------------
# Visaulisation: Barchart Diabetes outcome proportion across Age Groups
ggplot(AgeGroup_pct_df, aes(x = AgeGroup, y = Percentage, fill = Outcome)) +
  geom_bar(stat = "identity", position = "stack", colour = "grey") +
  geom_text(aes(label = Percentage), position = position_stack(vjust = 0.5), size = 6, colour = "black")+
  scale_fill_manual(values = c("No Diabetes" = "white","Diabetes" = "lightgrey"))+
  
  labs(
    title = "Diabetes Outcome by Age Group",
    x = "Age Group",
    y = "Percentage", 
    fill = "Outcome") +
  
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 16, margin = margin(t = 5)),
        axis.title.y = element_text(size = 16),
        strip.text = element_text(size = 16),
        plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12))

# Cell count check followed by Chi-square test (AgeGroup)
chisq.test(table(diabetes[, c('AgeGroup', 'Outcome')]))$expected
chisq.test(table(diabetes[, c('AgeGroup', 'Outcome')]))

# ==============================================================================
# Hypothesis Testing 5: Association between Diabetes outcome and BMI Groups
#H0: There is no association between Diabetes outcome and BMI groups. The variables are independent
#H1: There is an association between Diabetes outcome and BMI groups.
## =============================================================================

# Visualise the relationship between Diabetes outcome and BMI Groups - Contingency table for BMI Group
BMIGroup_tb <- table(diabetes[, c('BMIGroup', 'Outcome')], dnn = c('BMIGroup', 'Outcome')) # dnn adds the variable names
BMIGroup_tb <- addmargins(BMIGroup) # addmargins adds the row and column totals
BMIGroup_tb

# Calculating the BMI Group percentages
BMIGroup_pct_tb <- round(prop.table(table(diabetes$BMIGroup, diabetes$Outcome), margin = 1) * 100, 1)

# BMIGroup_pct converted into a data frame
BMIGroup_pct_df <- data.frame(BMIGroup_pct_tb)
names(BMIGroup_pct_df) <- c("BMIGroup", "Outcome", "Percentage")

#-------------------------------------------------------------------------------
# Visaulisation: Barchart Diabetes Outcome proportion across BMI Groups.
ggplot(BMIGroup_pct_df, aes(x = BMIGroup, y = Percentage, fill = Outcome)) +
  geom_bar(stat = "identity", position = "stack", colour = "grey") +
  geom_text(aes(y = Percentage, label = ifelse(Percentage > 0, Percentage, "")), position = position_stack(vjust = 0.5), size = 6, colour = "black")+
  scale_fill_manual(values = c("No Diabetes" = "white","Diabetes" = "lightgrey"))+
  
  labs(
    title = "Diabetes Outcome by BMI Group",
    x = "BMI Group",
    y = "Percentage", 
    fill = "Outcome") +
  
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 16, margin = margin(t = 5)),
        axis.title.y = element_text(size = 16),
        strip.text = element_text(size = 16),
        plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12))

# Cell count check followed by Chi-square test (BMIGroup)
chisq.test(table(diabetes[, c('BMIGroup', 'Outcome')]))$expected
chisq.test(table(diabetes[, c('BMIGroup', 'Outcome')]))

# Fisher test (Chi-squared approximation incorrect)
fisher.test(table(diabetes[, c('BMIGroup', 'Outcome')]))

## ==============================================================================
# Correlation Matrix across all continuous variables
## =============================================================================

#Correlation matrix (Heatmap)
library(GGally)
corr_diabetes <- diabetes[, c("Pregnancies", "Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI", "DiabetesPedigreeFunction", "Age")]

ggcorr(corr_diabetes,
       label = TRUE, label_round = 2, label_size = 3, hjust = 0.9, layout.exp = 2,
       low = "darkred", mid = "white",high = "steelblue",
       colour = "black") + 
  labs(title = "Correlation Analysis of Clinical Predictors") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.7)) 


#Scatterplot matrix (additional visualisation)
ggpairs(diabetes[, sapply(diabetes, is.numeric)],
        title = "Correlation Analysis of Diabetes Risk Factors")

## ==============================================================================
# Multiple Linear Regression model
# Relationships Glucose ~ Age + BMI, Pregnancies + BloodPressure + 
# + SkinThickness + Insulin + DiabetesPedigreeFunction
## =============================================================================
# Evaluating all variables

#Age
summary(diabetes$Age)
Mode(diabetes$Age)
sd(diabetes$Age)
IQR(diabetes$Age)

#BMI
summary(diabetes$BMI, na.rm = TRUE)
Mode(diabetes$BMI, na.rm = TRUE)
sd(diabetes$BMI, na.rm = TRUE)
IQR(diabetes$BMI, na.rm = TRUE)

#Blood Pressure
summary(diabetes$BloodPressure, na.rm = TRUE)
Mode(diabetes$BloodPressure, na.rm = TRUE)
sd(diabetes$BloodPressure, na.rm = TRUE)
IQR(diabetes$BloodPressure, na.rm = TRUE)

#Pregnancies
summary(diabetes$Pregnancies)
Mode(diabetes$Pregnancies)
sd(diabetes$Pregnancies)
IQR(diabetes$Pregnancies)

#SkinThickness 
summary(diabetes$Glucose, na.rm = TRUE)
Mode(diabetes$Glucose, na.rm = TRUE)
sd(diabetes$Glucose, na.rm = TRUE)
IQR(diabetes$Glucose, na.rm = TRUE)

#Insulin
summary(diabetes$Insulin, na.rm = TRUE)
Mode(diabetes$Insulin, na.rm = TRUE)
sd(diabetes$Insulin, na.rm = TRUE)
IQR(diabetes$Insulin, na.rm = TRUE)

#DiabetesPedigreeFunction
summary(diabetes$DiabetesPedigreeFunction)
Mode(diabetes$DiabetesPedigreeFunction)
sd(diabetes$DiabetesPedigreeFunction)
IQR(diabetes$DiabetesPedigreeFunction)

#Glucose
summary(diabetes$Glucose, na.rm = TRUE)
Mode(diabetes$Glucose, na.rm = TRUE)
sd(diabetes$Glucose, na.rm = TRUE)
IQR(diabetes$Glucose, na.rm = TRUE)

#-------------------------------------------------------------------------------
#Visualisation (Additional investigation)
# Relationships between Glucose ~ Age + BMI, Pregnancies + BloodPressure + 
# + SkinThickness + Insulin + DiabetesPedigreeFunction)

# Scatterplotes (Glucose ~ Age)
p1 <- ggplot(diabetes, aes(x = Age, y = Glucose)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, colour = "red") +
  labs(x = "Age",
       y = "Glucose level") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 15, face = "bold"))

# Scatterplotes (Glucose ~ BMI)
p2 <- ggplot(diabetes, aes(x = BMI, y = Glucose)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, colour = "red") +
  labs(x = "BMI",
       y = "Glucose level") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 15, face = "bold"))

# Scatterplotes (Glucose ~ Blood Pressure)
p3 <- ggplot(diabetes, aes(x = BloodPressure, y = Glucose)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, colour = "red") +
  labs(x = "BloodPressure",
       y = "Glucose level") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 15, face = "bold"))

# Scatterplotes (Glucose ~ Pregnancies)
p4 <- ggplot(diabetes, aes(x = Pregnancies, y = Glucose)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, colour = "red") +
  labs(x = "No of Pregnancies",
       y = "Glucose level") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 15, face = "bold"))

# Scatterplotes (Glucose ~ SkinThickness)
p5 <- ggplot(diabetes, aes(x = SkinThickness, y = Glucose)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, colour = "red") +
  labs(x = "SkinThickness",
       y = "Glucose level") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 15, face = "bold"))

# Scatterplotes (Glucose ~ Insulin)
p6 <- ggplot(diabetes, aes(x = Insulin, y = Glucose)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, colour = "red") +
  labs(x = "Insulin",
       y = "Glucose level") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 15, face = "bold"))

# Scatterplotes (Glucose ~ DiabetesPedigreeFunction)
p7 <- ggplot(diabetes, aes(x = DiabetesPedigreeFunction, y = Glucose)) +
  geom_point(alpha = 0.7, na.rm = TRUE) +
  geom_smooth(method = "lm", se = TRUE, colour = "red", na.rm = TRUE) +
  labs(x = "DiabetesPedigreeFunction",
       y = "Glucose level") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 15, face = "bold"))

# Plotting all scatterplots together
library(patchwork)
wrap_plots(p1, p2, p3, p4, p5, p6, p7, ncol = 3) +
  plot_annotation(title = "Relationships between Glucose and Predictor variables") &
  theme(plot.title = element_text(size = 15, face = "bold"))

## ==============================================================================
# Multiple Linear Regression Model: Glucose ~ Age + BMI + Pregnancies +
# BloodPressure + SkinThickness + Insulin + DiabetesPedigreeFunction
## =============================================================================
lm_glucose <- lm(
  Glucose ~ BMI + Age + Pregnancies + BloodPressure + SkinThickness + Insulin + 
    DiabetesPedigreeFunction, data = diabetes)

summary(lm_glucose) #coefficients and p-values
confint(lm_glucose) # confidence intervals

# Multicollinearity check (Additional investigation)
library(car)
vif(lm_glucose)

# Visualisation
par(mfrow = c(2, 2))
plot(lm_glucose)

## ==============================================================================
# Logistic Regression model: Outcome ~ Age + BMI + Glucose
## =============================================================================

# Fitting logistic regression model
glm_outcome_m1 <- glm(Outcome ~ BMI + Age + Glucose, 
                 data = diabetes, family = binomial)

summary(glm_outcome_m1)

# Obtaining odds ratio m1
exp(coef(glm_outcome_m1))
exp(confint(glm_outcome_m1))

#Combined outcome
cbind(OR = exp(coef(glm_outcome_m1)), exp(confint(glm_outcome_m1)))

## ==============================================================================
# Outcome Model (glm_outcome_m1): Hosmer-Lemeshow test (goodness of fit) 
## =============================================================================
library(ResourceSelection)

### ----------------------------------------------------------------------------
# Fix the Hoslem errors: Check the values expected  the length
as.numeric(diabetes$Outcome)
as.numeric(diabetes$Outcome) - 1

# Check the the observation of original datset and fitted observations
length(fitted(glm_outcome))
length(diabetes$Outcome)
### ----------------------------------------------------------------------------

# Model data to align observed diabetes values with model predicted probabilities
glm_outcome_data <- model.frame(glm_outcome)

### ----------------------------------------------------------------------------
# Check the the observation of original dataset and fitted observations
length(fitted(glm_outcome_m1))
length(glm_outcome_data$Outcome)
### ----------------------------------------------------------------------------

# Run Hoslem test with new data frame and internal conversion to (0, 1 values)
hoslem.test(x = as.numeric(glm_outcome_data$Outcome) - 1,
            y = fitted(glm_outcome_m1))

## ==============================================================================
# Outcome Model Evaluation (Accuracy, Specificity, Sensitivity) based on Confusion Matrix
## =============================================================================
library(caret)

# Probabilities creation 
prob_outcome <- predict(glm_outcome_m1, type = "response")
pred_outcome <- ifelse(prob_outcome >= 0.5, "Diabetes", "No Diabetes")

#Convert pred_outcome4 into factor as both matrix variable must be factor
pred_outcome <- factor(pred_outcome, levels = levels(glm_outcome_data$Outcome))

#Confusion Matrix
confusionMatrix(pred_outcome, glm_outcome_data$Outcome, positive = "Diabetes")

## =============================================================================
# Likelihood ratio tests 
# H0: The interaction between BMI and Age does not improve the model predictions
# H1: The interaction between BMI and Age improves the model predictions
## =============================================================================

# Fitting logistic regression base model (BMI + Age + Glucose)
glm_outcome_m1 <- glm(Outcome ~ BMI + Age + Glucose, 
                      data = diabetes, family = binomial)

# Fitting the interaction Model: (BMI + Age + Glucose + BMI x Age)
glm_outcome_m1_inter <- glm(Outcome ~  BMI + Age + Glucose + BMI: Age, data = diabetes, family = binomial)

#Model comparison using Likelihood Ratio Test (LRT)
anova(glm_outcome_m1, glm_outcome_m1_inter, test = "Chisq")

## =============================================================================
# Likelihood ratio tests Outcome ~ Main Effect vs Main Effect + Interactions
## Main effects only (BMI + Age + Glucose + Pregnancies)
## Main effects + BMI × Age interaction
## =============================================================================

# Fitting the extended Main effect Model: BMI + Age + Glucose + Pregnancies
glm_outcome_m2 <- glm(Outcome ~ BMI + Age + Glucose + Pregnancies, data = diabetes, family = binomial)

# Fitting the interaction Model: Main effects + BMI × Age interaction
glm_outcome_m2_inter <- glm(Outcome ~ BMI + Age + Glucose + Pregnancies + BMI:Age, data = diabetes, family = binomial)

# Model comparison using Likelihood Ratio Test (LRT)
anova(glm_outcome_m2, glm_outcome_m2_inter, test = "Chisq")

# Retaining extended Main Effect Model
summary(glm_outcome_m2)

#Coefficient and Confidence interval
exp(coef(glm_outcome_m2))
exp(confint(glm_outcome_m2))

#Combined outcome
cbind(OR = exp(coef(glm_outcome_m2)), exp(confint(glm_outcome_m2)))

## =============================================================================
############ Additional analysis (OUT OF SCOPE of the assignment) ##############
## =============================================================================

## =============================================================================
# Outcome Model (glm_outcome_m2): Hosmer-Lemeshow test (goodness of fit) 
## =============================================================================
library(ResourceSelection)

# Model data to align observed diabetes values with model predicted probabilities
glm_outcome_data2 <- model.frame(glm_outcome_m2)

### ----------------------------------------------------------------------------
# Check the the observation of original dataset and fitted observations
length(fitted(glm_outcome_m2))
length(glm_outcome_data$Outcome)
### ----------------------------------------------------------------------------

# Run Hoslem test with new data frame and internal conversion to (0, 1 values)
hoslem.test(x = as.numeric(glm_outcome_data4$Outcome) - 1,
            y = fitted(glm_outcome_m2))

## ==============================================================================
# Outcome Model Evaluation (Accuracy, Specificity, Sensitivity) based on Confusion Matrix
## =============================================================================
library(caret)

# Probabilities creation 
prob_outcome2 <- predict(glm_outcome_m2, type = "response")
pred_outcome2 <- ifelse(prob_outcome2 >= 0.5, "Diabetes", "No Diabetes")

#Convert pred_outcome4 into factor as both matrix variable must be factor
pred_outcome2 <- factor(pred_outcome2, levels = levels(glm_outcome_data2$Outcome))

#Confusion Matrix
confusionMatrix(pred_outcome2, glm_outcome_data2$Outcome, positive = "Diabetes")


