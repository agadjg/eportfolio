## =============================================================================
# Unit 7: Health_Data_Activities and Hypothesis Testing
## =============================================================================
library(haven)
library(DescTools)
library(ggplot2)
library(nortest)
library(vioplot)

# Import data
Health_Data <- read_sav("Health_data/Health Data.sav")

View(Health_Data)
summary(Health_Data)
sapply(Health_Data, class)

## =============================================================================
# Mean, Median, Mode for sbp, dbp and income
## =============================================================================
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
#Hypothesis testing 1: sbp ~ pepticulcer (Pepticulcer and No Pepticulcer)
## =============================================================================
#Inspecting data type

str(Health_Data$sbp)
str(Health_Data$pepticulcer)
table(Health_Data$pepticulcer)

#Converting pepticulcer into categorical factor variable
Health_Data$pepticulcer <- factor(Health_Data$pepticulcer, levels = c(1, 2), labels = c("Pepticulcer", "No Pepticulcer"))
str(Health_Data$pepticulcer)

#Data characteristics: sbp across pepticulcer condition
aggregate(sbp ~ pepticulcer, data = Health_Data, median)
aggregate(sbp ~ pepticulcer, data = Health_Data, mean)
aggregate(sbp ~ pepticulcer, data = Health_Data, IQR)
aggregate(sbp ~ pepticulcer, data = Health_Data, Mode)
aggregate(sbp ~ pepticulcer, data = Health_Data, sd)

#Outliers of sbp across pepticulcer groups
boxplot.stats(Health_Data$sbp[Health_Data$pepticulcer == "Pepticulcer"])$out
boxplot.stats(Health_Data$sbp[Health_Data$pepticulcer == "No Pepticulcer"])$out

# sbp distribution by pepticulcer groups
boxplot(sbp ~ pepticulcer, data = Health_Data, 
        main = "Distribution of sbp across pepticulcer groups")

# Distribution of sbp across pepticulcer groups
ggplot(Health_Data, aes(x = sbp)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 15,
                 fill = "lightgrey", 
                 colour = "black") +
  geom_density(colour = "orange") +
  facet_wrap(~pepticulcer) + 
  labs(title = "Distribution of sbp across pepticulcer groups") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 15, face = "bold"))


# Q-Q-Plot: Distribution of sbp across pepticulcer groups
ggplot(Health_Data, aes(sample = sbp)) +
  stat_qq() +
  stat_qq_line(colour = "red") + 
  facet_wrap(~ pepticulcer) +
  labs(title = "Distribution of sbp across pepticulcer groups") +
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
# Normality Assumption - shapiro.test
## =============================================================================

shapiro.test(Health_Data$sbp[Health_Data$pepticulcer == "Pepticulcer"])
shapiro.test(Health_Data$sbp[Health_Data$pepticulcer == "No Pepticulcer"])

## =============================================================================
# Hypothesis Testing: Wilcox.test
# H0: The rank sum of systolic blood pressure (sbp) is the same for patients with and without peptic ulcer condition.
# H1: The rank sum of systolic blood pressure (sbp) differs between patients with and without peptic ulcer condition
## =============================================================================
wilcox.test(sbp~pepticulcer,data = Health_Data)

# =============================================================================
#Hypothesis testing 2: diastolic blood pressure ~ diabetes 
## =============================================================================
#Inspecting data type

str(Health_Data$dbp)
str(Health_Data$diabetes)
table(Health_Data$diabetes)

#Converting diabetes into categorical factor variable
Health_Data$diabetes <- factor(Health_Data$diabetes, levels = c(1, 2), labels = c("Diabetes", "No Diabetes"))
str(Health_Data$diabetes)

#Data characteristics: dbp across pepticulcer condition
aggregate(dbp ~ diabetes, data = Health_Data, median)
aggregate(dbp ~ diabetes, data = Health_Data, mean)
aggregate(dbp ~ diabetes, data = Health_Data, IQR)
aggregate(dbp ~ diabetes, data = Health_Data, Mode)
aggregate(dbp ~ diabetes, data = Health_Data, sd)

#Outliers of dbp across diabetes groups
boxplot.stats(Health_Data$sbp[Health_Data$diabetes == "Diabetes"])$out
boxplot.stats(Health_Data$dbp[Health_Data$diabetes == "No Diabetes"])$out

# dbp distribution by diabetes groups
boxplot(dbp ~ diabetes, data = Health_Data, 
        main = "Distribution of dbp across diabetes groups")

# Distribution of dbp across pepticulcer groups
ggplot(Health_Data, aes(x = dbp)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 5,
                 fill = "lightgrey", 
                 colour = "black") +
  geom_density(colour = "orange") +
  facet_wrap(~diabetes) + 
  labs(title = "Distribution of dbp across diabetes groups") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 15, face = "bold"))

# Q-Q-Plot: Distribution of dbp across diabetes groups
ggplot(Health_Data, aes(sample = dbp)) +
  stat_qq() +
  stat_qq_line(colour = "red") + 
  facet_wrap(~ diabetes) +
  labs(title = "Distribution of dbp across diabetes groups") +
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
# Normality Assumption - shapiro.test
## =============================================================================

shapiro.test(Health_Data$dbp[Health_Data$diabetes == "Diabetes"])
shapiro.test(Health_Data$dbp[Health_Data$diabetes == "No Diabetes"])

## =============================================================================
# Hypothesis Testing: Mann-Whitney U test 
# H0: The rank sum of diastolic blood pressure (dbp) is the same among diabetic and non-diabetic participants.
# H1: The rank sum of  diastolic blood pressure (dbp) differs among diabetes and non-diabetes. 
## =============================================================================
wilcox.test(dbp~diabetes,data = Health_Data)
t.test(dbp~diabetes,data = Health_Data) #robustness check

# =============================================================================
#Hypothesis testing 3: systolic blood pressure ~ occupation 
## =============================================================================
#Inspecting data type

str(Health_Data$sdp)
str(Health_Data$occupation)
table(Health_Data$occupation)

#Converting occupation into categorical factor variable
Health_Data$occupation <- factor(Health_Data$occupation, levels = c(1, 2, 3, 4), labels = c("GOVT JOB", "PRIVATE JOB", "BUSINESS", "OTHERS"))
str(Health_Data$occupation)

#Data characteristics: sbp across occupations
aggregate(sbp ~ occupation, data = Health_Data, median)
aggregate(sbp ~ occupation, data = Health_Data, mean)
aggregate(sbp ~ occupation, data = Health_Data, IQR)
aggregate(sbp ~ occupation, data = Health_Data, Mode)
aggregate(sbp ~ occupation, data = Health_Data, sd)

#Outliers of sbp across pepticulcer groups
boxplot.stats(Health_Data$sbp[Health_Data$occupation == "GOVT JOB"])$out
boxplot.stats(Health_Data$sbp[Health_Data$occupation == "PRIVATE JOB"])$out
boxplot.stats(Health_Data$sbp[Health_Data$occupation == "BUSINESS"])$out
boxplot.stats(Health_Data$sbp[Health_Data$occupation == "OTHERS"])$out

# sbp distribution by occupation groups
boxplot(sbp ~ occupation, data = Health_Data, 
        main = "Distribution of sbp across occupation groups")

# Distribution of sbp across occupation groups
ggplot(Health_Data, aes(x = sbp)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 5,
                 fill = "lightgrey", 
                 colour = "black") +
  geom_density(colour = "orange") +
  facet_wrap(~occupation) + 
  labs(title = "Distribution of sbp across occupation groups") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 15, face = "bold"))

# Q-Q-Plot: Distribution of sbp across occupational groups
ggplot(Health_Data, aes(sample = sbp)) +
  stat_qq() +
  stat_qq_line(colour = "red") + 
  facet_wrap(~ occupation) +
  labs(title = "Distribution of sbp across occupational groups") +
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
# Normality Assumption - shapiro.test
## =============================================================================

shapiro.test(Health_Data$sbp[Health_Data$occupation == "GOVT JOB"])
shapiro.test(Health_Data$sbp[Health_Data$occupation == "PRIVATE JOB"])
shapiro.test(Health_Data$sbp[Health_Data$occupation == "BUSINESS"])
shapiro.test(Health_Data$sbp[Health_Data$occupation == "OTHERS"])

## =============================================================================
# Hypothesis Testing: Kruskalis_Wallis
# H0: The rank sum of systolic blood pressure (sbp) is the same across the occupational groups.
# H1: The rank sum of systolic blood pressure (sbp) is different across the occupational groups.
## =============================================================================
kruskal.test(sbp ~ occupation, data = Health_Data)

## =============================================================================
# Unit 10: Correlation testing (sbp ~ dbp)
# Is there any association between sbp and dbp
## =============================================================================
library(nortest)
library(vioplot)

# Normality Assumption: Anderson-Darling normality test
ad.test(Health_Data$sbp)
ad.test(Health_Data$dbp)

# Visualisation sbp
hist(Health_Data$sbp, main = "Distribution of sbp")
boxplot(Health_Data$sbp, main = "Distribution of sbp")
vioplot(Health_Data$sbp, main = "Distribution of sbp")

#Histogram + Density curve: sbp
ggplot(Health_Data, aes(x = sbp))+ 
  geom_histogram(aes(y= after_stat(density)),
                 binwidth = 10,
                 fill = "lightgrey",
                 colour = "black", 
                 na.rm = TRUE)+
  geom_density(colour = "orange", linewidth = 0.7, na.rm = TRUE)+
  labs(title = "Distribution of Diastolic blood pressure", 
       x = "Systolic blood pressure", 
       y = "Density")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 20, face = "bold"))

# Q-Q-Plot: Distribution sbp 
ggplot(Health_Data, aes(sample = sbp)) +
  stat_qq() +
  stat_qq_line(colour = "red") +
  labs(title = "Distribution of Systolic blood pressure") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 15, face = "bold"))

# Visualisation dbp
hist(Health_Data$dbp, main = "Distribution of dbp")
boxplot(Health_Data$dbp, main = "Distribution of dbp")
vioplot(Health_Data$dbp, main = "Distribution of dbp")

#Histogram + Density curve: dbp
ggplot(Health_Data, aes(x = dbp))+ 
  geom_histogram(aes(y= after_stat(density)),
                 binwidth = 10,
                 fill = "lightgrey",
                 colour = "black", 
                 na.rm = TRUE)+
  geom_density(colour = "orange", linewidth = 0.7, na.rm = TRUE)+
  labs(title = "Distribution of Diastolic blood pressure", 
       x = "Diastolic blood pressure", 
       y = "Density")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 20, face = "bold"))

# Q-Q-Plot: Distribution dbp 
ggplot(Health_Data, aes(sample = dbp)) +
  stat_qq() +
  stat_qq_line(colour = "red") +
  labs(title = "Distribution of Diastolic blood pressure") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 15, face = "bold"))

# Relationships between sbp and bdp
ggplot(Health_Data, aes(x = sbp, y = dbp)) +
  geom_point(colour = "lightgrey", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, colour = "orange") +
  labs(title = "Relationship between sbp and bdp", 
       x = "sbp",
       y = "dbp") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 15, face = "bold"))

# Correlation test

cor.test(Health_Data$sbp, Health_Data$dbp, method = "pearson") 

## -----------------------------------------------------------------------------
## (Unit 11) Simpler linear regression analysis to find the population regression 
## equation to predict the diastolic BP by systolic BP.
## -----------------------------------------------------------------------------

# Scatter plot diastolic BP vs systolic BP.
ggplot(Health_Data, aes(x = dbp, y = sbp)) +
  geom_point() +
  stat_smooth()

#Correlation coefficient between dbp vs sbp
cor(Health_Data$dbp, Health_Data$sbp)

#Linear model predictng dbp by sbp
model <- lm(dbp ~ sbp, data = Health_Data)
model

summary(model)
confint(model)