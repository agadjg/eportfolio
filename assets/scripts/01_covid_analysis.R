# ==================================================
# COVID ANALYSIS: Activity 1 (Unit 1 /Seminar 1)
# ==================================================

# 1. Clear workspace
rm(list = ls())
# --------------------------------------------------

# 2. Load packages
# --------------------------------------------------
library(psych)
library(vioplot)
library(skimr)
# --------------------------------------------------

# 3. Import CSV
covid <- read.csv("data/covid_india.csv")
# --------------------------------------------------

# 4. Inspect data structure
View(covid)
str(covid)
summary(covid)
names(covid)
# --------------------------------------------------

# 5. Remove 'Sno' ID
head(covid$Sno)
covid$Sno <- NULL
ncol(covid)
# --------------------------------------------------

# 6. Data format and variable type check
sapply(covid, class)
sum(sapply(covid, is.numeric)) 
sum(sapply(covid, is.character))
# --------------------------------------------------

# 7. Convert date field from character to date format
covid$Date <- as.Date(covid$Date,format = "%d-%m-%Y")
class(covid$Date)
head(covid$Date)

# 8. Date range covered 
range(covid$Date)
min(covid$Date)
max(covid$Date)
# --------------------------------------------------

# 9. Number of unique states/union territories
length(unique(covid$State.UnionTerritory))

# 10. Most frequent state
state_freq <- table(covid$State.UnionTerritory)
sort(state_freq, decreasing = TRUE)[1]
# --------------------------------------------------

# 10. Data format final version
str(covid)
summary(covid)
skim(covid)
# --------------------------------------------------

# 11. Save clean dataset
write.csv(covid, "data/covid_india_clean.csv", row.names = FALSE)

# ==================================================
# COVID ANALYSIS: Activity 2 (Unit 2)
# ==================================================

# --------------------------------------------------
# 1. Binary variable (has_recovery) & Frequency table 
has_recovery <-covid$Cured > 0
table(has_recovery)
# --------------------------------------------------

# 2. Recovery status
recovery_status <- ifelse(covid$Cured > 0, "Recovered", "No Recovered")
table(recovery_status)
# --------------------------------------------------

# 3. Percentages of recovery cases
frequency1 <- table(recovery_status)
frequency1/sum(frequency1)*100
round(frequency1/sum(frequency1)*100,1)
# --------------------------------------------------

# 4. Visualisation
barplot(table(recovery_status))
pie(table(recovery_status))

# ==================================================
# COVID ANALYSIS: Activity 2 (Unit 2/Seminar 2)
# ==================================================

# 1. Central tendency of ConfirmedIndianNational
# --------------------------------------------------
mean(covid$ConfirmedIndianNational)
median(covid$ConfirmedIndianNational)
names(sort(table(covid$ConfirmedIndianNational), decreasing = TRUE))[1]
# --------------------------------------------------

# 9. Dispersion of ConfirmedIndianNational
# --------------------------------------------------
sd(covid$ConfirmedIndianNational)
var(covid$ConfirmedIndianNational)
range(covid$ConfirmedIndianNational)
# --------------------------------------------------

# 10. Visualisations of 4 variables
# --------------------------------------------------
hist(covid$ConfirmedIndianNational)
hist(covid$ConfirmedIndianNational, probability = TRUE) 
lines(density(covid$ConfirmedIndianNational),  col = "blue")
boxplot(covid$ConfirmedIndianNational)
vioplot(covid$ConfirmedIndianNational)
# --------------------------------------------------

hist(covid$ConfirmedForeignNational)
hist(covid$ConfirmedForeignNational, probability = TRUE) 
lines(density(covid$ConfirmedForeignNational),  col = "orange")
boxplot(covid$ConfirmedForeignNational)
vioplot(covid$ConfirmedForeignNational)

# --------------------------------------------------
hist(covid$Deaths)
hist(covid$Deaths, probability = TRUE) 
lines(density(covid$Deaths),  col = "red")
boxplot(covid$Deaths)
vioplot(covid$Deaths)

# --------------------------------------------------
hist(covid$Cured)
hist(covid$Cured, probability = TRUE) 
lines(density(covid$Cured),  col = "green")
boxplot(covid$Cured)
vioplot(covid$Cured)

# ==================================================
# COVID ANALYSIS: Activity 3 (Unit 3)
# ==================================================

# 1. Binary variable (has_deaths) & Frequency table 
has_deaths <-covid$Deaths > 0
table(has_deaths)
# --------------------------------------------------

# 2. Factor variable with labels ("No Deaths", "Deaths Reported").
death_factor <- factor(has_deaths, levels = c(FALSE, TRUE), labels = c("No Deaths", "Deaths Reported"))
table(death_factor)
# --------------------------------------------------

# 3. Percentage of deaths cases
frequency2 <- table(death_factor)
frequency2/sum(frequency2)*100
round(frequency2/sum(frequency2)*100,1)
# --------------------------------------------------

# 4. Visualisation
barplot(table(death_factor))
pie(table(death_factor))
# --------------------------------------------------

# 5. Categorical Variable for confirmed cases (Indian + Foreign nationals)
total_cases <- covid$ConfirmedIndianNational + covid$ConfirmedForeignNational
case_level <- ifelse(total_cases == 0, "No Cases", 
                     ifelse(total_cases <= 5, "Low Cases", 
                            ifelse(total_cases <= 15, "Medium Cases", "High Cases")))
# --------------------------------------------------

# 6. Frequency table for the most frequent reporting 
table(case_level)
case_level <- factor(case_level, levels = c("No Cases", "Low Cases", "Medium Cases", "High Cases"))
table(case_level)
# --------------------------------------------------

# 7. Frequency table for State/Union Territory for the most frequent reporting
table(covid$State.UnionTerritory)
table(covid$State.UnionTerritory, case_level)
addmargins(table(covid$State.UnionTerritory, case_level))

# --------------------------------------------------
# 8. Top 10 states with the highest number of daily reports in the dataset
head(sort(table(covid$State.UnionTerritory), decreasing = TRUE), 10)

state_case_table <- table(covid$State.UnionTerritory, case_level)
top_10_frequency_table <- state_case_table[order(rowSums(state_case_table), decreasing = TRUE)[1:10],, drop = FALSE]
addmargins(top_10_frequency_table)
# --------------------------------------------------


