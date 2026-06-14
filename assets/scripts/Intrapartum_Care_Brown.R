#Loading libraries
library(tidyr)
library(tidyverse)

#Loading data
data(X1_competency_care)
data(X2_no_care_reasons)
data(X3_care_requirements)
data(X3_1_care_requirements)

##==============================================================================
## Plot1: Competency and Readiness in Intrapartum Care Provision
##==============================================================================

#Transform data for ggplot visualisation
competency_care <- pivot_longer(X1_competency_care,
cols = c(agree, disagree, no_opinion),
names_to = "Response",
values_to = "Percentage")

View(competency_care)

#Visualise barplot
competency_care%>%
ggplot(aes(x = statement, y = Percentage, fill = Response))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label = Percentage), position = position_dodge(width = 0.9), vjust = 3, colour = "white")+
  ylim(0, 70)+
  
  scale_fill_manual(
    values = c("agree" = "steelblue","disagree" = "orange", "no_opinion" = "grey"))+
  
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.title = element_text(face = "bold"))+

labs(title = "Plot 1: PGs Competency and Readiness in Intrapartum Care Provision",
  x = "", 
  y = "Percentage",
  fill = "")

##==============================================================================
## Plot2: Barriers to Intrapartum Care Provision
##==============================================================================

#Transform data for ggplot visualisation
no_care_reason <- pivot_longer(X2_no_care_reasons,
cols = c(agree, disagree, no_opinion),
names_to = "Response",
values_to = "Percentage")

View(no_care_reason)

#Order reasons in descending "agree" order
order_reason_agree <- no_care_reason %>%
  filter(Response == "agree") %>%
  arrange(desc(Percentage)) %>%
  pull(reason)

#Used factor to display descending "agree" order and "agree" as a first category
no_care_reason %>%
  mutate(reason = factor(reason, levels = order_reason_agree),
         Response = factor(Response, levels = c("agree", "disagree", "no_opinion"))) %>%

#Visualise barplot (Option 1)
  ggplot(aes(x = reason, y = Percentage, fill = Response)) +
  geom_bar(stat = "identity", position = "dodge")+

  scale_fill_manual(
    values = c("agree" = "steelblue","disagree" = "orange", "no_opinion" = "grey"))+

  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12, angle = 30, hjust = 1),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.title = element_text(face = "bold"))+
  
  labs(title = "Plot 2: Barriers to Intrapartum Care Provision",
       x = "", 
       y = "Percentage",
       fill = "")

####################################################################################
#Visualise stacked barplot (Improved Option 2)
####################################################################################

#Order reasons in descending "agree" order
order_reason_agree <- no_care_reason %>%
  filter(Response == "agree") %>%
  arrange(Percentage) %>%
  pull(reason)


#Used factor to display descending "agree" order and "agree" as a first category
no_care_reason %>%
  mutate(reason = factor(reason, levels = order_reason_agree),
         Response = factor(Response, levels = c("no_opinion", "disagree", "agree"))) %>%

ggplot(aes(x = reason, y = Percentage, fill = Response)) +
  geom_bar(stat = "identity", position = "stack")+
  geom_text(aes(label = Percentage), position = position_stack(vjust = 0.5), colour = "white")+
  coord_flip()+
  
  
  scale_fill_manual(
    values = c("agree" = "steelblue","disagree" = "orange", "no_opinion" = "grey"))+
  
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12, angle = 30, hjust = 1),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.title = element_text(face = "bold"))+
  
  labs(title = "Plot 2: Barriers to Intrapartum Care Provision",
       x = "", 
       y = "Percentage",
       fill = "")


##==============================================================================
## Plot3: Regulations as a Most Pronounced Aspect Impacting the Provision of Intrapartum Care - REGULATIONS ONLY
##==============================================================================  

#Transform data for ggplot visualisation
care_requirements <- pivot_longer(X3_care_requirements,
cols = c(agree, disagree, no_opinion),
names_to = "Response",
values_to = "Percentage")

regulations <- care_requirements %>%
  filter(category == "Regulations")
View(regulations)

#Order requirements in ascending "agree" order
regulations_agree <- regulations %>%
  filter(Response == "agree") %>%
  arrange(Percentage) %>%
  pull(requirements)

#Used factor to display "agree" in ascending order and as last category
regulations%>%
  mutate(requirements = factor(requirements, levels = regulations_agree),
         Response = factor(Response, levels = c("no_opinion", "disagree", "agree"))) %>%
  
#Visualise a horizontal stacked barplot
  ggplot(aes(x = requirements, y = Percentage, fill = Response))+
  geom_bar(stat = "identity", position = "stack")+
  geom_text(aes(label = Percentage), position = position_stack(vjust = 0.5), colour = "white")+
  coord_flip()+
  
  scale_fill_manual(values = c("agree" = "steelblue","disagree" = "orange", "no_opinion" = "grey"))+
  
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.title = element_text(face = "bold"))+
    
  labs(title = "Plot 3: Policy and Regulations as a Most Pronounced Aspects Defining the Provision of Intrapartum Care",
         x = "", 
         y = "Percentage",
         fill = "")

##==============================================================================
## Plot 3.1: Aspects Defining the Provision of Intrapartum Care (by category)
##==============================================================================  

#Transform data for ggplot visualisation
care_requirements_V2 <- pivot_longer(X3_1_care_requirements,
                                  cols = c(agree, disagree, no_opinion),
                                  names_to = "Response",
                                  values_to = "Percentage")

View(care_requirements_V2)

#Order requirements in ascending "agree" order
care_requirements_agree <- care_requirements_V2 %>%
  filter(Response == "agree") %>%
  arrange(Percentage) %>%
  pull(requirements)

#Used factor to display "agree" in ascending order and as last category
care_requirements_V2%>%
  mutate(requirements = factor(requirements, levels = care_requirements_agree),
         Response = factor(Response, levels = c("no_opinion", "disagree", "agree"))) %>%

#Visualise a horizontal stacked barplot
  ggplot(aes(x = requirements, y = Percentage, fill = Response))+
  geom_bar(stat = "identity", position = "stack")+
  coord_flip()+
  
  scale_fill_manual(values = c("agree" = "steelblue","disagree" = "orange", "no_opinion" = "grey"))+
  
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(face = "bold")) +
  
  labs(title = "Plot 3: Aspects Defining the Provision of Intrapartum Care",
       x = "", y = "Percentage", fill = "")