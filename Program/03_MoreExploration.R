## --------------------------------------------------------------------------
##
## Script name: Marginal Analysis
##
## Purpose: Explore the marginal association of outcome with different variables
##
## Author: Qinghua Li
##
## Date Created: 12/11/2023
##
## Email: qinghua_li@med.unc.edu
##
## -------------------------------------------------------------------------
##
## Notes:
##   
##
## ------------------------------------------------------------------------



# -------------------------------
# load in libraries
library(tidyverse)
library(ggplot2)
library(readr)
library(ggcorrplot)


# ------------
# Paths & data
# 1. set up paths & data name
setwd("/home/rstudio/work")
getwd()
data_name1 <- "Sleep_Efficiency.csv"
data_name2 <- "Sleep_health_and_lifestyle_dataset.csv"

# 2. Read in data
SE_df <- read_csv(paste0("Data/", data_name1), skip = 1)
SE_df <- SE_df %>% 
  select(-Bedtime, -`Wakeup_time`) %>% 
  mutate(Gender = as.factor(Gender),
         Exercise_frequency = ifelse(Exercise_frequency %in% c(3, 4, 5), 
                                     ">=3", Exercise_frequency),
         Caffeine_consumption = ifelse(Caffeine_consumption %in% c(75, 100, 200), 
                                       ">=75", Caffeine_consumption),
         `Exercise_frequency` = as.factor(`Exercise_frequency`),
         SE_cat = ifelse(`SE_num` >= 0.85, "SE>=0.85", "SE<0.85"),
         Exercise_frequency = factor(Exercise_frequency,
                                     levels = c("0", "1", "2", ">=3")),
         Caffeine_consumption = factor(Caffeine_consumption)
         ) 

slp_life_df <- read_csv(paste0("Data/", data_name2), skip = 1)
slp_life_df <- separate(slp_life_df, col = `Blood_Pressure`, 
                        into = c("SBP", "DBP"), sep = "/") %>% 
  mutate(BMI_Cat = ifelse(BMI_Cat == "Normal Weight", "Normal", BMI_Cat),
         Sleep_duration_cat = ifelse(Sleep_Duration >= 7, ">=7", "<7"),
         Occupation = ifelse(Occupation %in% c("Doctor", "Nurse"), 
                             "Medical Field", "Non-Medical Field"),
         BMI_Cat = as.factor(BMI_Cat),
         Sleep_duration_cat = as.factor(Sleep_duration_cat),
         Occupation = as.factor(Occupation),
         SBP = as.numeric(SBP),
         DBP = as.numeric(DBP))


# -----------------------------
# More explorations and Graphs


# 1. Sleep Efficiency vs. Life habit
## Number of awakenings vs. Sleep Efficiency
plot <- SE_df %>% 
  ggplot(aes(x = SE_cat, y = Awakenings)) + 
  geom_boxplot() + 
  labs(x = "Sleep Efficiency Category", y = "Number of Awakenings during Sleep")

getwd()
ggsave(filename = "Figures/SE_vs_Awakenings.jpeg", plot = plot, 
       width = 4, height = 6, dpi = 400)

## Alcohol consumption vs. Sleep Efficiency
plot <- SE_df %>% 
  ggplot(aes(x = SE_cat, y = Alcohol_consumption)) + 
  geom_boxplot() + 
  labs(x = "Sleep Efficiency Category", y = "Alcohol Consumption")

ggsave(filename = "Figures/SE_vs_Alcohol.jpeg", plot = plot, 
       width = 4, height = 6, dpi = 400)


## Exercise frequency vs. Sleep Efficiency
plot <- SE_df %>% 
  filter(!is.na(Exercise_frequency)) %>% 
  ggplot(aes(x = Exercise_frequency, y = SE_num)) + 
  geom_boxplot() + 
  labs(x = "Weekly Exercise Frequency", y = "Sleep Efficiency")
 
ggsave(filename = "Figures/SE_vs_Exercisse.jpeg", plot = plot, 
       width = 4, height = 6, dpi = 400)


## Correlation exploration among numerical variables
cor_df <- SE_df %>% 
  select_if(., is.numeric) %>% 
  select(-ID) %>% 
  as.matrix() %>% 
  cor() 

plot <- ggcorrplot(cor_df, type = "lower", lab = TRUE)
ggsave(filename = "Figures/SE_Correlation.jpeg", plot = plot, 
       width = 6, height = 6, dpi = 400)
  
  

# 2. Total sleep time vs. Occupation, sleep disorder, etc.
## Sleep disorder vs. Total sleep time
plot <- slp_life_df %>% 
  ggplot(aes(x = Sleep_Disorder, y = Sleep_Duration)) + 
  geom_boxplot() + 
  labs(x = "Sleep Disorder", y = "Total Sleep Time")

ggsave(filename = "Figures/TST_vs_SleepDisorder.jpeg", plot = plot, 
       width = 4, height = 6, dpi = 400)


plot <- slp_life_df %>% 
  ggplot(aes(x = BMI_Cat, y = Sleep_Duration)) + 
  geom_boxplot() + 
  labs(x = "BMI Category", y = "Total Sleep Time")

ggsave(filename = "Figures/TST_vs_BMI.jpeg", plot = plot, 
       width = 4, height = 6, dpi = 400)

cor_df <- slp_life_df %>% 
  select_if(., is.numeric) %>% 
  select(-ID) %>% 
  as.matrix() %>% 
  cor() 

plot <- ggcorrplot(cor_df, type = "lower", lab = TRUE)
ggsave(filename = "Figures/TST_Correlation.jpeg", plot = plot, 
       width = 6, height = 6, dpi = 400)