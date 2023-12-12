## --------------------------------------------------------------------------
##
## Script name: Data Exploration
##
## Purpose: Explore the data for BIOS 611 Project
##
## Author: Qinghua Li
##
## Date Created: 10/22/2023
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


# ------------
# Paths & data
# 1. set up paths & data name
setwd("/home/rstudio/work")
getwd()
data_name1 <- "Sleep_Efficiency.csv"
data_name2 <- "Sleep_health_and_lifestyle_dataset.csv"
  
# 2. Read in data
SE_df <- read_csv(paste0("Data/", data_name1))
slp_life_df <- read_csv(paste0("Data/", data_name2))
slp_life_df <- separate(slp_life_df, col = `Blood Pressure`, 
                        into = c("SBP", "DBP"), sep = "/") %>% 
  rename(BMI_cat = `BMI Category`) %>% 
  mutate(BMI_cat = ifelse(BMI_cat == "Normal Weight", "Normal", BMI_cat))



# ------------------------
# Data exploration
# 1. Check missing data
SE_df %>% 
  select(-Bedtime, -`Wakeup time`) %>% 
  mutate(Gender = as.factor(Gender)) %>% 
  summary()


slp_life_df %>% 
  mutate(Gender = as.factor(Gender),
         Occupation = as.factor(Occupation),
         BMI_cat = as.factor(BMI_cat),
         `Sleep Disorder` = as.factor(`Sleep Disorder`),
         SBP = as.numeric(SBP),
         DBP = as.numeric(DBP)) %>% 
  summary()


# -----------
# Graphs

# 1. Sleep efficiency by smoking status using sleep efficiency data
SE_smoke <- SE_df %>% 
  filter(!is.na(`Smoking status`)) %>% 
  ggplot(aes(x = `Sleep efficiency`, fill = `Smoking status`)) + 
  geom_histogram(binwidth = 0.1, position = 'dodge') +
  scale_x_continuous(breaks = seq(0.5, 1, 0.1)) + 
  labs(x = "Sleep Efficiency (%)", y = "Count") +
  theme_minimal()

ggsave(SE_smoke, filename = "Figures/SE_smoking.jpeg", 
       dpi = 400, width = 5, height = 3.5)

# 2. Histogram of Sleep duration by BMI category using sleep and life style data
slp_BMI <- slp_life_df %>% 
  ggplot(aes(x = `Sleep Duration`, fill = BMI_cat)) + 
  geom_histogram(binwidth = 1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 10, 0.5))


ggsave(slp_BMI, filename = "Figures/SleepDuration_BMI.jpeg", 
       dpi = 400, width = 5, height = 3.5)



