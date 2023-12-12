## --------------------------------------------------------------------------
##
## Script name: Linear Model
##
## Purpose: Build linear model for BIOS 611 Project
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
         Caffeine_consumption = factor(Caffeine_consumption, 
                                       levels = c("0", "25", "50", ">=75"))) 

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
         DBP = as.numeric(DBP),
         Sleep_Disorder = factor(Sleep_Disorder, 
                                 levels = c("None", "Sleep Apnea", "Insomnia")))


plot <- SE_df %>% 
  filter(!is.na(Caffeine_consumption)) %>% 
  ggplot(aes(y = Age, x = factor(Caffeine_consumption))) + 
  geom_boxplot() + 
  labs(x = "Caffeine Consumption (mg)", y = "Age (y)")
ggsave(filename = "Figures/Age_vs_Caffeine for SE.jpeg", 
       width = 4, height = 6, dpi = 400)

# ---------------
# Linear model 

# 1. Use sleep efficiency vs. life habit data
lm_se <- lm(SE_num ~ Gender + Age + REM_pct + Deep_sleep_pct + Awakenings + 
              Smoking_status + Exercise_frequency + Caffeine_consumption + 
              Alcohol_consumption, data = SE_df)
summary(lm_se)

jpeg("Figures/Residual_scatter_plot_SE.jpeg", 
     width = 700, height = 800, quality = 100)
plot(lm_se, which = 1)
dev.off()


jpeg("Figures/Residual_QQ_SE.jpeg", 
     width = 700, height = 800, quality = 100)
plot(lm_se, which = 2)
dev.off()

# 2. Use TST and occupation, sleep disorder, etc, data
lm_tst <- lm(Sleep_Duration ~ Gender + Age + Occupation + 
               Physical_Activity_Level + Stress_Level + BMI_Cat + 
               SBP + HR + Daily_Steps + Sleep_Disorder, 
             data = slp_life_df)
summary(lm_tst)

jpeg("Figures/Residual_scatter_plot_TST.jpeg", 
     width = 700, height = 800, quality = 100)
plot(lm_tst, which = 1)
dev.off()


jpeg("Figures/Residual_QQ_TST.jpeg", 
     width = 700, height = 800, quality = 100)
plot(lm_tst, which = 2)
dev.off()


plot <- slp_life_df %>% 
  ggplot(aes(x = Sleep_Disorder, y = SBP)) + 
  geom_boxplot() + 
  labs(x = "Sleep Disorder", y = "Systolic Blood Pressure")
ggsave(filename = "Figures/SleepDisorder_vs_SBP_TST.jpeg",
       width = 4, height = 6, dpi = 400)


plot <- slp_life_df %>% 
  ggplot(aes(x = Sleep_Disorder, y = Stress_Level)) + 
  geom_boxplot() + 
  labs(x = "Sleep Disorder", y = "Stress Level")
ggsave(filename = "Figures/SleepDisorder_vs_Stress_TST.jpeg",
       width = 4, height = 6, dpi = 400)
