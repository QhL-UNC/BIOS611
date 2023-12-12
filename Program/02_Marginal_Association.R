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
library(table1)


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
         Sleep_duration_cat = ifelse(`SE_num` >= 0.85, "SE>=0.85", "SE<0.85"),
         Exercise_frequency = factor(Exercise_frequency,
                                     levels = c("0", "1", "2", ">=3")),
         Caffeine_consumption = factor(Caffeine_consumption))
  
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


# ---------------------
# Marginal Association

# 1. Using the sleep efficiency and life habits data
var_list <- SE_df %>% 
  select(-ID, -`Sleep_duration`, -SE_num, -Sleep_duration_cat) %>% 
  colnames()

var_list <- paste0(var_list, collapse = "+")
var_formula <- as.formula(paste0("~", var_list, "|Sleep_duration_cat"))

SE_df$Sleep_duration_cat <- as.factor(SE_df$Sleep_duration_cat) %>% 
  forcats::fct_expand(., "P-value")

rndr <- function(x, name, ...) {
  if (length(x) == 0) {
    y <- SE_df[[name]]
    s <- rep("", length(render.default(x=y, name=name, ...)))
    if (is.numeric(y)) {
      p <- t.test(y ~ SE_df$Sleep_duration_cat)$p.value
    } else {
      p <- chisq.test(table(y, droplevels(SE_df$Sleep_duration_cat)))$p.value
    }
    s[2] <- sub("<", "&lt;", format.pval(p, digits=3, eps=0.001))
    s
  } else {
    render.default(x=x, name=name, ...)
  }
}

rndr.strat <- function(label, n, ...) {
  ifelse(n == 0, label, render.strat.default(label, n, ...))
}

table1(var_formula, data = SE_df, droplevels = F, render = rndr,
       render.strat = rndr.strat, overall = F)

# 2. Using the sleep duration vs. Occupation, sleep disorder data
var_list <- slp_life_df %>% 
  select(-ID, -`Sleep_Duration`, -Sleep_duration_cat) %>% 
  colnames()

var_list <- paste0(var_list, collapse = "+")
var_formula <- as.formula(paste0("~", var_list, "|Sleep_duration_cat"))

slp_life_df$Sleep_duration_cat <- as.factor(slp_life_df$Sleep_duration_cat) %>%
  forcats::fct_expand(., "P-value")

rndr <- function(x, name, ...) {
  if (length(x) == 0) {
    y <- slp_life_df[[name]]
    s <- rep("", length(render.default(x=y, name=name, ...)))
    if (is.numeric(y)) {
      p <- t.test(y ~ slp_life_df$Sleep_duration_cat)$p.value
    } else {
      p <- chisq.test(table(y, droplevels(slp_life_df$Sleep_duration_cat)))$p.value
    }
    s[2] <- sub("<", "&lt;", format.pval(p, digits=3, eps=0.001))
    s
  } else {
    render.default(x=x, name=name, ...)
  }
}

rndr.strat <- function(label, n, ...) {
  ifelse(n == 0, label, render.strat.default(label, n, ...))
}

table1(var_formula, data = slp_life_df, droplevels = F, render = rndr,
       render.strat = rndr.strat, overall = F)

