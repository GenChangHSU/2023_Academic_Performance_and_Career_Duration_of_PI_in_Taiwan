## -----------------------------------------------------------------------------
## Title: Analysis of the academic performance and career duration of Taiwanese 
##        EEB PIs
##
## Author: Gen-Chang Hsu
##
## Date: 2022-02-06
##
## Description:
## 1. Summarize the PhD university countries, PhD university rankings, and gender 
##    composition of the faculty members in the study 
##
## Notes:
## 1. This is only part of the entire data analyses of this project. Full code is 
##    available at the corresponding author Dr. Syuan-Jyun Sun (sysun@umich.edu)
##
## -----------------------------------------------------------------------------
set.seed(123)


# Libraries --------------------------------------------------------------------
library(tidyverse)
library(magrittr)


# Import files -----------------------------------------------------------------
Performance_duration_df <- read.csv("./Data_raw/publishperishdata.csv", header = T)


############################### Code starts here ###############################

# 1. Data summary --------------------------------------------------------------
### (1) Number of faculty members
Performance_duration_df %>% 
  filter(stage == "assistant" & beforeafter == "before") %$% 
  unique(Name) %>%
  length()

### (2) Gender composition
Performance_duration_df %>% 
  filter(stage == "assistant" & beforeafter == "before") %>%
  count(sex)

### (3) Academic rank composition
Performance_duration_df %>% 
  filter(stage == "assistant" & beforeafter == "before") %>%
  count(Position) %>%
  mutate(Prop = n/sum(n))

### (4) Number of countries where the PhD universities are
Performance_duration_df %>% 
  filter(stage == "assistant" & beforeafter == "before") %$%
  unique(PhD.country) %>%
  length()

### (5) Country composition of the PhD universities
Performance_duration_df %>% 
  filter(stage == "assistant" & beforeafter == "before") %>%
  count(PhD.country, sort = T) %>%
  mutate(Prop = n/sum(n))

Performance_duration_df %>% 
  filter(stage == "assistant" & beforeafter == "before") %>%
  count(PhD.country, sort = T) %>%
  mutate(Prop = n/sum(n)) %>%
  filter(!PhD.country %in% c("USA", "Taiwan", "UK")) %$%
  sum(Prop)

### (6) Median of the PhD university ranking for Taiwanese and foreign degrees
Performance_duration_df %>% 
  filter(stage == "assistant" & beforeafter == "before") %>%
  group_by(PhD.taiwan) %>%
  summarise(Median = median(PhD.uni.rank, na.rm = T))




