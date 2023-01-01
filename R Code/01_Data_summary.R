## -----------------------------------------------------------------------------
## Title: Analysis of the academic performance and career duration of Taiwanese 
##        EEB PIs
##
## Author: Gen-Chang Hsu
##
## Date: 2023-01-01
##
## Description:
## 1. Summary of the variables in the data set
##
## -----------------------------------------------------------------------------
set.seed(123)


# Libraries --------------------------------------------------------------------
library(tidyverse)
library(magrittr)


# Import files -----------------------------------------------------------------
PI_df <- read.csv("./Data_raw/publishperishdata.csv", header = T)


############################### Code starts here ###############################

# 1. Data summary --------------------------------------------------------------
### (1) Number of faculty members
PI_df %$% 
  unique(Name) %>%
  length()

### (2) Gender composition
PI_df %>% 
  filter(stage == "assistant" & beforeafter == "before") %>%
  count(sex)

### (3) Academic rank composition
PI_df %>% 
  filter(stage == "assistant" & beforeafter == "before") %>%
  count(Position) %>%
  mutate(Prop = n/sum(n))

### (4) Number of countries where the PhD universities are
PI_df %>% 
  filter(stage == "assistant" & beforeafter == "before") %$%
  unique(PhD.country) %>%
  length()

### (5) Country composition of the PhD universities
PI_df %>% 
  filter(stage == "assistant" & beforeafter == "before") %>%
  count(PhD.country, sort = T) %>%
  mutate(Prop = n/sum(n))

PI_df %>% 
  filter(stage == "assistant" & beforeafter == "before") %>%
  count(PhD.country, sort = T) %>%
  mutate(Prop = n/sum(n)) %>%
  filter(!PhD.country %in% c("USA", "Taiwan", "UK")) %$%
  sum(Prop)

### (6) Median of the PhD university ranking for Taiwanese and foreign degrees
PI_df %>% 
  filter(stage == "assistant" & beforeafter == "before") %>%
  group_by(PhD.taiwan) %>%
  summarise(Median = median(PhD.uni.rank, na.rm = T))




