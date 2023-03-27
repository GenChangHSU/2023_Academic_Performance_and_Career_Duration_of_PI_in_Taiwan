## -----------------------------------------------------------------------------
## Title: Analysis of the academic performance and career duration of Taiwanese 
##        EEB PIs
##
## Author: Gen-Chang Hsu
##
## Date: 2023-03-27
##
## Description:
## 1. Summary of the variables in the data set
##
## -----------------------------------------------------------------------------
set.seed(123)


# Libraries --------------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(readxl)


# Import files -----------------------------------------------------------------
PI_df <- read_xlsx("./Data_raw/PI_data.xlsx", sheet = 1)
PI_df <- PI_df %>% 
  mutate(PhD.uni.rank = as.numeric(PhD.uni.rank),
         h_index = as.numeric(h_index),
         Assistant.since = as.numeric(Assistant.since),
         full.professor = as.numeric(full.professor),
         time.to.assistant = as.numeric(time.to.assistant),
         time.to.full = as.numeric(time.to.full))
other_PI_df <- read_xlsx("./Data_raw/other_PI_data.xlsx", sheet = 1)

############################### Code starts here ###############################

# 1. Data summary --------------------------------------------------------------
### (1) Number of faculty members
PI_df %$% 
  unique(search.id) %>%
  length()

### (2) Gender composition
PI_df %>% 
  filter(stage == "assistant" & beforeafter == "before") %>%
  count(sex) %>% 
  mutate(prop = n/sum(n))

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

### (7) Number of PIs from each university/institute
PI_df %>% 
  filter(stage == "assistant" & beforeafter == "before") %>%
  group_by(University) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))


# 2. Comparisons between the studied PIs and the entire PI population ----------
### (1) Full data set of all EEB PIs in Taiwan
all_PI_df <- PI_df %>% 
  filter(stage == "assistant" & beforeafter == "before") %>%
  select(University, Position, sex) %>% 
  mutate(university_type = "public",
         data_status = "included") %>% 
  bind_rows(other_PI_df %>% 
              select(University, Position, sex, university_type) %>% 
              mutate(data_status = "not-included"))

### (2) Number of public universities at which all the EEB PIs are
all_PI_df %>% 
  filter(university_type == "public") %$%
  unique(University)

### (3) Number of all EEB PIs from public universities
all_PI_df %>% 
  filter(university_type == "public") %>% 
  nrow()

### (4) Gender composition of all EEB PIs from public universities
all_PI_df %>% 
  filter(university_type == "public") %>%
  count(sex) %>% 
  mutate(prop = n/sum(n))

n_observed <- PI_df %>% 
  filter(stage == "assistant" & beforeafter == "before") %>%
  count(sex) %>% 
  pull(n)

p_expected <- all_PI_df %>% 
  filter(university_type == "public") %>% 
  count(sex) %>% 
  mutate(prop = n/sum(n)) %>% 
  pull(prop)

chisq.test(x = n_observed, p = p_expected)

### (5) Academic rank composition of PIs from public universities
all_PI_df %>% 
  filter(university_type == "public") %>% 
  count(Position) %>% 
  mutate(prop = n/sum(n))

n_observed <- PI_df %>% 
  filter(stage == "assistant" & beforeafter == "before") %>%
  count(Position) %>% 
  pull(n)

p_expected <- all_PI_df %>% 
  filter(university_type == "public") %>% 
  count(Position) %>% 
  mutate(prop = n/sum(n)) %>% 
  pull(prop)

chisq.test(x = n_observed, p = p_expected)





