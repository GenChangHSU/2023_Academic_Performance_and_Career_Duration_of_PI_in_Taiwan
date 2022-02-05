## -----------------------------------------------------------------------------
## Title: Analysis of the academic performance and career duration of Taiwanese 
##        EEB PIs
##
## Author: Gen-Chang Hsu
##
## Date: 2022-02-03
##
## Description:
## 1. Fit GLMMs to examine the relationship of academic performance before 
##    recruitment/promotion vs. year of recruitment/promotion, PhD university origin, 
##    PhD university ranking, and gender
## 2. Fit GLMMs to examine the relationship of duration for recruitment/promotion vs. 
##    year of recruitment/promotion, PhD university origin, PhD university ranking,
##    gender, and academic performance
## 3. Fit GLMMs to examine the relationship of difference in academic performance 
##    before and after recruitment/promotion vs. year of recruitment/promotion, 
##    PhD university origin, PhD university ranking, and gender
##
## Notes:
## 1. This is only part of the entire data analyses of this project. Full code is 
##    available at the corresponding author Dr. Syuan-Jyun Sun (sysun@umich.edu)
##
## -----------------------------------------------------------------------------
set.seed(123)


# Libraries --------------------------------------------------------------------
library(tidyverse)
library(lme4)
library(car)


# Import files -----------------------------------------------------------------
Performance_duration_df <- read.csv("./Data_raw/publishperishdata.csv", header = T)
Performance_diff_df <- read.csv("./Data_raw/publishperish_h_index_diff.csv", header = T)


############################### Code starts here ###############################

# 1. Academic performance ------------------------------------------------------
### (1) Recruitment 
Performance_recruit_model <- lmer(log(h_index+1) ~ Assistant.since + PhD.taiwan + 
                                  PhD.uni.rank + sex + (1|University/Department), 
                                  data = filter(Performance_duration_df, 
                                                stage == "assistant" & 
                                                beforeafter == "after"))

# Model results
summary(Performance_recruit_model)
Anova(Performance_recruit_model)

# Model validation
ggplot(data = data.frame(x = fitted(Performance_recruit_model),
                         y = resid(Performance_recruit_model))) + 
  geom_point(aes(x, y)) + 
  geom_smooth(aes(x, y))

### (2) Promotion
Performance_promotion_model <- lmer(log(h_index+1) ~ full.professor + PhD.taiwan + 
                                    PhD.uni.rank + sex + (1|University/Department), 
                                    data = filter(Performance_duration_df, 
                                                  stage == "full" & 
                                                  beforeafter == "before"))

# Model results
summary(Performance_promotion_model)
Anova(Performance_promotion_model)

# Model validation
ggplot(data = data.frame(x = fitted(Performance_promotion_model),
                         y = resid(Performance_promotion_model))) + 
  geom_point(aes(x, y)) + 
  geom_smooth(aes(x, y))


# 2. Career duration -----------------------------------------------------------
### (1) Recruitment 
Duration_recruit_model <- lmer(log(time.to.assistant+1) ~ h_index*Assistant.since + 
                               PhD.taiwan + PhD.uni.rank + sex + (1|University/Department),
                               data = filter(Performance_duration_df, 
                                             stage == "assistant" & 
                                             beforeafter == "before"))

# Model results
summary(Duration_recruit_model)
Anova(Duration_recruit_model, type = 3)  # type 3 anova for significant interaction term

# Model validation
ggplot(data = data.frame(x = fitted(Duration_recruit_model),
                         y = resid(Duration_recruit_model))) + 
  geom_point(aes(x, y)) + 
  geom_smooth(aes(x, y))

### (2) Promotion 
Duration_promotion_model <- lmer(log(time.to.full+1) ~ h_index + full.professor + 
                                 PhD.taiwan + PhD.uni.rank + sex + (1|University/Department),
                                 data = filter(Performance_duration_df, 
                                               stage == "full" & 
                                               beforeafter == "before"))

# Model results
summary(Duration_promotion_model)
Anova(Duration_promotion_model)

# Model validation
ggplot(data = data.frame(x = fitted(Duration_promotion_model),
                         y = resid(Duration_promotion_model))) + 
  geom_point(aes(x, y)) + 
  geom_smooth(aes(x, y))


# 3. Performance difference ----------------------------------------------------
### (1) Recruitment
Diff_recruit_model <- lmer(h_index_diff ~ Assistant.since + PhD.taiwan + 
                           PhD.uni.rank + sex + (1|University/Department),
                           data = filter(Performance_diff_df, stage == "assistant"))

# Model results
summary(Diff_recruit_model)
Anova(Diff_recruit_model)

# Model validation
ggplot(data = data.frame(x = fitted(Diff_recruit_model),
                         y = resid(Diff_recruit_model))) + 
  geom_point(aes(x, y)) + 
  geom_smooth(aes(x, y))

### (2) Promotion
Diff_promotion_model <- lmer(h_index_diff ~ full.professor + PhD.taiwan + 
                             PhD.uni.rank + sex + (1|University/Department),
                             data = filter(Performance_diff_df, stage == "full"))

# Model results
summary(Diff_promotion_model)
Anova(Diff_promotion_model)

# Model validation
ggplot(data = data.frame(x = fitted(Diff_promotion_model),
                         y = resid(Diff_promotion_model))) + 
  geom_point(aes(x, y)) + 
  geom_smooth(aes(x, y))


