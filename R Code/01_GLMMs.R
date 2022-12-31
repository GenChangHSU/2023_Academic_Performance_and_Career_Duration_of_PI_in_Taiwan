## -----------------------------------------------------------------------------
## Title: Analysis of the academic performance and career duration of Taiwanese 
##        EEB PIs
##
## Author: Gen-Chang Hsu
##
## Date: 2022-12-31
##
## Description:
## 1. 
##
##
## -----------------------------------------------------------------------------
set.seed(123)


# Libraries --------------------------------------------------------------------
library(tidyverse)
library(lme4)
library(emmeans)
library(car)
library(performance)


# Import files -----------------------------------------------------------------
PI_df <- read.csv("./Data_raw/publishperishdata.csv", header = T)


############################### Code starts here ###############################

# 1. Academic performance ------------------------------------------------------
### (1) Recruitment 
Performance_recruitment_model_poisson <- glmer(h_index ~ Assistant.since + PhD.taiwan + PhD.uni.rank + sex + (1|University/Department), 
                                               data = filter(PI_df, stage == "assistant" & beforeafter == "before"),
                                               family = "poisson")

Performance_recruitment_model_gaussian <- lmer(h_index ~ Assistant.since + PhD.taiwan + PhD.uni.rank + sex + (1|University/Department), 
                                               data = filter(PI_df, stage == "assistant" & beforeafter == "before"))

# Poisson error distribution fits better
AIC(Performance_recruitment_model_poisson, Performance_recruitment_model_gaussian)

# Model results
summary(Performance_recruitment_model_poisson)
Anova(Performance_recruitment_model_poisson)
confint(Performance_recruitment_model_poisson, method = "boot")

# Model validation
check_model(Performance_recruitment_model_gaussian, check = "normality")
check_model(Performance_recruitment_model_gaussian, check = "qq")
check_model(Performance_recruitment_model_gaussian, check = "homogeneity")
check_model(Performance_recruitment_model_gaussian, check = "linearity")









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


