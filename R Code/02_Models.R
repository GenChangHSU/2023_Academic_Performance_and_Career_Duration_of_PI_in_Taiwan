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
# Model fitting
Performance_recruitment_model <- lmer(h_index ~ Assistant.since + PhD.taiwan + PhD.uni.rank + sex + (1|University/Department), 
                                      data = filter(PI_df, stage == "assistant" & beforeafter == "before"))

# Model outputs
summary(Performance_recruitment_model)
Anova(Performance_recruitment_model)
Performance_recruitment_model_CI <- confint(Performance_recruitment_model, method = "boot", nsim = 1000)

# Model diagnostics
check_model(Performance_recruitment_model, check = c("normality", "qq", "homogeneity", "linearity"))
ggsave("./Outputs/Figures/Performance_recruitment.tiff", width = 7, height = 7, dpi = 600, device = "tiff")


### (2) Promotion 
# Model fitting
Performance_promotion_model <- lmer(h_index ~ full.professor + PhD.taiwan + PhD.uni.rank + sex + (1|University/Department), 
                                    data = filter(PI_df, stage == "full" & beforeafter == "before"))

# Model outputs
summary(Performance_promotion_model)
Anova(Performance_promotion_model)
Performance_promotion_model_CI <- confint(Performance_promotion_model_poisson, method = "boot", nsim = 1000)

# Model diagnostics
check_model(Performance_promotion_model, check = c("normality", "qq", "homogeneity", "linearity"))
ggsave("./Outputs/Figures/Performance_promotion.tiff", width = 7, height = 7, dpi = 600, device = "tiff")



# 2. Career duration -----------------------------------------------------------
### (1) Recruitment
# Model fitting
Duration_recruitment_model <- lmer(time.to.assistant ~ h_index + Assistant.since + PhD.taiwan + PhD.uni.rank + sex + (1|University/Department), 
                                   data = filter(PI_df, stage == "assistant" & beforeafter == "before"))

# Model outputs
summary(Duration_recruitment_model)
Anova(Duration_recruitment_model)
Duration_recruitment_model_CI <- confint(Duration_recruitment_model, method = "boot", nsim = 1000)

# Model diagnostics
check_model(Duration_recruitment_model, check = c("normality", "qq", "homogeneity", "linearity"))
ggsave("./Outputs/Figures/Duration_recruitment.tiff", width = 7, height = 4, dpi = 600, device = "tiff")


### (2) Promotion
# Model fitting
Duration_promotion_model <- lmer(time.to.full ~ h_index + full.professor + PhD.taiwan + PhD.uni.rank + sex + (1|University/Department), 
                                 data = filter(PI_df, stage == "full" & beforeafter == "before"))

# Model outputs
summary(Duration_promotion_model)
Anova(Duration_promotion_model)
Duration_promotion_model_CI <- confint(Duration_promotion_model, method = "boot", nsim = 1000)

# Model diagnostics
check_model(Duration_promotion_model, check = c("normality", "qq", "homogeneity", "linearity"))
ggsave("./Outputs/Figures/Duration_promotion.tiff", width = 7, height = 4, dpi = 600, device = "tiff")










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


