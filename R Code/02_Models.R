## -----------------------------------------------------------------------------
## Title: Analysis of the academic performance and career duration of Taiwanese 
##        EEB PIs
##
## Author: Gen-Chang Hsu
##
## Date: 2022-12-31
##
## Description:
##  
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
Performance_recruitment_model_gaussian <- lmer(h_index ~ Assistant.since + PhD.taiwan + PhD.uni.rank + sex + (1|University/Department), 
                                               data = filter(PI_df, stage == "assistant" & beforeafter == "before"))

Performance_recruitment_model_poisson <- glmer(h_index ~ Assistant.since + PhD.taiwan + PhD.uni.rank + sex + (1|University/Department), 
                                               data = filter(PI_df, stage == "assistant" & beforeafter == "before"),
                                               family = "poisson")
# Model selection
AIC(Performance_recruitment_model_gaussian, Performance_recruitment_model_poisson)

# Model outputs
Performance_recruitment_model_poisson_summary <- summary(Performance_recruitment_model_poisson)
Performance_recruitment_model_poisson_Anova <- Anova(Performance_recruitment_model_poisson)
Performance_recruitment_model_poisson_CI <- confint(Performance_recruitment_model_poisson, method = "boot", nsim = 1000)

# Model diagnostics
check_model(Performance_recruitment_model_poisson, check = c("normality", "qq", "homogeneity", "linearity"))
ggsave("./Outputs/Figures/Performance_recruitment.tiff", width = 7, height = 4, dpi = 600, device = "tiff")


### (2) Promotion 
# Model fitting
Performance_promotion_model_gaussian <- lmer(h_index ~ full.professor + PhD.taiwan + PhD.uni.rank + sex + (1|University/Department), 
                                             data = filter(PI_df, stage == "full" & beforeafter == "before"))

Performance_promotion_model_poisson <- glmer(h_index ~ full.professor + PhD.taiwan + PhD.uni.rank + sex + (1|University/Department), 
                                             data = filter(PI_df, stage == "full" & beforeafter == "before"),
                                             family = "poisson")

# Model selection
AIC(Performance_promotion_model_gaussian, Performance_promotion_model_poisson)

# Model outputs
Performance_promotion_model_poisson_summary <- summary(Performance_promotion_model_poisson)
Performance_promotion_model_poisson_Anova <- Anova(Performance_promotion_model_poisson)
Performance_promotion_model_poisson_CI <- confint(Performance_promotion_model_poisson, method = "boot", nsim = 1000)

# Model diagnostics
check_model(Performance_promotion_model_poisson, check = c("normality", "qq", "homogeneity", "linearity"))
ggsave("./Outputs/Figures/Performance_promotion.tiff", width = 7, height = 4, dpi = 600, device = "tiff")



# 2. Career duration -----------------------------------------------------------
### (1) Recruitment
# Model fitting
Duration_recruitment_model_gaussian <- lmer(time.to.assistant ~ h_index + Assistant.since + PhD.taiwan + PhD.uni.rank + sex + (1|University/Department), 
                                            data = filter(PI_df, stage == "assistant" & beforeafter == "before"))

Duration_recruitment_model_poisson <- glmer(time.to.assistant ~ h_index + Assistant.since + PhD.taiwan + PhD.uni.rank + sex + (1|University/Department), 
                                            data = filter(PI_df, stage == "assistant" & beforeafter == "before"),
                                            family = "poisson")

# Model selection
AIC(Duration_recruitment_model_gaussian, Duration_recruitment_model_poisson)

# Model outputs
Duration_recruitment_model_poisson_summary <- summary(Duration_recruitment_model_poisson)
Duration_recruitment_model_poisson_Anova <- Anova(Duration_recruitment_model_poisson)
Duration_recruitment_model_poisson_CI <- confint(Duration_recruitment_model_poisson, method = "boot", nsim = 1000)

# Model diagnostics
check_model(Duration_recruitment_model_poisson, check = c("normality", "qq", "homogeneity", "linearity"))
ggsave("./Outputs/Figures/Duration_recruitment.tiff", width = 7, height = 4, dpi = 600, device = "tiff")


### (2) Promotion
# Model fitting
Duration_promotion_model_gaussian <- lmer(time.to.full ~ h_index + full.professor + PhD.taiwan + PhD.uni.rank + sex + (1|University/Department), 
                                          data = filter(PI_df, stage == "full" & beforeafter == "before"))

Duration_promotion_model_poisson <- glmer(time.to.full ~ h_index + full.professor + PhD.taiwan + PhD.uni.rank + sex + (1|University/Department), 
                                          data = filter(PI_df, stage == "full" & beforeafter == "before"),
                                          family = "poisson")

# Model selection
AIC(Duration_promotion_model_gaussian, Duration_promotion_model_poisson)

# Model outputs
Duration_promotion_model_poisson_summary <- summary(Duration_promotion_model_poisson)
Duration_promotion_model_poisson_Anova <- Anova(Duration_promotion_model_poisson)
Duration_promotion_model_poisson_CI <- confint(Duration_promotion_model_poisson, method = "boot", nsim = 1000)

# Model diagnostics
check_model(Duration_promotion_model_poisson, check = c("normality", "qq", "homogeneity", "linearity"))
ggsave("./Outputs/Figures/Duration_promotion.tiff", width = 7, height = 4, dpi = 600, device = "tiff")



# 3. Performance difference ----------------------------------------------------
# Compute the differences in h-index during recruitment and promotion phase
PI_diff_df <- PI_df %>% 
  select(search.id, University, Department, Assistant.since, full.professor, PhD.taiwan, PhD.uni.rank, sex, h_index, stage, beforeafter) %>% 
  pivot_wider(names_from = stage, values_from = h_index) %>% 
  pivot_wider(names_from = beforeafter, values_from = c(assistant, full)) %>% 
  mutate(recruitment_diff = assistant_after - assistant_before,
         promotion_diff = full_after - full_before)
  
### (1) Recruitment
Diff_recruitment_model_gaussain <- lmer(recruitment_diff ~ Assistant.since + PhD.taiwan + PhD.uni.rank + sex + (1|University/Department),
                                        data = PI_diff_df)

# Model outputs
Diff_recruitment_model_gaussain_summary <- summary(Diff_recruitment_model_gaussain)
Diff_recruitment_model_gaussain_Anova <- Anova(Diff_recruitment_model_gaussain)
Diff_recruitment_model_gaussain_CI <- confint(Diff_recruitment_model_gaussain, method = "boot", nsim = 1000)

# Model diagnostics
check_model(Diff_recruitment_model_gaussain, check = c("qq", "homogeneity"))
ggsave("./Outputs/Figures/Diff_recruitment.tiff", width = 7, height = 4, dpi = 600, device = "tiff")


### (2) Promotion
Diff_promotion_model_gaussain <- lmer(promotion_diff ~ full.professor + PhD.taiwan + PhD.uni.rank + sex + (1|University/Department),
                                      data = PI_diff_df)

# Model outputs
Diff_promotion_model_gaussain_summary <- summary(Diff_promotion_model_gaussain)
Diff_promotion_model_gaussain_Anova <- Anova(Diff_promotion_model_gaussain)
Diff_promotion_model_gaussain_CI <- confint(Diff_promotion_model_gaussain, method = "boot", nsim = 1000)

# Model diagnostics
check_model(Diff_promotion_model_gaussain, check = c("qq", "homogeneity"))
ggsave("./Outputs/Figures/Diff_promotion.tiff", width = 7, height = 4, dpi = 600, device = "tiff")



# 4. Summary table for the model results ---------------------------------------
model1_summary <- Performance_recruitment_model_poisson_summary$coefficients[2:5, 1:2] %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "Predictor") %>% 
  mutate(model = 1, 
         n = length(Performance_recruitment_model_poisson_summary$residuals),
         boot.CI.lower = as.data.frame(Performance_recruitment_model_poisson_CI[4:7, ]) %>% .$`2.5 %`,
         boot.CI.upper = as.data.frame(Performance_recruitment_model_poisson_CI[4:7, ]) %>% .$`97.5 %`) %>% 
  bind_cols(., as.data.frame(Performance_recruitment_model_poisson_Anova)) %>% 
  relocate(model)

model2_summary <- Performance_promotion_model_poisson_summary$coefficients[2:5, 1:2] %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "Predictor") %>% 
  mutate(model = 2, 
         n = length(Performance_promotion_model_poisson_summary$residuals),
         boot.CI.lower = as.data.frame(Performance_promotion_model_poisson_CI[4:7, ]) %>% .$`2.5 %`,
         boot.CI.upper = as.data.frame(Performance_promotion_model_poisson_CI[4:7, ]) %>% .$`97.5 %`) %>% 
  bind_cols(., as.data.frame(Performance_promotion_model_poisson_Anova)) %>% 
  relocate(model)

model3_summary <- Duration_recruitment_model_poisson_summary$coefficients[2:6, 1:2] %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "Predictor") %>% 
  mutate(model = 3, 
         n = length(Duration_recruitment_model_poisson_summary$residuals),
         boot.CI.lower = as.data.frame(Duration_recruitment_model_poisson_CI[4:8, ]) %>% .$`2.5 %`,
         boot.CI.upper = as.data.frame(Duration_recruitment_model_poisson_CI[4:8, ]) %>% .$`97.5 %`) %>% 
  bind_cols(., as.data.frame(Duration_recruitment_model_poisson_Anova)) %>% 
  relocate(model)

model4_summary <- Duration_promotion_model_poisson_summary$coefficients[2:6, 1:2] %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "Predictor") %>% 
  mutate(model = 4, 
         n = length(Duration_promotion_model_poisson_summary$residuals),
         boot.CI.lower = as.data.frame(Duration_promotion_model_poisson_CI[4:8, ]) %>% .$`2.5 %`,
         boot.CI.upper = as.data.frame(Duration_promotion_model_poisson_CI[4:8, ]) %>% .$`97.5 %`) %>% 
  bind_cols(., as.data.frame(Duration_promotion_model_poisson_Anova)) %>% 
  relocate(model)

model5_summary <- Diff_recruitment_model_gaussain_summary$coefficients[2:5, 1:2] %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "Predictor") %>% 
  mutate(model = 5, 
         n = length(Diff_recruitment_model_gaussain_summary$residuals),
         boot.CI.lower = as.data.frame(Diff_recruitment_model_gaussain_CI[5:8, ]) %>% .$`2.5 %`,
         boot.CI.upper = as.data.frame(Diff_recruitment_model_gaussain_CI[5:8, ]) %>% .$`97.5 %`) %>% 
  bind_cols(., as.data.frame(Diff_recruitment_model_gaussain_Anova)) %>% 
  relocate(model)

model6_summary <- Diff_promotion_model_gaussain_summary$coefficients[2:5, 1:2] %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "Predictor") %>% 
  mutate(model = 6, 
         n = length(Diff_promotion_model_gaussain_summary$residuals),
         boot.CI.lower = as.data.frame(Diff_promotion_model_gaussain_CI[5:8, ]) %>% .$`2.5 %`,
         boot.CI.upper = as.data.frame(Diff_promotion_model_gaussain_CI[5:8, ]) %>% .$`97.5 %`) %>% 
  bind_cols(., as.data.frame(Diff_promotion_model_gaussain_Anova)) %>% 
  relocate(model)

summary_all <- bind_rows(model1_summary,
                         model2_summary,
                         model3_summary,
                         model4_summary,
                         model5_summary,
                         model6_summary)

write_csv(summary_all, "./Outputs/Tables/summary_all.csv")


