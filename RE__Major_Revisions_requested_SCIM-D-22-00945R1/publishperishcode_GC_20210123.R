library(tidyverse)
library(lme4)
library(emmeans)
library(car)

### Part 1. Duration and h_index
# Read the data
data = read.csv("publishperishdata.csv", header = T)

# Split the university ranking into different categories
data <- data %>%
  mutate(
    PhD.uni.rank.cat = case_when(
      PhD.uni.rank <= 20 ~ "top_20",
      PhD.uni.rank > 20 & PhD.uni.rank <= 100 ~ "20_100",
      PhD.uni.rank > 100 & PhD.uni.rank <= 200 ~ "100_200",
      PhD.uni.rank > 200 ~ "over_200")
    )

# Subset the data
dataassistant <- data[data$stage == "assistant", ]
datafull <- data[data$stage == "full", ]
dataassistantbefore <- dataassistant[dataassistant$beforeafter == "before", ]
datafullbefore <- datafull[datafull$beforeafter == "before", ]


# (1) h_index get a job
model_h_index_job = lmer(log(h_index + 1) ~ (Assistant.since) + PhD.taiwan + PhD.uni.rank 
              + sex + (1 | University / Department), data = dataassistantbefore)
model_h_index_job_cat = lmer(log(h_index + 1) ~ (Assistant.since) + PhD.taiwan + PhD.uni.rank.cat 
                         + sex + (1 | University / Department), data = dataassistantbefore)

Anova(model_h_index_job)
# Chisq Df Pr(>Chisq)    
# Assistant.since 74.6819  1     <2e-16 ***
# PhD.taiwan       1.4165  1     0.2340    
# PhD.uni.rank     0.4495  1     0.5026    
# sex              5.7279  1     0.0167 *  

Anova(model_h_index_job_cat)
# Chisq Df Pr(>Chisq)    
# Assistant.since  72.9566  1    < 2e-16 ***
# PhD.taiwan        1.0587  1    0.30352    
# PhD.uni.rank.cat  1.2015  3    0.75264    
# sex               5.4044  1    0.02009 * 


# (2) h_index promotion
model_h_index_promotion = lmer(log(h_index + 1) ~ (Assistant.since) + PhD.taiwan + PhD.uni.rank 
                         + sex + (1 | University / Department), data = datafullbefore)
model_h_index_promotion_cat = lmer(log(h_index + 1) ~ (Assistant.since) + PhD.taiwan + PhD.uni.rank.cat 
                             + sex + (1 | University / Department), data = datafullbefore)

Anova(model_h_index_promotion)
# Chisq Df Pr(>Chisq)
# Assistant.since 1.1181  1     0.2903
# PhD.taiwan      0.3219  1     0.5705
# PhD.uni.rank    0.3404  1     0.5596
# sex             0.0030  1     0.9562

Anova(model_h_index_promotion_cat)
# Chisq Df Pr(>Chisq)
# Assistant.since  0.6072  1     0.4358
# PhD.taiwan       0.3152  1     0.5745
# PhD.uni.rank.cat 0.7712  3     0.8563
# sex              0.0127  1     0.9102


# (3) Duration to get a job
model_duration_job = lmer(log(time.to.assistant + 1) ~ h_index * (Assistant.since) + PhD.taiwan +
    PhD.uni.rank + sex + (1 | University / Department), data = dataassistantbefore)
model_duration_job_cat = lmer(log(time.to.assistant + 1) ~ h_index * (Assistant.since) + PhD.taiwan +
    PhD.uni.rank.cat + sex + (1 | University / Department), data = dataassistantbefore)

Anova(model_duration_job)
# Chisq Df Pr(>Chisq)    
# h_index                  3.9285  1    0.04748 *  
# Assistant.since         48.8380  1   2.78e-12 ***
# PhD.taiwan               1.0080  1    0.31538    
# PhD.uni.rank             1.8183  1    0.17751    
# sex                      0.7814  1    0.37670    
# h_index:Assistant.since  6.0609  1    0.01382 *  

Anova(model_duration_job_cat)
# Chisq Df Pr(>Chisq)    
# h_index                  4.1604  1    0.04138 *  
# Assistant.since         48.5092  1  3.287e-12 ***
# PhD.taiwan               0.9192  1    0.33770    
# PhD.uni.rank.cat         3.5597  3    0.31310    
# sex                      0.9475  1    0.33035    
# h_index:Assistant.since  5.6120  1    0.01784 *  


# (4) Duration to get a promotion
model_duration_promotion = lmer(log(time.to.assistant + 1) ~ h_index * (Assistant.since) + PhD.taiwan +
                            PhD.uni.rank + sex + (1 | University / Department), data = datafullbefore)
model_duration_promotion_cat = lmer(log(time.to.assistant + 1) ~ h_index * (Assistant.since) + PhD.taiwan +
                                PhD.uni.rank.cat + sex + (1 | University / Department), data = datafullbefore)

Anova(model_duration_promotion)
# Chisq Df Pr(>Chisq)    
# h_index                  3.0068  1    0.08292 .  
# Assistant.since         28.3751  1  9.994e-08 ***
# PhD.taiwan               0.0734  1    0.78640    
# PhD.uni.rank             0.1835  1    0.66838    
# sex                      6.4810  1    0.01090 *  
# h_index:Assistant.since  2.9211  1    0.08743 .  

Anova(model_duration_promotion_cat)
# Chisq Df Pr(>Chisq)    
# h_index                  3.6637  1   0.055609 .  
# Assistant.since         29.0242  1  7.148e-08 ***
# PhD.taiwan               0.0750  1   0.784130    
# PhD.uni.rank.cat         3.4240  3   0.330752    
# sex                      8.4771  1   0.003596 ** 
# h_index:Assistant.since  4.3014  1   0.038081 *  


### Part 2. h_index difference before and after
# Read the data
data = read.csv("publishperish_h_index_diff.csv", header = T)

# Split the university ranking into different categories
data <- data %>%
  mutate(
    PhD.uni.rank.cat = case_when(
      PhD.uni.rank <= 20 ~ "top_20",
      PhD.uni.rank > 20 & PhD.uni.rank <= 100 ~ "20_100",
      PhD.uni.rank > 100 & PhD.uni.rank <= 200 ~ "100_200",
      PhD.uni.rank > 200 ~ "over_200")
  )

# Subset the data
dataassistant <- data[data$stage == "assistant", ]
datafull <- data[data$stage == "full", ]


# (1) h_index difference get a job
model_diff_job=lmer(h_index_diff~sex+PhD.taiwan+(PhD.uni.rank)+(Assistant.since)+sex+(1|University/Department),data=dataassistant)
model_diff_job_cat=lmer(h_index_diff~sex+PhD.taiwan+(PhD.uni.rank.cat)+(Assistant.since)+sex+(1|University/Department),data=dataassistant)

Anova(model_diff_job)
# Chisq Df Pr(>Chisq)    
# sex              0.0639  1     0.8004    
# PhD.taiwan       0.4205  1     0.5167    
# PhD.uni.rank     0.3814  1     0.5369    
# Assistant.since 15.3835  1  8.775e-05 ***

Anova(model_diff_job_cat)
# Chisq Df Pr(>Chisq)    
# sex               0.0973  1  0.7550681    
# PhD.taiwan        0.4010  1  0.5265789    
# PhD.uni.rank.cat  1.1738  3  0.7592888    
# Assistant.since  14.6611  1  0.0001287 ***


# (2) h_index difference promotion
model_diff_promotion=lmer(h_index_diff~sex+PhD.taiwan+(PhD.uni.rank)+(Assistant.since)+sex+(1|University/Department),data=datafull)
model_diff_promotion_cat=lmer(h_index_diff~sex+PhD.taiwan+(PhD.uni.rank.cat)+(Assistant.since)+sex+(1|University/Department),data=datafull)

Anova(model_diff_promotion)
# Chisq Df Pr(>Chisq)  
# sex             0.5103  1    0.47503  
# PhD.taiwan      6.2291  1    0.01257 *
# PhD.uni.rank    1.2978  1    0.25461  
# Assistant.since 1.8536  1    0.17337  

Anova(model_diff_promotion_cat)
# Chisq Df Pr(>Chisq)  
# sex              0.3318  1    0.56458  
# PhD.taiwan       5.7097  1    0.01687 *
# PhD.uni.rank.cat 1.7621  3    0.62322  
# Assistant.since  2.4449  1    0.11791  



