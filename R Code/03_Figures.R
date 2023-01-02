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
library(patchwork)
library(ggsci)


# Import files -----------------------------------------------------------------
PI_df <- read.csv("./Data_raw/publishperishdata.csv", header = T)


# ggplot theme -----------------------------------------------------------------
my_theme <- 
   theme(
    # Axis
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 15, margin = margin(t = 10)),
    axis.title.y = element_text(size = 15, margin = margin(r = 8)),
    
    # Plot
    plot.title = element_text(hjust = 0.5, size = 18),
    plot.background = element_rect(colour = "transparent"),
    
    # Panel
    panel.background = element_rect(fill = "transparent"),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    # Legend
    legend.position = "right",
    legend.spacing.x = unit(0.2, "cm"),
    legend.key.width = unit(0.5, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.key = element_blank(),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10, hjust = 0.5, vjust = 0.5),
    legend.box.just = "center",
    legend.justification = c(0.5, 0.5),
    legend.title.align = 0.5,
    legend.background = element_rect(fill = "transparent", color = NA),
    
    # Facet strip
    strip.background = element_rect(fill = "transparent"),
    strip.text = element_text(size = 12, hjust = 0.5)
  )


############################### Code starts here ###############################

# 1. Histogram of university rankings ------------------------------------------
Phd_rank_median <- distinct(PI_df, Name, .keep_all = T) %>% 
  group_by(PhD.taiwan) %>% 
  summarise(median = median(PhD.uni.rank, na.rm = T),
            n = n())

ggplot(data = distinct(PI_df, Name, .keep_all = T)) + 
  geom_histogram(aes(x = PhD.uni.rank, color = PhD.taiwan, fill = PhD.taiwan), 
                 alpha = 0.7, size = 0.7, bins = 60) + 
  geom_vline(data = Phd_rank_median, aes(xintercept = median, color = PhD.taiwan),
             linetype = "dashed", show.legend = F) +
  labs(x = "PhD university ranking", y = "Number of PIs") + 
  scale_x_continuous(limits = c(0, 1250), breaks = c(1, seq(300, 1200, 300))) + 
  scale_y_continuous(limits = c(0, 40), expand = c(0, 0)) +
  scale_fill_manual(values = c("#d95f02", "#1b9e77"), name = "PhD origin", labels = c("Foreign", "Taiwan")) +
  scale_color_manual(values = c("#d95f02", "#1b9e77"), name = "PhD origin", labels = c("Foreign", "Taiwan")) + 
  guides(color = "none", fill = guide_legend(override.aes = list(alpha = 1))) + 
  theme_classic() + 
  theme(axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title.x = element_text(size = 15, margin = margin(t = 10)),
        axis.title.y = element_text(size = 15, margin = margin(r = 8)),
        legend.position = c(0.75, 0.75))

ggsave("./Outputs/Figures/Phd_hist.tiff", width = 5, height = 4, dpi = 600, device = "tiff")


# 2. Scatterplots of academic performance/career duration vs. year -------------
P_performance_recruitment <- ggplot(data = filter(PI_df, beforeafter == "before" & stage == "assistant")) + 
  geom_point(aes(x = Assistant.since, y = h_index)) + 
  geom_smooth(aes(x = Assistant.since, y = h_index, color = sex), method = "glm", method.args = list(family = "poisson"),
              se = T) + 
  labs(x = "Year of recruitment", y = 'Academic performance ("Before" h-index)',
       title = "(a)") +
  scale_x_continuous(limits = c(1990, 2020), breaks = c(1990, 2000, 2010, 2020)) +
  scale_y_continuous(limits = c(0, 25)) + 
  scale_color_brewer(palette = "Set1", name = "", labels = c("Female", "Male")) + 
  guides(color = guide_legend(override.aes = list(fill = "white"))) +
  my_theme + 
  theme(plot.title = element_text(vjust = 1, hjust = 0),
        legend.position = c(0.32, 0.9),
        legend.direction = "horizontal",
        legend.key.width = unit(0.3, "inch"),
        legend.text = element_text(margin = margin(r = 10, unit = "pt")))
  
P_performance_recruitment

P_performance_promotion <- ggplot(data = filter(PI_df, beforeafter == "before" & stage == "full")) + 
  geom_point(aes(x = full.professor, y = h_index)) + 
  geom_smooth(aes(x = full.professor, y = h_index), method = "glm", method.args = list(family = "poisson"),
              se = T, color = "black") + 
  labs(x = "Year of promotion", y = 'Academic performance ("Before" h-index)',
       title = "(b)") +
  scale_x_continuous(limits = c(1990, 2020), breaks = c(1990, 2000, 2010, 2020)) +
  scale_y_continuous(limits = c(0, 25)) + 
  my_theme + 
  theme(plot.title = element_text(vjust = 1, hjust = 0))

P_performance_promotion

P_duration_recruitment <- ggplot(data = filter(PI_df, beforeafter == "before" & stage == "assistant")) + 
  geom_point(aes(x = Assistant.since, y = time.to.assistant)) + 
  geom_smooth(aes(x = Assistant.since, y = time.to.assistant), method = "glm", method.args = list(family = "poisson"),
              se = T, color = "black") + 
  labs(x = "Year of recruitment", y = 'Duration before recruitment (year)',
       title = "(c)") +
  scale_x_continuous(limits = c(1990, 2020), breaks = c(1990, 2000, 2010, 2020)) +
  scale_y_continuous(limits = c(0, 11.5)) + 
  my_theme + 
  theme(plot.title = element_text(vjust = 1, hjust = 0))

P_duration_recruitment

P_duration_promotion <- ggplot(data = filter(PI_df, beforeafter == "before" & stage == "full")) + 
  geom_point(aes(x = full.professor, y = time.to.full)) + 
  geom_smooth(aes(x = full.professor, y = time.to.full), method = "glm", method.args = list(family = "poisson"),
              se = T, color = "black") + 
  labs(x = "Year of promotion", y = 'Duration before promotion (year)',
       title = "(d)") +
  scale_x_continuous(limits = c(1990, 2020), breaks = c(1990, 2000, 2010, 2020)) +
  scale_y_continuous(limits = c(0, 25)) + 
  my_theme + 
  theme(plot.title = element_text(vjust = 1, hjust = 0))

P_duration_promotion

P_performance_recruitment + P_performance_promotion +
  P_duration_recruitment + P_duration_promotion + 
  plot_layout(nrow = 2)
  
ggsave("./Outputs/Figures/perf_duration_scatterplot.tiff", width = 10, height = 9.5, dpi = 600, device = "tiff")


# 3. Scatterplots of difference in academic performance vs. year & boxplots of PhD origin -----
PI_diff_df <- PI_df %>% 
  select(1, University, Department, Assistant.since, full.professor, PhD.taiwan, PhD.uni.rank, sex, h_index, stage, beforeafter) %>% 
  pivot_wider(names_from = stage, values_from = h_index) %>% 
  pivot_wider(names_from = beforeafter, values_from = c(assistant, full)) %>% 
  mutate(recruitment_diff = assistant_after - assistant_before,
         promotion_diff = full_after - full_before)




