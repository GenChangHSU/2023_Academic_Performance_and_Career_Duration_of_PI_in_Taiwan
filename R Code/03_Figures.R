## -----------------------------------------------------------------------------
## Title: Analysis of the academic performance and career duration of Taiwanese 
##        EEB PIs
##
## Author: Gen-Chang Hsu
##
## Date: 2023-02-12
##
## Description:
## 1. Histogram of university rankings.
## 2. Scatterplots of academic performance/career duration vs. year
## 3. Scatterplots of difference in academic performance vs. year & boxplots of PhD origin
## -----------------------------------------------------------------------------
set.seed(123)


# Libraries --------------------------------------------------------------------
library(tidyverse)
library(patchwork)
library(MASS)


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
  geom_smooth(aes(x = Assistant.since, y = h_index, color = sex), method = "glm.nb", se = T) + 
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
        legend.text = element_text(margin = margin(r = 10, unit = "pt"))) + 
  annotate(geom = "text", x = 1995, y = 20, label = "italic(P) < 0.001", 
           parse = T, size = 5)
  
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
  theme(plot.title = element_text(vjust = 1, hjust = 0)) + 
  annotate(geom = "text", x = 1995, y = 20, label = "italic(P) < 0.001", 
           parse = T, size = 5)

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
  theme(plot.title = element_text(vjust = 1, hjust = 0)) + 
  annotate(geom = "text", x = 1995, y = 9.3, label = "italic(P) < 0.001", 
           parse = T, size = 5)

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
  theme(plot.title = element_text(vjust = 1, hjust = 0)) + 
  annotate(geom = "text", x = 1995, y = 20, label = "italic(P) == 0.02", 
           parse = T, size = 5)

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

P_diff_recruitment <- ggplot(data = PI_diff_df) + 
  geom_point(aes(x = Assistant.since, y = recruitment_diff)) + 
  geom_smooth(aes(x = Assistant.since, y = recruitment_diff), method = "lm",
              se = T, color = "black") + 
  labs(x = "Year of recruitment", y = "Difference in academic performance (h-index)",
       title = "(a)") +
  scale_x_continuous(limits = c(1990, 2020), breaks = c(1990, 2000, 2010, 2020)) +
  scale_y_continuous(limits = c(-10, 22)) + 
  my_theme + 
  theme(plot.title = element_text(vjust = 1, hjust = 0)) + 
  annotate(geom = "text", x = 1995, y = 20, label = "italic(P) < 0.001", 
           parse = T, size = 5)

P_diff_recruitment

P_diff_recruitment_botplot <- ggplot(data = PI_diff_df) + 
  geom_boxplot(aes(x = PhD.taiwan, y = recruitment_diff, fill = PhD.taiwan),
               color = "black", show.legend = F, outlier.color = NA, size = 1.3,
               width = 0.6) + 
  geom_point(aes(x = PhD.taiwan, y = recruitment_diff), color = "grey50", alpha = 0.5,
             position = position_jitter(width = 0.1)) + 
  labs(x = "Year of recruitment", y = "Difference in academic performance (h-index)",
       title = "(b)") +
  scale_y_continuous(limits = c(-10, 22)) +
  scale_x_discrete(labels = c("Foreign", "Taiwan")) + 
  scale_fill_manual(values = c("#d95f02", "#1b9e77")) +
  my_theme + 
  theme(plot.title = element_text(vjust = 1, hjust = 0)) + 
  annotate(geom = "text", x = 1.5, y = 20, label = "italic(P) == 0.52", 
           parse = T, size = 5)

P_diff_recruitment_botplot 

P_diff_promotion <- ggplot(data = PI_diff_df) + 
  geom_point(aes(x = full.professor, y = promotion_diff)) + 
  geom_smooth(aes(x = full.professor, y = promotion_diff), method = "lm",
              se = F, color = "black", linetype = "dashed") + 
  labs(x = "Year of promotion", y = "Difference in academic performance (h-index)",
       title = "(c)") +
  scale_x_continuous(limits = c(1990, 2020), breaks = c(1990, 2000, 2010, 2020)) +
  scale_y_continuous(limits = c(-10, 10)) + 
  my_theme + 
  theme(plot.title = element_text(vjust = 1, hjust = 0)) + 
  annotate(geom = "text", x = 1995, y = 8, label = "italic(P) == 0.09", 
           parse = T, size = 5)

P_diff_promotion

P_diff_promotion_botplot <- ggplot(data = PI_diff_df) + 
  geom_boxplot(aes(x = PhD.taiwan, y = promotion_diff, fill = PhD.taiwan),
               color = "black", show.legend = F, outlier.color = NA, size = 1.3,
               width = 0.6) + 
  geom_point(aes(x = PhD.taiwan, y = promotion_diff), color = "grey50", alpha = 0.5,
             position = position_jitter(width = 0.1)) + 
  labs(x = "Year of promotion", y = "Difference in academic performance (h-index)",
       title = "(d)") +
  scale_y_continuous(limits = c(-10, 10)) +
  scale_x_discrete(labels = c("Foreign", "Taiwan")) + 
  scale_fill_manual(values = c("#d95f02", "#1b9e77")) +
  my_theme + 
  theme(plot.title = element_text(vjust = 1, hjust = 0)) + 
  annotate(geom = "text", x = 1.5, y = 8, label = "italic(P) == 0.06", 
           parse = T, size = 5)

P_diff_promotion_botplot 

P_diff_recruitment + P_diff_recruitment_botplot + 
  P_diff_promotion + P_diff_promotion_botplot + 
  plot_layout(nrow = 2)

ggsave("./Outputs/Figures/diff_scatterplot_boxplot.tiff", width = 10, height = 9.5, dpi = 600, device = "tiff")


