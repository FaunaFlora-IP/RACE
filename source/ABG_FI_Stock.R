rm(list = ls()) # clear workspace

#Preparation####
##load packages####

library(tidyr)     #tidy and manipulate data 
library(tidyverse)  
library(tidyquant)
library(broom)
library(ggplot2)   #fancy plots
library(qqplotr)
library(pastecs)  #descriptive stats
library(moments)
library(patchwork)
library(BIOMASS)
library(forcats)

#add directory by pressing "ctrl" + "shift" + "H"

##load dataset####
FI_set <- read.csv("data/GAN_ForestInventory.csv") #Forest Inventory
LULC_set <- read.csv("data/GAN_StratumArea.csv")   #Area Information per stratum

# Functions ####
source("allo_func.R")
source("wood_dens_func.R")
source("classify_foresty.R")

# Step 0: Exploratory data analysis ####

## 0a) Check DBH distribution group ####
FI_set %>%
  ggplot(aes(x = factor(DBH.G, levels = c("C", "B", "A")), y = DBH, fill=factor(DBH.G))) +
  
  ggdist::stat_halfeye(
    adjust = 0.5,
    justification = -.2,
    .width = 0,
    point_colour = NA
  ) + 
  
  geom_boxplot(
    width =.12,
    outlier.colour = NA,
    alpha = .5
  ) +  
  
  ggdist::stat_dots(
    side = "left",
    justification = 1.1,
    binwidth = .25
  ) + 
  
  scale_fill_tq() + 
  theme_tq() +
  labs(
    title = "Raincloud Plot of DBH",
    subtitle = "",
    x = "DBH.G",
    y = "DBH (cm)",
    fill = "DBH.G"
  ) + 
  coord_flip()


## 0b) Check linear relationships of DBH and Tree height ####

lm_fit <- lm(Total.Height ~ DBH, data = FI_set)
r2_lm  <- summary(lm_fit)$r.squared

ggplot(FI_set, aes(DBH, Total.Height)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  theme_bw() +
  annotate(
    "label",
    x = max(FI_set$DBH, na.rm = TRUE),
    y = max(FI_set$Total.Height, na.rm = TRUE),
    hjust = 1, vjust = 1,
    label = paste("R² =", round(r2_lm, 2))
  )


# Step 1: Wood density ####
FI_with_rho <- attach_wood_density(FI_set, default_rho = 0.57)

FI_with_rho %>%
  dplyr::count(wd_level) %>%
  dplyr::mutate(prop = round(100*n/sum(n), 1))


# Step 2: Allometric selection + -> AGB (t/ha) ####
FI_calc <- calc_AGB(FI_with_rho, method = "Manuri2017_DG2") %>%
  add_agb_tpha()   # adds `AGB(ton/ha)`

# Step 3: Classify forest cover by plot ####
plot_class <- classify_forest_by_agb(FI_calc)

# (Optional) attach plot-level landcov back to each tree row:
FI_calc <- FI_calc |>
  left_join(plot_class |> select(Plot.Name, landcov), by = "Plot.Name")

# Step 4: Check allometric fit & normality ####

## 4a) Linear fit: AGB_kg ~ DBH ####
sel_eq <- lm(AGB_kg ~ DBH, data = FI_calc)
r2 <- round(summary(sel_eq)$r.squared, 3)

(p_fit <- ggplot(FI_calc, aes(x = DBH, y = AGB_kg)) +
  geom_point(color = "forestgreen", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 1) +
  labs(
    title = "AGB vs DBH",
    x = "DBH (cm)",
    y = "Tree Biomass (kg/tree)"
  ) +
  theme_bw(base_size = 14) +
  # place R² inside plot at top-right
  annotate(
    "text",
    x = Inf, y = Inf, hjust = 7, vjust = 1.5,
    label = paste("R² =", r2),
    color = "blue", size = 5
  ))

## 4b) Skewness (plot-level AGB t/ha) ####
agb_skew <- skewness(plot_class$AGB_tpha_plot, na.rm = TRUE) |> round(3)

## 4c) Normal Q–Q plot with skewness annotation
(p_qq <- ggplot(plot_class, aes(sample = AGB_tpha_plot)) +
  stat_qq_point(size = 2, color = "black") +
  stat_qq_line(color = "forestgreen") +
  labs(
    title = "Normal Q–Q Plot of AGB (t/ha) per Plot",
    x = "Theoretical quantiles (Normal)",
    y = "Sample quantiles"
  ) +
  theme_bw() +
  annotate(
    "text",
    x = Inf, y = Inf, 
    hjust = 4, vjust = 2.5,
    label = paste("Skewness =", agb_skew),
    color = "blue", size = 5
  ))

## 4c) Boxplot (plot-level AGB t/ha) ####
(p_box <- ggplot(plot_class, aes(x = "", y = AGB_tpha_plot)) +
  geom_boxplot(fill = "forestgreen", alpha = 0.5, outlier.colour = "red") +
  labs(x = NULL, y = "AGB per Plot (t/ha)") +
  theme_bw())

# Step back to allometric selection if step 4 is not satisfied.

# Step 5: Summarise tree density per stratum ####

# Step 7: Summarise AGC per stratum ####
## 7a) Plot-level AGC (tC/ha) from your already area-expanded AGB(ton/ha)  ####
plot_agc <- FI_calc %>%
  group_by(landcov, Transect, Plot.Name) %>%   # keep Transect grouping
  summarise(
    AGB_tpha = sum(`AGB(ton/ha)`, na.rm = TRUE),
    AGC_tpha = AGB_tpha * 0.47,                # convert to carbon (tC/ha)
    .groups = "drop"
  )

## Summarise per landcover with CI suppressed when N < 4 ####
agc_stats <- plot_agc %>%
  group_by(landcov) %>%
  summarise(
    N    = sum(!is.na(AGC_tpha)),
    DF   = pmax(N - 1, 0),
    mean = mean(AGC_tpha, na.rm = TRUE),
    sd   = sd(AGC_tpha, na.rm = TRUE),
    SE   = ifelse(N > 0, sd / sqrt(N), NA_real_),
    # Only compute t critical and CI when N >= 5
    t_crit   = ifelse(N >= 4, qt(0.975, DF), NA_real_),
    CI_lower = ifelse(N >= 4, mean - t_crit * SE, NA_real_),
    CI_upper = ifelse(N >= 4, mean + t_crit * SE, NA_real_),
    precision = ifelse(is.finite(mean) & mean != 0, (SE / mean) * 100, NA_real_),
    .groups = "drop"
  ) %>%
  select(landcov, N, DF, mean, SE, CI_lower, CI_upper, precision)

## 7b) AGC distribution across plot in the study areas  ####
plot_agc %>% 
  mutate(Plot.Name = fct_reorder(Plot.Name, AGC_tpha)) %>%
  ggplot(aes(x = Plot.Name, y = AGC_tpha)) +
  geom_col(aes(fill = AGC_tpha), width = 0.9, show.legend = FALSE) +
  geom_hline(yintercept = 100, linetype = 2) +
  geom_hline(yintercept = 250, linetype = 4) +
  scale_fill_gradient(low = "yellow", high = "green", na.value = "grey80") +
  labs(x = "Plot Name", y = "Above Ground Carbon (tC/ha)",
       title = "AGC across plots (ordered)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


## 7c) Hierarchical clustering####
set.seed(123)
plot_agc <- plot_agc %>%
  mutate(
    AGC_group = cutree(hclust(dist(AGC_tpha)), k = 3), #change this value
    AGC_group = paste("Cluster", AGC_group)   # convert to character
  )

# Whole plot
ggplot(plot_agc, aes(x = fct_reorder(Plot.Name, AGC_tpha),
                     y = AGC_tpha, fill = AGC_group)) +
  geom_col(show.legend = TRUE, width = 0.9) +
  scale_fill_brewer(palette = "YlGn", name = "AGC Group") +
  theme_bw() +
  labs(
    x = "Plot Name",
    y = "AGC (tC/ha)",
    title = "Cluster-based Stratification of AGC (tC/ha)"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "right"
  )

# Reorder by AGC
plot_agc_box <- plot_agc %>%
  group_by(AGC_group) %>%
  mutate(mean_AGC = mean(AGC_tpha, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(AGC_group = fct_reorder(AGC_group, mean_AGC))

# Boxplot
ggplot(plot_agc_box, aes(x = AGC_group, y = AGC_tpha, fill = AGC_group)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.shape = 21) +
  geom_jitter(width = 0.15, alpha = 0.5, size = 1.6, color = "black") +
  scale_fill_brewer(palette = "YlGn", name = "Cluster") +
  labs(
    title = "Distribution of AGC (tC/ha) by Cluster-based Strata",
    x = "AGC Cluster Group",
    y = "Above Ground Carbon (tC/ha)"
  ) +
  theme_bw(base_size = 13) +
  geom_hline(yintercept = 100, linetype = 2) +
  geom_hline(yintercept = 250, linetype = 4) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(face = "bold")
  )

## 7d) Arbitrary clustering ####
AGC_summary_all <- plot_agc %>%
  #filter(!(Transect %in% c("T3","T1"))) %>%
  #filter(!(Transect %in% c("LT1","BD1"))) %>%
  summarise(
    N  = sum(!is.na(AGC_tpha)),
    mean_AGC = mean(AGC_tpha, na.rm = TRUE),
    sd_AGC   = sd(AGC_tpha, na.rm = TRUE),
    SE_AGC   = sd_AGC / sqrt(N),
    t_crit   = qt(0.975, df = N - 1),
    CI_lower = mean_AGC - t_crit * SE_AGC,
    CI_upper = mean_AGC + t_crit * SE_AGC
  )
AGC_summary_all
mean_val <- AGC_summary_all$mean_AGC
se_val   <- AGC_summary_all$SE_AGC

plot_agc %>% 
  #filter(!(Transect %in% c("T3","T1"))) %>%
  #filter(!(Transect %in% c("LT1","BD1"))) %>%
  mutate(Plot.Name = forcats::fct_reorder(Plot.Name, AGC_tpha)) %>%
  ggplot(aes(x = Plot.Name, y = AGC_tpha)) +
  geom_col(aes(fill = AGC_tpha), width = 0.9, show.legend = FALSE) +
  geom_hline(yintercept = 100, linetype = 2) +
  geom_hline(yintercept = 250, linetype = 4) +
  geom_hline(yintercept = mean_val, color = "blue", linewidth = 1) +
  geom_hline(yintercept = mean_val + se_val, color = "blue", linetype = "dashed") +
  geom_hline(yintercept = mean_val - se_val, color = "blue", linetype = "dashed") +
  annotate("text", x = 2, y = mean_val + se_val * 1.5,
           label = paste0("Mean ± SE = ", round(mean_val, 1), " ± ", round(se_val, 1), " tC/ha"),
           color = "blue", hjust = 0, size = 3.8) +
  scale_fill_gradient(low = "yellow", high = "green", na.value = "grey80") +
  labs(
    x = "Plot Name",
    y = "Above Ground Carbon (tC/ha)",
    title = "AGC across plots",
    subtitle = "Blue line = mean ± SE (overall)"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

## 7e) Final summary ####
agc_stats <- plot_agc %>%
  filter(!(Transect %in% c("T3","T1"))) %>%
  filter(!(Transect %in% c("LT1","BD1"))) %>%
  summarise(
    N    = sum(!is.na(AGC_tpha)),
    DF   = pmax(N - 1, 0),
    mean = mean(AGC_tpha, na.rm = TRUE),
    sd   = sd(AGC_tpha, na.rm = TRUE),
    SE   = ifelse(N > 0, sd / sqrt(N), NA_real_),
    # Only compute t critical and CI when N >= 5
    t_crit   = ifelse(N >= 4, qt(0.975, DF), NA_real_),
    CI_lower = ifelse(N >= 4, mean - t_crit * SE, NA_real_),
    CI_upper = ifelse(N >= 4, mean + t_crit * SE, NA_real_),
    precision = ifelse(is.finite(mean) & mean != 0, (SE / mean) * 100, NA_real_),
    .groups = "drop"
  ) %>%
  select( N, DF, mean, SE, CI_lower, CI_upper, precision)



