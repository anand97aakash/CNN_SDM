# ----------------------------------------------------------
# 0. Packages
# ----------------------------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# ----------------------------------------------------------
# 1. Read your Excel file
# ----------------------------------------------------------
# Change this to your actual file name and sheet
df <- read_excel(
  "X:\\akash\\aa_PhD\\Elith et al 2006\\papers\\2021 paper with new models and codes\\models_output\\cnn1\\with_augmentation\\lr_scheduler\\5 Augs\\With&withoutAug_ROCRanked.xlsx",
  sheet = "all_in_table"
)

library(tidyverse)

# df <- readr::read_csv("your_file.csv")

# If you don't have a species identifier column, create one
if(!("Species" %in% names(df))) {
  df <- df %>% mutate(Species = paste0("sp_", row_number()))
}

library(tidyverse)
library(stringr)

# ---- Set model order + nice labels (edit as needed) ----
model_order  <- c("biomod", "brt", "Ensemble", "rf_downsampled", "merged", "merged_aug")
model_labels <- c(
  biomod = "BIOCLIM",
  brt = "BRT",
  Ensemble = "Ensemble",
  merged = "CNN_Optimal",
  merged_aug = "CNN_Optimal_Aug",
  rf_downsampled = "RF_downsampled"
)

model_order  <- c("rf", "svm", "biomod", "brt", "cforest_w", "cforest",
                  "mars","glm","gam","xgboost","ridge","lasso","glm_iwlr",
                  "glm_unw","gam_iwlr","Ensemble","maxent_tuned","maxent",
                  "rf_downsampled", "merged", "merged_aug")

model_labels <- c(
  "rf" = "RF",
  "svm" = "SVM",
  "biomod"    = "Biomod",
  "brt"       = "BRT",
  "cforest_w" = "cForest_w",
  "cforest"   = "cForest",
  "mars" = "MARS",
  "glm" = "GLM",
  "gam" = "GAM",
  "xgboost" = "xgBoost",
  "ridge" = "Ridge",
  "lasso" = "Lasso",
  "glm_iwlr" = "GLM_iwlr",
  "glm_unw" = "GLM_unw",
  "gam_iwlr" = "GAM_iwlr",
  "Ensemble" = "Ensemble",
  "maxent_tuned" = "Maxent_tuned",
  "maxent" = "Maxent",
  "rf_downsampled" = "RF_downsampled",
  "merged" = "CNN_Optimal",
  "merged_aug" = "CNN_Optimal_Aug"
)

# ---- Build rank table: mean ROC per group × model, then rank within group ----
rank_df <- roc_long %>%
  group_by(Species, model) %>%
  summarise(roc_mean = mean(roc, na.rm = TRUE), .groups = "drop") %>%
  group_by(Species) %>%
  mutate(rank = dense_rank(desc(roc_mean))) %>%
  ungroup() %>%
  mutate(
    Species = str_wrap(Species, 28),
    model   = factor(model, levels = model_order)
  ) %>%
  filter(!is.na(model))

# Order rows by best-performing (lowest) rank for a cleaner narrative
rank_df <- rank_df %>%
  group_by(Species) %>%
  mutate(best_rank = min(rank, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Species = fct_reorder(Species, best_rank, .desc = FALSE))

# ---- Manuscript-ready heatmap ----
p <- ggplot(rank_df, aes(x = model, y = Species, fill = rank)) +
  geom_tile(color = "white", linewidth = 0.7) +
  geom_text(aes(label = model_labels), size = 3.8, fontface = "bold") +
  # lighter, clean palette; rank 1 = best (lighter/brighter end)
  scale_fill_viridis_c(
    option = "C", direction = -1, begin = 0.15, end = 0.95,
    name = "Rank (1 = best)",
    breaks = sort(unique(rank_df$rank))
  ) +
  scale_x_discrete(labels = model_labels) +
  labs(x = NULL, y = NULL) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size = 11),
    axis.text.y = element_text(size = 11),
    legend.title = element_text(size = 11),
    legend.text  = element_text(size = 10),
    legend.key.height = unit(0.9, "cm"),
    plot.margin = margin(6, 6, 6, 6)
  )
p <- ggplot(rank_df, aes(x = model, y = Species, fill = rank)) +
  geom_tile(color = "white", linewidth = 0.7) +
  geom_text(aes(label = rank), size = 3) +
  scale_fill_gradient(
    low = "#3182BD", high = "#DEEBF7",
    name = "Rank (1 = best)",
    breaks = sort(unique(rank_df$rank))
  ) +
  scale_x_discrete(
    limits = model_order,      # fixes order
    labels = model_labels      # applies your custom labels
  ) +
  labs(x = NULL, y = NULL) +
  theme_classic(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  scale_fill_gradient(
    low = "#3182BD", high = "#DEEBF7",
    limits = c(1, 21),
    breaks = c(1,  21),
    name = "Rank (1 = best)"
  ) +
  guides(fill = guide_colorbar(
    barheight = unit(4.5, "cm"),
    barwidth  = unit(0.5, "cm"),
    ticks.colour = "grey30",
    frame.colour = "grey30",
    title.position = "top",
    title.hjust = 0.5
  ))+
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.position = "bottom"
  )+ guides(fill = guide_colorbar(direction = "horizontal"))

p

# ---- Save for manuscript (recommended sizes) ----
ggsave("ROC_rank_heatmap.png", p, width = 7.2, height = 4.2, dpi = 600)
ggsave("ROC_rank_heatmap.pdf", p, width = 7.2, height = 4.2)
