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

# We assume the species column is called "species"
# and all other columns are metric_model (e.g., roc_biomod, prg_biomod, cor_biomod, ...)
df_long <- df %>%
  pivot_longer(
    cols = -species,
    names_to = "metric_model",
    values_to = "value"
  ) %>%
  separate(
    metric_model,
    into = c("metric", "model"),
    sep  = "_",
    extra = "merge"     # keep everything after first "_" as model
  ) %>%
  mutate(
    metric = tolower(metric),  # roc / prg / cor
    model  = tolower(model)
  ) %>%
  filter(metric %in% c("roc", "prg", "cor")) %>%
  filter(!is.na(value))

# ----------------------------------------------------------
# 3. Order models by overall mean AUCROC (roc) from low → high
# ----------------------------------------------------------
roc_means <- df_long %>%
  filter(metric == "roc") %>%
  group_by(model) %>%
  summarise(mean_roc = mean(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_roc)

model_order <- roc_means$model

# ----------------------------------------------------------
# 4. For EACH metric separately, rank models within species
# ----------------------------------------------------------
rank_df <- df_long %>%
  group_by(metric, species) %>%
  arrange(metric, species, desc(value)) %>%
  mutate(rank = row_number()) %>%
  ungroup()

# ----------------------------------------------------------
# 5. % of species where model is top 1 / top 2 / top 3, per metric
# ----------------------------------------------------------
rank_summary <- rank_df %>%
  group_by(metric, model) %>%
  summarise(
    pct_top1 = mean(rank == 1) * 100,
    pct_top2 = mean(rank <= 2) * 100,
    pct_top3 = mean(rank <= 3) * 100,
    .groups = "drop"
  )

rank_long <- rank_summary %>%
  pivot_longer(
    cols = starts_with("pct_top"),
    names_to = "top_k",
    values_to = "percent"
  ) %>%
  mutate(
    top_k = recode(top_k,
                   "pct_top1" = "Top 1",
                   "pct_top2" = "Top 2",
                   "pct_top3" = "Top 3"),
    model = factor(model, levels = model_order),
    # <-- change this line
    top_k = factor(top_k, levels = c("Top 3", "Top 2", "Top 1")),
    metric = factor(metric, levels = c("roc", "prg", "cor"))
  )

# ----------------------------------------------------------
# 6. Plot: 3 panels (ROC / PRG / COR) combined in one figure
# ----------------------------------------------------------

# text color for contrast
# ----------------------------------------------------------
# 6. Plot: 3 panels (ROC / PRG / COR) with light YlGnBu palette
# ----------------------------------------------------------

rank_long <- rank_long %>%
  mutate(
    metric = factor(metric, levels = c("roc", "prg", "cor"))
  )

ggplot(rank_long, aes(x = model, y = top_k, fill = percent)) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = sprintf("%.1f", percent), color = text_col),
    size = 3
  ) +
  scale_color_identity() +
  scale_fill_distiller(
    palette = "YlGnBu",
    direction = 1,
    limits = c(0, 100),
    name = "% of species"
  ) +
  scale_x_discrete(
    labels = c(
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
      "ensemble" = "Ensemble",
      "brt" = "BRT",
      "maxent_tuned" = "Maxent_tuned",
      "maxent" = "Maxent",
      "rf_downsampled" = "RF_downsampled",
      "merged" = "CNN_Optimal",
      "merged_aug" = "CNN_Optimal_Aug"   # etc – use your actual model names
    )
  ) +
  facet_grid(
    rows = vars(metric),
    labeller = labeller(
      metric = as_labeller(
        c(roc = "AUC[ROC]", prg = "AUC[PRG]", cor = "COR"),
        label_parsed
      )
    )
  ) +
  labs(
    x = "",
    y = "",
    fill = "% species"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    panel.grid   = element_blank(),
    strip.text.y = element_text(face = "bold")
  )
