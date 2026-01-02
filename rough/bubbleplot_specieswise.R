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

data <- read_excel(
  "X:\\akash\\aa_PhD\\Elith et al 2006\\papers\\2021 paper with new models and codes\\models_output\\cnn1\\with_augmentation\\lr_scheduler\\5 Augs\\With&withoutAug_ROCRanked.xlsx",
  sheet = "best_extent"
)

data_nsw <- data %>%
  filter(Region == "NSW")

summary_df <- data_nsw %>%
  group_by(Species, Extent) %>%
  summarise(
    n_species = n_distinct(species),
    .groups = "drop"
  ) %>%
  group_by(Species) %>%
  mutate(
    pct_n_species = 100 * n_species / sum(n_species, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(Extent = factor(Extent, levels = c(32, 64, 128)))

summary_df

species_order <- c(
  "Diurnal Birds",
  "Nocturnal Birds",
  "Small Reptiles",
  "Bats",
  "Open Forest Trees",
  "Open Forest Understorey Vascular Plants",
  "Rainforest Trees",
  "Rainforest Understorey Vascular Plants"
  
)

summary_df <- summary_df %>%
  mutate(Species = factor(Species, levels = species_order))

# (optional but recommended) warn if you missed/typo’d any names
setdiff(unique(as.character(summary_df$Species)), species_order)
setdiff(species_order, unique(as.character(summary_df$Species)))


# total per Species (sum across Extent)
species_totals <- summary_df %>%
  group_by(Species) %>%
  summarise(total_n = sum(n_species, na.rm = TRUE), .groups = "drop")

# named vector: Species -> "Species\n(n=total)"
x_labs <- setNames(
  paste0(as.character(species_totals$Species), "\n(n=", species_totals$total_n, ")"),
  as.character(species_totals$Species)
)

library(scales)

lower <- 10
upper <- max(summary_df$pct_n_species, na.rm = TRUE)

# breaks that *must* include 10
brks <- pretty(c(lower, upper), n = 4)
brks <- sort(unique(c(lower, brks)))
brks <- brks[brks >= lower & brks <= upper]

# colors for those legend keys (same gradient, same order as brks)
cols <- scales::col_numeric(
  #palette = c("#F2F0F7", "#3F007D"),
  palette = c("#caf0f8", "#03045e"),
  domain  = c(lower, upper)
)(brks)

p<- ggplot(summary_df,
       aes(x = Species, y = Extent, size = pct_n_species, fill = pct_n_species)) +
  geom_point(shape = 21, alpha = 0.7, color = "grey20", stroke = 0.35) +
  
  scale_size_area(
    max_size = 15,
    name   = "Percent of species (%)",
    limits = c(lower, upper),   # <- key: keeps 10 in legend
    breaks = brks
  ) +
  
  scale_fill_gradient(
    low = "#caf0f8", high = "#03045e",
    limits = c(lower, upper),   # <- keep gradient consistent with legend colors
    guide = "none"
  ) +
  
  scale_x_discrete(labels = x_labs) +
  labs(
    x = "Species",
    y = "Extent",
    #title = "Number of species for which each extent is optimal (NSW)"
  ) +
  scale_y_discrete(labels = c(`32` = "32x32", `64` = "64x64", `128` = "128x128"))+
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    theme(text = element_text(size = 14)),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "bottom",
    legend.box = "horizontal"
  ) +
  guides(
    size = guide_legend(
      title.position = "top",
      nrow = 1,
      override.aes = list(fill = cols)  # length matches brks
    )
  )

ggsave("Fig_optimal_extent_bubble.png", p,
       width = 180, height = 130, units = "mm",
       dpi = 600)


#####################################################
#### Birds
data <- read_excel(
  "X:\\akash\\aa_PhD\\Elith et al 2006\\papers\\2021 paper with new models and codes\\models_output\\cnn1\\with_augmentation\\lr_scheduler\\5 Augs\\With&withoutAug_ROCRanked.xlsx",
  sheet = "best_extent"
)

data_nsw <- data %>%
  filter(Species == "Birds")


summary_df <- data_nsw %>%
  group_by(Region, Extent) %>%
  summarise(
    n_species = n_distinct(species),
    .groups = "drop"
  ) %>%
  group_by(Region) %>%
  mutate(
    pct_n_species = 100 * n_species / sum(n_species, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(Extent = factor(Extent, levels = c(32, 64, 128)))

region_levels <- c("AWT","CAN")#,"NSW","NZ","SA","SWI")

plot_df <- summary_df %>%
  mutate(
    Region = factor(Region, levels = region_levels),
    Extent = factor(Extent, levels = c(32, 64, 128))  # order in legend + stacking
  )

p<- ggplot(plot_df, aes(x = Region, y = pct_n_species, fill = Extent)) +
  geom_col(
    width = 0.75,
    color = NA,
    position = position_stack(reverse = TRUE)   # remove this if you don't want reversed stacking
  ) +
  scale_fill_manual(
    name   = "Extent",
    values = c(`32` = "#E6F7F5", `64` = "#7FCDBB", `128` = "#006D6F"),
    breaks = c(32, 64, 128),
    labels = c("32x32", "64x64", "128x128")
  ) +
  labs(x = "Region", y = "Percent of species (%)") +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    theme(text = element_text(size = 14)),
    axis.text.x = element_text( hjust = 1),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

ggsave("Fig_optimal_extent_bubble_birds.png", p,
       width = 72, height = 80, units = "mm",
       dpi = 600)

#### Vascular Plants
data <- read_excel(
  "X:\\akash\\aa_PhD\\Elith et al 2006\\papers\\2021 paper with new models and codes\\models_output\\cnn1\\with_augmentation\\lr_scheduler\\5 Augs\\With&withoutAug_ROCRanked.xlsx",
  sheet = "best_extent"
)

data_nsw <- data %>%
  filter(Species == "Vascular Plants")


summary_df <- data_nsw %>%
  group_by(Region, Extent) %>%
  summarise(
    n_species = n_distinct(species),
    .groups = "drop"
  ) %>%
  group_by(Region) %>%
  mutate(
    pct_n_species = 100 * n_species / sum(n_species, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(Extent = factor(Extent, levels = c(32, 64, 128)))

region_levels <- c("AWT","NZ", "SA")#,"NSW","NZ","SA","SWI")

plot_df <- summary_df %>%
  mutate(
    Region = factor(Region, levels = region_levels),
    Extent = factor(Extent, levels = c(32, 64, 128))  # order in legend + stacking
  )

p<- ggplot(plot_df, aes(x = Region, y = pct_n_species, fill = Extent)) +
  geom_col(
    width = 0.75,
    color = NA,
    position = position_stack(reverse = TRUE)   # remove this if you don't want reversed stacking
  ) +
  scale_fill_manual(
    name   = "Extent",
    values = c(`32` = "#E6F7F5", `64` = "#7FCDBB", `128` = "#006D6F"),
    breaks = c(32, 64, 128),
    labels = c("32x32", "64x64", "128x128")
  ) +
  labs(x = "Region", y = "Percent of species (%)") +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    theme(text = element_text(size = 14)),
    axis.text.x = element_text( hjust = 1),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

ggsave("Fig_optimal_extent_bubble_Vplants.png", p,
       width = 108, height = 80, units = "mm",
       dpi = 600)

