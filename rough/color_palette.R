# Load required libraries
library(raster)
library(RColorBrewer)

# Read the raster file (replace with your file path)
r <- raster("X:\\akash\\aa_PhD\\Elith et al 2006\\papers\\2021 paper with new models and codes\\predicted rasters\\Ensemble.tif")

# Remove NA values for clean plotting
r_values <- getValues(r)
r_values <- r_values[!is.na(r_values)]


# Approximate Shadow to Sunshine palette
shadow_sunshine_palette <- c("#035585", "#46AEA2", "#8BCF6F", "#FEE08B")
shadow_sunshine_gradient <- colorRampPalette(shadow_sunshine_palette)(512)

ggplot(data.frame(r_values), aes(x = r_values)) +
  geom_histogram(
    aes(y = after_stat(count), fill = after_stat(x)),
    bins = 300,
    color = NA,
    alpha = 0.8
  ) +
  scale_fill_gradientn(
    colors = shadow_sunshine_gradient,
    name = "Bin Value",
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(3, "mm"),
      barwidth = unit(100, "mm")
    )
  ) +
  labs(
    title = "Histogram: Fill Color Represents Bin Midpoints",
    x = "Raster Value",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title.align = 0.5
  )
