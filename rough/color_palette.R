# Load required libraries
library(raster)
library(RColorBrewer)

# Read the raster file (replace with your file path)
r <- raster("X:\\akash\\aa_PhD\\Elith et al 2006\\papers\\2021 paper with new models and codes\\predicted rasters\\CNN_32x32_final_mosai1c.tif")

# Remove NA values for clean plotting
r_values <- getValues(r)
r_values <- r_values[!is.na(r_values)]


# Approximate Shadow to Sunshine palette
shadow_sunshine_palette <- c("#035585", "#46AEA2", "#8BCF6F", "#FEE08B")
shadow_sunshine_gradient <- colorRampPalette(shadow_sunshine_palette)(512)
# 
# ggplot(data.frame(r_values), aes(x = r_values)) +
#   geom_histogram(
#     aes(y = after_stat(count), fill = after_stat(x)),
#     bins = 300,
#     color = NA,
#     alpha = 1
#   ) +
#   scale_fill_gradientn(
#     colors = shadow_sunshine_gradient,
#     name = "Bin Value",
#     guide = guide_colorbar(
#       direction = "horizontal",
#       barheight = unit(3, "mm"),
#       barwidth = unit(100, "mm")
#     )
#   ) +
#   labs(
#     x = NULL,
#     y = NULL,
#     title = NULL
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.y = element_blank(),             # Remove y-axis text
#     axis.title.y = element_blank(),            # Remove y-axis label
#     axis.text.x = element_text(size = 18),
#     axis.line = element_line(color = "black"), # Add both axis lines
#     panel.grid = element_blank(),              # Remove gridlines
#     legend.position = "none"                   # Remove legend
#   )

######## Export

jpeg("X:/akash/aa_PhD/Elith et al 2006/papers/2021 paper with new models and codes/predicted rasters/output_maps/PNAS/CNN_hist.jpg",
     width = 100, height = 80, quality = 1000)


ggplot(data.frame(r_values), aes(x = r_values)) +
  geom_histogram(
    aes(y = after_stat(count), fill = after_stat(x)),
    bins = 87,
    color = NA,
    alpha = 1
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
    x = NULL,
    y = NULL,
    title = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 10, face = "bold"),
    axis.line = element_line(color = "black"),
    panel.grid = element_blank(),
    legend.position = "none"
  )

dev.off()

####### Export CNN


jpeg("X:/akash/aa_PhD/Elith et al 2006/papers/2021 paper with new models and codes/predicted rasters/output_maps/PNAS/CNN_hist.jpg",
     width = 500, height = 400, quality = 600)


ggplot(data.frame(r_values), aes(x = r_values)) +
  geom_histogram(
    aes(y = after_stat(count), fill = after_stat(x)),
    bins = 300,
    color = NA,
    alpha = 1
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
    x = NULL,
    y = NULL,
    title = NULL
  ) +
  coord_cartesian(ylim = c(0, 330000)) +  # Limit y-axis here
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 50, face = "bold"),
    axis.line = element_line(color = "black"),
    panel.grid = element_blank(),
    legend.position = "none"
  )

dev.off()
