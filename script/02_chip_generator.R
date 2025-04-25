# 02_chip_generator.R
# --------------------
# This script generates square raster chips around occurrence points.
# It also performs image-based data augmentation using Keras if specified.

# Load libraries
library(raster)
library(abind)
library(keras)

# ------------------ CONFIGURATION ------------------

region <- "NZ"
chip_size <- 32
pad <- chip_size  # already applied during preprocessing
bands <- 12        # number of raster bands; auto-detectable from raster if needed
n_aug <- 5         # Number of augmentations per image

input_raster_path <- file.path("data", "processed", paste0(region, "_env_data_normalized_padded.tif"))
occurrence_file <- file.path("data", "raw", region, "occurrence.csv")  # CSV must have x, y columns
output_chip_path <- file.path("data", "processed", paste0("chips_", region, ".RData"))

# ------------------ FUNCTIONS ------------------

# Extract chips from raster
extract_chips <- function(raster_stack, points_df, chip_size, bands) {
  chip_list <- list()
  valid_ids <- c()

  for (i in 1:nrow(points_df)) {
    lon <- points_df[i, "x"]
    lat <- points_df[i, "y"]
    bbox <- extent(lon - chip_size / 2 * res(raster_stack)[1],
                   lon + chip_size / 2 * res(raster_stack)[1],
                   lat - chip_size / 2 * res(raster_stack)[2],
                   lat + chip_size / 2 * res(raster_stack)[2])
    chip <- crop(raster_stack, bbox)

    if (!is.null(chip) && all(dim(chip)[1:2] == chip_size)) {
      chip_array <- array(getValues(chip), dim = c(chip_size, chip_size, bands))
      chip_list[[length(chip_list) + 1]] <- chip_array
      valid_ids <- c(valid_ids, i)
    }
  }

  chip_array_out <- abind::abind(chip_list, along = 0)
  return(list(chips = chip_array_out, indices = valid_ids))
}

# Apply data augmentation to chips using Keras
augment_chips <- function(chip_array, n_aug, generator) {
  num_samples <- dim(chip_array)[1]
  img_height <- dim(chip_array)[2]
  img_width <- dim(chip_array)[3]
  num_channels <- dim(chip_array)[4]

  augmented_images <- array(0, dim = c(num_samples * n_aug, img_height, img_width, num_channels))
  idx <- 1

  for (i in 1:num_samples) {
    for (j in 1:n_aug) {
      augmented_sample <- array(0, dim = c(1, img_height, img_width, num_channels))
      input_data <- array(chip_array[i,,,], dim = c(1, img_height, img_width, num_channels))

      for (ch in 1:num_channels) {
        aug_gen <- flow_images_from_data(input_data[,,,ch, drop = FALSE], generator, batch_size = 1)
        augmented <- generator_next(aug_gen)
        augmented_sample[,,,ch] <- augmented[1,,,1]
      }

      augmented_images[idx,,,] <- augmented_sample[1,,,]
      idx <- idx + 1
    }
  }

  return(augmented_images)
}

# ------------------ MAIN SCRIPT ------------------

# Load raster and point data
r_stack <- brick(input_raster_path)
points <- read.csv(occurrence_file)

# Extract chips
cat("🔍 Extracting chips...\n")
chip_data <- extract_chips(r_stack, points, chip_size, nlayers(r_stack))
chip_array <- chip_data$chips
valid_points <- points[chip_data$indices, ]

cat("Extracted", dim(chip_array)[1], "valid chips.\n")

# Create data augmentation generator
cat("Applying data augmentation...\n")
img_gen <- image_data_generator(
  featurewise_center = FALSE,
  featurewise_std_normalization = FALSE,
  rotation_range = 180,
  width_shift_range = 0.2,
  height_shift_range = 0.2,
  zoom_range = 0.2,
  horizontal_flip = TRUE,
  vertical_flip = TRUE,
  fill_mode = "reflect"
)

# Apply augmentation
augmented_chips <- augment_chips(chip_array, n_aug, img_gen)

# Combine original and augmented
final_chip_array <- abind::abind(chip_array, augmented_chips, along = 1)

# Save chip array and labels
save(final_chip_array, valid_points, file = output_chip_path)
cat("Chips saved to:", output_chip_path, "\n")
