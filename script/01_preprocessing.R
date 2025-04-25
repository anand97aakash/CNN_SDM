# 01_preprocessing.R
# ------------------
# This script loads environmental rasters, normalizes them, and applies padding.
# Outputs a normalized, padded raster stack ready for chip extraction and modeling.

# Load required libraries
library(raster)
library(abind)
library(stringr)

# ------------------ CONFIGURATION ------------------

# Set working region and paths
region <- "NZ"
input_raster_dir <- file.path("data", "raw", region, "covars")
output_raster_path <- file.path("data", "processed", paste0(region, "_env_data_normalized_padded.tif"))

# Chip size (used for padding)
chip_size <- 32
chip <- 32
pad_value <- 0  # Fill value for padding

# ------------------ FUNCTIONS ------------------

# Normalize each layer of the raster stack to 0.01–1 range
normalize_raster <- function(raster_stack) {
  normalized_stack <- stack()
  for (i in 1:nlayers(raster_stack)) {
    layer <- raster_stack[[i]]
    values_layer <- getValues(layer)
    min_val <- min(values_layer, na.rm = TRUE)
    max_val <- max(values_layer, na.rm = TRUE)
    norm_vals <- 0.01 + (values_layer - min_val) * (1 - 0.01) / (max_val - min_val)
    values(layer) <- norm_vals
    normalized_stack <- addLayer(normalized_stack, layer)
  }
  names(normalized_stack) <- paste0("Band_", 1:nlayers(raster_stack))
  return(normalized_stack)
}

# Pad a single raster layer
pad_raster_layer <- function(layer, rows, cols, pad_val) {
  padded <- raster(nrow = nrow(layer) + 2 * rows,
                   ncol = ncol(layer) + 2 * cols,
                   crs = crs(layer))
  extent(padded) <- extent(layer) + c(-cols * res(layer)[1], cols * res(layer)[1],
                                      -rows * res(layer)[2], rows * res(layer)[2])
  values(padded) <- pad_val
  padded[(rows + 1):(rows + nrow(layer)), (cols + 1):(cols + ncol(layer))] <- layer[]
  return(padded)
}

# Pad each layer in a raster stack
pad_raster_stack <- function(stack, chip_size, pad_val) {
  padded_stack <- stack()
  for (i in 1:nlayers(stack)) {
    padded_layer <- pad_raster_layer(stack[[i]], chip_size, chip_size, pad_val)
    padded_stack <- addLayer(padded_stack, padded_layer)
  }
  return(padded_stack)
}

# ------------------ MAIN SCRIPT ------------------

# Create output directory if it doesn't exist
dir.create(dirname(output_raster_path), showWarnings = FALSE, recursive = TRUE)

# Load raster files
raster_files <- list.files(input_raster_dir, pattern = "\\.tif$", full.names = TRUE)
r_stack <- stack(raster_files)

# Normalize raster stack
r_stack_norm <- normalize_raster(r_stack)

# Pad raster stack
r_stack_padded <- pad_raster_stack(r_stack_norm, chip_size, pad_value)

# Save output raster
writeRaster(r_stack_padded, filename = output_raster_path, format = "GTiff", overwrite = TRUE)

cat("✅ Preprocessing complete. Output saved to:\n", output_raster_path, "\n")
