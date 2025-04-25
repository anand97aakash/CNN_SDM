# 05_utilities.R
# ----------------
# This script defines reusable utility functions for preprocessing, normalization,
# raster padding, and chip validation used across the SDM CNN pipeline.

library(raster)
library(abind)

# ------------------ NORMALIZATION ------------------

# Normalize each raster layer to range 0.01–1
normalize_raster <- function(raster_stack) {
  if (!inherits(raster_stack, "RasterStack")) {
    stop("Input must be a RasterStack.")
  }

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

# ------------------ PADDING ------------------

# Pad a single raster layer
pad_raster_layer <- function(layer, rows, cols, pad_val = 0) {
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
pad_raster_stack <- function(stack, chip_size, pad_val = 0) {
  padded_stack <- stack()
  for (i in 1:nlayers(stack)) {
    padded_layer <- pad_raster_layer(stack[[i]], chip_size, chip_size, pad_val)
    padded_stack <- addLayer(padded_stack, padded_layer)
  }
  return(padded_stack)
}

# ------------------ CHIP VALIDATION ------------------

# Validate chip dimensions and drop invalid entries
filter_valid_chips <- function(chip_list, chip_size, bands) {
  valid_list <- chip_list[sapply(chip_list, function(arr) {
    all(dim(arr) == c(chip_size, chip_size, bands))
  })]
  return(valid_list)
}

# ------------------ CHIP EXTRACTION ------------------

# Extract chip as 3D array from raster stack
extract_chip <- function(raster_stack, x, y, chip_size) {
  bbox <- extent(x - chip_size / 2 * res(raster_stack)[1],
                 x + chip_size / 2 * res(raster_stack)[1],
                 y - chip_size / 2 * res(raster_stack)[2],
                 y + chip_size / 2 * res(raster_stack)[2])
  chip <- crop(raster_stack, bbox)

  if (!is.null(chip) && all(dim(chip)[1:2] == chip_size)) {
    chip_array <- array(getValues(chip), dim = c(chip_size, chip_size, nlayers(raster_stack)))
    return(chip_array)
  } else {
    return(NULL)
  }
}

# ------------------ CHIP TO TENSOR ------------------

# Convert list of chips to 4D tensor [samples, height, width, channels]
chips_to_tensor <- function(chip_list) {
  abind::abind(chip_list, along = 0)
}
