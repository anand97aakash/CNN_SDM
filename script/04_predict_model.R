# 04_predict_model.R
# -------------------
# This script loads a trained CNN model and uses it to make predictions
# across a normalized and padded raster using sliding window chips.

# Load libraries
library(raster)
library(keras)
library(abind)

# ------------------ CONFIGURATION ------------------

region <- "NZ"
chip_size <- 32
chip <- 32
bands <- 12
batch_size <- 512

input_raster_path <- file.path("data", "processed", paste0(region, "_env_data_normalized_padded.tif"))
model_path <- file.path("models", paste0(region, "_CNN_VGG16_model.h5"))
prediction_output_path <- file.path("predictions", paste0("prediction_", region, ".tif"))

# ------------------ HELPER FUNCTION ------------------

generate_prediction_chips <- function(raster_stack, chip_size) {
  nrows <- nrow(raster_stack)
  ncols <- ncol(raster_stack)
  chips <- list()
  coords <- list()

  for (row in 1:(nrows - chip_size + 1)) {
    for (col in 1:(ncols - chip_size + 1)) {
      chip_extent <- extent(
        xmin(raster_stack) + (col - 1) * res(raster_stack)[1],
        xmin(raster_stack) + (col - 1 + chip_size) * res(raster_stack)[1],
        ymax(raster_stack) - (row - 1 + chip_size) * res(raster_stack)[2],
        ymax(raster_stack) - (row - 1) * res(raster_stack)[2]
      )

      chip <- crop(raster_stack, chip_extent)

      if (all(dim(chip)[1:2] == chip_size)) {
        chip_array <- array(getValues(chip), dim = c(chip_size, chip_size, nlayers(raster_stack)))
        chips[[length(chips) + 1]] <- chip_array
        coords[[length(coords) + 1]] <- c(row, col)
      }
    }
  }

  chip_tensor <- abind::abind(chips, along = 0)
  return(list(tensor = chip_tensor, positions = coords))
}

# ------------------ LOAD MODEL AND RASTER ------------------

cat("Loading model and raster...\n")
model <- load_model_hdf5(model_path)
r_stack <- brick(input_raster_path)
r_stack[is.na(r_stack)] <- 0

# ------------------ SLIDE & PREDICT ------------------

cat("Generating chips for prediction...\n")
chip_data <- generate_prediction_chips(r_stack, chip_size)
chip_tensor <- chip_data$tensor
positions <- chip_data$positions

cat("Running predictions...\n")
preds <- numeric(length = dim(chip_tensor)[1])

for (i in seq(1, length(preds), by = batch_size)) {
  batch_end <- min(i + batch_size - 1, length(preds))
  batch <- chip_tensor[i:batch_end,,,, drop = FALSE]
  batch[is.na(batch)] <- 0
  preds[i:batch_end] <- predict(model, batch)
}

# ------------------ RECONSTRUCT RASTER ------------------

cat("Reconstructing prediction raster...\n")
pred_raster <- raster(r_stack)
pred_raster[] <- NA

# Create a blank matrix to fill with predictions
pred_matrix <- matrix(NA, nrow = nrow(r_stack), ncol = ncol(r_stack))

for (i in seq_along(positions)) {
  row <- positions[[i]][1]
  col <- positions[[i]][2]
  center_row <- row + floor(chip_size / 2)
  center_col <- col + floor(chip_size / 2)
  pred_matrix[center_row, center_col] <- preds[i]
}

pred_raster <- setValues(pred_raster, as.vector(pred_matrix))
crs(pred_raster) <- crs(r_stack)
extent(pred_raster) <- extent(r_stack)

# ------------------ SAVE OUTPUT ------------------

dir.create("predictions", showWarnings = FALSE, recursive = TRUE)
writeRaster(pred_raster, prediction_output_path, format = "GTiff", overwrite = TRUE)
cat("Prediction raster saved to:", prediction_output_path, "\n")

# ------------------ OPTIONAL VISUALIZATION ------------------

# library(ggplot2)
# plot(pred_raster, main = "CNN Prediction")
