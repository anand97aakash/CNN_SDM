# 03_train_model.R
# ----------------
# This script trains a CNN model for species distribution modeling using a selected architecture.

# Load required libraries
library(keras)
library(caret)
library(abind)

# Load model architectures
source("scripts/06_cnn_architectures.R")

# ------------------ CONFIGURATION ------------------

region <- "NZ"
chip_size <- 32
chip <- 32
bands <- 12
architecture <- "vgg16"   # Choose from: "CNN_MS", "vgg16", "vgg19"

batch_size <- 16
epochs <- 100
validation_split <- 0.2

input_chip_file <- file.path("data", "processed", paste0("chips_", region, ".RData"))
model_output_file <- file.path("models", paste0(region, "_", toupper(architecture), "_model.h5"))

# ------------------ LOAD DATA ------------------

cat("Loading chip data...\n")
load(input_chip_file)  # Loads final_chip_array and valid_points

# Check for labels
if (!"occ" %in% colnames(valid_points)) {
  stop("Column 'occ' (presence/absence labels) is missing in valid_points.")
}

labels <- as.numeric(valid_points$occ)
labels <- rep(labels, each = (dim(final_chip_array)[1] / length(labels)))  # match to augmented samples

# Replace NAs and shuffle
final_chip_array[is.na(final_chip_array)] <- 0
set.seed(123)
indices <- sample(1:length(labels))
train_idx <- createDataPartition(labels[indices], p = 1 - validation_split, list = FALSE)

x_train <- final_chip_array[indices[train_idx],,,,drop = FALSE]
y_train <- labels[indices[train_idx]]
x_val <- final_chip_array[indices[-train_idx],,,,drop = FALSE]
y_val <- labels[indices[-train_idx]]

cat("Training samples:", dim(x_train)[1], "| Validation samples:", dim(x_val)[1], "\n")

# ------------------ BUILD & COMPILE MODEL ------------------

cat("Building CNN model using architecture:", architecture, "\n")
input_shape <- c(chip_size, chip_size, bands)
model <- get_cnn_model(type = architecture, input_shape = input_shape)

model %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_adam(learning_rate = 0.001),
  metrics = c("accuracy")
)

# ------------------ TRAINING ------------------

cat("Training started...\n")

history <- model %>% fit(
  x = x_train, y = y_train,
  batch_size = batch_size,
  epochs = epochs,
  validation_data = list(x_val, y_val),
  callbacks = list(
    callback_early_stopping(patience = 10, monitor = "val_loss"),
    callback_reduce_lr_on_plateau(patience = 5, factor = 0.2)
  )
)

cat("Training complete.\n")

# ------------------ SAVE MODEL ------------------

dir.create("models", showWarnings = FALSE, recursive = TRUE)
save_model_hdf5(model, model_output_file)
cat("Model saved at:", model_output_file, "\n")
