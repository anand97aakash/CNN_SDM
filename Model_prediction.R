
# Load necessary libraries
library(keras)
library(raster)
library(caret)
library(abind)
library(disdat)
library(randomForest)
library(forcats)

# Set environment and directory
Sys.setenv(CUDA_VISIBLE_DEVICES = "0")
options(stringsAsFactors = FALSE)
use_condaenv("C:\\Program Files\\ArcGIS\\Pro\\bin\\Python\\envs\\arcgispro-py3")

# Create output directory if it doesn’t exist
outdir <- "F:\\akash\\aa_PhD\\Elith et al 2006\\papers\\2021 paper with new models and codes\\models_output\\cnn1"
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# Define parameters
regions <- "NZ" #choosing one of the six region from Elith et al (2006)
categoricalvars <- c("ontveg", "vegsys", "toxicats", "age", "calc")
covars <- list(
  AWT = c("bc04", "bc05", "bc06", "bc12", "bc15", "slope", "topo", "tri"),
  CAN = c("alt", "asp2", "ontprec", "ontslp", "onttemp", "ontveg", "watdist"),
  NSW = c("cti", "disturb", "mi", "rainann", "raindq", "rugged", "soildepth", "soilfert", "solrad", "tempann", "topo", "vegsys"),
  NZ = c("age", "deficit", "hillshade", "mas", "mat", "r2pet", "slope", "sseas", "toxicats", "tseas", "vpd"),
  SA = c("sabio12", "sabio15", "sabio17", "sabio18", "sabio2", "sabio4", "sabio5", "sabio6"),
  SWI = c("bcc", "calc", "ccc", "ddeg", "nutri", "pday", "precyy", "sfroyy", "slope", "sradyy", "swb", "topo")
)
batch_sizes <- c(4, 8, 16)
chip_sizes <- c(32, 64, 128)
n_aug <- 5

# Function to normalize a raster stack
normalize_raster <- function(raster_stack) {
  if (!inherits(raster_stack, "RasterStack")) {
    stop("Input must be a RasterStack.")
  }
  
  num_bands <- nlayers(raster_stack)
  normalized_raster_stack <- stack()
  
  for (i in 1:num_bands) {
    band_values <- getValues(raster_stack[[i]])
    
    # Get the min and max values of the band
    min_value <- min(band_values, na.rm = TRUE)
    max_value <- max(band_values, na.rm = TRUE)
    
    # Normalize values to range between 0.01 and 1
    normalized_values <- 0.01 + (band_values - min_value) * (1 - 0.01) / (max_value - min_value)
    
    # Replace values in the original raster object with normalized values
    raster_object_normalized <- raster_stack[[i]]
    values(raster_object_normalized) <- normalized_values
    
    # Add the normalized raster layer to the stack
    normalized_raster_stack <- addLayer(normalized_raster_stack, raster_object_normalized)
  }
  
  names(normalized_raster_stack) <- paste0("Band_", 1:num_bands)
  return(normalized_raster_stack)
}

pad_raster_layer <- function(raster_layer, extra_rows, extra_cols, extra_value) {
  nrows <- nrow(raster_layer)
  ncols <- ncol(raster_layer)
  larger_raster_layer <- raster(nrow = nrows + 2 * extra_rows, ncol = ncols + 2 * extra_cols, crs = crs(raster_layer))
  extent(larger_raster_layer) <- extent(raster_layer) + c(-extra_cols * res(raster_layer)[1], extra_cols * res(raster_layer)[1], -extra_rows * res(raster_layer)[2], extra_rows * res(raster_layer)[2])
  values(larger_raster_layer) <- extra_value
  subset_raster <- raster_layer[]
  larger_raster_layer[(extra_rows + 1):(extra_rows + nrows), (extra_cols + 1):(extra_cols + ncols)] <- subset_raster
  return(larger_raster_layer)
}

# # Define a function for data augmentation
# augment_channels <- function(presence_chips, num_augmentations, img_gen) {
#   augmented_images <- array(0, dim = c(dim(presence_chips)[1] * num_augmentations, dim(presence_chips)[2:4]))
#   idx <- 1
#   for (sample_idx in 1:dim(presence_chips)[1]) {
#     input_data <- array(presence_chips[sample_idx, , , , drop = FALSE], dim = c(1, dim(presence_chips)[2:4]))
#     for (aug in 1:num_augmentations) {
#       for (ch in 1:dim(input_data)[4]) {
#         channels_gen <- flow_images_from_data(input_data[,,,ch, drop = FALSE], img_gen, batch_size = 1)
#         augmented_images[idx,,,ch] <- generator_next(channels_gen)[1,,,1]
#       }
#       idx <- idx + 1
#     }
#   }
#   return(augmented_images)
# }
# 
# # Set up for training
# set.seed(40)
# datagen <- image_data_generator(
#   featurewise_center = FALSE, featurewise_std_normalization = FALSE,
#   rotation_range = 180, width_shift_range = 0.2, height_shift_range = 0.2,
#   zoom_range = 0.2, horizontal_flip = TRUE, vertical_flip = TRUE, fill_mode = "reflect"
# )

# Training Loop
set.seed(40)  # Set a global seed for reproducibility

for (n_aug in n_aug) {
  for (batch in batch_sizes) {
    for (chip in chip_sizes) {
      n <- 0
      for (r in regions) {
        presences <- disPo(r)
        background <- read.csv(paste0("X:\\aanand37\\PhD\\Elith et al 2006\\papers\\2021 paper with new models and codes\\ecm1486-sup-0003-datas1\\DataS1\\background_50k\\", r, ".csv"))
        raster_dir <- paste0("F:\\akash\\aa_PhD\\Elith et al 2006\\data\\data\\Environment\\", r, "\\covars")
        raster_files <- list.files(raster_dir, pattern = "*.tif$", full.names = TRUE)
        raster_stack <- stack(raster_files)
        
        extent_stack <- extent(raster_stack)
        new_raster <- raster(extent_stack, res = res(raster_stack), crs = crs(raster_stack))
        filled_raster_stack <- stack(lapply(1:nlayers(raster_stack), function(i) setValues(new_raster, 0)))
        
        for (i in 1:nlayers(raster_stack)) {
          layer_values <- raster_stack[[i]]
          filled_raster_stack[[i]] <- layer_values
        }
        
        #filled_raster_stack <- reclassify(filled_raster_stack, cbind(NA, 0))
        extra_rows <- chip
        extra_cols <- chip
        extra_value <- 0
        
        larger_raster_stack <- stack(lapply(1:nlayers(filled_raster_stack), function(i) {
          pad_raster_layer(filled_raster_stack[[i]], extra_rows, extra_cols, extra_value)
        }))
        
        # replace_na_with_zero <- function(x) {
        #   x[is.na(x)] <- 0
        #   return(x)
        # }
        
        larger_raster_stack_norm <- normalize_raster(larger_raster_stack)
        raster_stack_zero_na <- reclassify(larger_raster_stack_norm, cbind(NA, 0))
        #raster_stack_zero_na <- calc(larger_raster_stack_norm, replace_na_with_zero)
        
        #raster_stack_zero_na_stack <- stack(raster_stack_zero_na)
        env_data <- raster_stack_zero_na #normalize_raster(raster_stack_zero_na_stack)
        
        chip_size <- chip * res(env_data)[1]
        bands <- nlayers(env_data)
        desired_dimensions <- c(chip, chip, bands)
        
        for (grp in unique(presences$group)) {
          evaluation <- disEnv(r)			#, grp)
          presence_subset <- presences[presences$group == grp, ]
          
          for (i in 1:ncol(presences)) {
            if (colnames(presences)[i] %in% categoricalvars) {
              fac_col <- colnames(presences)[i]
              presences[, fac_col] <- as.factor(presences[, fac_col])
              evaluation[, fac_col] <- as.factor(evaluation[, fac_col])
              evaluation[, fac_col] <- forcats::fct_expand(evaluation[, fac_col], levels(presences[, fac_col]))
              evaluation[, fac_col] <- forcats::fct_relevel(evaluation[, fac_col], levels(presences[, fac_col]))
            }
          }
          
          chip_list_eval_out <- list()
          
          for (i in 1:nrow(evaluation)) {
            lon <- evaluation[i, "x"]
            lat <- evaluation[i, "y"]
            bbox <- extent(lon - chip_size / 2, lon + chip_size / 2, lat - chip_size / 2, lat + chip_size / 2)
            chip_eval <- crop(env_data, bbox)
            chip_array_eval <- array(chip_eval, dim = c(dim(chip_eval)[1], dim(chip_eval)[2], dim(chip_eval)[3]))
            
            if (all(dim(chip_array_eval) == desired_dimensions)) {
              chip_list_eval_out[[i]] <- chip_array_eval
            } else {
              print(paste("Skipping chip", i, "due to mismatched dimensions"))
            }
          }
          
          filtered_chip_list_eval <- chip_list_eval_out[sapply(chip_list_eval_out, function(arr) all(dim(arr) == c(chip, chip, bands)))]
          pr_bg_chips_eval <- abind(filtered_chip_list_eval, along = 4)
          pr_bg_chips_eval <- aperm(pr_bg_chips_eval, c(4, 1, 2, 3))
          pr_bg_chips_eval[is.na(pr_bg_chips_eval)] <- 0
          
          #species <- unique(presence_subset$spid)
          # Remove "nsw30" if it exists in the list as per elith paper
          #species <- setdiff(species, c("swi02","swi06","swi08","swi10","swi13","swi14","swi28","swi30"))
          species <- "nz30"
          #species <- setdiff(species, c("swi01","swi03","swi04","swi05","swi06","swi07","swi09","swi11","swi12","swi15","swi16","swi17","swi18","swi19",
          #     "swi20","swi21","swi22","swi23","swi24","swi25","swi26","swi27","swi29"))
          
          
          for (s in species) {
            n <- n + 1
            set.seed(sum(presences$occ) + 300 + n)  # Seed set before species loop
            sp_presence <- presences[presences$spid == s, ]
            
            chip_list <- list()
            label_vector <- numeric()
            
            for (i in 1:nrow(sp_presence)) {
              lon <- sp_presence[i, "x"]
              lat <- sp_presence[i, "y"]
              label <- sp_presence[i, "occ"]
              bbox <- extent(lon - chip_size / 2, lon + chip_size / 2, lat - chip_size / 2, lat + chip_size / 2)
              chip_raster <- crop(env_data, bbox)
              chip_array <- array(chip_raster, dim = c(dim(chip_raster)[1], dim(chip_raster)[2], dim(chip_raster)[3]))
              
              if (all(dim(chip_array) == desired_dimensions)) {
                chip_list[[i]] <- chip_array
                label_vector <- c(label_vector, label)
              } else {
                print(paste("Skipping chip", i, "due to mismatched dimensions"))
              }
            }
            
            filtered_chip_list <- chip_list[sapply(chip_list, function(arr) all(dim(arr) == c(chip, chip, bands)))]
            presence_chips <- abind(filtered_chip_list, along = 4)
            presence_chips <- aperm(presence_chips, c(4, 1, 2, 3))
            presence_chips[is.na(presence_chips)] <- 0
            
            # Data Augmentation
            datagen_hh_flip <- image_data_generator(
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
            
            augment_channels <- function(presence_chips, num_augmentations = n_aug) {
              num_samples <- dim(presence_chips)[1]
              img_height <- dim(presence_chips)[2]
              img_width <- dim(presence_chips)[3]
              num_channels <- dim(presence_chips)[4]
              
              augmented_images <- array(0, dim = c(num_samples * num_augmentations, img_height, img_width, num_channels))
              aug_idx <- 1
              
              for (sample_idx in 1:num_samples) {
                for (aug in 1:num_augmentations) {
                  augmented_sample <- array(0, dim = c(1, img_height, img_width, num_channels))
                  input_data <- array(0, dim = c(1, img_height, img_width, num_channels))
                  input_data[1,,,] <- presence_chips[sample_idx,,,]
                  
                  for (channel in 1:num_channels) {
                    channels_gen <- flow_images_from_data(
                      x = input_data[,,,channel, drop = FALSE],
                      generator = datagen_hh_flip,
                      batch_size = 1
                    )
                    augmented_channels <- generator_next(channels_gen)
                    augmented_sample[,,,channel] <- augmented_channels[1,,,1]
                  }
                  
                  augmented_images[aug_idx,,,] <- augmented_sample[1,,,]
                  aug_idx <- aug_idx + 1
                }
              }
              return(augmented_images)
            }
            
            augmented_presence_chips <- augment_channels(presence_chips)
            
            presence_array <- abind(augmented_presence_chips, presence_chips, along = 1)
            
            num_presence_records <- nrow(presence_array)
            selected_background <- background[sample(nrow(background), num_presence_records), ]
            
            chip_list <- list()
            label_vector <- numeric()
            
            for (i in 1:nrow(selected_background)) {
              lon <- selected_background[i, "x"]
              lat <- selected_background[i, "y"]
              label <- selected_background[i, "occ"]
              bbox <- extent(lon - chip_size / 2, lon + chip_size / 2, lat - chip_size / 2, lat + chip_size / 2)
              chip_raster <- crop(env_data, bbox)
              chip_array <- array(chip_raster, dim = c(dim(chip_raster)[1], dim(chip_raster)[2], dim(chip_raster)[3]))
              
              if (all(dim(chip_array) == desired_dimensions)) {
                chip_list[[i]] <- chip_array
                label_vector <- c(label_vector, label)
              } else {
                print(paste("Skipping chip", i, "due to mismatched dimensions"))
              }
            }
            
            filtered_chip_list_background <- chip_list[sapply(chip_list, function(arr) all(dim(arr) == c(chip, chip, bands)))]
            background_chips <- abind(filtered_chip_list_background, along = 4)
            background_chips <- aperm(background_chips, c(4, 1, 2, 3))
            background_chips[is.na(background_chips)] <- 0
            
            combined_arrays <- abind(presence_array, background_chips, along = 1)
            labels <- c(rep(1, nrow(presence_array)), rep(0, nrow(background_chips)))
            set.seed(123)
            perm_indices <- sample(length(labels))
            full_data <- combined_arrays[perm_indices, , , ]
            full_labels <- labels[perm_indices]
            train_indices <- createDataPartition(full_labels, p = 0.8, list = FALSE)
            train_raster_array <- full_data[train_indices, , , , drop = FALSE]
            train_label_vector <- full_labels[train_indices]
            test_raster_array <- full_data[-train_indices, , , , drop = FALSE]
            test_label_vector <- full_labels[-train_indices]
            
            # Define VGG-16 architecture
            # model <- keras_model_sequential() %>%
            #   layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu", padding = "same", input_shape = c(chip, chip, bands), kernel_regularizer = regularizer_l2(0.00001)) %>%
            #   layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            #   layer_batch_normalization() %>%
            #   layer_max_pooling_2d(pool_size = c(2, 2), strides = 2) %>%
            #   layer_dropout(rate = 0.25) %>%
            #   
            #   layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            #   layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            #   layer_batch_normalization() %>%
            #   layer_max_pooling_2d(pool_size = c(2, 2), strides = 2) %>%
            #   layer_dropout(rate = 0.25) %>%
            #   
            #   layer_conv_2d(filters = 256, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            #   layer_conv_2d(filters = 256, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            #   layer_conv_2d(filters = 256, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            #   layer_batch_normalization() %>%
            #   layer_max_pooling_2d(pool_size = c(2, 2), strides = 2) %>%
            #   layer_dropout(rate = 0.25) %>%
            #   
            #   layer_conv_2d(filters = 512, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            #   layer_conv_2d(filters = 512, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            #   layer_conv_2d(filters = 512, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            #   #layer_conv_2d(filters = 512, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            #   layer_batch_normalization() %>%
            #   layer_max_pooling_2d(pool_size = c(2, 2), strides = 2) %>%
            #   layer_dropout(rate = 0.25) %>%
            #   
            #   layer_conv_2d(filters = 512, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            #   layer_conv_2d(filters = 512, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            #   layer_conv_2d(filters = 512, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            #   #layer_conv_2d(filters = 512, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            #   layer_batch_normalization() %>%
            #   layer_max_pooling_2d(pool_size = c(2, 2), strides = 2) %>%
            #   layer_dropout(rate = 0.25) %>%
            #   
            #   layer_flatten() %>%
            #   layer_dense(units = 4096, activation = "relu", kernel_initializer = initializer_he_normal()) %>%
            #   layer_dropout(rate = 0.5) %>%
            #   layer_dense(units = 4096, activation = "relu", kernel_initializer = initializer_he_normal()) %>%
            #   layer_dropout(rate = 0.5) %>%
            #   layer_dense(units = 1000, activation = "relu", kernel_initializer = initializer_he_normal()) %>%
            #   layer_dense(units = 1, activation = "sigmoid")
            
            # # Define VGG-19 architecture
            # model <- keras_model_sequential() %>%
            #   layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu", padding = "same", input_shape = c(chip, chip, bands), kernel_regularizer = regularizer_l2(0.00001)) %>%
            #   layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            #   layer_batch_normalization() %>%
            #   layer_max_pooling_2d(pool_size = c(2, 2), strides = 2) %>%
            #   layer_dropout(rate = 0.25) %>%
            # 
            #   layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            #   layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            #   layer_batch_normalization() %>%
            #   layer_max_pooling_2d(pool_size = c(2, 2), strides = 2) %>%
            #   layer_dropout(rate = 0.25) %>%
            # 
            #   layer_conv_2d(filters = 256, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            #   layer_conv_2d(filters = 256, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            #   layer_conv_2d(filters = 256, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            #   layer_conv_2d(filters = 256, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            #   layer_batch_normalization() %>%
            #   layer_max_pooling_2d(pool_size = c(2, 2), strides = 2) %>%
            #   layer_dropout(rate = 0.25) %>%
            # 
            #   layer_conv_2d(filters = 512, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            #   layer_conv_2d(filters = 512, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            #   layer_conv_2d(filters = 512, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            #   layer_conv_2d(filters = 512, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            #   layer_batch_normalization() %>%
            #   layer_max_pooling_2d(pool_size = c(2, 2), strides = 2) %>%
            #   layer_dropout(rate = 0.25) %>%
            # 
            #   layer_conv_2d(filters = 512, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            #   layer_conv_2d(filters = 512, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            #   layer_conv_2d(filters = 512, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            #   layer_conv_2d(filters = 512, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            #   layer_batch_normalization() %>%
            #   layer_max_pooling_2d(pool_size = c(2, 2), strides = 2) %>%
            #   layer_dropout(rate = 0.25) %>%
            # 
            #   layer_flatten() %>%
            #   layer_dense(units = 4096, activation = "relu", kernel_initializer = initializer_he_normal()) %>%
            #   layer_dropout(rate = 0.5) %>%
            #   layer_dense(units = 4096, activation = "relu", kernel_initializer = initializer_he_normal()) %>%
            #   layer_dropout(rate = 0.5) %>%
            #   layer_dense(units = 1000, activation = "relu", kernel_initializer = initializer_he_normal()) %>%
            #   layer_dense(units = 1, activation = "sigmoid")
            
            # Define the model architecture
            model <- keras_model_sequential() %>%
              layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu", padding = "same", input_shape = c(chip, chip, bands), kernel_initializer = initializer_he_normal()) %>%
              layer_batch_normalization() %>%
              layer_max_pooling_2d(pool_size = c(2, 2), strides = 2) %>%
              layer_dropout(rate = 0.25) %>%
              
              layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
              layer_batch_normalization() %>%
              layer_max_pooling_2d(pool_size = c(2, 2), strides = 2) %>%
              layer_dropout(rate = 0.25) %>%
              
              layer_conv_2d(filters = 256, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
              layer_batch_normalization() %>%
              layer_max_pooling_2d(pool_size = c(2, 2), strides = 2) %>%
              layer_dropout(rate = 0.25) %>%
              
              layer_conv_2d(filters = 512, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
              layer_batch_normalization() %>%
              layer_max_pooling_2d(pool_size = c(2, 2), strides = 2) %>%
              layer_dropout(rate = 0.25) %>%
              
              layer_flatten() %>%
              layer_dense(units = 2048, activation = "relu", kernel_initializer = initializer_he_normal()) %>%
              layer_dropout(rate = 0.5) %>%
              layer_dense(units = 2048, activation = "relu", kernel_initializer = initializer_he_normal()) %>%
              layer_dropout(rate = 0.5) %>%
              layer_dense(units = 1000, activation = "relu", kernel_initializer = initializer_he_normal()) %>%
              layer_dense(units = 1, activation = "sigmoid")
            # 
            # Compile the model
            model %>% compile(
              loss = "binary_crossentropy",
              optimizer = optimizer_adam(learning_rate = 0.001),
              metrics = c("accuracy")
            )
            
            reduce_lr <- callback_reduce_lr_on_plateau(
              monitor = 'val_loss',
              factor = 0.2,
              patience = 5,
              min_lr = 1e-6
            )
            # Callbacks for early stopping and learning rate reduction
            early_stopping <- callback_early_stopping(monitor = "val_loss", patience = 10)
            
            ptm <- proc.time()
            # Train the model
            history <- model %>% fit(
              train_raster_array, train_label_vector,
              epochs = 100,
              batch_size = batch,
              validation_data = list(test_raster_array, test_label_vector),
              callbacks = list(reduce_lr, early_stopping)
            )
            t <- proc.time() - ptm
            
            final_lr <- k_get_value(model$optimizer$learning_rate)
            
            out_file <- evaluation[, 1:4]
            out_file$spid <- sp_presence$spid[1]
            out_file$region <- r
            out_file$model <- "CNN_VGG19"
            
            batch_size <- 50
            predictions <- numeric(nrow(pr_bg_chips_eval))
            
            for (i in seq(1, nrow(pr_bg_chips_eval), by = batch_size)) {
              indices <- i:min(i + batch_size - 1, nrow(pr_bg_chips_eval))
              current_batch <- pr_bg_chips_eval[indices, , ,]
              predictions[indices] <- as.numeric(predict(model, current_batch, device = "cpu"))
            }
            
            out_file$prediction <- predictions
            out_file$time <- t[3]
            write.csv(out_file, sprintf("%s/%s_CNN_VGG16_%sAugs%s_batch%s_with_dropout0.25.csv", outdir, s, n_aug, chip, batch), row.names = FALSE)
            print(n)
          }
        }
      }
    }
  }
}


# Save the model after training
model_file_path <- file.path(outdir, sprintf("%s_CNN_VGG16_%sAugs%s_batch%s_model_withnorm_0.1to1_na_0.h5", s, n_aug, chip, batch))
save_model_hdf5(model, model_file_path)

#load the saved model
model <- load_model_hdf5("F:\\akash\\aa_PhD\\Elith et al 2006\\papers\\2021 paper with new models and codes\\models_output\\cnn1\\nz30_CNN_VGG16_5Augs32_batch4_model_withnorm_0.1to1_na_0.h5")


# load the packages
library(raster)
library(myspatial)
library(disdat)
library(parallel)
library(doParallel)
# load the raster files
#li <- list.files("F:\\akash\\aa_PhD\\Elith et al 2006\\data\\data\\Environment\\NZ\\covars", pattern = ".tif$", full.names = TRUE)
#r <- stack(li)
#normalize the data 
#r <- normalize_raster(r)
r <- brick("X:\\aanand37\\PhD\\Elith et al 2006\\akash code edits\\modelling_codes\\Akash CNN codes\\CNN\\nz30\\NZ_env_data_withnorm_0.1to1_na_0.tif")
plot(r)
chip_size <- 32

slice_raster_parallel_foreach <- function(raster_data, chip_size,input_raster) {
  #raster_data<- r_mask
  nrows <- nrow(raster_data)
  ncols <- ncol(raster_data)
  
  # Calculate padding
  pad_rows <- 16#((chip_size-1)/2)# - 1
  pad_cols <- 16#((chip_size-1)/2)# - 1
  
  min_value <- cellStats(raster_data, stat = 'min', na.rm = TRUE)
  # Pad the raster if needed
  if (pad_rows > 0 || pad_cols > 0) {
    padded_extent <- extent(raster_data)
    
    # Adjust the extent symmetrically in all directions
    raster_data <- extend(
      raster_data,
      extent(
        xmin(padded_extent) - pad_cols * res(raster_data)[1],
        xmax(padded_extent) + pad_cols * res(raster_data)[1],
        ymin(padded_extent) - pad_rows * res(raster_data)[2],
        ymax(padded_extent) + pad_rows * res(raster_data)[2]
      ),
      value =  0#0.5  # You can change the fill value here if needed
    )
  }
  
  # Parallel processing of rows using foreach
  chips <- foreach(i = seq(1, nrow(input_raster) , by = 1), .combine = 'c', .packages = c('raster')) %dopar% {
    row_chips <- list()
    for (j in seq(1, ncol(input_raster), by = 1)) {
      crop_extent <- extent(
        xmin(raster_data) + (j - 1) * res(raster_data)[1],
        xmin(raster_data) + (j + chip_size - 1) * res(raster_data)[1],
        ymax(raster_data) - (i + chip_size - 1) * res(raster_data)[2],
        ymax(raster_data) - (i - 1) * res(raster_data)[2]
      )
      
      chip <- crop(raster_data, crop_extent)
      if (nrow(chip) == chip_size && ncol(chip) == chip_size) {
        chip_array <- round(as.array(chip), 4)
        row_chips[[length(row_chips) + 1]] <- chip_array
      }
    }
    return(row_chips)
  }
  
  return(chips)
}

library(sf)

# Load shapefile once outside the loop
shapefile_path <- "X:\\aanand37\\PhD\\Elith et al 2006\\akash code edits\\modelling_codes\\Akash CNN codes\\CNN\\nz30\\nz_100splits_polygon.shp"
shapefile <- st_read(shapefile_path)
num_polygons <- nrow(shapefile)

# Setup parallel backend
num_cores <- 50
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Main processing loop
for (n in 91:100) {
  tryCatch({
    single_polygon <- shapefile[n, ]
    shapefile_sp <- as(single_polygon, "Spatial")
    buffer_shapefile <- st_buffer(single_polygon, dist = 1600)  # Buffer of 1600 meters, my pixel size were 100 and chip size 32 therefore 100x16
    
    cropped_raster <- crop(r, extent(buffer_shapefile))
    masked_raster <- mask(cropped_raster, buffer_shapefile)
    
    r_mask <- reclassify(masked_raster, cbind(NA, 0))
    
    # Slice raster in parallel
    raster_chips <- slice_raster_parallel_foreach(r_mask, chip_size, r_mask)
    
    # Batch processing for predictions
    batch_size <- 1000
    num_chips <- length(raster_chips)
    num_batches <- ceiling(num_chips / batch_size)
    predictions <- vector("list", num_chips)
    
    for (i in 1:num_batches) {
      start_idx <- (i-1) * batch_size + 1
      end_idx <- min(i * batch_size, num_chips)
      
      batch <- array(
        data = unlist(raster_chips[start_idx:end_idx]), 
        dim = c(chip_size, chip_size, nlayers(r), end_idx - start_idx + 1)
      )
      
      batch <- aperm(batch, c(4,1,2,3))
      
      # Predict using the model
      batch_predictions <- predict(model, batch)
      
      predictions[start_idx:end_idx] <- split(batch_predictions, row(batch_predictions))
    }
    
    # Convert predictions to raster
    predictions_vector <- unlist(predictions)
    predictions_matrix <- matrix(predictions_vector, nrow = nrow(r_mask), ncol = ncol(r_mask), byrow = TRUE)
    predictions_raster <- raster(predictions_matrix)
    
    # Set extent and CRS
    extent(predictions_raster) <- extent(r_mask)
    crs(predictions_raster) <- crs(r_mask)
    final_pred <- mask(predictions_raster,single_polygon)
    # Save the raster
    writeRaster(final_pred, paste0("F:\\akash\\aa_PhD\\Elith et al 2006\\papers\\2021 paper with new models and codes\\predicted rasters\\Aug_NZ100_",n,".tif"), overwrite = TRUE)
    print(n)
  }, error = function(e) {
    message("Error with polygon ", n, ": ", e)
  })
}

# Close the parallel backend
stopCluster(cl)

# Visualize the predicted raster
plot(pred_raster)
