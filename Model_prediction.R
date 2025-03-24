
# Load necessary libraries

library(raster)
library(myspatial)
library(disdat)
library(parallel)
library(doParallel)

# Set environment and directory
Sys.setenv(CUDA_VISIBLE_DEVICES = "0") # this is GPU ID (check yours before running this)
options(stringsAsFactors = FALSE)
use_condaenv("C:\\Program Files\\ArcGIS\\Pro\\bin\\Python\\envs\\arcgispro-py3")

#load the saved model
model <- load_model_hdf5("F:\\akash\\aa_PhD\\models_output\\cnn1\\nz30_CNN_VGG16_5Augs32_batch4_model_withnorm_0.1to1_na_0.h5")

# From here on the code is for making maps using model you trained above

# load the raster files
#li <- list.files("F:\\akash\\aa_PhD\\Elith et al 2006\\data\\data\\Environment\\NZ\\covars", pattern = ".tif$", full.names = TRUE)
#r <- stack(li)
#normalize the data 
#r <- normalize_raster(r)
r <- brick("X:\\aanand37\\PhD\\modelling_codes\\Akash CNN codes\\CNN\\nz30\\NZ_env_data_withnorm_0.1to1_na_0.tif")
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
