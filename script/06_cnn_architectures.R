# 06_cnn_architectures.R
# -----------------------
# This script defines multiple CNN architectures for modeling species distributions.
# Users can select "CNN-MS", "vgg16", or "vgg19".

library(keras)

# ------------------ CNN-MS ------------------

build_cnn_ms <- function(input_shape) {
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
              
              layer_global_average_pooling_2d() %>% #layer_flatten() %>%
              layer_dense(units = 512, activation = "relu", kernel_initializer = initializer_he_normal()) %>%
              layer_dropout(rate = 0.5) %>%
              layer_dense(units = 256, activation = "relu", kernel_initializer = initializer_he_normal()) %>%
              layer_dropout(rate = 0.5) %>%
              layer_dense(units = 1000, activation = "relu", kernel_initializer = initializer_he_normal()) %>%
              layer_dense(units = 1, activation = "sigmoid")

  return(model)
}

# ------------------ VGG-16 CNN ------------------

build_vgg16 <- function(input_shape) {
  model <- keras_model_sequential() %>%
            layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu", padding = "same", input_shape = c(chip, chip, bands), kernel_regularizer = regularizer_l2(0.00001)) %>%
            layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            layer_batch_normalization() %>%
            layer_max_pooling_2d(pool_size = c(2, 2), strides = 2) %>%
            layer_dropout(rate = 0.25) %>%
            #   
            layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            layer_batch_normalization() %>%
            layer_max_pooling_2d(pool_size = c(2, 2), strides = 2) %>%
            layer_dropout(rate = 0.25) %>%
            #   
            layer_conv_2d(filters = 256, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            layer_conv_2d(filters = 256, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            layer_conv_2d(filters = 256, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            layer_batch_normalization() %>%
            layer_max_pooling_2d(pool_size = c(2, 2), strides = 2) %>%
            layer_dropout(rate = 0.25) %>%
            #   
            layer_conv_2d(filters = 512, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            layer_conv_2d(filters = 512, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            layer_conv_2d(filters = 512, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            #layer_conv_2d(filters = 512, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            layer_batch_normalization() %>%
            layer_max_pooling_2d(pool_size = c(2, 2), strides = 2) %>%
            layer_dropout(rate = 0.25) %>%
            #   
            layer_conv_2d(filters = 512, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            layer_conv_2d(filters = 512, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            layer_conv_2d(filters = 512, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            #layer_conv_2d(filters = 512, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            layer_batch_normalization() %>%
            layer_max_pooling_2d(pool_size = c(2, 2), strides = 2) %>%
            layer_dropout(rate = 0.25) %>%
            #   
            layer_global_average_pooling_2d() %>% #layer_flatten() %>%
            layer_dense(units = 1024, activation = "relu", kernel_initializer = initializer_he_normal()) %>%
            layer_dropout(rate = 0.5) %>%
            layer_dense(units = 512, activation = "relu", kernel_initializer = initializer_he_normal()) %>%
            layer_dropout(rate = 0.5) %>%
            layer_dense(units = 256, activation = "relu", kernel_initializer = initializer_he_normal()) %>%
            layer_dense(units = 1, activation = "sigmoid")

  return(model)
}

# ------------------ VGG-19 CNN ------------------

build_vgg19 <- function(input_shape) {
  model <- keras_model_sequential() %>%
            layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu", padding = "same", input_shape = c(chip, chip, bands), kernel_regularizer = regularizer_l2(0.00001)) %>%
            layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            layer_batch_normalization() %>%
            layer_max_pooling_2d(pool_size = c(2, 2), strides = 2) %>%
            layer_dropout(rate = 0.25) %>%
            # 
            layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            layer_batch_normalization() %>%
            layer_max_pooling_2d(pool_size = c(2, 2), strides = 2) %>%
            layer_dropout(rate = 0.25) %>%
            # 
            layer_conv_2d(filters = 256, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            layer_conv_2d(filters = 256, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            layer_conv_2d(filters = 256, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            layer_conv_2d(filters = 256, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            layer_batch_normalization() %>%
            layer_max_pooling_2d(pool_size = c(2, 2), strides = 2) %>%
            layer_dropout(rate = 0.25) %>%
            # 
            layer_conv_2d(filters = 512, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            layer_conv_2d(filters = 512, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            layer_conv_2d(filters = 512, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            layer_conv_2d(filters = 512, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            layer_batch_normalization() %>%
            layer_max_pooling_2d(pool_size = c(2, 2), strides = 2) %>%
            layer_dropout(rate = 0.25) %>%
            # 
            layer_conv_2d(filters = 512, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            layer_conv_2d(filters = 512, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            layer_conv_2d(filters = 512, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            layer_conv_2d(filters = 512, kernel_size = c(3, 3), activation = "relu", padding = "same", kernel_initializer = initializer_he_normal()) %>%
            layer_batch_normalization() %>%
            layer_max_pooling_2d(pool_size = c(2, 2), strides = 2) %>%
            layer_dropout(rate = 0.25) %>%
            # 
            layer_global_average_pooling_2d() %>% #layer_flatten() %>%
            layer_dense(units = 1024, activation = "relu", kernel_initializer = initializer_he_normal()) %>%
            layer_dropout(rate = 0.5) %>%
            layer_dense(units = 512, activation = "relu", kernel_initializer = initializer_he_normal()) %>%
            layer_dropout(rate = 0.5) %>%
            layer_dense(units = 256, activation = "relu", kernel_initializer = initializer_he_normal()) %>%
            layer_dense(units = 1, activation = "sigmoid")
  return(model)
}

# ------------------ SELECT MODEL ------------------

# General-purpose wrapper
get_cnn_model <- function(type = "CNN_MS", input_shape = c(chip, chip, bands)) {
  if (type == "CNN_MS") {
    return(build_cnn_ms(input_shape))
  } else if (type == "vgg16") {
    return(build_vgg16(input_shape))
  } else if (type == "vgg19") {
    return(build_vgg19(input_shape))
  } else {
    stop("Invalid model type. Choose from 'CNN-MS', 'vgg16', or 'vgg19'.")
  }
}
