library(tidyverse)
library(keras)
library(tensorflow)
library(reticulate)

setwd("C:/Users/T A/OneDrive - purdue.edu")

model_list <- load_model_tf("./dandelion_model")
model <- model_list

target_size <- c(224, 224)
res <- data.frame(file = character(), class = character(), percent_dandelion = numeric(), percent_grass = numeric(), stringsAsFactors = FALSE)

f <- list.files("./grass")
for (i in f){
  test_image <- image_load(paste("./grass/",i,sep=""),
                           target_size = target_size)
  n_pixels <- 224*224
  n_random_pixels <- round(n_pixels * 0.01)
  random_indices <- sample(n_pixels, n_random_pixels)
  random_pixels <- extract(test_image, x = as.matrix(expand.grid(x = 1:dim(test_image)[1], y = 1:dim(test_image)[2]))[random_indices, ])
  
  # Change the colors of the random pixels
  new_pixels <- matrix(sample(256, n_random_pixels * 3, replace = TRUE) - 1, ncol = 3)
  random_pixels[] <- new_pixels
  
  # Put the new pixels back into the image
  test_image[as.matrix(expand.grid(x = 1:dim(test_image)[1], y = 1:dim(test_image)[2]))[random_indices, ]] <- random_pixels
  
  x <- image_to_array(test_image)
  x <- array_reshape(x, c(1, dim(x)))
  x <- x/255
  pred <- model %>% predict(x)
  if (pred[1,2] < 0.50){
    res <- rbind(res, data.frame(file = i, class = "not_grass", percent_dandelion = pred[1,1], percent_grass = 1- pred[1,1]))
  } else {
    res <- rbind(res, data.frame(file = i, class = "grass", percent_dandelion = pred[1,1], percent_grass = 1 - pred[1,1]))
  }
}