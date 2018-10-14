
rm(list = ls())
gc()

# loading the 'keras' library

library(keras)

# list of fruits

frt_list <- c("Kiwi", "Banana", "Apricot", "Avocado", "Cocos", "Clementine", "Mandarine", "Orange",
                "Limes", "Lemon", "Peach", "Plum", "Raspberry", "Strawberry", "Pineapple", "Pomegranate")

# no of output (fruits)

out_n <- length(frt_list)

# original images are 100x100 (given in .pdf provided), scale it down

img_wd <- 20
img_ht <- 20
target_s <- c(img_wd,img_ht)

# the images are color, require the RGB
channels <- 3

# stored image locations

train_path <- "E:\\Study\\R Projects\\Common files\\Image_Fruit\\fruits-360\\Training\\"
test_path <- "E:\\Study\\R Projects\\Common files\\Image_Fruit\\fruits-360\\Test\\"

# image preprocessing: https://keras.io/preprocessing/image/

# image preprocessing options

train_d_gen <- image_data_generator(
  rescale = 1/255
)

# same preprocessing as that of training
valid_d_gen <- image_data_generator(
  rescale = 1/255
)

# training images

train_i_array <- flow_images_from_directory(train_path,  # directory path. one subdirectory per class as required
                                            train_d_gen, # generator
                                            target_size = target_s,
                                            class_mode = "categorical", # same as default. one hot encoded
                                            classes = frt_list, # optional list
                                            seed = 931992)


test_i_array <- flow_images_from_directory(test_path,  # directory path. one subdirectory per class as required
                                            valid_d_gen, # generator
                                            target_size = target_s,
                                            class_mode = "categorical", # same as default. one hot encoded
                                            classes = frt_list, # optional list
                                            seed = 931992)

table(train_i_array$classes)  # no of images per class

dim(train_i_array$classes) # total images

train_i_array$class_indices # class and index map 
# validate with folder image count
# Pomegranate has 492 images in folder. class 15 has 492 as well

train_i_array$target_size  # 20x20
train_i_array$image_data_generator # class name
train_i_array$image_shape  # 20x20x3

frt_class_indices <- train_i_array$class_indices
save(frt_class_indices, file = "E:\\Study\\R Projects\\Common files\\Image_Fruit\\fruits-360\\frt_class_indices.RData")

# no. of training sample

train_sample <- train_i_array$n
train_sample
valid_sample <- test_i_array$n
valid_sample

# batch size 
batch_size <- 32
epochs <- 10

# simple sequential CNN

# initialise model
model <- keras_model_sequential()

# layers
model %>%
  layer_conv_2d(filters = 32,
                kernel_size = c(3,3),
                padding = "same",
                input_shape = c(img_wd, img_ht, channels)) %>% 
  layer_activation("relu") %>% 
  layer_conv_2d(filters = 16,
                kernel_size = c(3,3),
                padding = "same") %>% 
  layer_activation_leaky_relu(0.5) %>% 
  layer_batch_normalization() %>% 
  # max pooling
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_dropout(0.25) %>% 
  layer_flatten() %>% 
  layer_dense(100) %>% 
  layer_activation("relu") %>% 
  layer_dropout(0.5) %>% 
  layer_dense(out_n) %>% 
  layer_activation("softmax")
  
  
#compile
model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(lr = 0.0001, decay = 1e-6),
  metrics = "accuracy"
)

# fit
hist <- model %>% fit_generator(
  train_i_array,   # training data
  steps_per_epoch = as.integer(train_sample/batch_size),
  epochs = epochs,
  validation_data = test_i_array,
  validation_steps = as.integer(valid_sample/batch_size),
  verbose = 2,
  callbacks = list(
    # save model after every epoch
    callback_model_checkpoint("E:\\Study\\R Projects\\Common files\\Image_Fruit\\fruits-360\\f_check.h5", save_best_only = TRUE),
    #tensorboard
    callback_tensorboard(log_dir = "E:\\Study\\R Projects\\Common files\\Image_Fruit\\fruits-360\\logs")
  )
)

sessionInfo()

  



