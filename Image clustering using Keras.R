gc()
rm(list = ls())

#loading the packages

load_lb <- function()
{
  library(keras)
  library(magick)
  library(tidyverse)
  library(imager)
}

load_lb()

# pretrained model

model <- application_vgg16(weights = "imagenet",
                           include_top = FALSE)
model

# training images

image_prep <- function(x)
{
  arrays <- lapply(x, function(path)
  {
    img <- image_load(path, target_size = c(224,224))
    x <- image_to_array(img)
    x <- array_reshape(x, c(1,dim(x)))
    x <- imagenet_preprocess_input(x)
  })
  do.call(abind::abind, c(arrays,list(along = 1)))
}


image_files_path <- "E:/Study/R Projects/Common files/Image_Fruit/fruits-360/Training"
test_path <- "E:\\Study\\R Projects\\Common files\\Image_Fruit\\fruits-360\\Test\\"

sample_fruits <- sample(list.dirs(image_files_path),2)


file_list <- list.files(sample_fruits, full.names = TRUE,
                        recursive = TRUE)
head(file_list)

# feature list

vgg16_feature_list <- data.frame()

for(image in file_list)
{
  print(image)
  cat("Image", which(file_list == image), "from", length(file_list))
  
  vgg16_feature <- predict(model, image_prep(image))
  
  flatten <- as.data.frame.table(vgg16_feature,
                                 responseName = "value") %>% 
    select(value)
  flatten <- cbind(image, as.data.frame(t(flatten)))

  vgg16_feature_list <- rbind(vgg16_feature_list, flatten)
}

save(vgg16_feature_list, file = "E:/Study/R Projects/Common files/Image_Fruit/fruits-360/vgg16_feature_list.RData")


# clustering
pca <- prcomp(vgg16_feature_list[, -1],
              center = TRUE,
              scale = FALSE)

save(pca, file = "E:/Study/R Projects/Common files/Image_Fruit/fruits-360/pca.RData")

data.frame(PC1 = pca$x[, 1],
           PC2 = pca$x[, 2]) %>% 
  ggplot(aes(x = PC1, y = PC2))+
  geom_point()


# k-means

set.seed(120)
cluster_pca <- kmeans(pca$x[, 1:10], 4)
cluster_feature <- kmeans(vgg16_feature_list[, -1], 4)


cluster_list <- data.frame(cluster_pca = cluster_pca$cluster,
                           cluster_feature = cluster_feature$cluster,
                           vgg16_feature_list) %>% 
  select(cluster_pca, cluster_feature, image) %>% 
  mutate(class = gsub("E:/Study/R Projects/Common files/Image_Fruit/fruits-360/Training/","", image),
         class = substr(class, start = 1, stop = 3))

head(cluster_list)
head(image)

# PCA

cluster_list %>% 
  count(cluster_pca, class)

cluster_list %>% 
  count(cluster_feature, class)


cluster_list %>% 
  mutate(PC1 = pca$x[, 1],
         PC2 = pca$x[, 2]) %>% 
  ggplot(aes(x = PC1, y = PC2, color= class, shape=factor(cluster_pca)))+
  geom_point()


plot_random_images <- function(n_img = 49,
                               cluster = 1,
                               rows = 7,
                               cols = 7) {
  cluster_list_random_cl_1 <- cluster_list %>%
    filter(cluster_pca == cluster) %>%
    sample_n(n_img)
  
  graphics::layout(matrix(c(1:n_img), rows, cols, byrow = TRUE))
  for (i in 1:n_img) {
    path <- as.character(cluster_list_random_cl_1$image[i])
    img <- load.image(path)
    plot(img, axes = FALSE)
    title(main = paste("Cluster PCA", cluster))
  }
}

sapply(c(1:4), function(x) plot_random_images(cluster = x))
