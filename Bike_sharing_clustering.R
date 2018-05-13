

rm(list = ls())

# load libraries
library(cluster)
library(factoextra)
library(tidyverse)
library(readxl)

# loading the required files

bikes <- read_excel("E:\\Study\\R Projects\\Common files\\bikes.xlsx")
orders <- read_excel("E:\\Study\\R Projects\\Common files\\orders.xlsx")
customers <- read_excel("E:\\Study\\R Projects\\Common files\\bikeshops.xlsx")

glimpse(orders)
glimpse(bikes)
glimpse(customers)

order <- merge(orders, customers, by.x = "customer.id",by.y = "bikeshop.id")
order <- merge(order, bikes, by.x = "product.id",by.y = "bike.id")

glimpse(order)

# required columns
names(order)


order %>%
  mutate(amount = price*quantity) %>%
  select(order.date,order.id,order.line,
         bikeshop.name,model, quantity,price, category1, category2,frame,
         amount) %>%
  arrange(order.id,order.line) -> order
glimpse(order)

## hypothesis creation
# bike lovers buy models based on features sush as:
# mountain or road bikes, price range


# price in two buckets
# scaling

order %>%
  group_by(bikeshop.name,model, category1, category2,frame, price) %>%
  summarise(tot.q = sum(quantity)) %>%
  spread(bikeshop.name, tot.q)-> trend

View(trend)  
trend[is.na(trend)] <- 0

med_price <- median(trend$price)

trend %>%
  mutate(price = ifelse(price > med_price,1,0)) -> trend

glimpse(trend)

library(recipes)

obj <- recipe(price~., data = trend) %>%
  step_scale(all_numeric(),-all_nominal(),-all_outcomes()) %>%
  prep(data = trend)

obj
trend_new <- bake(obj, newdata = trend)

# K-means

kmeanD <- trend_new[,-(1:5)]  # required colums
kmeanD.t <- t(kmeanD)

# from first script

# function to visualize
fviz <- function(x)
{
  fviz_dist(x, 
            gradient = list(low="#00AFBB",mid="white",high="#FC4E07"))
}

dist.euc <- get_dist(kmeanD.t)
fviz(dist.euc)
dist.man <- get_dist(kmeanD.t, method = "manhattan")
fviz(dist.man)

# silhouette
fviz_nbclust(kmeanD.t, kmeans, method = "silhouette")    #2 clusters

# one more method

avg_sil <- function(x)
{
  km <- kmeans(kmeanD.t, x, nstart = 25)
  ss <- silhouette(km$cluster,dist(kmeanD.t))
  return(mean(ss[,3]))
}

k_values <- 2:8
sil <- c()
for (i in 2:8)
{
  sil[i] <- avg_sil(i)
  i=i+1
}

plot(1:8,sil, type = "b", pch = 19)        # 2 or 4 clusters

# elbow method

set.seed(931992)
fviz_nbclust(kmeanD.t,kmeans, method = "wss")   # 5 clusters

# gap stats

set.seed(123)

gap_stat <- clusGap(kmeanD.t, kmeans, nstart = 25, K.max = 10, B=50)
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)                       # 4 clusters


# customer segmentation

km <- kmeans(kmeanD.t, 5, nstart = 40)
km$cluster



