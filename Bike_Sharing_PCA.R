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

# calculating the total price
order %>%
  mutate(amount = price*quantity) %>%
  select(order.date,order.id,order.line,
         bikeshop.name,model, quantity,price, category1, category2,frame,
         amount) %>%
  arrange(order.id,order.line) -> order
glimpse(order)

order %>%
  group_by(bikeshop.name,model, category1, category2,frame, price) %>%
  summarise(tot.q = sum(quantity)) %>%
  spread(bikeshop.name, tot.q)-> trend
View(trend)  
trend[is.na(trend)] <- 0    # 0 for NA's

# price is skewed
names(trend)
ggplot(trend, aes(x=price, y=..density..))+
  geom_density()
library(Hmisc)
trend$price <- cut2(trend$price, g=2)

trend.mat <- as.matrix(trend[,-(1:5)]) # numerical columns
trend.mat <- prop.table(trend.mat,margin = 2)
trend <- bind_cols(trend[,(1:5)],as.data.frame(trend.mat))

# kmeans for both 4 and 5 
set.seed(123)
k4 <- kmeans(t(trend[,-(1:5)]),4,nstart = 35)
k5 <- kmeans(t(trend[,-(1:5)]),5,nstart = 35)

# PCA - prcomp()

pca <- prcomp(t(trend[,-(1:5)]),scale. = TRUE,center = T)

covar <- cov(t(trend[,-(1:5)]))
eign <- eigen(covar)
PVE <- eign$values/sum(eign$values)
#OR
pca.var <- pca$sdev^2
PVE1 <- pca.var/sum(pca.var)


# bar chart for PC1 and PC2
qplot(c(1:5),head(PVE,5)) +
  geom_bar(stat = "identity")   #pc1 and pc2 

library(ggfortify)
pca.f <- fortify(pca)

pca4.dat <- cbind(pca.f, group=k4$cluster)
pca5.dat <- cbind(pca.f, group=k5$cluster)


library(ggplot2)
# Script for plotting k=4
ggplot(pca4.dat) +
  geom_point(aes(x=PC1, y=PC2, col=factor(group), text=rownames(pca4.dat)), size=2) +
  labs(title = "Visualizing K-Means Clusters Against First Two Principal Components") +
  scale_color_brewer(name="", palette = "Set1")

# Script for plotting k=5
ggplot(pca5.dat) +
  geom_point(aes(x=PC1, y=PC2, col=factor(group), text=rownames(pca5.dat)), size=2) +
  labs(title = "Visualizing K-Means Clusters Against First Two Principal Components") +
  scale_color_brewer(name="", palette = "Set1")

# Switch Group 2 Bike Shops with misclassified Bike Shops in Group 4 -----------
pca.final.dat <- pca5.dat
pca.final.dat[rownames(pca.final.dat) %in% 
                c("San Antonio Bike Shop", "Philadelphia Bike Shop"), 128] <- 4
pca.final.dat[rownames(pca.final.dat) %in% 
                c("Denver Bike Shop", "Kansas City 29ers"), 128] <- 2

ggplot(pca.final.dat) +
  geom_point(aes(x=PC1, y=PC2, col=factor(group), text=rownames(pca.final.dat)), size=2) +
  labs(title = "Visualizing K-Means Clusters Against First Two Principal Components") +
  scale_color_brewer(name="", palette = "Set1")


library(dplyr)
group.num <- 2 # Set group number
group.names <- rownames(pca.final.dat[pca.final.dat$group == group.num, ])
groupTrends <- trend %>%
  select(model:price, match(group.names, names(.))) # Use match() to select column names
group.avg <- apply(trend[6:ncol(trend)], 1, mean) # Take average of values
groupTrends <- bind_cols(trend, as_data_frame(group.avg)) %>%
  arrange(-group.avg)
head(groupTrends, 10)     # top 10 products by group avg.


# Checking t-sne on the data
library(Rtsne)
tsne <- Rtsne(t(trend[,-(1:5)]), 
              dims = 2,
              perplexity = 5, 
              verbose = TRUE,
              max_iter = 500)
ts <- cbind(tsne$Y, pca4.dat$group)
ts[,1]
qplot(x=ts[,1],y=ts[,2],col = ts[,3])
