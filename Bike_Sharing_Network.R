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

## Network visualization

# cosine similarity
library(lsa)
simMat <- cosine(trend.mat)
diag(simMat) <- 0

# tree pruning
edglimit <- 0.7
simMat[(simMat<edglimit)] <- 0

#iGraph
library(igraph)
simI <- graph_from_adjacency_matrix(simMat, mode = 'undirected',weighted = TRUE)
cb <- cluster_edge_betweenness(simI)

# dendrograph
dendPlot(cb, mode = "hclust")
plot(x=cb, y=simI)

library(networkD3)
members <- membership(cb)
simI_d3 <- igraph_to_networkD3(simI, group = members)

# force directed network plot
forceNetwork(Links = simI_d3$links, Nodes = simI_d3$nodes,
             Source = 'source', Target = 'target',
             NodeID = 'name', Group = 'group',
             fontSize = 16, fontFamily = 'Arial',linkDistance = 100,
             zoom = TRUE)



