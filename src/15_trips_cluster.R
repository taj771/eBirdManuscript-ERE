library(factoextra)
library(NbClust)

df <- read.csv("./data/processed/full_rec_ebird.csv")
trip_cluster <- select(df,c(1,3))

fviz_nbclust(trip_cluster, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

cls <- kmeans(x = trip_cluster$count, centers = 2)
trip_cluster$cluster <- as.character(cls$cluster)


p <- ggplot() +
  geom_point(data = trip_cluster, 
             mapping = aes(x = ID_rec, 
                           y = count, 
                           colour = cluster))

p <- ggMarginal(p, type="boxplot", margins = c("y"))
plot(p)


trip_cluster_eb <- select(df,c(6))%>%
  drop_na()
fviz_nbclust(trip_cluster_eb, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

cls <- kmeans(x = trip_cluster_eb, centers = 2)
trip_cluster_eb$cluster <- as.character(cls$cluster)

trip_cluster_eb <- trip_cluster_eb%>%
  left_join(df, by = c("count_1"))

ggplot() +
  geom_point(data = trip_cluster_eb, 
             mapping = aes(x = ID_ebird, 
                           y = count_1, 
                           colour = cluster))




head(iris_cluster)