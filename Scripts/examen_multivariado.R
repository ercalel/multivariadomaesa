# P1-----------------------------------

ingreso <- read_csv("ingreso.csv")

ingreso.matrix <- as.matrix(ingreso[,2:16])
rownames(ingreso.matrix) <-  as.matrix(ingreso[,1])

ingreso.matrix

ingreso.distancia <- dist(ingreso.matrix, method = "euclidean")

ingreso.cluster <-hclust(ingreso.distancia, method = "average")
ingreso.cluster

plot(ingreso.cluster, hang = -0.01, cex = 0.6)

# P2-----------------------------------

fviz_dend(ingreso.cluster, k=3, cex= 0.5, color_labels_by_k = TRUE, rect = TRUE, rect_fill = T) +
  geom_hline(yintercept = 100000, linetype = "dashed") +
  labs(title = "Herarchical clustering",
       subtitle = "Distancia euclídea, Método de average")

ajuste <- cutree(ingreso.cluster, k=3)
table(ajuste)

fviz_cluster(list(data = ingreso.matrix, cluster = ajuste), ellipse.type = "convex", 
             show.clust.cent = FALSE, ggtheme = theme_minimal())

print(rownames(ingreso.matrix)[ajuste==1])
print(rownames(ingreso.matrix)[ajuste==2])
print(rownames(ingreso.matrix)[ajuste==3])

df_analisis <- function(k, m, ajuste, df) {
  vector.ocupacion <- c()
  vector.cluster <- c()
  fila <- 1
  for (i in 1:k) {
    for (elemento in rownames(m)[ajuste==i]) {
      vector.ocupacion[fila] <- elemento
      vector.cluster[fila] <- i
      fila <- fila + 1
    }
  }
  
  df_clusters <- data.frame(vector.ocupacion, vector.cluster)
  names(df_clusters) <- c("X1", "Cluster")
  
  ingreso.clusters <- gather(left_join(df, df_clusters, by = "X1"), "Año", "Salario_promedio", 2:16)
  names(ingreso.clusters) <- c("Ocupacion", "Cluster", "Anio", "Salario_promedio")
  
  ingreso.clusters
}

ingreso.clusters <- df_analisis(3, ingreso.matrix, ajuste, ingreso)
ingreso.clusters

# P3-----------------------------------

ggplot(ingreso.clusters, aes(x=Anio, y=Salario_promedio, group=Ocupacion)) +
  geom_line(aes(color=as.factor(Cluster)))+
  geom_point(aes(color=as.factor(Cluster)))
  
# P4-----------------------------------

set.seed(2)

ingreso.km <- kmeans(ingreso.matrix, centers = 1)$betweenss

for (i in 2:10) {
  ingreso.km[i] <- kmeans(ingreso.matrix, centers = i)$betweenss
}

fviz_nbclust(ingreso.matrix, kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2)

plot(1:10, ingreso.km, type = "b", xlab = "Número de clusters", ylab = "Suma de cuadrados")

ingreso.km2 <- kmeans(ingreso.matrix, nstart = 25, centers = 2)

fviz_cluster(ingreso.km2, data = ingreso.matrix, star.plot = TRUE, ellipse.type = "eclid", repel=T, ggtheme = theme_minimal())

ingreso.km7 <- kmeans(ingreso.matrix, nstart = 25, centers = 7)

fviz_cluster(ingreso.km7, data = ingreso.matrix, star.plot = TRUE, ellipse.type = "eclid", repel=T, ggtheme = theme_minimal())

ingreso.clusters <- df_analisis(2, ingreso.matrix, ingreso.km2$cluster, ingreso)

ggplot(ingreso.clusters, aes(x=Anio, y=Salario_promedio, group=Ocupacion)) +
  geom_line(aes(color=Cluster))+
  geom_point(aes(color=Cluster))

ingreso.clusters <- df_analisis(7, ingreso.matrix, ingreso.km7$cluster, ingreso)

ggplot(ingreso.clusters, aes(x=Anio, y=Salario_promedio, group=Ocupacion)) +
  geom_line(aes(color=Cluster))+
  geom_point(aes(color=Cluster))

# P5-----------------------------------



# P6-----------------------------------

ingreso.km7

aggregate(ingreso.matrix, by=list(cluster=ingreso.km7$cluster), mean)
aggregate(ingreso.matrix, by=list(cluster=ingreso.km7$cluster), sd)


aggregate(ingreso.matrix, by=list(cluster=ajuste), mean)
aggregate(ingreso.matrix, by=list(cluster=ajuste), sd)
