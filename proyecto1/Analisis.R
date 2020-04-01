data_importaciones <- read.csv("importacionesVehiculosSAT.csv", stringsAsFactors = FALSE)
data_filtered_quantitativeCluster <- data_importaciones[, c("Modelo.del.Vehiculo", "Centimetros.Cubicos", "Asientos", "Puertas", "Valor.CIF", "Impuesto", "Anio")]

data_sample <- data_filtered_quantitative[sample(nrow(data_filtered_quantitative), 10000), ]
head(data_filtered_quantitative)

pairs(~Modelo.del.Vehiculo+Centimetros.Cubicos+Tonelaje+Valor.CIF+Impuesto+Anio,data=data_sample,main="Matriz de dispersion")

library(ggplot2)

data_sample <- data_sample[data_sample$Centimetros.Cubicos > 2,]
data_sample <- data_sample[data_sample$Modelo.del.Vehiculo > 1900,]
data_sample <- data_sample[data_sample$Impuesto < 2000000,]

summary(data_sample$Anio)
summary(data_sample$Modelo.del.Vehiculo)
summary(data_sample$Impuesto)
summary(data_sample$Centimetros.Cubicos)


ggplot(data_sample, aes(x = Anio)) +
  geom_histogram(aes(y = stat(count)), bins = 12, color="black", fill="grey") +
  scale_y_continuous()+
  xlim(c(2011, 2019))

ggplot(data_sample, aes(x = Modelo.del.Vehiculo)) +
  geom_histogram(aes(y = stat(count)), bins = 20, color="black", fill="grey") +
  scale_y_continuous(labels = scales::percent)+
  xlim(c(1980, 2020))

ggplot(data_sample, aes(x = Impuesto)) +
  geom_histogram(aes(y = stat(count)), bins = 20, color="black", fill="grey") +
  scale_y_continuous(labels = scales::percent)+
  xlim(c(0, 10000))

ggplot(data_sample, aes(x = Centimetros.Cubicos)) +
  geom_histogram(aes(y = stat(count)), bins = 20, color="black", fill="grey") +
  scale_y_continuous(labels = scales::percent) +
  xlim(c(500, 8000))

#sssssssssssssssss
ggplot(data_sample, aes(x = Modelo.del.Vehiculo)) +
  geom_boxplot()

ggplot(data_sample, aes(x = Centimetros.Cubicos)) +
  geom_boxplot()

ggplot(data_sample, aes(x = Impuesto)) +
  geom_boxplot()


library(corrplot)
matriz_cor <- cor(data_sample)
corrplot(matriz_cor)


hist(data_sample$Anio, main = "Histograma año", xlab = "Año")
hist(data_sample$Modelo.del.Vehiculo, main = "Histograma modelo", xlab = "Modelo")
hist(data_sample$Impuesto, main = "Histograma Impuesto", xlab = "Impuesto")
hist(data_sample$Centimetros.Cubicos, main = "Histograma Centimetros Cubicos", xlab = "Centimetros Cubicos")
hist(data_sample$Valor.CIF, main = "Histograma Valor CIF", xlab = "Valor CIF")

boxplot(data_sample$Anio, main = "Caja Año")
boxplot(data_sample$Modelo.del.Vehiculo, main = "Caja Modelo")
boxplot(data_sample$Impuesto, main = "Caja Impuesto")
boxplot(data_sample$Centimetros.Cubicos, main = "Caja Centimetros Cubicos")
boxplot(data_sample$Valor.CIF, main = "Caja Valor CIF")

# ------- Clustering ------

# Numero de clusters optimo
data_filtered_quantitativeCluster <- data_importaciones[, c("Modelo.del.Vehiculo", "Centimetros.Cubicos", "Asientos", "Puertas", "Valor.CIF", "Impuesto", "Anio")]
data_filtered_quantitativeCluster <- na.omit(data_filtered_quantitativeCluster)
wss <- (nrow(data_filtered_quantitativeCluster)-1)*sum(apply(data_filtered_quantitativeCluster,2,var))
for (i in 2:10)
  wss[i] <- sum(kmeans(data_filtered_quantitativeCluster, centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")

# Clustering
data_sample <- data_filtered_quantitativeCluster[sample(nrow(data_filtered_quantitativeCluster), 10000), ]
km<-kmeans(data_sample,3)
plotcluster(data_sample, km$cluster)
data_sample$Class<-km$cluster

# Silueta
silkm<-silhouette(km$cluster,dist(data_sample))
mean(silkm[,3])

# Agrupamientos
cluster1 <- data_sample[data_sample$Class==1,]
cluster2 <- data_sample[data_sample$Class==2,]
cluster3 <- data_sample[data_sample$Class==3,]
summary(cluster1)
summary(cluster2)
summary(cluster3)