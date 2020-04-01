# ------- Clustering ------

data_importaciones <- read.csv("importacionesVehiculosSAT.csv", stringsAsFactors = FALSE)

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




# ------- Analisis variables cuantitativas ------

# Separacion variables cuantitativas
data_filtered_quantitativeCluster <- data_importaciones[, c("Modelo.del.Vehiculo", "Centimetros.Cubicos", "Asientos", "Puertas", "Valor.CIF", "Impuesto", "Anio")]

# Tener un sample aleatorio, las graficas no se generan por la gran cantidad de datos
data_sample <- data_filtered_quantitative[sample(nrow(data_filtered_quantitative), 10000), ]
head(data_filtered_quantitative)

# Matriz de dispersion entre variables
pairs(~Modelo.del.Vehiculo+Centimetros.Cubicos+Tonelaje+Valor.CIF+Impuesto+Anio,data=data_sample,main="Matriz de dispersion")

library(ggplot2)

data_sample <- data_sample[data_sample$Centimetros.Cubicos > 2,]
data_sample <- data_sample[data_sample$Modelo.del.Vehiculo > 1900,]
data_sample <- data_sample[data_sample$Impuesto < 2000000,]

# Resumen de variables
summary(data_sample$Anio)
summary(data_sample$Modelo.del.Vehiculo)
summary(data_sample$Impuesto)
summary(data_sample$Centimetros.Cubicos)

# Histogramas
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

# Mostrar histogramas
ggplot(data_sample, aes(x = Modelo.del.Vehiculo)) +
  geom_boxplot()
ggplot(data_sample, aes(x = Centimetros.Cubicos)) +
  geom_boxplot()
ggplot(data_sample, aes(x = Impuesto)) +
  geom_boxplot()

# Matriz de correlacion
library(corrplot)
matriz_cor <- cor(data_sample)
corrplot(matriz_cor)

# Normalidad de los datos
# Histograma
hist(data_sample$Anio, main = "Histograma a?o", xlab = "A?o")
hist(data_sample$Modelo.del.Vehiculo, main = "Histograma modelo", xlab = "Modelo")
hist(data_sample$Impuesto, main = "Histograma Impuesto", xlab = "Impuesto")
hist(data_sample$Centimetros.Cubicos, main = "Histograma Centimetros Cubicos", xlab = "Centimetros Cubicos")
hist(data_sample$Valor.CIF, main = "Histograma Valor CIF", xlab = "Valor CIF")
# Caja y bigotes
boxplot(data_sample$Anio, main = "Caja A?o")
boxplot(data_sample$Modelo.del.Vehiculo, main = "Caja Modelo")
boxplot(data_sample$Impuesto, main = "Caja Impuesto")
boxplot(data_sample$Centimetros.Cubicos, main = "Caja Centimetros Cubicos")
boxplot(data_sample$Valor.CIF, main = "Caja Valor CIF")





# ------- Analisis variables cualitativas ------

install.packages("haven")
library("haven")
listaArchivosAccidentes<-list.files("DatosAccidentes/")
for (archivo in listaArchivosAccidentes){
  archivo=paste("DatosAccidentes/",archivo,sep="")
  if(!exists("accidentes")){
    accidentes<-read_sav(archivo)
  }else{
    temp_dataset<-read_sav(archivo)
    accidentes<-rbind(accidentes,temp_dataset)
    rm(temp_dataset)
  }
}
a<-read_sav("DatosAccidentes/oVGWNimc4xUWDFj0317UxhyHmsBLvaW7.sav")
b<-read_sav("DatosAccidentes/JHtnT62UiTxEP6AbhA4RcFDN6Nokk4c9.sav")

# Frecuencias de cada variable
frecuenciaPais=table(dataset$Pais.de.Proveniencia)
df<-as.data.frame(frecuenciaPais)
head(df[order(-df$Freq),],30)

frecuenciaAduana=table(dataset$Aduana.de.Ingreso)
df<-as.data.frame(frecuenciaAduana)
head(df[order(-df$Freq),],30)

frecuenciaFecha=table(dataset$Fecha.de.la.Poliza)
df<-as.data.frame(frecuenciaFecha)
head(df[order(-df$Freq),],30)

frecuenciaPartida=table(dataset$Partida.Arancelaria)
df<-as.data.frame(frecuenciaPartida)
head(df[order(-df$Freq),],30)

frecuenciaModelo=table(dataset$Modelo.del.Vehiculo)
df<-as.data.frame(frecuenciaModelo)
head(df[order(-df$Freq),],30)

frecuenciaMarca=table(dataset$Marca)
df<-as.data.frame(frecuenciaMarca)
head(df[order(-df$Freq),],30)

frecuenciaLinea=table(dataset$Linea)
df<-as.data.frame(frecuenciaLinea)
head(df[order(-df$Freq),],30)

frecuenciaDistintivo=table(dataset$Distintivo)
df<-as.data.frame(frecuenciaDistintivo)
head(df[order(-df$Freq),],30)

frecuenciaTipoVehiculo=table(dataset$Tipo.de.Vehiculo)
df<-as.data.frame(frecuenciaTipoVehiculo)
head(df[order(-df$Freq),],30)

frecuenciaTipoImport=table(dataset$Tipo.de.Importador)
df<-as.data.frame(frecuenciaTipoImport)
head(df[order(-df$Freq),],30)

frecuenciaTipoCombust=table(dataset$Tipo.Combustible)
df<-as.data.frame(frecuenciaTipoCombust)
head(df[order(-df$Freq),],30)

frecuenciaAnio=table(dataset$Anio)
df<-as.data.frame(frecuenciaAnio)
head(df[order(df$Var1),],30)

frecuenciaMes=table(dataset$Mes)
df<-as.data.frame(frecuenciaMes)
head(df[order(df$Var1),],30)

frecuenciaDia=table(dataset$Dia)
df<-as.data.frame(frecuenciaDia)
head(df[order(df$Var1),],30)

frecuenciaDiaSem=table(dataset$DiaSem)
df<-as.data.frame(frecuenciaDiaSem)
head(df[order(df$Var1),],30)
barplot (frecuenciaAnio)
