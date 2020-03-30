# Universidad del Valle de Guatemala
# Mineria de Datos
# Christopher Sandoval 13660
# Fernanda Estrada 14198
# Luis Delgado 17187

# Paquetes necesarios
library("tools")
library("lubridate")
library("stringr")
library("cluster")

# Extraer primero los archivos .zip y porner el working directory 
# de R a leer de la carpeta donde estan los txt para leer y unir todo
listaArchivos<-list.files("Data/")
dataset <- data.frame()
for (archivo in listaArchivos){
  archivo=paste("Data/",archivo,sep="")
  print (archivo)
  if (file_ext(archivo) == "txt"){
    if (!exists("dataset")){
      dataset<-read.table(archivo,sep = "|", stringsAsFactors = F)
    }
    else{
      temp_dataset <-read.table(archivo,sep = "|", stringsAsFactors = F,row.names = NULL)
      dataset<-rbind(dataset, temp_dataset)
      rm(temp_dataset)
    }
  }
}


# -------- Descripcion de los datos -------

# Limpiar los datos, tienen una columna extra y esta en desorden
names(dataset)<-names(dataset)[2:length(names(dataset))]
dataset<-dataset[,1:ncol(dataset)-1]
dataset$DatePoliza<-dmy(dataset$Fecha.de.la.Poliza)
dataset$Anio<-year(dataset$DatePoliza)
dataset$Mes<-month(dataset$DatePoliza)
dataset$Dia<-day(dataset$DatePoliza)
dataset$DiaSem<-wday(dataset$DatePoliza)
dataset$DatePoliza<-NULL
dataset[dataset$Modelo.del.Vehiculo == 3015,"Modelo.del.Vehiculo"]<-2015
write.csv(dataset, file="importacionesVehiculosSAT.csv",row.names = F)
save(dataset, file="importacionesSAT.RData")

view(dataset)
summary(dataset)


# ------- Analisis Exploratorio -------

# Separar datos cualitativos de los cauntitativos
# Cuantitativos
data_filtered_quantitative <- dataset[, c("Partida.Arancelaria", "Modelo.del.Vehiculo", "Centimetros.Cubicos", "Asientos", "Puertas","Tonelaje", "Valor.CIF", "Impuesto", "Anio", "Mes", "Dia", "DiaSem")]
data_filtered_quantitative <- na.omit(data_filtered_quantitative)
# Cualitativos
data_filtered_qualitative <- dataset[, c("Pais.de.Proveniencia", "Aduana.de.Ingreso", "Fecha.de.la.Poliza", "Marca", "Linea", "Distintivo", "Tipo.de.Vehiculo", "Tipo.de.Importador", "Tipo.Combustible")]



# Numero de clusters optimo
data_filtered_quantitative <- dataset[, c("Modelo.del.Vehiculo", "Centimetros.Cubicos", "Asientos", "Puertas", "Valor.CIF", "Impuesto")]
data_filtered_quantitative <- na.omit(data_filtered_quantitative)
wss <- (nrow(data_filtered_quantitative)-1)*sum(apply(data_filtered_quantitative,2,var))
for (i in 2:10) 
  wss[i] <- sum(kmeans(data_filtered_quantitative, centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")
# Clustering
