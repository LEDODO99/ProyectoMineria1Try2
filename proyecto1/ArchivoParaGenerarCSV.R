#Lynette García Pérez
#Febrero 2019
#Script que sirve para descargar y unir archivos de datos de importación de vehículos de la SAT

#Paquetes necesarios
#lubridate
#stringr

library("tools")
library("lubridate")
library("stringr")

#Extraer primero los archivos .zip y porner el working directory 
# de R a leer de la carpeta donde est?n los txt
# PAra leer y unir todo

listaArchivos<-list.files("Data/")
head(listaArchivos,30)
dataset <-data.frame()
for (archivo in listaArchivos){
  archivo=paste("Data/",archivo,sep="")
  # if (file_ext(archivo) == "zip")
  #   archivo<-unzip(archivo)
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