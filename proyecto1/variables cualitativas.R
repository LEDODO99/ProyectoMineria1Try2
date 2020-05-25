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

dataImp=read.csv("importacionesVehiculosSAT.csv")
