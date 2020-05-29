# Universidad del Valle
# Resultados Finales - Minería de Datos
# 25/05/2020
# Christopher Sandoval
# Fernanda Estrada
# Luis Delgado


#-------------------------IMPORTACION DE DATOS---------------------------

install.packages("haven")
library("haven")

# Se importan los datos y se estandarizan los nombres de las columnas.
# TambiÃ©n se unen los diferentes archivos en una sola tabla

accidentes_train<-read_sav("DatosAccidentes/accidentes_2009.sav")
accidentes_train<-accidentes_train[,c("estado_pil", "dia_sem_ocu", "hora_ocu", "sexo_pil","edad_pil", "tipo_vehi", "color_vehi")]
colnames(accidentes_train) <- c("estado_pil", "dia_sem_ocu", "hora_ocu", "sexo_pil","edad_pil", "tipo_veh", "color_veh")

temp_dataset<-read_sav("DatosAccidentes/accidentes_2010.sav")
temp_dataset<-temp_dataset[,c("estado_pil", "dia_sem_ocu", "hora_ocu", "sexo_pil","edad_pil", "tipo_v", "color_v")]
colnames(temp_dataset) <- c("estado_pil", "dia_sem_ocu", "hora_ocu", "sexo_pil","edad_pil", "tipo_veh", "color_veh")

accidentes_train<-rbind(accidentes_train,temp_dataset)

temp_dataset<-read_sav("DatosAccidentes/accidentes_2011.sav")
temp_dataset<-temp_dataset[,c("estado_pil", "dia_sem_ocu", "hora_ocu", "sexo_pil","edad_pil", "tipo_vehiculo", "color_vehi")]
colnames(temp_dataset) <- c("estado_pil", "dia_sem_ocu", "hora_ocu", "sexo_pil","edad_pil", "tipo_veh", "color_veh")

accidentes_train<-rbind(accidentes_train,temp_dataset)

temp_dataset<-read_sav("DatosAccidentes/accidentes_2012.sav")
temp_dataset<-temp_dataset[,c("condicion_pil", "dia_sem_ocu", "hora_ocu", "sexo_pil","edad_pil", "tipo_vehi", "color_vehi")]
colnames(temp_dataset) <- c("estado_pil", "dia_sem_ocu", "hora_ocu", "sexo_pil","edad_pil", "tipo_veh", "color_veh")

accidentes_train<-rbind(accidentes_train,temp_dataset)

temp_dataset<-read_sav("DatosAccidentes/accidentes_2013.sav")
temp_dataset<-temp_dataset[,c("estado_pil", "dia_sem_ocu", "hora_ocu", "sexo_pil","edad_pil", "tipo_veh", "color_veh")]
colnames(temp_dataset) <- c("estado_pil", "dia_sem_ocu", "hora_ocu", "sexo_pil","edad_pil", "tipo_veh", "color_veh")

accidentes_train<-rbind(accidentes_train,temp_dataset)

temp_dataset<-read_sav("DatosAccidentes/accidentes_2014.sav")
temp_dataset<-temp_dataset[,c("estado_con", "día_sem_ocu", "hora_ocu", "sexo_con","edad_con", "tipo_veh", "color_veh")]
colnames(temp_dataset) <- c("estado_pil", "dia_sem_ocu", "hora_ocu", "sexo_pil","edad_pil", "tipo_veh", "color_veh")

accidentes_train<-rbind(accidentes_train,temp_dataset)


#--------------------------------LIMPIEZA DE DATOS----------------------------------

# Se quitan los daton invalidos
accidentes_train<-accidentes_train[(accidentes_train$estado_pil < 9),]
accidentes_train<-accidentes_train[(accidentes_train$sexo_pil < 9),]
accidentes_train<-accidentes_train[(accidentes_train$color_veh < 99),]
accidentes_train<-accidentes_train[(accidentes_train$tipo_veh < 99),]
accidentes_train<-accidentes_train[(accidentes_train$edad_pil < 999),]

accidentes_train<-as.data.frame(accidentes_train)

accidentes_train <- na.omit(accidentes_train)

# Se vuelven factores todas las columnas
accidentes_train[]<-lapply(accidentes_train, factor)

# Se vuelven columnas numericas las columnas que se normalizaran
accidentes_train$edad_pil<-as.numeric(accidentes_train$edad_pil)
accidentes_train$hora_ocu<-as.numeric(accidentes_train$hora_ocu)

# Se estandariza la forma de contar las horas (0-23)
accidentes_train$hora_ocu<-ifelse(accidentes_train$hora_ocu == 24, 0,accidentes_train$hora_ocu)

# Se transforma la columna de estado del piloto a ser 0 si estaba normal o 1 si estaba bajo la influencia
accidentes_train$estado_pil<-as.factor(ifelse(accidentes_train$estado_pil == 1, 0,1))


#----------------------------------ANALISIS EXPLORATORIO------------------------------------------------

library(ggplot2)

# Creamos una copia de la tabla para analizar
accidentes_analisis <- accidentes_train

# Resumen de los campos
summary(accidentes_analisis)

# Le asignamos nombres a los estados para que sea mas facil de leer las graficas
accidentes_analisis$estado_pil<-ifelse(accidentes_analisis$estado_pil == 0, "Sobrio","Bajo efectos")

# Grafica de las horas diferenciando entre sobrio y bajo efectos
ggplot(accidentes_analisis, aes(factor(accidentes_analisis$hora_ocu),fill = accidentes_analisis$estado_pil)) +
  geom_bar(stat="count", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")

# Le asignamos nombres a los dias
accidentes_analisis$dia_sem_ocu<-ifelse(accidentes_analisis$dia_sem_ocu == 1, "Lunes",accidentes_analisis$dia_sem_ocu)
accidentes_analisis$dia_sem_ocu<-ifelse(accidentes_analisis$dia_sem_ocu == 2, "Martes",accidentes_analisis$dia_sem_ocu)
accidentes_analisis$dia_sem_ocu<-ifelse(accidentes_analisis$dia_sem_ocu == 3, "Miercoles",accidentes_analisis$dia_sem_ocu)
accidentes_analisis$dia_sem_ocu<-ifelse(accidentes_analisis$dia_sem_ocu == 4, "Jueves",accidentes_analisis$dia_sem_ocu)
accidentes_analisis$dia_sem_ocu<-ifelse(accidentes_analisis$dia_sem_ocu == 5, "Viernes",accidentes_analisis$dia_sem_ocu)
accidentes_analisis$dia_sem_ocu<-ifelse(accidentes_analisis$dia_sem_ocu == 6, "Sabado",accidentes_analisis$dia_sem_ocu)
accidentes_analisis$dia_sem_ocu<-ifelse(accidentes_analisis$dia_sem_ocu == 7, "Domingo",accidentes_analisis$dia_sem_ocu)

accidentes_analisis$dia_sem_ocu <- reorder(accidentes_analisis$dia_sem_ocu,accidentes_analisis$dia_sem_ocu,FUN=length)

# Grafica de los dias diferenciando entre sobrio y bajo efectos
ggplot(accidentes_analisis, aes(factor(accidentes_analisis$dia_sem_ocu),fill = accidentes_analisis$estado_pil)) +
  geom_bar(stat="count", position = "dodge") + 
  scale_fill_brewer(palette = "Set1") +
  scale_x_discrete(limits = rev(levels(factor(accidentes_analisis$dia_sem_ocu))))

# Histograma de las edades
ggplot(data=accidentes_analisis, aes(accidentes_analisis$edad_pil)) + 
  geom_histogram(breaks=seq(0, 60, by=3),
                 col="black",
                 fill="darkgreen")

# Le ponemos nombres a los sexos para que sea mas facil leer las graficas
accidentes_analisis$sexo_pil<-ifelse(accidentes_analisis$sexo_pil == 1, "Hombre","Mujer")

# Grafica de los sexos diferenciando entre sobrio y bajo efectos
ggplot(accidentes_analisis, aes(factor(accidentes_analisis$sexo_pil),fill = accidentes_analisis$estado_pil)) +
  geom_bar(stat="count", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")

# Le ponemos nombres a los colores para que sea mas facil leer las graficas y seleccionamos solo los 10 colores mas comunes
accidentes_analisis<-accidentes_analisis[is.element(accidentes_analisis$color_veh, tail(names(sort(table(accidentes_analisis$color_veh))), 10)),]

accidentes_analisis$color_veh<-ifelse(accidentes_analisis$color_veh == 1, "Rojo",accidentes_analisis$color_veh)
accidentes_analisis$color_veh<-ifelse(accidentes_analisis$color_veh == 2, "Blanco",accidentes_analisis$color_veh)
accidentes_analisis$color_veh<-ifelse(accidentes_analisis$color_veh == 3, "Azul",accidentes_analisis$color_veh)
accidentes_analisis$color_veh<-ifelse(accidentes_analisis$color_veh == 4, "Gris",accidentes_analisis$color_veh)
accidentes_analisis$color_veh<-ifelse(accidentes_analisis$color_veh == 5, "Negro",accidentes_analisis$color_veh)
accidentes_analisis$color_veh<-ifelse(accidentes_analisis$color_veh == 6, "Verde",accidentes_analisis$color_veh)
accidentes_analisis$color_veh<-ifelse(accidentes_analisis$color_veh == 7, "Amarillo",accidentes_analisis$color_veh)
accidentes_analisis$color_veh<-ifelse(accidentes_analisis$color_veh == 8, "Celeste",accidentes_analisis$color_veh)
accidentes_analisis$color_veh<-ifelse(accidentes_analisis$color_veh == 9, "Corinto",accidentes_analisis$color_veh)
accidentes_analisis$color_veh<-ifelse(accidentes_analisis$color_veh == 11, "Beige",accidentes_analisis$color_veh)

accidentes_analisis$color_veh <- reorder(accidentes_analisis$color_veh,accidentes_analisis$color_veh,FUN=length)

# Grafica de los colores diferenciando entre sobrio y bajo efectos
ggplot(accidentes_analisis, aes(factor(accidentes_analisis$color_veh),fill = accidentes_analisis$estado_pil)) +
  geom_bar(stat="count", position = "dodge") + 
  scale_fill_brewer(palette = "Set1") +
  scale_x_discrete(limits = rev(levels(factor(accidentes_analisis$color_veh))))


#----------------------------------------NORMALIZACION DE VARIABLES---------------------------------------------

# Se normalizan las columnas numericas
accidentes_train$edad_pil<-accidentes_train$edad_pil/100
accidentes_train$hora_ocu<-accidentes_train$hora_ocu/23

# Se crea un conjunto para los entrenamientos y otro para los tests
t1 <- accidentes_train[(accidentes_train$estado_pil == 0),]
t2 <- accidentes_train[(accidentes_train$estado_pil == 1),]

accidentes_train<-t1[0:3000,]
accidentes_train<-rbind(accidentes_train,t2[0:2250,])

accidentes_test<-t1[3001:nrow(t1),]
accidentes_test<-rbind(accidentes_test,t2[2251:nrow(t2),])

# Se seleccionan solo los 14 colores mas comunes de carros y los 16 tipos de vehiculos mas comunes
accidentes_train<-accidentes_train[is.element(accidentes_train$color_veh, tail(names(sort(table(accidentes_train$color_veh))), 14)),]
accidentes_train<-accidentes_train[is.element(accidentes_train$tipo_veh, tail(names(sort(table(accidentes_train$tipo_veh))), 16)),]

accidentes_test<-accidentes_test[is.element(accidentes_test$color_veh, tail(names(sort(table(accidentes_train$color_veh))), 14)),]
accidentes_test<-accidentes_test[is.element(accidentes_test$tipo_veh, tail(names(sort(table(accidentes_train$tipo_veh))), 16)),]

# Se garantiza que los datos de prueba tengan los mismos niveles en las columnas categoricas
levels(accidentes_test$estado_pil) <- levels(accidentes_train$estado_pil)
levels(accidentes_test$dia_sem_ocu) <- levels(accidentes_train$dia_sem_ocu)
levels(accidentes_test$sexo_pil) <- levels(accidentes_train$sexo_pil)
levels(accidentes_test$tipo_veh) <- levels(accidentes_train$tipo_veh)
levels(accidentes_test$color_veh) <- levels(accidentes_train$color_veh)



#----------------------------------CREACION DE MODELOS-------------------------------

# Instalacion de paquetes
install.packages("neural")
install.packages("dummy")
install.packages("nnet")
install.packages("RWeka")
install.packages("neuralnet")

# Librerias necesarias
library(caret)
library(nnet)
library(RWeka)
library(neural)
library(dummy)
library(neuralnet)
library(e1071)


# Modelo 1 usando caret y nnet
modeloCaret <- train(estado_pil~., data=accidentes_train, method="nnet", trace=F)

prediccion_caret<-predict(modeloCaret, newdata = accidentes_test[,2:7])

cfmCaret<-confusionMatrix(prediccion_caret,accidentes_test$estado_pil)
cfmCaret

modeloCaret

# Modelo 2 usando caret y pcaNNet

modeloCaretPCANNet <- train(estado_pil~., data=accidentes_train, method="pcaNNet", trace=F)

prediccion_caret<-predict(modeloCaretPCANNet, newdata = accidentes_test[,2:7])

cfmCaretPCANNet<-confusionMatrix(prediccion_caret,accidentes_test$estado_pil)
cfmCaretPCANNet

modeloCaretPCANNet

# Modelo 2 usando caret y pcaNNet

modeloSVM<-svm(estado_pil~sexo_pil+dia_sem_ocu+edad_pil+color_veh+tipo_veh,data=accidentes_train )

pred<-predict(modeloSVM,accidentes_test)
cfmSVM<-confusionMatrix(pred,accidentes_test$estado_pil)
cfmSVM

modeloSVM

