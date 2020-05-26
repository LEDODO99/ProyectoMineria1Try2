#-------------------------IMPORTACION DE DATOS---------------------------


install.packages("haven")
library("haven")

accidentes_train<-read_sav("DatosAccidentes/accidentes_2009.sav")
accidentes_train<-accidentes_train[,c("estado_pil", "dia_ocu", "dia_sem_ocu", "hora_ocu", "sexo_pil","edad_pil", "tipo_vehi", "color_vehi")]
colnames(accidentes_train) <- c("estado_pil", "dia_ocu", "dia_sem_ocu", "hora_ocu", "sexo_pil","edad_pil", "tipo_veh", "color_veh")


temp_dataset<-read_sav("DatosAccidentes/accidentes_2010.sav")
temp_dataset<-temp_dataset[,c("estado_pil", "dia_ocu", "dia_sem_ocu", "hora_ocu", "sexo_pil","edad_pil", "tipo_v", "color_v")]
colnames(temp_dataset) <- c("estado_pil", "dia_ocu", "dia_sem_ocu", "hora_ocu", "sexo_pil","edad_pil", "tipo_veh", "color_veh")

accidentes_train<-rbind(accidentes_train,temp_dataset)

temp_dataset<-read_sav("DatosAccidentes/accidentes_2011.sav")
temp_dataset<-temp_dataset[,c("estado_pil", "dia_ocu", "dia_sem_ocu", "hora_ocu", "sexo_pil","edad_pil", "tipo_vehiculo", "color_vehi")]
colnames(temp_dataset) <- c("estado_pil", "dia_ocu", "dia_sem_ocu", "hora_ocu", "sexo_pil","edad_pil", "tipo_veh", "color_veh")

accidentes_train<-rbind(accidentes_train,temp_dataset)

temp_dataset<-read_sav("DatosAccidentes/accidentes_2012.sav")
temp_dataset<-temp_dataset[,c("condicion_pil", "dia_ocu", "dia_sem_ocu", "hora_ocu", "sexo_pil","edad_pil", "tipo_vehi", "color_vehi")]
colnames(temp_dataset) <- c("estado_pil", "dia_ocu", "dia_sem_ocu", "hora_ocu", "sexo_pil","edad_pil", "tipo_veh", "color_veh")

accidentes_train<-rbind(accidentes_train,temp_dataset)

temp_dataset<-read_sav("DatosAccidentes/accidentes_2013.sav")
temp_dataset<-temp_dataset[,c("estado_pil", "dia_ocu", "dia_sem_ocu", "hora_ocu", "sexo_pil","edad_pil", "tipo_veh", "color_veh")]
colnames(temp_dataset) <- c("estado_pil", "dia_ocu", "dia_sem_ocu", "hora_ocu", "sexo_pil","edad_pil", "tipo_veh", "color_veh")

accidentes_train<-rbind(accidentes_train,temp_dataset)



temp_dataset<-read_sav("DatosAccidentes/accidentes_2014.sav")
temp_dataset<-temp_dataset[,c("estado_con", "día_ocu", "día_sem_ocu", "hora_ocu", "sexo_con","edad_con", "tipo_veh", "color_veh")]
colnames(temp_dataset) <- c("estado_pil", "dia_ocu", "dia_sem_ocu", "hora_ocu", "sexo_pil","edad_pil", "tipo_veh", "color_veh")

accidentes_test<-temp_dataset





#--------------------------------LIMPIEZA DE DATOS----------------------------------


accidentes_train<-accidentes_train[(accidentes_train$estado_pil < 9),]
accidentes_train<-accidentes_train[(accidentes_train$sexo_pil < 9),]
accidentes_train<-accidentes_train[(accidentes_train$color_veh < 99),]
accidentes_train<-accidentes_train[(accidentes_train$tipo_veh < 99),]
accidentes_train<-accidentes_train[(accidentes_train$edad_pil < 999),]

accidentes_test<-accidentes_test[(accidentes_test$estado_pil < 9),]
accidentes_test<-accidentes_test[(accidentes_test$sexo_pil < 9),]
accidentes_test<-accidentes_test[(accidentes_test$color_veh < 99),]
accidentes_test<-accidentes_test[(accidentes_test$tipo_veh < 99),]
accidentes_test<-accidentes_test[(accidentes_test$edad_pil < 999),]

accidentes_train<-as.data.frame(accidentes_train)
accidentes_test<-as.data.frame(accidentes_test)

accidentes_train <- na.omit(accidentes_train)
accidentes_test <- na.omit(accidentes_test)

accidentes_train[]<-lapply(accidentes_train, factor)
accidentes_test[]<-lapply(accidentes_test, factor)

accidentes_train$edad_pil<-as.numeric(accidentes_train$edad_pil)
accidentes_train$dia_ocu<-as.numeric(accidentes_train$dia_ocu)
accidentes_train$hora_ocu<-as.numeric(accidentes_train$hora_ocu)

accidentes_test$edad_pil<-as.numeric(accidentes_test$edad_pil)
accidentes_test$dia_ocu<-as.numeric(accidentes_test$dia_ocu)
accidentes_test$hora_ocu<-as.numeric(accidentes_test$hora_ocu)

accidentes_train$hora_ocu<-ifelse(accidentes_train$hora_ocu == 24, 0,accidentes_train$hora_ocu)

accidentes_test$hora_ocu<-ifelse(accidentes_test$hora_ocu == 24, 0,accidentes_test$hora_ocu)


accidentes_train$edad_pil<-accidentes_train$edad_pil/100
accidentes_train$dia_ocu<-accidentes_train$dia_ocu/31
accidentes_train$hora_ocu<-accidentes_train$hora_ocu/23

accidentes_test$edad_pil<-accidentes_test$edad_pil/100
accidentes_test$dia_ocu<-accidentes_test$dia_ocu/31
accidentes_test$hora_ocu<-accidentes_test$hora_ocu/23

levels(accidentes_test$estado_pil) <- levels(accidentes_train$estado_pil)
levels(accidentes_test$dia_sem_ocu) <- levels(accidentes_train$dia_sem_ocu)
levels(accidentes_test$sexo_pil) <- levels(accidentes_train$sexo_pil)
levels(accidentes_test$tipo_veh) <- levels(accidentes_train$tipo_veh)
levels(accidentes_test$color_veh) <- levels(accidentes_train$color_veh)


#----------------------------------CREACION DE MODELO-------------------------------
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


# Modelo 1 usando nnet
modeloCaret <- train(estado_pil~., data=accidentes_train, method="nnet", trace=F)

prediccion_caret<-predict(modeloCaret, newdata = accidentes_test[,2:3])

cfmCaret<-confusionMatrix(prediccion_caret,accidentes_test$estado_pil)
cfmCaret



modeloSVM_1<-svm(estado_pil~., data=accidentes_train, cost=2^5, kernel="linear")

prediccion_svm<-predict(modeloSVM_1,newdata=accidentes_test[,2:4])
confusionMatrix(prediccion_svm,accidentes_test$estado_pil)



#porcentaje de hechos que tienen un coonfirmado ebrio o no ebrio
require(neuralnet)

str(accidentes_train)
str(provisional)
provisional=accidentes_train
head(model.matrix(~dia_sem_ocu,data = provisional))
provisional$dia_sem_ocu<-relevel(provisional$dia_sem_ocu,ref="2")
provisional$sexo_pil<-relevel(provisional$sexo_pil,ref="2")
provisional$tipo_veh<-relevel(provisional$tipo_veh,ref="2")
provisional$color_veh<-relevel(provisional$color_veh,ref="2")

m=model.matrix(~dia_sem_ocu+sexo_pil+edad_pil+tipo_veh+color_veh,data=provisional)
nn<-neuralnet(estado_pil~as.numeric(levels(sexo_pil))[sexo_pil]+dia_sem_ocu+edad_pil+color_veh+tipo_veh , data=accidentes_train, hidden=2)

install.packages("e1071")
library(e1071)
install.packages("caret")
library(caret)
a<-svm(estado_pil~sexo_pil+dia_sem_ocu+edad_pil+color_veh+tipo_veh,data=accidentes_train )

summary(a)

pred<-predict(a,accidentes_test)
summary(pred)
summary(accidentes_test)
matrix<-confusionMatrix(pred,accidentes_test$estado_pil)
matrix


a<-svm(estado_pil~sexo_pil+dia_sem_ocu+edad_pil+color_veh+tipo_veh,data=accidentes_train )
pred<-predict(a,accidentes_test)
matrix<-confusionMatrix(pred,accidentes_test$estado_pil)
matrix


a<-svm(estado_pil~dia_sem_ocu+dia_ocu+hora_ocu,data=accidentes_train )
pred<-predict(a,accidentes_test)
matrix<-confusionMatrix(pred,accidentes_test$estado_pil)
matrix

a<-svm(estado_pil~sexo_pil+edad_pil,data=accidentes_train )
pred<-predict(a,accidentes_test)
matrix<-confusionMatrix(pred,accidentes_test$estado_pil)
matrix


a<-svm(estado_pil~hora_ocu,data=accidentes_train )
pred<-predict(a,accidentes_test)
matrix<-confusionMatrix(pred,accidentes_test$estado_pil)
matrix
