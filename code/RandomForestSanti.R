library(randomForest)
library(spdep)
library(ape)
library(caret)
library(dplyr)
library(sampling)

#Establecer directorio de trabajo
setwd("/Users/Santi/Documents/Ctfc/TESIS/5_RESILFOR/1_INCENDIS/4_ESTUDIO_COBERTURA/ESTADISTICA/R")

#Importar tabla de datos
datafile="Data_filt50_RF.txt"
data_types=read.table(datafile, header = TRUE, sep="\t", dec=",")

#Definir el grupo como factor
data_types$TYPE<-as.factor(data_types$TYPE)
colnames(data_types)
cor(data_types[,c(2:9)], use="complete.obs", method="kendall")
correl_pearson <- cor(data_types[,c(2:9)], use="complete.obs", method="pearson")
cor(data_types[,c(2:9)], use="complete.obs", method="spearman")
as.matrix(correl_pearson)

cor.test(data_types$UNB_DIST, data_types$UNB_AREA, alternative = "two.sided", method = "pearson", exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(data_types$CANOP95, data_types$NORTH, alternative = "two.sided", method = "pearson", exact = NULL, conf.level = 0.95, continuity = FALSE)

##de momento no hace falta# data_types_filt <- downSample(data_types, data_types$TYPE)

##reclasificar tabla para que TYPE1=1 y el resto=0##

data_types_1toall <- data_types
data_types_1toall$TYPEbin <- ifelse(data_types_1toall$TYPE==1,1,0)
data_types_1toall$TYPEbin<-as.factor(data_types_1toall$TYPEbin)
summary(data_types_1toall$TYPEbin)

#########Ejecucion de random forest para TYPE 1#########

rf_T1<-randomForest(data_types_1toall$TYPEbin ~ ., data=data_types_1toall[,c(2:10)], ntree=10000, importance=T, replace=TRUE, classwt=c(0.9,0.1),strata=data_types_1toall$TYPE, sampsize=c(400,100,100,100,100))
rf_T1

#Importancia de las variables
importance(rf_T1)
as.data.frame(rf_T1$importanceSD)
varImpPlot(rf_T1)

#Repeticion ejecucion de random forest con variables con MIR>=0.2
imp<-importance(rf_T1)
colnames(imp)
importance(rf_T1)[,3]
max(importance(rf_T1)[,3])
I = importance(rf_T1)[,3]
MIR = I/max(I)
MIR

#########Repeticion ejecucion de random forest para TYPE 2#########

##reclasificar tabla para que TYPE2=1 y el resto=0##

data_types_2toall <- data_types
data_types_2toall$TYPEbin <- ifelse(data_types_2toall$TYPE==2,1,0)
data_types_2toall$TYPEbin<-as.factor(data_types_2toall$TYPEbin)
summary(data_types_2toall$TYPEbin)
summary(data_types_2toall$TYPE)

#########Ejecucion de random forest para TYPE 2#########

rf_T2<-randomForest(data_types_2toall$TYPEbin ~ ., data=data_types_2toall[,c(2:10)], ntree=10000, importance=T, replace=TRUE, classwt=c(0.9,0.1),strata=data_types_2toall$TYPE, sampsize=c(100,400,100,100,100))
rf_T2

#Importancia de las variables
importance(rf_T2)
varImpPlot(rf_T2)
varImpPlot(rf_T2)

#Calculo MIR
imp<-importance(rf_T2)
colnames(imp)
importance(rf_T2)[,3]
max(importance(rf_T2)[,3])
I = importance(rf_T2)[,3]
MIR = I/max(I)
MIR

#########Repeticion ejecucion de random forest para TYPE 3#########

##reclasificar tabla para que TYPE3=1 y el resto=0##

data_types_3toall <- data_types
data_types_3toall$TYPEbin <- ifelse(data_types_3toall$TYPE==3,1,0)
data_types_3toall$TYPEbin<-as.factor(data_types_3toall$TYPEbin)
summary(data_types_3toall$TYPEbin)
summary(data_types_3toall$TYPE)

#########Ejecucion de random forest para TYPE 3#########

rf_T3<-randomForest(data_types_3toall$TYPEbin ~ ., data=data_types_3toall[,c(2:10)], ntree=10000, importance=T, replace=TRUE, classwt=c(0.9,0.1),strata=data_types_3toall$TYPE, sampsize=c(100,100,400,100,100))
rf_T3

#Importancia de las variables
importance(rf_T3)
varImpPlot(rf_T3)

#Calculo MIR
imp<-importance(rf_T3)
colnames(imp)
importance(rf_T3)[,3]
max(importance(rf_T3)[,3])
I = importance(rf_T3)[,3]
MIR = I/max(I)
MIR

#########Repeticion ejecucion de random forest para TYPE 4#########

##reclasificar tabla para que TYPE4=1 y el resto=0##

data_types_4toall <- data_types
data_types_4toall$TYPEbin <- ifelse(data_types_4toall$TYPE==4,1,0)
data_types_4toall$TYPEbin<-as.factor(data_types_4toall$TYPEbin)
summary(data_types_4toall$TYPEbin)
summary(data_types_4toall$TYPE)

#########Ejecucion de random forest para TYPE 4#########

rf_T4<-randomForest(data_types_4toall$TYPEbin ~ ., data=data_types_4toall[,c(2:10)], ntree=10000, importance=T, replace=TRUE, classwt=c(0.9,0.1),strata=data_types_4toall$TYPE, sampsize=c(100,100,100,400,100))
rf_T4

#Importancia de las variables
importance(rf_T4)
varImpPlot(rf_T4)

#Calculo MIR
imp<-importance(rf_T4)
colnames(imp)
importance(rf_T4)[,3]
max(importance(rf_T4)[,3])
I = importance(rf_T4)[,3]
MIR = I/max(I)
MIR

#########Repeticion ejecucion de random forest para TYPE 5#########

##reclasificar tabla para que TYPE4=1 y el resto=0##

data_types_5toall <- data_types
data_types_5toall$TYPEbin <- ifelse(data_types_5toall$TYPE==5,1,0)
data_types_5toall$TYPEbin<-as.factor(data_types_5toall$TYPEbin)
summary(data_types_5toall$TYPEbin)
summary(data_types_5toall$TYPE)

#########Ejecucion de random forest para TYPE 4#########

rf_T5<-randomForest(data_types_5toall$TYPEbin ~ ., data=data_types_5toall[,c(2:10)], ntree=10000, importance=T, replace=TRUE, classwt=c(0.9,0.1),strata=data_types_5toall$TYPE, sampsize=c(100,100,100,100,400))
rf_T5

#Importancia de las variables
importance(rf_T5, scale=TRUE)
as.data.frame(rf_T5$importanceSD)
varImpPlot(rf_T5)

#Calculo MIR
imp<-importance(rf_T5)
colnames(imp)
importance(rf_T5)[,3]
max(importance(rf_T5)[,3])
I = importance(rf_T5)[,3]
MIR = I/max(I)
MIR

#########MODELIZACION DE LA REGENERACION 5 TIPOS DOS A DOS#########

#1 contra 2#

datafile1y2="Data_filt50_RF_T1y2.txt"
data_types1y2=read.table(datafile1y2, header = TRUE, sep="\t", dec=",")

data_types1y2$TYPE<-as.factor(data_types1y2$TYPE)
dim(data_types1y2)

rf_1y2<-randomForest(data_types1y2$TYPE ~ ., data = data_types1y2[,c(2:10)], ntree=5000, importance=T, replace=TRUE, strata=data_types1y2$TYPE, sampsize=rep(700,2))
rf_1y2

importance(rf_1y2)
varImpPlot(rf_1y2)

imp<-importance(rf_1y2)
colnames(imp)
importance(rf_1y2)[,3]
max(importance(rf_1y2)[,3])
I = importance(rf_1y2)[,3]
MIR = I/max(I)
MIR

#1 contra 3#

datafile1y3="Data_filt50_RF_T1y3.txt"
data_types1y3=read.table(datafile1y3, header = TRUE, sep="\t", dec=",")

data_types1y3$TYPE<-as.factor(data_types1y3$TYPE)
dim(data_types1y3)

rf_1y3<-randomForest(data_types1y3$TYPE ~ ., data = data_types1y3[,c(2:10)], ntree=5000, importance=T, replace=TRUE, strata=data_types1y3$TYPE, sampsize=rep(700,2))
rf_1y3

importance(rf_1y3)
varImpPlot(rf_1y3)

imp<-importance(rf_1y3)
colnames(imp)
importance(rf_1y3)[,3]
max(importance(rf_1y3)[,3])
I = importance(rf_1y3)[,3]
MIR = I/max(I)
MIR

#1 contra 4#

datafile1y4="Data_filt50_RF_T1y4.txt"
data_types1y4=read.table(datafile1y4, header = TRUE, sep="\t", dec=",")

data_types1y4$TYPE<-as.factor(data_types1y4$TYPE)
dim(data_types1y4)

rf_1y4<-randomForest(data_types1y4$TYPE ~ ., data = data_types1y4[,c(2:10)], ntree=5000, importance=T, replace=TRUE, strata=data_types1y4$TYPE, sampsize=rep(700,2))
rf_1y4

importance(rf_1y4)
varImpPlot(rf_1y4)

imp<-importance(rf_1y4)
colnames(imp)
importance(rf_1y4)[,3]
max(importance(rf_1y4)[,3])
I = importance(rf_1y4)[,3]
MIR = I/max(I)
MIR

#1 contra 5#

datafile1y5="Data_filt50_RF_T1y5.txt"
data_types1y5=read.table(datafile1y5, header = TRUE, sep="\t", dec=",")

data_types1y5$TYPE<-as.factor(data_types1y5$TYPE)
dim(data_types1y5)
summary(data_types1y5$TYPE)

rf_1y5<-randomForest(data_types1y5$TYPE ~ ., data = data_types1y5[,c(2:10)], ntree=5000, importance=T, replace=TRUE, strata=data_types1y5$TYPE, sampsize=rep(700,2))
rf_1y5

importance(rf_1y5)
varImpPlot(rf_1y5)

imp<-importance(rf_1y5)
colnames(imp)
importance(rf_1y5)[,3]
max(importance(rf_1y5)[,3])
I = importance(rf_1y5)[,3]
MIR = I/max(I)
MIR

#2 contra 3#

datafile2y3="Data_filt50_RF_T2y3.txt"
data_types2y3=read.table(datafile2y3, header = TRUE, sep="\t", dec=",")

data_types2y3$TYPE<-as.factor(data_types2y3$TYPE)
dim(data_types2y3)

rf_2y3<-randomForest(data_types2y3$TYPE ~ ., data = data_types2y3[,c(2:10)], ntree=5000, importance=T, replace=TRUE, strata=data_types2y3$TYPE, sampsize=rep(700,2))
rf_2y3

importance(rf_2y3)
varImpPlot(rf_2y3)

imp<-importance(rf_2y3)
colnames(imp)
importance(rf_2y3)[,3]
max(importance(rf_2y3)[,3])
I = importance(rf_2y3)[,3]
MIR = I/max(I)
MIR


#2 contra 4#

datafile2y4="Data_filt50_RF_T2y4.txt"
data_types2y4=read.table(datafile2y4, header = TRUE, sep="\t", dec=",")

data_types2y4$TYPE<-as.factor(data_types2y4$TYPE)
dim(data_types2y4)

rf_2y4<-randomForest(data_types2y4$TYPE ~ ., data = data_types2y4[,c(2:10)], ntree=5000, importance=T, replace=TRUE, strata=data_types2y4$TYPE, sampsize=rep(700,2))
rf_2y4

importance(rf_2y4)
varImpPlot(rf_2y4)

imp<-importance(rf_2y4)
colnames(imp)
importance(rf_2y4)[,3]
max(importance(rf_2y4)[,3])
I = importance(rf_2y4)[,3]
MIR = I/max(I)
MIR

#2 contra 5#

datafile2y5="Data_filt50_RF_T2y5.txt"
data_types2y5=read.table(datafile2y5, header = TRUE, sep="\t", dec=",")

data_types2y5$TYPE<-as.factor(data_types2y5$TYPE)
dim(data_types2y5)

rf_2y5<-randomForest(data_types2y5$TYPE ~ ., data = data_types2y5[,c(2:10)], ntree=5000, importance=T, replace=TRUE, strata=data_types2y5$TYPE, sampsize=rep(700,2))
rf_2y5

importance(rf_2y5)
varImpPlot(rf_2y5)

imp<-importance(rf_2y5)
colnames(imp)
importance(rf_2y5)[,3]
max(importance(rf_2y5)[,3])
I = importance(rf_2y5)[,3]
MIR = I/max(I)
MIR

#3 contra 4#

datafile3y4="Data_filt50_RF_T3y4.txt"
data_types3y4=read.table(datafile3y4, header = TRUE, sep="\t", dec=",")

data_types3y4$TYPE<-as.factor(data_types3y4$TYPE)
dim(data_types3y4)

rf_3y4<-randomForest(data_types3y4$TYPE ~ ., data = data_types3y4[,c(2:10)], ntree=5000, importance=T, replace=TRUE, strata=data_types3y4$TYPE, sampsize=rep(700,2))
rf_3y4

importance(rf_3y4)
varImpPlot(rf_3y4)

imp<-importance(rf_3y4)
colnames(imp)
importance(rf_3y4)[,3]
max(importance(rf_3y4)[,3])
I = importance(rf_3y4)[,3]
MIR = I/max(I)
MIR

#3 contra 5#

datafile3y5="Data_filt50_RF_T3y5.txt"
data_types3y5=read.table(datafile3y5, header = TRUE, sep="\t", dec=",")

data_types3y5$TYPE<-as.factor(data_types3y5$TYPE)
dim(data_types3y5)

rf_3y5<-randomForest(data_types3y5$TYPE ~ ., data = data_types3y5[,c(2:10)], ntree=5000, importance=T, replace=TRUE, strata=data_types3y5$TYPE, sampsize=rep(700,2))
rf_3y5

importance(rf_3y5)
varImpPlot(rf_3y5)

imp<-importance(rf_3y5)
colnames(imp)
importance(rf_3y5)[,3]
max(importance(rf_3y5)[,3])
I = importance(rf_3y5)[,3]
MIR = I/max(I)
MIR

#4 contra 5#

datafile4y5="Data_filt50_RF_T4Y5.txt"
data_types4y5=read.table(datafile4y5, header = TRUE, sep="\t", dec=",")

data_types4y5$TYPE<-as.factor(data_types4y5$TYPE)
dim(data_types4y5)

rf_4y5<-randomForest(data_types4y5$TYPE ~ ., data = data_types4y5[,c(2:10)], ntree=5000, importance=T, replace=TRUE, strata=data_types4y5$TYPE, sampsize=rep(700,2))
rf_4y5

importance(rf_4y5)
varImpPlot(rf_4y5)

imp<-importance(rf_4y5)
colnames(imp)
importance(rf_4y5)[,3]
max(importance(rf_4y5)[,3])
I = importance(rf_4y5)[,3]
MIR = I/max(I)
MIR

#########Repeticion ejecucion de random forest para TYPE 2 en orientaciones SUR#########

#Importar tabla de datos
datafileT2="Data_filt50_RF_T2.txt"
data_type2=read.table(datafileT2, header = TRUE, sep="\t", dec=",")

#Definir el grupo como factor
data_type2$TYPE2<-as.factor(data_type2$TYPE2)

###
data_type2_South<-data_type2[which(data_type2$NORTH < -0.2), ]
summary(data_type2_South$TYPE2)

#Primera ejecuci?n de random forest con las variables preseleccionadas
rf_T2_South<-randomForest(data_type2_South$TYPE2 ~ ., data=data_type2_South[,c(2:10)], ntree=5000, importance=T, replace=TRUE, strata=data_type2_South$TYPE2, sampsize=rep(800,2))
rf_T2_South

#Importancia de las variables
importance(rf_T2_South)
varImpPlot(rf_T2_South)

#Calculo MIR
imp<-importance(rf_T2_South)
colnames(imp)
importance(rf_T2_South)[,3]
max(importance(rf_T2_South)[,3])
I = importance(rf_T2_South)[,3]
MIR = I/max(I)
MIR

#########Repeticion ejecucion de random forest para TYPE 2 en orientaciones NORTE#########

#Importar tabla de datos
datafileT2="Data_filt50_RF_T2.txt"
data_type2=read.table(datafileT2, header = TRUE, sep="\t", dec=",")

#Definir el grupo como factor
data_type2$TYPE2<-as.factor(data_type2$TYPE2)

###
data_type2_North<-data_type2[which(data_type2$NORTH > 0.2), ]
summary(data_type2_North$TYPE2)

#Primera ejecuci?n de random forest con las variables preseleccionadas
rf_T2_North<-randomForest(data_type2_North$TYPE2 ~ ., data=data_type2_North[,c(2:10)], ntree=5000, importance=T, replace=TRUE, strata=data_type2_North$TYPE2, sampsize=rep(450,2))
rf_T2_North

#Importancia de las variables
importance(rf_T2_North)
varImpPlot(rf_T2_North)

#Calculo MIR
imp<-importance(rf_T2_North)
colnames(imp)
importance(rf_T2_North)[,3]
max(importance(rf_T2_North)[,3])
I = importance(rf_T2_North)[,3]
MIR = I/max(I)
MIR

#########Repeticion ejecucion de random forest para TYPE 3 en orientaciones SUR#########

#Importar tabla de datos
datafileT3="Data_filt50_RF_T3.txt"
data_type3=read.table(datafileT3, header = TRUE, sep="\t", dec=",")

#Definir el grupo como factor
data_type3$TYPE3<-as.factor(data_type3$TYPE3)

###
data_type3_South<-data_type3[which(data_type3$NORTH < -0.2), ]
summary(data_type3_South$TYPE3)

#Primera ejecuci?n de random forest con las variables preseleccionadas
rf_T3_South<-randomForest(data_type3_South$TYPE3 ~ ., data=data_type3_South[,c(2:10)], ntree=5000, importance=T, replace=TRUE, strata=data_type3_South$TYPE3, sampsize=rep(400,2))
rf_T3_South

#Importancia de las variables
importance(rf_T3_South)
varImpPlot(rf_T3_South)

#Calculo MIR
imp<-importance(rf_T3_South)
colnames(imp)
importance(rf_T3_South)[,3]
max(importance(rf_T3_South)[,3])
I = importance(rf_T3_South)[,3]
MIR = I/max(I)
MIR

#########Repeticion ejecucion de random forest para TYPE 3 en orientaciones NORTE#########

#Importar tabla de datos
datafileT3="Data_filt50_RF_T3.txt"
data_type3=read.table(datafileT3, header = TRUE, sep="\t", dec=",")

#Definir el grupo como factor
data_type3$TYPE3<-as.factor(data_type3$TYPE3)

###
data_type3_North<-data_type3[which(data_type3$NORTH > 0.2), ]
summary(data_type3_North$TYPE3)

#Primera ejecuci?n de random forest con las variables preseleccionadas
rf_T3_North<-randomForest(data_type3_North$TYPE3 ~ ., data=data_type3_North[,c(2:10)], ntree=5000, importance=T, replace=TRUE, strata=data_type3_North$TYPE3, sampsize=rep(600,2))
rf_T3_North

#Importancia de las variables
importance(rf_T3_North)
varImpPlot(rf_T3_North)

#Calculo MIR
imp<-importance(rf_T3_North)
colnames(imp)
importance(rf_T3_North)[,3]
max(importance(rf_T3_North)[,3])
I = importance(rf_T3_North)[,3]
MIR = I/max(I)
MIR


