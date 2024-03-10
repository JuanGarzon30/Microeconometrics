#INTRODUCCIÓN----

#Estimado lector, he aquí un código para estimar el CAPM de Toyota en sus cotizaciones en Japón. 

#Algunas recomendaciones antes de iniciar:

#El código corre completamente solo (puede comprobarlo), sin embargo, es mejor si lo corre por secciones para que pueda leer las anotaciones y ver los objetos que se van creando.

#Los objetos guardados como resumen, son listas que almacenan gráficas o data frames donde se hace resumen de muchos campos, se guardan así para mantener un orden, en caso de que quiera verlas dirijase, llamelas con el comando View.

#Aún no está completamente flexible, sin embargo está en proceso de mejora.

#Estamos atentos a cualquier recomendación que pueda mejorar este código.
#PREÁMBULO ---------------------------------------------------------------

##Llamar las librerias----

#Por favor revise si las librerias a continuación están instaladas en su dispositivo, de lo contrario instálelas con el comando install.packages("Nombre de la libreria"), ej: install.packages("lsr")

library(readxl)
library(dplyr)
library(ggplot2)
library(lsr)
library(DescToolsAddIns)
library(ggpubr)
library(plotrix)

##Importar el archivo de datos----
##Importante: la base de datos a usar debe tener como primera fila los titulos de cada columna, si no tiene titulos, sino que la primera columna ya es parte de la base de datos, estos se guardarían como el título de las columnas en el data frame y los cálculos serán incorrectos
##Importante que quede guardado como data frame, porque despues se usan funciones que extraen argumentos de este tipo de objeto y no funcionarian con otro, ej: una matriz.
# Especificar la ruta del archivo importar
path <-normalizePath(file.choose())

# Leer todas las hojas del archivo Excel
toyotadata <- excel_sheets(path)

# Crear un bucle para leer cada hoja y guardarla como un objeto separado

#La función assign guarda para un nombre del primer argumento la función que le digamos en el segundo argumento, en este caso, al estar dentro de un for le estamos pidiendo que para cada hoja que extrajo el archivo de excel toyotadata, asigne un objeto con ese mismo nombre y sea guardado como un dataframe

for (i in 1:length(toyotadata)) {
  # Leer cada hoja del archivo Excel y guardarla como un objeto separado
  assign(toyotadata[i], as.data.frame(read_excel(path, sheet = i)))
}


#DEFINICIÓN DE FUNCIONES-------------------------------------------------

##Código para definir las funciones usadas posteriormente

##Media 
media<-function(x){
  N<-(length(na.omit(x)))
  media<-(sum(na.omit(x)/N))
  return(as.numeric(media))
  #La función na.omit es usada aquí y en adelante para omitir los valores que se hallen como NA, permitiendo  tener bases de datos con números de observaciones diferentes pero haciendo que el código flexible funcione
}
##Varianza y desviación estándar
varianza<-function(x){
  N<-(length(na.omit(x)))
  varianza<-(sum((na.omit(x)-media(x))^2))/(N-1)
  return(varianza)
}
desviacion<-function(x){
  desviacion<-(sqrt(varianza(x)))
  return(desviacion)
}
covarianza<-function(x,y){
  N<-(length(na.omit(x)))
  sum((x-media(x))*(y-media(y)), na.rm = TRUE)/N
}
##Cuartiles 
cuartiles<-function(x){
  temporal<-quantile(x,prob=c(0,0.25,0.5,0.75,1), na.rm=TRUE)
  return(temporal)
}
##Suma de cuadradros
ssc<-function(x){
  ssc<-sum(((x-media(x)))^2)
  return(ssc)}
##Betas----
B1 <- function(x,y){
  covarianza(x,y)/varianza(x)
} 
B0 <- function(x,y){
  media(y)-B1(x,y)*media(x)
}


##Estadistica descriptiva
festdes<-function(x){
  Estadística.descriptiva<-c(length(na.omit(x)),media(x), varianza(x), desviacion(x), cuartiles(x))
  return(as.data.frame(x=Estadística.descriptiva, row.names=c("Cantidad de datos",   "Media", "Varianza", "Desviación estándar", "Minímo (0%)", "Cuantil 1 (25%)", "Mediana (50%)", "Cuantil 3 (75%)", "Máximo (100%)" )))
}

#MODELO A ESTIMAR----
#Modelo a estimar

#Ri-Rf = B0 + B1(Rm - Rf)
#DIA----
rownames(DiaL) <- DiaL[,1]
DiaL<-DiaL[,-1]
NvarDiaL<-ncol(DiaL)
##CorDiaL----
CorDiaL<-as.data.frame(matrix(data=NA, ncol=ncol(DiaL), nrow=ncol(DiaL)), do.NULL=FALSE)
rownames(CorDiaL)<-colnames(DiaL)
colnames(CorDiaL)<-colnames(DiaL)

for(i in 1:NvarDiaL){
  for(j in 1:NvarDiaL)
  {CorDiaL[i,j]<-cor(x=(DiaL[,i]), y=(DiaL[,j]), method="spearman", use="na.or.complete") 
  }
}
CORRELACIONDiaL<-CorDiaL

##Est.des.DiaL ------------------------------------------------------------
Est.des.DiaL<-data.frame(
  x=matrix(data=NA,nrow=9, ncol=NvarDiaL),row.names=c("Cantidad de datos", "Media", "Varianza","Desviacion estándar", "Minímo (0%)", "Cuantil 1 (25%)", "Mediana (50%)", "Cuantil 3 (75%)", "Máximo (100%)"))
colnames(Est.des.DiaL)<-colnames(DiaL)

for(i in 1:NvarDiaL){
  Est.des.DiaL[,i]<-festdes(DiaL[,i])
}

RiDiaL <- DiaL$"Var7203"
RfDiaL <- DiaL$"VarJP10"
RmDiaL <- DiaL$"VarNKY"
#Convertir a X y Y (Var globales)
YDiaL <<- RiDiaL - RfDiaL
XDiaL <<- RmDiaL - RfDiaL

##Boxplot DiaL----
dfDiaL<-(data.frame(x=RiDiaL, y=RmDiaL, z=RfDiaL))
boxplotRiDiaL <- ggplot(dfDiaL, aes(x = "RiDiaL", y=RiDiaL)) +
  stat_boxplot(geom = "errorbar",
               width = 0.25) +
  geom_boxplot(fill = "#ef2b0c",
               colour = "#665956",
               alpha = 0.5,
               outlier.colour = "black")+ ggtitle("    Boxplot rentabilidad Toyota periodicidad diaria") + scale_y_continuous(limits = c(-0.15,0.15))

boxplotRmDiaL <- ggplot(dfDiaL, aes(x = "RmDiaL", y=RmDiaL)) +
  stat_boxplot(geom = "errorbar",
               width = 0.25) +
  geom_boxplot(fill = "#ef2b0c",
               colour = "#665956",
               alpha = 0.5,
               outlier.colour = "black")+ ggtitle("Boxplot rentabilidad Nikkei 225 periodicidad diaria") + scale_y_continuous(limits = c(-0.15,0.15))

boxplotRfDiaL <- ggplot(dfDiaL, aes(x = "RfDiaL", y=RfDiaL)) +
  stat_boxplot(geom = "errorbar",
               width = 0.25) +
  geom_boxplot(fill = "#ef2b0c",
               colour = "#665956",
               alpha = 0.5,
               outlier.colour = "black")+ ggtitle("Boxplot rentabilidad bonos japoneses 10 años periodicidad diaria") + scale_y_continuous(limits = c(-0.15,0.15))
#la mayoria de datos se encuentran cercanos al 0.0005 pero se ve así ya que se está manejando la misma escala en el eje y para los 3

ggarrange(boxplotRiDiaL, boxplotRmDiaL, boxplotRfDiaL, ncol = 3, nrow = 1, 
          common.legend = TRUE, legend = "bottom")
##Estimaciones----
###YestDiaL----
B0DiaL<-B0(XDiaL,YDiaL)
B1DiaL<-B1(XDiaL,YDiaL)
BetaDiaL<-c(B0DiaL, B1DiaL)
YestDiaL <<- B0(XDiaL,YDiaL)+B1(XDiaL,YDiaL)*XDiaL



###EestDiaL----
EestDiaL <<- YDiaL - YestDiaL
###GráficaDiaL----

graficaDiaL <- plot(x=XDiaL, y=YDiaL, main = "CAPM diario Toyota vs Nikkei", xlab = "Rendimiento Nikkei vs Bono Japón 10Y", ylab = "Rendimiento Toyota vs Bono Japón 10y",col="#EB0A1E",pch=19,cex=0.25)
abline(B0(XDiaL,YDiaL), B1(XDiaL,YDiaL), col="#2F4F4F")


grafica.evsx.DiaL <- plot(x=XDiaL, y=EestDiaL, main = "Error estimado al cuadrado vs (Rm-Rf) Diario", xlab = "Rendimiento Nikkei vs Bono Japón 10Y", ylab = "Error estimado del modelo", col= "#2F4F4F")

grafica.e2vsx.DiaL <- plot(x=XDiaL, y=EestDiaL^2, main = "Error estimado al cuadrado vs (Rm-Rf) Diario", xlab = "Rendimiento Nikkei vs Bono Japón 10Y", ylab = "Error estimado del modelo", col= "#2F4F4F")
c("graficaDiaL","grafica.evsx.DiaL", "grafica.e2vsx.DiaL")

###Bondad de ajuste DiaL----

#STC
STCDiaL <- ssc(YDiaL)
#SRC
SRCDiaL <- round(ssc(EestDiaL),3)
#SEC
SECDiaL <- round(ssc(YestDiaL),3)
#R^2
R2DiaL <- SECDiaL/STCDiaL

#Piechart
valoresPieDiaL <- round(c(SRCDiaL,SECDiaL),3)
par(bg = "#bcbcbc")
pieDiaL<-pie3D(valoresPieDiaL, labels = c(SRCDiaL,SECDiaL), col = c("#EB0A1E", "#2F4F4F"), main = "Piechart R2 Diario", line = -1)
legend("bottomright", c("SRC", "SEC"), fill = c("#EB0A1E", "#2F4F4F"), bg = "#bcbcbc")

###Resumen gráficos DiaL----


Lista.graf.DiaL<-list(graficaDiaL,grafica.evsx.DiaL, grafica.e2vsx.DiaL, pieDiaL)
names(Lista.graf.DiaL)<-c("graficaDiaL","grafica.evsx.DiaL", "grafica.e2vsx.DiaL", "pieDiaL")
saveRDS(Lista.graf.DiaL, "Lista.graf.DiaL")

#Mes----
RiMesL <- MesL$"Var7203"
RfMesL <- MesL$"VarJP10"
RmMesL <- MesL$"VarNKY"
#Convertir a X y Y (Var globales)
YMesL <<- RiMesL - RfMesL
XMesL <<- RmMesL - RfMesL
rownames(MesL) <- MesL[,1]
MesL<-MesL[,-1]
NvarMesL<-ncol(MesL)
##CorMesL----
CorMesL<-as.data.frame(matrix(data=NA, ncol=ncol(MesL), nrow=ncol(MesL)), do.NULL=FALSE)
rownames(CorMesL)<-colnames(MesL)
colnames(CorMesL)<-colnames(MesL)

for(i in 1:NvarMesL){
  for(j in 1:NvarMesL)
  {CorMesL[i,j]<-cor(x=(MesL[,i]), y=(MesL[,j]), method="spearman", use="na.or.complete") 
  }
}
CORRELACIONMesL<-CorMesL
##Est.des.MesL ------------------------------------------------------------
Est.des.MesL<-data.frame(
  x=matrix(data=NA,nrow=9, ncol=NvarMesL),row.names=c("Cantidad de datos", "Media", "Varianza","Desviacion estándar", "Minímo (0%)", "Cuantil 1 (25%)", "Mediana (50%)", "Cuantil 3 (75%)", "Máximo (100%)"))
colnames(Est.des.MesL)<-colnames(MesL)

for(i in 1:NvarMesL){
  Est.des.MesL[,i]<-festdes(MesL[,i])
}
##Boxplot MesL----

dfMesL<-(data.frame(x=RiMesL, y=RmMesL, z=RfMesL))
boxplotRiMesL <- ggplot(dfMesL, aes(x = "RiMesL", y=RiMesL)) +
  stat_boxplot(geom = "errorbar",
               width = 0.25) +
  geom_boxplot(fill = "#EB0A1E",
               colour = "black",
               alpha = 0.5,
               outlier.colour = "black")+ ggtitle("Boxplot rentabilidad Toyota periodicidad mensual") + scale_y_continuous(limits = c(-0.30,0.30))

boxplotRmMesL <- ggplot(dfMesL, aes(x = "RmMesL", y=RmMesL)) +
  stat_boxplot(geom = "errorbar",
               width = 0.25) +
  geom_boxplot(fill = "#EB0A1E",
               colour = "black",
               alpha = 0.5,
               outlier.colour = "black")+ ggtitle("Boxplot rentabilidad Nikkei 225 periodicidad mensual") + scale_y_continuous(limits = c(-0.30,0.30))

boxplotRfMesL <- ggplot(dfMesL, aes(x = "RfMesL", y=RfMesL)) +
  stat_boxplot(geom = "errorbar",
               width = 0.25) +
  geom_boxplot(fill = "#EB0A1E",
               colour = "black",
               alpha = 0.5,
               outlier.colour = "black")+ ggtitle("Boxplot rentabilidad bonos japoneses 10 años periodicidad mensual") + scale_y_continuous(limits = c(-0.30,0.30))

ggarrange(boxplotRiMesL, boxplotRmMesL, boxplotRfMesL, ncol = 3, nrow = 1, 
          common.legend = TRUE, legend = "bottom")

##Estimación MesL----

RiMesL <- MesL$"Var7203"
RfMesL <- MesL$"VarJP10"
RmMesL <- MesL$"VarNKY"
#Convertir a X y Y (Var globales)
YMesL <<- RiMesL - RfMesL
XMesL <<- RmMesL - RfMesL

###Yest----
B0MesL<-B0(XMesL,YMesL)
B1MesL<-B1(XMesL,YMesL)
BetaMesL<-c(B0MesL, B1MesL)

YestMesL <<- B0(XMesL,YMesL)+B1(XMesL,YMesL)*XMesL

###EestMesL----

EestMesL <<- YMesL - YestMesL

###GráficaMesL----

graficaMesL <- plot(x=XMesL, y=YMesL, main = "CAPM Mensual Toyota vs Nikkei", xlab = "Rendimiento Nikkei vs Bono Japón 10Y", ylab = "Rendimiento Toyota vs Bono Japón 10y",col="#FF0022",pch=19,cex=0.5)
abline(B0(XMesL,YMesL), B1(XMesL,YMesL), col="#3d85c6")


grafica.evsx.MesL <- plot(x=XMesL, y=EestMesL, main = "Error vs (Rm-Rf) Mensual", xlab = "Rendimiento Nikkei vs Bono Japón 10Y", ylab = "Error estimado del modelo", col= "#3d85c6")

grafica.e2vsx.MesL <- plot(x=XMesL, y=EestMesL^2, main = "Error estimado al cuadrado vs (Rm-Rf) Mensual", xlab = "Rendimiento Nikkei vs Bono Japón 10Y", ylab = "Error estimado del modelo", col="#3d85c6")


###Bondad de ajuste MesL----

#STC
STCMesL <- ssc(YMesL)
#SRC
SRCMesL <- round(ssc(EestMesL),3)
#SEC
SECMesL <- round(ssc(YestMesL),3)
#R^2
R2MesL <- SECMesL/STCMesL

#Piechart
par(bg = "#bcbcbc")
valoresPieMesL <- round(c(SRCMesL/STCMesL,SECMesL/STCMesL),3)
pieMesL<-pie3D(valoresPieMesL, labels = c(SRCMesL,SECMesL), col = c("#EB0A1E", "#3d85c6"), main = "Piechart R2 Mensual", line = -1)
legend("bottomright", c("SRC","SEC"), fill = c("#EB0A1E","#3d85c6"))

###Resumen gráficos MesL----
Lista.graf.MesL<-list(graficaMesL,grafica.evsx.MesL, grafica.e2vsx.MesL, pieMesL)
names(Lista.graf.MesL)<-c("graficaMesL","grafica.evsx.MesL", "grafica.e2vsx.MesL", "pieMesL")
saveRDS(Lista.graf.MesL, "Lista.graf.MesL")


#Año----

RiAñoL <- AñoL$"Var7203"
RfAñoL <- AñoL$"VarJP10"
RmAñoL <- AñoL$"VarNKY"
#Convertir a X y Y (Var globales)
YAñoL <<- RiAñoL - RfAñoL
XAñoL <<- RmAñoL - RfAñoL

rownames(AñoL) <- AñoL[,1]
AñoL<-AñoL[,-1]
NvarAñoL<-ncol(AñoL)
## CorAñoL -----------------------------------------------------------------
CorAñoL<-as.data.frame(matrix(data=NA, ncol=ncol(AñoL), nrow=ncol(AñoL)), do.NULL=FALSE)
rownames(CorAñoL)<-colnames(AñoL)
colnames(CorAñoL)<-colnames(AñoL)

for(i in 1:NvarAñoL){
  for(j in 1:NvarAñoL)
  {CorAñoL[i,j]<-cor(x=(AñoL[,i]), y=(AñoL[,j]), method="spearman", use="na.or.complete") 
  }
}
CORRELACIONAñoL<-CorAñoL
##Estimación AñoL
##Boxplot AñoL----
dfAñoL<-(data.frame(x=RiAñoL, y=RmAñoL, z=RfAñoL))
boxplotRiAñoL <- ggplot(dfAñoL, aes(x = "RiAñoL", y=RiAñoL)) +
  stat_boxplot(geom = "errorbar",
               width = 0.25) +
  geom_boxplot(fill = "#FF0022",
               colour = "black",
               alpha = 0.5,
               outlier.colour = "black")+ ggtitle("Boxplot rentabilidad Toyota periodicidad diaria") + scale_y_continuous(limits = c(-0.40,0.40))

boxplotRmAñoL <- ggplot(dfAñoL, aes(x = "RmAñoL", y=RmAñoL)) +
  stat_boxplot(geom = "errorbar",
               width = 0.25) +
  geom_boxplot(fill = "#FF0022",
               colour = "black",
               alpha = 0.5,
               outlier.colour = "black")+ ggtitle("Boxplot rentabilidad Nikkei 225 periodicidad diaria") + scale_y_continuous(limits = c(-0.40,0.40))

boxplotRfAñoL <- ggplot(dfAñoL, aes(x = "RfAñoL", y=RfAñoL)) +
  stat_boxplot(geom = "errorbar",
               width = 0.25) +
  geom_boxplot(fill = "#FF0022",
               colour = "black",
               alpha = 0.5,
               outlier.colour = "black")+ ggtitle("Boxplot rentabilidad bonos japoneses 10 años periodicidad diaria") + scale_y_continuous(limits = c(-0.40,0.40))

ggarrange(boxplotRiAñoL, boxplotRmAñoL, boxplotRfAñoL, ncol = 3, nrow = 1, 
          common.legend = TRUE, legend = "bottom")

## Est.des.AñoL ------------------------------------------------------------
Est.des.AñoL<-data.frame(
  x=matrix(data=NA,nrow=9, ncol=NvarAñoL),row.names=c("Cantidad de datos", "Media", "Varianza","Desviacion estándar", "Minímo (0%)", "Cuantil 1 (25%)", "Mediana (50%)", "Cuantil 3 (75%)", "Máximo (100%)"))
colnames(Est.des.AñoL)<-colnames(AñoL)

for(i in 1:NvarAñoL){
  Est.des.AñoL[,i]<-festdes(AñoL[,i])
}


##Estimación AñoL----
###YestAñoL----
B0AñoL<-B0(XAñoL,YAñoL)
B1AñoL<-B1(XAñoL,YAñoL)
BetaAñoL<-c(B0AñoL, B1AñoL)

YestAñoL <<- B0(XAñoL,YAñoL)+B1(XAñoL,YAñoL)*XAñoL

###EestAñoL----

EestAñoL <<- YAñoL - YestAñoL

###GráficaAñoL----

graficaAñoL <- plot(x=XAñoL, y=YAñoL, main = "CAPM diario Toyota vs Nikkei", xlab = "Rendimiento Nikkei vs Bono Japón 10Y", ylab = "Rendimiento Toyota vs Bono Japón 10y",col="#FF0022",pch=19,cex=0.5)
abline(B0(XAñoL,YAñoL), B1(XAñoL,YAñoL), col="#351c75")



grafica.evsx.AñoL <- plot(x=XAñoL, y=EestAñoL, main = "Error estimado vs (Rm-Rf) Anual", xlab = "Rendimiento Nikkei vs Bono Japón 10Y", ylab = "Error estimado del modelo", col="#351c75")

grafica.e2vsx.AñoL <- plot(x=XAñoL, y=EestAñoL^2, main = "Error estimado al cuadrado vs (Rm-Rf) Anual", xlab = "Rendimiento Nikkei vs Bono Japón 10Y", ylab = "Error estimado del modelo", col="#351c75")


###Bondad de ajuste AñoL----

#STC
STCAñoL <- ssc(YAñoL)
#SRC
SRCAñoL <- round(ssc(EestAñoL),3)
#SEC
SECAñoL <- round(ssc(YestAñoL),3)
#R^2
R2AñoL <- SECAñoL/STCAñoL

#Piechart
par(bg = "#bcbcbc")
valoresPieAñoL <- round(c(SRCAñoL,SECAñoL),3)
pieAñoL<-pie3D(valoresPieAñoL, labels = c("SRC", "SEC"), explode = 0.1, col = c("#FF0022", "#351c75"), main = "Piechart R^2 Anual", line = 1)
legend("bottomright", c("SRC","SEC"), fill = c("#FF0022", "#351c75"))

###Resumen gráficos AñoL----
Lista.graf.AñoL<-list(graficaAñoL,grafica.evsx.AñoL, grafica.e2vsx.AñoL, pieAñoL)
names(Lista.graf.AñoL)<-c("graficaAñoL","grafica.evsx.AñoL", "grafica.e2vsx.AñoL", "pieAñoL")

saveRDS(Lista.graf.AñoL, "Lista.graf.AñoL")

Lista.graf<-list(Lista.graf.DiaL, Lista.graf.MesL, Lista.graf.AñoL)
##Resumen Estimaciones AñoL----
Lista.Beta<-as.data.frame(rbind(BetaDiaL,BetaMesL,BetaAñoL))
colnames(Lista.Beta)<-c("B0", "B1")

#RESUMEN----

##Resumen Correlaciones----
Lista.Cor<-list(CorDiaL, CorMesL, CorAñoL)
names(Lista.Cor)<-c("CorDiaL", "CorMesl","CorAñoL")
saveRDS(Lista.Cor, "Lista.Cor")
##Resumen Estadística Descriptiva -----------------------------------------

Lista.Est.des<-list(Est.des.DiaL, Est.des.MesL, Est.des.AñoL)
names(Lista.Est.des)<-c("Est.desDiaL", "Est.desMesl","Est.desAñoL")
saveRDS(Lista.Est.des, "Lista.Est.des")

##Resumen gráficos CAPM----
ggarrange(graficaDiaL, graficaMesL, graficaAñoL, ncol = 3, nrow = 1, 
          common.legend = TRUE, legend = "bottom")
##Resumen Betas----
Lista.Beta<-as.data.frame(rbind(BetaDiaL,BetaMesL,BetaAñoL))
colnames(Lista.Beta)<-c("B0", "B1")
##Resumen Final----
RESUMEN.CAPM<-list(Lista.Cor,Lista.Est.des,Lista.graf.DiaL, Lista.graf.MesL, Lista.graf.AñoL, Lista.Beta)
names(RESUMEN.CAPM)<-c("Lista.Cor","Lista.Est.des","Lista.graf.DiaL","Lista.graf.MesL","Lista.graf.AñoL", "Lista.Beta")

View(RESUMEN.CAPM)

#Estimado lector, con este último comando, podrás ver un gran resumen de todo este código, con sus principales resultados, desde allí puedes navegar por los indíces y visualizar tablas y gráficas que consignan todo este trabajo.

#Gracias por leer este código

