rm(list=ls())
set.seed(666)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
library(readxl)
library(ggpubr)
library(dplyr)
library(DataExplorer)
library(VIM)
library(DMwR)
library(mice)
library(caret)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(discretization)
library(gsubfn)
library(proto)
library(RSQLite)
library(sqldf)
library(factoextra)
library(FactoMineR)
library(reshape)
library(data.table)
library(MLmetrics)
library(party)
library(lattice)
library(sqldf)
library(mlr)
library(nortest)
library(tseries)
library(factoextra)
library(kohonen)

# LEYENDO DATASET ####
data <- read_excel("data.xls")


# PRE PROCESAMIENTO DE DATOS ####
# VALORES PERDIDOS ####
# 1. DETECCION DE VALORES PERDIDOS####
  windows()
  plot_missing(data) 
  windows()
  plot_missing(data2) 
  windows()
  plot_missing(data3) 

# LIMPIEZA PREVIA
data2 <- data %>% select(-Joined,-RS,-LW,-LF,-CF,-RF,-RW,-LAM,-CAM,-RAM,-LM,-LCM,-CM,-RCM,
                         -RM,-LWB,-LDM,-CDM,-RDM,-RWB,-LB,-LCB,-CB,-RCB,-RB,-ST,-LS,-ReleaseClauseM,-Position, -Club, -WorkRate)
str(data3)
# IMPUTACIÓN INICIAL

#RealFace 1: NO, 2: YES
table(data2$RealFace)
data2$RealFace <- as.factor(data2$RealFace) 
data2$RealFace <- as.numeric(data2$RealFace)
data2$RealFace <- as.factor(data2$RealFace) 
table(data2$RealFace)

#BodyType
# Variable sucia, se decide modificar los 7 registros equivocados y asignarles la moda
table(data$BodyType)
data2$BodyType <- 
  ifelse(data2$BodyType == 'Akinfenwa',"Normal",
         ifelse(data2$BodyType=="C. Ronaldo","Normal",
                ifelse(data2$BodyType=="Courtois","Normal",
                       ifelse(data2$BodyType=="Messi","Normal",
                              ifelse(data2$BodyType=="Neymar","Normal",
                                     ifelse(data2$BodyType=="PLAYER_BODY_TYPE_25","Normal",
                                            ifelse(data2$BodyType=="Shaqiri","Normal",data2$BodyType)))))))
str(data2$BodyType)
data2$BodyType <- as.factor(data2$BodyType) 
table(data2$BodyType)
#data2$BodyType <- data$BodyType


#WorkRate Evaluar si es que se considera o no
#Consultar con Luis
#Eliminada
#table(data$WorkRate)

#PreferredFoot  Left:1, Right:2
table(data2$PreferredFoot)
str(data2$PreferredFoot)
data2$PreferredFoot <- as.factor(data2$PreferredFoot) 
#data2$PreferredFoot <- as.numeric(data2$PreferredFoot)
#data2$PreferredFoot <- as.factor(data2$PreferredFoot) 
table(data2$PreferredFoot)


#Position 27 categorías
#Consultar diccionario
# Se decidió retirar
table(data$Position)


#WorkRate
# Se decidió retirar temporalmente
table(data3$WorkRate)


data3 <- impute(data2, classes = list(factor = imputeMode(), 
                                           integer = imputeMode(),
                                           numeric = imputeMedian()))
data3 <- data3$data

str(data3)

#Nationality
table(data3$Nationality)
data3$Nationality <- as.factor(data3$Nationality) 

#ValueM
# Se imputó por 0
#VALIDAR na
data3$ValueM <- as.numeric(data3$ValueM) 
table(data3$ValueM)
data3$ValueM[is.na(data3$ValueM)] <- 0
table(data3$ValueM)
table(data$ValueM)

#InternationalReputation
data3$InternationalReputation <- as.factor(data3$InternationalReputation)

#WeakFoot
table(data3$WeakFoot)
data3$WeakFoot <- as.factor(data3$WeakFoot)

#SkillMoves
table(data3$SkillMoves)
data3$SkillMoves <- as.factor(data3$SkillMoves)

# VALORES ATIPICOS ####
# 1. DETECCION OUTLIERS UNIVARIADOS ####

windows()
par(mfrow=c(3,4))
boxplot(data3$Age,col="peru", main="Age")
boxplot(data3$Overall,col="peru", main="Overall")
boxplot(data3$Potential,col="peru", main="Potential")
boxplot(data3$ValueM,col="peru", main="ValueM")
boxplot(data3$WageK,col="peru", main="WageK")
boxplot(data3$Special,col="peru", main="Special")
boxplot(data3$JerseyNumber,col="peru", main="JerseyNumber")#No queda claro que es
boxplot(data3$Years_Contract,col="peru", main="Years_Contract")
boxplot(data3$Height_cm,col="peru", main="Height_cm")
boxplot(data3$Years_Contract,col="peru", main="Years_Contract")
boxplot(data3$Weight_kg,col="peru", main="Weight_kg")

#ATTACKING
windows()
par(mfrow=c(2,3))
boxplot(data3$Crossing,col="peru", main="Crossing")
boxplot(data3$Finishing,col="peru", main="Finishing")
boxplot(data3$HeadingAccuracy,col="peru", main="HeadingAccuracy")
boxplot(data3$ShortPassing,col="peru", main="ShortPassing")
boxplot(data3$Volleys,col="peru", main="Volleys")
title("ATTACKING", line = -1, outer = TRUE)


#SKILL
windows()
par(mfrow=c(2,3))
boxplot(data3$Dribbling,col="peru", main="Dribbling")
boxplot(data3$Curve,col="peru", main="Curve")
boxplot(data3$FKAccuracy,col="peru", main="FKAccuracy")
boxplot(data3$LongPassing,col="peru", main="LongPassing")
boxplot(data3$BallControl,col="peru", main="BallControl")
title("SKILL", line = -1, outer = TRUE)

#MOVEMENT
windows()
par(mfrow=c(2,3))
boxplot(data3$Acceleration,col="peru", main="Acceleration")
boxplot(data3$SprintSpeed,col="peru", main="SprintSpeed")
boxplot(data3$Agility,col="peru", main="Agility")
boxplot(data3$Reactions,col="peru", main="Reactions")
boxplot(data3$Balance,col="peru", main="Balance")
title("MOVEMENT", line = -1, outer = TRUE)



#POWER
windows()
par(mfrow=c(2,3))
boxplot(data3$ShotPower,col="peru", main="ShotPower")
boxplot(data3$Jumping,col="peru", main="Jumping")
boxplot(data3$Stamina,col="peru", main="Stamina")
boxplot(data3$Strength,col="peru", main="Strength")
boxplot(data3$LongShots,col="peru", main="LongShots")
title("POWER", line = -1, outer = TRUE)


#MENTALITY
windows()
par(mfrow=c(2,3))
boxplot(data3$Aggression,col="peru", main="Aggression")
boxplot(data3$Interceptions,col="peru", main="Interceptions")
boxplot(data3$Positioning,col="peru", main="Positioning")
boxplot(data3$Vision,col="peru", main="Vision")
boxplot(data3$Penalties,col="peru", main="Penalties")
boxplot(data3$Composure,col="peru", main="Composure")
title("MENTALITY", line = -1, outer = TRUE)


#DEFENDING
windows()
par(mfrow=c(1,3))
boxplot(data3$Marking,col="peru", main="Marking")
boxplot(data3$StandingTackle,col="peru", main="StandingTackle")
boxplot(data3$SlidingTackle,col="peru", main="SlidingTackle")
title("DEFENDING", line = -1, outer = TRUE)


#GOALKEEPING
windows()
par(mfrow=c(2,3))
boxplot(data3$GKDiving,col="peru", main="GKDiving")
boxplot(data3$GKHandling,col="peru", main="GKHandling")
boxplot(data3$GKKicking,col="peru", main="GKKicking")
boxplot(data3$GKPositioning,col="peru", main="GKPositioning")
boxplot(data3$GKReflexes ,col="peru", main="GKReflexes")
title("GOALKEEPING", line = -1, outer = TRUE)




table(data3$JerseyNumber)

outliers1 <- boxplot(data$PIM)$out
outliers1 ; length(outliers1)
boxplot.stats(data$PIM)
data_set <- data


# PCA ####

# Selección de variables para el PCA
pca_data3 <- data3 %>% select(Crossing,Finishing,HeadingAccuracy,ShortPassing,Volleys,Dribbling,Curve,
                              FKAccuracy,LongPassing,BallControl,Acceleration,SprintSpeed,Agility,Reactions,
                              Balance,ShotPower,Jumping,Stamina,Strength,LongShots,Aggression,Interceptions,
                              Positioning,Vision,Penalties,Composure,Marking,StandingTackle,SlidingTackle,GKDiving,
                              GKHandling,GKKicking,GKPositioning,GKReflexes)



# Aplicando el PCA
#pca.data3 <- princomp(pca_data3)

# Resumen
summary(pca.data3)
  #Se podrian tomar 3 componentes (81.2%)

# GrÃ¡fico de la varianza explicada
windows()
plot(pca.data3)

# Utilizando solo 3 componentes
pca.data3 <- prcomp(pca_data3,scale=T, rank. = 3) 
head(pca.data3$x, 10) # Puntuaciones factoriales 
pca.data3$x

#Aplicando Kohonen

## -- Primer Modelo Kohonen  -- ##

kohonem_data3 <- som(pca.data3$x, grid = somgrid(5,5,"hexagonal"))

summary(kohonem_data3)

kohonem_data3$unit.classif 

plot(kohonem_data3, main="Datos de vino")
  
