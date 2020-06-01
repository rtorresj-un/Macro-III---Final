#Macro III - Trabajo Final
##Lectura de datos####
library(readr)
Datos <- read_delim(file.choose(), 
                    ";", escape_double = FALSE, trim_ws = TRUE)
View(Datos)

library(plm); library(tidyverse); library(strucchange)
Datos <- pdata.frame(Datos)
Datos <- Datos %>%
  group_by(Country)
table(Datos$Country)
table(Datos$Year)
attach(Datos)

##Calculando variables####
Depre<- 0.05

#Crecimiento de la inversi?n####
FBKF_COL<- subset(Datos, Country == "COL")$FBKF
laggedFBKF_COL<- lag(FBKF_COL)  
InvGrowth_COL<- (na.exclude(FBKF_COL/laggedFBKF_COL))-1
InvGrowthLP_COL<- mean(InvGrowth_COL)

FBKF_IRN<- subset(Datos, Country == "IRN")$FBKF
laggedFBKF_IRN<- lag(FBKF_IRN)  
InvGrowth_IRN<- (na.exclude(FBKF_IRN/laggedFBKF_IRN))-1
InvGrowthLP_IRN<- mean(InvGrowth_IRN)

FBKF_TUR<- subset(Datos, Country == "TUR")$FBKF
laggedFBKF_TUR<- lag(FBKF_TUR)
InvGrowth_TUR<- (na.exclude(FBKF_TUR/laggedFBKF_TUR))-1
InvGrowthLP_TUR<- mean(InvGrowth_TUR)

FBKF_SWZ<- subset(Datos, Country == "SWZ")$FBKF
laggedFBKF_SWZ<- lag(FBKF_SWZ)  
InvGrowth_SWZ<- (na.exclude(FBKF_SWZ/laggedFBKF_SWZ))-1
InvGrowthLP_SWZ<- mean(InvGrowth_SWZ)

cbind(InvGrowthLP_COL, InvGrowthLP_IRN, InvGrowthLP_TUR, InvGrowthLP_SWZ)

#Capital####
Ko_COL<- na.exclude(FBKF_COL/(InvGrowthLP_COL+Depre))[1]
K_COL<- Ko_COL*(1-Depre)+FBKF_COL

Ko_IRN<- na.exclude(FBKF_IRN/(InvGrowthLP_IRN+Depre))[1]
K_IRN<- Ko_IRN*(1-Depre)+FBKF_IRN

Ko_TUR<- na.exclude(FBKF_TUR/(InvGrowthLP_TUR+Depre))[1]
K_TUR<- Ko_TUR*(1-Depre)+FBKF_TUR

Ko_SWZ<- na.exclude(FBKF_SWZ/(InvGrowthLP_SWZ+Depre))[1]
K_SWZ<- Ko_SWZ*(1-Depre)+FBKF_SWZ

#Regresi?n del producto####
Y_COL<- subset(Datos, Country=="COL")$GDP
L_COL<- subset(Datos, Country=="COL")$Pop
Reg1_COL<- plm(log(Y_COL)~log(L_COL)+log(K_COL), data = Datos, model = "fd")
summary(Reg1_COL)

cusum.prueba1 = efp(log(Y_COL)~log(L_COL)+log(K_COL),type = "OLS-CUSUM")
plot(cusum.prueba1)
sctest(log(Y_COL)~log(L_COL)+log(K_COL),type = "Chow", point = 40)
plot(log(K_COL), type = "l")
plot(log(Y_COL), type = "l")
sh99<- as.numeric(subset(Datos, Country=="COL")$Year)>=40
sh99<- as.numeric(sh99)

Reg2_COL<- plm(log(Y_COL)~I(sh99)+log(L_COL)+log(K_COL), data = Datos, model = "fd")
summary(Reg2_COL)
linearHypothesis(Reg2_COL, "log(L_COL)+log(K_COL)=1")

Y_IRN<- subset(Datos, Country=="IRN")$GDP
L_IRN<- subset(Datos, Country=="IRN")$Pop
Reg1_IRN<- plm(log(Y_IRN)~log(L_IRN)+log(K_IRN), data = Datos, model = "fd")
summary(Reg1_IRN)

cusum.prueba2 = efp(log(Y_IRN)~log(L_IRN)+log(K_IRN),type = "OLS-CUSUM")
plot(cusum.prueba2)
plot(log(K_IRN), type = "l")
plot(log(Y_IRN), type = "l")
sh79<- as.numeric(subset(Datos, Country=="IRN")$Year)>=19
sh79<- as.numeric(sh79)
linearHypothesis(Reg2_IRN, "log(L_IRN)+log(K_IRN)=1")

Reg2_IRN<- plm(log(Y_IRN)~I(sh79)+log(L_IRN)+log(K_IRN), data = Datos, model = "fd")
summary(Reg2_IRN)

Y_TUR<- subset(Datos, Country=="TUR")$GDP
L_TUR<- subset(Datos, Country=="TUR")$Pop
Reg1_TUR<- plm(log(Y_TUR)~log(L_TUR)+log(K_TUR), data = Datos, model = "fd")
summary(Reg1_TUR)

cusum.prueba3 = efp(log(Y_TUR)~log(L_TUR)+log(K_TUR),type = "OLS-CUSUM")
plot(cusum.prueba3)
plot(log(K_TUR), type = "l")
plot(log(Y_TUR), type = "l")

Y_SWZ<- subset(Datos, Country=="SWZ")$GDP
L_SWZ<- subset(Datos, Country=="SWZ")$Pop
Reg1_SWZ<- plm(log(Y_SWZ)~log(L_SWZ)+log(K_SWZ), data = Datos, model = "fd")
summary(Reg1_SWZ)

cusum.prueba4 = efp(log(Y_SWZ)~log(L_SWZ)+log(K_SWZ),type = "OLS-CUSUM")
plot(cusum.prueba4)
plot(log(K_SWZ), type = "l")
plot(log(Y_SWZ), type = "l")

