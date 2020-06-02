#Macro III - Trabajo Final
##Lectura de datos####
library(readr)
Datos <- read_delim(file.choose(), 
                    ";", escape_double = FALSE, trim_ws = TRUE)
View(Datos)

library(plm); library(tidyverse); library(strucchange); library(car)
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
K_COL[6]<- na.exclude(FBKF_COL/(InvGrowthLP_COL+Depre))[1]
for (i in 7:59) {
  K_COL[i]<- K_COL[i-1]*(1-Depre)+FBKF_COL[i]
  i+1
}

K_IRN[1]<- na.exclude(FBKF_IRN/(InvGrowthLP_IRN+Depre))[1]
for (i in 2:59) {
  K_IRN[i]<- K_IRN[i-1]*(1-Depre)+FBKF_IRN[i]
  i+1
}

K_TUR[27]<- na.exclude(FBKF_TUR/(InvGrowthLP_TUR+Depre))[1]
for (i in 28:59) {
  K_TUR[i]<- K_TUR[i-1]*(1-Depre)+FBKF_TUR[i]
  i+1
}

K_SWZ[11]<- na.exclude(FBKF_SWZ/(InvGrowthLP_SWZ+Depre))[1]
for (i in 12:59) {
  K_SWZ[i]<- K_SWZ[i-1]*(1-Depre)+FBKF_SWZ[i]
  i+1
}

#Regresión del producto####
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

Reg2_IRN<- plm(log(Y_IRN)~I(sh79)+log(L_IRN)+log(K_IRN), data = Datos, model = "fd")
summary(Reg2_IRN)
linearHypothesis(Reg2_IRN, "log(L_IRN)+log(K_IRN)=1")

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
linearHypothesis(Reg1_SWZ, "log(L_SWZ)+log(K_SWZ)=1")

#Crecimiento de la inversión, producto y capital####
GR_Y_COL<-NULL
for (i in 2:59) {
  GR_Y_COL[i]<- Y_COL[i]/Y_COL[i-1]-1
  i+1
}; GR_Y_COL<- ts(GR_Y_COL, start = 1960)
GR_FBKF_COL<-NULL
for (i in 7:59) {
  GR_FBKF_COL[i]<- FBKF_COL[i]/FBKF_COL[i-1]-1
  i+1
}; GR_FBKF_COL<- ts(GR_FBKF_COL, start = 1965)

plot.ts(GR_Y_COL, type='l', col=c("#D60404C6"), ylab = NULL, axes = F)
par(new=T)
plot.ts(GR_FBKF_COL, type='l', col=c("#F07503C0"), ylab = NULL, main = 'Crecimiento del PIB e inversión - COL')

