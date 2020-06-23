#Macro III - Trabajo Final
##Lectura de datos####
library(readr)
Datos <- read_delim(file.choose(), 
                    ";", escape_double = FALSE, trim_ws = TRUE)

library(plm); library(tidyverse); library(strucchange); library(mFilter)
library(car); library(tseries); library(lmtest); library(vars); library(ggfortify)
Datos <- pdata.frame(Datos, index = c('Year', 'Country'))
Datos <- Datos %>%
  group_by(Country)
View(Datos)
table(Datos$Country)
table(Datos$Year)
attach(Datos)

##Calculando variables####
Depre<- 0.05

#Crecimiento de la inversión####
FBKF_COL<- subset(Datos, Country == "COL")$FBKF
laggedFBKF_COL<- lag(FBKF_COL)  
InvGrowth_COL<- (na.exclude(FBKF_COL/laggedFBKF_COL))-1
InvGrowthLP_COL<- mean(InvGrowth_COL)

FBKF_IRN<- subset(Datos, Country == "IRN")$FBKF
laggedFBKF_IRN<- lag(FBKF_IRN)  
InvGrowth_IRN<- (na.exclude(FBKF_IRN/laggedFBKF_IRN))-1
InvGrowthLP_IRN<- mean(InvGrowth_IRN)

FBKF_THA<- subset(Datos, Country == "THA")$FBKF
laggedFBKF_THA<- lag(FBKF_THA)
InvGrowth_THA<- (na.exclude(FBKF_THA/laggedFBKF_THA))-1
InvGrowthLP_THA<- mean(InvGrowth_THA)

FBKF_SWZ<- subset(Datos, Country == "SWZ")$FBKF
laggedFBKF_SWZ<- lag(FBKF_SWZ)  
InvGrowth_SWZ<- (na.exclude(FBKF_SWZ/laggedFBKF_SWZ))-1
InvGrowthLP_SWZ<- mean(InvGrowth_SWZ)

cbind(InvGrowthLP_COL, InvGrowthLP_IRN, InvGrowthLP_THA, InvGrowthLP_SWZ)

#Capital####
K_COL<- NULL
K_COL[1]<- na.exclude(FBKF_COL/(InvGrowthLP_COL+Depre))[1]
for (i in 2:54) {
  K_COL[i]<- K_COL[i-1]*(1-Depre)+FBKF_COL[i]
  i+1
}

K_IRN<- NULL
K_IRN[1]<- na.exclude(FBKF_IRN/(InvGrowthLP_IRN+Depre))[1]
for (i in 2:54) {
  K_IRN[i]<- K_IRN[i-1]*(1-Depre)+FBKF_IRN[i]
  i+1
}

K_THA<- NULL
K_THA[1]<- na.exclude(FBKF_THA/(InvGrowthLP_THA+Depre))[1]
for (i in 2:54) {
  K_THA[i]<- K_THA[i-1]*(1-Depre)+FBKF_THA[i]
  i+1
}

K_SWZ <- NULL
K_SWZ[1]<- na.exclude(FBKF_SWZ/(InvGrowthLP_SWZ+Depre))[1]
for (i in 2:54) {
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

Reg3_COL<- plm(diff(log(Y_COL))~I(sh99)[2:59]+diff(log(L_COL))+diff(log(K_COL)), data = Datos, model = "fd")
summary(Reg3_COL)

adf.test(log(Y_COL)); adf.test(na.exclude(log(K_COL))); adf.test(log(L_COL))
plot(diff(log(K_COL)), type = "l")
plot(diff(log(Y_COL)), type = "l")

grangertest(log(Y_COL)~log(K_COL), order=1)
grangertest(log(K_COL)~log(Y_COL), order=1)
COL_varm<-data.frame(log(Y_COL)[6:59], log(K_COL)[6:59], log(L_COL)[6:59])
VARselect(COL_varm, lag.max=15)
VAR_COL<-VAR(COL_varm,p=1)
summary(VAR_COL)
plot(VAR_COL)

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

Y_THA<- subset(Datos, Country=="THA")$GDP
L_THA<- subset(Datos, Country=="THA")$Pop
Reg1_THA<- plm(log(Y_THA)~log(L_THA)+log(K_THA), data = Datos, model = "fd")
summary(Reg1_THA)

cusum.prueba3 = efp(log(Y_THA)~log(L_THA)+log(K_THA),type = "OLS-CUSUM")
plot(cusum.prueba3)
plot(log(K_THA), type = "l")
plot(log(Y_THA), type = "l")

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
for (i in 2:54) {
  GR_Y_COL[i]<- Y_COL[i]/Y_COL[i-1]-1
  i+1
}; GR_Y_COL<- ts(GR_Y_COL, start = 1965)
GR_FBKF_COL<-NULL
for (i in 2:54) {
  GR_FBKF_COL[i]<- FBKF_COL[i]/FBKF_COL[i-1]-1
  i+1
}; GR_FBKF_COL<- ts(GR_FBKF_COL, start = 1965)

GRO_plot_COL<-autoplot(cbind(GR_Y_COL,GR_FBKF_COL), facets = F)
GRO_plot_COL<- GRO_plot_COL + 
  ggtitle("Crecimiento del PIB e inversión - COL") +
  guides(fill=FALSE) +
  labs(colour = "Crecimiento") +
  scale_color_manual(labels = c("I", "PIB"), values = c("#D60404C6", "#F07503C0")) +
  theme(legend.position="bottom") + geom_abline(aes(slope = 0, intercept = 0), colour=c("#03030377"))
print(GRO_plot_COL)

GR_Y_IRN<-NULL
for (i in 2:54) {
  GR_Y_IRN[i]<- Y_IRN[i]/Y_IRN[i-1]-1
  i+1
}; GR_Y_IRN<- ts(GR_Y_IRN, start = 1965)
GR_FBKF_IRN<-NULL
for (i in 2:54) {
  GR_FBKF_IRN[i]<- FBKF_IRN[i]/FBKF_IRN[i-1]-1
  i+1
}; GR_FBKF_IRN<- ts(GR_FBKF_IRN, start = 1965)

GRO_plot_IRN<-autoplot(cbind(GR_Y_IRN,GR_FBKF_IRN), facets = F)
GRO_plot_IRN<- GRO_plot_IRN + 
  ggtitle("Crecimiento del PIB e inversión - IRN") +
  guides(fill=FALSE) +
  labs(colour = "Crecimiento") +
  scale_color_manual(labels = c("I", "PIB"), values = c("#D60404C6", "#F07503C0")) +
  theme(legend.position="bottom") + geom_abline(aes(slope = 0, intercept = 0), colour=c("#03030377"))
print(GRO_plot_IRN)

GR_Y_THA<-NULL
for (i in 2:54) {
  GR_Y_THA[i]<- Y_THA[i]/Y_THA[i-1]-1
  i+1
}; GR_Y_THA<- ts(GR_Y_THA, start = 1965)
GR_FBKF_THA<-NULL
for (i in 2:54) {
  GR_FBKF_THA[i]<- FBKF_THA[i]/FBKF_THA[i-1]-1
  i+1
}; GR_FBKF_THA<- ts(GR_FBKF_THA, start = 1965)

GRO_plot_THA<-autoplot(cbind(GR_Y_THA,GR_FBKF_THA), facets = F)
GRO_plot_THA<- GRO_plot_THA + 
  ggtitle("Crecimiento del PIB e inversión - THA") +
  guides(fill=FALSE) +
  labs(colour = "Crecimiento") +
  scale_color_manual(labels = c("I", "PIB"), values = c("#D60404C6", "#F07503C0")) +
  theme(legend.position="bottom") + geom_abline(aes(slope = 0, intercept = 0), colour=c("#03030377"))
print(GRO_plot_THA)

GR_Y_SWZ<-NULL
for (i in 2:54) {
  GR_Y_SWZ[i]<- Y_SWZ[i]/Y_SWZ[i-1]-1
  i+1
}; GR_Y_SWZ<- ts(GR_Y_SWZ, start = 1965)
GR_FBKF_SWZ<-NULL
for (i in 2:54) {
  GR_FBKF_SWZ[i]<- FBKF_SWZ[i]/FBKF_SWZ[i-1]-1
  i+1
}; GR_FBKF_SWZ<- ts(GR_FBKF_SWZ, start = 1965)

#plot.ts(GR_Y_SWZ, type='l', col=c("#D60404C6"), ylab = NULL, axes = F)
#par(new=T)
#plot.ts(GR_FBKF_SWZ, type='l', col=c("#F07503C0"), ylab = NULL, main = 'Crecimiento del PIB e inversión - SWZ')
#legend(x = "bottomright", legend = c("PIB", "I"), fill = c("#D60404C6", "#F07503C0"), title = "Crecimiento")
#abline(a = 0,b = 0)

GRO_plot_SWZ<-autoplot(cbind(GR_Y_SWZ,GR_FBKF_SWZ), facets = F)
GRO_plot_SWZ<- GRO_plot_SWZ + 
  ggtitle("Crecimiento del PIB e inversión - SWZ") +
  guides(fill=FALSE) +
  labs(colour = "Crecimiento") +
  scale_color_manual(labels = c("I", "PIB"), values = c("#D60404C6", "#F07503C0")) +
  theme(legend.position="bottom") + geom_abline(aes(slope = 0, intercept = 0), colour=c("#03030377"))
print(GRO_plot_SWZ)

#Cálculo de A####
alpha_COL=0.35
A_COL=(Y_COL/(K_COL^(alpha_COL)*L_COL^(1-alpha_COL)))^(1/(1-alpha_COL))
A_COL<-ts(na.exclude(A_COL), start = 1965)
autoplot(A_COL, main='A - COL')
autoplot(diff(log(A_COL)), main='Crecimiento de A - COL')

A.f_COL <- hpfilter(A_COL, freq = 4)
A.ft_COL<-A.f_COL[["trend"]]
A.fc_COL<-A.f_COL[["cycle"]]
autoplot(A.ft_COL, main='A para COL', ts.colour = c("#B22222"))
autoplot(A.fc_COL, main='Componente cíclico de A para COL', ts.colour = c("#B22222"))

write.table(cbind(A.ft_COL,A.fc_COL) , file = "A.filtrado_COL.csv",sep = ";", row.names = F)

alpha_IRN=0.234
A_IRN=(Y_IRN/(K_IRN^alpha_IRN*L_IRN^(1-alpha_IRN)))^(1/(1-alpha_IRN))
A_IRN<-ts(na.exclude(A_IRN), start = 1965)
autoplot(A_IRN, main='A - IRN')
autoplot(diff(log(A_IRN)), main='Crecimiento de A - IRN')

A.f_IRN <- hpfilter(A_IRN,freq = 4)
A.ft_IRN<-A.f_IRN[["trend"]]
A.fc_IRN<-A.f_IRN[["cycle"]]
autoplot(A.ft_IRN, main='A para IRN', ts.colour = c("#B22222"))
autoplot(A.fc_IRN, main='Componente cíclico de A para IRN', ts.colour = c("#B22222"))

write.table(cbind(A.ft_IRN,A.fc_IRN) , file = "A.filtrado_IRN.csv",sep = ";", row.names = F)

alpha_THA=0.2829
A_THA=(Y_THA/(K_THA^alpha_THA*L_THA^(1-alpha_THA)))^(1/(1-alpha_THA))
A_THA<-ts(na.exclude(A_THA), start = 1965)
autoplot(A_THA, main='A - THA')
autoplot(diff(log(A_THA)), main='Crecimiento de A - THA')

A.f_THA <- hpfilter(A_THA,freq = 4)
A.ft_THA<-A.f_THA[["trend"]]
A.fc_THA<-A.f_THA[["cycle"]]
autoplot(A.ft_THA, main='A para THA', ts.colour = c("#B22222"))
autoplot(A.fc_THA, main='Componente cíclico de A para THA', ts.colour = c("#B22222"))

write.table(cbind(A.ft_THA,A.fc_THA) , file = "A.filtrado_THA.csv",sep = ";", row.names = F)

alpha_SWZ=0.24
A_SWZ=(Y_SWZ/(K_SWZ^alpha_SWZ*L_SWZ^(1-alpha_SWZ)))^(1/(1-alpha_SWZ))
A_SWZ<-ts(na.exclude(A_SWZ), start = 1965)
autoplot(A_SWZ, main='A - SWZ')
autoplot(diff(log(A_SWZ)), main='Crecimiento de A - SWZ')

A.f_SWZ <- hpfilter(A_SWZ,freq = 4)
A.ft_SWZ<-A.f_SWZ[["trend"]]
A.fc_SWZ<-A.f_SWZ[["cycle"]]
autoplot(A.ft_SWZ, main='A para SWZ', ts.colour = c("#B22222"))
autoplot(A.fc_SWZ, main='Componente cíclico de A para SWZ', ts.colour = c("#B22222"))

write.table(cbind(A.ft_SWZ,A.fc_SWZ) , file = "A.filtrado_SWZ.csv",sep = ";", row.names = F)

#Estado estacionario####
s_COL=mean(na.exclude(FBKF_COL/Y_COL))
n_COL=mean(na.exclude(L_COL/lag(L_COL)-1))
g_COL=mean(na.exclude(as.numeric(A.ft_COL)/lag(as.numeric(A.ft_COL))-1))
K_EE_COL=A.ft_COL*(s_COL/(Depre+g_COL+n_COL))^(1/(1-alpha_COL))
Y_EE_COL=A.ft_COL*(s_COL/(Depre+g_COL+n_COL))^(alpha_COL/(1-alpha_COL))

Kpercap_COL<-K_COL/L_COL
K_plot_COL<-autoplot(cbind(Kpercap_COL,K_EE_COL), facets = F)
K_plot_COL<- K_plot_COL + 
  ggtitle("Comparación de K - COL") +
  guides(fill=FALSE) +
  labs(colour = "Tipos") +
  scale_color_manual(labels = c("k^*","k"), values = c("#8B2500", "#EE0000")) +
  theme(legend.position="bottom")
print(K_plot_COL)

s_IRN=mean(na.exclude(FBKF_IRN/Y_IRN))
n_IRN=mean(na.exclude(L_IRN/lag(L_IRN)-1))
g_IRN=mean(na.exclude(as.numeric(A.ft_IRN)/lag(as.numeric(A.ft_IRN))-1))
K_EE_IRN=A.ft_IRN*(s_IRN/(Depre+g_IRN+n_IRN))^(1/(1-alpha_IRN))
Y_EE_IRN=A.ft_IRN*(s_IRN/(Depre+g_IRN+n_IRN))^(alpha_IRN/(1-alpha_IRN))

Kpercap_IRN<-K_IRN/L_IRN
K_plot_IRN<-autoplot(cbind(Kpercap_IRN,K_EE_IRN), facets = F)
K_plot_IRN<- K_plot_IRN + 
  ggtitle("Comparación de K - IRN") +
  guides(fill=FALSE) +
  labs(colour = "Tipos") +
  scale_color_manual(labels = c("k^*","k"), values = c("#8B2500", "#EE0000")) +
  theme(legend.position="bottom")
print(K_plot_IRN)

s_THA=mean(na.exclude(FBKF_THA/Y_THA))
n_THA=mean(na.exclude(L_THA/lag(L_THA)-1))
g_THA=mean(na.exclude(as.numeric(A.ft_THA)/lag(as.numeric(A.ft_THA))-1))
K_EE_THA=A.ft_THA*(s_THA/(Depre+g_THA+n_THA))^(1/(1-alpha_THA))
Y_EE_THA=A.ft_THA*(s_THA/(Depre+g_THA+n_THA))^(alpha_THA/(1-alpha_THA))

Kpercap_THA<-K_THA/L_THA
K_plot_THA<-autoplot(cbind(Kpercap_THA,K_EE_THA), facets = F)
K_plot_THA<- K_plot_THA + 
  ggtitle("Comparación de K - THA") +
  guides(fill=FALSE) +
  labs(colour = "Tipos") +
  scale_color_manual(labels = c("k^*","k"), values = c("#8B2500", "#EE0000")) +
  theme(legend.position="bottom")
print(K_plot_THA)

s_SWZ=mean(na.exclude(FBKF_SWZ/Y_SWZ))
n_SWZ=mean(na.exclude(L_SWZ/lag(L_SWZ)-1))
g_SWZ=mean(na.exclude(as.numeric(A.ft_SWZ)/lag(as.numeric(A.ft_SWZ))-1))
K_EE_SWZ=A.ft_SWZ*(s_SWZ/(Depre+g_SWZ+n_SWZ))^(1/(1-alpha_SWZ))
Y_EE_SWZ=A.ft_SWZ*((s_SWZ/(Depre+g_SWZ+n_SWZ))^(alpha_SWZ/(1-alpha_SWZ)))

Kpercap_SWZ<-K_SWZ/L_SWZ
K_plot_SWZ<-autoplot(cbind(Kpercap_SWZ,K_EE_SWZ), facets = F)
K_plot_SWZ<- K_plot_SWZ + 
  ggtitle("Comparación de K - SWZ") +
  guides(fill=FALSE) +
  labs(colour = "Tipos") +
  scale_color_manual(labels = c("k^*","k"), values = c("#8B2500", "#EE0000")) +
  theme(legend.position="bottom")
print(K_plot_SWZ)
  
