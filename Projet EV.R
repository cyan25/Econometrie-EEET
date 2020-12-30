rm(list=ls())  # To clear all
graphics.off() # To close all graphics
# 1. : lecture des données
library(readr)
Immat_elec_v2020 <- read_delim("Immat_elec_v2020.csv", ";", escape_double = FALSE, trim_ws = TRUE)

#Exclure les premiers lignes
ImmatEV <- subset(Immat_elec_v2020,X1>= 2004)
names(ImmatEV)[names(ImmatEV)=='VE+VHR'] <- "VE_VHR"
names(ImmatEV)[names(ImmatEV)=='PIB/hab'] <- "PIB.Hab"
attach(ImmatEV)

n=length(VE_VHR)
# 2. : Choix des variables
vec <- c(Population,PIB.Hab/IPC,PGazole/Pelec)
X <- matrix(as.numeric(vec),ncol=3)
Y <- matrix(VE_VHR,n,1)

x=X;
y=Y;
# 3. : Estimation MCO
OLS = lm(formula=y~x)
summary(OLS)
res<-OLS$residuals

#Durbin-Watson
d1 = t(res) %*% res
d2 =  t(res[2:n]-res[1:n-1]) %*% (res[2:n]-res[1:n-1])
dw = d2/d1
print (dw)

# Regression auxiliaire de White
#
