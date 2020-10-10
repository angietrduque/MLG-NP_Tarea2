#----------------------------------------------------------------------------------------#

# MLG-NP
# Tarea 2
# Cesar A. Saavedra Vanegas
# Angie Rodriguez Duque 

#----------------------------------------------------------------------------------------#

# Librerias 
suppressMessages(library(dplyr))
suppressMessages(library(readxl))
suppressMessages(library(tidyverse))
suppressMessages(library(FactoMineR))
suppressMessages(library(factoextra))
suppressMessages(library(foreign))
suppressMessages(library(corrplot))
suppressMessages(library(polycor))
suppressMessages(library(psych))
suppressMessages(library(gplots))
suppressMessages(library(gridExtra))
suppressMessages(library(viridis))
suppressMessages(library(lsr))
suppressMessages(library(DescTools))
suppressMessages(library(magrittr))
suppressMessages(library(nlme))
suppressMessages(library(MASS))
suppressMessages(library(multilevel))
suppressMessages(library(plspm))
suppressMessages(library(reshape))
suppressMessages(library(homals))
suppressMessages(library(GGally))
suppressMessages(library(CCA))
suppressMessages(library(plotly))
suppressMessages(library(broom))
suppressMessages(library(readr))

#----------------------------------------------------------------------------------------#
# Actividad 1
#----------------------------------------------------------------------------------------#
# Cargar los datos




#----------------------------------------------------------------------------------------#
# Actividad 2
#----------------------------------------------------------------------------------------#
# Cargar los datos
Datos <- read.csv("/Users/cesar.saavedra/Documents/GitHub/MLG-NP_Tarea2/winequality-red.csv", sep = ";")
Datos <- read.csv("C:/Users/Angie Rodr?guez/Desktop/Semestre 9/2. Modelos Lineales/MLG&NP_Tarea2/winequality-red.csv", sep = ";")

Datos

# Categrorizar variable pH a pHi
summary(Datos[,"pH"])
pHi <- cut(Datos$pH, breaks = c(2.6,3.1,3.6,4.1), labels = c("Bajo", "Medio", "Alto"))
head(pHi)
table(pHi)

