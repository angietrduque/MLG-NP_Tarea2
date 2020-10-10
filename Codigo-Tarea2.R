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
suppressMessages(library(readxl))

#----------------------------------------------------------------------------------------#
# Fijar directorio
setwd("C:/Users/Angie Rodr?guez/Desktop/Semestre 9/2. Modelos Lineales/MLG&NP_Tarea2")
setwd("/Users/cesar.saavedra/Documents/GitHub/MLG-NP_Tarea2")
#----------------------------------------------------------------------------------------#
# Actividad 1
#----------------------------------------------------------------------------------------#
# Cargar los datos




#----------------------------------------------------------------------------------------#
# Actividad 2
#----------------------------------------------------------------------------------------#
# Cargar los datos
Datos <- read.table("Datos.txt",header=T,sep = ",")
Datos

View(Datos)
str(Datos)

# Categrorizar variable pH a pHi
summary(Datos[,"pH"])
pHi <- cut(Datos$pH, breaks = c(2.6,3.1,3.6,4.1), labels = c("Bajo", "Medio", "Alto"))
head(pHi)
table(pHi)

# Estadisticas descriptivas
G1 <- ggplot(data = Datos, aes(x=pHi, fill=pHi)) +
  geom_bar(position="dodge") + 
  ylab("") + xlab(" ") +
  scale_fill_discrete(name = "Nivel pH:", labels = c("Bajo", "Medio", "Alto"))
G1

G2 <- ggplot(Datos, aes(y=Datos$density, x=pHi, fill=pHi)) + geom_boxplot(show.legend = T) + 
  scale_fill_discrete(name = "Ph:") + xlab("") + ylab("Densidad")
G2

G3 <- ggplot(Datos, aes(y=Datos$alcohol, x=Datos$quality, group=Datos$quality)) + geom_boxplot(show.legend = T)
G3

G4 <- ggplot(Datos, aes(y=Datos$fixed.acidity, x=Datos$quality, group=Datos$quality)) + geom_boxplot(show.legend = T)
G4


# Correlacion
corrplot(cor(Datos), method = "number")

# Modelo 
Modelo <- glm(Datos$quality ~ Datos$fixed.acidity + pHi, data=Datos)
summary(Modelo)

