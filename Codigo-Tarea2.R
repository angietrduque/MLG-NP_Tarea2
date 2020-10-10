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
dim(Datos)
names(Datos)
str(Datos)

# Categrorizar variable pH a pHi
summary(Datos[,"pH"])
pHi <- cut(Datos$pH, breaks = c(2.6,3.1,3.6,4.1), labels = c("Bajo", "Medio", "Alto"))
head(pHi)
table(pHi)

# Estadisticas descriptivas
summary(Datos)

# Analisis univariado
p1 <- ggplot(Datos) + geom_histogram(aes(alcohol), color="black", fill="#ce2d4f")
p2 <- ggplot(Datos) + geom_histogram(aes(chlorides), color="black", fill="#ce6d8b")
p3 <- ggplot(Datos) + geom_histogram(aes(citric.acid), color="black", fill="#cebbc9")
p4 <- ggplot(Datos) + geom_histogram(aes(density), color="black", fill="#4056f4")
p5 <- ggplot(Datos) + geom_histogram(aes(fixed.acidity), color="black", fill="#470ff4")
p6 <- ggplot(Datos) + geom_histogram(aes(free.sulfur.dioxide), color="black", fill="#e54b4b")
p7 <- ggplot(Datos) + geom_histogram(aes(pH), color="black", fill="#ffa987")
p8 <- ggplot(Datos) + geom_histogram(aes(quality), color="black", fill="#c8d5b9")
p9 <- ggplot(Datos) + geom_histogram(aes(residual.sugar), color="black", fill="#4a7c59")
p10 <- ggplot(Datos) + geom_histogram(aes(sulphates), color="black", fill="#c4b7cb")
p11 <- ggplot(Datos) + geom_histogram(aes(total.sulfur.dioxide), color="black", fill="#98e2c6")
p12 <- ggplot(Datos) + geom_histogram(aes(volatile.acidity), color="black", fill="#06bee1")

grid.arrange(p1, p2, p3, p4, p5, p6,p7, p8, p9, p10, p11, p12, ncol= 3)

# Correlacion
corrplot(cor(Datos), method = "number")

# Variable indicadora: pHi
G1 <- ggplot(data = Datos, aes(x=pHi, fill=pHi)) +
  geom_bar(position="dodge") + ylab("") + xlab(" ") +
  scale_fill_discrete(name = "Nivel pH:", labels = c("Bajo", "Medio", "Alto"))
G1

# Analisis bivariado: Variable de respuesta calidad y explicativas pHi y Acidez fija
G2 <- ggplot(Datos, aes(group = cut_width(quality, 1)))+ 
  geom_boxplot(aes(quality, fixed.acidity), colour = "#417b5a")+
  xlab("Calidad")+ylab("Acidez fija")
G2

# Modelo 
Modelo <- glm(Datos$quality ~ Datos$fixed.acidity + pHi, data=Datos)
summary(Modelo)

