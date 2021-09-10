## Analisis de datos de la endutih

# Instalamos

install.packages("sjmisc")
install.packages("sjlabelled")
install.packages("tidyverse")
install.packages("haven")
install.packages("survey")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("pacman")

## Cargamos librerias 

library(pacman)
p_load(sjmisc, sjlabelled, tidyverse, haven, survey, dplyr, ggplot2)

## Carga de base de datos

base_endutih <- readRDS("base_endutih.rds")

## CONOCIENDO LA ESTRUCTURA DE LOS DATOS ##

# Estructura de la base de datos y variables
str(base_endutih)
glimpse(base_endutih)

# Para una sola variable: Generalmente ¿Cuántas horas al día utiliza Internet?
str(base_endutih$P7_4)

# Tabla de frecuencias
base_endutih %>% frq(P7_4)

# Dado que es factor, la convertimos a numerica
base_endutih <- base_endutih %>%
  mutate_at("P7_4", ~as.numeric(.)) 

## Estadisticos descriptivos del uso de internet
summary(base_endutih$P7_4)

## La variable de edad también está en factor y la convertimos a numerica
base_endutih <- base_endutih %>%
  mutate_at("EDAD", ~as.numeric(.))

## Estadisticos descriptivos de la edad
summary(base_endutih$EDAD)

## Tabla de frecuencias
base_endutih %>% frq(SEXO)

## PREGUNTA 1
## Para los datos en general, determina el promedio de tiempo dedicado a Internet.
summary(base_endutih$P7_4)

## PREGUNTA 2
## Para el total de datos, determina la varianza y la desviación estándar del tiempo que dedican al uso de Internet.
# varianza
var(base_endutih$P7_4, na.rm = TRUE)
# desviación estandar
sd(base_endutih$P7_4, na.rm = TRUE)

## PREGUNTA 3
# Para los datos por género, determina en promedio quién dedica más tiempo a Internet: 
## hombres o mujeres.

tapply(base_endutih$P7_4, base_endutih$SEXO, summary)

## Distribución de datos
qplot(P7_4, data=base_endutih, geom="histogram", fill=factor(SEXO), alpha=I(.5), 
      main="Distribución de las horas de uso de internet", xlab="Horas dedicadas", 
      ylab="Frecuencia")

## PREGUNTA 4 y 5
## Para los hombres y mujeres, calcula el coeficiente de correlación lineal 
## entre la edad y el tiempo dedicado al uso de Internet.

## Vamos a hacer uso de plyr para crear una funcion que nos permita ver 
## la correlacion por grupo, es decir, genero

install.packages("plyr")
library(plyr)

require(plyr)
func <- function(base_endutih)
{
  return(data.frame(COR = cor(base_endutih$EDAD, base_endutih$P7_4, use="complete.obs")))
}

ddply(base_endutih, .(SEXO), func)

## PREGUNTA 6
## Para los datos por género: determina la mediana de la edad y del tiempo dedicado a Internet.

tapply(base_endutih$P7_4, base_endutih$SEXO, summary)

## PARTE 2 - PRUEBA DE HIPOTESIS
## Imagina que el promedio que dedica una persona a Internet 
## (sin importar su género) es de 7 horas diarias. 
## Con los datos anteriores, prueba las siguientes hipótesis:

## H0: µ = 7 contra la alternativa de que Ha : µ ≠ 7 
## con un nivel de significancia de 0.05. 
## Realiza todas las etapas de una prueba de hipótesis 
## y concluye sobre el contexto del problema. 
## ¿Es el tiempo promedio dedicado a Internet diferente a 7?

mean.x <- 4.42
SE.x   <- 3.60 / sqrt(length(3.60))
Ho     <- 7
z      <- (mean.x - Ho) / SE.x

P.Ho  <- pnorm(z, lower.tail=FALSE)
P.Ho

## Intervalo de confianza
install.packages("Rmisc")
library(Rmisc)

CIvector <- CI(na.omit(base_endutih$P7_4))

CIvector
