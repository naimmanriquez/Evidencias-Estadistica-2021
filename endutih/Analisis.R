## Analisis de datos de la endutih

# 1. Removemos notación científica
options(scipen = 999)

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

endutih_mayores <- base_endutih %>%
  filter(EDAD %in% c(18:99))

## CONOCIENDO LA ESTRUCTURA DE LOS DATOS ##

# Estructura de la base de datos y variables
str(base_endutih)
glimpse(base_endutih)

# Para una sola variable: Generalmente cuantas horas al dia utiliza Internet?
str(base_endutih$P7_4)

# Tabla de frecuencias
base_endutih %>% frq(P7_4)

# Dado que es factor, la convertimos a numerica
base_endutih <- base_endutih %>%
  mutate_at("P7_4", ~as.numeric(.)) 

## Estadisticos descriptivos del uso de internet
summary(base_endutih$P7_4)

## La variable de edad tambien esta¡ en factor y la convertimos a numerica
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
## Para el total de datos, determina la varianza y la desviacion estandar del tiempo que dedican al uso de Internet.
# varianza
var(base_endutih$P7_4, na.rm = TRUE)
# desviacion estandar
sd(base_endutih$P7_4, na.rm = TRUE)

## PREGUNTA 3
# Para los datos por genero, determina en promedio quien dedica mas tiempo a Internet: 
## hombres o mujeres.

tapply(base_endutih$P7_4, base_endutih$SEXO, summary)

## Distribucion de datos
qplot(P7_4, data=base_endutih, geom="histogram", fill=factor(SEXO), alpha=I(.5), 
      main="Distribucion de las horas de uso de internet", xlab="Horas dedicadas", 
      ylab="Frecuencia")

ggplot(data=endutih_mayores, aes(EDAD, P7_4, col = SEXO)) + geom_point() +
  ggtitle("Edad vs Uso de Internet") +
  ylab("Uso de Internet") +
  xlab("Edad") + 
  theme(plot.title = element_text(hjust = 0.5)) + theme_bw()

## PREGUNTA 4 y 5
## Para los hombres y mujeres, calcula el coeficiente de correlacion lineal 
## entre la edad y el tiempo dedicado al uso de Internet.

# Dado que es factor, la convertimos a numerica
base_endutih <- base_endutih %>%
  mutate_at("SEXO", ~as.numeric(.)) 

r <- by(base_endutih, base_endutih$SEXO, FUN = function(X) cor(base_endutih$EDAD, base_endutih$P7_4, method = "spearman", use="complete.obs"))

r

## PREGUNTA 6
## Para los datos por genero: determina la mediana de la edad y del tiempo dedicado a Internet.

tapply(base_endutih$P7_4, base_endutih$SEXO, summary)

## PARTE 2 - PRUEBA DE HIPOTESIS
## Imagina que el promedio que dedica una persona a Internet 
## (sin importar su genero) es de 7 horas diarias. 
## Con los datos anteriores, prueba las siguientes hipotesis:

## H0: µ = 7 contra la alternativa de que Ha : µ =/ 7 
## con un nivel de significancia de 0.05. 
## Realiza todas las etapas de una prueba de hipotesis 
## y concluye sobre el contexto del problema. 
## ¿Es el tiempo promedio dedicado a Internet diferente a 7?

mu0=7

mu0

sol.test=t.test(base_endutih$P7_4,mu=7,alternative="two.sided",conf.level=0.95)

#Resumen del test
sol.test

## Intervalo de confianza
install.packages("Rmisc")
library(Rmisc)

CIvector <- CI(na.omit(base_endutih$P7_4))

CIvector

