library(foreign)

## Archivo 1 - Hogares

hogares <- read.dbf("TIC_2015_HOGARES.dbf")

saveRDS(hogares, file = "hogares.rds")

# Archivo 2 - Vivienda

vivienda <- read.dbf("TIC_2015_VIVIENDAS.dbf")

saveRDS(vivienda, file = "vivienda.rds")

# Archivo 3 - Usuarios

usuarios1 <- read.dbf("TIC_2015_USUARIOS.dbf")

saveRDS(usuarios1, file = "usuarios1.rds")


# Archivo 4 - RES

res1 <- read.dbf("TIC_2015_RESIDENTES.dbf")

saveRDS(res1, file = "res1.rds")

## Merge de las bases de datos res1 y usuarios1

library(tidyverse)
library(dplyr)

base_endutih <- merge(res1, usuarios1, join="leftouter", split=TRUE)
