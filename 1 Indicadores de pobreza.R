# Librerías ----
library(haven)
library(dplyr)
library(janitor)
library(stringr)


# Indicadores de pobreza ----

ruta_data <- "Data/Enaho 2025/"

sumaria <- read_dta(paste0(ruta_data, "1031-Modulo34/sumaria-2025.dta"))
hogar <- read_dta(paste0(ruta_data, "1031-Modulo01/enaho01-2025-100.dta"))


# Variable geográficas ----
## Departamento ----
sumaria <- sumaria %>%
  mutate(
    ubigeo_departamento = substr(ubigeo, 1, 2)
  )