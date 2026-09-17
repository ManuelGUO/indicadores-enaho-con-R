# Librerías ----
library(haven)
library(dplyr)
library(survey)
library(labelled)

options(survey.lonely.psu = "adjust")


# Combustible usado ----

ruta_data <- "Data/Enaho 2025/"

hogar <- read_dta(
  paste0(ruta_data, "1031-Modulo01/enaho01-2025-100.dta")
)


## Variables de control ----

hogar <- hogar %>%
  mutate(
    area = case_when(
      estrato %in% 1:5 ~ 1, # Urbano
      estrato %in% 6:8 ~ 2, # Rural
      TRUE ~ NA_real_
    ),
    region_natural = case_when(
      dominio <= 3 | dominio == 8 ~ 1, # Costa
      dominio >= 4 & dominio <= 6 ~ 2, # Sierra
      dominio == 7 ~ 3, # Selva
      TRUE ~ NA_real_
    )
  )

hogar$area <- labelled(
  hogar$area,
  labels = c("Urbano" = 1, "Rural" = 2)
)
var_label(hogar$area) <- "Área de residencia"

hogar$region_natural <- labelled(
  hogar$region_natural,
  labels = c(
    "Costa" = 1,
    "Sierra" = 2,
    "Selva" = 3
  )
)
var_label(hogar$region_natural) <- "Región natural"


# Preparación de la data ----

hogar <- hogar %>%
  mutate(
    combustible = as_factor(p113a),
    region_natural = as_factor(region_natural),
    area = as_factor(area),
  )


### Diseño de la encuesta ----

disenio <- svydesign(
  id = ~conglome,
  strata = ~estrato,
  weights = ~factor07,
  data = hogar,
  nest = TRUE
)


## Resultados ----
#### Indicadores por área ----

tabla_combustible_area <- svytable(
  ~combustible + area,
  design = disenio
)

prop_combustible_area <- prop.table(
  tabla_combustible_area,
  margin = 2
) * 100

tabla_combustible_area
prop_combustible_area

#### Indicadores por región natural ----

tabla_combustible_regnat <- svytable(
  ~combustible + region_natural + area,
  design = disenio
)

prop_combustible_regnat <- prop.table(
  tabla_combustible_regnat,
  margin = 2
) * 100

tabla_combustible_regnat
prop_combustible_regnat
