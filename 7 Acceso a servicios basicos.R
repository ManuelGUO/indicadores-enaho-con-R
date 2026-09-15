# Librerías ----
library(haven)
library(dplyr)
library(survey)
library(labelled)

options(survey.lonely.psu = "adjust")


# Acceso a los servicios básicos ----

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


## Preparación de la data ----

hogar <- hogar %>%
  mutate(
    agua = case_when(
      p110 == 1 ~ 1, # Red pública dentro de la vivienda
      p110 == 2 ~ 2, # Red pública fuera de la vivienda
      p110 == 3 ~ 3, # Pilón de uso público
      TRUE ~ NA_real_
    ),
    alcantarillado = case_when(
      p111a %in% 1:2 ~ 1, # Red de alcantarillado
      p111a %in% 3:9 ~ 2, # Sin red de alcantarillado
      TRUE ~ NA_real_
    ),
    tipo_alcantarillado = as_factor(p111a),
    electricidad = case_when(
      p1121 == 1 ~ 1, # Red electrica
      p1123 == 1 ~ 0,
      p1124 == 1 ~ 0,
      p1125 == 1 ~ 0,
      p1126 == 1 ~ 0,
      p1127 == 1 ~ 0,
      TRUE ~ NA_real_
    )
  )

hogar$agua <- labelled(
  hogar$agua,
  labels = c(
    "Red pública dentro de la vivienda" = 1,
    "Red pública fuera de la vivienda" = 2,
    "Pilón de uso público" = 3
  )
)
var_label(hogar$agua) <- "Agua por red pública"

hogar$alcantarillado <- labelled(
  hogar$alcantarillado,
  labels = c(
    "Red de alcantarillado" = 1,
    "Sin red de alcantarillado" = 2
  )
)
var_label(hogar$alcantarillado) <- "Alcantarillado por red pública"

var_label(hogar$tipo_alcantarillado) <- "Tipo de alcantarillado"

hogar$electricidad <- labelled(
  hogar$electricidad,
  labels = c(
    "Red elétrica" = 1,
    "Sin red eléctrica" = 0
  )
)
var_label(hogar$electricidad) <- "Electricidad por red pública"


### Diseño de la encuesta ----

hogar <- hogar %>%
  mutate(
    area = as_factor(area),
    region_natural = as_factor(region_natural),
    agua = as_factor(agua),
    alcantarillado = as_factor(alcantarillado),
    tipo_alcantarillado = as_factor(tipo_alcantarillado),
    electricidad = as_factor(electricidad)
  )


disenio <- svydesign(
  id = ~conglome,
  strata = ~estrato,
  weights = ~factor07,
  data = hogar,
  nest = TRUE
)


## Resultados ----
### Agua de red pública ----
#### Indicadores por área ----

tabla_agua_area <- svytable(
  ~agua + area,
  design = disenio
)

prop_agua_area <- prop.table(
  tabla_agua_area,
  margin = 2
) * 100

tabla_agua_area
prop_agua_area


### Alcantarillado público ----
#### Indicadores por área ----

tabla_alcantarillado_area <- svytable(
  ~alcantarillado + area,
  design = disenio
)

prop_alcantarillado_area <- prop.table(
  tabla_alcantarillado_area,
  margin = 2
) * 100

tabla_alcantarillado_area
prop_alcantarillado_area


### Tipo de alcantarillado ----
#### Indicadores por area ----

tabla_tipo_alcantarillado_area <- svytable(
  ~tipo_alcantarillado + area,
  design = disenio
)

prop_tipo_alcantarillado_area <- prop.table(
  tabla_tipo_alcantarillado_area,
  margin = 2
) * 100

tabla_tipo_alcantarillado_area
prop_tipo_alcantarillado_area


### Alumbrado público ----
#### Indicadores por area ----

tabla_electricidad_area <- svytable(
  ~electricidad + area,
  design = disenio
)

prop_electricidad_area <- prop.table(
  tabla_electricidad_area,
  margin = 2
) * 100

tabla_electricidad_area
prop_electricidad_area
