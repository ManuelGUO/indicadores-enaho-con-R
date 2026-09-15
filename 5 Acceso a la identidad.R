# Librerías ----
library(haven)
library(dplyr)
library(survey)
library(labelled)

options(survey.lonely.psu = "adjust")


# Acceso a la identidad ----

ruta_data <- "Data/Enaho 2025/"

salud <- read_dta(
  paste0(ruta_data, "1031-Modulo04/enaho01a-2025-400.dta")
)


## Variables de control ----

salud <- salud %>%
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
    ),
    grupo_etario = case_when(
      p208a >= 0 & p208a <= 5 ~ 1, # 0 a 5 años
      p208a >= 6 & p208a <= 10 ~ 2, # 6 a 10 años
      p208a >= 11 & p208a <= 17 ~ 3, # 11 a 17 años
      p208a >= 18 ~ 4, # Mayor de 18
      TRUE ~ NA_real_
    )
  )

salud$area <- labelled(
  salud$area,
  labels = c("Urbano" = 1, "Rural" = 2)
)
var_label(salud$area) <- "Área de residencia"

salud$region_natural <- labelled(
  salud$region_natural,
  labels = c(
    "Costa" = 1,
    "Sierra" = 2,
    "Selva" = 3
  )
)
var_label(salud$region_natural) <- "Región natural"

salud$grupo_etario <- labelled(
  salud$grupo_etario,
  labels = c(
    "0 a 5 años" = 1,
    "6 a 10 años" = 2,
    "11 a 17 años" = 3,
    "18 y más" = 4
  )
)
var_label(salud$grupo_etario) <- "Grupo de edad"


## Acceso a la identidad menor de 18 años ----
salud <- salud %>%
  mutate(
    tiene_dni = case_when(
      p401c == 1 ~ 1, # Tiene DNI
      p401c == 2 ~ 2, # No tiene DNI
      p401c == 3 ~ 3, # No sabe
      TRUE ~ NA_real_
    )
  )

salud$tiene_dni <- labelled(
  salud$tiene_dni,
  labels = c("Tiene DNI" = 1, "No tiene DNI" = 2, "No sabe" = 3)
)
var_label(salud$tiene_dni) <- "Tenencia de DNI"


### Diseño de la encuesta ----

salud <- salud %>%
  mutate(
    area = as_factor(area),
    region_natural = as_factor(region_natural),
    grupo_etario = as_factor(grupo_etario),
    tiene_dni = as_factor(tiene_dni)
  ) %>%
  filter(grupo_etario != "18 y más")


disenio <- svydesign(
  id = ~conglome,
  strata = ~estrato,
  weights = ~factor07,
  data = salud,
  nest = TRUE
)


### Indicadores por área ----

tabla_dni_area <- svytable(
  ~tiene_dni + area,
  design = disenio
)

prop_dni_area <- prop.table(
  tabla_dni_area,
  margin = 2
) * 100

tabla_dni_area
prop_dni_area


### Indicadores por grupo etario ----

tabla_dni_edad <- svytable(
  ~tiene_dni + grupo_etario,
  design = disenio
)

prop_dni_edad <- prop.table(
  tabla_dni_edad,
  margin = 2
) * 100

tabla_dni_edad
prop_dni_edad


### Indicadores por área y grupo etario ----

tabla_dni_area_edad <- svytable(
  ~tiene_dni + area +grupo_etario,
  design = disenio
)

prop_dni_area_edad <- prop.table(
  tabla_dni,
  margin = c(2, 3)
) * 100

tabla_dni_area_edad
prop_dni_area_edad
