# Librerías ----
library(haven)
library(dplyr)
library(survey)
library(labelled)

options(survey.lonely.psu = "adjust")


# Acceso programas sociales ----

ruta_data <- "Data/Enaho 2025/"

programas <- read_dta(
  paste0(ruta_data, "1031-Modulo37/enaho01-2025-700.dta")
)


## Variables de control ----

programas <- programas %>%
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

programas$area <- labelled(
  programas$area,
  labels = c("Urbano" = 1, "Rural" = 2)
)
var_label(programas$area) <- "Área de residencia"

programas$region_natural <- labelled(
  programas$region_natural,
  labels = c(
    "Costa" = 1,
    "Sierra" = 2,
    "Selva" = 3
  )
)
var_label(programas$region_natural) <- "Región natural"


## Hogares con al menos un miembro beneficiario con algún programa alimentario ----

programas <- programas %>%
  mutate(
    al_menos_un_programa = case_when(
      p701_09 == 1 ~ 0, # No recibió
      p701_09 == 0 ~ 1, # Recibió
      TRUE ~ NA_real_
    ),
    tipo_programa = case_when(
      p701_01 == 1 ~ 1, # Vaso de leche
      p701_02 == 1 ~ 2, # Comedor popular
      p701_03 == 1 ~ 3, # Desayuno escolar
      p701_04 == 1 ~ 4, # Almuerzo escolar
      p701_05 == 1 ~ 5, # Wawa Wasi / Cuna más
      p701_10 == 1 ~ 6, # Víveres municipales
      p701_06 == 1 ~ 7, # Otros
      p701_07 == 1 ~ 7, # Otros
      p701_08 == 1 ~ 7, # Otros
      TRUE ~ NA_real_
    )
  )

programas$al_menos_un_programa <- labelled(
  programas$al_menos_un_programa,
  labels = c("Recibió" = 1, "No recibió" = 0)
)
var_label(programas$al_menos_un_programa) <- "Recibió al menos un programa alimentario"

programas$tipo_programa <- labelled(
  programas$tipo_programa,
  labels = c("Vaso de leche" = 1, "Comedor popular" = 2,
             "Desayuno escolar" = 3, "Almuerzo escolar" = 4,
             "Wawa Wasi / Cuna más" = 5, "Víveres municipales" = 6,
             "Otros" = 7)
)
var_label(programas$tipo_programa) <- "Programa que recibió"

### Por área de referencia ----
#### Diseño de la encuesta ----

programas <- programas %>%
  mutate(
    area = as_factor(area),
    region_natural = as_factor(region_natural),
    al_menos_un_programa = as_factor(al_menos_un_programa),
    tipo_programa = as_factor(tipo_programa)
  )

disenio <- svydesign(
  id = ~conglome,
  strata = ~estrato,
  weights = ~factor07,
  data = programas,
  nest = TRUE
)

#### Resultados ----
tabla_un_programa <- svytable(
  ~al_menos_un_programa + area,
  design = disenio
)

prop_un_programa <- prop.table(
  tabla_un_programa,
  margin = 1
) * 100

tabla_un_programa
prop_un_programa


### Por región natural ----
#### Resultados ----
tabla_un_programa_regnat <- svytable(
  ~al_menos_un_programa + region_natural,
  design = disenio
)

prop_un_programa_regnat <- prop.table(
  tabla_un_programa_regnat,
  margin = 2
) * 100

tabla_un_programa_regnat
prop_un_programa_regnat


## Tipo de programa ----
### Por área de referencia ----
#### Resultados ----
tabla_tipo_programa <- svytable(
  ~tipo_programa + area,
  design = disenio
)

prop_tipo_programa <- prop.table(
  tabla_tipo_programa,
  margin = 1
) * 100

tabla_tipo_programa
prop_tipo_programa


### Por región natural ----
#### Resultados ----
tabla_tipo_programa_regnat <- svytable(
  ~tipo_programa + region_natural,
  design = disenio
)

prop_tipo_programa_regnat <- prop.table(
  tabla_tipo_programa_regnat,
  margin = 2
) * 100

tabla_tipo_programa_regnat
prop_tipo_programa_regnat
