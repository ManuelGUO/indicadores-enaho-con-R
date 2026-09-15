# Librerías ----
library(haven)
library(dplyr)
library(survey)
library(labelled)

options(survey.lonely.psu = "adjust")


# Indicadores de educación ----

ruta_data <- "Data/Enaho 2025/"

educacion <- read_dta(
  paste0(ruta_data, "1031-Modulo03/enaho01a-2025-300.dta")
)


## Variables geográficas ----

educacion <- educacion %>%
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

educacion$area <- labelled(
  educacion$area,
  labels = c("Urbano" = 1, "Rural" = 2)
)
var_label(educacion$area) <- "Área de residencia"

educacion$region_natural <- labelled(
  educacion$region_natural,
  labels = c(
    "Costa" = 1,
    "Sierra" = 2,
    "Selva" = 3
  )
)
var_label(educacion$region_natural) <- "Región natural"


## Matrícula ----
# Edad normativa:
# Inicial: 3-5 años
# Primaria: 6-11 años
# Secundaria: 12-16 años
# https://www.gob.pe/institucion/minedu/informes-publicaciones/2742610-edades-normativas

educacion <- educacion %>%
  mutate(
    edad_normativa = case_when(
      p208a >= 6 & p208a <= 11 ~ 1, # Primaria
      p208a >= 12 & p208a <= 16 ~ 2, # Secundaria
      TRUE ~ NA_real_
    ),
    matricula = case_when(
      p208a >= 6 & p208a <= 16 & p306 == 1 ~ 1, # Matriculado
      p208a >= 6 & p208a <= 16 & p306 == 2 ~ 2, # No matriculado
      TRUE ~ NA_real_
    )
  )

educacion$edad_normativa <- labelled(
  educacion$edad_normativa,
  labels = c(
    "Primaria" = 1,
    "Secundaria" = 2
  )
)
var_label(educacion$edad_normativa) <- "Edad normativa"

educacion$matricula <- labelled(
  educacion$matricula,
  labels = c(
    "Matriculado" = 1,
    "No matriculado" = 2
  )
)
var_label(educacion$matricula) <- "Matrícula escolar"


### Convertir a factor ----

educacion <- educacion %>%
  mutate(
    edad_normativa = as_factor(edad_normativa),
    matricula = as_factor(matricula),
    p307 = as_factor(p307),
    p306 = as_factor(p306),
    p301a = as_factor(p301a)
    area = as_factor(area),
    region_natural = as_factor(region_natural),
  )


# Diseño muestral ----

disenio <- svydesign(
  id = ~conglome,
  strata = ~estrato,
  weights = ~factora07,
  data = educacion,
  nest = TRUE
)


### Resultado ----

tabla_matricula <- svytable(
  ~matricula + edad_normativa,
  design = subset(
    disenio,
    !is.na(matricula) & !is.na(edad_normativa)
  )
)

tabla_matricula

### Porcentaje por columna ----

porcentajes <- prop.table(
  tabla_matricula,
  margin = 2
) * 100

round(porcentajes, 2)


## Asistencia escolar ----

### Por área de residencia ----

tabla_asistencia_area <- svytable(
  ~p307 + area,
  design = subset(
    disenio,
    p208a >= 6 & p208a <= 17 &
      !is.na(p307) &
      !is.na(area)
  )
)

tabla_asistencia_area

#### Porcentaje por columna ----

porcentaje_asistencia_area <- prop.table(
  tabla_asistencia_area,
  margin = 2
) * 100

round(porcentaje_asistencia_area, 2)


### Asistencia escolar por edad normativa ----

tabla_asistencia_edad <- svytable(
  ~p307 + edad_normativa,
  design = subset(
    disenio,
    p208a >= 6 & p208a <= 16 &
      !is.na(p307) &
      !is.na(edad_normativa)
  )
)

tabla_asistencia_edad

#### Porcentaje por columna ----

porcentaje_asistencia_edad <- prop.table(
  tabla_asistencia_edad,
  margin = 2
) * 100

round(porcentaje_asistencia_edad, 2)


## Grado de estudios: Primaria ----

tabla_grado_primaria <- svytable(
  ~p308c + p208a,
  design = subset(
    disenio,
    p208a >= 6 & p208a <= 11 &
      !is.na(p308c)
  )
)

tabla_grado_primaria


## Año de estudios: Secundaria ----

tabla_anio_secundaria <- svytable(
  ~p308b + p208a,
  design = subset(
    disenio,
    p208a >= 12 & p208a <= 17 &
      !is.na(p308b)
  )
)

tabla_anio_secundaria


## Analfabetismo ----

educacion <- educacion %>%
  mutate(
    analfabeto = case_when(
      p208a >= 15 & p204 == 1 & p302 == 2 ~ 1, # Es analfabeto
      p208a >= 15 & p204 == 1             ~ 0, # No es analfabeto
      TRUE ~ NA_real_
    )
  )

educacion$analfabeto <- labelled(
  educacion$analfabeto,
  labels = c(
    "No analfabeto" = 0,
    "Analfabeto" = 1
  )
)

var_label(educacion$analfabeto) <- "Analfabetismo"


### Convertir a factor ----

educacion <- educacion %>%
  mutate(
    analfabeto = as_factor(analfabeto)
  )


### Recrear el diseño ----

disenio <- svydesign(
  id = ~conglome,
  strata = ~estrato,
  weights = ~factora07,
  data = educacion,
  nest = TRUE
)


### Resultado ----

tabla_analfabetismo <- svytable(
  ~analfabeto,
  design = subset(
    disenio,
    !is.na(analfabeto)
  )
)

tabla_analfabetismo

### Porcentaje ----

porcentaje_analfabetismo <- prop.table(
  tabla_analfabetismo
) * 100

round(porcentaje_analfabetismo, 2)


## Nivel de escolaridad ----

tabla_nivel_escolaridad <- svytable(
  ~p301a,
  design = subset(
    disenio,
    p208a >= 18 &
      !is.na(p301a)
  )
)

tabla_nivel_escolaridad

### Porcentaje ----

porcentaje_nivel_escolaridad <- prop.table(
  tabla_nivel_escolaridad
) * 100

round(porcentaje_nivel_escolaridad, 2)
