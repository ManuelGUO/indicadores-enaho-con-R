# Librerías ----
library(haven)
library(dplyr)
library(survey)
library(labelled)

options(survey.lonely.psu = "adjust")


# Empleo e ingreso ----

ruta_data <- "Data/Enaho 2025/"

empleo <- read_dta(
  paste0(ruta_data, "1031-Modulo05/enaho01a-2025-500.dta")
)


## Variables de control ----

empleo <- empleo %>%
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

empleo$area <- labelled(
  empleo$area,
  labels = c("Urbano" = 1, "Rural" = 2)
)
var_label(empleo$area) <- "Área de residencia"

empleo$region_natural <- labelled(
  empleo$region_natural,
  labels = c(
    "Costa" = 1,
    "Sierra" = 2,
    "Selva" = 3
  )
)
var_label(empleo$region_natural) <- "Región natural"


## Tasa de actividad según grupo de edad y nivel educativo alcanzado ----
empleo <- empleo %>%
  filter(p500i != 0) %>%
  mutate(
    residente_habitual = case_when(
      (p204 == 1 & p205 == 2) | (p204 == 2 & p206 == 1) ~ 1, # Residente habitual
      TRUE ~ NA_real_
    ),
    grupo_etario = case_when(
      p208a >= 0 & p208a <= 13 ~ 1, # 0 a 13 años
      p208a >= 14 & p208a <= 49 ~ 2, # 14 a 49 años
      p208a >= 50 & p208a <= 59 ~ 3, # 50 a 59 años
      p208a >= 60 ~ 4, # Mayor de 60
      TRUE ~ NA_real_
    ),
    nivel_educacion = case_when(
      p301a < 5 | p301a == 1 ~ 1, # Primaria
      p301a == 5 | p301a == 6 ~ 2, # Secundaria
      p301a == 7 | p301a == 8 ~ 3, # Superior no universitaria
      p301a == 9 | p301a == 10 | p301a == 11 ~ 4, # Superior universitaria
      TRUE ~ NA_real_
    ),
    pea = case_when(
      ocu500 %in% c(1, 2, 3) ~ 1, # PEA: ocupado, desocupado abierto, desocupado oculto
      ocu500 == 4 ~ 0, # No PEA
      TRUE ~ NA_real_ # código 0 sin etiqueta / posibles missing
    )
  )

empleo$residente_habitual <- labelled(
  empleo$residente_habitual,
  labels = c(
    "Residente habitual" = 1
  )
)
var_label(empleo$residente_habitual) <- "Residente habitual"

empleo$grupo_etario <- labelled(
  empleo$grupo_etario,
  labels = c(
    "0 a 13 años" = 1,
    "14 a 49 años" = 2,
    "50 a 59 años" = 3,
    "60 y más" = 4
  )
)
var_label(empleo$grupo_etario) <- "Grupo de edad"

empleo$nivel_educacion <- labelled(
  empleo$nivel_educacion,
  labels = c(
    "Primaria" = 1,
    "Secundaria" = 2,
    "Superior no universitaria" = 3,
    "Superior universitaria" = 4
  )
)
var_label(empleo$nivel_educacion) <- "Nivel educativo alcanzado"

empleo$pea <- labelled(
  empleo$pea,
  labels = c("No PEA" = 0, "PEA" = 1)
)
var_label(empleo$pea) <- "Condición de actividad (PEA)"

## Preparación de la data ----

empleo <- empleo %>%
  mutate(
    region_natural = as_factor(region_natural),
    area = as_factor(area),
    residente_habitual = as_factor(residente_habitual),
    grupo_etario = as_factor(grupo_etario),
    nivel_educacion = as_factor(nivel_educacion),
    pea = as_factor(pea)
  )


### Diseño de la encuesta ----

disenio <- empleo %>%
  filter(!is.na(residente_habitual), p208a >= 14) %>%
  svydesign(
    ids = ~conglome + vivienda + hogar,
    strata = ~estrato,
    weights = ~fac500a,
    data = .,
    nest = TRUE
  )


### Resultado ----
tabla_edad_pea <- svytable(
  ~grupo_etario + pea,
  design = disenio
)

tasa_edad_pea <- prop.table(tabla_actividad, margin = c(1, 2))[, , "PEA"]
round(tasa_edad_pea * 100, 1)
