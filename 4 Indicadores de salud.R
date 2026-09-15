# Librerías ----
library(haven)
library(dplyr)
library(survey)
library(labelled)

options(survey.lonely.psu = "adjust")


# Indicadores de salud ----

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
      p208a >= 0 & p208a <= 14 ~ 1, # 0 a 14 años
      p208a >= 15 & p208a <= 49 ~ 2, # 15 a 49 años
      p208a >= 50 & p208a <= 59 ~ 3, # 50 a 59 años
      p208a >= 60 ~ 4, # Mayor de 60
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
    "0 a 14 años" = 1,
    "15 a 49 años" = 2,
    "50 a 59 años" = 3,
    "60 y más" = 4
  )
)
var_label(salud$grupo_etario) <- "Grupo de edad"


## Problemas de salud crónico ----

salud <- salud %>%
  mutate(
    cronico = case_when(
      p401 == 1 ~ 1, # Paciente crónico
      p401 == 2 ~ 0, # Sin problemas crónicos
      TRUE ~ NA_real_
    ),
    otras_complicaciones = case_when(
      p4021 == 1 ~ 1,
      p4022 == 1 ~ 1,
      p4023 == 1 ~ 1,
      p4024 == 1 ~ 1,
      p4026 == 1 ~ 1,
      p4025 == 1 ~ 0,
      TRUE ~ NA_real_
    ) # 1 = Tiene otras complicaciones
  ) %>%
  mutate(
    cronico_otros = case_when(
      cronico == 1 & otras_complicaciones == 1 ~ 1, # Crónico y otros
      cronico == 1 & otras_complicaciones == 0 ~ 2, # Solo crónico
      TRUE ~ NA_real_
    )
  )

salud$cronico <- labelled(
  salud$cronico,
  labels = c("Paciente crónico" = 1, "Sin problemas crónicos" = 0)
)
var_label(salud$cronico) <- "Problemas de salud crónicos"

salud$cronico_otros <- labelled(
  salud$cronico_otros,
  labels = c("Paciente crónico y con otras complicaciones en las últimas 4 semanas" = 1,
             "Solo problema de salud crónico" = 2)
)
var_label(salud$cronico_otros) <- "Problemas de salud crónicos y otras complicaciones"


### Diseño de la encuesta ----

salud <- salud %>%
  mutate(
    area = as_factor(area),
    region_natural = as_factor(region_natural),
    grupo_etario = as_factor(grupo_etario),
    cronico = as_factor(cronico),
    otras_complicaciones = as_factor(otras_complicaciones),
    cronico_otros = as_factor(cronico_otros)
  )


disenio <- svydesign(
  id = ~conglome,
  strata = ~estrato,
  weights = ~factor07,
  data = salud,
  nest = TRUE
)


### Indicadores por área ----

tabla_cronico_area <- svytable(
  ~cronico + area,
  design = disenio
)

prop_cronico_area <- prop.table(
  tabla_cronico_area,
  margin = 2
) * 100

tabla_cronico_area
prop_cronico_area


### Indicadores por región natural ----

tabla_cronico_region <- svytable(
  ~cronico + region_natural,
  design = disenio
)

prop_cronico_region <- prop.table(
  tabla_cronico_region,
  margin = 2
) * 100

tabla_cronico_region
prop_cronico_region


### Indicadores por grupo etario ----

tabla_cronico_edad <- svytable(
  ~cronico + grupo_etario,
  design = disenio
)

prop_cronico_edad <- prop.table(
  tabla_cronico_edad,
  margin = 2
) * 100

tabla_cronico_edad
prop_cronico_edad


### Problemas crónicos por área, región y grupo etario ----
tabla_cronico <- svytable(
  ~cronico + area + region_natural + grupo_etario,
  design = disenio
)

prop_cronico <- prop.table(
  tabla_cronico,
  margin = c(2, 3, 4)
) * 100

tabla_cronico
prop_cronico


### Crónico y otras complicaciones ----

tabla_cronico_otros <- svytable(
  ~cronico_otros + area + region_natural + grupo_etario,
  design = disenio
)

prop_cronico_otros <- prop.table(
  tabla_cronico_otros,
  margin = c(2, 3, 4)
) * 100

tabla_cronico_otros
prop_cronico_otros


## Búsqueda de atención en salud ----

salud <- salud %>%
  mutate(
    busco_atencion = case_when(
      p4031 == 1 ~ 1,
      p4032 == 1 ~ 1,
      p4033 == 1 ~ 1,
      p4034 == 1 ~ 1,
      p4035 == 1 ~ 1,
      p4036 == 1 ~ 1,
      p4037 == 1 ~ 1,
      p4038 == 1 ~ 1,
      p4039 == 1 ~ 1,
      p40310 == 1 ~ 1,
      p40311 == 1 ~ 1,
      p40313 == 1 ~ 1,
      p40314 == 1 ~ 0,
      TRUE ~ NA_real_
    ) # 1 = Buscó atención, 0 = No buscó atención
  )

salud$busco_atencion <- labelled(
  salud$busco_atencion,
  labels = c("Buscó atención" = 1, "No buscó atención" = 0)
)
var_label(salud$busco_atencion) <- "Búsqueda de atención"


### Diseño de la encuesta ----

salud <- salud %>%
  mutate(
    busco_atencion = as_factor(busco_atencion)
  )


disenio <- svydesign(
  id = ~conglome,
  strata = ~estrato,
  weights = ~factor07,
  data = salud,
  nest = TRUE
)


### Indicadores por área ----

tabla_atencion_area <- svytable(
  ~busco_atencion + area,
  design = disenio
)

prop_atencion_area <- prop.table(
  tabla_atencion_area,
  margin = 2
) * 100

tabla_atencion_area
prop_atencion_area


## Lugar de consulta en salud ----

salud <- salud %>%
  mutate(
    lugar_atencion = case_when(
      p4031 == 1 ~ 1, # MINSA
      p4032 == 1 ~ 1, # MINSA
      p4033 == 1 ~ 1, # MINSA
      p4034 == 1 ~ 2, # EsSalud
      p4035 == 1 ~ 1, # MINSA
      p4036 == 1 ~ 2, # EsSalud
      p4037 == 1 ~ 3, # FF.AA. y/o PNP
      p4038 == 1 ~ 4, # Particular
      p4039 == 1 ~ 4, # Particular
      p40310 == 1 ~ 5, # Farmacia o botica
      p40311 == 1 ~ 6, # Otros
      p40313 == 1 ~ 6, # Otros
      TRUE ~ NA_real_
    )
  )

salud$lugar_atencion <- labelled(
  salud$lugar_atencion,
  labels = c("MINSA" = 1, "EsSalud" = 2,
             "FF.AA. y/o PNP" = 3, "Particular" = 4,
             "Farmacia o botica" = 5, "Otros" = 6)
)
var_label(salud$lugar_atencion) <- "Lugar de atención"


### Diseño de la encuesta ----

salud <- salud %>%
  mutate(
    lugar_atencion = as_factor(lugar_atencion)
  )


disenio <- svydesign(
  id = ~conglome,
  strata = ~estrato,
  weights = ~factor07,
  data = salud,
  nest = TRUE
)


### Indicadores por área ----

tabla_lugar_area <- svytable(
  ~lugar_atencion + area,
  design = disenio
)

prop_lugar_area <- prop.table(
  tabla_lugar_area,
  margin = 2
) * 100

tabla_lugar_area
prop_lugar_area


## Acceso a seguro de salud ----

salud <- salud %>%
  mutate(
    seguro = case_when(
      p4191 == 1 & p4192 == 2 & p4193 == 2 & p4194 == 2 & p4195 == 2 & p4196 == 2 & p4197 == 2 & p4198 == 2 ~ 1, # Solo EsSalud
      p4191 == 2 & p4192 == 2 & p4193 == 2 & p4194 == 2 & p4195 == 1 & p4196 == 2 & p4197 == 2 & p4198 == 2 ~ 2, # Solo SIS
      p4191 == 2 & p4195 == 2 & (p4192 == 1 | p4193 == 1 | p4194 == 1 | p4196 == 1 | p4197 == 1 | p4198 == 1) ~ 3, # Otros seguros
      TRUE ~ NA_real_
    )
  )

salud$seguro <- labelled(
  salud$seguro,
  labels = c("Solo EsSalud" = 1, "Solo SIS" = 2,
             "Otros seguros" = 3)
)
var_label(salud$seguro) <- "Tipo de seguro de salud"


### Diseño de la encuesta ----

salud <- salud %>%
  mutate(
    seguro = as_factor(seguro)
  )


disenio <- svydesign(
  id = ~conglome,
  strata = ~estrato,
  weights = ~factor07,
  data = salud,
  nest = TRUE
)


### Indicadores por área ----

tabla_seguro_area <- svytable(
  ~seguro + area,
  design = disenio
)

prop_seguro_area <- prop.table(
  tabla_seguro_area,
  margin = 2
) * 100

tabla_seguro_area
prop_seguro_area
