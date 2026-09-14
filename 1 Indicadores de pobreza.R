# Librerías ----
library(haven)
library(stringr)
library(dplyr)
library(survey)

options(survey.lonely.psu = "adjust")

# Indicadores de pobreza ----

ruta_data <- "Data/Enaho 2025/"

sumaria <- read_dta(paste0(ruta_data, "1031-Modulo34/sumaria-2025.dta"))


# Variable geográficas ----
sumaria <- sumaria %>%
  mutate(
    ubigeo = str_pad(as.character(ubigeo), width = 6, side = "left", pad = "0"),
    departamento_cod = substr(ubigeo, 1, 2),
    departamento = labelled(
      departamento_cod,
      c(
        "Amazonas" = "01", "Ancash" = "02", "Apurimac" = "03",
        "Arequipa" = "04", "Ayacucho" = "05", "Cajamarca" = "06",
        "Callao" = "07", "Cusco" = "08", "Huancavelica" = "09",
        "Huanuco" = "10", "Ica" = "11", "Junín" = "12",
        "La Libertad" = "13", "Lambayeque" = "14", "Lima" = "15",
        "Loreto" = "16", "Madre de Dios" = "17", "Moquegua" = "18",
        "Pasco" = "19", "Piura" = "20", "Puno" = "21",
        "San Martín" = "22", "Tacna" = "23", "Tumbes" = "24",
        "Ucayali" = "25"
      ),
      label = "Departamento"
    ),
    departamento_f = as_factor(departamento),
    area_residencia = case_when(
      estrato <= 5 ~ 1,
      estrato >= 6 & estrato <= 8 ~ 2
    ),
    area_residencia = labelled(
      area_residencia,
      c("Urbano" = 1, "Rural" = 2),
      label = "Área de residencia"
    ),
    region_natural = case_when(
      dominio <= 3 | dominio == 8 ~ 1,
      dominio >= 4 & dominio <= 6 ~ 2,
      dominio == 7 ~ 3
    ),
    region_natural = labelled(
      region_natural,
      c(
        "Costa" = 1,
        "Sierra" = 2,
        "Selva" = 3
      ),
      label = "Región natural"
    )
  )


# Pobreza monetaria ----
sumaria <- sumaria %>%
  mutate(
    gpcm = gashog2d/(mieperho*12), # Gasto promedio per cápita mensual
    facpob = factor07*mieperho, # Factor de ponderación a nivel poblacional
    pobre_lab = case_when(
      gpcm < linpe ~ 1,
      gpcm >= linpe & gpcm < linea ~ 2,
      gpcm >= linea ~ 3
    ),
    pobre_lab = labelled(
      pobre_lab,
      c(
        "Pobre extremo" = 1,
        "Pobre no extremo" = 2,
        "No pobre" = 3
      ),
      label = "Pobreza monetaria"
    ),
    pobre_1 = if_else(gpcm < linea, 1, 0),
    pobre_1_lab = labelled(
      pobre_1,
      c("Pobre" = 1, "No pobre" = 0),
      label = "Pobreza monetaria total"
    )
  )


# Verificación de distribución ----
table(as_factor(sumaria$pobre_lab), useNA = "ifany")

table(as_factor(sumaria$pobre_1_lab), useNA = "ifany")

table(as_factor(sumaria$departamento_f),
      as_factor(sumaria$pobre_1_lab),
      useNA = "ifany")


# Características de la encuesta ----
disenio <- svydesign(
  ids = ~conglome,
  strata = ~estrato,
  weights = ~facpob,
  data = sumaria,
  nest = TRUE
)


# Resultados ----
## Incidencia de la pobreza monetaria ----
### Nacional ----
incidencia_nacional <- svymean(~pobre_1, design = disenio, na.rm = TRUE)
coef(incidencia_nacional) * 100
SE(incidencia_nacional) * 100

### Departamental ----
incidencia_departamental <- svyby(
  ~pobre_1,
  ~departamento_f,
  design = disenio,
  FUN = svymean,
  na.rm = TRUE
) %>%
  mutate(incidencia = pobre_1*100)

### FGT(0), FGT(1) y FGT(2)
sumaria <- sumaria %>%
  mutate(
    fgt0 = if_else(gpcm < linea, 1, 0), # Incidencia
    brecha = if_else(gpcm < linea, (linea - gpcm) / linea, 0), # Brecha
    severidad = if_else( # Severidad
      gpcm < linea,
      ((linea - gpcm) / linea)^2,
      0
    )
  )

disenio <- svydesign(
  ids = ~conglome,
  strata = ~estrato,
  weights = ~facpob,
  data = sumaria,
  nest = TRUE
)

#### FGT nacional ----

fgt <- svymean(
  ~fgt0 + brecha + severidad,
  design = disenio,
  na.rm = TRUE
)

fgt
coef(fgt) * 100
SE(fgt) * 100

#### FGT departamental ----
fgt_departamental <- svyby(
  ~fgt0 + brecha + severidad,
  ~departamento_f,
  design = disenio,
  FUN = svymean,
  na.rm = TRUE
)

fgt_departamental
coef(fgt_departamental) * 100
SE(fgt_departamental) * 100
