# Librerías ----
library(haven)
library(dplyr)
library(survey)
library(labelled)

options(survey.lonely.psu = "adjust")


# Necesidades básicas insatisfechas ----

ruta_data <- "Data/Enaho 2025/"

sumaria <- read_dta(paste0(ruta_data, "1031-Modulo34/sumaria-2025.dta"))
hogar <- read_dta(paste0(ruta_data, "1031-Modulo01/enaho01-2025-100.dta"))


# Filtro encuestas completa e incompletas ----

hogar <- hogar %>%
  filter(result <= 2)


# Necesidades básicas insatisfechas ----
hogar_nbi <- hogar %>%
  group_by(conglome, vivienda, hogar) %>%
  summarise(across(c(nbi1, nbi2, nbi3, nbi4, nbi5),
                   ~ mean(.x, na.rm = TRUE)),
            .groups = "drop")


# NBI a nivel de hogar y variables de pobreza ----

base <- hogar_nbi %>%
  inner_join(sumaria, by = c("conglome", "vivienda", "hogar")) %>%
  mutate(
    facpob = factor07 * mieperho,
    nbihog = nbi1 + nbi2 + nbi3 + nbi4 + nbi5,
    NBI1_POBRE = case_when(
      nbihog > 0 ~ 1,
      TRUE ~ 0
    ),
    NBI2_POBRE = case_when(
      nbihog > 1 ~ 1,
      TRUE ~ 0
    )
  )

base$NBI1_POBRE <- labelled(base$NBI1_POBRE,
                            labels = c("Ninguna NBI" = 0, "Al menos un NBI" = 1))
base$NBI2_POBRE <- labelled(base$NBI2_POBRE,
                            labels = c("Menos de dos NBI" = 0, "Al menos dos NBI" = 1))
var_label(base$NBI1_POBRE) <- "Con al menos una NBI"
var_label(base$NBI2_POBRE) <- "Con al menos dos NBI"


# Variables geográficas ----
base <- base %>%
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
    departamento = round(as.numeric(ubigeo)/10000)
  )

base$area   <- labelled(base$area, labels = c("Urbano" = 1, "Rural" = 2))
base$region_natural <- labelled(base$region_natural, labels = c("Costa" = 1, "Sierra" = 2, "Selva" = 3))
var_label(base$region_natural) <- "Region natural"

dpto_labels <- c(
  "Amazonas" = 1, "Ancash" = 2, "Apurimac" = 3, "Arequipa" = 4, "Ayacucho" = 5,
  "Cajamarca" = 6, "Callao" = 7, "Cusco" = 8, "Huancavelica" = 9, "Huanuco" = 10,
  "Ica" = 11, "Junin" = 12, "La_Libertad" = 13, "Lambayeque" = 14, "Lima" = 15,
  "Loreto" = 16, "Madre_de_Dios" = 17, "Moquegua" = 18, "Pasco" = 19, "Piura" = 20,
  "Puno" = 21, "San_Martin" = 22, "Tacna" = 23, "Tumbes" = 24, "Ucayali" = 25
)

base$departamento <- labelled(base$departamento, labels = dpto_labels)
var_label(base$departamento) <- "Departamento"


# Renombrar año a anio ----
names(base) <- gsub("^a.o$", "anio", names(base))

var_label(base$nbi1) <- "Poblacion en viviendas con caracteristicas fisicas inadecuadas"
var_label(base$nbi2) <- "Poblacion en viviendas con hacinamiento"
var_label(base$nbi3) <- "Poblacion en viviendas sin desague de ningun tipo"
var_label(base$nbi4) <- "Poblacion en hogares con ninos (6 a 12) que no asisten a la escuela"
var_label(base$nbi5) <- "Poblacion en hogares con alta dependencia economica"

## Convertir a factor ----
base_factor <- base %>%
  mutate(
    NBI1_POBRE = as_factor(NBI1_POBRE),
    NBI2_POBRE = as_factor(NBI2_POBRE),
    area = as_factor(area),
    region_natural = as_factor(region_natural),
    departamento = as_factor(departamento)
  )

## Diseño de la encuesta ----
disenio <- svydesign(
  ids = ~conglome,
  strata = ~estrato,
  weights = ~facpob,
  data = base_factor,
  nest = TRUE
)

## Resultados ----
tab_nbi <- svymean(
  ~nbi1 + nbi2 + nbi3 + nbi4 + nbi5,
  disenio,
  na.rm = TRUE
)

tab_nbi1_pobre <- svymean(
  ~NBI1_POBRE,
  disenio,
  na.rm = TRUE
)

tab_area <- svyby(
  ~NBI1_POBRE,
  ~area,
  disenio,
  svymean,
  na.rm = TRUE
)

tab_regnat <- svyby(
  ~NBI1_POBRE,
  ~region_natural,
  disenio,
  svymean,
  na.rm = TRUE
)


# Intervalos de confianza al 95% ----
ci_nbi <- confint(tab_nbi)

ci_nbi1_pobre <- confint(tab_nbi1_pobre)
