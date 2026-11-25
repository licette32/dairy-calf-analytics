library(dplyr); library(readr); library(lubridate)

x <- read_csv(
  "data/clean/terneros_anonimizado.csv",
  show_col_types = FALSE
)

stopifnot("peso_nacido (kg)" %in% names(x))

x <- x |>
  rename(peso_nacer_kg = `peso_nacido (kg)`)

# Observaciones en texto libre
obs <- tolower(iconv(x$Observaciones, to = "ASCII//TRANSLIT"))
obs[is.na(obs)] <- ""

x <- x |>
  mutate(
    muerte = as.logical(muerte),

    # Corrección de inconsistencias de estado (red de seguridad si no se validó)
    estado_egreso = if_else(
      muerte & estado_egreso != "Muerto",
      "Muerto",
      estado_egreso
    ),

    # Peso 0 significa que no se pesó
    peso_nacer_kg = if_else(
      peso_nacer_kg == 0,
      NA_real_,
      as.numeric(peso_nacer_kg)
    ),

    # Resultados del nacimiento
    nacido_vivo   = tipo_parto %in% c("Normal", "Asistido"),
    nacido_muerto = tipo_parto == "Nacido muerto",
    aborto        = tipo_parto == "Aborto",

    # Tiempo desde nacimiento hasta egreso
    dias_guachera = as.integer(fecha_egreso - fecha_nacimiento),

    fecha_dudosa = fecha_ingreso != fecha_nacimiento,

    # Mortalidad
    muerte_n = as.integer(muerte),

    # Nacido vivo que falleció durante el mismo día calendario
    muerte_dia0 = muerte & nacido_vivo & dias_guachera == 0,

    # Muerte posterior al día del nacimiento
    muerte_posterior = muerte & nacido_vivo & dias_guachera > 0,

    # Variable para análisis de supervivencia
    tiempo_surv = pmax(dias_guachera, 0.5),

    anio = year(fecha_nacimiento),
    mes  = month(fecha_nacimiento),

    estacion = case_when(
      mes %in% c(12, 1, 2) ~ "verano",
      mes %in% 3:5         ~ "otoño",
      mes %in% 6:8         ~ "invierno",
      TRUE                 ~ "primavera"
    ),

    # Indicadores extraídos de observaciones
    obs_neumonia      = grepl("neumon", obs),
    obs_diarrea       = grepl("diarrea", obs),
    obs_timpanismo    = grepl("timpanismo", obs),
    obs_mellizos      = grepl("mellizo", obs),
    obs_asistido_cola = grepl("\\bcola\\b", obs),
    obs_asistido_vet  = grepl("veterinari", obs)
  )

# ---- Controles ----
cat("Filas:", nrow(x), "\n")
cat("Nacidos vivos:", sum(x$nacido_vivo, na.rm = TRUE), "\n")
cat("Nacidos muertos:", sum(x$nacido_muerto, na.rm = TRUE), "\n")
cat("Abortos:", sum(x$aborto, na.rm = TRUE), "\n")
cat("Muertes día 0:", sum(x$muerte_dia0, na.rm = TRUE), "\n")
cat("Muertes posteriores:", sum(x$muerte_posterior, na.rm = TRUE), "\n")
cat("tipo_parto NA:", sum(is.na(x$tipo_parto)), "\n")
cat("tiempo_surv NA:", sum(is.na(x$tiempo_surv)), "\n")
print(count(x, tipo_parto, muerte, nacido_vivo))
print(count(x, tipo_parto, muerte_dia0))

stopifnot(
  !anyDuplicated(x$id_registro),
  !anyDuplicated(x$codigo_ternero),
  all(x$dias_guachera >= 0, na.rm = TRUE),
  !any(is.na(x$tipo_parto))
)

write_csv(x, "data/clean/terneros_limpio.csv")