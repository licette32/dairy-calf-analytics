library(dplyr); library(readr); library(lubridate)

x <- read_csv("data/clean/terneros_anonimizado.csv", show_col_types = FALSE) |>
  rename(peso_nacer_kg = `peso_nacido (kg)`)

# Observaciones en texto libre -> indicadores (sin tildes, en minúscula)
obs <- tolower(iconv(x$Observaciones, to = "ASCII//TRANSLIT"))
obs[is.na(obs)] <- ""

x <- x |>
  mutate(
    muerte        = as.logical(muerte),
    # muerte registrada pero estado "Activo" (1 caso): se toma como muerte
    estado_egreso = if_else(muerte & estado_egreso != "Muerto", "Muerto", estado_egreso),
    # peso 0 = no se pesó
    peso_nacer_kg = if_else(peso_nacer_kg == 0, NA_real_, as.numeric(peso_nacer_kg)),
    nacido_vivo   = tipo_parto %in% c("Normal", "Asistido"),
    # el tiempo se mide desde el nacimiento
    dias_guachera = as.integer(fecha_egreso - fecha_nacimiento),
    fecha_dudosa  = fecha_ingreso != fecha_nacimiento,
    muerte_n      = as.integer(muerte),
    muerte_dia0   = muerte & nacido_vivo & dias_guachera == 0,
    tiempo_surv   = pmax(dias_guachera, 0.5),
    anio = year(fecha_nacimiento),
    mes  = month(fecha_nacimiento),
    estacion = case_when(mes %in% c(12, 1, 2) ~ "verano",
                         mes %in% 3:5         ~ "otoño",
                         mes %in% 6:8         ~ "invierno",
                         TRUE                 ~ "primavera"),
    obs_neumonia      = grepl("neumon", obs),
    obs_diarrea       = grepl("diarrea", obs),
    obs_timpanismo    = grepl("timpanismo", obs),
    obs_mellizos      = grepl("mellizo", obs),
    obs_asistido_cola = grepl("cola", obs),
    obs_asistido_vet  = grepl("veterinari", obs)
  )

# ---- Controles ----
stopifnot(!anyDuplicated(x$id_registro),
          !anyDuplicated(x$codigo_ternero),
          all(x$dias_guachera >= 0))
cat("Filas:", nrow(x), "\n")
cat("Pesos pasados a NA:", sum(is.na(x$peso_nacer_kg)), "\n")
cat("Fechas dudosas (ingreso != nacimiento):", sum(x$fecha_dudosa), "\n")
cat("Nacidos vivos:", sum(x$nacido_vivo),
    "| muertes entre vivos:", sum(x$muerte & x$nacido_vivo), "\n")
cat("Nacidos vivos con 0 días:", sum(x$nacido_vivo & x$dias_guachera == 0), "\n")
print(count(x, nacido_vivo, tipo_parto, muerte))

write_csv(x, "data/clean/terneros_limpio.csv")