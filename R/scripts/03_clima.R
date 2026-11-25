library(dplyr); library(readr); library(lubridate)

# ---- Clima diario (Open-Meteo) ----
clima <- read_csv(
  "data/clima/open-meteo-deptoSF.csv",
  show_col_types = FALSE
) |>
  transmute(
    fecha  = as_date(time),
    tmax   = `temperature_2m_max (°C)`,
    tmin   = `temperature_2m_min (°C)`,
    tmed   = `temperature_2m_mean (°C)`,
    precip = `precipitation_sum (mm)`,
    hum    = `relative_humidity_2m_mean (%)`
  ) |>
  mutate(
    thi_media = tmed - (0.55 - 0.0055 * hum) * (tmed - 14.5),
    thi_tmax_aprox = tmax - (0.55 - 0.0055 * hum) * (tmax - 14.5)
  )

stopifnot(!anyDuplicated(clima$fecha), !any(is.na(clima$fecha)))

cat("Clima:", format(min(clima$fecha)), "a", format(max(clima$fecha)), "\n")
cat("Días con datos incompletos:", sum(!complete.cases(clima)), "\n")

dir.create("data/clean", showWarnings = FALSE, recursive = TRUE)
write_csv(clima, "data/clean/clima_diario.csv")

# Terneros: un registro por ternero (id_registro), no por fecha
terneros <- read_csv("data/clean/terneros_limpio.csv", show_col_types = FALSE) |>
  select(id_registro, fecha_nacimiento)

stopifnot(
  !anyDuplicated(terneros$id_registro),
  !any(is.na(terneros$fecha_nacimiento))
)

# FUNCIÓN PARA VENTANAS PRENATALES FIJAS
# inicio y fin son días relativos al nacimiento (-7 a -1 = siete días previos).
exposicion_previa <- function(terneros, clima, inicio, fin, sufijo) {

  terneros |>
    mutate(
      fecha_inicio   = fecha_nacimiento + inicio,
      fecha_fin      = fecha_nacimiento + fin,
      dias_esperados = as.integer(fecha_fin - fecha_inicio) + 1
    ) |>
    inner_join(
      clima,
      by = join_by(fecha_inicio <= fecha, fecha_fin >= fecha)
    ) |>
    group_by(id_registro, dias_esperados) |>
    summarise(
      n_dias = sum(complete.cases(tmax, tmed, hum, precip)),

      tmax_media = mean(tmax, na.rm = TRUE),
      tmax_max   = max(tmax,  na.rm = TRUE),
      tmin_media = mean(tmin, na.rm = TRUE),
      tmed_media = mean(tmed, na.rm = TRUE),
      hum_media  = mean(hum,  na.rm = TRUE),

      thi_media     = mean(thi_media,     na.rm = TRUE),
      thi_max_aprox = max(thi_tmax_aprox, na.rm = TRUE),

      precip_total = sum(precip, na.rm = TRUE),
      dias_lluvia  = sum(precip >= 1, na.rm = TRUE),

      # Días con THI máximo aproximado >= 68 (estrés térmico)
      dias_thi_mayor_68 = sum(thi_tmax_aprox >= 68, na.rm = TRUE),

      .groups = "drop"
    ) |>
    mutate(
      # No aceptar ventanas incompletas como si fueran comparables
      across(
        c(
          tmax_media, tmax_max, tmin_media, tmed_media,
          hum_media, thi_media, thi_max_aprox,
          precip_total, dias_lluvia, dias_thi_mayor_68
        ),
        ~ if_else(n_dias == dias_esperados, .x, NA_real_)
      )
    ) |>
    select(-dias_esperados) |>
    rename_with(
      ~ paste0(.x, "_", sufijo),
      -id_registro
    )
}

# VENTANAS PRENATALES
pre3  <- exposicion_previa(terneros, clima, inicio = -3,  fin = -1, sufijo = "pre3d")
pre7  <- exposicion_previa(terneros, clima, inicio = -7,  fin = -1, sufijo = "pre7d")
pre21 <- exposicion_previa(terneros, clima, inicio = -21, fin = -1, sufijo = "pre21d")

# Clima del día de nacimiento
dia0 <- terneros |>
  left_join(clima, by = c("fecha_nacimiento" = "fecha")) |>
  transmute(
    id_registro,
    tmax_nac    = tmax,
    tmin_nac    = tmin,
    tmed_nac    = tmed,
    hum_nac     = hum,
    precip_nac  = precip,
    thi_nac     = thi_media,
    thi_tmax_nac = thi_tmax_aprox
  )

# UNIÓN FINAL
clima_terneros <- terneros |>
  select(id_registro) |>
  left_join(dia0,  by = "id_registro") |>
  left_join(pre3,  by = "id_registro") |>
  left_join(pre7,  by = "id_registro") |>
  left_join(pre21, by = "id_registro")

stopifnot(
  nrow(clima_terneros) == nrow(terneros),
  !anyDuplicated(clima_terneros$id_registro)
)

cat("Terneros sin clima del día de nacimiento:", sum(is.na(clima_terneros$thi_nac)), "\n")
cat("Terneros sin ventana prenatal de 7 días completa:",
    sum(is.na(clima_terneros$thi_media_pre7d)), "\n")

print(summary(clima_terneros |>
  select(thi_nac, thi_media_pre3d, thi_media_pre7d, thi_media_pre21d)))

write_csv(clima_terneros, "data/clean/clima_terneros.csv")