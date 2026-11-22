library(dplyr); library(readr); library(lubridate)

# ---- Clima diario (Open-Meteo) ----
clima <- read_csv("data/clima/open-meteo-30.97S61.85W101m.csv", show_col_types = FALSE)
stopifnot(ncol(clima) == 6)
names(clima) <- c("fecha", "tmax", "tmin", "tmed", "precip", "hum")
clima <- clima |> mutate(fecha = as_date(fecha))

stopifnot(!anyDuplicated(clima$fecha))
cat("Clima:", format(min(clima$fecha)), "a", format(max(clima$fecha)), "\n")
cat("Días con NA:", sum(!complete.cases(clima)), "\n")

dir.create("data/clean", showWarnings = FALSE, recursive = TRUE)
write_csv(clima, "data/clean/clima_diario.csv")

# ---- Terneros: un registro por ternero (id_registro), no por fecha ----
# Varios terneros nacen el mismo día: se comparte el clima del día, pero cada
# ternero tiene su propia ventana (se corta en fecha_egreso para no usar
# clima posterior a su muerte o salida).
terneros <- read_csv("data/clean/terneros_limpio.csv", show_col_types = FALSE) |>
  select(id_registro, fecha_nacimiento, fecha_egreso)

stopifnot(!anyDuplicated(terneros$id_registro))

exposicion <- function(terneros, clima, dias) {
  terneros |>
    mutate(fin = pmin(fecha_nacimiento + (dias - 1), fecha_egreso, na.rm = TRUE)) |>
    inner_join(clima, by = join_by(fecha_nacimiento <= fecha, fin >= fecha)) |>
    group_by(id_registro) |>
    summarise(
      n_dias     = n(),
      tmin_media = mean(tmin),
      tmin_min   = min(tmin),
      tmed       = mean(tmed),
      tmax_media = mean(tmax),
      precip_tot = sum(precip),
      dias_lluvia = sum(precip >= 1),
      dias_frio   = sum(tmin <= 5),
      hum_media  = mean(hum),
      .groups = "drop"
    ) |>
    rename_with(~ paste0(.x, "_", dias, "d"), -id_registro)
}

e7  <- exposicion(terneros, clima, 7)
e21 <- exposicion(terneros, clima, 21)

# Clima del día del nacimiento
dia0 <- terneros |>
  left_join(clima, by = c("fecha_nacimiento" = "fecha")) |>
  transmute(id_registro,
            tmin_nac = tmin, tmed_nac = tmed, precip_nac = precip, hum_nac = hum)

clima_terneros <- terneros |> select(id_registro) |>
  left_join(dia0, by = "id_registro") |>
  left_join(e7,   by = "id_registro") |>
  left_join(e21,  by = "id_registro")

# ---- Controles ----
stopifnot(nrow(clima_terneros) == nrow(terneros),
          !anyDuplicated(clima_terneros$id_registro))
cat("Terneros sin clima del día de nacimiento:", sum(is.na(clima_terneros$tmin_nac)), "\n")
cat("Terneros sin ventana de 7 días:", sum(is.na(clima_terneros$n_dias_7d)), "\n")
print(summary(clima_terneros |> select(tmin_media_7d, precip_tot_7d, tmin_media_21d)))

write_csv(clima_terneros, "data/clean/clima_terneros.csv")