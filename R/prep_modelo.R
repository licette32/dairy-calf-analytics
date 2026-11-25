# Preparación de datos para análisis de supervivencia y modelos
library(dplyr)
library(readr)

cargar_analisis <- function() {
  read_csv("data/clean/terneros_analisis.csv", show_col_types = FALSE) |>
    mutate(
      categoria_madre = factor(
        categoria_madre,
        levels = c("vaca", "vaquillona_1er_parto")
      ),
      sexo     = factor(sexo),
      raza     = factor(raza),
      asistido = factor(
        if_else(tipo_parto == "Asistido", "Asistido", "Normal"),
        levels = c("Normal", "Asistido")
      ),
      anio = factor(anio),
      mellizos = factor(obs_mellizos, levels = c(FALSE, TRUE)),
      peso_cat = cut(
        peso_nacer_kg,
        breaks = c(-Inf, 38, 40, Inf),
        labels = c("<=38 kg", "38-40 kg", ">40 kg"),
        right = TRUE
      ),
      peso5 = peso_nacer_kg / 5
    )
}

cargar_modelo <- function() {
  d <- cargar_analisis() |>
    filter(nacido_vivo, !is.na(codigo_madre)) |>
    mutate(
      prop_lluvia_pre7d = dias_lluvia_pre7d / n_dias_pre7d,
      prop_thi68_pre7d  = dias_thi_mayor_68_pre7d / n_dias_pre7d
    )

  cat("Filas antes de filtro NA clima:", nrow(d), "\n")
  d <- d |>
    filter(
      !is.na(prop_lluvia_pre7d),
      !is.na(prop_thi68_pre7d),
      !is.na(tiempo_surv)
    )
  cat("Filas tras filtro NA clima:", nrow(d), "\n")
  d
}