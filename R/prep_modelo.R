# prep_modelo

library(dplyr); library(readr)

cargar_modelo <- function() {
  read_csv("data/clean/terneros_supervivencia.csv", show_col_types = FALSE) |>
    filter(!is.na(codigo_madre)) |>
    mutate(
      categoria_madre = factor(categoria_madre,
                               levels = c("vaca", "vaquillona_1er_parto", "vaquillona_2do_parto")),
      sexo     = factor(sexo),
      raza     = factor(raza, levels = c("Holando", "Jersey")),
      asistido = factor(if_else(tipo_parto == "Asistido", "Asistido", "Normal"),
                        levels = c("Normal", "Asistido")),
      anio     = factor(anio),
      peso_cat = cut(peso_nacer_kg, c(0, 38, 40, Inf),
                     labels = c("<=38 kg", "39-40 kg", ">40 kg")),
      peso5    = peso_nacer_kg / 5,                       # HR por +5 kg
      
      # clima: medias y proporciones (no totales) para no sesgar por la ventana
      prop_lluvia_7d  = dias_lluvia_7d  / n_dias_7d,
      prop_frio_7d    = dias_frio_7d    / n_dias_7d,
      prop_lluvia_21d = dias_lluvia_21d / n_dias_21d,
      prop_frio_21d   = dias_frio_21d   / n_dias_21d,
      tmin5_7d    = tmin_media_7d / 5,                    # HR por +5 °C
      tmin5_21d   = tmin_media_21d / 5,
      lluvia10_7d = 10 * prop_lluvia_7d,                  # HR por +10 pp de días con lluvia
      frio10_7d   = 10 * prop_frio_7d
    )
}