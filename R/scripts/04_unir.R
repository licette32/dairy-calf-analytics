library(dplyr); library(readr)

terneros <- read_csv("data/clean/terneros_limpio.csv", show_col_types = FALSE)
clima    <- read_csv("data/clean/clima_terneros.csv", show_col_types = FALSE)

analisis <- left_join(terneros, clima, by = "id_registro")

stopifnot(
  nrow(analisis) == nrow(terneros),
  !anyDuplicated(analisis$id_registro),
  !anyDuplicated(analisis$codigo_ternero)
)

cat("Registros totales:", nrow(analisis), "\n")
cat("Nacidos vivos:", sum(analisis$nacido_vivo), "\n")
cat("Nacidos muertos:", sum(analisis$nacido_muerto), "\n")
cat("Abortos:", sum(analisis$aborto), "\n")
cat("Muertes día 0:", sum(analisis$muerte_dia0), "\n")
cat("Muertes posteriores:", sum(analisis$muerte_posterior), "\n")
cat("Sin clima pre7d:", sum(is.na(analisis$thi_media_pre7d)), "\n")

# Dataset completo: nacidos vivos + nacidos muertos + abortos
write_csv(analisis, "data/clean/terneros_analisis.csv")

# Dataset de nacidos vivos: para supervivencia
supervivencia <- analisis |> filter(nacido_vivo)
write_csv(supervivencia, "data/clean/terneros_supervivencia.csv")