library(dplyr); library(readr)

terneros <- read_csv("data/clean/terneros_limpio.csv", show_col_types = FALSE)
clima    <- read_csv("data/clean/clima_terneros.csv", show_col_types = FALSE)

# Unión por id_registro (uno a uno). No se une por fecha: varios terneros
# comparten día de nacimiento y se duplicarían filas.
analisis <- left_join(terneros, clima, by = "id_registro")

stopifnot(nrow(analisis) == nrow(terneros),
          !anyDuplicated(analisis$id_registro),
          !anyDuplicated(analisis$codigo_ternero))

cat("Filas:", nrow(analisis), "| columnas:", ncol(analisis), "\n")
cat("Sin clima (ventana 7 d):", sum(is.na(analisis$n_dias_7d)), "\n")

# Población para supervivencia: nacidos vivos
surv <- analisis |> filter(nacido_vivo)
cat("Nacidos vivos:", nrow(surv), "| muertes:", sum(surv$muerte), "\n")

write_csv(analisis, "data/clean/terneros_analisis.csv")
write_csv(surv,     "data/clean/terneros_supervivencia.csv")