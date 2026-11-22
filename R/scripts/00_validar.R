library(readxl); library(dplyr); library(lubridate)
source("R/validaciones.R")

archivo <- "data/raw/guachera.xlsx"
x <- suppressWarnings(read_excel(archivo)) |>
  filter(!is.na(fecha_nacimiento), year(fecha_nacimiento) <= 2025)

problemas <- validar_guachera(x)

if (nrow(problemas) == 0) {
  cat("Validación OK: sin inconsistencias\n")
} else {
  print(problemas, n = Inf)
  cat("\nResumen por regla:\n")
  print(count(problemas, regla))
  stop("Hay ", nrow(problemas), " inconsistencias: corregí el Excel y volvé a correr.",
       call. = FALSE)
}

