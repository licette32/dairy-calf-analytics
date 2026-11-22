library(testthat); library(dplyr)
source("../../R/validaciones.R")

base <- function(...) {
  tibble(ID = 1, numero_vaca = 100, numero_ternero = "10",
         fecha_ingreso = as.Date("2024-03-01"), fecha_egreso = as.Date("2024-05-30"),
         estado_egreso = "Activo", fecha_nacimiento = as.Date("2024-03-01"),
         sexo = "Hembra", `peso_nacido (kg)` = 38, raza = "Holando",
         tipo_parto = "Normal", muerte = FALSE, destino_productivo = "Cría Hembra",
         Observaciones = NA_character_) |>
    mutate(...)
}

test_that("un registro correcto no genera problemas", {
  expect_equal(nrow(validar_guachera(base())), 0)
})

test_that("detecta aborto con caravana", {
  r <- validar_guachera(base(tipo_parto = "Aborto", muerte = TRUE,
                             estado_egreso = "Muerto", destino_productivo = "Muerto",
                             fecha_egreso = as.Date("2024-03-01")))
  expect_true("aborto_vs_caravana" %in% r$regla)
})

test_that("detecta muerte y estado distintos", {
  r <- validar_guachera(base(muerte = TRUE))
  expect_true("muerte_vs_estado" %in% r$regla)
})

test_that("detecta ingreso distinto de nacimiento", {
  r <- validar_guachera(base(fecha_ingreso = as.Date("2024-03-02")))
  expect_true("ingreso_vs_nacimiento" %in% r$regla)
})

test_that("detecta peso fuera de rango pero acepta 65 kg", {
  expect_true("peso_fuera_de_rango" %in% validar_guachera(base(`peso_nacido (kg)` = 120))$regla)
  expect_equal(nrow(validar_guachera(base(`peso_nacido (kg)` = 65))), 0)
})