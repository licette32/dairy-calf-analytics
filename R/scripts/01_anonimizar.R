library(readxl); library(dplyr); library(readr); library(lubridate); library(tibble)

# ---- Configuración ----
archivo <- "data/raw/guachera.xlsx"
salida  <- "data/clean/terneros_anonimizado.csv"
corte_primer_2023 <- 24500   # PROVISORIO: verificar (caravana mínima de vaquillonas 2023)

# ---- Lectura: solo campañas 2023-2025, fechas como Date (no fecha-hora) ----
x <- read_excel(archivo) |>
  filter(!is.na(fecha_nacimiento), year(fecha_nacimiento) <= 2025) |>
  mutate(across(c(fecha_ingreso, fecha_egreso, fecha_nacimiento), as_date),
         anio = year(fecha_nacimiento))

# ---- Umbral de primer parto por año ----
# 2023: manual. Luego: mayor que la caravana máxima del año anterior.
umbral <- x |> group_by(anio) |>
  summarise(m = max(numero_vaca, na.rm = TRUE)) |>
  mutate(anio = anio + 1, umbral = m + 1) |> select(anio, umbral) |>
  bind_rows(tibble(anio = 2023, umbral = corte_primer_2023))
print(umbral)

# ---- Categoría de la madre ----
# Criterio:
# - Primer parto identificado: vaquillona_1er_parto.
# - Desde el segundo parto en adelante: vaca.
# - La identificación del primer parto depende del umbral de caravana.
#   Verificar especialmente el umbral provisional de 2023.

madre_anio <- x |>
  filter(!is.na(numero_vaca)) |>
  distinct(numero_vaca, anio) |>
  left_join(umbral, by = "anio") |>
  arrange(numero_vaca, anio) |>
  group_by(numero_vaca) |>
  mutate(
    rango = row_number(),
    parto = if (first(numero_vaca >= umbral)) rango else NA_integer_,
    categoria_madre = case_when(
      parto == 1 ~ "vaquillona_1er_parto",
      TRUE       ~ "vaca"
    )
  ) |>
  ungroup() |>
  select(numero_vaca, anio, categoria_madre)

x <- left_join(x, madre_anio, by = c("numero_vaca", "anio"))

print(count(x, anio, categoria_madre))

# ---- Códigos al azar con tabla privada de equivalencias ----
asignar_codigos <- function(claves, archivo, prefijo) {
  claves <- sort(unique(as.character(na.omit(claves))))
  mapeo <- if (file.exists(archivo)) {
    read_csv(archivo, col_types = cols(.default = col_character()))
  } else {
    tibble(clave = character(), codigo = character())
  }
  nuevas <- setdiff(claves, mapeo$clave)
  if (length(nuevas) > 0) {
    libres <- setdiff(1:9999, as.integer(sub(prefijo, "", mapeo$codigo)))
    mapeo <- bind_rows(mapeo, tibble(
      clave  = nuevas,
      codigo = sprintf(paste0(prefijo, "%04d"), sample(libres, length(nuevas)))))
    write_csv(mapeo, archivo)
  }
  mapeo
}

# Terneros sin caravana ("S/N") reciben un código propio basado en su ID interno
x <- x |>
  mutate(clave_ternero = if_else(coalesce(numero_ternero == "S/N", FALSE),
                                 paste0("SN-", ID), as.character(numero_ternero)))

map_madres   <- asignar_codigos(x$numero_vaca,   "data/raw/mapeo_madres_PRIVADO.csv",   "M")
map_terneros <- asignar_codigos(x$clave_ternero, "data/raw/mapeo_terneros_PRIVADO.csv", "T")

# ---- Dataset anonimizado ----
anon <- x |>
  mutate(codigo_madre   = map_madres$codigo[match(as.character(numero_vaca), map_madres$clave)],
         codigo_ternero = map_terneros$codigo[match(clave_ternero, map_terneros$clave)]) |>
  rename(id_registro = ID) |>
  select(id_registro, codigo_ternero, codigo_madre, categoria_madre, everything(),
         -numero_vaca, -numero_ternero, -clave_ternero, -anio)

stopifnot(!anyDuplicated(anon$codigo_ternero))
dir.create("data/clean", showWarnings = FALSE, recursive = TRUE)
write_csv(anon, salida)