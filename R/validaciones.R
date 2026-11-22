library(dplyr)

# Devuelve una tabla con las filas (número de fila en Excel) que rompen cada regla
validar_guachera <- function(x) {
  peso <- x[["peso_nacido (kg)"]]
  
  x <- x |>
    mutate(
      fila_excel = ID + 1,
      ternero    = as.character(numero_ternero),
      madre      = as.character(numero_vaca),
      muerte     = as.logical(muerte),
      dias       = as.integer(as.Date(fecha_egreso) - as.Date(fecha_ingreso)),
      mortinato  = tipo_parto %in% c("Nacido muerto", "Aborto")
    ) |>
    group_by(madre, fecha_nacimiento) |>
    mutate(parto_multiple = !is.na(madre) & n() >= 2) |>
    ungroup()
  
  falla <- list(
    aborto_vs_caravana    = (x$tipo_parto == "Aborto" & x$ternero != "S/N") |
      (x$tipo_parto != "Aborto" & x$ternero == "S/N"),
    ingreso_vs_nacimiento = x$fecha_ingreso != x$fecha_nacimiento,
    egreso_antes_ingreso  = x$fecha_egreso < x$fecha_ingreso,
    muerte_vs_estado      = x$muerte != (x$estado_egreso == "Muerto"),
    destino_vs_sexo       = (x$destino_productivo == "Cría Hembra" & x$sexo != "Hembra") |
      (x$destino_productivo %in% c("Engorde", "Toro") & x$sexo != "Macho"),
    mortinato_con_dias    = x$mortinato & x$dias != 0,
    mortinato_sin_muerte  = x$mortinato & !x$muerte,
    peso_fuera_de_rango   = peso > 0 & (peso < 15 | peso > 80),
    caravana_duplicada    = x$ternero != "S/N" &
      (duplicated(x$ternero) | duplicated(x$ternero, fromLast = TRUE)),
    mellizos_sin_obs      = x$parto_multiple & !grepl("melliz", tolower(x$Observaciones))
  )
  
  bind_rows(lapply(names(falla), function(nombre) {
    tibble(regla = nombre, fila_excel = x$fila_excel[which(falla[[nombre]])])
  }))
}