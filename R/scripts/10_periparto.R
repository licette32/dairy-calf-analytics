# MORTALIDAD PERIPARTO Y EXPOSICIÓN TÉRMICA

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(broom)

source("R/prep_modelo.R")

dir.create("reports/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("reports/figures", recursive = TRUE, showWarnings = FALSE)

x <- cargar_analisis()



# 1. NACIDOS MUERTOS
# Se excluyen los abortos.
partos <- x |>
  filter(tipo_parto %in% c("Normal", "Asistido", "Nacido muerto")) |>
  mutate(
    resultado_nacido_muerto = as.integer(nacido_muerto)
  )

cat("\nANÁLISIS DE NACIDOS MUERTOS\n")
cat("Partos incluidos:", nrow(partos), "\n")
cat("Nacidos muertos:", sum(partos$resultado_nacido_muerto), "\n")

print(
  partos |>
    count(tipo_parto, resultado_nacido_muerto)
)

# Descripción de la exposición según resultado
tabla_nacidos_muertos <- partos |>
  group_by(resultado_nacido_muerto) |>
  summarise(
    n = n(),
    thi_nac_media = mean(thi_nac, na.rm = TRUE),
    thi_pre3_media = mean(thi_media_pre3d, na.rm = TRUE),
    thi_pre7_media = mean(thi_media_pre7d, na.rm = TRUE),
    thi_pre21_media = mean(thi_media_pre21d, na.rm = TRUE),
    tmax_pre7_media = mean(tmax_media_pre7d, na.rm = TRUE),
    .groups = "drop"
  )

print(tabla_nacidos_muertos)

write_csv(
  tabla_nacidos_muertos,
  "reports/tables/periparto_nacidos_muertos_descriptivo.csv"
)


# 2. MODELOS LOGÍSTICOS PARA NACIDOS MUERTOS
# Se ajusta un modelo por exposición térmica.
# No se incluyen muchas variables simultáneamente porque hay solamente 37 nacidos muertos.

ajustar_logistica <- function(datos, variable, resultado, nombre) {
  
  datos_modelo <- datos |>
    select(all_of(c(resultado, variable))) |>
    drop_na()
  
  if (sum(datos_modelo[[resultado]] == 1) < 5 ||
      sum(datos_modelo[[resultado]] == 0) < 5) {
    return(tibble(
      modelo = nombre,
      variable = variable,
      n = nrow(datos_modelo),
      eventos = sum(datos_modelo[[resultado]] == 1),
      OR = NA_real_,
      IC_inf = NA_real_,
      IC_sup = NA_real_,
      p = NA_real_,
      observacion = "Cantidad insuficiente de casos para ajustar"
    ))
  }
  
  formula <- as.formula(
    paste(resultado, "~", variable)
  )
  
  modelo <- glm(
    formula,
    data = datos_modelo,
    family = binomial()
  )
  
  coeficientes <- summary(modelo)$coefficients
  
  tibble(
    modelo = nombre,
    variable = variable,
    n = nrow(datos_modelo),
    eventos = sum(datos_modelo[[resultado]] == 1),
    OR = exp(coeficientes[2, 1]),
    IC_inf = exp(coeficientes[2, 1] - 1.96 * coeficientes[2, 2]),
    IC_sup = exp(coeficientes[2, 1] + 1.96 * coeficientes[2, 2]),
    p = coeficientes[2, 4],
    observacion = "Modelo exploratorio no ajustado"
  )
}

variables_termicas <- c(
  "thi_nac",
  "thi_media_pre3d",
  "thi_media_pre7d",
  "thi_media_pre21d",
  "tmax_media_pre7d",
  "hum_media_pre7d"
)

modelos_nacidos_muertos <- bind_rows(
  lapply(
    variables_termicas,
    function(v) {
      ajustar_logistica(
        partos,
        variable = v,
        resultado = "resultado_nacido_muerto",
        nombre = "Nacidos muertos"
      )
    }
  )
)

print(modelos_nacidos_muertos)

write_csv(
  modelos_nacidos_muertos,
  "reports/tables/logistica_nacidos_muertos.csv"
)



# 3. MUERTES DURANTE EL DÍA 0 EN NACIDOS VIVOS

vivos <- x |>
  filter(nacido_vivo) |>
  mutate(
    resultado_muerte_dia0 = as.integer(muerte_dia0)
  )

cat("\nANÁLISIS DE MUERTE DURANTE EL DÍA 0\n")
cat("Nacidos vivos:", nrow(vivos), "\n")
cat("Muertes día 0:", sum(vivos$resultado_muerte_dia0), "\n")

print(
  vivos |>
    count(tipo_parto, resultado_muerte_dia0)
)

tabla_dia0 <- vivos |>
  group_by(resultado_muerte_dia0) |>
  summarise(
    n = n(),
    thi_nac_media = mean(thi_nac, na.rm = TRUE),
    thi_pre3_media = mean(thi_media_pre3d, na.rm = TRUE),
    thi_pre7_media = mean(thi_media_pre7d, na.rm = TRUE),
    thi_pre21_media = mean(thi_media_pre21d, na.rm = TRUE),
    tmax_pre7_media = mean(tmax_media_pre7d, na.rm = TRUE),
    .groups = "drop"
  )

print(tabla_dia0)

write_csv(
  tabla_dia0,
  "reports/tables/periparto_muerte_dia0_descriptivo.csv"
)


# 4. MUERTE DÍA 0: ANÁLISIS EXPLORATORIO

# Los 11 eventos registrados corresponden a partos asistidos.
# No se ajusta un modelo con tipo de parto e interacción: con estos datos se produciría separación o estimaciones inestables.
# Se explora cada variable térmica por separado.
# Los resultados se interpretan con cautela por el bajo númerode eventos.

modelos_dia0 <- bind_rows(
  lapply(
    variables_termicas,
    function(v) {
      ajustar_logistica(
        vivos,
        variable = v,
        resultado = "resultado_muerte_dia0",
        nombre = "Muerte día 0"
      )
    }
  )
)

print(modelos_dia0)

write_csv(
  modelos_dia0,
  "reports/tables/logistica_muerte_dia0.csv"
)



# 5. ANÁLISIS DESCRIPTIVO ENTRE PARTOS ASISTIDOS
asistidos <- vivos |>
  filter(tipo_parto == "Asistido")

cat("\nPARTOS ASISTIDOS\n")
cat("Total:", nrow(asistidos), "\n")
cat("Muertes día 0:", sum(asistidos$muerte_dia0), "\n")

tabla_asistidos <- asistidos |>
  group_by(muerte_dia0) |>
  summarise(
    n = n(),
    thi_nac_media = mean(thi_nac, na.rm = TRUE),
    thi_pre3_media = mean(thi_media_pre3d, na.rm = TRUE),
    thi_pre7_media = mean(thi_media_pre7d, na.rm = TRUE),
    tmax_pre7_media = mean(tmax_media_pre7d, na.rm = TRUE),
    .groups = "drop"
  )

print(tabla_asistidos)

write_csv(
  tabla_asistidos,
  "reports/tables/periparto_asistidos.csv"
)



# 6. GRÁFICOS DESCRIPTIVOS

g1 <- ggplot(
  partos,
  aes(
    x = factor(resultado_nacido_muerto),
    y = thi_media_pre7d
  )
) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.12, alpha = 0.25) +
  labs(
    title = "Exposición térmica prenatal y nacidos muertos",
    x = "Resultado del parto",
    y = "THI medio de los 7 días previos"
  ) +
  scale_x_discrete(
    labels = c("0" = "Nacido vivo", "1" = "Nacido muerto")
  ) +
  theme_minimal()

ggsave(
  "reports/figures/thi_prenatal_nacidos_muertos.png",
  g1,
  width = 7,
  height = 5,
  dpi = 220
)

g2 <- ggplot(
  vivos,
  aes(
    x = factor(resultado_muerte_dia0),
    y = thi_media_pre7d
  )
) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.12, alpha = 0.25) +
  labs(
    title = "Exposición térmica prenatal y mortalidad durante el día 0",
    x = "Resultado",
    y = "THI medio de los 7 días previos"
  ) +
  scale_x_discrete(
    labels = c("0" = "Sobrevivió al día 0", "1" = "Murió día 0")
  ) +
  theme_minimal()

ggsave(
  "reports/figures/thi_prenatal_muerte_dia0.png",
  g2,
  width = 7,
  height = 5,
  dpi = 220
)

cat("\nAnálisis periparto finalizado.\n")