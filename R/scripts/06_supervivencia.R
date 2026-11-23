# Supervivencia

library(dplyr)
library(readr)
library(tibble)
library(ggplot2)
library(survival)
library(survminer)

for (d in c("reports/figures", "reports/tables", "data/clean/powerbi")) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

source("R/prep_modelo.R")
x <- cargar_modelo()
cat("Terneros:", nrow(x), "| muertes:", sum(x$muerte_n), "\n")

# Paletas temáticas por análisis
paletas <- list(
  mortalidad = "#B84A39",
  cat_madre  = c("#C2822A", "#3B6E4C", "#2B5B84"),
  parto      = c("#2B5B84", "#C2593F"),
  anio       = c("#355C7D", "#4E7969", "#B84A39"),
  peso       = c("#B84A39", "#C2822A", "#3B6E4C"),
  hr_alto    = "#B84A39",
  hr_bajo    = "#2B5B84",
  hr_neutro  = "#6C757D"
)

etiquetas_termino <- c(
  "anio2023"                            = "Año 2023",
  "anio2024"                            = "Año 2024",
  "anio2025"                            = "Año 2025",
  "categoria_madrevaca"                 = "Madre: Vaca",
  "categoria_madrevaquillona_1er_parto" = "Madre: Vaquillona 1.er parto",
  "categoria_madrevaquillona_2do_parto" = "Madre: Vaquillona 2.do parto",
  "sexoMacho"                           = "Sexo: Macho",
  "sexoHembra"                          = "Sexo: Hembra",
  "asistidoAsistido"                    = "Parto asistido",
  "asistidoNormal"                      = "Parto normal",
  "peso5"                               = "Peso al nacer (cada +5 kg)",
  "tmin5_7d"                            = "T. mín. primeros 7 días (cada +5 °C)",
  "lluvia10_7d"                         = "Lluvia primeros 7 días (cada +10 mm)",
  "frio10_7d"                           = "Frío primeros 7 días",
  "tmin5_21d"                           = "T. mín. primeros 21 días"
)

limpiar_termino <- function(v) {
  out <- etiquetas_termino[v]
  ifelse(is.na(out), v, unname(out))
}

# 1. Kaplan-Meier global

km <- survfit(Surv(tiempo_surv, muerte_n) ~ 1, data = x)
print(summary(km, times = c(7, 14, 30, 60, 90)))   # 1 - survival = mortalidad acumulada

guardar <- function(g, archivo) {
  png(archivo, 2200, 1800, res = 220)
  print(g)
  dev.off()
}

graficar_km <- function(fit, data, titulo, subtitulo, etiquetas_leyenda,
                        colores, archivo, ylim_inf = 0.87,
                        pval_coord = NULL) {
  if (is.null(pval_coord)) {
    pval_coord <- c(4, ylim_inf + 0.008)
  }
  
  g <- ggsurvplot(
    fit,
    data = data,
    palette = colores,
    pval = TRUE,
    pval.size = 3.8,
    pval.coord = pval_coord,
    conf.int = FALSE,
    risk.table = TRUE,
    risk.table.title = "En riesgo",
    risk.table.height = 0.24,
    risk.table.fontsize = 3.4,
    xlim = c(0, 100),
    break.time.by = 20,
    ylim = c(ylim_inf, 1),
    xlab = "Días desde el nacimiento",
    ylab = "Probabilidad de supervivencia",
    title = titulo,
    subtitle = subtitulo,
    legend.title = "",
    legend.labs = etiquetas_leyenda,
    ggtheme = theme_bw(base_size = 11) +
      theme(
        plot.title = element_text(face = "bold", size = 13, color = "#1F2933"),
        plot.subtitle = element_text(size = 10, color = "#52606D"),
        legend.position = "top",
        legend.text = element_text(size = 9.5),
        panel.grid.minor = element_blank()
      ),
    tables.theme = theme_bw(base_size = 10) +
      theme(
        panel.grid = element_blank(),
        axis.title.y = element_blank()
      )
  )
  suppressMessages(
    g$table <- g$table + scale_y_discrete(labels = rev(etiquetas_leyenda))
  )
  guardar(g, archivo)
}

f_cat  <- survfit(Surv(tiempo_surv, muerte_n) ~ categoria_madre, data = x)
f_asis <- survfit(Surv(tiempo_surv, muerte_n) ~ asistido,        data = x)
f_anio <- survfit(Surv(tiempo_surv, muerte_n) ~ anio,            data = x)
f_peso <- survfit(Surv(tiempo_surv, muerte_n) ~ peso_cat,        data = x)

graficar_km(
  f_cat, x,
  titulo = "Supervivencia según categoría de madre",
  subtitulo = "Curvas de Kaplan-Meier y prueba log-rank",
  etiquetas_leyenda = c("Vaca", "Vaquillona 1.er parto", "Vaquillona 2.do parto"),
  colores = paletas$cat_madre,
  archivo = "reports/figures/km_categoria_madre.png"
)

graficar_km(
  f_asis, x,
  titulo = "Supervivencia según tipo de parto",
  subtitulo = "Comparación entre parto normal y asistido",
  etiquetas_leyenda = c("Normal", "Asistido"),
  colores = paletas$parto,
  archivo = "reports/figures/km_tipo_parto.png"
)

graficar_km(
  f_anio, x,
  titulo = "Supervivencia por año de nacimiento",
  subtitulo = "Evolución interanual en guachera",
  etiquetas_leyenda = c("2023", "2024", "2025"),
  colores = paletas$anio,
  archivo = "reports/figures/km_anio.png"
)

graficar_km(
  f_peso, x,
  titulo = "Supervivencia según categoría de peso al nacer",
  subtitulo = "Distribución por rangos de peso inicial",
  etiquetas_leyenda = c("\u2264 38 kg", "39–40 kg", "\u2265 40 kg"),
  colores = paletas$peso,
  archivo = "reports/figures/km_peso.png"
)

# 2. ¿Cuándo se concentra el riesgo? Tasa por tramo de edad

tramos    <- c(0, 7, 14, 30, 60, Inf)
etiquetas <- c("0-7 d", "8-14 d", "15-30 d", "31-60 d", ">60 d")
riesgo <- bind_rows(lapply(seq_along(etiquetas), function(i) {
  a <- tramos[i]; b <- tramos[i + 1]
  tibble(
    tramo       = etiquetas[i],
    dias_riesgo = sum(pmax(0, pmin(x$tiempo_surv, b) - a)),
    muertes     = sum(x$muerte_n == 1 & x$tiempo_surv > a & x$tiempo_surv <= b)
  )
})) |>
  rowwise() |>
  mutate(
    tasa_x1000 = 1000 * muertes / dias_riesgo,
    ic_inf     = 1000 * poisson.test(muertes, dias_riesgo)$conf.int[1],
    ic_sup     = 1000 * poisson.test(muertes, dias_riesgo)$conf.int[2]
  ) |>
  ungroup() |>
  mutate(tramo = factor(tramo, levels = etiquetas))

print(riesgo)
write_csv(riesgo, "reports/tables/riesgo_por_tramo.csv")
write_csv(riesgo, "data/clean/powerbi/riesgo_por_tramo.csv")

g_riesgo <- ggplot(riesgo, aes(x = tramo, y = tasa_x1000)) +
  geom_col(fill = paletas$mortalidad, alpha = 0.85, width = 0.62) +
  geom_errorbar(aes(ymin = ic_inf, ymax = ic_sup),
                width = 0.18, linewidth = 0.7, color = "#1F2933") +
  labs(
    title = "Tasa de mortalidad por tramo de edad",
    subtitle = "Muertes por 1.000 terneros-día con IC 95% (distribución de Poisson)",
    x = "Edad",
    y = "Muertes por 1.000 terneros-día"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13, color = "#1F2933"),
    plot.subtitle = element_text(size = 10, color = "#52606D"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )
ggsave("reports/figures/riesgo_por_tramo.png", g_riesgo, width = 7.5, height = 4.8, dpi = 220)

# 3. Cox

hr_tabla <- function(m, nombre) {
  s <- summary(m)
  tibble(
    modelo  = nombre,
    termino = rownames(s$conf.int),
    HR      = s$conf.int[, 1],
    IC_inf  = s$conf.int[, 3],
    IC_sup  = s$conf.int[, 4],
    p       = s$coefficients[, ncol(s$coefficients)]
  )
}

forest <- function(t, titulo, subtitulo, archivo) {
  t_plot <- t |>
    mutate(
      etiqueta = limpiar_termino(termino),
      direccion = case_when(
        IC_inf > 1 ~ "Mayor riesgo (IC > 1)",
        IC_sup < 1 ~ "Menor riesgo (IC < 1)",
        TRUE       ~ "No concluyente (IC incluye 1)"
      )
    )
  
  g_for <- ggplot(t_plot, aes(x = HR, y = reorder(etiqueta, HR), color = direccion)) +
    geom_vline(xintercept = 1, linetype = 2, color = "#52606D", linewidth = 0.7) +
    geom_errorbar(aes(xmin = IC_inf, xmax = IC_sup),
                  orientation = "y", width = 0.22, linewidth = 0.8) +
    geom_point(size = 2.8) +
    scale_x_log10() +
    scale_color_manual(
      values = c(
        "Mayor riesgo (IC > 1)"         = paletas$hr_alto,
        "Menor riesgo (IC < 1)"         = paletas$hr_bajo,
        "No concluyente (IC incluye 1)" = paletas$hr_neutro
      ),
      name = NULL
    ) +
    labs(
      title = titulo,
      subtitle = subtitulo,
      x = "Hazard ratio (IC 95%, escala log)",
      y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 13, color = "#1F2933"),
      plot.subtitle = element_text(size = 10, color = "#52606D"),
      legend.position = "top",
      panel.grid.minor = element_blank()
    )
  
  ggsave(archivo, g_for, width = 8.5, height = 5.2, dpi = 220)
}

cl <- "+ cluster(codigo_madre)"   # errores robustos agrupados por madre

# a. Univariable
uni_vars <- c("categoria_madre", "sexo", "asistido", "peso5", "anio",
              "tmin5_7d", "lluvia10_7d", "frio10_7d", "tmin5_21d")
univ <- bind_rows(lapply(uni_vars, function(nm)
  hr_tabla(coxph(as.formula(paste("Surv(tiempo_surv, muerte_n) ~", nm, cl)), data = x), nm)))
print(univ, n = Inf)

# b. Modelo A: animal, parto y año
fA <- Surv(tiempo_surv, muerte_n) ~ categoria_madre + asistido + peso5 + sexo + anio
cox_A <- coxph(update(fA, paste(". ~ . ", cl)), data = x)
col_muerte <- if ("muerte" %in% names(x)) x$muerte else x$muerte_n
if ("raza" %in% names(x)) {
  print(fisher.test(table(x$raza, col_muerte)))
}
print(summary(cox_A))

# c. Modelo B: A + clima de los primeros 7 días
fB <- update(fA, . ~ . + tmin5_7d + lluvia10_7d)
cox_B <- coxph(update(fB, paste(". ~ . ", cl)), data = x)
print(summary(cox_B))

# d. Supuesto de riesgos proporcionales (sobre los modelos sin cluster)
print(cox.zph(coxph(fA, data = x)))
print(cox.zph(coxph(fB, data = x))) # Si alguna variable da p<0.05: probar strata(año) en lugar de año.

# e. Sensibilidad: sin muertes del día 0 (mortinatos tardíos/parto)
cox_B0 <- coxph(update(fB, paste(". ~ . ", cl)), data = filter(x, !muerte_dia0))

tabla_hr <- bind_rows(
  hr_tabla(cox_A,  "A: animal+parto+año"),
  hr_tabla(cox_B,  "B: A + clima 7 d"),
  hr_tabla(cox_B0, "B sin muertes día 0")
)

print(tabla_hr, n = Inf)
write_csv(tabla_hr, "reports/tables/cox_modelos.csv")
write_csv(tabla_hr, "data/clean/powerbi/cox_modelos.csv")

forest(
  filter(tabla_hr, modelo == "A: animal+parto+año"),
  titulo = "Modelo A — Factores de animal, parto y año",
  subtitulo = "Hazard ratios con errores robustos agrupados por madre",
  archivo = "reports/figures/forest_A.png"
)
forest(
  filter(tabla_hr, modelo == "B: A + clima 7 d"),
  titulo = "Modelo B — Modelo A + clima de los primeros 7 días",
  subtitulo = "Hazard ratios con errores robustos agrupados por madre",
  archivo = "reports/figures/forest_B.png"
)