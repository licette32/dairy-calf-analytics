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
cat("Sin peso (peso_cat NA):", sum(is.na(x$peso_cat)), "\n")
cat("Sin fecha_egreso:", sum(is.na(x$fecha_egreso)), "\n")

# mellizos como factor para que aparezca en la tabla de HR
x <- x |> mutate(mellizos = factor(mellizos, levels = c(FALSE, TRUE)))

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
  "sexoMacho"                           = "Sexo: Macho",
  "sexoHembra"                          = "Sexo: Hembra",
  "asistidoAsistido"                    = "Parto asistido",
  "mellizosTRUE"                        = "Mellizos",
  "peso5"                               = "Peso al nacer (cada +5 kg)",
  "peso5_t30"                           = "Peso al nacer (cada +5 kg, a 30 d)",
  "tt(peso5)"                           = "Peso × log(tiempo)",
  "thi_media_pre7d"                     = "THI medio (7 días previos)",
  "thi_media_pre21d"                    = "THI medio (21 días previos)",
  "thi_nac"                             = "THI del día del nacimiento"
)

limpiar_termino <- function(v) {
  out <- etiquetas_termino[v]
  ifelse(is.na(out), v, unname(out))
}

# 1. Kaplan-Meier global
km <- survfit(Surv(tiempo_surv, muerte_n) ~ 1, data = x)
print(summary(km, times = c(7, 14, 30, 60, 90)))

guardar <- function(g, archivo) {
  png(archivo, 2200, 1800, res = 220)
  print(g)
  dev.off()
}

graficar_km <- function(fit, data, titulo, subtitulo, etiquetas_leyenda,
                        colores, archivo, ylim_inf = 0.87,
                        pval_coord = NULL) {
  if (is.null(pval_coord)) pval_coord <- c(4, ylim_inf + 0.08)
  g <- ggsurvplot(
    fit, data = data, palette = colores,
    pval = TRUE, pval.size = 3.8, pval.coord = pval_coord,
    conf.int = FALSE,
    risk.table = TRUE, risk.table.title = "En riesgo",
    risk.table.height = 0.24, risk.table.fontsize = 3.4,
    xlim = c(0, 100), break.time.by = 20, ylim = c(ylim_inf, 1),
    xlab = "Días desde el nacimiento",
    ylab = "Probabilidad de supervivencia",
    title = titulo, subtitle = subtitulo,
    legend.title = "", legend.labs = etiquetas_leyenda,
    ggtheme = theme_bw(base_size = 11) +
      theme(
        plot.title = element_text(face = "bold", size = 13, color = "#1F2933"),
        plot.subtitle = element_text(size = 10, color = "#52606D"),
        legend.position = "top",
        legend.text = element_text(size = 9.5),
        panel.grid.minor = element_blank()
      ),
    tables.theme = theme_bw(base_size = 10) +
      theme(panel.grid = element_blank(), axis.title.y = element_blank())
  )
  suppressMessages(g$table <- g$table + scale_y_discrete(labels = rev(etiquetas_leyenda)))
  guardar(g, archivo)
}

f_cat  <- survfit(Surv(tiempo_surv, muerte_n) ~ categoria_madre, data = x)
f_asis <- survfit(Surv(tiempo_surv, muerte_n) ~ asistido,        data = x)
f_anio <- survfit(Surv(tiempo_surv, muerte_n) ~ anio,            data = x)
f_peso <- survfit(Surv(tiempo_surv, muerte_n) ~ peso_cat,        data = x)

graficar_km(f_cat, x, "Supervivencia según categoría de madre",
            "Curvas de Kaplan-Meier y prueba log-rank",
            c("Vaca", "Vaquillona 1.er parto"), paletas$cat_madre,
            "reports/figures/km_categoria_madre.png")

graficar_km(f_asis, x, "Supervivencia según tipo de parto",
            "Comparación entre parto normal y asistido",
            c("Normal", "Asistido"), paletas$parto,
            "reports/figures/km_tipo_parto.png")

graficar_km(f_anio, x, "Supervivencia por año de nacimiento",
            "Evolución interanual en guachera",
            c("2023", "2024", "2025"), paletas$anio,
            "reports/figures/km_anio.png")

graficar_km(f_peso, x, "Supervivencia según categoría de peso al nacer",
            "Distribución por rangos de peso inicial. 8 terneros sin peso excluidos.",
            c("\u2264 38 kg", "38–40 kg", "\u2265 40 kg"), paletas$peso,
            "reports/figures/km_peso.png")

# 2. Tasa por tramo de edad
tramos    <- c(-Inf, 7, 14, 30, 60, Inf)
etiquetas <- c("0-7 d", "8-14 d", "15-30 d", "31-60 d", ">60 d")
riesgo <- bind_rows(lapply(seq_along(etiquetas), function(i) {
  a <- tramos[i]; b <- tramos[i + 1]
  tibble(
    tramo       = etiquetas[i],
    dias_riesgo = sum(pmax(0, pmin(x$tiempo_surv, b) - pmax(a, 0)), na.rm = TRUE),
    muertes     = sum(x$muerte_n == 1 & x$tiempo_surv > a & x$tiempo_surv <= b, na.rm = TRUE)
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
  labs(title = "Tasa de mortalidad por tramo de edad",
       subtitle = "Muertes por 1.000 terneros-día con IC 95% (distribución de Poisson)",
       x = "Edad", y = "Muertes por 1.000 terneros-día") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 13, color = "#1F2933"),
        plot.subtitle = element_text(size = 10, color = "#52606D"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())
ggsave("reports/figures/riesgo_por_tramo.png", g_riesgo, width = 7.5, height = 4.8, dpi = 220)

# 3. Cox
x_cox <- x |> filter(!muerte_dia0)
cat("\nCox: terneros incluidos:", nrow(x_cox),
    "| muertes posteriores:", sum(x_cox$muerte_n), "\n")

cluster_madre <- "+ cluster(codigo_madre)"

# tt() para peso5: HR que cambia con log(tiempo)
tt_peso <- function(x, t, ...) x * log(t)

hr_tabla <- function(m, nombre) {
  s <- summary(m)
  tibble(
    modelo = nombre,
    termino = rownames(s$conf.int),
    HR = s$conf.int[, "exp(coef)"],
    IC_inf = s$conf.int[, "lower .95"],
    IC_sup = s$conf.int[, "upper .95"],
    p = s$coefficients[, "Pr(>|z|)"]
  )
}

# HR de peso5 a t = 30 días (a partir del modelo con tt)
hr_peso_t30 <- function(m, t_ref = 30) {
  s   <- summary(m)
  b   <- s$coefficients
  V   <- vcov(m)
  est <- b["peso5", "coef"] + b["tt(peso5)", "coef"] * log(t_ref)
  se  <- sqrt(
    V["peso5", "peso5"] +
      2 * log(t_ref) * V["peso5", "tt(peso5)"] +
      log(t_ref)^2 * V["tt(peso5)", "tt(peso5)"]
  )
  tibble(
    termino = "peso5_t30",
    HR      = exp(est),
    IC_inf  = exp(est - 1.96 * se),
    IC_sup  = exp(est + 1.96 * se),
    p       = 2 * pnorm(-abs(est / se))
  )
}

# Modelo A: factores del animal y del parto, con tt(peso5) y mellizos
fA <- Surv(tiempo_surv, muerte_n) ~
  categoria_madre + asistido + peso5 + sexo + anio + mellizos + tt(peso5)

cox_A <- coxph(
  fA, data = x_cox, cluster = codigo_madre,
  tt = tt_peso
)
print(summary(cox_A))

# Modelo B: A + THI prenatal 7 días
fB <- Surv(tiempo_surv, muerte_n) ~
  categoria_madre + asistido + peso5 + sexo + anio + mellizos +
  thi_media_pre7d + tt(peso5)

cox_B <- coxph(
  fB, data = x_cox, cluster = codigo_madre,
  tt = tt_peso
)
print(summary(cox_B))

# Modelo C: A + THI del día del nacimiento
fC <- Surv(tiempo_surv, muerte_n) ~
  categoria_madre + asistido + peso5 + sexo + anio + mellizos +
  thi_nac + tt(peso5)

cox_C <- coxph(
  fC, data = x_cox, cluster = codigo_madre,
  tt = tt_peso
)
print(summary(cox_C))

# Supuesto de riesgos proporcionales
# cox.zph no acepta cluster; se evalúa sobre el modelo convencional con tt()
cat("\ncox.zph (modelo A, sin cluster, con tt):\n")
print(cox.zph(coxph(fA, data = x_cox, tt = tt_peso)))
cat("\ncox.zph (modelo B, sin cluster, con tt):\n")
print(cox.zph(coxph(fB, data = x_cox, tt = tt_peso)))
cat("\ncox.zph (modelo C, sin cluster, con tt):\n")
print(cox.zph(coxph(fC, data = x_cox, tt = tt_peso)))

# Tabla comparativa: se reemplaza la fila de peso5 por su HR a 30 días
tabla_hr <- bind_rows(
  hr_tabla(cox_A, "A: Animal y parto") |>
    filter(termino != "tt(peso5)") |>
    bind_rows(hr_peso_t30(cox_A) |> mutate(modelo = "A: Animal y parto")) |>
    filter(termino != "peso5"),
  hr_tabla(cox_B, "B: A + THI prenatal 7 días") |>
    filter(termino != "tt(peso5)") |>
    bind_rows(hr_peso_t30(cox_B) |> mutate(modelo = "B: A + THI prenatal 7 días")) |>
    filter(termino != "peso5"),
  hr_tabla(cox_C, "C: A + THI día nacimiento") |>
    filter(termino != "tt(peso5)") |>
    bind_rows(hr_peso_t30(cox_C) |> mutate(modelo = "C: A + THI día nacimiento")) |>
    filter(termino != "peso5")
)

print(tabla_hr, n = Inf)
write_csv(tabla_hr, "reports/tables/cox_modelos.csv")
write_csv(tabla_hr, "data/clean/powerbi/cox_modelos.csv")

# Gráficos de HR
forest <- function(t, titulo, archivo) {
  t_plot <- t |>
    mutate(
      etiqueta = limpiar_termino(termino),
      direccion = case_when(
        IC_inf > 1 ~ "Mayor hazard",
        IC_sup < 1 ~ "Menor hazard",
        TRUE ~ "No concluyente"
      )
    )
  g <- ggplot(t_plot, aes(x = HR, y = reorder(etiqueta, HR), color = direccion)) +
    geom_vline(xintercept = 1, linetype = 2, color = "#52606D") +
    geom_errorbar(aes(xmin = IC_inf, xmax = IC_sup),
                  orientation = "y", width = 0.22) +
    geom_point(size = 2.8) +
    scale_x_log10() +
    scale_color_manual(values = c("Mayor hazard" = "#B84A39",
                                  "Menor hazard" = "#2B5B84",
                                  "No concluyente" = "#6C757D")) +
    labs(title = titulo,
         subtitle = "Hazard ratios e IC 95%. Peso al nacer: HR a 30 días (modelo con tt).",
         x = "Hazard ratio (escala logarítmica)", y = NULL, color = NULL) +
    theme_minimal(base_size = 11)
  ggsave(archivo, g, width = 8.5, height = 5.2, dpi = 220)
}

forest(filter(tabla_hr, modelo == "A: Animal y parto"),
       "Modelo A — Factores del animal y parto", "reports/figures/forest_A.png")
forest(filter(tabla_hr, modelo == "B: A + THI prenatal 7 días"),
       "Modelo B — Modelo A y THI prenatal", "reports/figures/forest_B.png")
forest(filter(tabla_hr, modelo == "C: A + THI día nacimiento"),
       "Modelo C — Modelo A y THI del nacimiento", "reports/figures/forest_C.png")

# Nota metodológica para el texto del análisis
cat("\n--- Notas metodológicas ---\n")
cat("- peso5 viola PH (cox.zph p = 0.02). Se modeló con tt(peso5) = peso5 * log(t).\n")
cat("  El HR de peso al nacer reportado corresponde a t = 30 días.\n")
cat("- Jersey: 0/96 muertes, IC 95% 0-3.8%. No incluida en Cox (0 eventos).\n")
cat("- Verano n = 54; primavera n = 92. IC amplios, interpretar con cautela.\n")
cat("- 8 terneros sin peso registrado excluidos del KM de peso.\n")
cat("- cox.zph se evaluó sin cluster (cox.zph no acepta cluster).\n")