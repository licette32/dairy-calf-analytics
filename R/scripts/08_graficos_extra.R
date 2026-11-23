# Gráficos complementarios

library(dplyr); library(readr); library(ggplot2); library(lubridate); library(survival)
library(survminer)
library(tidyr)
library(dplyr)
library(tibble)
library(pheatmap)

for (d in c("reports/figures", "reports/tables")) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

source("R/prep_modelo.R")
x <- cargar_modelo() |>
  mutate(resultado = if_else(muerte, "Murió", "Sobrevivió"))

etiquetas_madre <- c(
  "vaca"                 = "Vaca",
  "vaquillona_1er_parto" = "Vaquillona 1.er parto",
  "vaquillona_2do_parto" = "Vaquillona 2.do parto"
)

limpiar_madre <- function(v) {
  out <- etiquetas_madre[as.character(v)]
  ifelse(is.na(out), as.character(v), unname(out))
}

# 1. Tabla 1: sobrevivientes vs muertos
num_vars <- c("peso_nacer_kg", "tmin_media_7d", "tmin_media_21d", "prop_lluvia_7d", "hum_media_7d")
tabla1 <- bind_rows(lapply(num_vars, function(nm) {
  v1 <- x[[nm]][!x$muerte]
  v2 <- x[[nm]][x$muerte]
  tibble(
    variable      = nm,
    sobrevivieron = sprintf("%.2f (%.2f)", mean(v1, na.rm = TRUE), sd(v1, na.rm = TRUE)),
    murieron      = sprintf("%.2f (%.2f)", mean(v2, na.rm = TRUE), sd(v2, na.rm = TRUE)),
    p_wilcoxon    = round(wilcox.test(v1, v2)$p.value, 3)
  )
}))
print(tabla1)
write_csv(tabla1, "reports/tables/tabla1_numericas.csv")

# 2. Peso al nacer según resultado
g_peso <- ggplot(x, aes(x = resultado, y = peso_nacer_kg, fill = resultado)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.85, width = 0.48) +
  geom_jitter(width = 0.14, alpha = 0.15, size = 1.2, color = "#1F2933") +
  scale_fill_manual(values = c("Sobrevivió" = "#3B6E4C", "Murió" = "#B84A39"), guide = "none") +
  labs(
    title = "Peso al nacer según resultado en guachera",
    x = NULL,
    y = "Peso al nacer (kg)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13, color = "#1F2933"),
    panel.grid.minor = element_blank()
  )
ggsave("reports/figures/peso_por_resultado.png", g_peso, width = 6.5, height = 4.5, dpi = 220)

# 3. ¿A qué edad mueren? (con las etapas del manejo)
g_edad <- x |>
  filter(muerte) |>
  ggplot(aes(x = dias_guachera)) +
  geom_histogram(binwidth = 5, boundary = 0, fill = "#B84A39", color = "white", alpha = 0.9) +
  geom_vline(xintercept = c(7, 30, 45, 60), linetype = 2, color = "#1F2933", linewidth = 0.7) +
  labs(
    title = "Edad a la muerte en guachera",
    subtitle = "Líneas: 7 d (primera semana), 30 d (fin 6 L), 45 d (transición), 60 d (destete)",
    x = "Edad a la muerte (días)",
    y = "Terneros"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13, color = "#1F2933"),
    plot.subtitle = element_text(size = 10, color = "#52606D"),
    panel.grid.minor = element_blank()
  )
ggsave("reports/figures/edad_a_la_muerte.png", g_edad, width = 8, height = 4.5, dpi = 220)

# 4. Evolución mensual de muertes (agrupado por mes para evitar barras finas dispersas)
curva_mes <- x |>
  filter(muerte) |>
  mutate(
    mes_muerte = floor_date(as.Date(fecha_egreso), "month"),
    madre_etq  = factor(limpiar_madre(categoria_madre),
                        levels = c("Vaca", "Vaquillona 1.er parto", "Vaquillona 2.do parto"))
  ) |>
  count(mes_muerte, madre_etq)

g_curva <- ggplot(curva_mes, aes(x = mes_muerte, y = n, fill = madre_etq)) +
  geom_col(width = 22, alpha = 0.92) +
  scale_fill_manual(values = c("#C2822A", "#3B6E4C", "#2B5B84")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
  labs(
    title = "Evolución mensual de muertes en guachera",
    subtitle = "Conteo por mes de egreso/muerte según categoría de madre",
    x = "Mes de la muerte",
    y = "Muertes",
    fill = "Madre"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13, color = "#1F2933"),
    plot.subtitle = element_text(size = 10, color = "#52606D"),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )
ggsave("reports/figures/curva_semanal_muertes.png", g_curva, width = 10, height = 4.5, dpi = 220)

# 5. Mapa de calor mes x año de la tasa mensual

tm_ordenado <- tm |>
  mutate(mes_f = factor(mes_f, levels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
                                          "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))) |>
  arrange(anio_f, mes_f)

mat_tasa <- tm_ordenado |>
  select(anio_f, mes_f, tasa_plot) |>
  tidyr::pivot_wider(names_from = mes_f, values_from = tasa_plot) |>
  tibble::column_to_rownames("anio_f") |>
  as.matrix()

mat_muertes <- tm_ordenado |>
  select(anio_f, mes_f, muertes) |>
  tidyr::pivot_wider(names_from = mes_f, values_from = muertes) |>
  tibble::column_to_rownames("anio_f") |>
  as.matrix()

# Reemplaza NA por "" solo para el texto visible en las celdas
etiquetas_celdas <- ifelse(is.na(mat_muertes), "", sprintf("%.0f", mat_muertes))

png("reports/figures/mapa_calor_mensual.png", width = 1980, height = 836, res = 220)
pheatmap(
  mat_tasa,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  display_numbers = etiquetas_celdas,
  na_col = "#E4E7EB",
  color = colorRampPalette(c("#F4F6F8", "#B84A39"))(50),
  main = "Tasa mensual (color) y muertes (número) · gris = baja exposición o sin dato",
  legend = TRUE
)
dev.off()


# 6. Supervivencia según frío de la primera semana (terciles)
x <- x |>
  mutate(frio_cat = cut(
    tmin_media_7d,
    breaks = quantile(tmin_media_7d, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
    include.lowest = TRUE,
    dig.lab = 3
  ))

print(table(x$frio_cat))
print(survdiff(Surv(tiempo_surv, muerte_n) ~ frio_cat, data = x))

f_frio <- survfit(Surv(tiempo_surv, muerte_n) ~ frio_cat, data = x)
g_frio <- ggsurvplot(
  f_frio,
  data = x,
  palette = c("#2B5B84", "#C2822A", "#3B6E4C"),
  pval = TRUE,
  pval.coord = c(3, 0.81),
  conf.int = FALSE,
  risk.table = TRUE,
  risk.table.title = "En riesgo",
  xlim = c(0, 100),
  break.time.by = 20,
  ylim = c(0.8, 1),
  xlab = "Días desde el nacimiento",
  ylab = "Probabilidad de supervivencia",
  title = "Supervivencia según temperatura mínima de la primera semana",
  subtitle = "Terciles de temperatura mínima media (7 días)",
  legend.title = "Tmín media 7 d (°C)",
  ggtheme = theme_bw(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 13, color = "#1F2933"),
      plot.subtitle = element_text(size = 10, color = "#52606D"),
      legend.position = "top",
      panel.grid.minor = element_blank()
    )
)

png("reports/figures/km_frio.png", 2200, 1750, res = 220)
print(g_frio)
dev.off()