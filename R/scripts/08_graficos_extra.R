# Gráficos complementarios

library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(survival)
library(survminer)
library(tidyr)
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
  "vaquillona_1er_parto" = "Vaquillona 1.er parto"
)

limpiar_madre <- function(v) {
  out <- etiquetas_madre[as.character(v)]
  ifelse(is.na(out), as.character(v), unname(out))
}

# 1. Tabla 1: sobrevivientes vs muertos
num_vars <- c("peso_nacer_kg", "thi_nac", "thi_media_pre7d", "thi_media_pre21d",
              "tmax_media_pre7d", "hum_media_pre7d")
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
  labs(title = "Peso al nacer según resultado en guachera",
       x = NULL, y = "Peso al nacer (kg)") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 13, color = "#1F2933"),
        panel.grid.minor = element_blank())
ggsave("reports/figures/peso_por_resultado.png", g_peso, width = 6.5, height = 4.5, dpi = 220)

# 3. ¿A qué edad mueren?
g_edad <- x |>
  filter(muerte) |>
  ggplot(aes(x = dias_guachera)) +
  geom_histogram(binwidth = 5, boundary = 0, fill = "#B84A39", color = "white", alpha = 0.9) +
  geom_vline(xintercept = c(7, 30, 45, 60), linetype = 2, color = "#1F2933", linewidth = 0.7) +
  labs(title = "Edad a la muerte en guachera",
       subtitle = "Líneas: 7 d (primera semana), 30 d (fin 6 L), 45 d (transición), 60 d (destete)",
       x = "Edad a la muerte (días)", y = "Terneros") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 13, color = "#1F2933"),
        plot.subtitle = element_text(size = 10, color = "#52606D"),
        panel.grid.minor = element_blank())
ggsave("reports/figures/edad_a_la_muerte.png", g_edad, width = 8, height = 4.5, dpi = 220)

# 4. Evolución mensual de muertes
curva_mes <- x |>
  filter(muerte) |>
  mutate(
    mes_muerte = floor_date(as.Date(fecha_egreso), "month"),
    madre_etq  = factor(limpiar_madre(categoria_madre),
                        levels = c("Vaca", "Vaquillona 1.er parto"))
  ) |>
  count(mes_muerte, madre_etq)

g_curva <- ggplot(curva_mes, aes(x = mes_muerte, y = n, fill = madre_etq)) +
  geom_col(width = 22, alpha = 0.92) +
  scale_fill_manual(values = c("#C2822A", "#3B6E4C")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
  labs(title = "Evolución mensual de muertes en guachera",
       subtitle = "Conteo por mes de egreso/muerte según categoría de madre",
       x = "Mes de la muerte", y = "Muertes", fill = "Madre") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 13, color = "#1F2933"),
        plot.subtitle = element_text(size = 10, color = "#52606D"),
        legend.position = "top",
        panel.grid.minor = element_blank())
ggsave("reports/figures/curva_semanal_muertes.png", g_curva, width = 10, height = 4.5, dpi = 220)

# 5. Mapa de calor mes x año de la tasa mensual (2023-2025)
nac <- as.numeric(as.Date(x$fecha_nacimiento))
egr <- as.numeric(as.Date(x$fecha_egreso))
meses <- seq(floor_date(min(as.Date(x$fecha_nacimiento), na.rm = TRUE), "month"),
             floor_date(max(as.Date(x$fecha_egreso), na.rm = TRUE), "month"), by = "month")

tm <- bind_rows(lapply(as.list(meses), function(m0) {
  a <- as.numeric(m0)
  b <- as.numeric(m0 %m+% months(1))
  dias <- pmax(0, pmin(egr + 1, b) - pmax(nac, a))
  tibble(
    mes = m0,
    dias_ternero = sum(dias, na.rm = TRUE),
    muertes = sum(x$muerte %in% TRUE & !is.na(egr) & egr >= a & egr < b)
  )
})) |>
  mutate(
    tasa_x1000 = if_else(dias_ternero > 0, 1000 * muertes / dias_ternero, NA_real_),
    tasa_plot  = if_else(dias_ternero >= 1000, tasa_x1000, NA_real_)
  )

meses_es <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
              "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

tm_ordenado <- tm |>
  filter(year(mes) %in% 2023:2025) |>
  mutate(
    anio_f  = factor(year(mes), levels = c("2023", "2024", "2025")),
    mes_num = month(mes),
    mes_f   = factor(meses_es[mes_num], levels = meses_es)
  ) |>
  select(anio_f, mes_f, tasa_plot, muertes) |>
  tidyr::complete(anio_f, mes_f) |>
  arrange(anio_f, mes_f)

mat_tasa <- tm_ordenado |>
  select(anio_f, mes_f, tasa_plot) |>
  tidyr::pivot_wider(names_from = mes_f, values_from = tasa_plot) |>
  tibble::column_to_rownames("anio_f") |>
  as.matrix()
storage.mode(mat_tasa) <- "numeric"

mat_muertes <- tm_ordenado |>
  select(anio_f, mes_f, muertes) |>
  tidyr::pivot_wider(names_from = mes_f, values_from = muertes) |>
  tibble::column_to_rownames("anio_f") |>
  as.matrix()
storage.mode(mat_muertes) <- "numeric"

etiquetas_celdas <- matrix(
  ifelse(is.na(mat_muertes), "", sprintf("%.0f", mat_muertes)),
  nrow = nrow(mat_muertes),
  ncol = ncol(mat_muertes),
  dimnames = dimnames(mat_muertes)
)

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

# 6. Supervivencia según THI prenatal
x <- x |>
  mutate(
    thi_cat = ntile(thi_media_pre7d, 3),
    thi_cat = factor(
      thi_cat,
      levels = 1:3,
      labels = c("THI bajo", "THI intermedio", "THI alto")
    )
  )

print(table(x$thi_cat, useNA = "ifany"))

x_thi <- x |>
  filter(!is.na(thi_cat), !muerte_dia0)

f_thi <- survfit(
  Surv(tiempo_surv, muerte_n) ~ thi_cat,
  data = x_thi
)

g_thi <- ggsurvplot(
  f_thi,
  data = x_thi,
  pval = TRUE,
  conf.int = FALSE,
  risk.table = TRUE,
  xlim = c(0, 100),
  break.time.by = 20,
  xlab = "Días desde el nacimiento",
  ylab = "Probabilidad de supervivencia",
  title = "Supervivencia según THI prenatal",
  subtitle = "Terciles del THI medio de los 7 días previos al nacimiento",
  legend.title = "Exposición térmica",
  legend.labs = c("THI bajo", "THI intermedio", "THI alto"),
  ggtheme = theme_minimal()
)

png("reports/figures/km_thi_prenatal.png", width = 2200, height = 1750, res = 220)
print(g_thi)
dev.off()