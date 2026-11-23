# Graficos

library(dplyr); library(readr); library(ggplot2); library(lubridate)
library(survival); library(survminer)


for (d in c("reports/figures", "reports/tables"))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
source("R/prep_modelo.R")
x <- cargar_modelo() |> mutate(resultado = if_else(muerte, "Murió", "Sobrevivió"))



# 1. Tabla 1: sobrevivientes vs muertos

num_vars <- c("peso_nacer_kg", "tmin_media_7d", "tmin_media_21d", "prop_lluvia_7d", "hum_media_7d")
tabla1 <- bind_rows(lapply(num_vars, function(nm) {
  v1 <- x[[nm]][!x$muerte]; v2 <- x[[nm]][x$muerte]
  tibble(variable = nm,
         sobrevivieron = sprintf("%.2f (%.2f)", mean(v1, na.rm = TRUE), sd(v1, na.rm = TRUE)),
         murieron      = sprintf("%.2f (%.2f)", mean(v2, na.rm = TRUE), sd(v2, na.rm = TRUE)),
         p_wilcoxon    = round(wilcox.test(v1, v2)$p.value, 3))
}))
print(tabla1); write_csv(tabla1, "reports/tables/tabla1_numericas.csv")



# 2. Peso al nacer según resultado

ggplot(x, aes(resultado, peso_nacer_kg)) +
  geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.15, alpha = 0.2) +
  labs(x = NULL, y = "Peso al nacer (kg)") + theme_minimal()
ggsave("reports/figures/peso_por_resultado.png", width = 6, height = 4.5, dpi = 200)



# 3. ¿A qué edad mueren? (con las etapas del manejo)

ggplot(filter(x, muerte), aes(dias_guachera)) +
  geom_histogram(binwidth = 5, boundary = 0, fill = "grey60", colour = "white") +
  geom_vline(xintercept = c(7, 30, 45, 60), linetype = 2) +
  labs(x = "Edad a la muerte (días)", y = "Terneros",
       subtitle = "Líneas: 7 d (primera semana), 30 d (fin 6 L), 45 d (transición), 60 d (destete)") +
  theme_minimal()
ggsave("reports/figures/edad_a_la_muerte.png", width = 8, height = 4.5, dpi = 200)



# 4. Curva de muertes por semana (como curva epidémica)

curva <- x |> filter(muerte) |>
  mutate(semana = floor_date(fecha_egreso, "week", week_start = 1)) |>
  count(semana, categoria_madre)
ggplot(curva, aes(semana, n, fill = categoria_madre)) + geom_col() +
  labs(x = "Semana de la muerte", y = "Muertes", fill = "Madre") + theme_minimal()
ggsave("reports/figures/curva_semanal_muertes.png", width = 10, height = 4.5, dpi = 200)




# 5. Mapa de calor mes x año de la tasa mensual 

tm <- read_csv("data/clean/powerbi/tasa_mensual.csv", show_col_types = FALSE) |>
  mutate(anio = factor(year(mes)), mes_n = month(mes, label = TRUE, abbr = TRUE),
         tasa_plot = if_else(dias_ternero >= 300, tasa_x1000, NA_real_))
ggplot(tm, aes(mes_n, anio, fill = tasa_plot)) +
  geom_tile(colour = "white") + geom_text(aes(label = muertes), size = 3) +
  scale_fill_gradient(low = "grey95", high = "firebrick", na.value = "grey85") +
  labs(x = NULL, y = NULL, fill = "Muertes por\n1.000 terneros-día",
       subtitle = "Número = muertes del mes; gris = meses con poca exposición (< 300 terneros-día)") +
  theme_minimal()
ggsave("reports/figures/mapa_calor_mensual.png", width = 9, height = 3.5, dpi = 200)




# 6. Supervivencia según frío de la primera semana (terciles)

x <- x |> mutate(frio_cat = cut(tmin_media_7d,
                                quantile(tmin_media_7d, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                                include.lowest = TRUE, dig.lab = 3))
print(table(x$frio_cat))
print(survdiff(Surv(tiempo_surv, muerte_n) ~ frio_cat, data = x))
f_frio <- survfit(Surv(tiempo_surv, muerte_n) ~ frio_cat, data = x)
png("reports/figures/km_frio.png", 2200, 1700, res = 220)
print(ggsurvplot(f_frio, data = x, pval = TRUE, risk.table = TRUE, xlim = c(0, 100),
                 break.time.by = 20, ylim = c(0.8, 1), legend.title = "Tmín media 7 d (°C)"))
dev.off()