# Descriptivo

library(dplyr); library(readr); library(tidyr); library(ggplot2); library(lubridate)


for (d in c("reports/figures", "reports/tables", "data/clean/powerbi"))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

d <- read_csv("data/clean/terneros_analisis.csv", show_col_types = FALSE)
v <- d |> filter(nacido_vivo) |>
  mutate(mellizos = obs_mellizos,
         peso_cat = cut(peso_nacer_kg, c(0, 38, 40, Inf),
                        labels = c("<=38 kg", "39-40 kg", ">40 kg")))



# 1. Flujo de la población

flujo <- tibble(
  etapa = c("Registros totales", "Nacidos muertos / abortos",
            "Nacidos vivos (población de estudio)", "Muertes en guachera"),
  n = c(nrow(d), sum(!d$nacido_vivo), nrow(v), sum(v$muerte)))
print(flujo); write_csv(flujo, "reports/tables/flujo_poblacion.csv")



# 2- Mortalidad por grupo, con IC 95% exacto 

tasa <- function(df, nm) {
  df |> mutate(grupo = as.character(.data[[nm]])) |>
    filter(!is.na(grupo)) |> group_by(grupo) |>
    summarise(n = n(), muertes = sum(muerte), .groups = "drop") |>
    rowwise() |>
    mutate(mortalidad_pct = 100 * muertes / n,
           ic_inf = 100 * binom.test(muertes, n)$conf.int[1],
           ic_sup = 100 * binom.test(muertes, n)$conf.int[2]) |>
    ungroup()
}

vars  <- c("categoria_madre", "sexo", "raza", "tipo_parto", "mellizos",
           "peso_cat", "estacion", "anio")

tabla <- bind_rows(lapply(vars, function(nm) mutate(tasa(v, nm), variable = nm, .before = 1)))


print(tabla, n = Inf)

write_csv(tabla, "reports/tables/mortalidad_por_grupo.csv")
write_csv(tabla, "data/clean/powerbi/mortalidad_por_grupo.csv")


ggplot(tabla, aes(grupo, mortalidad_pct)) +
  geom_pointrange(aes(ymin = ic_inf, ymax = ic_sup)) +
  facet_wrap(~variable, scales = "free_x", ncol = 4) +
  labs(x = NULL, y = "Mortalidad (%) e IC 95%") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("reports/figures/mortalidad_por_grupo.png", width = 12, height = 6, dpi = 200)




# 3. Peso al nacer
print(v |> group_by(muerte) |> summarise(n = n(), media = mean(peso_nacer_kg, na.rm = TRUE),
                                         sd = sd(peso_nacer_kg, na.rm = TRUE)))
print(wilcox.test(peso_nacer_kg ~ muerte, data = v))




# 4- Causas anotadas en terneros muertos (solo lo descriptivo)

obs <- tolower(iconv(replace_na(v$Observaciones, ""), to = "ASCII//TRANSLIT"))

v <- v |> mutate(causa = case_when(
  grepl("diarre|enteritis", obs)                    ~ "Diarrea / enteritis",
  grepl("neumon", obs)                              ~ "Neumonía",
  grepl("timpan|intestin|torsion|torcion", obs)     ~ "Timpanismo / problema intestinal",
  obs == ""                                         ~ "Sin observación",
  TRUE                                              ~ "Otra"))

print(v |> filter(muerte) |> count(causa, sort = TRUE))



# 5- Serie mensual

mes_nac <- v |> mutate(mes = floor_date(fecha_nacimiento, "month")) |>
  group_by(mes) |> summarise(nacimientos = n(), muertes = sum(muerte), .groups = "drop") |>
  complete(mes = seq(min(mes), max(mes), by = "month"),
           fill = list(nacimientos = 0, muertes = 0)) |>
  mutate(mortalidad_pct = if_else(nacimientos > 0, 100 * muertes / nacimientos, NA_real_))


ggplot(mes_nac, aes(mes, mortalidad_pct)) +
  geom_line(na.rm = TRUE, colour = "grey60") +
  geom_point(aes(size = nacimientos), na.rm = TRUE) +
  labs(x = "Mes de nacimiento", y = "Mortalidad de la cohorte (%)", size = "Nacimientos") +
  theme_minimal()
ggsave("reports/figures/mortalidad_mensual.png", width = 9, height = 4.5, dpi = 200)


# Descomposición STL: nacimientos y muertes (por mes de ocurrencia)
a_ts <- function(meses, valores)
  ts(valores, start = c(year(min(meses)), month(min(meses))), frequency = 12)
fuerza_est <- function(s) {
  r <- s$time.series[, "remainder"]; se <- s$time.series[, "seasonal"]
  max(0, 1 - var(r) / var(se + r))
}



nac_stl <- stl(a_ts(mes_nac$mes, mes_nac$nacimientos), "periodic", robust = TRUE)
mm <- v |> filter(muerte) |> count(mes = floor_date(fecha_egreso, "month"), name = "muertes") |>
  complete(mes = seq(min(mes), max(mes), by = "month"), fill = list(muertes = 0))
mue_stl <- stl(a_ts(mm$mes, mm$muertes), "periodic", robust = TRUE)
cat("Fuerza estacional nacimientos:", round(fuerza_est(nac_stl), 2),
    "| muertes:", round(fuerza_est(mue_stl), 2), "\n")

png("reports/figures/stl_nacimientos.png", 2000, 1400, res = 200)
plot(nac_stl, main = "Nacimientos mensuales: descomposición STL"); dev.off()
png("reports/figures/stl_muertes.png", 2000, 1400, res = 200)
plot(mue_stl, main = "Muertes mensuales: descomposición STL"); dev.off()

# Relación con clima mensual (Spearman, meses con >= 10 nacimientos)
clima_m <- read_csv("data/clean/clima_diario.csv", show_col_types = FALSE) |>
  mutate(mes = floor_date(fecha, "month")) |> group_by(mes) |>
  summarise(tmin_mes = mean(tmin), precip_mes = sum(precip), .groups = "drop")
mc <- left_join(mes_nac, clima_m, by = "mes")

print(cor.test(~ mortalidad_pct + tmin_mes, data = filter(mc, nacimientos >= 10),
               method = "spearman", exact = FALSE))
write_csv(mc, "data/clean/powerbi/resumen_mensual.csv")

# ---- 5b. Tasa de mortalidad mensual (mes calendario, terneros-día en riesgo) ----
nac <- as.numeric(v$fecha_nacimiento); egr <- as.numeric(v$fecha_egreso)
meses <- seq(floor_date(min(v$fecha_nacimiento), "month"),
             floor_date(max(v$fecha_egreso), "month"), by = "month")

tm <- bind_rows(lapply(as.list(meses), function(m0) {
  a <- as.numeric(m0); b <- as.numeric(m0 %m+% months(1))
  dias <- pmax(0, pmin(egr + 1, b) - pmax(nac, a))     # días que cada ternero estuvo en el mes
  tibble(mes = m0,
         dias_ternero   = sum(dias),
         presentes_prom = sum(dias) / (b - a),          # terneros presentes promedio
         nacimientos    = sum(nac >= a & nac < b),
         muertes        = sum(v$muerte & egr >= a & egr < b))
}))

p0 <- sum(tm$muertes) / sum(tm$dias_ternero)            # tasa media del período
tm <- tm |>
  mutate(tasa_x1000 = 1000 * muertes / dias_ternero,
         mortalidad_mensual_pct = 100 * muertes / presentes_prom,
         lim_sup = 1000 * (p0 + 3 * sqrt(p0 / dias_ternero))) |>   # gráfico de control (Poisson)
  rowwise() |>
  mutate(ic_inf = 1000 * poisson.test(muertes, dias_ternero)$conf.int[1],
         ic_sup = 1000 * poisson.test(muertes, dias_ternero)$conf.int[2]) |>
  ungroup() |>
  mutate(fuera_control = tasa_x1000 > lim_sup)
print(tm, n = Inf)
write_csv(tm, "data/clean/powerbi/tasa_mensual.csv")

ggplot(tm, aes(mes, tasa_x1000)) +
  geom_hline(yintercept = 1000 * p0, linetype = 2) +
  geom_line(aes(y = lim_sup), colour = "red", linewidth = 0.4) +
  geom_pointrange(aes(ymin = ic_inf, ymax = ic_sup, colour = fuera_control)) +
  scale_colour_manual(values = c("grey30", "red"), guide = "none") +
  labs(x = NULL, y = "Muertes por 1.000 terneros-día",
       subtitle = "Línea punteada: tasa media; línea roja: límite de control (media + 3 DE)") +
  theme_minimal()
ggsave("reports/figures/tasa_mensual.png", width = 9, height = 4.5, dpi = 200)

# STL sobre la tasa: meses con < 300 terneros-día se interpolan (muy inestables)
tr <- tm |> mutate(tasa_i = if_else(dias_ternero >= 300, tasa_x1000, NA_real_))
tr$tasa_i <- approx(seq_along(tr$tasa_i), tr$tasa_i, xout = seq_along(tr$tasa_i), rule = 2)$y
tasa_stl <- stl(a_ts(tr$mes, tr$tasa_i), "periodic", robust = TRUE)
cat("Fuerza estacional de la tasa:", round(fuerza_est(tasa_stl), 2), "\n")
png("reports/figures/stl_tasa.png", 2000, 1400, res = 200)
plot(tasa_stl, main = "Tasa mensual de mortalidad: descomposición STL"); dev.off()