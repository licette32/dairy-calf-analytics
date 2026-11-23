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


orden <- tribble(
  ~variable,         ~grupo,                  ~var_lab,                 ~etiqueta,
  "anio",            "2023",                  "Año de nacimiento",      "2023",
  "anio",            "2024",                  "Año de nacimiento",      "2024",
  "anio",            "2025",                  "Año de nacimiento",      "2025",
  "categoria_madre", "vaca",                  "Categoría de la madre",  "Vaca",
  "categoria_madre", "vaquillona_1er_parto",  "Categoría de la madre",  "Vaquillona 1.er parto",
  "categoria_madre", "vaquillona_2do_parto",  "Categoría de la madre",  "Vaquillona 2.º parto",
  "tipo_parto",      "Normal",                "Tipo de parto",          "Normal",
  "tipo_parto",      "Asistido",              "Tipo de parto",          "Asistido",
  "peso_cat",        "<=38 kg",               "Peso al nacer",          "≤ 38 kg",
  "peso_cat",        "39-40 kg",              "Peso al nacer",          "39–40 kg",
  "peso_cat",        ">40 kg",                "Peso al nacer",          "> 40 kg",
  "raza",            "Holando",               "Raza",                   "Holando",
  "raza",            "Jersey",                "Raza",                   "Jersey",
  "sexo",            "Hembra",                "Sexo",                   "Hembra",
  "sexo",            "Macho",                 "Sexo",                   "Macho",
  "estacion",        "verano",                "Estación de nacimiento", "Verano",
  "estacion",        "otoño",                 "Estación de nacimiento", "Otoño",
  "estacion",        "invierno",              "Estación de nacimiento", "Invierno",
  "estacion",        "primavera",             "Estación de nacimiento", "Primavera",
  "mellizos",        "FALSE",                 "Gestación",              "Simple",
  "mellizos",        "TRUE",                  "Gestación",              "Mellizos")

p0_pct <- 100 * sum(v$muerte) / nrow(v)      # mortalidad global (línea de referencia)

g <- tabla |>
  inner_join(orden, by = c("variable", "grupo")) |>
  mutate(var_lab  = factor(var_lab, levels = unique(orden$var_lab)),
         etiqueta = factor(etiqueta, levels = rev(orden$etiqueta)),
         txt      = paste0(muertes, "/", n))

ggplot(g, aes(mortalidad_pct, etiqueta)) +
  geom_vline(xintercept = p0_pct, linetype = 2, colour = "grey40") +
  geom_linerange(aes(xmin = ic_inf, xmax = ic_sup), orientation = "y", colour = "grey45") +
  geom_point(size = 2.6, colour = "#2c5530") +
  geom_text(aes(x = 30, label = txt), hjust = 1, size = 3, colour = "grey30") +
  facet_grid(var_lab ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_x_continuous(limits = c(0, 30), breaks = seq(0, 25, 5)) +
  labs(x = "Mortalidad en guachera (%) con IC 95 % exacto",
       y = NULL,
       caption = paste0("Línea punteada: mortalidad global (", round(p0_pct, 1),
                        " %). A la derecha: muertes/terneros de cada grupo.")) +
  theme_minimal(base_size = 11) +
  theme(strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0, hjust = 0, face = "bold"),
        panel.grid.major.y = element_blank(),
        panel.spacing.y = unit(0.6, "lines"))
ggsave("reports/figures/mortalidad_por_grupo.png", width = 9, height = 8, dpi = 200)




# 3. Peso al nacer
print(v |> group_by(muerte) |> summarise(n = n(), media = mean(peso_nacer_kg, na.rm = TRUE),
                                         sd = sd(peso_nacer_kg, na.rm = TRUE)))
print(wilcox.test(peso_nacer_kg ~ muerte, data = v))




# 4- Causas anotadas en terneros muertos (solo lo descriptivo)

obs <- tolower(iconv(replace_na(v$Observaciones, ""), to = "ASCII//TRANSLIT"))

v <- v |> mutate(causa = case_when(
  grepl("diarre|enteritis|enterotox", obs)                      ~ "Diarrea / enteritis",
  grepl("neumon", obs)                                          ~ "Neumonía",
  grepl("timpan|intestin|torsion|torcion|abomaso|ulcera", obs)  ~ "Timpanismo / problema intestinal",
  grepl("parto|asist|mellizo|piso|quebrad", obs)                ~ "Parto / traumatismo",
  obs == ""                                                     ~ "Sin observación",
  TRUE                                                          ~ "Otra"))

print(v |> filter(muerte) |> count(causa, sort = TRUE))



# 5- Serie mensual

mes_nac <- v |> mutate(mes = floor_date(fecha_nacimiento, "month")) |>
  group_by(mes) |> summarise(nacimientos = n(), muertes = sum(muerte), .groups = "drop") |>
  complete(mes = seq(min(mes), max(mes), by = "month"),
           fill = list(nacimientos = 0, muertes = 0)) |>
  mutate(mortalidad_pct = if_else(nacimientos > 0, 100 * muertes / nacimientos, NA_real_))


trim <- v |>
  mutate(trim = floor_date(fecha_nacimiento, "quarter")) |>
  group_by(trim) |> summarise(n = n(), muertes = sum(muerte), .groups = "drop") |>
  filter(n >= 30) |>                        # se omiten trimestres con muy pocos nacimientos
  rowwise() |>
  mutate(mortalidad_pct = 100 * muertes / n,
         ic_inf = 100 * binom.test(muertes, n)$conf.int[1],
         ic_sup = 100 * binom.test(muertes, n)$conf.int[2]) |>
  ungroup()

ggplot(trim, aes(trim, mortalidad_pct)) +
  geom_hline(yintercept = 100 * sum(v$muerte) / nrow(v), linetype = 2, colour = "grey40") +
  geom_linerange(aes(ymin = ic_inf, ymax = ic_sup), colour = "#2c5530") +
  geom_point(size = 2.8, colour = "#2c5530") +
  geom_text(aes(y = ic_sup, label = paste0(muertes, "/", n)), vjust = -0.7, size = 2.8, colour = "grey30") +
  scale_x_date(breaks = trim$trim, date_labels = "%b\n%Y") +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.12))) +
  labs(x = "Trimestre de nacimiento (primer mes)", y = "Mortalidad de la cohorte (%)",
       caption = "Barras: IC 95 % exacto. Sobre cada punto: muertes/terneros. Se omiten trimestres con menos de 30 terneros. Línea punteada: mortalidad global.") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())
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

roll3 <- function(x, w) as.numeric(stats::filter(x, rep(1, 3), sides = 1))   # suma móvil de 3 meses
tm_g <- tm |>
  mutate(d3 = roll3(dias_ternero, 3), m3 = roll3(muertes, 3),
         tasa_3m = if_else(!is.na(d3) & d3 >= 3000, 1000 * m3 / d3, NA_real_),
         ok = dias_ternero >= 1000)          # meses con exposición suficiente para interpretarse

ggplot(filter(tm_g, ok), aes(mes)) +
  geom_hline(yintercept = 1000 * p0, linetype = 2, colour = "grey40") +
  geom_step(aes(y = lim_sup), colour = "firebrick", linewidth = 0.4) +
  geom_point(aes(y = tasa_x1000, colour = fuera_control), size = 2, alpha = 0.7) +
  geom_line(data = tm_g, aes(y = tasa_3m), colour = "#2c5530", linewidth = 1, na.rm = TRUE) +
  scale_colour_manual(values = c("FALSE" = "grey50", "TRUE" = "firebrick"), guide = "none") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  coord_cartesian(ylim = c(0, 4)) +
  labs(x = NULL, y = "Muertes por 1.000 terneros-día",
       caption = paste0("Puntos: tasa mensual (solo meses con ≥ 1.000 terneros-día). Línea verde: tasa móvil de 3 meses (se corta en los meses con pocos terneros).\n",
                        "Línea punteada: tasa media del período; línea roja: límite de control (media + 3 DE, Poisson).")) +
  theme_minimal(base_size = 11)
ggsave("reports/figures/tasa_mensual.png", width = 9, height = 4.5, dpi = 200)

# STL sobre la tasa: meses con < 300 terneros-día se interpolan (muy inestables)
tr <- tm |> mutate(tasa_i = if_else(dias_ternero >= 300, tasa_x1000, NA_real_))
tr$tasa_i <- approx(seq_along(tr$tasa_i), tr$tasa_i, xout = seq_along(tr$tasa_i), rule = 2)$y
tasa_stl <- stl(a_ts(tr$mes, tr$tasa_i), "periodic", robust = TRUE)
cat("Fuerza estacional de la tasa:", round(fuerza_est(tasa_stl), 2), "\n")
png("reports/figures/stl_tasa.png", 2000, 1400, res = 200)
plot(tasa_stl, main = "Tasa mensual de mortalidad: descomposición STL"); dev.off()