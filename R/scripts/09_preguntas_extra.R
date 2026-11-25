# Preguntas extra . Anexo

library(dplyr); library(readr); library(tidyr); library(ggplot2); library(lubridate)

dir.create("reports/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("reports/tables",  showWarnings = FALSE, recursive = TRUE)


# Datos
a <- read_csv("data/clean/terneros_analisis.csv", show_col_types = FALSE) |>
  # Vaquillona = solo primer parto; desde el segundo parto es vaca
  mutate(
    categoria_madre = if_else(
      categoria_madre == "vaquillona_2do_parto",
      "vaca",
      categoria_madre
    )
  ) |>
  # Período de análisis: 2023-2025
  filter(anio >= 2023, anio <= 2025)

clima <- read_csv("data/clean/clima_diario.csv", show_col_types = FALSE)

v <- filter(a, nacido_vivo)

# Estilo
verde <- "#2E5A44"; rojo <- "#B84A39"; ambar <- "#C98A2E"
tema <- theme_minimal(base_size = 14) +
  theme(plot.title    = element_text(face = "bold", colour = "#24211E"),
        plot.subtitle = element_text(colour = "#5E564E"),
        plot.caption  = element_text(colour = "#5E564E"),
        panel.grid.minor = element_blank(),
        plot.background  = element_rect(fill = "#FFFDF9", colour = NA))
sin_ejes <- theme(axis.text.y = element_blank(), panel.grid = element_blank())

etq   <- c(vaca = "Vaca (2.º parto o más)", vaquillona_1er_parto = "Vaquillona (1.er parto)")
fmt   <- function(x) format(round(x, 1), decimal.mark = ",", nsmall = 1)
fmtp  <- function(x) format(round(x, 2), decimal.mark = ",", nsmall = 2)
guardar <- function(g, f, w = 8, h = 4.5)
  ggsave(paste0("reports/figures/", f), g, width = w, height = h, dpi = 220)



# 1. Partos vs muertes por año
anual <- v |> group_by(anio) |>
  summarise(nacidos = n(), muertes = sum(muerte), tasa = 100 * muertes / nacidos)
print(anual)
veces <- anual$tasa[anual$anio == 2025] / anual$tasa[anual$anio == 2023]

guardar(
  ggplot(anual, aes(factor(anio), tasa)) +
    geom_col(fill = rojo, width = .55) +
    geom_text(aes(label = paste0(fmt(tasa), " %")), vjust = -.4, fontface = "bold", size = 5) +
    scale_x_discrete(labels = paste0(anual$anio, "\n", anual$nacidos, " nacidos")) +
    scale_y_continuous(expand = expansion(mult = c(0, .15))) +
    labs(title = sprintf("Casi los mismos partos por año, pero la mortalidad se multiplicó por %s",
                         fmt(veces)),
         subtitle = "Porcentaje de terneros nacidos vivos que murieron en guachera",
         x = NULL, y = NULL) +
    tema + sin_ejes + theme(axis.text.x = element_text(size = 13)),
  "nacimientos_vs_muertes_anual.png", 8, 4.5)

# los meses con más partos tienen más mortalidad?
mensual <- v |> mutate(mes = floor_date(fecha_nacimiento, "month")) |>
  group_by(mes) |>
  summarise(nacidos = n(), muertes = sum(muerte), tasa = 100 * muertes / nacidos) |>
  filter(nacidos >= 10)
ct <- cor.test(mensual$nacidos, mensual$tasa, method = "spearman", exact = FALSE)
print(ct)
guardar(
  ggplot(mensual, aes(nacidos, tasa)) +
    geom_smooth(method = "lm", se = FALSE, colour = "grey60", linewidth = .6) +
    geom_point(aes(size = muertes), colour = verde, alpha = .75) +
    labs(title = "¿Meses con más partos tienen más mortalidad?",
         subtitle = sprintf("Sin relación clara (Spearman = %s, p = %s); meses con 10 o más nacimientos",
                            fmtp(ct$estimate), fmtp(ct$p.value)),
         x = "Nacimientos en el mes", y = "Mortalidad de esa camada (%)", size = "Muertes") +
    tema,
  "nacimientos_vs_mortalidad_mensual.png", 8, 4.5)



# 2. Vaquillona vs vaca

vc <- filter(v, !is.na(categoria_madre))
res <- vc |> group_by(categoria_madre) |>
  summarise(n = n(), x_mort = sum(muerte), x_asis = sum(tipo_parto == "Asistido"),
            .groups = "drop")
long <- bind_rows(
  transmute(res, grupo = unname(etq[categoria_madre]), indicador = "Terneros que murieron", x = x_mort, n),
  transmute(res, grupo = unname(etq[categoria_madre]), indicador = "Partos asistidos",      x = x_asis, n)) |>
  mutate(p = 100 * x / n,
         indicador = factor(indicador, c("Terneros que murieron", "Partos asistidos")),
         etiqueta  = paste0(fmt(p), " %\n(", x, " de ", n, ")"))
print(long)
p_mort <- chisq.test(table(vc$categoria_madre, vc$muerte))$p.value
p_asis <- chisq.test(table(vc$categoria_madre, vc$tipo_parto == "Asistido"))$p.value

guardar(
  ggplot(long, aes(grupo, p, fill = grupo)) +
    geom_col(width = .6, show.legend = FALSE) +
    geom_text(aes(label = etiqueta), vjust = -.3, size = 4, lineheight = .9) +
    facet_wrap(~indicador) +
    scale_fill_manual(values = setNames(c(ambar, verde), unname(etq))) +
    scale_y_continuous(expand = expansion(mult = c(0, .3))) +
    labs(title = "Las vacas tienen más partos asistidos que las vaquillonas",
         subtitle = sprintf("Partos asistidos: diferencia significativa (p = %s). Mortalidad: no es clara (p = %s)",
                            fmtp(p_asis), fmtp(p_mort)),
         x = NULL, y = NULL) +
    tema + sin_ejes + theme(strip.text = element_text(face = "bold", size = 13)),
  "vacas_vs_vaquillonas.png", 9, 4.5)

# solo consola: partos asistidos por año y categoría
print(vc |> group_by(anio, categoria_madre) |>
        summarise(n = n(), asistidos = sum(tipo_parto == "Asistido"),
                  pct = round(100 * asistidos / n, 1), .groups = "drop"))



# 3. Clima: mortalidad según temperatura mínima del día

f <- seq(as.Date("2023-02-01"), as.Date("2025-12-31"), by = "day")
d <- tibble(
  fecha     = f,
  en_riesgo = vapply(f, \(x) sum(v$fecha_nacimiento <= x & v$fecha_egreso >= x), numeric(1)),
  muertes   = vapply(f, \(x) sum(v$muerte & v$fecha_egreso == x), numeric(1))) |>
  left_join(clima, by = "fecha") |>
  filter(en_riesgo > 0, !is.na(tmin))

frio <- d |>
  mutate(grupo = cut(tmin, c(-Inf, 3, 5, 10, 15, Inf),
                     labels = c("≤ 3 °C", "3–5 °C", "5–10 °C", "10–15 °C", "> 15 °C"))) |>
  group_by(grupo) |>
  summarise(dias = n(), muertes = sum(muertes), ct = sum(en_riesgo), .groups = "drop") |>
  mutate(tasa = 1000 * muertes / ct)
print(frio)

p_de <- function(mask) {
  r <- d |> mutate(g = mask) |>
    group_by(g) |> summarise(m = sum(muertes), ct = sum(en_riesgo), .groups = "drop")
  poisson.test(c(r$m[r$g], r$m[!r$g]), c(r$ct[r$g], r$ct[!r$g]))$p.value
}
p3 <- p_de(d$tmin <= 3); p5 <- p_de(d$tmin <= 5)
m_f <- sum(d$muertes[d$tmin <= 3]); dias_f <- sum(d$tmin <= 3)
cat("p (≤ 3 °C):", p3, "| p (≤ 5 °C):", p5, "\n")

guardar(
  ggplot(frio, aes(grupo, tasa)) +
    geom_col(fill = verde, width = .6) +
    geom_text(aes(label = paste0(fmtp(tasa), "\n(", muertes, " muertes)")),
              vjust = -.3, size = 4.2, lineheight = .95) +
    scale_y_continuous(expand = expansion(mult = c(0, .3))) +
    labs(title = "Solo los días muy fríos muestran más mortalidad, y es una señal débil",
         subtitle = sprintf("%d muertes en %d días con ≤ 3 °C (p = %s); con el umbral de 5 °C desaparece (p = %s)",
                            m_f, dias_f, fmtp(p3), fmtp(p5)),
         x = "Temperatura mínima del día", y = NULL,
         caption = "Muertes por 1.000 terneros-día en riesgo") +
    tema + sin_ejes + theme(axis.text.x = element_text(size = 13)),
  "mortalidad_vs_tmin_diaria.png", 8, 4.5)

# Nacidos muertos y abortos según frío el día del parto (solo consola)
ab <- a |> group_by(fecha_nacimiento) |>
  summarise(partos = n(), perdidas = sum(!nacido_vivo), .groups = "drop") |>
  left_join(clima, by = c("fecha_nacimiento" = "fecha")) |> filter(!is.na(tmin)) |>
  group_by(frio = tmin <= 5) |>
  summarise(perdidas = sum(perdidas), ok = sum(partos) - sum(perdidas), .groups = "drop")
print(ab); print(fisher.test(as.matrix(ab[, c("perdidas", "ok")])))

# Tabla de clima por año (para el anexo)
clima_anual <- clima |> filter(year(fecha) <= 2025) |>
  group_by(anio = year(fecha)) |>
  summarise(lluvia_mm = sum(precip), dias_lluvia = sum(precip >= 1),
            heladas = sum(tmin <= 0), dias_tmin_5 = sum(tmin <= 5))
write_csv(clima_anual, "reports/tables/clima_anual.csv")



# 4. Causas anotadas (Observaciones avaladas por veterinarios)

vm  <- filter(v, muerte)
obs <- tolower(iconv(replace_na(vm$Observaciones, ""), to = "ASCII//TRANSLIT"))
vm  <- vm |> mutate(causa = case_when(
  grepl("diarre|enteritis|enterotox|e\\. coli|coccid", obs)         ~ "Diarrea / enteritis",
  grepl("neumon", obs)                                              ~ "Neumonía",
  grepl("timpan|intestin|torsion|torcion|abomas|ulcera", obs)       ~ "Timpanismo / intestinal",
  grepl("parto|asist|mellizo|piso|quebrad|fractur|veterinari", obs) ~ "Parto / traumatismo",
  obs == ""                                                         ~ "Sin observación",
  TRUE                                                              ~ "Otra"))

cau <- count(vm, anio, causa)
print(pivot_wider(cau, names_from = anio, values_from = n, values_fill = 0))
print(vm |> group_by(causa) |> summarise(n = n(), mediana_dias = median(dias_guachera)))
write_csv(cau, "reports/tables/causas_por_anio.csv")

# 4a. Diarrea vs el resto, por año
cau3 <- vm |>
  mutate(grupo = case_when(causa == "Diarrea / enteritis" ~ "Diarrea / enteritis",
                           causa == "Sin observación"     ~ "Sin causa anotada",
                           TRUE                           ~ "Otras causas")) |>
  count(anio, grupo) |>
  complete(anio, grupo, fill = list(n = 0)) |>
  mutate(grupo = factor(grupo, c("Sin causa anotada", "Otras causas", "Diarrea / enteritis")))
tot <- count(vm, anio, name = "total")
d25 <- sum(vm$anio == 2025 & vm$causa == "Diarrea / enteritis")
r24 <- sum(vm$anio == 2024 & vm$causa != "Diarrea / enteritis")
r25 <- sum(vm$anio == 2025 & vm$causa != "Diarrea / enteritis")

guardar(
  ggplot(cau3, aes(factor(anio), n)) +
    geom_col(aes(fill = grupo), width = .6) +
    geom_text(aes(label = ifelse(n > 0, n, ""), colour = grupo),
              position = position_stack(vjust = .5), fontface = "bold") +
    geom_text(data = tot, aes(factor(anio), total, label = total),
              vjust = -.4, fontface = "bold", size = 5, inherit.aes = FALSE) +
    scale_fill_manual(values = c("Sin causa anotada" = "#E2D9C8", "Otras causas" = "#B8B0A2",
                                 "Diarrea / enteritis" = rojo)) +
    scale_colour_manual(values = c("Sin causa anotada" = "#5E564E", "Otras causas" = "#24211E",
                                   "Diarrea / enteritis" = "white"), guide = "none") +
    scale_y_continuous(expand = expansion(mult = c(0, .12))) +
    labs(title = sprintf("En 2025, %d de %d muertes fueron por diarrea", d25, sum(vm$anio == 2025)),
         subtitle = sprintf("Las muertes por el resto de las causas (o sin causa anotada) pasaron de %d a %d",
                            r24, r25),
         caption = "Causa anotada por el veterinario en Observaciones. En 2023 casi no se registraba: comparar 2024 con 2025.",
         x = NULL, y = NULL, fill = NULL) +
    tema + sin_ejes + theme(legend.position = "top", axis.text.x = element_text(size = 13)),
  "causas_por_anio.png", 8.5, 4.8)

# 4b. Muertes por mes: diarrea vs otras causas
mes_causa <- vm |>
  mutate(mes   = floor_date(as.Date(fecha_egreso), "month"),
         grupo = if_else(causa == "Diarrea / enteritis", "Diarrea / enteritis",
                         "Otras causas o sin causa anotada")) |>
  count(mes, grupo)
n_brote <- sum(vm$causa == "Diarrea / enteritis" & vm$anio == 2025 &
                 month(vm$fecha_egreso) %in% 4:8)

guardar(
  ggplot(mes_causa, aes(mes, n, fill = grupo)) +
    geom_col(width = 22) +
    scale_fill_manual(values = c("Diarrea / enteritis" = rojo,
                                 "Otras causas o sin causa anotada" = "#B8B0A2")) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y") +
    labs(title = sprintf("%d de las %d muertes por diarrea de 2025 ocurrieron entre abril y agosto",
                         n_brote, d25),
         subtitle = "Muertes por mes, según la causa anotada por el veterinario",
         x = NULL, y = "Muertes", fill = NULL) +
    tema + theme(legend.position = "top"),
  "muertes_mes_causa.png", 9, 4.5)

# 4c. Cuándo mueren? (por semana de vida)
pct30 <- round(100 * mean(vm$dias_guachera <= 30))
sem <- vm |> mutate(sem = pmin(dias_guachera %/% 7 + 1, 10)) |>
  count(sem) |> complete(sem = 1:10, fill = list(n = 0))
n1 <- sem$n[sem$sem == 1]

guardar(
  ggplot(sem, aes(factor(sem), n, fill = sem == 1)) +
    geom_col(width = .75, show.legend = FALSE) +
    geom_text(aes(label = n), vjust = -.4, fontface = "bold") +
    scale_fill_manual(values = c("TRUE" = rojo, "FALSE" = "#B8B0A2")) +
    scale_x_discrete(labels = c(1:9, "10+")) +
    scale_y_continuous(expand = expansion(mult = c(0, .12))) +
    labs(title = sprintf("%d de cada 100 muertes ocurren en el primer mes de vida", pct30),
         subtitle = sprintf("La primera semana concentra %d de las %d muertes", n1, nrow(vm)),
         x = "Semana de vida", y = NULL) +
    tema + sin_ejes,
  "edad_a_la_muerte.png", 8, 4.5)