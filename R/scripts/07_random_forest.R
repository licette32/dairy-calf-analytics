# RF

library(dplyr)
library(ggplot2)
library(survival)
library(ranger)

dir.create("reports/figures", recursive = TRUE, showWarnings = FALSE)
source("R/prep_modelo.R")
x <- cargar_modelo()

# Paleta y etiquetas legibles
color_imp_pos <- "#3B6E4C"
color_imp_baj <- "#9AA5B1"

etiquetas_pred <- c(
  "categoria_madre" = "Categoría de madre",
  "sexo"            = "Sexo",
  "raza"            = "Raza",
  "asistido"        = "Tipo de parto (asistido)",
  "peso_nacer_kg"   = "Peso al nacer (kg)",
  "anio"            = "Año",
  "mes"             = "Mes de nacimiento",
  "thi_media_pre7d" = "THI medio (7 días previos)",
  "thi_media_pre21d" = "THI medio (21 días previos)",
  "thi_nac"         = "THI del nacimiento",
  "tmax_media_pre7d" = "Temperatura máxima media (7 días previos)",
  "hum_media_pre7d" = "Humedad media (7 días previos)"
)

limpiar_pred <- function(v) {
  out <- etiquetas_pred[v]
  ifelse(is.na(out), v, unname(out))
}

# Solo variables conocidas al nacimiento o ambientales (sin obs_neumonia/diarrea/etc.)
pred <- c(
  "categoria_madre",
  "sexo",
  "raza",
  "asistido",
  "peso_nacer_kg",
  "anio",
  "mes",
  "thi_media_pre7d",
  "thi_media_pre21d",
  "thi_nac",
  "tmax_media_pre7d",
  "hum_media_pre7d"
)

rf_df <- x |>
  filter(!muerte_dia0) |>
  select(tiempo_surv, muerte_n, all_of(pred)) |>
  tidyr::drop_na()

set.seed(2026)
rf <- ranger(Surv(tiempo_surv, muerte_n) ~ ., data = rf_df, num.trees = 1000,
             min.node.size = 15, importance = "permutation")
cat("C-index OOB del Random Forest:", round(1 - rf$prediction.error, 3), "\n")

imp <- tibble::enframe(rf$variable.importance, "variable", "importancia") |>
  mutate(
    etiqueta = limpiar_pred(variable),
    aporte   = ifelse(importancia > 0.001, "Aporte positivo", "Bajo / nulo")
  )

g_imp <- ggplot(imp, aes(x = importancia, y = reorder(etiqueta, importancia), fill = aporte)) +
  geom_col(width = 0.68) +
  geom_vline(xintercept = 0, linetype = 2, color = "#52606D", linewidth = 0.6) +
  scale_fill_manual(
    values = c("Aporte positivo" = color_imp_pos, "Bajo / nulo" = color_imp_baj),
    name = NULL
  ) +
  labs(
    title = "Importancia de variables en Random Forest de supervivencia",
    subtitle = paste0("Importancia por permutación (C-index OOB = ", round(1 - rf$prediction.error, 3), ")"),
    x = "Importancia por permutación",
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13, color = "#1F2933"),
    plot.subtitle = element_text(size = 10, color = "#52606D"),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )
ggsave("reports/figures/rf_importancia.png", g_imp, width = 8, height = 5.2, dpi = 220)

# Validación cruzada 5 folds: Cox vs Random Forest
set.seed(2026)
folds <- sample(rep(1:5, length.out = nrow(rf_df)))
cv <- t(sapply(1:5, function(k) {
  tr <- rf_df[folds != k, ]; te <- rf_df[folds == k, ]
  ct <- te$tiempo_surv; ce <- te$muerte_n
  m_cox <- coxph(Surv(tiempo_surv, muerte_n) ~
    categoria_madre + asistido + peso_nacer_kg +
    anio + thi_media_pre7d + thi_nac, data = tr)
  lp  <- predict(m_cox, newdata = te, type = "lp")
  m_rf <- ranger(Surv(tiempo_surv, muerte_n) ~ ., data = tr, num.trees = 500, min.node.size = 15)
  chf <- rowSums(predict(m_rf, data = te)$chf)
  c(cox = concordance(Surv(ct, ce) ~ lp,  reverse = TRUE)$concordance,
    rf  = concordance(Surv(ct, ce) ~ chf, reverse = TRUE)$concordance)
}))
print(round(cv, 3))
cat("C-index medio (5-fold): Cox =", round(mean(cv[, "cox"]), 3),
    "| RF =", round(mean(cv[, "rf"]), 3), "\n")