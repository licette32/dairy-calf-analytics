# RF

library(dplyr); library(ggplot2); library(survival); library(ranger)

dir.create("reports/figures", recursive = TRUE, showWarnings = FALSE)
source("R/prep_modelo.R")
x <- cargar_modelo()



# Solo variables conocidas al nacimiento o ambientales (sin obs_neumonia/diarrea/etc.)
pred <- c("categoria_madre", "sexo", "raza", "asistido", "peso_nacer_kg", "anio", "mes",
          "tmin_media_7d", "prop_lluvia_7d", "prop_frio_7d", "hum_media_7d",
          "tmin_media_21d", "prop_lluvia_21d", "prop_frio_21d")
rf_df <- x |> select(tiempo_surv, muerte_n, all_of(pred)) |> tidyr::drop_na()

set.seed(2026)
rf <- ranger(Surv(tiempo_surv, muerte_n) ~ ., data = rf_df, num.trees = 1000,
             min.node.size = 15, importance = "permutation")
cat("C-index OOB del Random Forest:", round(1 - rf$prediction.error, 3), "\n")

imp <- tibble::enframe(rf$variable.importance, "variable", "importancia")
ggplot(imp, aes(importancia, reorder(variable, importancia))) + geom_col(fill = "grey60") +
  labs(x = "Importancia por permutación", y = NULL) + theme_minimal()
ggsave("reports/figures/rf_importancia.png", width = 7, height = 5, dpi = 200)


# Validación cruzada 5 folds: Cox vs Random Forest
set.seed(2026)
folds <- sample(rep(1:5, length.out = nrow(rf_df)))
cv <- t(sapply(1:5, function(k) {
  tr <- rf_df[folds != k, ]; te <- rf_df[folds == k, ]
  ct <- te$tiempo_surv; ce <- te$muerte_n
  m_cox <- coxph(Surv(tiempo_surv, muerte_n) ~ categoria_madre + asistido + peso_nacer_kg +
                   anio + tmin_media_7d + prop_lluvia_7d, data = tr)
  lp  <- predict(m_cox, newdata = te, type = "lp")
  m_rf <- ranger(Surv(tiempo_surv, muerte_n) ~ ., data = tr, num.trees = 500, min.node.size = 15)
  chf <- rowSums(predict(m_rf, data = te)$chf)
  c(cox = concordance(Surv(ct, ce) ~ lp,  reverse = TRUE)$concordance,
    rf  = concordance(Surv(ct, ce) ~ chf, reverse = TRUE)$concordance)
}))
print(round(cv, 3))
cat("C-index medio (5-fold): Cox =", round(mean(cv[, "cox"]), 3),
    "| RF =", round(mean(cv[, "rf"]), 3), "\n")