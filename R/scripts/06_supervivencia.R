# Supervivencia

library(dplyr); library(readr); library(tibble); library(ggplot2)
library(survival); library(survminer)


for (d in c("reports/figures", "reports/tables", "data/clean/powerbi"))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
source("R/prep_modelo.R")
x <- cargar_modelo()
cat("Terneros:", nrow(x), "| muertes:", sum(x$muerte_n), "\n")



# 1. Kaplan-Meier global

km <- survfit(Surv(tiempo_surv, muerte_n) ~ 1, data = x)
print(summary(km, times = c(7, 14, 30, 60, 90)))   # 1 - survival = mortalidad acumulada

guardar <- function(g, archivo) {
  png(archivo, 2200, 1700, res = 220); print(g); dev.off()
}

f_cat  <- survfit(Surv(tiempo_surv, muerte_n) ~ categoria_madre, data = x)
f_asis <- survfit(Surv(tiempo_surv, muerte_n) ~ asistido,        data = x)
f_anio <- survfit(Surv(tiempo_surv, muerte_n) ~ anio,            data = x)
f_peso <- survfit(Surv(tiempo_surv, muerte_n) ~ peso_cat,        data = x)
args <- list(data = x, pval = TRUE, risk.table = TRUE, xlim = c(0, 100),
             break.time.by = 20, ylim = c(0.8, 1), xlab = "Días desde el nacimiento")
guardar(do.call(ggsurvplot, c(list(f_cat),  args)), "reports/figures/km_categoria_madre.png")
guardar(do.call(ggsurvplot, c(list(f_asis), args)), "reports/figures/km_tipo_parto.png")
guardar(do.call(ggsurvplot, c(list(f_anio), args)), "reports/figures/km_anio.png")
guardar(do.call(ggsurvplot, c(list(f_peso), args)), "reports/figures/km_peso.png")



# 2. ¿Cuándo se concentra el riesgo? Tasa por tramo de edad

tramos    <- c(0, 7, 14, 30, 60, Inf)
etiquetas <- c("0-7 d", "8-14 d", "15-30 d", "31-60 d", ">60 d")
riesgo <- bind_rows(lapply(seq_along(etiquetas), function(i) {
  a <- tramos[i]; b <- tramos[i + 1]
  tibble(tramo = etiquetas[i],
         dias_riesgo = sum(pmax(0, pmin(x$tiempo_surv, b) - a)),
         muertes     = sum(x$muerte_n == 1 & x$tiempo_surv > a & x$tiempo_surv <= b))
})) |>
  rowwise() |>
  mutate(tasa_x1000 = 1000 * muertes / dias_riesgo,
         ic_inf = 1000 * poisson.test(muertes, dias_riesgo)$conf.int[1],
         ic_sup = 1000 * poisson.test(muertes, dias_riesgo)$conf.int[2]) |>
  ungroup() |> mutate(tramo = factor(tramo, levels = etiquetas))

print(riesgo)
write_csv(riesgo, "reports/tables/riesgo_por_tramo.csv")
write_csv(riesgo, "data/clean/powerbi/riesgo_por_tramo.csv")
ggplot(riesgo, aes(tramo, tasa_x1000)) + geom_col(fill = "grey70") +
  geom_errorbar(aes(ymin = ic_inf, ymax = ic_sup), width = 0.2) +
  labs(x = "Edad", y = "Muertes por 1.000 terneros-día") + theme_minimal()
ggsave("reports/figures/riesgo_por_tramo.png", width = 7, height = 4.5, dpi = 200)



# 3. Cox

hr_tabla <- function(m, nombre) {
  s <- summary(m)
  tibble(modelo = nombre, termino = rownames(s$conf.int),
         HR = s$conf.int[, 1], IC_inf = s$conf.int[, 3], IC_sup = s$conf.int[, 4],
         p = s$coefficients[, ncol(s$coefficients)])
}

forest <- function(t, titulo, archivo) {
  ggplot(t, aes(HR, reorder(termino, HR))) +
    geom_vline(xintercept = 1, linetype = 2) +
    geom_pointrange(aes(xmin = IC_inf, xmax = IC_sup)) +
    scale_x_log10() + labs(x = "Hazard ratio (IC 95%, escala log)", y = NULL, title = titulo) +
    theme_minimal()
  ggsave(archivo, width = 8, height = 5, dpi = 200)
}

cl <- "+ cluster(codigo_madre)"   # errores robustos agrupados por madre


# a. Univariable
uni_vars <- c("categoria_madre", "sexo", "raza", "asistido", "peso5", "anio",
              "tmin5_7d", "lluvia10_7d", "frio10_7d", "tmin5_21d")
univ <- bind_rows(lapply(uni_vars, function(nm)
  hr_tabla(coxph(as.formula(paste("Surv(tiempo_surv, muerte_n) ~", nm, cl)), data = x), nm)))
print(univ, n = Inf)


# b. Modelo A: animal, parto y año
fA <- Surv(tiempo_surv, muerte_n) ~ categoria_madre + asistido + peso5 + sexo + raza + anio
cox_A <- coxph(update(fA, paste(". ~ . ", cl)), data = x)
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

tabla_hr <- bind_rows(hr_tabla(cox_A, "A: animal+parto+año"),
                      hr_tabla(cox_B, "B: A + clima 7 d"),
                      hr_tabla(cox_B0, "B sin muertes día 0"))

print(tabla_hr, n = Inf)
write_csv(tabla_hr, "reports/tables/cox_modelos.csv")
write_csv(tabla_hr, "data/clean/powerbi/cox_modelos.csv")
forest(filter(tabla_hr, modelo == "A: animal+parto+año"), "Modelo A", "reports/figures/forest_A.png")
forest(filter(tabla_hr, modelo == "B: A + clima 7 d"),    "Modelo B", "reports/figures/forest_B.png")