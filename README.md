# Dairy Calf Analytics

**Análisis de supervivencia de terneros de guachera en R: qué factores se asocian con la mortalidad y cuándo se concentra el riesgo.**

![R](https://img.shields.io/badge/R-%E2%89%A5%204.3-276DC3?style=flat-square&logo=r&logoColor=white)
![Estado](https://img.shields.io/badge/estado-en%20desarrollo-E6B980?style=flat-square)
![Licencia](https://img.shields.io/badge/licencia-MIT-3FAF8F?style=flat-square)

Trabajo final de la Diplomatura en Análisis de Datos para el Agro.

---

## Resumen

Análisis de datos reales de una guachera lechera (más de 800 terneros) para identificar **qué factores se asocian con la mortalidad neonatal y en qué momento de la vida se concentra el riesgo**.
Cruza registros de nacimiento con clima público y aplica **análisis de supervivencia (Kaplan-Meier y regresión de Cox) en R**.
Pipeline reproducible con datos sintéticos y tablero de seguimiento (en desarrollo).
Los datos originales son privados y están anonimizados; no se incluyen en el repositorio.

---

## Preguntas que responde

- ¿En qué período después del nacimiento se concentra el riesgo de muerte?
- ¿Influyen el tipo de parto, la categoría de la madre (vaquillona o vaca), el peso al nacer, el sexo o la raza?
- ¿Se asocian el frío o la lluvia de las primeras semanas con más mortalidad?

---

## Datos

| Fuente | Qué aporta | Acceso |
|--------|------------|--------|
| Registros de la guachera | Nacimiento, sexo, raza, peso, tipo de parto, madre, fechas, desenlace | Privado, anonimizado, no incluido |
| [Open-Meteo](https://open-meteo.com/en/docs/historical-weather-api) | Temperatura, lluvia y humedad diarias | Público |

El repositorio incluirá un **dataset sintético con el mismo esquema** para poder
ejecutar todo el análisis sin los datos reales.

---

## Enfoque

- **Limpieza y validación** de los registros, con pruebas automáticas.
- **Supervivencia:** curvas de Kaplan-Meier, log-rank y modelos de Cox, con errores agrupados por madre.
- **Eventos poco frecuentes:** regresión logística penalizada (Firth) como control de robustez.
- **Clima:** exposición en los primeros 7 y 21 días de vida.
- **Reporte** con intervalos de confianza y tablero de seguimiento.

---

## Estructura

```
dairy-calf-analytics/
├── data/          # raw (privado, en .gitignore), synthetic, external
├── R/             # funciones del análisis
├── scripts/       # pipeline
├── tests/         # validación de datos
├── reports/       # informe en Quarto
├── dashboard/     # tablero (Power BI)
└── renv.lock      # versiones fijadas
```

---

## Estado

- [x] Pregunta y alcance definidos
- [ ] Limpieza y validación
- [ ] Integración de clima
- [ ] Análisis descriptivo y de supervivencia
- [ ] Dataset sintético y pruebas
- [ ] Tablero
- [ ] Informe final

---

## Limitaciones

Un solo establecimiento, datos observacionales (se hablan de asociaciones, no
de causas) y pocos eventos, lo que limita la complejidad de los modelos.
Detalle completo en el informe.

---

## Herramientas

R · survival · tidyverse · renv · Quarto · testthat · Power BI

## Autora

**Beverly Gonzalez Casanova** · [@licette32](https://github.com/licette32) · Código bajo licencia MIT