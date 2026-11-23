<div align="center">

# Dairy Calf Analytics

### Mortalidad de terneros en guachera: supervivencia y factores de riesgo

![R](https://img.shields.io/badge/R-%E2%89%A5%204.3-276DC3?style=flat-square&logo=r&logoColor=white)
![Quarto](https://img.shields.io/badge/Quarto-Informe%20%26%20Dashboard-75AADB?style=flat-square)
![Licencia](https://img.shields.io/badge/licencia-MIT-3FAF8F?style=flat-square)

Trabajo final de la Diplomatura en Analítica de Datos e Inteligencia de Negocios para el Agro.

</div>

---

## De qué trata

Análisis de la mortalidad de terneros en la guachera de un tambo lechero comercial, combinando registros de crianza con clima diario público.

**Pregunta:** ¿qué factores se asocian con la mortalidad y en qué momento de la vida se concentra el riesgo?

**Informe:** `informe.qmd` · **Tablero de seguimiento:** `dashboard.qmd`

## Enfoque

- Supervivencia (Kaplan-Meier, regresión de Cox) y tasas por etapa de edad.
- Tasa de mortalidad mensual con gráfico de control y series de tiempo.
- Random Forest de supervivencia comparado con Cox.
- Pipeline reproducible de validación, anonimización y limpieza.

## Hallazgos principales

- *(completar con 3 o 4 líneas cualitativas)*

El detalle, los gráficos y la discusión están en el informe.

## Datos y privacidad

Los registros de la guachera son privados y están anonimizados; no se incluyen en el repositorio. El clima proviene de [Open-Meteo](https://open-meteo.com/en/docs/historical-weather-api).

## Estructura

```text
calf-analytics/
├── R/
│   ├── prep_modelo.R
│   ├── validaciones.R
│   └── scripts/          # pipeline
├── data/                 # no versionada
├── reports/
│   ├── figures/
│   └── tables/
├── tests/
├── informe.qmd
├── dashboard.qmd
└── .gitignore
```

## Cómo reproducir

Requiere R y [Quarto](https://quarto.org) (≥ 1.4). Los scripts de `R/scripts/` se corren en orden numérico desde la raíz del proyecto, y los documentos se generan con:

```bash
quarto render informe.qmd
quarto render dashboard.qmd
```

Como los datos reales son privados, la reproducción completa requiere el archivo original.

## Limitaciones

- Datos observacionales de un solo establecimiento: muestran asociaciones, no causalidad.
- No hay registros sistemáticos de calostro, sanidad individual ni causa de muerte.

## Autora y licencia

**Beverly J. Licette Gonzalez Casanova** ([@licette32](https://github.com/licette32)) · Licencia MIT.