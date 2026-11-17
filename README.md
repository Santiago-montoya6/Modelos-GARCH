# Modelos GARCH: aplicación al S&P 500

Seminario de Series de Tiempo I, Universidad de Antioquia.

## Estructura

```
garch_spx/
├── data/spy_diario_2015_2025.csv     precios diarios del ETF SPY (Twelve Data)
├── R/00_configurar_entorno.R         crea el entorno renv (una sola vez)
├── R/01_analisis_garch.R             análisis completo → output/
├── R/02_exportar_latex.R             pasa tablas y cifras al escrito → escrito/tablas/
├── R/03_ejecutar_app.R               abre la app Shiny
├── app/app.R                         app Shiny interactiva
├── escrito/Seminario_GARCH.tex       documento escrito (formato del seminario)
├── escrito/Seminario_GARCH.pdf
├── presentacion/Presentacion_GARCH.pptx   diapositivas (18, para 25 min)
├── presentacion/crear_presentacion.js     regenera el .pptx desde los resultados (Node + pptxgenjs)
├── output/figuras/  output/tablas/   resultados (PNG 300 dpi y CSV)
└── .vscode/settings.json
```

## Uso en Visual Studio Code

1. Instalar R (≥ 4.3), la extensión **R** (REditorSupport) y, para el escrito, una distribución
   LaTeX (MiKTeX o TeX Live) con la extensión **LaTeX Workshop**.
2. Abrir la carpeta `garch_spx` con *File → Open Folder*.
3. En la terminal de VS Code, desde la raíz del proyecto:
   ```bash
   Rscript R/00_configurar_entorno.R     # una vez: crea renv/ y renv.lock
   Rscript R/01_analisis_garch.R         # análisis, figuras y tablas (~15 s)
   Rscript R/02_exportar_latex.R         # actualiza cifras y tablas del escrito
   Rscript R/03_ejecutar_app.R           # abre la app en el navegador
   ```
4. Compilar `escrito/Seminario_GARCH.tex` con pdfLaTeX (LaTeX Workshop lo hace al guardar).
   Todas las cifras del texto vienen de `escrito/tablas/cifras.tex`, generado por R.

## App Shiny

| Pestaña | Para qué sirve |
|---|---|
| 1 · Hechos estilizados | FAC de r y r², curtosis, pruebas; elegir el periodo |
| 2 · Simulador GARCH(1,1) | mover ω, α, β y ν y ver la serie, la persistencia y si se cumplen las condiciones de estacionariedad |
| 3 · Estimación | reestimar con otra muestra, orden, distribución o *variance targeting*, con diagnóstico |
| 4 · Pronóstico y VaR | pronóstico de σ, VaR de mañana en % y en dólares, *backtesting* fuera de muestra |

Nota: el SPY se usa como proxy del índice S&P 500. Los precios no están ajustados por dividendos; el
efecto (≈0,3 % por trimestre) es despreciable frente a la volatilidad diaria (≈1,1 %).
