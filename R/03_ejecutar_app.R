# Abre la app Shiny en el navegador. Ejecutar desde la raíz del proyecto:
#     Rscript R/03_ejecutar_app.R
# (requiere haber corrido antes R/01_analisis_garch.R, que genera output/modelos.rds)
shiny::runApp("app", launch.browser = TRUE)
