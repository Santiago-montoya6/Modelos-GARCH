# =============================================================================
#  Configuración del entorno virtual de R (renv) — ejecutar UNA sola vez
#  Desde la raíz del proyecto:   Rscript R/00_configurar_entorno.R
# =============================================================================
paquetes <- c("rugarch", "FinTS", "tseries", "ggplot2", "patchwork", "jsonlite",
              "shiny", "bslib", "languageserver")   # languageserver: soporte de R en VS Code

options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")

if (!file.exists("renv.lock")) {
  renv::init(bare = TRUE, restart = FALSE)   # crea la librería aislada del proyecto
  renv::install(paquetes)
  renv::snapshot(packages = paquetes, prompt = FALSE)
} else {
  renv::restore(prompt = FALSE)              # reproduce exactamente las versiones del lock
}
cat("\nEntorno listo. Versiones clave:\n")
for (p in c("rugarch", "ggplot2", "shiny")) cat(sprintf("  %-8s %s\n", p, packageVersion(p)))
