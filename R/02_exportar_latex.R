# =============================================================================
#  Exporta tablas y cifras del análisis al escrito LaTeX (escrito/)
#  Ejecutar después de 01_analisis_garch.R:   Rscript R/02_exportar_latex.R
#  Así toda cifra del documento proviene directamente de los resultados.
# =============================================================================
library(jsonlite)

dir.create("escrito/tablas",  recursive = TRUE, showWarnings = FALSE)
dir.create("escrito/figuras", recursive = TRUE, showWarnings = FALSE)
invisible(file.copy(list.files("output/figuras", full.names = TRUE), "escrito/figuras", overwrite = TRUE))

tb  <- function(f) read.csv(file.path("output/tablas", f), check.names = FALSE)
res <- fromJSON("output/resultados.json")

# Formato numérico con coma decimal ("{,}" funciona en modo texto y matemático)
num <- function(x, d = 3) {
  s <- formatC(x, format = "f", digits = d, big.mark = "")
  gsub("\\.", "{,}", s)
}
pval <- function(p) ifelse(p < 0.001, "$<0{,}001$", num(p, 3))

# ---- Macros con las cifras ---------------------------------------------------
d   <- tb("t1_descriptivos.csv")
t3  <- tb("t3_estimacion_garch11_t.csv");  rownames(t3) <- t3$parametro
t3n <- tb("t3b_estimacion_garch11_normal.csv"); rownames(t3n) <- t3n$parametro
t4  <- tb("t4_estacionariedad.csv")
t4b <- tb("t4b_sensibilidad_variance_targeting.csv")
t5  <- tb("t5_diagnostico.csv")
t5b <- tb("t5b_sesgo_signo.csv")
t6  <- tb("t6_pronostico_volatilidad.csv")
t7  <- tb("t7_backtesting_var.csv")
t2  <- tb("t2_seleccion_modelos.csv")
vr  <- tb("t7b_var_fuera_muestra.csv")

fecha_es <- function(x) {
  m <- c("enero","febrero","marzo","abril","mayo","junio","julio","agosto",
         "septiembre","octubre","noviembre","diciembre")
  x <- as.Date(x); sprintf("%d de %s de %s", as.integer(format(x, "%d")),
                           m[as.integer(format(x, "%m"))], format(x, "%Y"))
}

ll_t <- t2$logverosimilitud[t2$modelo == "GARCH(1,1)" & t2$distribucion == "t-Student"]
ll_n <- t2$logverosimilitud[t2$modelo == "GARCH(1,1)" & t2$distribucion == "Normal"]

m <- c(
  gN = sub("(\\d)(\\d{3})$", "\\1\\\\,\\2", res$n),
  gFechaIni = fecha_es(res$periodo[1]), gFechaFin = fecha_es(res$periodo[2]),
  gOosIni = fecha_es(min(vr$fecha)), gOosN = nrow(vr),
  gMedia = num(d$valor[2], 3), gDE = num(d$valor[3], 3),
  gMin = num(d$valor[4], 2), gMax = num(d$valor[5], 2),
  gAsim = num(d$valor[6], 2), gCurt = num(d$valor[7], 1),
  gJB = sub("(\\d)(\\d{3})$", "\\1\\\\,\\2", num(d$valor[8], 0)), gLBr = num(d$valor[10], 1), gLBrr = num(d$valor[12], 0),
  gARCHLM = num(d$valor[14], 1),
  gMu = num(t3["mu", 2], 4), gOmega = num(t3["omega", 2], 4),
  gAlpha = num(t3["alpha1", 2], 3), gBeta = num(t3["beta1", 2], 3),
  gNu = num(t3["shape", 2], 2),
  gSEmu = num(t3["mu", 3], 4), gSEomega = num(t3["omega", 3], 4),
  gSEalpha = num(t3["alpha1", 3], 3), gSEbeta = num(t3["beta1", 3], 3),
  gSEnu = num(t3["shape", 3], 2),
  gNalpha = num(t3n["alpha1", 2], 3), gNbeta = num(t3n["beta1", 2], 3),
  gLLt = num(ll_t, 1), gLLn = num(ll_n, 1), gDLL = num(ll_t - ll_n, 1),
  gPersist = num(res$persistencia, 4), gVidaMedia = num(res$vida_media_dias, 0),
  gVarInc = num(res$var_incondicional, 2), gSigInc = num(sqrt(res$var_incondicional), 2),
  gVolInc = num(res$vol_anual_incondicional, 1), gVolMuestral = num(res$vol_anual_muestral, 1),
  gEstricta = num(res$estricta_E_log, 3), gCuarto = num(res$cuarto_momento, 3),
  gKappaZ = num(3 * (t3["shape", 2] - 2) / (t3["shape", 2] - 4), 2),
  gVTalpha = num(t4b$alpha[2], 3), gVTbeta = num(t4b$beta[2], 3),
  gVTpersist = num(t4b$persistencia[2], 3), gVTnu = num(t4b$nu[2], 2),
  gVTll = num(t4b$logverosimilitud[2], 1),
  gSigmaUltimo = num(res$sigma_ultimo, 2),
  gSigmaUno = num(t6$sigma[1], 2), gSigmaUnoAnual = num(t6$sigma_anual[1], 1),
  gSigmaH = num(t6$sigma[nrow(t6)], 2), gSigmaHvt = num(t6$sigma_vt[nrow(t6)], 2),
  gH = nrow(t6),
  gSEpers = num(t4$valor[8], 3), gICinf = num(t4$valor[9], 3), gICsup = num(t4$valor[10], 3),
  gZpers = num(t4$valor[11], 2),
  gLBz = num(t5$estadistico[1], 2), gLBzp = num(t5$p_valor[1], 2),
  gLBzz = num(t5$estadistico[2], 2), gLBzzp = num(t5$p_valor[2], 2),
  gLMz = num(t5$estadistico[3], 2), gLMzp = num(t5$p_valor[3], 2),
  gSB = num(t5$estadistico[4], 2), gSBp = pval(t5$p_valor[4]),
  gSBt = num(t5b$`t-value`[1], 2), gSBtp = num(t5b$prob[1], 3),
  gExcUno = t7$observadas[1], gEspUno = t7$esperadas[1],
  gKupUno = num(t7$kupiec_LR[1], 2), gKupUnop = num(t7$kupiec_p[1], 3),
  gChrUno = num(t7$christoffersen_LR[1], 2), gChrUnop = num(t7$christoffersen_p[1], 3),
  gExcCinco = t7$observadas[2], gEspCinco = t7$esperadas[2],
  gKupCinco = num(t7$kupiec_LR[2], 2), gKupCincop = num(t7$kupiec_p[2], 3),
  gChrCinco = num(t7$christoffersen_LR[2], 2), gChrCincop = num(t7$christoffersen_p[2], 3),
  gRugarch = as.character(packageVersion("rugarch")),
  gRversion = paste(R.version$major, R.version$minor, sep = ".")
)
writeLines(c("% Generado automáticamente por R/02_exportar_latex.R. No editar a mano.",
             sprintf("\\newcommand{\\%s}{%s}", names(m), m)),
           "escrito/tablas/cifras.tex", useBytes = TRUE)

# ---- Tabla 1: estadísticos descriptivos y pruebas ---------------------------
t1 <- c(
  "\\begin{tabular}{lr@{\\hspace{2.2em}}lrr}", "\\toprule",
  "Estadístico & Valor & Prueba & Estadístico & $p$-valor \\\\", "\\midrule",
  sprintf("Media (\\%%) & %s & Jarque-Bera & %s & %s \\\\", num(d$valor[2], 3), num(d$valor[8], 1), pval(d$valor[9])),
  sprintf("Desv. estándar (\\%%) & %s & Ljung-Box $Q(10)$, $r_t$ & %s & %s \\\\", num(d$valor[3], 3), num(d$valor[10], 1), pval(d$valor[11])),
  sprintf("Mínimo (\\%%) & %s & Ljung-Box $Q(10)$, $r_t^2$ & %s & %s \\\\", num(d$valor[4], 2), num(d$valor[12], 1), pval(d$valor[13])),
  sprintf("Máximo (\\%%) & %s & ARCH-LM(12) & %s & %s \\\\", num(d$valor[5], 2), num(d$valor[14], 1), pval(d$valor[15])),
  sprintf("Asimetría & %s & & & \\\\", num(d$valor[6], 2)),
  sprintf("Curtosis & %s & & & \\\\", num(d$valor[7], 2)),
  "\\bottomrule", "\\end{tabular}")
writeLines(t1, "escrito/tablas/t1_descriptivos.tex", useBytes = TRUE)

# ---- Tabla 2: selección de modelos ------------------------------------------
t2o <- t2[order(t2$distribucion, t2$modelo), ]
best <- which.min(t2o$BIC)
filas <- sprintf("%s & %s & %s & %s & %s \\\\", t2o$modelo, t2o$distribucion,
                 num(t2o$logverosimilitud, 2), num(t2o$AIC, 4),
                 ifelse(seq_len(nrow(t2o)) == best, paste0("\\textbf{", num(t2o$BIC, 4), "}"), num(t2o$BIC, 4)))
filas <- append(filas, "\\midrule", after = 4)
writeLines(c("\\begin{tabular}{llrrr}", "\\toprule",
             "Modelo & Distribución & $\\ln L$ & AIC & BIC \\\\", "\\midrule",
             filas, "\\bottomrule", "\\end{tabular}"),
           "escrito/tablas/t2_seleccion.tex", useBytes = TRUE)

# ---- Tabla 3: estimaciones t-Student y normal --------------------------------
nom <- c(mu = "$\\mu$", omega = "$\\omega$", alpha1 = "$\\alpha_1$", beta1 = "$\\beta_1$", shape = "$\\nu$")
dg  <- c(mu = 4, omega = 4, alpha1 = 3, beta1 = 3, shape = 2)
f3 <- sapply(names(nom), function(k) {
  tt <- sprintf("%s & (%s)", num(t3[k, 2], dg[k]), num(t3[k, 3], dg[k]))
  nn <- if (k %in% rownames(t3n)) sprintf("%s & (%s)", num(t3n[k, 2], dg[k]), num(t3n[k, 3], dg[k])) else "-- & "
  sprintf("%s & %s & %s \\\\", nom[k], tt, nn)
})
writeLines(c("\\begin{tabular}{lr@{\\,}lr@{\\,}l}", "\\toprule",
             " & \\multicolumn{2}{c}{$t$-Student} & \\multicolumn{2}{c}{Normal} \\\\",
             "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
             "Parámetro & Estimación & (EE) & Estimación & (EE) \\\\", "\\midrule", f3,
             "\\midrule",
             sprintf("$\\alpha_1+\\beta_1$ & %s & & %s & \\\\", num(t3["alpha1", 2] + t3["beta1", 2], 4),
                     num(t3n["alpha1", 2] + t3n["beta1", 2], 4)),
             sprintf("$\\ln L$ & %s & & %s & \\\\", num(ll_t, 2), num(ll_n, 2)),
             "\\bottomrule", "\\end{tabular}"),
           "escrito/tablas/t3_estimacion.tex", useBytes = TRUE)

# ---- Tabla 4: condiciones de estacionariedad y momentos ---------------------
cumple <- function(ok) ifelse(ok, "Sí", "No")
writeLines(c("\\begin{tabular}{lrc}", "\\toprule",
             "Condición & Valor & ¿Se cumple? \\\\", "\\midrule",
             sprintf("Segundo orden: $\\alpha_1+\\beta_1<1$ & %s & %s \\\\", num(t4$valor[1], 4), cumple(t4$valor[1] < 1)),
             sprintf("Estricta: $\\mathrm{E}\\ln(\\beta_1+\\alpha_1 z_t^2)<0$ & %s & %s \\\\", num(t4$valor[2], 3), cumple(t4$valor[2] < 0)),
             sprintf("Cuarto momento: $(\\alpha_1+\\beta_1)^2+(\\kappa_z-1)\\alpha_1^2<1$ & %s & %s \\\\", num(t4$valor[3], 3), cumple(t4$valor[3] < 1)),
             "\\midrule",
             sprintf("Vida media de un choque (días) & %s & \\\\", num(t4$valor[4], 0)),
             sprintf("Volatilidad anual de largo plazo implícita (\\%%) & %s & \\\\", num(t4$valor[6], 1)),
             sprintf("Volatilidad anual muestral (\\%%) & %s & \\\\", num(t4$valor[7], 1)),
             "\\bottomrule", "\\end{tabular}"),
           "escrito/tablas/t4_estacionariedad.tex", useBytes = TRUE)

# ---- Tabla 5: diagnóstico ---------------------------------------------------
writeLines(c("\\begin{tabular}{lrr}", "\\toprule",
             "Prueba sobre $\\hat z_t$ & Estadístico & $p$-valor \\\\", "\\midrule",
             sprintf("Ljung-Box $Q(10)$, $\\hat z_t$ & %s & %s \\\\", num(t5$estadistico[1], 2), pval(t5$p_valor[1])),
             sprintf("Ljung-Box $Q(10)$, $\\hat z_t^2$ & %s & %s \\\\", num(t5$estadistico[2], 2), pval(t5$p_valor[2])),
             sprintf("ARCH-LM(12) & %s & %s \\\\", num(t5$estadistico[3], 2), pval(t5$p_valor[3])),
             sprintf("Sesgo de signo (Engle-Ng), conjunta & %s & %s \\\\", num(t5$estadistico[4], 2), pval(t5$p_valor[4])),
             "\\bottomrule", "\\end{tabular}"),
           "escrito/tablas/t5_diagnostico.tex", useBytes = TRUE)

# ---- Tabla 6: backtesting del VaR -------------------------------------------
writeLines(c("\\begin{tabular}{lrrrrrr}", "\\toprule",
             " & \\multicolumn{2}{c}{Excedencias} & \\multicolumn{2}{c}{Kupiec} & \\multicolumn{2}{c}{Christoffersen} \\\\",
             "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\\cmidrule(lr){6-7}",
             "Nivel & Esperadas & Observadas & LR & $p$-valor & LR & $p$-valor \\\\", "\\midrule",
             sprintf("%s & %d & %d & %s & %s & %s & %s \\\\", gsub("%", "\\\\%", t7$nivel), t7$esperadas, t7$observadas,
                     num(t7$kupiec_LR, 2), num(t7$kupiec_p, 3), num(t7$christoffersen_LR, 2), num(t7$christoffersen_p, 3)),
             "\\bottomrule", "\\end{tabular}"),
           "escrito/tablas/t6_var.tex", useBytes = TRUE)

cat("Tablas y cifras exportadas a escrito/tablas/\n")
