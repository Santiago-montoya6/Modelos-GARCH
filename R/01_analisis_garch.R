# =============================================================================
#  Modelos GARCH: aplicación a los rendimientos diarios del S&P 500 (ETF SPY)
#  Seminario de Series de Tiempo I — Universidad de Antioquia
#  Autores: Rafael Alexander Riatiga Durán, Santiago Montoya Vanegas
# -----------------------------------------------------------------------------
#  Ejecutar desde la raíz del proyecto (carpeta garch_spx):
#      Rscript R/01_analisis_garch.R
#  Salidas:
#      output/figuras/*.png   -> figuras para el escrito y las diapositivas
#      output/tablas/*.csv    -> tablas de resultados
#      output/resultados.json -> cifras clave (las usa la app Shiny)
#      output/modelos.rds     -> objetos ajustados
# =============================================================================

suppressPackageStartupMessages({
  library(rugarch)   # especificación, estimación, pronóstico y backtesting GARCH
  library(tseries)   # prueba de Jarque-Bera
  library(FinTS)     # prueba ARCH-LM de Engle
  library(ggplot2)
  library(patchwork)
  library(jsonlite)
})

set.seed(20261110)
options(OutDec = ",")            # coma decimal en los ejes de las figuras
sprintf <- function(fmt, ...) gsub("(\\d)\\.(\\d)", "\\1,\\2", base::sprintf(fmt, ...))
dir.create("output/figuras", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tablas",  recursive = TRUE, showWarnings = FALSE)

# ---- Estilo gráfico común (minimalista, fondo blanco) -----------------------
COL_PRIN <- "#1F3A5F"   # azul oscuro: serie principal
COL_ACEN <- "#C8553D"   # terracota: acento (volatilidad, VaR, excedencias)
COL_GRIS <- "#8A8F98"
tema <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "grey35"),
        axis.title = element_text(color = "grey25"))
theme_set(tema)
guardar <- function(p, nombre, w = 9, h = 4.5) {
  ggsave(file.path("output/figuras", nombre), p, width = w, height = h,
         dpi = 300, bg = "white")
}

# =============================================================================
# 1. Datos y rendimientos
# =============================================================================
# Precios de cierre diarios del ETF SPY (proxy del índice S&P 500), 2015–2025,
# descargados de Twelve Data. Precio no ajustado por dividendos: los saltos
# trimestrales por dividendo (~0,3 %) son pequeños frente a la volatilidad diaria.
datos <- read.csv("data/spy_diario_2015_2025.csv")
datos$date <- as.Date(datos$date)
datos <- datos[order(datos$date), ]

# Rendimiento logarítmico en porcentaje: r_t = 100 * ln(P_t / P_{t-1})
r     <- 100 * diff(log(datos$close))
fecha <- datos$date[-1]
n     <- length(r)
df_r  <- data.frame(fecha = fecha, r = r)

# =============================================================================
# 2. Hechos estilizados
# =============================================================================
asim  <- mean((r - mean(r))^3) / sd(r)^3
curt  <- mean((r - mean(r))^4) / sd(r)^4
jb    <- jarque.bera.test(r)
lb_r  <- Box.test(r,   lag = 10, type = "Ljung-Box")
lb_r2 <- Box.test(r^2, lag = 10, type = "Ljung-Box")
lm_ar <- ArchTest(r, lags = 12)

descr <- data.frame(
  estadistico = c("n", "Media (%)", "Desv. estándar (%)", "Mínimo (%)", "Máximo (%)",
                  "Asimetría", "Curtosis", "Jarque-Bera", "p-valor JB",
                  "Ljung-Box Q(10) r", "p-valor", "Ljung-Box Q(10) r^2", "p-valor",
                  "ARCH-LM(12)", "p-valor"),
  valor = c(n, mean(r), sd(r), min(r), max(r), asim, curt,
            jb$statistic, jb$p.value, lb_r$statistic, lb_r$p.value,
            lb_r2$statistic, lb_r2$p.value, lm_ar$statistic, lm_ar$p.value))
write.csv(descr, "output/tablas/t1_descriptivos.csv", row.names = FALSE)

# Figura 1: precio y rendimientos
p1a <- ggplot(datos, aes(date, close)) + geom_line(color = COL_PRIN, linewidth = .4) +
  labs(title = "Precio de cierre del SPY", x = NULL, y = "USD")
p1b <- ggplot(df_r, aes(fecha, r)) + geom_line(color = COL_PRIN, linewidth = .3) +
  labs(title = "Rendimientos logarítmicos diarios",
       subtitle = "Periodos de calma y de turbulencia se alternan: agrupamiento de volatilidad",
       x = NULL, y = "%")
guardar(p1a / p1b, "f1_precio_rendimientos.png", h = 6.5)

# Figura 2: FAC de r y de r^2
fac <- function(x, titulo) {
  a <- acf(x, lag.max = 30, plot = FALSE)
  d <- data.frame(rezago = a$lag[-1], fac = a$acf[-1])
  b <- 1.96 / sqrt(length(x))
  ggplot(d, aes(rezago, fac)) +
    geom_hline(yintercept = 0, color = "grey60") +
    geom_hline(yintercept = c(-b, b), linetype = "dashed", color = COL_ACEN) +
    geom_segment(aes(xend = rezago, yend = 0), color = COL_PRIN, linewidth = .7) +
    coord_cartesian(ylim = c(-0.15, 0.5)) +
    labs(title = titulo, x = "Rezago", y = "FAC")
}
guardar(fac(r, "FAC de los rendimientos") + fac(r^2, "FAC de los rendimientos al cuadrado"),
        "f2_fac_r_r2.png", h = 3.8)

# Figura 3: histograma frente a la normal
p3 <- ggplot(df_r, aes(r)) +
  geom_histogram(aes(y = after_stat(density)), bins = 120, fill = COL_PRIN, alpha = .8) +
  stat_function(fun = dnorm, args = list(mean = mean(r), sd = sd(r)),
                color = COL_ACEN, linewidth = .8) +
  coord_cartesian(xlim = c(-6, 6)) +
  labs(title = "Distribución de los rendimientos frente a la normal",
       subtitle = sprintf("Curtosis = %.1f (normal = 3): pico alto y colas pesadas", curt),
       x = "%", y = "Densidad")
guardar(p3, "f3_histograma.png", h = 4)

# =============================================================================
# 3. Especificación y selección del modelo
# =============================================================================
# Media constante (ARMA(0,0)): la autocorrelación de r_t es pequeña; la
# estructura relevante está en la varianza. Se comparan órdenes (p,q) y las
# distribuciones normal y t-Student mediante criterios de información.
espec <- function(p, q, dist) {
  ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(q, p)),
             mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
             distribution.model = dist)
}
# Nota: en rugarch garchOrder = c(orden ARCH, orden GARCH) = c(q, p) con la
# notación GARCH(p,q) de Bollerslev, donde p acompaña a sigma^2 y q a eps^2.
grid <- expand.grid(p = 1:2, q = 1:2, dist = c("norm", "std"), stringsAsFactors = FALSE)
grid <- grid[order(grid$dist, grid$p + grid$q), ]   # el (1,1) se ajusta primero
ajustes <- list()
# Los órdenes mayores anidan al GARCH(1,1), pero la verosimilitud es condicional
# a los valores iniciales de la recursión (rugarch inicia en t = max(p,q)+1), así
# que pequeñas diferencias de log-verosimilitud entre órdenes no son comparables
# en sentido estricto. Para descartar óptimos locales se prueban varios puntos de
# partida (incluido el óptimo del (1,1) con parámetros extra cerca de cero) y
# solucionadores, y se conserva el mejor ajuste.
ajustar_mejor <- function(p, q, dist) {
  sp <- espec(p, q, dist)
  inicios <- list(NULL)
  base <- ajustes[[paste0(dist, "_11")]]
  if (!is.null(base) && (p + q) > 2) {
    c0 <- as.list(coef(base))
    if (q == 2) c0$alpha2 <- 0.001
    if (p == 2) c0$beta2  <- 0.001
    inicios <- c(inicios, list(c0))
  }
  mejor <- NULL
  for (s0 in inicios) for (sv in c("hybrid", "solnp")) {
    if (!is.null(s0)) setstart(sp) <- s0
    f <- tryCatch(ugarchfit(sp, data = r, solver = sv), error = function(e) NULL)
    if (!is.null(f) && f@fit$convergence == 0 &&
        (is.null(mejor) || likelihood(f) > likelihood(mejor))) mejor <- f
  }
  mejor
}
sel <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
  g <- grid[i, ]
  fit <- ajustar_mejor(g$p, g$q, g$dist)
  ajustes[[paste0(g$dist, "_", g$p, g$q)]] <<- fit
  ic <- infocriteria(fit)
  data.frame(modelo = sprintf("GARCH(%d,%d)", g$p, g$q),
             distribucion = ifelse(g$dist == "norm", "Normal", "t-Student"),
             logverosimilitud = likelihood(fit),
             AIC = ic[1], BIC = ic[2])
}))
sel <- sel[order(sel$BIC), ]
write.csv(sel, "output/tablas/t2_seleccion_modelos.csv", row.names = FALSE)

fit_n <- ajustes[["norm_11"]]
fit_t <- ajustes[["std_11"]]      # modelo elegido: GARCH(1,1) t-Student

# =============================================================================
# 4. Estimación e interpretación del GARCH(1,1)-t
# =============================================================================
cf  <- coef(fit_t)
ser <- fit_t@fit$robust.matcoef      # errores estándar robustos (QMV)
est <- data.frame(parametro = rownames(ser), estimacion = ser[, 1],
                  ee_robusto = ser[, 2], t = ser[, 3], p_valor = ser[, 4])
write.csv(est, "output/tablas/t3_estimacion_garch11_t.csv", row.names = FALSE)

ser_n <- fit_n@fit$robust.matcoef
est_n <- data.frame(parametro = rownames(ser_n), estimacion = ser_n[, 1],
                    ee_robusto = ser_n[, 2], t = ser_n[, 3], p_valor = ser_n[, 4])
write.csv(est_n, "output/tablas/t3b_estimacion_garch11_normal.csv", row.names = FALSE)

w <- cf["omega"]; a <- cf["alpha1"]; b <- cf["beta1"]; nu <- cf["shape"]
persist   <- a + b
vida_m    <- log(0.5) / log(persist)          # días para disipar la mitad de un choque
var_inc   <- w / (1 - persist)                # varianza incondicional (%^2 diario)
vol_inc_a <- sqrt(252 * var_inc)              # volatilidad anual de largo plazo (%)

# Condiciones de estacionariedad
#  (i)  Segundo orden (Bollerslev, 1986): alpha + beta < 1
#  (ii) Estricta (Nelson, 1990): E[ln(beta + alpha z^2)] < 0, z ~ t estandarizada
z_sim   <- rdist("std", 1e6, mu = 0, sigma = 1, shape = nu)
gamma_e <- mean(log(b + a * z_sim^2))
#  (iii) Cuarto momento finito: (alpha+beta)^2 + (kappa_z - 1) alpha^2 < 1,
#        con kappa_z = E z^4 = 3(nu-2)/(nu-4) para la t estandarizada (nu > 4)
#  Inferencia sobre la persistencia: EE robusto de alpha+beta (método delta lineal)
V_rob   <- vcov(fit_t, robust = TRUE)
ip      <- match(c("alpha1", "beta1"), names(cf))
ee_pers <- sqrt(sum(V_rob[ip, ip]))
z_pers  <- (persist - 1) / ee_pers            # H0: alpha + beta = 1 (IGARCH)
ic_pers <- persist + c(-1, 1) * 1.96 * ee_pers
kappa_z <- if (nu > 4) 3 * (nu - 2) / (nu - 4) else Inf
cond_4  <- persist^2 + (kappa_z - 1) * a^2

estac <- data.frame(
  condicion = c("alpha + beta (segundo orden: < 1)",
                "E[ln(beta + alpha z^2)] (estricta: < 0)",
                "(alpha+beta)^2 + (E z^4 - 1) alpha^2 (cuarto momento: < 1)",
                "Vida media de un choque (días)",
                "Varianza incondicional diaria (%^2)",
                "Volatilidad incondicional anualizada (%)",
                "Volatilidad muestral anualizada (%)",
                "EE robusto de alpha + beta", "IC 95% alpha + beta (inf)", "IC 95% alpha + beta (sup)",
                "z para H0: alpha + beta = 1"),
  valor = c(persist, gamma_e, cond_4, vida_m, var_inc, vol_inc_a, sd(r) * sqrt(252),
            ee_pers, ic_pers, z_pers))
write.csv(estac, "output/tablas/t4_estacionariedad.csv", row.names = FALSE)

# Sensibilidad: con alpha+beta cerca de 1, omega/(1-alpha-beta) es muy inestable.
# "Variance targeting" (Engle y Mezrich, 1996) fija la varianza de largo plazo
# en la varianza muestral y estima solo alpha, beta y nu.
sp_vt <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
                                          variance.targeting = TRUE),
                    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                    distribution.model = "std")
fit_vt <- ugarchfit(sp_vt, data = r, solver = "hybrid")
cf_vt  <- coef(fit_vt)
sens <- data.frame(
  especificacion = c("QMV sin restricción", "Variance targeting"),
  alpha = c(a, cf_vt["alpha1"]), beta = c(b, cf_vt["beta1"]),
  persistencia = c(persist, cf_vt["alpha1"] + cf_vt["beta1"]),
  nu = c(nu, cf_vt["shape"]),
  vol_anual_largo_plazo = c(vol_inc_a, sd(r) * sqrt(252)),
  logverosimilitud = c(likelihood(fit_t), likelihood(fit_vt)))
write.csv(sens, "output/tablas/t4b_sensibilidad_variance_targeting.csv", row.names = FALSE)

# Figura 4: rendimientos absolutos y volatilidad condicional estimada
sig <- as.numeric(sigma(fit_t))
df_s <- data.frame(fecha = fecha, abs_r = abs(r), sigma = sig)
p4 <- ggplot(df_s, aes(fecha)) +
  geom_line(aes(y = abs_r), color = COL_GRIS, linewidth = .25, alpha = .7) +
  geom_line(aes(y = sigma), color = COL_ACEN, linewidth = .6) +
  geom_hline(yintercept = sqrt(var_inc), linetype = "dashed", color = COL_PRIN) +
  labs(title = "Volatilidad condicional estimada, GARCH(1,1)-t",
       subtitle = "Gris: |r|. Rojo: σ estimada. Línea discontinua: volatilidad incondicional implícita",
       x = NULL, y = "%")
guardar(p4, "f4_volatilidad_condicional.png", h = 4)

# =============================================================================
# 5. Diagnóstico de los residuales estandarizados z_t = eps_t / sigma_t
# =============================================================================
z <- as.numeric(residuals(fit_t, standardize = TRUE))
diag_t <- data.frame(
  prueba = c("Ljung-Box Q(10) z", "Ljung-Box Q(10) z^2", "ARCH-LM(12) z",
             "Sesgo de signo (conjunta)"),
  estadistico = NA_real_, p_valor = NA_real_)
lb_z  <- Box.test(z,   lag = 10, type = "Ljung-Box")
lb_z2 <- Box.test(z^2, lag = 10, type = "Ljung-Box", fitdf = 2)
lm_z  <- ArchTest(z, lags = 12)
sb    <- signbias(fit_t)
diag_t$estadistico <- c(lb_z$statistic, lb_z2$statistic, lm_z$statistic, sb["Joint Effect", "t-value"])
diag_t$p_valor     <- c(lb_z$p.value,   lb_z2$p.value,   lm_z$p.value,   sb["Joint Effect", "prob"])
write.csv(diag_t, "output/tablas/t5_diagnostico.csv", row.names = FALSE)
write.csv(data.frame(prueba = rownames(sb), sb, check.names = FALSE),
          "output/tablas/t5b_sesgo_signo.csv", row.names = FALSE)

# Figura 5: QQ de z frente a la t ajustada y FAC de z^2
qq <- data.frame(teor = qdist("std", ppoints(n), mu = 0, sigma = 1, shape = nu),
                 emp = sort(z))
p5a <- ggplot(qq, aes(teor, emp)) +
  geom_abline(color = COL_ACEN) + geom_point(color = COL_PRIN, size = .6, alpha = .6) +
  labs(title = sprintf("QQ de z frente a t-Student (ν = %.1f)", nu),
       x = "Cuantiles teóricos", y = "Cuantiles empíricos")
p5b <- fac(z^2, "FAC de z al cuadrado")
guardar(p5a + p5b, "f5_diagnostico.png", h = 3.8)

# =============================================================================
# 6. Pronóstico de la volatilidad
# =============================================================================
# E_t[sigma^2_{t+k}] = sigma^2 + (alpha+beta)^(k-1) (sigma^2_{t+1} - sigma^2)
H  <- 250
fc <- ugarchforecast(fit_t, n.ahead = H)
s_fc <- as.numeric(sigma(fc))
df_fc <- data.frame(h = 1:H, sigma = s_fc, sigma_anual = s_fc * sqrt(252))
write.csv(df_fc, "output/tablas/t6_pronostico_volatilidad.csv", row.names = FALSE)

# Pronóstico alterno con variance targeting (largo plazo = varianza muestral)
s_vt <- as.numeric(sigma(ugarchforecast(fit_vt, n.ahead = H)))
df_fc$sigma_vt <- s_vt
write.csv(df_fc, "output/tablas/t6_pronostico_volatilidad.csv", row.names = FALSE)

ult <- tail(df_s, 120)
ult$h <- -(nrow(ult) - 1):0
p6 <- ggplot() +
  geom_line(data = ult, aes(h, sigma), color = COL_PRIN, linewidth = .6) +
  geom_line(data = df_fc, aes(h, sigma), color = COL_ACEN, linewidth = .8) +
  geom_line(data = df_fc, aes(h, sigma_vt), color = COL_ACEN, linewidth = .8, linetype = "longdash") +
  geom_hline(yintercept = sqrt(var_inc), linetype = "dotted", color = COL_GRIS) +
  geom_hline(yintercept = sd(r), linetype = "dotted", color = COL_GRIS) +
  geom_vline(xintercept = 0, color = "grey70") +
  annotate("text", x = H, y = sqrt(var_inc), vjust = -.5, hjust = 1, size = 3.3, color = "grey30",
           label = sprintf("σ incondicional QMV = %.2f %%", sqrt(var_inc))) +
  annotate("text", x = H, y = sd(r), vjust = 1.4, hjust = 1, size = 3.3, color = "grey30",
           label = sprintf("σ muestral = %.2f %%", sd(r))) +
  labs(title = "Pronóstico de la volatilidad diaria a h pasos",
       subtitle = sprintf("Origen: %s. Continua: QMV sin restricción (α+β = %.4f). Discontinua: variance targeting",
                          format(tail(fecha, 1), "%d/%m/%Y"), persist),
       x = "Días (negativo: estimada; positivo: pronosticada)", y = "σ (%)") +
  scale_y_continuous(expand = expansion(mult = c(.05, .12)))
guardar(p6, "f6_pronostico.png", h = 4)

# =============================================================================
# 7. Uso práctico: Valor en Riesgo (VaR) con backtesting fuera de muestra
# =============================================================================
# Pronóstico a un paso con ventana móvil; reestimación cada 25 días hábiles.
# Muestra de evaluación: últimos 500 días (≈ 2024–2025).
n_oos <- 500
roll <- ugarchroll(espec(1, 1, "std"), data = r, n.ahead = 1,
                   forecast.length = n_oos, refit.every = 25,
                   refit.window = "moving", window.size = n - n_oos,
                   solver = "hybrid", calculate.VaR = TRUE,
                   VaR.alpha = c(0.01, 0.05))
vr <- as.data.frame(roll, which = "VaR")
real <- vr[, "realized"]
bt <- do.call(rbind, lapply(c(0.01, 0.05), function(al) {
  v  <- vr[, paste0("alpha(", al * 100, "%)")]
  tt <- VaRTest(al, real, v)
  data.frame(nivel = paste0(al * 100, "%"), esperadas = tt$expected.exceed,
             observadas = tt$actual.exceed,
             kupiec_LR = tt$uc.LRstat, kupiec_p = tt$uc.LRp,
             christoffersen_LR = tt$cc.LRstat, christoffersen_p = tt$cc.LRp)
}))
write.csv(bt, "output/tablas/t7_backtesting_var.csv", row.names = FALSE)

df_v <- data.frame(fecha = tail(fecha, n_oos), r = real,
                   var1 = vr[, "alpha(1%)"], var5 = vr[, "alpha(5%)"])
df_v$exced <- df_v$r < df_v$var1
write.csv(df_v, "output/tablas/t7b_var_fuera_muestra.csv", row.names = FALSE)

p7 <- ggplot(df_v, aes(fecha)) +
  geom_col(aes(y = r), fill = COL_GRIS, width = 1, alpha = .7) +
  geom_line(aes(y = var5), color = COL_PRIN, linewidth = .5) +
  geom_line(aes(y = var1), color = COL_ACEN, linewidth = .6) +
  geom_point(data = subset(df_v, exced), aes(y = r), color = COL_ACEN, size = 2) +
  labs(title = "VaR diario fuera de muestra, GARCH(1,1)-t",
       subtitle = sprintf("Azul: VaR 5%%. Rojo: VaR 1%%. Puntos: excedencias del VaR 1%% (%d de %d esperadas)",
                          bt$observadas[1], bt$esperadas[1]),
       x = NULL, y = "%")
guardar(p7, "f7_var_backtesting.png", h = 4)

# =============================================================================
# 8. Cifras clave y objetos para la app Shiny
# =============================================================================
res <- list(
  periodo = c(format(min(fecha)), format(max(fecha))), n = n,
  descriptivos = setNames(as.list(descr$valor), descr$estadistico),
  modelo = "GARCH(1,1) con innovaciones t-Student, media constante",
  coeficientes = as.list(cf),
  persistencia = unname(persist), ee_persistencia = ee_pers,
  ic95_persistencia = unname(ic_pers), z_igarch = unname(z_pers), vida_media_dias = unname(vida_m),
  var_incondicional = unname(var_inc), vol_anual_incondicional = unname(vol_inc_a),
  estricta_E_log = gamma_e, cuarto_momento = unname(cond_4),
  sigma_ultimo = tail(sig, 1), sigma_pronostico_h1 = s_fc[1],
  sigma_pronostico_h250 = s_fc[H],
  vol_anual_muestral = sd(r) * sqrt(252),
  variance_targeting = as.list(cf_vt),
  seleccion = sel,
  backtesting = bt)
write_json(res, "output/resultados.json", auto_unbox = TRUE, pretty = TRUE, digits = 8)
saveRDS(list(datos = df_r, fit_t = fit_t, fit_n = fit_n, roll_var = df_v),
        "output/modelos.rds")

cat("\nListo. Modelo elegido (menor BIC):\n"); print(head(sel, 3))
cat("\nCoeficientes GARCH(1,1)-t:\n"); print(round(cf, 5))
cat(sprintf("\nPersistencia = %.4f | vida media = %.1f días | vol. anual incond. = %.2f %%\n",
            persist, vida_m, vol_inc_a))
