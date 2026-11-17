# =============================================================================
#  App Shiny: Modelos GARCH y volatilidad del S&P 500 (ETF SPY)
#  Seminario de Series de Tiempo I — Universidad de Antioquia
#  Ejecutar desde la raíz del proyecto (después de R/01_analisis_garch.R):
#      Rscript -e 'shiny::runApp("app", launch.browser = TRUE)'
# =============================================================================
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(ggplot2); library(rugarch); library(FinTS)
})
# Coma decimal solo en las gráficas (un OutDec global rompe los deslizadores de Shiny)
renderPlotC <- function(expr) {
  q <- substitute(expr); env <- parent.frame()
  renderPlot({ op <- options(OutDec = ","); on.exit(options(op), add = TRUE); print(eval(q, env)) })
}

# ---- Datos y modelo base ----------------------------------------------------
raiz <- if (file.exists("data")) "." else ".."
precios <- read.csv(file.path(raiz, "data/spy_diario_2015_2025.csv"))
precios$date <- as.Date(precios$date)
precios <- precios[order(precios$date), ]
datos <- data.frame(fecha = precios$date[-1], r = 100 * diff(log(precios$close)))
base  <- readRDS(file.path(raiz, "output/modelos.rds"))   # ajuste y VaR del análisis

COL_PRIN <- "#1F3A5F"; COL_ACEN <- "#C8553D"; COL_GRIS <- "#8A8F98"
theme_set(theme_minimal(base_size = 13) +
            theme(panel.grid.minor = element_blank(),
                  plot.title = element_text(face = "bold")))
fmt <- function(x, d = 3) formatC(x, format = "f", digits = d, decimal.mark = ",")

grafico_fac <- function(x, titulo, lags = 30) {
  a <- acf(x, lag.max = lags, plot = FALSE)
  d <- data.frame(k = a$lag[-1], fac = a$acf[-1]); b <- 1.96 / sqrt(length(x))
  ggplot(d, aes(k, fac)) + geom_hline(yintercept = 0, color = "grey60") +
    geom_hline(yintercept = c(-b, b), linetype = "dashed", color = COL_ACEN) +
    geom_segment(aes(xend = k, yend = 0), color = COL_PRIN, linewidth = .8) +
    labs(title = titulo, x = "Rezago", y = "FAC")
}

espec <- function(p, q, dist, vt = FALSE)
  ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(q, p), variance.targeting = vt),
             mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
             distribution.model = dist)

# Simulación de un GARCH(1,1): eps_t = sigma_t z_t
simular_garch <- function(n, w, a, b, dist, nu, semilla) {
  set.seed(semilla)
  z <- if (dist == "norm") rnorm(n + 500) else rt(n + 500, nu) * sqrt((nu - 2) / nu)
  s2 <- numeric(n + 500); e <- numeric(n + 500)
  s2[1] <- if (a + b < 1) w / (1 - a - b) else w / (1 - b)
  e[1] <- sqrt(s2[1]) * z[1]
  for (t in 2:(n + 500)) {
    s2[t] <- w + a * e[t - 1]^2 + b * s2[t - 1]
    e[t]  <- sqrt(s2[t]) * z[t]
  }
  i <- 501:(n + 500)                      # se descartan 500 valores de calentamiento
  data.frame(t = seq_len(n), eps = e[i], sigma = sqrt(s2[i]))
}

tarjeta <- function(titulo, salida) card(class = "text-center",
  card_body(tags$div(class = "text-muted small", titulo),
            tags$div(style = "font-size:1.5rem;font-weight:600;", textOutput(salida, inline = TRUE))))

# =============================================================================
ui <- page_navbar(
  title = "GARCH · S&P 500",
  theme = bs_theme(version = 5, bg = "#FFFFFF", fg = "#1E1E1E", primary = COL_PRIN,
                   base_font = font_collection("Segoe UI", "Helvetica Neue", "Arial", "sans-serif")),
  fillable = FALSE,

  # ---- 1. Hechos estilizados ------------------------------------------------
  nav_panel("1 · Hechos estilizados",
    layout_sidebar(
      sidebar = sidebar(width = 280,
        dateRangeInput("rango1", "Periodo", start = min(datos$fecha), end = max(datos$fecha),
                       min = min(datos$fecha), max = max(datos$fecha), language = "es", separator = "a"),
        sliderInput("lags1", "Rezagos de la FAC", 10, 60, 30, step = 5),
        helpText("Los rendimientos casi no se autocorrelacionan, pero sus cuadrados sí:",
                 "la varianza depende del pasado. Esa es la motivación del GARCH.")),
      layout_columns(col_widths = c(3, 3, 3, 3),
        tarjeta("Observaciones", "n1"), tarjeta("Curtosis (normal = 3)", "curt1"),
        tarjeta("Ljung-Box Q(10) de r²", "lb1"), tarjeta("p-valor ARCH-LM(12)", "lm1")),
      card(plotOutput("serie1", height = 280)),
      layout_columns(col_widths = c(6, 6),
        card(plotOutput("fac_r", height = 260)), card(plotOutput("fac_r2", height = 260))),
      card(plotOutput("hist1", height = 260))
    )),

  # ---- 2. Simulador --------------------------------------------------------
  nav_panel("2 · Simulador GARCH(1,1)",
    layout_sidebar(
      sidebar = sidebar(width = 300,
        tags$p(class = "text-center fs-5", HTML("σ<sup>2</sup><sub>t</sub> = ω + α ε<sup>2</sup><sub>t−1</sub> + β σ<sup>2</sup><sub>t−1</sub>")),
        sliderInput("w2", "ω (nivel)", 0.005, 0.30, 0.05, step = 0.005),
        sliderInput("a2", "α (reacción)", 0, 0.50, 0.10, step = 0.01),
        sliderInput("b2", "β (memoria)", 0, 0.99, 0.85, step = 0.01),
        radioButtons("d2", "Innovación z", c("Normal" = "norm", "t-Student" = "std"), inline = TRUE),
        conditionalPanel("input.d2 == 'std'", sliderInput("nu2", "ν (grados de libertad)", 4.1, 30, 6, step = 0.1)),
        sliderInput("n2", "Longitud", 500, 3000, 1500, step = 250),
        actionButton("sim2", "Nueva simulación", class = "btn-outline-primary w-100"),
        helpText("Pruebe α + β cerca de 1, o α grande con β pequeño, y compare.")),
      layout_columns(col_widths = c(3, 3, 3, 3),
        tarjeta("Persistencia α + β", "pers2"), tarjeta("Vida media (días)", "vm2"),
        tarjeta("Volatilidad de largo plazo", "vol2"), tarjeta("Curtosis teórica", "curt2")),
      card(card_header("Condiciones de estacionariedad"), tableOutput("cond2")),
      card(plotOutput("sim2plot", height = 340)),
      layout_columns(col_widths = c(6, 6),
        card(plotOutput("sim2fac", height = 240)), card(plotOutput("sim2fac2", height = 240)))
    )),

  # ---- 3. Estimación -------------------------------------------------------
  nav_panel("3 · Estimación",
    layout_sidebar(
      sidebar = sidebar(width = 290,
        dateRangeInput("rango3", "Muestra", start = min(datos$fecha), end = max(datos$fecha),
                       min = min(datos$fecha), max = max(datos$fecha), language = "es", separator = "a"),
        layout_columns(col_widths = c(6, 6),
          selectInput("p3", "p (σ²)", 1:2, 1), selectInput("q3", "q (ε²)", 1:2, 1)),
        radioButtons("d3", "Distribución", c("t-Student" = "std", "Normal" = "norm"), inline = TRUE),
        checkboxInput("vt3", "Variance targeting", FALSE),
        actionButton("est3", "Estimar", class = "btn-primary w-100"),
        helpText("Al abrir se muestra el modelo del trabajo: GARCH(1,1)-t, 2015–2025.")),
      layout_columns(col_widths = c(3, 3, 3, 3),
        tarjeta("Persistencia α + β", "pers3"), tarjeta("Vida media (días)", "vm3"),
        tarjeta("Vol. anual largo plazo", "vol3"), tarjeta("BIC", "bic3")),
      layout_columns(col_widths = c(6, 6),
        card(card_header("Parámetros (EE robustos)"), tableOutput("coef3")),
        card(card_header("Diagnóstico de los residuales estandarizados"), tableOutput("diag3"))),
      card(plotOutput("vol3plot", height = 300)),
      layout_columns(col_widths = c(6, 6),
        card(plotOutput("qq3", height = 260)), card(plotOutput("fac3", height = 260)))
    )),

  # ---- 4. Pronóstico y VaR ---------------------------------------------------
  nav_panel("4 · Pronóstico y VaR",
    layout_sidebar(
      sidebar = sidebar(width = 290,
        sliderInput("h4", "Horizonte de pronóstico (días)", 5, 500, 250, step = 5),
        numericInput("monto4", "Monto invertido (USD)", 10000, min = 100, step = 1000),
        radioButtons("nivel4", "Nivel del VaR", c("1 %" = "0.01", "5 %" = "0.05"), inline = TRUE),
        helpText("Usa el último modelo estimado en la pestaña 3.",
                 "El VaR fuera de muestra (abajo) viene del análisis: 500 días, reestimación cada 25.")),
      layout_columns(col_widths = c(4, 4, 4),
        tarjeta("σ pronosticada mañana", "s4"), tarjeta("VaR de mañana (%)", "var4"),
        tarjeta("Pérdida máxima esperada (USD)", "usd4")),
      card(plotOutput("pron4", height = 300)),
      card(plotOutput("var4plot", height = 300)),
      card(card_header("Backtesting fuera de muestra"), tableOutput("bt4"))
    ))
)

# =============================================================================
server <- function(input, output, session) {

  # ---- 1 ----
  sub1 <- reactive(subset(datos, fecha >= input$rango1[1] & fecha <= input$rango1[2]))
  output$n1    <- renderText(format(nrow(sub1()), big.mark = "."))
  output$curt1 <- renderText({ r <- sub1()$r; fmt(mean((r - mean(r))^4) / sd(r)^4, 1) })
  output$lb1   <- renderText(fmt(Box.test(sub1()$r^2, 10, "Ljung-Box")$statistic, 0))
  output$lm1   <- renderText({ p <- ArchTest(sub1()$r, lags = 12)$p.value
                               if (p < 0.001) "< 0,001" else fmt(p, 3) })
  output$serie1 <- renderPlotC(ggplot(sub1(), aes(fecha, r)) + geom_line(color = COL_PRIN, linewidth = .3) +
                                labs(title = "Rendimientos logarítmicos diarios (%)", x = NULL, y = "%"))
  output$fac_r  <- renderPlotC(grafico_fac(sub1()$r, "FAC de los rendimientos", input$lags1))
  output$fac_r2 <- renderPlotC(grafico_fac(sub1()$r^2, "FAC de los rendimientos al cuadrado", input$lags1))
  output$hist1  <- renderPlotC({
    r <- sub1()$r
    ggplot(data.frame(r), aes(r)) +
      geom_histogram(aes(y = after_stat(density)), bins = 100, fill = COL_PRIN, alpha = .8) +
      stat_function(fun = dnorm, args = list(mean(r), sd(r)), color = COL_ACEN, linewidth = 1) +
      coord_cartesian(xlim = c(-6, 6)) +
      labs(title = "Distribución frente a la normal (rojo): pico alto y colas pesadas", x = "%", y = "Densidad")
  })

  # ---- 2 ----
  sim <- reactive({
    input$sim2
    simular_garch(input$n2, input$w2, input$a2, input$b2, input$d2,
                  if (is.null(input$nu2)) 6 else input$nu2, semilla = 100 + input$sim2)
  })
  kz2 <- reactive(if (input$d2 == "norm") 3 else if (input$nu2 > 4) 3 * (input$nu2 - 2) / (input$nu2 - 4) else Inf)
  output$pers2 <- renderText(fmt(input$a2 + input$b2, 2))
  output$vm2   <- renderText({ s <- input$a2 + input$b2
                               if (s < 1 && s > 0) fmt(log(.5) / log(s), 1) else "∞" })
  output$vol2  <- renderText({ s <- input$a2 + input$b2
                               if (s < 1) paste0(fmt(sqrt(input$w2 / (1 - s)), 2), " %") else "no existe" })
  output$curt2 <- renderText({
    s <- input$a2 + input$b2; den <- 1 - s^2 - (kz2() - 1) * input$a2^2
    if (is.finite(kz2()) && den > 0) fmt(kz2() * (1 - s^2) / den, 2) else "∞"
  })
  output$cond2 <- renderTable({
    a <- input$a2; b <- input$b2; s <- a + b
    set.seed(1)
    z <- if (input$d2 == "norm") rnorm(2e5) else rt(2e5, input$nu2) * sqrt((input$nu2 - 2) / input$nu2)
    g <- mean(log(b + a * z^2))
    c4 <- s^2 + (kz2() - 1) * a^2
    data.frame(
      Condición = c("Segundo orden: α + β < 1", "Estricta (Nelson): E ln(β + α z²) < 0",
                    "Cuarto momento: (α+β)² + (κz − 1)α² < 1"),
      Valor = c(fmt(s, 3), fmt(g, 3), if (is.finite(c4)) fmt(c4, 3) else "∞"),
      `¿Se cumple?` = c(ifelse(s < 1, "Sí", "No"), ifelse(g < 0, "Sí", "No"),
                        ifelse(is.finite(c4) && c4 < 1, "Sí", "No")), check.names = FALSE)
  }, width = "100%")
  output$sim2plot <- renderPlotC({
    d <- sim()
    ggplot(d, aes(t)) + geom_line(aes(y = eps), color = COL_GRIS, linewidth = .3) +
      geom_line(aes(y = sigma), color = COL_ACEN, linewidth = .6) +
      geom_line(aes(y = -sigma), color = COL_ACEN, linewidth = .6) +
      labs(title = "Serie simulada ε (gris) y bandas ± σ (rojo)", x = "t", y = NULL)
  })
  output$sim2fac  <- renderPlotC(grafico_fac(sim()$eps, "FAC de ε"))
  output$sim2fac2 <- renderPlotC(grafico_fac(sim()$eps^2, "FAC de ε²"))

  # ---- 3 ----
  ajuste <- reactiveVal(list(fit = base$fit_t, r = datos$r, fecha = datos$fecha))
  observeEvent(input$est3, ignoreInit = TRUE, {
    d <- subset(datos, fecha >= input$rango3[1] & fecha <= input$rango3[2])
    if (nrow(d) < 300) {
      showNotification("Se necesitan al menos 300 observaciones.", type = "warning"); return()
    }
    withProgress(message = "Estimando el modelo...", {
      f <- tryCatch(ugarchfit(espec(as.integer(input$p3), as.integer(input$q3), input$d3, input$vt3),
                              data = d$r, solver = "hybrid"), error = function(e) NULL)
    })
    if (is.null(f) || f@fit$convergence != 0) {
      showNotification("El optimizador no convergió; pruebe otra muestra o especificación.", type = "error")
    } else ajuste(list(fit = f, r = d$r, fecha = d$fecha))
  })
  coefs <- reactive(coef(ajuste()$fit))
  pers  <- reactive({ cf <- coefs(); sum(cf[grepl("^alpha|^beta", names(cf))]) })
  output$pers3 <- renderText(fmt(pers(), 4))
  output$vm3   <- renderText(if (pers() < 1) fmt(log(.5) / log(pers()), 0) else "∞")
  output$vol3  <- renderText({ v <- uncvariance(ajuste()$fit)
                               paste0(fmt(sqrt(252 * v), 1), " %") })
  output$bic3  <- renderText(fmt(infocriteria(ajuste()$fit)[2], 4))
  output$coef3 <- renderTable({
    m <- ajuste()$fit@fit$robust.matcoef
    nombres <- c(mu = "μ", omega = "ω", alpha1 = "α1", alpha2 = "α2", beta1 = "β1", beta2 = "β2", shape = "ν")
    data.frame(Parámetro = nombres[rownames(m)], Estimación = fmt(m[, 1], 4),
               `EE robusto` = fmt(m[, 2], 4), `p-valor` = ifelse(m[, 4] < 0.001, "< 0,001", fmt(m[, 4], 3)),
               check.names = FALSE)
  }, width = "100%")
  output$diag3 <- renderTable({
    f <- ajuste()$fit; z <- as.numeric(residuals(f, standardize = TRUE)); sb <- signbias(f)
    p <- c(Box.test(z, 10, "Ljung-Box")$p.value, Box.test(z^2, 10, "Ljung-Box", fitdf = 2)$p.value,
           ArchTest(z, lags = 12)$p.value, sb["Joint Effect", "prob"])
    data.frame(Prueba = c("Ljung-Box Q(10) de z", "Ljung-Box Q(10) de z²", "ARCH-LM(12)", "Sesgo de signo"),
               `p-valor` = ifelse(p < 0.001, "< 0,001", fmt(p, 3)),
               Lectura = ifelse(p < 0.05, "Se rechaza H0", "No se rechaza"), check.names = FALSE)
  }, width = "100%")
  output$vol3plot <- renderPlotC({
    a <- ajuste()
    d <- data.frame(fecha = a$fecha, abs = abs(a$r), s = as.numeric(sigma(a$fit)))
    ggplot(d, aes(fecha)) + geom_line(aes(y = abs), color = COL_GRIS, linewidth = .25, alpha = .7) +
      geom_line(aes(y = s), color = COL_ACEN, linewidth = .6) +
      labs(title = "Volatilidad condicional estimada σ (rojo) y |r| (gris)", x = NULL, y = "%")
  })
  output$qq3 <- renderPlotC({
    f <- ajuste()$fit; z <- sort(as.numeric(residuals(f, standardize = TRUE)))
    cf <- coef(f); dist <- f@model$modeldesc$distribution
    teo <- qdist(dist, ppoints(length(z)), mu = 0, sigma = 1,
                 shape = if ("shape" %in% names(cf)) cf["shape"] else 1)
    ggplot(data.frame(teo, z), aes(teo, z)) + geom_abline(color = COL_ACEN) +
      geom_point(color = COL_PRIN, size = .7, alpha = .6) +
      labs(title = "QQ de los residuales estandarizados", x = "Teóricos", y = "Empíricos")
  })
  output$fac3 <- renderPlotC(grafico_fac(as.numeric(residuals(ajuste()$fit, standardize = TRUE))^2,
                                        "FAC de z² (no debe quedar estructura)"))

  # ---- 4 ----
  fc <- reactive(ugarchforecast(ajuste()$fit, n.ahead = input$h4))
  var_manana <- reactive({
    f <- ajuste()$fit; cf <- coef(f); a <- as.numeric(input$nivel4)
    s1 <- as.numeric(sigma(fc()))[1]
    q <- qdist(f@model$modeldesc$distribution, a, mu = 0, sigma = 1,
               shape = if ("shape" %in% names(cf)) cf["shape"] else 1)
    cf["mu"] + s1 * q
  })
  output$s4   <- renderText(paste0(fmt(as.numeric(sigma(fc()))[1], 2), " %"))
  output$var4 <- renderText(paste0(fmt(var_manana(), 2), " %"))
  output$usd4 <- renderText(paste0("$ ", formatC(-input$monto4 * (exp(var_manana() / 100) - 1),
                                                 format = "f", digits = 0, big.mark = ".")))
  output$pron4 <- renderPlotC({
    f <- ajuste()$fit; s <- as.numeric(sigma(fc())); v <- uncvariance(f)
    hist <- tail(as.numeric(sigma(f)), 120)
    d <- rbind(data.frame(h = -(length(hist) - 1):0, s = hist, tipo = "Estimada"),
               data.frame(h = seq_along(s), s = s, tipo = "Pronosticada"))
    ggplot(d, aes(h, s, color = tipo)) + geom_line(linewidth = .8) +
      geom_hline(yintercept = sqrt(v), linetype = "dotted", color = COL_GRIS) +
      geom_vline(xintercept = 0, color = "grey75") +
      scale_color_manual(values = c(Estimada = COL_PRIN, Pronosticada = COL_ACEN), name = NULL) +
      labs(title = sprintf("Pronóstico de σ: converge a %s %% a la tasa (α+β)^h = %s^h",
                           fmt(sqrt(v), 2), fmt(pers(), 4)),
           x = "Días", y = "σ (%)") + theme(legend.position = "top")
  })
  output$var4plot <- renderPlotC({
    d <- base$roll_var; col <- if (input$nivel4 == "0.01") "var1" else "var5"
    d$v <- d[[col]]; d$exc <- d$r < d$v
    ggplot(d, aes(fecha)) + geom_col(aes(y = r), fill = COL_GRIS, width = 1, alpha = .7) +
      geom_line(aes(y = v), color = COL_ACEN, linewidth = .6) +
      geom_point(data = subset(d, exc), aes(y = r), color = COL_ACEN, size = 2.2) +
      labs(title = sprintf("VaR %s fuera de muestra: %d excedencias de %s esperadas",
                           ifelse(col == "var1", "1 %", "5 %"), sum(d$exc),
                           fmt(nrow(d) * as.numeric(input$nivel4), 0)),
           x = NULL, y = "%")
  })
  output$bt4 <- renderTable({
    d <- base$roll_var; a <- as.numeric(input$nivel4)
    v <- if (a == 0.01) d$var1 else d$var5
    t <- VaRTest(a, d$r, v)
    data.frame(Prueba = c("Kupiec (cobertura incondicional)", "Christoffersen (cobertura condicional)"),
               LR = fmt(c(t$uc.LRstat, t$cc.LRstat), 2), `p-valor` = fmt(c(t$uc.LRp, t$cc.LRp), 3),
               Decisión = ifelse(c(t$uc.LRp, t$cc.LRp) < 0.05, "Se rechaza: VaR mal calibrado",
                                 "No se rechaza: VaR adecuado"), check.names = FALSE)
  }, width = "100%")
}

shinyApp(ui, server)
