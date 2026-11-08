# ============================================================
# EMU341 - ARMA(p,q): Autoregressive Moving Average (Box-Jenkins)
# A stationary time series method; no differencing (d = 0).
# ============================================================

# 1. HELPER FUNCTIONS -----------------------------------------

arma_method_fit <- function(x, p, q) {
  # stats::arima parameterization: for d = 0, 'intercept' is the series mean.
  suppressWarnings(stats::arima(
    x, order = c(as.integer(p), 0L, as.integer(q)),
    include.mean = TRUE, method = "ML", transform.pars = TRUE
  ))
}

arma_method_bic <- function(fit, n) {
  # Count fitted AR, MA, mean parameters plus innovation variance.
  -2 * as.numeric(fit$loglik) + log(n) * (length(fit$coef) + 1L)
}

arma_method_metrics <- function(actual, fitted, warmup) {
  idx <- seq.int(max(1L, as.integer(warmup) + 1L), length(actual))
  a <- actual[idx]
  f <- fitted[idx]
  valid <- is.finite(a) & is.finite(f)
  a <- a[valid]
  f <- f[valid]
  if (length(a) == 0L) {
    return(c(MAD = NA_real_, MSE = NA_real_, MAPE = NA_real_))
  }
  err <- a - f
  nonzero <- a != 0
  c(MAD = mean(abs(err)), MSE = mean(err^2),
    MAPE = if (any(nonzero)) 100 * mean(abs(err[nonzero] / a[nonzero])) else NA_real_)
}

arma_method_select <- function(x, max_p, max_q, criterion) {
  grid <- expand.grid(p = 0:as.integer(max_p), q = 0:as.integer(max_q))
  fits <- lapply(seq_len(nrow(grid)), function(i) {
    tryCatch(arma_method_fit(x, grid$p[i], grid$q[i]), error = function(e) NULL)
  })
  scores <- vapply(fits, function(fit) {
    if (is.null(fit)) return(Inf)
    if (criterion == "AIC") as.numeric(fit$aic) else arma_method_bic(fit, length(x))
  }, numeric(1))
  if (!any(is.finite(scores))) stop("No ARMA model could be estimated.")
  best <- which.min(scores)
  list(p = grid$p[best], q = grid$q[best], fit = fits[[best]],
       comparison = data.frame(
         p = grid$p, q = grid$q, Criterion = criterion,
         Score = ifelse(is.finite(scores), round(scores, 3), NA_real_),
         Selected = seq_len(nrow(grid)) == best
       ))
}

# 2. USER INTERFACE -------------------------------------------

arma_method_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::fileInput(ns("file"), "Upload an Excel or CSV file",
                       accept = c(".xlsx", ".csv")),
      shiny::selectInput(ns("column"), "Demand column", choices = NULL),
      shiny::radioButtons(ns("order_mode"), "ARMA order selection",
                          choices = c("Manual" = "manual", "Automatic" = "auto"),
                          selected = "manual"),
      shiny::conditionalPanel(
        condition = "input.order_mode == 'manual'", ns = ns,
        shiny::numericInput(ns("p"), "AR order (p)", 1, min = 0, max = 6, step = 1),
        shiny::numericInput(ns("q"), "MA order (q)", 1, min = 0, max = 6, step = 1)
      ),
      shiny::conditionalPanel(
        condition = "input.order_mode == 'auto'", ns = ns,
        shiny::numericInput(ns("max_p"), "Maximum AR order", 3,
                            min = 0, max = 6, step = 1),
        shiny::numericInput(ns("max_q"), "Maximum MA order", 3,
                            min = 0, max = 6, step = 1),
        shiny::selectInput(ns("criterion"), "Model selection criterion",
                           choices = c("BIC", "AIC"), selected = "BIC")
      ),
      shiny::numericInput(ns("horizon"), "Future periods to forecast", 12,
                          min = 1, max = 36, step = 1),
      shiny::helpText("ARMA assumes a stationary series (no differencing). ",
                      "Select p = 0 for a pure MA model, q = 0 for a pure AR model. ",
                      "Automatic search also considers ARMA(0,0).")
    ),
    shiny::mainPanel(
      shiny::h3("Autoregressive Moving Average: ARMA(p,q)"),
      shiny::h4("Selected Model"), shiny::tableOutput(ns("model_summary")),
      shiny::h4("Parameter Estimates"), shiny::tableOutput(ns("coefficients")),
      shiny::h4("Actual, Fitted and Future Forecasts"),
      shiny::plotOutput(ns("forecast_plot"), height = "360px"),
      shiny::h4("ACF and PACF of Original Series"),
      shiny::plotOutput(ns("acf_plot"), height = "260px"),
      shiny::plotOutput(ns("pacf_plot"), height = "260px"),
      shiny::h4("Residual Diagnostics"),
      shiny::plotOutput(ns("residual_plot"), height = "250px"),
      shiny::tableOutput(ns("diagnostics")),
      shiny::h4("Error Measures (in-sample, after warm-up)"),
      shiny::tableOutput(ns("error_table")),
      shiny::h4("Historical Results"), shiny::tableOutput(ns("historical")),
      shiny::h4("Future Forecasts with 95% Intervals"),
      shiny::tableOutput(ns("future")),
      shiny::conditionalPanel(condition = "input.order_mode == 'auto'", ns = ns,
                              shiny::h4("Order Selection (AIC/BIC)"),
                              shiny::tableOutput(ns("order_table")))
    )
  )
}

# 3. SERVER ---------------------------------------------------

arma_method_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    
    data <- shiny::reactive({
      shiny::req(input$file)
      ext <- tolower(tools::file_ext(input$file$name))
      if (ext == "xlsx") {
        # Like the existing modules: use the first Excel sheet.
        readxl::read_excel(input$file$datapath)
      } else if (ext == "csv") {
        line <- readLines(input$file$datapath, n = 1L, warn = FALSE)
        sep <- if (grepl(";", line, fixed = TRUE)) ";" else ","
        utils::read.table(input$file$datapath, header = TRUE, sep = sep,
                          dec = if (sep == ";") "," else ".",
                          check.names = FALSE, stringsAsFactors = FALSE)
      } else {
        shiny::validate(shiny::need(FALSE, "Upload a CSV or XLSX file."))
      }
    })
    
    shiny::observeEvent(data(), {
      numeric_cols <- names(data())[vapply(data(), is.numeric, logical(1))]
      shiny::updateSelectInput(session, "column", choices = numeric_cols,
                               selected = if (length(numeric_cols)) numeric_cols[1] else character(0))
    })
    
    demand <- shiny::reactive({
      shiny::req(input$column)
      shiny::validate(shiny::need(input$column %in% names(data()),
                                  "Choose a numeric demand column."))
      x <- as.numeric(data()[[input$column]])
      shiny::validate(
        shiny::need(length(x) >= 40L, "At least 40 observations are required."),
        shiny::need(all(is.finite(x)), "Data contain missing or invalid values."),
        shiny::need(all(x >= 0), "Demand cannot be negative."),
        shiny::need(stats::sd(x) > 1e-10,
                    "ARMA parameters are not identifiable for a constant series.")
      )
      x
    })
    
    result <- shiny::reactive({
      x <- demand()
      shiny::req(input$order_mode, input$horizon)
      H <- input$horizon
      shiny::validate(shiny::need(is.finite(H) && H == floor(H) && H >= 1 && H <= 36,
                                  "Forecast horizon must be an integer from 1 to 36."))
      
      if (input$order_mode == "manual") {
        shiny::req(input$p, input$q)
        p <- input$p
        q <- input$q
        shiny::validate(
          shiny::need(all(is.finite(c(p, q))) && all(c(p, q) == floor(c(p, q))) &&
                        all(c(p, q) >= 0) && all(c(p, q) <= 6),
                      "Orders p and q must be whole numbers from 0 to 6."),
          shiny::need(length(x) >= 8 * (p + q + 1),
                      "Use a longer series or lower model orders.")
        )
        p <- as.integer(p)
        q <- as.integer(q)
        fit <- tryCatch(arma_method_fit(x, p, q), error = function(e) NULL)
        comparison <- NULL
      } else {
        shiny::req(input$max_p, input$max_q, input$criterion)
        mp <- input$max_p
        mq <- input$max_q
        shiny::validate(
          shiny::need(all(is.finite(c(mp, mq))) &&
                        all(c(mp, mq) == floor(c(mp, mq))) &&
                        all(c(mp, mq) >= 0) && all(c(mp, mq) <= 6),
                      "Maximum AR/MA orders must be integers from 0 to 6."),
          shiny::need(input$criterion %in% c("AIC", "BIC"), "Choose AIC or BIC.")
        )
        max_terms <- max(0L, as.integer(floor(length(x) / 8)) - 1L)
        mp <- min(as.integer(mp), max_terms)
        mq <- min(as.integer(mq), max_terms)
        selected <- tryCatch(arma_method_select(x, mp, mq, input$criterion),
                             error = function(e) NULL)
        shiny::validate(shiny::need(!is.null(selected),
                                    "Automatic model selection failed."))
        p <- selected$p
        q <- selected$q
        fit <- selected$fit
        comparison <- selected$comparison
      }
      shiny::validate(shiny::need(!is.null(fit),
                                  "ARMA model fitting failed. Try lower orders."))
      forecast <- tryCatch(stats::predict(fit, n.ahead = as.integer(H)),
                           error = function(e) NULL)
      shiny::validate(shiny::need(!is.null(forecast), "Forecast calculation failed."))
      future <- as.numeric(forecast$pred)
      se <- as.numeric(forecast$se)
      lower <- future - stats::qnorm(0.975) * se
      upper <- future + stats::qnorm(0.975) * se
      errors <- as.numeric(stats::residuals(fit))
      fitted <- x - errors
      metrics <- arma_method_metrics(x, fitted, warmup = p + q)
      lb_lag <- min(length(x) - 1L, max(10L, p + q + 5L))
      lb <- stats::Box.test(errors, lag = lb_lag,
                            type = "Ljung-Box", fitdf = p + q)
      list(x = x, p = p, q = q, fit = fit, fitted = fitted, errors = errors,
           future = future, lower = lower, upper = upper,
           metrics = metrics, lb = lb, comparison = comparison)
    })
    
    output$model_summary <- shiny::renderTable({
      r <- result()
      data.frame(Measure = c("Model", "Observations", "AIC", "BIC", "Residual SD"),
                 Value = c(sprintf("ARMA(%d,%d)", r$p, r$q), length(r$x),
                           round(r$fit$aic, 3),
                           round(arma_method_bic(r$fit, length(r$x)), 3),
                           round(sqrt(r$fit$sigma2), 3)))
    })
    
    output$coefficients <- shiny::renderTable({
      r <- result()
      pars <- r$fit$coef
      std_err <- rep(NA_real_, length(pars))
      if (!is.null(r$fit$var.coef)) {
        diagonal <- diag(r$fit$var.coef)
        if (length(diagonal) == length(pars)) {
          std_err <- sqrt(pmax(0, diagonal))
        }
      }
      data.frame(Parameter = names(pars), Estimate = round(as.numeric(pars), 5),
                 Std_Error = round(std_err, 5), row.names = NULL)
    })
    
    output$forecast_plot <- shiny::renderPlot({
      r <- result()
      n <- length(r$x)
      H <- length(r$future)
      yrange <- range(c(r$x, r$fitted, r$lower, r$upper), finite = TRUE)
      graphics::plot(seq_len(n), r$x, type = "l", lwd = 2, col = "grey30",
                     xlim = c(1, n + H), ylim = yrange,
                     xlab = "Period", ylab = "Demand",
                     main = sprintf("ARMA(%d,%d): Actual, Fitted and Forecast", r$p, r$q))
      graphics::lines(seq_len(n), r$fitted, col = "#2F6FB0", lty = 2)
      graphics::lines(n + seq_len(H), r$future, col = "#B4501E", lwd = 2)
      graphics::lines(n + seq_len(H), r$lower, col = "#B4501E", lty = 3)
      graphics::lines(n + seq_len(H), r$upper, col = "#B4501E", lty = 3)
      graphics::abline(v = n + 0.5, lty = 3)
      graphics::legend("topleft", legend = c("Actual", "In-sample fitted",
                                             "Future forecast", "95% interval"),
                       col = c("grey30", "#2F6FB0", "#B4501E", "#B4501E"),
                       lty = c(1, 2, 1, 3), bty = "n", cex = 0.8)
    })
    
    output$acf_plot <- shiny::renderPlot({
      stats::acf(demand(), lag.max = min(30L, length(demand()) - 1L),
                 main = "ACF: Original Series")
    })
    output$pacf_plot <- shiny::renderPlot({
      stats::pacf(demand(), lag.max = min(30L, length(demand()) - 1L),
                  main = "PACF: Original Series")
    })
    output$residual_plot <- shiny::renderPlot({
      r <- result()
      stats::acf(r$errors, lag.max = min(30L, length(r$x) - 1L),
                 main = "ACF: Model Residuals")
    })
    output$diagnostics <- shiny::renderTable({
      r <- result()
      data.frame(Check = c("Ljung-Box p-value", "Ljung-Box lag"),
                 Value = c(signif(r$lb$p.value, 4),
                           as.integer(r$lb$parameter)))
    })
    output$error_table <- shiny::renderTable({
      r <- result()
      data.frame(Measure = c("MAD", "MSE", "MAPE (%)"),
                 Value = round(unname(r$metrics), 3))
    })
    output$historical <- shiny::renderTable({
      r <- result()
      data.frame(Period = seq_along(r$x), Demand = round(r$x, 3),
                 Fitted = round(r$fitted, 3),
                 Error = round(r$x - r$fitted, 3))
    })
    output$future <- shiny::renderTable({
      r <- result()
      data.frame(Period = length(r$x) + seq_along(r$future),
                 Forecast = round(r$future, 3),
                 Lower_95 = round(r$lower, 3),
                 Upper_95 = round(r$upper, 3))
    })
    output$order_table <- shiny::renderTable({
      shiny::req(input$order_mode == "auto")
      result()$comparison
    })
  })
}
