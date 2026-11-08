# ============================================================
# EMU341 - ARIMA(p,d,q) Forecasting (Box-Jenkins)
# Uses R's built-in stats::arima; no new R packages required.
# ============================================================

# 1. HELPER FUNCTIONS -----------------------------------------

arima_method_fit <- function(x, p, d, q) {
  # A nonseasonal ARIMA without drift. For d=0, estimate the series mean.
  suppressWarnings(stats::arima(
    x, order = c(as.integer(p), as.integer(d), as.integer(q)),
    include.mean = (as.integer(d) == 0L),
    method = "ML", transform.pars = TRUE
  ))
}

arima_method_ic <- function(fit, n, criterion) {
  if (criterion == "AIC") return(as.numeric(fit$aic))
  # BIC includes variance sigma^2 as an estimated parameter.
  -2 * as.numeric(fit$loglik) + log(n) * (length(fit$coef) + 1L)
}

arima_method_metrics <- function(x, fitted, warmup) {
  valid_idx <- seq.int(as.integer(warmup) + 1L, length(x))
  actual <- x[valid_idx]
  forecast <- fitted[valid_idx]
  ok <- is.finite(actual) & is.finite(forecast)
  actual <- actual[ok]
  forecast <- forecast[ok]
  if (!length(actual)) {
    return(c(MAD = NA_real_, MSE = NA_real_, MAPE = NA_real_))
  }
  errors <- actual - forecast
  nonzero <- actual != 0
  c(
    MAD = mean(abs(errors)),
    MSE = mean(errors^2),
    MAPE = if (any(nonzero)) mean(abs(errors[nonzero] / actual[nonzero])) * 100 else NA_real_
  )
}

arima_method_select <- function(x, max_p, max_d, max_q, criterion) {
  grid <- expand.grid(
    p = 0:as.integer(max_p),
    d = 0:as.integer(max_d),
    q = 0:as.integer(max_q)
  )
  fits <- lapply(seq_len(nrow(grid)), function(i) {
    tryCatch(arima_method_fit(x, grid$p[i], grid$d[i], grid$q[i]),
             error = function(e) NULL)
  })
  scores <- vapply(fits, function(fit) {
    if (is.null(fit)) return(Inf)
    score <- arima_method_ic(fit, length(x), criterion)
    if (is.finite(score)) score else Inf
  }, numeric(1))
  if (!any(is.finite(scores))) return(NULL)
  best <- which.min(scores)
  list(
    p = grid$p[best], d = grid$d[best], q = grid$q[best],
    fit = fits[[best]],
    comparison = data.frame(
      p = grid$p, d = grid$d, q = grid$q,
      Criterion = criterion,
      Score = ifelse(is.finite(scores), round(scores, 3), NA_real_),
      Selected = seq_len(nrow(grid)) == best
    )
  )
}

# 2. USER INTERFACE -------------------------------------------

arima_method_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::fileInput(ns("file"), "Upload an Excel or CSV file",
                       accept = c(".xlsx", ".csv")),
      shiny::selectInput(ns("column"), "Demand column", choices = NULL),
      shiny::radioButtons(ns("order_mode"), "ARIMA order selection",
                          choices = c("Manual" = "manual", "Automatic" = "auto"),
                          selected = "manual"),
      shiny::conditionalPanel(
        condition = "input.order_mode == 'manual'", ns = ns,
        shiny::numericInput(ns("p"), "AR order (p)", value = 1,
                            min = 0, max = 6, step = 1),
        shiny::numericInput(ns("d"), "Differencing order (d)", value = 1,
                            min = 0, max = 2, step = 1),
        shiny::numericInput(ns("q"), "MA order (q)", value = 1,
                            min = 0, max = 6, step = 1)
      ),
      shiny::conditionalPanel(
        condition = "input.order_mode == 'auto'", ns = ns,
        shiny::numericInput(ns("max_p"), "Maximum AR order", value = 3,
                            min = 0, max = 6, step = 1),
        shiny::numericInput(ns("max_d"), "Maximum differencing order", value = 1,
                            min = 0, max = 2, step = 1),
        shiny::numericInput(ns("max_q"), "Maximum MA order", value = 3,
                            min = 0, max = 6, step = 1),
        shiny::selectInput(ns("criterion"), "Model selection criterion",
                           choices = c("BIC", "AIC"), selected = "BIC")
      ),
      shiny::numericInput(ns("horizon"), "Future periods to forecast",
                          value = 12, min = 1, max = 36, step = 1),
      shiny::helpText("ARIMA includes differencing: d=0 is ARMA; d=1 models first differences. ",
                      "The fitted models do not include drift when d>0. ",
                      "For Excel files the first sheet is used.")
    ),
    shiny::mainPanel(
      shiny::h3("Autoregressive Integrated Moving Average: ARIMA(p,d,q)"),
      shiny::h4("Selected Model"), shiny::tableOutput(ns("model_summary")),
      shiny::h4("Parameter Estimates"), shiny::tableOutput(ns("coefficients")),
      shiny::h4("Actual, Fitted and Future Forecasts"),
      shiny::plotOutput(ns("forecast_plot"), height = "360px"),
      shiny::h4("Original and Differenced Series"),
      shiny::plotOutput(ns("differenced_plot"), height = "300px"),
      shiny::h4("ACF / PACF of Modelled (Differenced) Series"),
      shiny::plotOutput(ns("acf_plot"), height = "260px"),
      shiny::plotOutput(ns("pacf_plot"), height = "260px"),
      shiny::h4("Residual Diagnostics"),
      shiny::plotOutput(ns("residual_plot"), height = "260px"),
      shiny::tableOutput(ns("diagnostics")),
      shiny::h4("Error Measures (in-sample, after warm-up)"),
      shiny::tableOutput(ns("error_table")),
      shiny::h4("Historical Results"), shiny::tableOutput(ns("historical")),
      shiny::h4("Future Forecasts with 95% Intervals"),
      shiny::tableOutput(ns("future")),
      shiny::conditionalPanel(
        condition = "input.order_mode == 'auto'", ns = ns,
        shiny::h4("Order Selection (AIC/BIC)"),
        shiny::tableOutput(ns("order_table"))
      )
    )
  )
}

# 3. SERVER ---------------------------------------------------

arima_method_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    
    data <- shiny::reactive({
      shiny::req(input$file)
      ext <- tolower(tools::file_ext(input$file$name))
      if (ext == "xlsx") {
        readxl::read_excel(input$file$datapath)
      } else if (ext == "csv") {
        first <- readLines(input$file$datapath, n = 1L, warn = FALSE)
        sep <- if (grepl(";", first, fixed = TRUE)) ";" else ","
        utils::read.table(input$file$datapath, header = TRUE, sep = sep,
                          dec = if (sep == ";") "," else ".",
                          check.names = FALSE, stringsAsFactors = FALSE)
      } else {
        shiny::validate(shiny::need(FALSE, "Upload a CSV or XLSX file."))
      }
    })
    
    shiny::observeEvent(data(), {
      numeric_cols <- names(data())[vapply(data(), is.numeric, logical(1))]
      shiny::updateSelectInput(
        session, "column", choices = numeric_cols,
        selected = if (length(numeric_cols)) numeric_cols[1] else character(0)
      )
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
        shiny::need(stats::sd(x) > 1e-10, "Select a nonconstant series.")
      )
      x
    })
    
    result <- shiny::reactive({
      x <- demand()
      shiny::req(input$order_mode, input$horizon)
      H <- input$horizon
      shiny::validate(shiny::need(is.finite(H) && H == floor(H) && H >= 1 && H <= 36,
                                  "Forecast horizon must be an integer from 1 to 36."))
      comparison <- NULL
      
      if (input$order_mode == "manual") {
        shiny::req(input$p, input$d, input$q)
        orders <- c(input$p, input$d, input$q)
        shiny::validate(shiny::need(
          all(is.finite(orders)) && all(orders == floor(orders)) &&
            all(orders >= 0) && orders[1] <= 6 && orders[2] <= 2 && orders[3] <= 6,
          "Choose integer p and q from 0 to 6, and d from 0 to 2."
        ))
        p <- as.integer(orders[1]); d <- as.integer(orders[2]); q <- as.integer(orders[3])
        shiny::validate(shiny::need(length(x) >= 8 * (p + q + d + 1),
                                    "Use a longer series or lower model orders."))
        fit <- tryCatch(arima_method_fit(x, p, d, q), error = function(e) NULL)
      } else {
        shiny::req(input$max_p, input$max_d, input$max_q, input$criterion)
        bounds <- c(input$max_p, input$max_d, input$max_q)
        shiny::validate(
          shiny::need(all(is.finite(bounds)) && all(bounds == floor(bounds)) &&
                        all(bounds >= 0) && bounds[1] <= 6 && bounds[2] <= 2 && bounds[3] <= 6,
                      "Maximum p, d, q must be valid whole numbers."),
          shiny::need(input$criterion %in% c("AIC", "BIC"), "Choose AIC or BIC.")
        )
        # Prevent over-parameterized models on short series.
        max_terms <- max(0L, as.integer(floor(length(x) / 8)) - 3L)
        mp <- min(as.integer(bounds[1]), max_terms)
        md <- as.integer(bounds[2])
        mq <- min(as.integer(bounds[3]), max_terms)
        selected <- arima_method_select(x, mp, md, mq, input$criterion)
        shiny::validate(shiny::need(!is.null(selected),
                                    "Automatic ARIMA search failed. Try smaller orders."))
        p <- selected$p; d <- selected$d; q <- selected$q
        fit <- selected$fit
        comparison <- selected$comparison
      }
      
      shiny::validate(shiny::need(!is.null(fit),
                                  "ARIMA model fitting failed. Try different p,d,q."))
      pred <- tryCatch(stats::predict(fit, n.ahead = as.integer(H)),
                       error = function(e) NULL)
      shiny::validate(shiny::need(!is.null(pred), "Forecast calculation failed."))
      future <- as.numeric(pred$pred)
      se <- as.numeric(pred$se)
      lower <- future - stats::qnorm(0.975) * se
      upper <- future + stats::qnorm(0.975) * se
      errors <- as.numeric(stats::residuals(fit))
      fitted <- x - errors
      # Early residuals can reflect diffuse initialization (especially when d>0).
      warmup <- max(d + 1L, p + q + 2L)
      metrics <- arima_method_metrics(x, fitted, warmup)
      lb_lag <- min(length(x) - 1L, max(10L, p + q + 5L))
      diagnostic_errors <- errors[(warmup + 1L):length(errors)]
      lb <- stats::Box.test(diagnostic_errors, lag = lb_lag,
                            type = "Ljung-Box", fitdf = p + q)
      diff_x <- if (d == 0L) x else diff(x, differences = d)
      list(x = x, p = p, d = d, q = q, fit = fit, diff_x = diff_x,
           fitted = fitted, errors = errors, future = future,
           lower = lower, upper = upper, metrics = metrics,
           lb = lb, lb_lag = lb_lag, comparison = comparison)
    })
    
    output$model_summary <- shiny::renderTable({
      r <- result()
      data.frame(
        Measure = c("Model", "Observations", "Differencing (d)",
                    "AIC", "BIC", "Residual SD"),
        Value = c(sprintf("ARIMA(%d,%d,%d)", r$p, r$d, r$q),
                  length(r$x), r$d,
                  round(r$fit$aic, 3),
                  round(arima_method_ic(r$fit, length(r$x), "BIC"), 3),
                  round(sqrt(r$fit$sigma2), 3))
      )
    })
    
    output$coefficients <- shiny::renderTable({
      r <- result()
      pars <- r$fit$coef
      se <- rep(NA_real_, length(pars))
      if (!is.null(r$fit$var.coef) && length(pars)) {
        v <- diag(r$fit$var.coef)
        if (length(v) == length(pars)) se <- sqrt(pmax(0, v))
      }
      if (!length(pars)) {
        return(data.frame(Parameter = "None", Estimate = NA_real_, Std_Error = NA_real_))
      }
      data.frame(Parameter = names(pars), Estimate = round(as.numeric(pars), 5),
                 Std_Error = round(se, 5), row.names = NULL)
    })
    
    output$forecast_plot <- shiny::renderPlot({
      r <- result()
      n <- length(r$x); H <- length(r$future)
      ylim <- range(c(r$x, r$fitted, r$lower, r$upper), finite = TRUE)
      graphics::plot(seq_len(n), r$x, type = "l", lwd = 2, col = "grey30",
                     xlim = c(1, n + H), ylim = ylim,
                     xlab = "Period", ylab = "Demand",
                     main = sprintf("ARIMA(%d,%d,%d): Actual and Forecast", r$p, r$d, r$q))
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
    
    output$differenced_plot <- shiny::renderPlot({
      r <- result()
      old <- graphics::par(mfrow = c(2, 1), mar = c(3, 4, 2, 1))
      on.exit(graphics::par(old))
      graphics::plot(r$x, type = "l", col = "grey30", xlab = "Period",
                     ylab = "Original", main = "Original Series")
      graphics::plot(seq.int(r$d + 1L, length(r$x)), r$diff_x, type = "l",
                     col = "#2F6FB0", xlab = "Period", ylab = "Differenced",
                     main = sprintf("Differenced Series (d = %d)", r$d))
      graphics::abline(h = 0, col = "grey70", lty = 3)
    })
    
    output$acf_plot <- shiny::renderPlot({
      r <- result()
      stats::acf(r$diff_x, lag.max = min(30L, length(r$diff_x) - 1L),
                 main = sprintf("ACF after differencing (d = %d)", r$d))
    })
    output$pacf_plot <- shiny::renderPlot({
      r <- result()
      stats::pacf(r$diff_x, lag.max = min(30L, length(r$diff_x) - 1L),
                  main = sprintf("PACF after differencing (d = %d)", r$d))
    })
    output$residual_plot <- shiny::renderPlot({
      r <- result()
      stats::acf(r$errors, lag.max = min(30L, length(r$errors) - 1L),
                 main = "ACF of Model Residuals")
    })
    output$diagnostics <- shiny::renderTable({
      r <- result()
      data.frame(Check = c("Ljung-Box p-value", "Ljung-Box lag"),
                 Value = c(signif(r$lb$p.value, 4), r$lb_lag))
    })
    output$error_table <- shiny::renderTable({
      r <- result()
      data.frame(Measure = c("MAD", "MSE", "MAPE (%)"),
                 Value = round(unname(r$metrics), 3))
    })
    output$historical <- shiny::renderTable({
      r <- result()
      data.frame(Period = seq_along(r$x), Demand = round(r$x, 3),
                 Fitted = round(r$fitted, 3), Error = round(r$x - r$fitted, 3))
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
