# ============================================================
# EMU341 - MA(q) Moving Average Error Model (Box-Jenkins)
# MA(q) is NOT a rolling average of demand observations.
# ============================================================

# 1. HELPER FUNCTIONS -----------------------------------------

ma_method_fit <- function(x, q) {
  # ARIMA(0,0,q) with an estimated non-zero mean.
  suppressWarnings(stats::arima(x, order = c(0L, 0L, as.integer(q)),
                                include.mean = TRUE, method = "ML"))
}

ma_method_metrics <- function(actual, fitted, start_period) {
  idx <- seq.int(as.integer(start_period), length(actual))
  a <- actual[idx]; f <- fitted[idx]
  ok <- is.finite(a) & is.finite(f)
  a <- a[ok]; f <- f[ok]
  if (!length(a)) return(c(MAD = NA_real_, MSE = NA_real_, MAPE = NA_real_))
  e <- a - f
  nonzero <- a != 0
  c(MAD = mean(abs(e)), MSE = mean(e^2),
    MAPE = if (any(nonzero)) 100 * mean(abs(e[nonzero] / a[nonzero])) else NA_real_)
}

ma_method_bic <- function(fit, n) {
  # Include innovation variance in the number of estimated parameters.
  as.numeric(-2 * fit$loglik + log(n) * (length(fit$coef) + 1L))
}

ma_method_select <- function(x, max_q, criterion) {
  orders <- 0:as.integer(max_q)
  fits <- lapply(orders, function(q) {
    tryCatch(ma_method_fit(x, q), error = function(e) NULL)
  })
  scores <- vapply(fits, function(fit) {
    if (is.null(fit)) return(Inf)
    if (criterion == "AIC") as.numeric(fit$aic) else ma_method_bic(fit, length(x))
  }, numeric(1))
  if (!any(is.finite(scores))) stop("No MA model could be estimated for this series.")
  best <- which.min(scores)
  list(q = orders[best], fit = fits[[best]],
       table = data.frame(MA_Order = orders,
                          Criterion = criterion,
                          Score = ifelse(is.finite(scores), round(scores, 3), NA_real_),
                          Selected = orders == orders[best]))
}

# 2. USER INTERFACE -------------------------------------------

ma_method_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::fileInput(ns("file"), "Upload an Excel or CSV file",
                       accept = c(".xlsx", ".csv")),
      shiny::selectInput(ns("column"), "Demand column", choices = NULL),
      shiny::radioButtons(ns("order_mode"), "MA order selection",
                          choices = c("Manual" = "manual", "Automatic" = "auto"),
                          selected = "manual"),
      shiny::conditionalPanel(
        condition = "input.order_mode == 'manual'", ns = ns,
        shiny::numericInput(ns("q"), "MA order (q)", value = 1,
                            min = 0, max = 10, step = 1)),
      shiny::conditionalPanel(
        condition = "input.order_mode == 'auto'", ns = ns,
        shiny::numericInput(ns("max_q"), "Maximum MA order", value = 6,
                            min = 1, max = 10, step = 1),
        shiny::selectInput(ns("criterion"), "Model selection criterion",
                           choices = c("AIC", "BIC"), selected = "BIC")),
      shiny::numericInput(ns("horizon"), "Future periods to forecast",
                          value = 12, min = 1, max = 36, step = 1),
      shiny::helpText("MA(q) uses previous innovations (forecast errors), not rolling averages of demand. Apply it to a stationary series. MA(0) is a white-noise benchmark.")
    ),
    shiny::mainPanel(
      shiny::h3("Moving Average Error Model MA(q)"),
      shiny::h4("Selected Model"), shiny::tableOutput(ns("model_summary")),
      shiny::h4("Parameter Estimates"), shiny::tableOutput(ns("coefficients")),
      shiny::h4("Actual, Fitted and Future Forecasts"),
      shiny::plotOutput(ns("forecast_plot"), height = "360px"),
      shiny::h4("ACF and PACF of Original Series"),
      shiny::plotOutput(ns("acf_plot"), height = "250px"),
      shiny::plotOutput(ns("pacf_plot"), height = "250px"),
      shiny::h4("Residual Diagnostics"),
      shiny::plotOutput(ns("residual_plot"), height = "250px"),
      shiny::tableOutput(ns("diagnostics")),
      shiny::h4("Error Measures (in-sample, after warm-up)"),
      shiny::tableOutput(ns("error_table")),
      shiny::h4("Historical Results"), shiny::tableOutput(ns("historical")),
      shiny::h4("Future Forecasts with 95% Intervals"),
      shiny::tableOutput(ns("future")),
      shiny::conditionalPanel(
        condition = "input.order_mode == 'auto'", ns = ns,
        shiny::h4("Order Selection"), shiny::tableOutput(ns("order_table")))
    )
  )
}

# 3. SERVER ---------------------------------------------------

ma_method_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    data <- shiny::reactive({
      shiny::req(input$file)
      ext <- tolower(tools::file_ext(input$file$name))
      if (ext == "xlsx") {
        readxl::read_excel(input$file$datapath)
      } else if (ext == "csv") {
        first_line <- readLines(input$file$datapath, n = 1, warn = FALSE)
        separator <- if (grepl(";", first_line, fixed = TRUE)) ";" else ","
        utils::read.table(input$file$datapath, header = TRUE,
                          sep = separator, dec = if (separator == ";") "," else ".",
                          check.names = FALSE, stringsAsFactors = FALSE)
      } else {
        shiny::validate(shiny::need(FALSE, "Upload an .xlsx or .csv file."))
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
                                  "Choose a valid numeric demand column."))
      x <- as.numeric(data()[[input$column]])
      shiny::validate(
        shiny::need(length(x) >= 30L, "At least 30 observations are required."),
        shiny::need(all(is.finite(x)), "Series has missing or invalid values."),
        shiny::need(all(x >= 0), "Demand cannot be negative."),
        shiny::need(stats::sd(x) > 1e-10,
                    "Series is constant; MA parameters cannot be identified reliably.")
      )
      x
    })
    
    result <- shiny::reactive({
      x <- demand()
      shiny::req(input$order_mode, input$horizon)
      H <- input$horizon
      shiny::validate(shiny::need(is.finite(H) && H == floor(H) && H >= 1 && H <= 36,
                                  "Forecast horizon must be a whole number from 1 to 36."))
      if (input$order_mode == "manual") {
        shiny::req(input$q)
        q <- input$q
        shiny::validate(shiny::need(is.finite(q) && q == floor(q) && q >= 0 && q <= 10,
                                    "MA order must be a whole number from 0 to 10."))
        shiny::validate(shiny::need(length(x) >= max(30L, 5L * q),
                                    "Use more data or a smaller MA order."))
        q <- as.integer(q)
        fit <- tryCatch(ma_method_fit(x, q), error = function(e) NULL)
        selection <- NULL
      } else {
        shiny::req(input$max_q, input$criterion)
        max_q <- input$max_q
        shiny::validate(
          shiny::need(is.finite(max_q) && max_q == floor(max_q) &&
                        max_q >= 1 && max_q <= 10, "Maximum MA order must be 1 to 10."),
          shiny::need(input$criterion %in% c("AIC", "BIC"), "Choose AIC or BIC.")
        )
        max_q <- min(as.integer(max_q), as.integer(floor(length(x) / 5)))
        selected <- tryCatch(ma_method_select(x, max_q, input$criterion),
                             error = function(e) NULL)
        shiny::validate(shiny::need(!is.null(selected), "Automatic selection failed."))
        q <- selected$q
        fit <- selected$fit
        selection <- selected$table
      }
      shiny::validate(shiny::need(!is.null(fit),
                                  "MA fitting failed. Try a lower order or another series."))
      next_values <- tryCatch(stats::predict(fit, n.ahead = as.integer(H)),
                              error = function(e) NULL)
      shiny::validate(shiny::need(!is.null(next_values), "Forecast calculation failed."))
      future <- as.numeric(next_values$pred)
      se <- as.numeric(next_values$se)
      lower <- future - stats::qnorm(0.975) * se
      upper <- future + stats::qnorm(0.975) * se
      errors <- as.numeric(stats::residuals(fit))
      fitted <- x - errors
      metrics <- ma_method_metrics(x, fitted, start_period = q + 1L)
      # Use > q lags so Ljung-Box adjusted degrees of freedom is positive.
      lb_lag <- min(length(x) - 1L, max(q + 5L, 10L))
      lb <- stats::Box.test(errors, lag = lb_lag,
                            type = "Ljung-Box", fitdf = q)
      list(x = x, q = q, fit = fit, fitted = fitted, errors = errors,
           future = future, lower = lower, upper = upper,
           metrics = metrics, lb = lb, selection = selection)
    })
    
    output$model_summary <- shiny::renderTable({
      r <- result()
      data.frame(Measure = c("Model", "Observations", "AIC", "BIC", "Residual SD"),
                 Value = c(paste0("MA(", r$q, ")"), length(r$x),
                           round(r$fit$aic, 3), round(ma_method_bic(r$fit, length(r$x)), 3),
                           round(sqrt(r$fit$sigma2), 3)))
    })
    
    output$coefficients <- shiny::renderTable({
      r <- result()
      coefs <- r$fit$coef
      # var.coef can be missing in rare singular fits; then show NA SE.
      std_err <- rep(NA_real_, length(coefs))
      if (!is.null(r$fit$var.coef)) {
        diag_values <- diag(r$fit$var.coef)
        if (length(diag_values) == length(coefs)) {
          std_err <- sqrt(pmax(0, diag_values))
        }
      }
      data.frame(Parameter = names(coefs), Estimate = round(as.numeric(coefs), 5),
                 Std_Error = round(std_err, 5), row.names = NULL)
    })
    
    output$forecast_plot <- shiny::renderPlot({
      r <- result(); n <- length(r$x); H <- length(r$future)
      yrange <- range(c(r$x, r$fitted, r$lower, r$upper), finite = TRUE)
      graphics::plot(seq_len(n), r$x, type = "l", lwd = 2, col = "grey30",
                     xlim = c(1, n + H), ylim = yrange,
                     xlab = "Period", ylab = "Demand",
                     main = "MA Model: Actual, Fitted and Forecast")
      graphics::lines(seq_len(n), r$fitted, col = "#2F6FB0", lty = 2)
      graphics::lines(n + seq_len(H), r$future, col = "#B4501E", lwd = 2)
      graphics::lines(n + seq_len(H), r$lower, col = "#B4501E", lty = 3)
      graphics::lines(n + seq_len(H), r$upper, col = "#B4501E", lty = 3)
      graphics::abline(v = n + 0.5, lty = 3)
      graphics::legend("topleft",
                       legend = c("Actual", "In-sample fitted", "Future forecast", "95% interval"),
                       col = c("grey30", "#2F6FB0", "#B4501E", "#B4501E"),
                       lty = c(1, 2, 1, 3), bty = "n", cex = 0.8)
    })
    
    output$acf_plot <- shiny::renderPlot({
      x <- demand()
      stats::acf(x, lag.max = min(30L, length(x) - 1L),
                 main = "ACF: Original Series")
    })
    output$pacf_plot <- shiny::renderPlot({
      x <- demand()
      stats::pacf(x, lag.max = min(30L, length(x) - 1L),
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
                 Value = c(signif(r$lb$p.value, 4), as.integer(r$lb$parameter)))
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
                 Forecast = round(r$future, 3), Lower_95 = round(r$lower, 3),
                 Upper_95 = round(r$upper, 3))
    })
    output$order_table <- shiny::renderTable({
      shiny::req(input$order_mode == "auto")
      result()$selection
    })
  })
}
