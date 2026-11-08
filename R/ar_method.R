# ============================================================
# EMU341 - AR (Autoregressive) Forecasting Module
# Box-Jenkins: AR(p), ACF / PACF, coefficient estimates
# ============================================================

# 1. HELPER FUNCTIONS -----------------------------------------------------

ar_errors <- function(actual, forecast, start_period) {
  idx <- seq.int(start_period, length(actual))
  good <- is.finite(actual[idx]) & is.finite(forecast[idx])
  a <- actual[idx][good]
  f <- forecast[idx][good]
  if (length(a) == 0L) {
    return(c(MAD = NA_real_, MSE = NA_real_, MAPE = NA_real_))
  }
  e <- a - f
  nonzero <- a != 0
  c(
    MAD = mean(abs(e)),
    MSE = mean(e^2),
    MAPE = if (any(nonzero)) 100 * mean(abs(e[nonzero] / a[nonzero])) else NA_real_
  )
}

ar_fit <- function(x, p) {
  # AR(p) with intercept (stationary AR model, no differencing)
  suppressWarnings(stats::arima(x, order = c(p, 0L, 0L),
                                include.mean = TRUE, method = "ML"))
}

ar_choose_order <- function(x, max_p, criterion) {
  orders <- seq_len(max_p)
  fits <- lapply(orders, function(p) {
    tryCatch(ar_fit(x, p), error = function(e) NULL)
  })
  scores <- vapply(fits, function(fit) {
    if (is.null(fit)) return(Inf)
    if (criterion == "AIC") return(as.numeric(fit$aic))
    # BIC computed using n observations and fitted parameters (AR + mean + variance)
    as.numeric(-2 * fit$loglik + log(length(x)) * (length(fit$coef) + 1L))
  }, numeric(1))
  if (!any(is.finite(scores))) stop("No AR model could be fitted. Try another series.")
  list(p = orders[which.min(scores)], fit = fits[[which.min(scores)]],
       selection = data.frame(Order = orders, Score = round(scores, 3)))
}

# 2. USER INTERFACE -------------------------------------------------------

ar_method_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::fileInput(ns("file"), "Upload an Excel or CSV file",
                       accept = c(".xlsx", ".csv")),
      shiny::selectInput(ns("column"), "Demand column", choices = NULL),
      shiny::radioButtons(ns("order_mode"), "AR order selection",
                          choices = c("Manual" = "manual", "Automatic" = "auto"),
                          selected = "manual"),
      shiny::conditionalPanel(
        condition = "input.order_mode == 'manual'", ns = ns,
        shiny::numericInput(ns("p"), "AR order (p)", value = 1, min = 1, max = 10, step = 1)
      ),
      shiny::conditionalPanel(
        condition = "input.order_mode == 'auto'", ns = ns,
        shiny::numericInput(ns("max_p"), "Maximum AR order", value = 6,
                            min = 1, max = 10, step = 1),
        shiny::selectInput(ns("criterion"), "Model selection criterion",
                           choices = c("AIC", "BIC"), selected = "AIC")
      ),
      shiny::numericInput(ns("horizon"), "Future periods to forecast", value = 12,
                          min = 1, max = 36, step = 1),
      shiny::helpText("AR models describe stationary series. Check the ACF/PACF and residual diagnostics before trusting forecasts.")
    ),
    shiny::mainPanel(
      shiny::h3("Autoregressive AR(p) Forecasting"),
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
      shiny::conditionalPanel("input.order_mode == 'auto'", ns = ns,
                              shiny::h4("Order Selection"),
                              shiny::tableOutput(ns("order_table")))
    )
  )
}

# 3. SERVER ---------------------------------------------------------------

ar_method_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    data <- shiny::reactive({
      shiny::req(input$file)
      ext <- tolower(tools::file_ext(input$file$name))
      if (ext == "xlsx") {
        # Uses the first worksheet, consistent with the Holt module.
        readxl::read_excel(input$file$datapath)
      } else if (ext == "csv") {
        first_line <- readLines(input$file$datapath, n = 1, warn = FALSE)
        sep <- if (grepl(";", first_line, fixed = TRUE)) ";" else ","
        utils::read.table(input$file$datapath, header = TRUE, sep = sep,
                          dec = if (sep == ";") "," else ".",
                          check.names = FALSE, stringsAsFactors = FALSE)
      } else {
        shiny::validate(shiny::need(FALSE, "Upload an .xlsx or .csv file."))
      }
    })
    
    shiny::observeEvent(data(), {
      cols <- names(data())[vapply(data(), is.numeric, logical(1))]
      shiny::updateSelectInput(session, "column", choices = cols,
                               selected = if (length(cols)) cols[1] else character(0))
    })
    
    demand <- shiny::reactive({
      shiny::req(input$column)
      shiny::validate(shiny::need(input$column %in% names(data()),
                                  "Select a valid numeric column."))
      x <- as.numeric(data()[[input$column]])
      shiny::validate(
        shiny::need(length(x) >= 20L, "At least 20 observations are required."),
        shiny::need(all(is.finite(x)), "Series contains missing or invalid values."),
        shiny::need(all(x >= 0), "Demand values cannot be negative."),
        shiny::need(stats::sd(x) > 1e-10,
                    "Series is constant. AR parameters cannot be identified reliably.")
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
        shiny::req(input$p)
        p <- input$p
        shiny::validate(shiny::need(is.finite(p) && p == floor(p) && p >= 1 && p <= 10,
                                    "AR order must be a whole number from 1 to 10."))
        shiny::validate(shiny::need(length(x) >= max(20L, 4L * p),
                                    "Use more observations or a smaller AR order."))
        fit <- tryCatch(ar_fit(x, as.integer(p)), error = function(e) NULL)
        selection <- NULL
      } else {
        shiny::req(input$max_p, input$criterion)
        max_p <- input$max_p
        shiny::validate(shiny::need(is.finite(max_p) && max_p == floor(max_p) &&
                                      max_p >= 1 && max_p <= 10,
                                    "Maximum AR order must be 1 through 10."))
        shiny::validate(shiny::need(input$criterion %in% c("AIC", "BIC"),
                                    "Select AIC or BIC."))
        max_p <- min(as.integer(max_p), floor(length(x) / 4L))
        selected <- tryCatch(ar_choose_order(x, max_p, input$criterion),
                             error = function(e) NULL)
        shiny::validate(shiny::need(!is.null(selected),
                                    "Automatic order selection failed for this series."))
        p <- selected$p
        fit <- selected$fit
        selection <- selected$selection
      }
      shiny::validate(shiny::need(!is.null(fit),
                                  "AR model fitting failed. Try a smaller order or another series."))
      
      next_values <- tryCatch(stats::predict(fit, n.ahead = as.integer(H)),
                              error = function(e) NULL)
      shiny::validate(shiny::need(!is.null(next_values),
                                  "Forecast calculation failed."))
      pred <- as.numeric(next_values$pred)
      se <- as.numeric(next_values$se)
      lower <- pred - stats::qnorm(0.975) * se
      upper <- pred + stats::qnorm(0.975) * se
      
      # ARIMA residual = observed - fitted, including its initialization.
      # Exclude the first p observations from error metrics (warm-up).
      resid <- as.numeric(stats::residuals(fit))
      fitted <- x - resid
      metrics <- ar_errors(x, fitted, start_period = as.integer(p) + 1L)
      # Residual independence check. Low p-values suggest remaining autocorrelation.
      lag_test <- min(length(x) - 1L, max(as.integer(p) + 1L, 10L))
      lb <- stats::Box.test(resid, lag = lag_test, type = "Ljung-Box", fitdf = p)
      
      list(x = x, p = as.integer(p), fit = fit, fitted = fitted,
           residuals = resid, pred = pred, lower = lower, upper = upper,
           metrics = metrics, selection = selection, lb = lb)
    })
    
    output$model_summary <- shiny::renderTable({
      r <- result()
      data.frame(Measure = c("Model", "Observations", "AIC", "BIC", "Residual SD"),
                 Value = c(paste0("AR(", r$p, ")"), length(r$x),
                           round(r$fit$aic, 3),
                           round(-2 * r$fit$loglik + log(length(r$x)) *
                                   (length(r$fit$coef) + 1L), 3),
                           round(sqrt(r$fit$sigma2), 3)))
    })
    
    output$coefficients <- shiny::renderTable({
      r <- result()
      estimates <- as.numeric(r$fit$coef)
      ses <- sqrt(pmax(0, diag(r$fit$var.coef)))
      data.frame(Parameter = names(r$fit$coef), Estimate = round(estimates, 5),
                 Std_Error = round(ses, 5), row.names = NULL)
    })
    
    output$forecast_plot <- shiny::renderPlot({
      r <- result()
      n <- length(r$x); H <- length(r$pred)
      yrange <- range(c(r$x, r$fitted, r$lower, r$upper), finite = TRUE)
      graphics::plot(seq_len(n), r$x, type = "l", lwd = 2, col = "grey30",
                     xlim = c(1, n + H), ylim = yrange,
                     xlab = "Period", ylab = "Demand", main = "AR Model: Actual and Forecast")
      graphics::lines(seq_len(n), r$fitted, col = "#2F6FB0", lty = 2)
      graphics::lines(n + seq_len(H), r$pred, col = "#B4501E", lwd = 2)
      graphics::lines(n + seq_len(H), r$lower, col = "#B4501E", lty = 3)
      graphics::lines(n + seq_len(H), r$upper, col = "#B4501E", lty = 3)
      graphics::abline(v = n + 0.5, lty = 3)
      graphics::legend("topleft", legend = c("Actual", "In-sample fitted", "Future forecast", "95% interval"),
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
      stats::acf(r$residuals, lag.max = min(30L, length(r$x) - 1L),
                 main = "ACF: Model Residuals")
    })
    
    output$diagnostics <- shiny::renderTable({
      r <- result()
      data.frame(Check = c("Ljung-Box p-value", "Ljung-Box lag"),
                 Value = c(signif(r$lb$p.value, 4), r$lb$parameter[[1]]))
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
      data.frame(Period = length(r$x) + seq_along(r$pred),
                 Forecast = round(r$pred, 3),
                 Lower_95 = round(r$lower, 3), Upper_95 = round(r$upper, 3))
    })
    
    output$order_table <- shiny::renderTable({
      shiny::req(input$order_mode == "auto")
      result()$selection
    })
  })
}
