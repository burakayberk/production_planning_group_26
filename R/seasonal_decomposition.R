# ============================================================
# EMU341 - Seasonal Decomposition with Moving Averages
# ============================================================

# 1. HELPER FUNCTIONS

# Centered moving average. For even m, average two adjacent m-period
# moving averages (a 2 x m moving average).
sd_centered_ma <- function(x, m) {
  n <- length(x)
  result <- rep(NA_real_, n)
  if (m %% 2 == 1) {
    half <- (m - 1) / 2
    for (t in seq.int(half + 1, n - half)) {
      result[t] <- mean(x[(t - half):(t + half)])
    }
  } else {
    half <- m / 2
    for (t in seq.int(half + 1, n - half)) {
      left <- mean(x[(t - half):(t + half - 1)])
      right <- mean(x[(t - half + 1):(t + half)])
      result[t] <- (left + right) / 2
    }
  }
  result
}

sd_calculate <- function(d, m, model = "additive", horizon = 12) {
  n <- length(d)
  stopifnot(length(d) >= 2 * m + 1, m >= 2,
            model %in% c("additive", "multiplicative"),
            horizon >= 1)
  
  t <- seq_len(n)
  season <- (t - 1) %% m + 1
  cma <- sd_centered_ma(d, m)
  valid <- is.finite(cma)
  
  if (model == "multiplicative") {
    if (any(d <= 0) || any(cma[valid] <= 0)) {
      stop("Multiplicative decomposition requires strictly positive values.")
    }
    raw <- d / cma
  } else {
    raw <- d - cma
  }
  
  factors <- sapply(seq_len(m), function(s) {
    vals <- raw[valid & season == s]
    if (!length(vals)) stop("Not enough observations for every season.")
    mean(vals)
  })
  
  # Normalize seasonal effects: sum = 0 (additive) or mean = 1 (multiplicative).
  if (model == "additive") {
    factors <- factors - mean(factors)
    deseasonal <- d - factors[season]
  } else {
    factors <- factors / mean(factors)
    deseasonal <- d / factors[season]
  }
  
  # Fit a linear trend to the deseasonalized historical observations.
  trend_fit <- stats::lm(deseasonal ~ t)
  trend_line <- as.numeric(stats::predict(trend_fit, newdata = data.frame(t = t)))
  if (model == "additive") {
    fitted <- trend_line + factors[season]
  } else {
    fitted <- trend_line * factors[season]
  }
  
  future_t <- n + seq_len(horizon)
  future_season <- (future_t - 1) %% m + 1
  future_trend <- as.numeric(stats::predict(
    trend_fit, newdata = data.frame(t = future_t)
  ))
  if (model == "additive") {
    future_forecast <- future_trend + factors[future_season]
  } else {
    future_forecast <- future_trend * factors[future_season]
  }
  
  # Retrospective fitted error (in-sample), not rolling out-of-sample error.
  errors <- fitted - d
  nonzero <- d != 0
  metrics <- c(
    MAD = mean(abs(errors)),
    MSE = mean(errors^2),
    MAPE = if (any(nonzero)) mean(abs(errors[nonzero] / d[nonzero])) * 100 else NA_real_
  )
  
  list(
    periods = t, actual = d, season = season, cma = cma,
    raw_seasonal = raw, factors = factors, deseasonal = deseasonal,
    trend = trend_line, fitted = fitted, error = errors,
    future_period = future_t, future_season = future_season,
    future_trend = future_trend, future = future_forecast,
    metrics = metrics,
    slope = unname(stats::coef(trend_fit)[2]),
    intercept = unname(stats::coef(trend_fit)[1])
  )
}

# 2. USER INTERFACE

seasonal_decomposition_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::fileInput(ns("file"), "Upload an Excel or CSV file",
                       accept = c(".xlsx", ".csv")),
      shiny::selectInput(ns("column"), "Demand column", choices = NULL),
      shiny::numericInput(ns("season_length"), "Season length (periods)",
                          value = 12, min = 2, max = 52, step = 1),
      shiny::radioButtons(ns("model"), "Decomposition model",
                          choices = c("Additive" = "additive",
                                      "Multiplicative" = "multiplicative"),
                          selected = "additive"),
      shiny::numericInput(ns("horizon"), "Future periods to forecast",
                          value = 12, min = 1, max = 36, step = 1),
      shiny::helpText("Additive: constant seasonal effect. ",
                      "Multiplicative: seasonal effect proportional to the level."),
      shiny::helpText("Forecast trend is fitted by linear regression ",
                      "to the deseasonalized series.")
    ),
    shiny::mainPanel(
      shiny::h3("Seasonal Decomposition with Moving Averages"),
      shiny::h4(shiny::textOutput(ns("title"))),
      shiny::h4("Actual Demand, Fitted Values and Future Forecast"),
      shiny::plotOutput(ns("forecast_plot"), height = "350px"),
      shiny::h4("Centered Moving Average and Deseasonalized Series"),
      shiny::plotOutput(ns("components_plot"), height = "300px"),
      shiny::h4("Seasonal Indices"),
      shiny::tableOutput(ns("seasonal_table")),
      shiny::h4("Model Parameters"),
      shiny::tableOutput(ns("parameter_table")),
      shiny::h4("Error Measures (In-Sample Fitted Values)"),
      shiny::tableOutput(ns("error_table")),
      shiny::h4("Historical Results"),
      shiny::tableOutput(ns("historical_table")),
      shiny::h4("Future Forecasts"),
      shiny::tableOutput(ns("future_table"))
    )
  )
}

# 3. SERVER

seasonal_decomposition_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    data <- shiny::reactive({
      shiny::req(input$file)
      ext <- tolower(tools::file_ext(input$file$name))
      if (ext == "xlsx") {
        if (!requireNamespace("readxl", quietly = TRUE)) {
          shiny::validate(shiny::need(FALSE,
                                      "Please install the readxl package: install.packages('readxl')"))
        }
        as.data.frame(readxl::read_excel(input$file$datapath))
      } else if (ext == "csv") {
        first <- readLines(input$file$datapath, n = 1, warn = FALSE)
        delimiter <- if (grepl(";", first, fixed = TRUE)) ";" else ","
        utils::read.table(input$file$datapath, header = TRUE,
                          sep = delimiter,
                          dec = if (delimiter == ";") "," else ".",
                          check.names = FALSE)
      } else {
        shiny::validate(shiny::need(FALSE, "Unsupported file type."))
      }
    })
    
    shiny::observeEvent(data(), {
      cols <- names(data())[vapply(data(), is.numeric, logical(1))]
      preferred <- c("Seasonal", "Trend_Seasonal", "Demand")
      first_preferred <- preferred[preferred %in% cols]
      chosen <- if (length(first_preferred)) first_preferred[1] else if (length(cols)) cols[1] else character(0)
      shiny::updateSelectInput(session, "column", choices = cols, selected = chosen)
    })
    
    result <- shiny::reactive({
      shiny::req(input$column, input$model, input$season_length, input$horizon)
      df <- data()
      shiny::validate(shiny::need(input$column %in% names(df), "Select a numeric demand column."))
      d <- as.numeric(df[[input$column]])
      m <- input$season_length
      h <- input$horizon
      shiny::validate(
        shiny::need(all(is.finite(d)), "Demand contains missing or invalid values."),
        shiny::need(all(d >= 0), "Demand cannot be negative."),
        shiny::need(is.finite(m) && m == floor(m) && m >= 2 && m <= 52,
                    "Season length must be a whole number from 2 to 52."),
        shiny::need(length(d) >= 2 * m + 1,
                    "At least 2 * season length + 1 observations are required."),
        shiny::need(is.finite(h) && h == floor(h) && h >= 1 && h <= 36,
                    "Forecast horizon must be a whole number from 1 to 36."),
        shiny::need(input$model != "multiplicative" || all(d > 0),
                    "Multiplicative decomposition requires positive demand values.")
      )
      sd_calculate(d, as.integer(m), input$model, as.integer(h))
    })
    
    output$title <- shiny::renderText({
      r <- result()
      paste0("Next-period forecast: ", round(r$future[1], 2))
    })
    
    output$forecast_plot <- shiny::renderPlot({
      r <- result()
      n <- length(r$actual)
      plot(r$periods, r$actual, type = "o", pch = 19,
           col = "grey30", xlim = c(1, max(r$future_period)),
           ylim = range(c(r$actual, r$fitted, r$future)),
           xlab = "Period", ylab = "Demand", main = "Actual vs Fitted vs Forecast")
      lines(r$periods, r$fitted, type = "l", lwd = 2, col = "#2F6FB0")
      lines(c(n, r$future_period), c(r$fitted[n], r$future),
            type = "o", pch = 15, lty = 2, col = "#B4501E")
      abline(v = n + 0.5, lty = 3)
      legend("topleft", legend = c("Actual", "Historical fitted", "Future forecast"),
             col = c("grey30", "#2F6FB0", "#B4501E"),
             lty = c(1, 1, 2), pch = c(19, NA, 15), bty = "n")
    })
    
    output$components_plot <- shiny::renderPlot({
      r <- result()
      plot(r$periods, r$actual, type = "l", lwd = 2, col = "grey30",
           ylim = range(c(r$actual, r$cma, r$deseasonal), na.rm = TRUE),
           xlab = "Period", ylab = "Units", main = "Deseasonalization and Centered MA")
      lines(r$periods, r$cma, col = "#2F6FB0", lwd = 2)
      lines(r$periods, r$deseasonal, col = "#B4501E", lty = 2, lwd = 2)
      legend("topleft", c("Actual", "Centered MA", "Deseasonalized"),
             col = c("grey30", "#2F6FB0", "#B4501E"), lty = c(1, 1, 2), bty = "n")
    })
    
    output$seasonal_table <- shiny::renderTable({
      r <- result()
      data.frame(Season = seq_along(r$factors),
                 Seasonal_Index = round(r$factors, 5))
    })
    
    output$parameter_table <- shiny::renderTable({
      r <- result()
      data.frame(Parameter = c("Trend intercept", "Trend slope", "Seasonal index sum", "Seasonal index mean"),
                 Value = round(c(r$intercept, r$slope, sum(r$factors), mean(r$factors)), 5))
    })
    
    output$error_table <- shiny::renderTable({
      r <- result()
      data.frame(Measure = c("MAD", "MSE", "MAPE (%)"),
                 Value = round(unname(r$metrics), 4))
    })
    
    output$historical_table <- shiny::renderTable({
      r <- result()
      data.frame(Period = r$periods, Actual = r$actual,
                 Season = r$season, Centered_MA = round(r$cma, 3),
                 Seasonal_Effect = round(r$factors[r$season], 4),
                 Deseasonalized = round(r$deseasonal, 3),
                 Trend = round(r$trend, 3), Fitted = round(r$fitted, 3),
                 Error = round(r$error, 3))
    }, na = "")
    
    output$future_table <- shiny::renderTable({
      r <- result()
      data.frame(Period = r$future_period, Season = r$future_season,
                 Trend = round(r$future_trend, 3),
                 Forecast = round(r$future, 3))
    })
  })
}
