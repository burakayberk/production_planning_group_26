# ============================================================
# EMU341 - Holt-Winters Triple Exponential Smoothing
# ============================================================
# No extra package is required for the calculations.
# The Shiny interface requires shiny; Excel import uses readxl.

# 1. HELPER FUNCTIONS

winters_initial_values <- function(d, m, model = "additive") {
  # Estimate the starting trend by comparing matching seasons in two cycles.
  first <- d[seq_len(m)]
  second <- d[m + seq_len(m)]
  b <- mean((second - first) / m)
  # Level at the end of the first season (period m).
  l <- mean(first) + b * (m - 1) / 2
  levels_first <- l + (seq_len(m) - m) * b
  if (model == "additive") {
    s <- first - levels_first
    s <- s - mean(s)
  } else {
    if (any(levels_first <= 0))
      stop("Estimated initial levels must be positive for a multiplicative model.")
    s <- first / levels_first
    s <- s / mean(s)
  }
  list(level = l, trend = b, seasonal = as.numeric(s))
}

winters_calculate <- function(d, m, model, alpha, beta, gamma,
                              initial, horizon) {
  n <- length(d)
  level <- rep(NA_real_, n)
  trend <- rep(NA_real_, n)
  seasonal <- rep(NA_real_, n)
  fitted <- rep(NA_real_, n)
  
  level[m] <- initial$level
  trend[m] <- initial$trend
  seasonal[seq_len(m)] <- initial$seasonal
  
  for (t in seq.int(m + 1L, n)) {
    prior_season <- seasonal[t - m]
    base <- level[t - 1L] + trend[t - 1L]
    if (model == "additive") {
      fitted[t] <- base + prior_season
      level[t] <- alpha * (d[t] - prior_season) + (1 - alpha) * base
      trend[t] <- beta * (level[t] - level[t - 1L]) +
        (1 - beta) * trend[t - 1L]
      seasonal[t] <- gamma * (d[t] - level[t]) +
        (1 - gamma) * prior_season
    } else {
      if (!is.finite(base) || base <= 0 || prior_season <= 0)
        stop("Nonpositive level or seasonal factor: use additive mode or different initial values.")
      fitted[t] <- base * prior_season
      level[t] <- alpha * (d[t] / prior_season) + (1 - alpha) * base
      if (!is.finite(level[t]) || level[t] <= 0)
        stop("Nonpositive level: choose additive mode or different parameters.")
      trend[t] <- beta * (level[t] - level[t - 1L]) +
        (1 - beta) * trend[t - 1L]
      seasonal[t] <- gamma * (d[t] / level[t]) +
        (1 - gamma) * prior_season
    }
  }
  
  h <- seq_len(horizon)
  future_season <- seasonal[n - m + ((h - 1L) %% m) + 1L]
  future_base <- level[n] + h * trend[n]
  if (model == "multiplicative" && any(future_base <= 0))
    stop("Future trend level is nonpositive for multiplicative forecasts.")
  future <- if (model == "additive") future_base + future_season else
    future_base * future_season
  
  idx <- seq.int(m + 1L, n)
  err <- fitted[idx] - d[idx]
  valid <- d[idx] != 0
  metrics <- c(
    MAD = mean(abs(err)),
    MSE = mean(err^2),
    MAPE = if (any(valid)) mean(abs(err[valid] / d[idx][valid])) * 100
    else NA_real_
  )
  
  list(actual = d, fitted = fitted, level = level, trend = trend,
       seasonal = seasonal, future = future, future_season = future_season,
       metrics = metrics, first_forecast_period = m + 1L)
}

# 2. USER INTERFACE

winters_method_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::fileInput(ns("file"), "Upload an Excel or CSV file",
                       accept = c(".xlsx", ".csv")),
      shiny::selectInput(ns("column"), "Demand column", choices = NULL),
      shiny::numericInput(ns("season_length"), "Season length (periods)",
                          value = 12, min = 2, max = 52, step = 1),
      shiny::radioButtons(ns("model"), "Seasonality type",
                          choices = c("Additive" = "additive",
                                      "Multiplicative" = "multiplicative"),
                          selected = "additive"),
      shiny::radioButtons(ns("parameter_mode"), "Parameter selection",
                          choices = c("Manual Alpha, Beta and Gamma" = "manual",
                                      "Optimize parameters" = "auto"),
                          selected = "manual"),
      shiny::conditionalPanel(
        condition = "input.parameter_mode == 'manual'", ns = ns,
        shiny::numericInput(ns("alpha"), "Alpha (level)", 0.3, min = 0, max = 1, step = 0.05),
        shiny::numericInput(ns("beta"), "Beta (trend)", 0.2, min = 0, max = 1, step = 0.05),
        shiny::numericInput(ns("gamma"), "Gamma (seasonality)", 0.2, min = 0, max = 1, step = 0.05)
      ),
      shiny::conditionalPanel(
        condition = "input.parameter_mode == 'auto'", ns = ns,
        shiny::selectInput(ns("criterion"), "Optimization criterion",
                           choices = c("MAD", "MSE", "MAPE"), selected = "MSE")
      ),
      shiny::radioButtons(ns("initial_mode"), "Initial values",
                          choices = c("Regression estimate" = "regression",
                                      "Estimate from first two seasons" = "estimate",
                                      "Manual initial level and trend" = "manual"),
                          selected = "regression"),
      shiny::conditionalPanel(
        condition = "input.initial_mode == 'manual'", ns = ns,
        shiny::numericInput(ns("initial_level"), "Level at end of first season (L_m)", 200),
        shiny::numericInput(ns("initial_trend"), "Initial trend per period (T_m)", 0)
      ),
      shiny::helpText("Initial seasonal factors follow the selected initialization method. ",
                      "For manual initialization, you may override level and trend."),
      shiny::numericInput(ns("horizon"), "Future periods to forecast",
                          12, min = 1, max = 36, step = 1)
    ),
    shiny::mainPanel(
      shiny::h3("Winters' Method (Triple Exponential Smoothing)"),
      shiny::h4(shiny::textOutput(ns("title"))),
      shiny::h4("Parameters Used"),
      shiny::tableOutput(ns("parameter_table")),
      shiny::h4("Actual Demand and Forecast"),
      shiny::plotOutput(ns("forecast_plot"), height = "350px"),
      shiny::h4("Error Measures (One-Step-Ahead Historical Forecasts)"),
      shiny::tableOutput(ns("error_table")),
      shiny::h4("Initial Seasonal Factors"),
      shiny::tableOutput(ns("seasonal_table")),
      shiny::h4("Historical Results"),
      shiny::tableOutput(ns("period_table")),
      shiny::h4("Future Forecasts"),
      shiny::tableOutput(ns("future_table"))
    )
  )
}

# 3. SERVER

winters_method_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    
    data <- shiny::reactive({
      shiny::req(input$file)
      ext <- tolower(tools::file_ext(input$file$name))
      if (ext == "xlsx") {
        shiny::validate(shiny::need(requireNamespace("readxl", quietly = TRUE),
                                    "Install readxl: install.packages('readxl')"))
        as.data.frame(readxl::read_excel(input$file$datapath))
      } else if (ext == "csv") {
        first <- readLines(input$file$datapath, n = 1, warn = FALSE)
        sep <- if (grepl(";", first, fixed = TRUE)) ";" else ","
        utils::read.table(input$file$datapath, header = TRUE, sep = sep,
                          dec = if (sep == ";") "," else ".",
                          check.names = FALSE)
      } else {
        shiny::validate(shiny::need(FALSE, "Unsupported file type."))
      }
    })
    
    shiny::observeEvent(data(), {
      cols <- names(data())[vapply(data(), is.numeric, logical(1))]
      preferred <- c("Seasonal", "Trend_Seasonal", "Demand")
      avail <- preferred[preferred %in% cols]
      chosen <- if (length(avail)) avail[1] else if (length(cols)) cols[1] else character(0)
      shiny::updateSelectInput(session, "column", choices = cols, selected = chosen)
    })
    
    validated <- shiny::reactive({
      shiny::req(input$column, input$season_length, input$horizon, input$model)
      df <- data()
      shiny::validate(shiny::need(input$column %in% names(df), "Select a numeric column."))
      d <- as.numeric(df[[input$column]])
      m <- input$season_length
      H <- input$horizon
      shiny::validate(
        shiny::need(all(is.finite(d)), "Demand contains missing or invalid values."),
        shiny::need(all(d >= 0), "Demand cannot be negative."),
        shiny::need(length(m) == 1 && is.finite(m) && m == floor(m) && m >= 2 && m <= 52,
                    "Season length must be an integer from 2 to 52."),
        shiny::need(length(d) >= 2 * m + 1,
                    "At least two complete seasons plus one period are required."),
        shiny::need(length(H) == 1 && is.finite(H) && H == floor(H) && H >= 1 && H <= 36,
                    "Forecast horizon must be an integer from 1 to 36."),
        shiny::need(input$model != "multiplicative" || all(d > 0),
                    "Multiplicative seasonality requires strictly positive demand.")
      )
      list(d = d, m = as.integer(m), H = as.integer(H))
    })
    
    initialization <- shiny::reactive({
      v <- validated()
      init <- tryCatch(winters_initial_values(v$d, v$m, input$model),
                       error = function(e) e)
      shiny::validate(shiny::need(!inherits(init, "error"),
                                  if (inherits(init, "error")) conditionMessage(init) else TRUE))
      if (input$initial_mode == "regression") {
        periods <- seq_along(v$d)
        season <- factor((periods - 1L) %% v$m + 1L)
        # Seasonal fixed effects keep the trend estimate separate from seasonality.
        fit <- stats::lm(v$d ~ periods + season)
        B <- unname(stats::coef(fit)["periods"])
        baseline_at_m <- mean(stats::predict(fit,
          newdata = data.frame(periods = rep(v$m, v$m),
                              season = factor(seq_len(v$m), levels = seq_len(v$m)))))
        baseline <- baseline_at_m + (periods - v$m) * B
        if (input$model == "additive") {
          factors <- as.numeric(tapply(v$d - baseline, season, mean))
          factors <- factors - mean(factors)
        } else {
          shiny::validate(shiny::need(all(baseline > 0), "Regression produces nonpositive levels; use additive or manual initialization."))
          factors <- as.numeric(tapply(v$d / baseline, season, mean))
          factors <- factors / mean(factors)
        }
        init <- list(level = baseline_at_m, trend = B, seasonal = factors)
      }
      if (input$initial_mode == "manual") {
        shiny::req(input$initial_level, input$initial_trend)
        L <- input$initial_level
        B <- input$initial_trend
        shiny::validate(
          shiny::need(is.finite(L) && (input$model == "additive" || L > 0),
                      "Initial level must be finite (and positive for multiplicative models)."),
          shiny::need(is.finite(B), "Initial trend must be finite.")
        )
        # Recalculate seasonal factors consistently with the manual level/trend.
        first <- v$d[seq_len(v$m)]
        baseline <- L + (seq_len(v$m) - v$m) * B
        if (input$model == "additive") {
          s <- first - baseline
          s <- s - mean(s)
        } else {
          shiny::validate(shiny::need(all(baseline > 0),
                                      "Manual level/trend gives a nonpositive first-season baseline."))
          s <- first / baseline
          s <- s / mean(s)
        }
        init <- list(level = L, trend = B, seasonal = s)
      }
      init
    })
    
    parameters <- shiny::reactive({
      v <- validated()
      init <- initialization()
      if (input$parameter_mode == "manual") {
        shiny::req(input$alpha, input$beta, input$gamma)
        p <- c(input$alpha, input$beta, input$gamma)
        shiny::validate(shiny::need(all(is.finite(p) & p >= 0 & p <= 1),
                                    "Alpha, Beta and Gamma must be between 0 and 1."))
        return(p)
      }
      shiny::req(input$criterion)
      criterion <- input$criterion
      shiny::validate(shiny::need(criterion %in% c("MAD", "MSE", "MAPE"),
                                  "Choose MAD, MSE or MAPE."))
      if (criterion == "MAPE") {
        shiny::validate(shiny::need(any(v$d[-seq_len(v$m)] != 0),
                                    "MAPE cannot be optimized when all evaluated demands are zero."))
      }
      objective <- function(p) {
        out <- tryCatch(winters_calculate(v$d, v$m, input$model,
                                          p[1], p[2], p[3], init, v$H), error = function(e) NULL)
        if (is.null(out)) return(1e30)
        value <- unname(out$metrics[criterion])
        if (length(value) != 1 || !is.finite(value)) 1e30 else value
      }
      # Multiple starting points reduce sensitivity to local optima.
      starts <- list(c(0.2, 0.2, 0.2), c(0.6, 0.2, 0.3),
                     c(0.8, 0.6, 0.5), c(0, 0, 0), c(1, 1, 1))
      scores <- vapply(starts, objective, numeric(1))
      best <- starts[[which.min(scores)]]
      best_score <- min(scores)
      for (start in starts[seq_len(3)]) {
        fit <- tryCatch(stats::optim(start, objective, method = "L-BFGS-B",
                                     lower = rep(0, 3), upper = rep(1, 3),
                                     control = list(maxit = 300)), error = function(e) NULL)
        if (!is.null(fit) && is.finite(fit$value) && fit$value < best_score) {
          best <- fit$par
          best_score <- fit$value
        }
      }
      shiny::validate(shiny::need(best_score < 1e30, "No valid parameter combination found."))
      best
    })
    
    result <- shiny::reactive({
      v <- validated()
      p <- parameters()
      init <- initialization()
      out <- tryCatch(winters_calculate(v$d, v$m, input$model,
                                        p[1], p[2], p[3], init, v$H), error = function(e) e)
      shiny::validate(shiny::need(!inherits(out, "error"),
                                  if (inherits(out, "error")) conditionMessage(out) else TRUE))
      c(out, list(initial = init, p = p, m = v$m, model = input$model))
    })
    
    output$title <- shiny::renderText({
      r <- result()
      paste0("Next-period forecast: ", round(r$future[1], 2))
    })
    
    output$parameter_table <- shiny::renderTable({
      r <- result()
      data.frame(Parameter = c("Model", "Season Length", "Alpha", "Beta", "Gamma",
                               "Initial Level (at period m)", "Initial Trend"),
                 Value = c(r$model, r$m, round(r$p, 5),
                           round(r$initial$level, 5), round(r$initial$trend, 5)))
    })
    
    output$forecast_plot <- shiny::renderPlot({
      r <- result()
      n <- length(r$actual)
      H <- length(r$future)
      plot(seq_len(n), r$actual, type = "o", pch = 19, col = "grey30",
           xlim = c(1, n + H),
           ylim = range(c(r$actual, r$fitted, r$future), na.rm = TRUE),
           xlab = "Period", ylab = "Demand",
           main = "Winters' Method - Actual and Forecast")
      lines(seq_len(n), r$fitted, col = "#2F6FB0", lty = 2, lwd = 2)
      lines(c(n, n + seq_len(H)), c(r$fitted[n], r$future),
            col = "#B4501E", lty = 2, lwd = 2, type = "o", pch = 15)
      abline(v = n + 0.5, lty = 3)
      legend("topleft", c("Actual Demand", "One-Step Forecast", "Future Forecast"),
             col = c("grey30", "#2F6FB0", "#B4501E"),
             lty = c(1, 2, 2), pch = c(19, NA, 15), bty = "n")
    })
    
    output$error_table <- shiny::renderTable({
      r <- result()
      data.frame(Measure = c("MAD", "MSE", "MAPE (%)"),
                 Value = round(unname(r$metrics), 5))
    })
    
    output$seasonal_table <- shiny::renderTable({
      r <- result()
      data.frame(Season = seq_len(r$m), Initial_Seasonal_Factor = round(r$initial$seasonal, 5),
                 Latest_Seasonal_Factor = round(tail(r$seasonal, r$m), 5))
    })
    
    output$period_table <- shiny::renderTable({
      r <- result()
      n <- length(r$actual)
      data.frame(Period = seq_len(n), Demand = r$actual,
                 Forecast = round(r$fitted, 3),
                 Level = round(r$level, 3), Trend = round(r$trend, 3),
                 Seasonal = round(r$seasonal, 3),
                 Error = round(r$fitted - r$actual, 3))
    }, na = "")
    
    output$future_table <- shiny::renderTable({
      r <- result()
      n <- length(r$actual)
      data.frame(Period = n + seq_along(r$future),
                 Forecast = round(r$future, 3))
    })
  })
}
