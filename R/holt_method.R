
# ============================================================
# EMU341 - Holt's Method (Double Exponential Smoothing)
# ============================================================

# 1. HELPER FUNCTIONS

holt_forecast <- function(d, alpha, beta, L1, T1) {
  n <- length(d)
  
  level <- numeric(n)
  trend <- numeric(n)
  F <- rep(NA_real_, n + 1)
  
  level[1] <- L1
  trend[1] <- T1
  
  for (t in 2:n) {
    F[t] <- level[t - 1] + trend[t - 1]
    
    level[t] <- alpha * d[t] +
      (1 - alpha) * F[t]
    
    trend[t] <- beta * (level[t] - level[t - 1]) +
      (1 - beta) * trend[t - 1]
  }
  
  F[n + 1] <- level[n] + trend[n]
  
  list(
    F = F,
    level = level,
    trend = trend
  )
}


holt_errors <- function(d, F) {
  periods <- 2:length(d)
  actual <- d[periods]
  errors <- F[periods] - actual
  
  valid <- actual != 0
  
  mape <- if (any(valid)) {
    mean(abs(errors[valid] / actual[valid])) * 100
  } else {
    NA_real_
  }
  
  c(
    MAD = mean(abs(errors)),
    MSE = mean(errors^2),
    MAPE = mape
  )
}


# 2. USER INTERFACE

holt_method_ui <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      
      fileInput(
        ns("file"),
        "Upload an Excel or CSV file",
        accept = c(".xlsx", ".csv")
      ),
      
      selectInput(
        ns("column"),
        "Demand column",
        choices = NULL
      ),
      
      radioButtons(
        ns("parameter_mode"),
        "Parameter selection",
        choices = c(
          "Manual Alpha and Beta" = "manual",
          "Optimize Alpha and Beta" = "auto"
        ),
        selected = "manual"
      ),
      
      conditionalPanel(
        condition = "input.parameter_mode == 'manual'",
        ns = ns,
        
        numericInput(
          ns("alpha"),
          "Alpha (level smoothing)",
          value = 0.3,
          min = 0,
          max = 1,
          step = 0.05
        ),
        
        numericInput(
          ns("beta"),
          "Beta (trend smoothing)",
          value = 0.2,
          min = 0,
          max = 1,
          step = 0.05
        )
      ),
      
      conditionalPanel(
        condition = "input.parameter_mode == 'auto'",
        ns = ns,
        
        selectInput(
          ns("criterion"),
          "Optimization criterion",
          choices = c("MAD", "MSE", "MAPE")
        )
      ),
      
      radioButtons(
        ns("initial_mode"),
        "Initial level and trend",
        choices = c(
          "Regression Estimate" = "regression",
          "Manual Initial Values" = "manual"
        ),
        selected = "regression"
      ),
      
      conditionalPanel(
        condition = "input.initial_mode == 'manual'",
        ns = ns,
        
        numericInput(
          ns("initial_level"),
          "Initial level (L1)",
          value = 100
        ),
        
        numericInput(
          ns("initial_trend"),
          "Initial trend (T1)",
          value = 0
        )
      ),
      
      numericInput(
        ns("horizon"),
        "Future periods to forecast",
        value = 4,
        min = 1,
        max = 12,
        step = 1
      )
    ),
    
    mainPanel(
      
      h3("Holt's Double Exponential Smoothing"),
      
      h4(textOutput(ns("title"))),
      
      h4("Parameters Used"),
      tableOutput(ns("parameter_table")),
      
      h4("Actual Demand and Forecast"),
      plotOutput(
        ns("forecast_plot"),
        height = "350px"
      ),
      
      h4("Error Measures"),
      tableOutput(ns("error_table")),
      
      h4("Historical Results"),
      tableOutput(ns("period_table")),
      
      h4("Future Forecasts"),
      tableOutput(ns("future_table"))
    )
  )
}


# 3. SERVER

holt_method_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    # Read Excel or CSV
    
    data <- reactive({
      req(input$file)
      
      ext <- tolower(tools::file_ext(input$file$name))
      
      if (ext == "xlsx") {
        
        readxl::read_excel(input$file$datapath)
        
      } else if (ext == "csv") {
        
        first_line <- readLines(
          input$file$datapath,
          n = 1,
          warn = FALSE
        )
        
        separator <- if (
          grepl(";", first_line, fixed = TRUE)
        ) ";" else ","
        
        read.table(
          input$file$datapath,
          header = TRUE,
          sep = separator,
          dec = if (separator == ";") "," else ".",
          check.names = FALSE
        )
        
      } else {
        
        validate(
          need(FALSE, "Unsupported file type.")
        )
      }
    })
    
    
    # Numeric column selection
    
    observeEvent(data(), {
      numeric_cols <- names(data())[
        sapply(data(), is.numeric)
      ]
      
      updateSelectInput(
        session,
        "column",
        choices = numeric_cols,
        selected = tail(numeric_cols, 1)
      )
    })
    
    
    # Demand validation
    
    demand <- reactive({
      req(input$column)
      
      d <- data()[[input$column]]
      
      validate(
        need(length(d) >= 3,
             "At least three observations are required."),
        
        need(all(is.finite(d)),
             "Demand contains missing or invalid values."),
        
        need(all(d >= 0),
             "Demand cannot be negative.")
      )
      
      as.numeric(d)
    })
    
    
    # Initial level and trend
    
    initial_values <- reactive({
      d <- demand()
      
      req(input$initial_mode)
      
      if (input$initial_mode == "regression") {
        
        periods <- seq_along(d)
        model <- lm(d ~ periods)
        
        intercept <- unname(coef(model)[1])
        slope <- unname(coef(model)[2])
        
        L1 <- intercept + slope
        T1 <- slope
        
      } else {
        
        req(input$initial_level, input$initial_trend)
        
        L1 <- input$initial_level
        T1 <- input$initial_trend
        
        validate(
          need(
            is.finite(L1) && L1 >= 0,
            "Initial level must be a nonnegative number."
          ),
          
          need(
            is.finite(T1),
            "Initial trend must be a valid number."
          )
        )
      }
      
      list(level = L1, trend = T1)
    })
    
    
    # Manual or optimized Alpha and Beta
    
    parameters <- reactive({
      d <- demand()
      initial <- initial_values()
      
      req(input$parameter_mode)
      
      if (input$parameter_mode == "manual") {
        
        req(input$alpha, input$beta)
        
        alpha <- input$alpha
        beta <- input$beta
        
        validate(
          need(
            is.finite(alpha) &&
              alpha >= 0 && alpha <= 1,
            "Alpha must be between 0 and 1."
          ),
          
          need(
            is.finite(beta) &&
              beta >= 0 && beta <= 1,
            "Beta must be between 0 and 1."
          )
        )
        
      } else {
        
        req(input$criterion)
        criterion <- input$criterion
        
        validate(
          need(
            criterion %in% c("MAD", "MSE", "MAPE"),
            "Choose MAD, MSE or MAPE."
          ),
          
          need(
            criterion != "MAPE" || any(d[-1] != 0),
            "MAPE is unavailable when all evaluated demands are zero."
          )
        )
        
        objective <- function(p) {
          fit <- holt_forecast(
            d, p[1], p[2],
            initial$level,
            initial$trend
          )
          
          err <- holt_errors(d, fit$F)
          
          as.numeric(err[criterion])
        }
        
        # Start with a grid search
        grid <- expand.grid(
          alpha = seq(0, 1, by = 0.1),
          beta = seq(0, 1, by = 0.1)
        )
        
        scores <- apply(
          grid, 1,
          function(p) objective(as.numeric(p))
        )
        
        best <- as.numeric(
          unlist(grid[which.min(scores), ], use.names = FALSE)
        )
        
        # Refine using bounded optimization
        solution <- optim(
          par = best,
          fn = objective,
          method = "L-BFGS-B",
          lower = c(0, 0),
          upper = c(1, 1),
          control = list(maxit = 2000)
        )
        
        if (
          is.finite(solution$value) &&
          solution$value <= min(scores)
        ) {
          best <- solution$par
        }
        
        alpha <- best[1]
        beta <- best[2]
      }
      
      list(alpha = alpha, beta = beta)
    })
    
    
    # Forecast calculations
    
    result <- reactive({
      d <- demand()
      p <- parameters()
      initial <- initial_values()
      
      req(input$horizon)
      
      H <- input$horizon
      
      validate(
        need(
          is.finite(H) &&
            H >= 1 &&
            H <= 12 &&
            H == floor(H),
          "Forecast horizon must be a whole number from 1 to 12."
        )
      )
      
      fit <- holt_forecast(
        d, p$alpha, p$beta,
        initial$level,
        initial$trend
      )
      
      n <- length(d)
      h <- seq_len(as.integer(H))
      
      future <- fit$level[n] +
        h * fit$trend[n]
      
      list(
        d = d,
        fit = fit,
        future = future,
        alpha = p$alpha,
        beta = p$beta,
        initial = initial,
        errors = holt_errors(d, fit$F)
      )
    })
    
    
    # Next-period forecast
    
    output$title <- renderText({
      r <- result()
      
      paste0(
        "Next-period forecast: ",
        round(r$future[1], 2)
      )
    })
    
    
    # Parameters table
    
    output$parameter_table <- renderTable({
      r <- result()
      
      data.frame(
        Parameter = c(
          "Alpha",
          "Beta",
          "Initial Level",
          "Initial Trend"
        ),
        
        Value = round(
          c(
            r$alpha,
            r$beta,
            r$initial$level,
            r$initial$trend
          ), 5
        )
      )
    })
    
    
    # Forecast plot
    
    output$forecast_plot <- renderPlot({
      r <- result()
      
      n <- length(r$d)
      H <- length(r$future)
      
      plot(
        1:n, r$d,
        type = "o",
        pch = 19,
        col = "grey30",
        xlim = c(1, n + H),
        ylim = range(
          c(r$d, r$fit$F, r$future),
          na.rm = TRUE
        ),
        xlab = "Period",
        ylab = "Demand",
        main = "Holt Method - Actual and Forecast"
      )
      
      lines(
        2:n,
        r$fit$F[2:n],
        type = "o",
        pch = 17,
        lty = 2,
        col = "#2F6FB0"
      )
      
      lines(
        (n + 1):(n + H),
        r$future,
        type = "o",
        pch = 15,
        lty = 2,
        col = "#B4501E"
      )
      
      abline(v = n + 0.5, lty = 3)
      
      legend(
        "topleft",
        legend = c(
          "Actual Demand",
          "Historical Forecast",
          "Future Forecast"
        ),
        col = c(
          "grey30",
          "#2F6FB0",
          "#B4501E"
        ),
        pch = c(19, 17, 15),
        lty = c(1, 2, 2),
        bty = "n"
      )
    })
    
    
    # Error table
    
    output$error_table <- renderTable({
      r <- result()
      
      data.frame(
        Measure = c("MAD", "MSE", "MAPE (%)"),
        Value = round(unname(r$errors), 3)
      )
    })
    
    
    # Historical table
    
    output$period_table <- renderTable({
      r <- result()
      n <- length(r$d)
      
      data.frame(
        Period = 1:n,
        Demand = r$d,
        Forecast = round(r$fit$F[1:n], 2),
        Level = round(r$fit$level, 2),
        Trend = round(r$fit$trend, 2),
        Error = round(
          r$fit$F[1:n] - r$d, 2
        )
      )
    }, na = "")
    
    
    # Future table
    
    output$future_table <- renderTable({
      r <- result()
      
      n <- length(r$d)
      H <- length(r$future)
      
      data.frame(
        Period = n + seq_len(H),
        Forecast = round(r$future, 2)
      )
    })
    
  })
}
