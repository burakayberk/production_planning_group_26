
# ============================================================
# EMU341 - Simple Exponential Smoothing (SES)
# ============================================================

# HELPER FUNCTIONS

ses_forecast <- function(d, alpha, initial_level) {
  n <- length(d)
  F <- rep(NA_real_, n + 1)
  
  F[2] <- alpha * d[1] +
    (1 - alpha) * initial_level
  
  for (t in 3:(n + 1)) {
    F[t] <- alpha * d[t - 1] +
      (1 - alpha) * F[t - 1]
  }
  
  F
}

ses_errors <- function(d, F) {
  actual <- d[2:length(d)]
  forecast <- F[2:length(d)]
  e <- forecast - actual
  
  nonzero <- actual != 0
  
  mape <- if (any(nonzero)) {
    mean(abs(e[nonzero] / actual[nonzero])) * 100
  } else {
    NA_real_
  }
  
  c(
    MAD = mean(abs(e)),
    MSE = mean(e^2),
    MAPE = mape
  )
}


# USER INTERFACE

simple_exponential_smoothing_ui <- function(id) {
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
        ns("alpha_mode"),
        "Alpha selection",
        choices = c(
          "Manual Alpha" = "manual",
          "Optimize Alpha" = "auto"
        ),
        selected = "manual"
      ),
      
      conditionalPanel(
        condition = "input.alpha_mode == 'manual'",
        ns = ns,
        numericInput(
          ns("alpha"),
          "Alpha (0 to 1)",
          value = 0.3,
          min = 0,
          max = 1,
          step = 0.05
        )
      ),
      
      conditionalPanel(
        condition = "input.alpha_mode == 'auto'",
        ns = ns,
        selectInput(
          ns("criterion"),
          "Optimization criterion",
          choices = c("MAD", "MSE", "MAPE")
        )
      ),
      
      radioButtons(
        ns("initial_mode"),
        "Initial level",
        choices = c(
          "First observation" = "first",
          "Regression estimate" = "regression",
          "Manual initial level" = "manual"
        ),
        selected = "first"
      ),
      
      conditionalPanel(
        condition = "input.initial_mode == 'manual'",
        ns = ns,
        numericInput(
          ns("initial_level"),
          "Initial level",
          value = 100
        )
      )
    ),
    
    mainPanel(
      
      h3("Simple Exponential Smoothing"),
      
      h4(textOutput(ns("title"))),
      
      tableOutput(ns("parameter_table")),
      
      plotOutput(
        ns("forecast_plot"),
        height = "350px"
      ),
      
      h4("Error Measures"),
      tableOutput(ns("error_table")),
      
      h4("Forecasts by Period"),
      tableOutput(ns("period_table"))
    )
  )
}


# SERVER

simple_exponential_smoothing_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    # Read data
    
    data <- reactive({
      req(input$file)
      
      ext <- tolower(
        tools::file_ext(input$file$name)
      )
      
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
        need(
          length(d) >= 3,
          "At least three observations are required."
        ),
        need(
          all(is.finite(d)),
          "Demand contains missing or invalid values."
        ),
        need(
          all(d >= 0),
          "Demand cannot be negative."
        )
      )
      
      as.numeric(d)
    })
    
    # Initial level
    
    initial_level <- reactive({
      d <- demand()
      
      if (input$initial_mode == "first") {
        
        d[1]
        
      } else if (input$initial_mode == "regression") {
        
        periods <- seq_along(d)
        model <- lm(d ~ periods)
        
        as.numeric(
          predict(
            model,
            newdata = data.frame(periods = 1)
          )
        )
        
      } else {
        
        req(input$initial_level)
        
        validate(
          need(
            is.finite(input$initial_level) &&
              input$initial_level >= 0,
            "Initial level must be a nonnegative number."
          )
        )
        
        input$initial_level
      }
    })
    
    # Manual or optimized alpha
    
    chosen_alpha <- reactive({
      d <- demand()
      level <- initial_level()
      
      if (input$alpha_mode == "manual") {
        
        req(input$alpha)
        
        validate(
          need(
            is.finite(input$alpha) &&
              input$alpha >= 0 &&
              input$alpha <= 1,
            "Alpha must be between 0 and 1."
          )
        )
        
        input$alpha
        
      } else {
        
        req(input$criterion)
        criterion <- input$criterion
        
        validate(
          need(
            criterion %in% c("MAD", "MSE", "MAPE"),
            "Select a valid optimization criterion."
          ),
          need(
            criterion != "MAPE" || any(d[-1] != 0),
            "MAPE cannot be calculated when all evaluated demands are zero."
          )
        )
        
        objective <- function(a) {
          F <- ses_forecast(d, a, level)
          errors <- ses_errors(d, F)
          
          as.numeric(errors[criterion])
        }
        
        # Grid search plus bounded local refinement
        grid <- seq(0, 1, by = 0.01)
        scores <- vapply(grid, objective, numeric(1))
        
        best_index <- which.min(scores)
        lower <- grid[max(1, best_index - 1)]
        upper <- grid[min(length(grid), best_index + 1)]
        
        candidates <- c(0, 1, grid[best_index])
        
        if (lower < upper) {
          fit <- optimize(
            f = objective,
            interval = c(lower, upper),
            tol = 1e-9
          )
          
          candidates <- c(candidates, fit$minimum)
        }
        
        candidate_scores <- vapply(
          candidates, objective, numeric(1)
        )
        
        candidates[which.min(candidate_scores)]
      }
    })
    
    # Forecast result
    
    result <- reactive({
      
      d <- demand()
      alpha <- chosen_alpha()
      level <- initial_level()
      
      F <- ses_forecast(d, alpha, level)
      
      list(
        d = d,
        F = F,
        alpha = alpha,
        initial_level = level,
        errors = ses_errors(d, F)
      )
    })
    
    # Title
    
    output$title <- renderText({
      r <- result()
      
      paste0(
        "Next-period forecast: ",
        round(tail(r$F, 1), 2)
      )
    })
    
    # Parameter table
    
    output$parameter_table <- renderTable({
      r <- result()
      
      data.frame(
        Parameter = c(
          "Alpha",
          "Initial level"
        ),
        Value = round(
          c(r$alpha, r$initial_level), 5
        )
      )
    })
    
    # Forecast plot
    
    output$forecast_plot <- renderPlot({
      r <- result()
      n <- length(r$d)
      
      plot(
        1:n, r$d,
        type = "o",
        pch = 19,
        col = "grey30",
        xlim = c(1, n + 1),
        ylim = range(c(r$d, r$F), na.rm = TRUE),
        xlab = "Period",
        ylab = "Demand",
        main = "Actual Demand vs SES Forecast"
      )
      
      lines(
        2:(n + 1),
        r$F[2:(n + 1)],
        type = "o",
        pch = 17,
        lty = 2,
        col = "#2F6FB0"
      )
      
      points(
        n + 1,
        tail(r$F, 1),
        pch = 17,
        col = "#B4501E",
        cex = 2
      )
      
      legend(
        "topleft",
        legend = c(
          "Actual Demand",
          "SES Forecast",
          "Next-period Forecast"
        ),
        col = c(
          "grey30",
          "#2F6FB0",
          "#B4501E"
        ),
        pch = c(19, 17, 17),
        lty = c(1, 2, NA),
        bty = "n"
      )
    })
    
    # Error measures
    
    output$error_table <- renderTable({
      r <- result()
      
      data.frame(
        Measure = c("MAD", "MSE", "MAPE (%)"),
        Value = round(unname(r$errors), 3)
      )
    })
    
    # Forecast table
    
    output$period_table <- renderTable({
      r <- result()
      n <- length(r$d)
      
      data.frame(
        "Period" = 1:(n + 1),
        "Demand" = c(r$d, NA),
        "Forecast" = round(r$F, 2),
        "Error (F - D)" = round(
          r$F - c(r$d, NA), 2
        ),
        check.names = FALSE
      )
    }, na = "")
    
  })
}
