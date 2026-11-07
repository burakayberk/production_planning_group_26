
# ============================================================
# EMU341 - Seasonal Factors (Quick & Dirty)
# ============================================================

# 1. HELPER FUNCTIONS

seasonal_quick_dirty <- function(d, m) {
  
  n <- length(d)
  
  season <- ((seq_len(n) - 1) %% m) + 1
  
  overall_mean <- mean(d)
  
  seasonal_means <- as.numeric(
    tapply(d, factor(season, levels = 1:m), mean)
  )
  
  if (overall_mean == 0) {
    factors <- rep(1, m)
  } else {
    factors <- seasonal_means / overall_mean
  }
  
  fitted <- overall_mean * factors[season]
  
  next_seasons <- ((n + seq_len(m) - 1) %% m) + 1
  
  future <- overall_mean * factors[next_seasons]
  
  list(
    factors = factors,
    seasonal_means = seasonal_means,
    overall_mean = overall_mean,
    fitted = fitted,
    future = future,
    season = season,
    next_seasons = next_seasons
  )
}


seasonal_error_measures <- function(d, fitted) {
  
  errors <- fitted - d
  
  nonzero <- d != 0
  
  mape <- if (any(nonzero)) {
    mean(abs(errors[nonzero] / d[nonzero])) * 100
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

seasonal_factors_ui <- function(id) {
  
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
      
      numericInput(
        ns("m"),
        "Number of periods per season",
        value = 4,
        min = 2,
        step = 1
      ),
      
      helpText(
        "Examples: 4 for quarterly data, 12 for monthly data."
      ),
      
      helpText(
        "At least two complete seasonal cycles are required."
      )
      
    ),
    
    mainPanel(
      
      h3("Seasonal Factors - Quick & Dirty"),
      
      h4("Overall Average Demand"),
      textOutput(ns("overall_mean")),
      
      h4("Seasonal Factors"),
      tableOutput(ns("factors_table")),
      
      h4("Actual Demand and Seasonal Estimates"),
      
      plotOutput(
        ns("forecast_plot"),
        height = "350px"
      ),
      
      h4("Error Measures - In-Sample Fit"),
      tableOutput(ns("error_table")),
      
      h4("Historical Seasonal Estimates"),
      tableOutput(ns("period_table")),
      
      h4("Next Seasonal Cycle Forecast"),
      tableOutput(ns("future_table"))
      
    )
  )
}


# 3. SERVER

seasonal_factors_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    # Read Excel or CSV
    
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
    
    
    # Update numeric column selection
    
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
          length(d) >= 4,
          "At least four observations are required."
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
    
    
    # Seasonal calculations
    
    result <- reactive({
      
      d <- demand()
      
      req(input$m)
      
      m <- input$m
      n <- length(d)
      
      validate(
        
        need(
          is.finite(m) &&
            m >= 2 &&
            m == floor(m),
          "Season length must be a whole number of at least 2."
        ),
        
        need(
          n >= 2 * m,
          "At least two full seasonal cycles are required."
        ),
        
        need(
          n %% m == 0,
          "The number of observations must contain complete seasonal cycles."
        )
        
      )
      
      m <- as.integer(m)
      
      calc <- seasonal_quick_dirty(d, m)
      
      errors <- seasonal_error_measures(
        d, calc$fitted
      )
      
      list(
        d = d,
        m = m,
        calc = calc,
        errors = errors
      )
    })
    
    
    # Overall mean
    
    output$overall_mean <- renderText({
      
      r <- result()
      
      round(r$calc$overall_mean, 2)
    })
    
    
    # Seasonal factors table
    
    output$factors_table <- renderTable({
      
      r <- result()
      
      data.frame(
        Season = seq_len(r$m),
        
        Seasonal_Mean = round(
          r$calc$seasonal_means, 3
        ),
        
        Seasonal_Factor = round(
          r$calc$factors, 4
        ),
        
        check.names = FALSE
      )
    })
    
    
    # Plot
    
    output$forecast_plot <- renderPlot({
      
      r <- result()
      
      n <- length(r$d)
      m <- r$m
      
      x_actual <- seq_len(n)
      x_future <- n + seq_len(m)
      
      y_values <- c(
        r$d,
        r$calc$fitted,
        r$calc$future
      )
      
      plot(
        x_actual,
        r$d,
        type = "o",
        pch = 19,
        col = "grey30",
        xlim = c(1, n + m),
        ylim = range(y_values),
        xlab = "Period",
        ylab = "Demand",
        main = "Seasonal Factors Forecast"
      )
      
      lines(
        x_actual,
        r$calc$fitted,
        type = "o",
        pch = 17,
        lty = 2,
        col = "#2F6FB0"
      )
      
      lines(
        x_future,
        r$calc$future,
        type = "o",
        pch = 15,
        lty = 2,
        col = "#B4501E"
      )
      
      abline(
        v = n + 0.5,
        lty = 3,
        col = "grey50"
      )
      
      legend(
        "topleft",
        legend = c(
          "Actual Demand",
          "Historical Seasonal Estimate",
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
    
    
    # Error measures
    
    output$error_table <- renderTable({
      
      r <- result()
      
      data.frame(
        Measure = c(
          "MAD",
          "MSE",
          "MAPE (%)"
        ),
        
        Value = round(
          unname(r$errors), 3
        )
      )
    })
    
    
    # Historical estimates
    
    output$period_table <- renderTable({
      
      r <- result()
      
      data.frame(
        
        Period = seq_along(r$d),
        
        Season = r$calc$season,
        
        Demand = r$d,
        
        Fitted = round(
          r$calc$fitted, 2
        ),
        
        Error = round(
          r$calc$fitted - r$d, 2
        )
        
      )
    })
    
    
    # Future forecasts
    
    output$future_table <- renderTable({
      
      r <- result()
      
      n <- length(r$d)
      
      data.frame(
        
        Period = n + seq_len(r$m),
        
        Season = r$calc$next_seasons,
        
        Forecast = round(
          r$calc$future, 2
        )
        
      )
    })
    
  })
}
