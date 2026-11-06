
# ============================================================
# Weighted Moving Average (WMA) - EMU341
# ============================================================

# 1. HELPER FUNCTIONS

wma_forecast <- function(d, weights) {
  n <- length(d)
  N <- length(weights)
  
  F <- rep(NA_real_, n + 1)
  
  for (t in seq.int(N + 1, n + 1)) {
    past_demand <- d[(t - N):(t - 1)]
    F[t] <- sum(past_demand * weights)
  }
  
  F
}

wma_errors <- function(d, F, N) {
  periods <- seq.int(N + 1, length(d))
  errors <- F[periods] - d[periods]
  
  valid_mape <- d[periods] != 0
  
  mape <- if (any(valid_mape)) {
    mean(
      abs(errors[valid_mape] /
            d[periods][valid_mape])
    ) * 100
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

weighted_moving_average_ui <- function(id) {
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
        ns("N"),
        "N (number of past periods)",
        value = 3,
        min = 1,
        step = 1
      ),
      
      radioButtons(
        ns("weight_mode"),
        "How should weights be chosen?",
        choices = c(
          "Manual Weights" = "manual",
          "Optimize Weights" = "auto"
        ),
        selected = "manual"
      ),
      
      conditionalPanel(
        condition = "input.weight_mode == 'manual'",
        ns = ns,
        
        textInput(
          ns("weights"),
          "Weights (oldest to newest)",
          value = "0.2, 0.3, 0.5"
        ),
        
        helpText(
          "Enter N weights separated by commas. Their sum must equal 1."
        )
      ),
      
      conditionalPanel(
        condition = "input.weight_mode == 'auto'",
        ns = ns,
        
        selectInput(
          ns("criterion"),
          "Optimization criterion",
          choices = c("MAD", "MSE", "MAPE")
        )
      )
    ),
    
    mainPanel(
      h3("Weighted Moving Average"),
      
      h4(textOutput(ns("title"))),
      
      plotOutput(
        ns("forecast_plot"),
        height = "350px"
      ),
      
      h4("Weights used"),
      tableOutput(ns("weights_table")),
      
      h4("Error measures"),
      tableOutput(ns("error_table")),
      
      h4("Forecasts by period"),
      tableOutput(ns("period_table"))
    )
  )
}


# 3. SERVER

weighted_moving_average_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    # Read uploaded data
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
    
    # Update numeric column choices
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
    
    # Demand values
    demand <- reactive({
      req(input$column)
      
      d <- data()[[input$column]]
      
      validate(
        need(
          length(d) >= 2,
          "At least two observations are required."
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
    
    # Validate N and choose weights
    parameters <- reactive({
      
      req(input$N, input$weight_mode)
      
      d <- demand()
      N <- input$N
      
      validate(
        need(
          is.finite(N) &&
            N >= 1 &&
            N == floor(N) &&
            N < length(d),
          "N must be a positive whole number smaller than the number of observations."
        )
      )
      
      N <- as.integer(N)
      
      if (input$weight_mode == "manual") {
        
        req(input$weights)
        
        weights <- suppressWarnings(
          as.numeric(
            trimws(
              strsplit(
                input$weights, ",", fixed = TRUE
              )[[1]]
            )
          )
        )
        
        validate(
          need(
            length(weights) == N &&
              all(is.finite(weights)),
            "Enter exactly N valid numeric weights."
          ),
          need(
            all(weights >= 0),
            "Weights cannot be negative."
          ),
          need(
            abs(sum(weights) - 1) < 0.000001,
            "The weights must sum to 1."
          )
        )
        
      } else {
        
        req(input$criterion)
        
        criterion <- input$criterion
        
        validate(
          need(
            criterion %in% c("MAD", "MSE", "MAPE"),
            "Select a valid optimization criterion."
          ),
          need(
            criterion != "MAPE" ||
              any(d[(N + 1):length(d)] != 0),
            "MAPE cannot be calculated when all evaluated demand values are zero."
          )
        )
        
        
        # Optimize nonnegative weights that sum to 1
        
        if (N == 1) {
          
          weights <- 1
          
        } else {
          
          # Use N-1 decision variables.
          # The last weight is 1 minus their sum.
          objective <- function(x) {
            
            w <- c(x, 1 - sum(x))
            
            F <- wma_forecast(d, w)
            errors <- wma_errors(d, F, N)
            
            as.numeric(errors[criterion])
          }
          
          # Constraints:
          # x[i] >= 0
          # sum(x) <= 1
          ui_matrix <- rbind(
            diag(N - 1),
            -rep(1, N - 1)
          )
          
          ci_vector <- c(
            rep(0, N - 1),
            -1
          )
          
          # Start strictly inside the feasible region
          initial <- rep(1 / N, N - 1)
          
          solution <- constrOptim(
            theta = initial,
            f = objective,
            grad = NULL,
            ui = ui_matrix,
            ci = ci_vector,
            method = "Nelder-Mead",
            control = list(
              maxit = 10000,
              reltol = 1e-12
            )
          )
          
          weights <- c(
            solution$par,
            1 - sum(solution$par)
          )
          
          # Remove tiny numerical deviations
          weights <- pmax(weights, 0)
          weights <- weights / sum(weights)
        }
        
      }
      
      list(
        N = N,
        weights = weights
      )
    })
    
    # Calculate forecasts
    result <- reactive({
      
      d <- demand()
      p <- parameters()
      
      F <- wma_forecast(d, p$weights)
      
      list(
        d = d,
        N = p$N,
        F = F,
        errors = wma_errors(d, F, p$N)
      )
    })
    
    # Forecast title
    output$title <- renderText({
      
      r <- result()
      
      paste0(
        "WMA(", r$N,
        ") - Next-period forecast: ",
        round(tail(r$F, 1), 2)
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
        ylim = range(
          c(r$d, r$F),
          na.rm = TRUE
        ),
        xlab = "Period",
        ylab = "Demand",
        main = paste0(
          "Actual Demand and WMA(", r$N, ")"
        )
      )
      
      lines(
        1:(n + 1),
        r$F,
        type = "o",
        pch = 17,
        lty = 2,
        col = "#2F6FB0"
      )
      
      points(
        n + 1,
        tail(r$F, 1),
        pch = 17,
        cex = 2,
        col = "#B4501E"
      )
      
      legend(
        "topleft",
        legend = c(
          "Actual Demand",
          "WMA Forecast"
        ),
        col = c(
          "grey30",
          "#2F6FB0"
        ),
        pch = c(19, 17),
        lty = c(1, 2),
        bty = "n"
      )
    })
    
    # Weights table
    output$weights_table <- renderTable({
      
      p <- parameters()
      
      data.frame(
        "Period lag" = paste0(
          "t-", seq(p$N, 1)
        ),
        "Weight" = round(
          p$weights, 5
        ),
        check.names = FALSE
      )
    })
    
    # Error measures
    output$error_table <- renderTable({
      
      r <- result()
      
      data.frame(
        Measure = c(
          "MAD", "MSE", "MAPE (%)"
        ),
        Value = round(
          unname(r$errors), 3
        ),
        check.names = FALSE
      )
    })
    
    # Forecast table
    output$period_table <- renderTable({
      
      r <- result()
      n <- length(r$d)
      
      data.frame(
        "Period" = 1:(n + 1),
        "Demand (D)" = c(r$d, NA),
        "Forecast (F)" = round(r$F, 2),
        "Error (F - D)" = round(
          r$F - c(r$d, NA), 2
        ),
        check.names = FALSE
      )
    }, na = "")
    
  })
}
