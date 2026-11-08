library(shiny)

ui <- navbarPage("Production Planning",
                 tabPanel("Learning Curve",
                          learning_curve_ui("learning_curve")),
                 
                 tabPanel("Moving Average",
                          moving_average_ui("moving_average")),
                 
                 tabPanel("Test Module",
                          test_module_ui("test_module")),
                 
                 tabPanel("Weighted Moving Average",
                          weighted_moving_average_ui("weighted_moving_average")),
                 tabPanel(
                   "Simple Exponential Smoothing",
                   simple_exponential_smoothing_ui("ses")),
                   tabPanel(
                     "Seasonal Factors",
                     seasonal_factors_ui("seasonal_factors")),
                 tabPanel(
                   "Holt's Method",
                   holt_method_ui("holt")),
                 tabPanel(
                   "Seasonal Decomposition",
                   seasonal_decomposition_ui("seasonal_decomposition")), 
                 tabPanel(
                   "Winters' Method",
                   winters_method_ui("winters")),
                 tabPanel(
                   "AR Method",
                   ar_method_ui("ar")),
                 tabPanel(
                   "MA Method",
                   ma_method_ui("ma")),
                 
                 
              
                   )
                
server <- function(input, output, session) {
  learning_curve_server("learning_curve")
  moving_average_server("moving_average")
  test_module_server("test_module")
  weighted_moving_average_server("weighted_moving_average")
  simple_exponential_smoothing_server("ses")
  seasonal_factors_server("seasonal_factors")
  holt_method_server("holt")
  seasonal_decomposition_server("seasonal_decomposition")
  winters_method_server("winters")
  ar_method_server("ar")
  ma_method_server("ma")
}

shinyApp(ui, server)
