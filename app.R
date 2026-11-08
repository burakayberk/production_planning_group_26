library(shiny)

ui <- navbarPage("Production Planning — Group 26",
  tabPanel("Learning Curve", learning_curve_ui("learning_curve")),
  navbarMenu("Forecasting",
    tabPanel("Moving Average", moving_average_ui("moving_average")),
    tabPanel("Weighted Moving Average", weighted_moving_average_ui("weighted_moving_average")),
    tabPanel("Simple Exponential Smoothing", simple_exponential_smoothing_ui("ses")),
    tabPanel("Seasonal Factors", seasonal_factors_ui("seasonal_factors")),
    tabPanel("Holt’s Method", holt_method_ui("holt")),
    tabPanel("Seasonal Decomposition", seasonal_decomposition_ui("seasonal_decomposition")),
    tabPanel("Winters’ Method", winters_method_ui("winters")),
    tabPanel("AR", ar_method_ui("ar")),
    tabPanel("MA (Box–Jenkins)", ma_method_ui("ma")),
    tabPanel("ARMA", arma_method_ui("arma")),
    tabPanel("ARIMA", arima_method_ui("arima"))
  ),
  navbarMenu("Aggregate Planning",
    tabPanel("Chase Plan", chase_plan_ui("chase")),
    tabPanel("Level Plan", level_plan_ui("level")),
    tabPanel("Mixed Plan", mixed_plan_ui("mixed")),
    tabPanel("Linear Programming", linear_programming_ui("lp")),
    tabPanel("Transportation", transportation_method_ui("transport")),
    tabPanel("Disaggregation", disaggregation_ui("disaggregation"))
  )
)
server <- function(input, output, session) {
  learning_curve_server("learning_curve")
  moving_average_server("moving_average")
  weighted_moving_average_server("weighted_moving_average")
  simple_exponential_smoothing_server("ses")
  seasonal_factors_server("seasonal_factors")
  holt_method_server("holt")
  seasonal_decomposition_server("seasonal_decomposition")
  winters_method_server("winters")
  ar_method_server("ar")
  ma_method_server("ma")
  arma_method_server("arma")
  arima_method_server("arima")
  chase_plan_server("chase")
  level_plan_server("level")
  mixed_plan_server("mixed")
  linear_programming_server("lp")
  transportation_method_server("transport")
  disaggregation_server("disaggregation")
}
shinyApp(ui, server)
