#' runPlotDens
#'
#' Shiny version of PlotDens
#' see ?PlotDens
#'
#' @return
#' @export
#' @import ggplot2
#' @import shiny
#'
#' @examples runPlotDens()
runPlotDens <- function() {
  ui <- fluidPage(
    titlePanel("Normal vs Student"),
    sidebarPanel(
      sliderInput(
        inputId = "range",
        label = "Interval (a,b):",
        min = -5, max = 5, value = c(-1, 1), step = 0.5
      ),
      sliderInput(
        inputId = "mean",
        label = "Mean - Normal",
        min = -5, max = 5, value = 0, step = 0.5
      ),
      sliderInput(
        inputId = "var",
        label = "Variance - Normal",
        min = 0.1, max = 3, value = 1, step = 0.1
      ),
      sliderInput(
        inputId = "df",
        label = "Degree of freedom ('df') - Student",
        min = 1, max = 50, value = 1, step = 1
      ),
      radioButtons(inputId = 'plot',
                   label= 'Output',
                   choices = c("Overlay" = 1 , "Normal"=2 , "Student" = 3)
      )
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
  server <- function(input, output) {
    output$plot <- renderPlot({
      PlotDens(a = input$range[1] ,
               b = input$range[2] ,
               mu = input$mean ,
               var = input$var,
               df = input$df,
               output = input$plot)
    })
  }
  runApp(shinyApp(ui, server))
}
