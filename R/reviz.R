#' Launch interactive reviz app
#'
#' @export
launch_reviz <- function() {
  shinyApp(
    ui = fluidPage(
      titlePanel("reviz: Variable Explorer"),

      sidebarLayout(
        sidebarPanel(
          selectInput("x", "X Variable:",
                      choices = names(mtcars),
                      selected = "wt"),
          selectInput("y", "Y Variable:",
                      choices = names(mtcars),
                      selected = "mpg"),
          radioButtons("model", "Model:",
                       choices = c("Linear" = "lm", "LOESS" = "loess"),
                       selected = "lm")
        ),

        mainPanel(
          plotOutput("plot", height = "500px")
        )
      )
    ),

    server = function(input, output) {
      output$plot <- renderPlot({
        p <- ggplot(mtcars, aes_string(x = input$x, y = input$y)) +
          geom_point(alpha = 0.7, size = 3) +
          geom_smooth(method = input$model, se = TRUE) +
          theme_minimal(base_size = 14) +
          labs(title = paste(input$y, "~", input$x))
        p
      })
    }
  )
}
