#' Launch interactive reviz app
#'
#' @export
launch_reviz <- function() {
  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::titlePanel("reviz: Interactive Variable Explorer"),

      shiny::sidebarLayout(
        shiny::sidebarPanel(
          # Data package selector
          shiny::selectInput("data_pkg", "Data Package:",
                             choices = c("Base R" = "base",
                                         "critstats" = "critstats"),
                             selected = "base"),

          # Dataset selector (dynamic)
          shiny::selectInput("dataset", "Dataset:",
                             choices = c("mtcars" = "mtcars",
                                         "iris" = "iris"),
                             selected = "mtcars"),

          shiny::selectInput("x", "X Variable:",
                             choices = names(mtcars),
                             selected = "wt"),
          shiny::selectInput("y", "Y Variable:",
                             choices = names(mtcars),
                             selected = "mpg"),

          shiny::radioButtons("model", "Model:",
                              choices = c("Linear" = "lm", "LOESS" = "loess"),
                              selected = "lm")
        ),

        shiny::mainPanel(
          shiny::plotOutput("plot", height = "600px"),
          shiny::verbatimTextOutput("info")
        )
      )
    ),

    server = function(input, output, session) {

      # Update datasets when package changes
      shiny::observeEvent(input$data_pkg, {
        if (input$data_pkg == "base") {
          shiny::updateSelectInput(session, "dataset",
                                   choices = c("mtcars" = "mtcars",
                                               "iris" = "iris",
                                               "USArrests" = "USArrests",
                                               "ChickWeight" = "ChickWeight",
                                               "ToothGrowth" = "ToothGrowth",
                                               "faithful" = "faithful",
                                               "cars" = "cars"),
                                   selected = "mtcars")
        } else if (input$data_pkg == "critstats") {
          datasets <- if (requireNamespace("critstats", quietly = TRUE)) {
            c("africa_data_all" = "africa_data_all",
              "africa_data_2020" = "africa_data_2020",
              "africa_data_2023" = "africa_data_2023",
              "presidential_elections" = "presidential_elections",
              "us_presidents" = "us_presidents",
              "world_pop" = "world_pop")
          } else {
            c("Install critstats first" = "africa_data_all")
          }
          shiny::updateSelectInput(session, "dataset",
                                   choices = datasets,
                                   selected = names(datasets)[1])
        }
      })

      # Update variables when dataset changes
      shiny::observeEvent(input$dataset, {
        data <- switch(input$data_pkg,
                       "base" = get(input$dataset, "package:datasets"),
                       "critstats" = if(requireNamespace("critstats", quietly = TRUE)) {
                         get(input$dataset, "package:critstats")
                       } else mtcars
        )
        shiny::req(data)
        vars <- names(data)
        shiny::updateSelectInput(session, "x", choices = vars, selected = vars[1])
        shiny::updateSelectInput(session, "y", choices = vars, selected = vars[2])
      })

      # Main plot
      output$plot <- shiny::renderPlot({
        # Load data
        data <- switch(input$data_pkg,
                       "base" = get(input$dataset, "package:datasets"),
                       "critstats" = if(requireNamespace("critstats", quietly = TRUE)) {
                         get(input$dataset, "package:critstats")
                       } else {
                         shiny::showNotification("Install critstats: remotes::install_github('professornaite/critstats')",
                                                 type = "warning", duration = 5)
                         mtcars
                       }
        )
        shiny::req(data, input$x, input$y)

        p <- ggplot2::ggplot(data, ggplot2::aes_string(x = input$x, y = input$y)) +
          ggplot2::geom_point(alpha = 0.7, size = 3) +
          ggplot2::geom_smooth(method = input$model, se = TRUE, size = 1.2) +
          ggplot2::theme_minimal(base_size = 14) +
          ggplot2::labs(
            title = paste(input$y, "~", input$x),
            subtitle = paste("Dataset:", input$dataset, "| Package:", input$data_pkg),
            x = input$x,
            y = input$y
          )
        p
      })

      # Plot info
      output$info <- shiny::renderPrint({
        "Click or hover on points for more info (feature coming soon!)"
      })
    }
  )
}
