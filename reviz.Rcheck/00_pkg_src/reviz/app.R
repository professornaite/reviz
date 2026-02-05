#' Launch interactive reviz interface
#'
#' @export
launch_reviz_app <- function() {
  app <- list(
    ui = fluidPage(
      titlePanel("reviz: Interactive Variable Explorer"),

      sidebarLayout(
        # LEFT: Controls
        shiny::sidebarPanel(
shiny::sidebarPanel(
  shiny::selectInput("data_pkg", "📦 Data Package:", selected = "base"),
  shiny::selectInput("dataset", "📊 Dataset:", selected = "mtcars"),
  shiny::selectInput("x", "📈 X Variable:", selected = "wt"),
  shiny::selectInput("y", "📉 Y Variable:", selected = "mpg"),
  shiny::radioButtons("model", "📏 Model:", selected = "lm")
),



          hr(),

          selectInput("xvar", "X Variable:", choices = NULL),
          selectInput("yvar", "Y Variable:", choices = NULL),
          selectInput("zvar", "Color/Facet (optional):",
                      choices = c("None" = "", "country", "med.age", "fertility.rate")),

          radioButtons("model", "Model Type:",
                       choices = c("Linear" = "lm", "LOESS" = "loess"),
                       selected = "lm"),

          radioButtons("color_or_facet", "Use Z variable for:",
                       choices = c("Color" = "color", "Facets" = "facet")),

          actionButton("update", "Update Plot", class = "btn-primary")
        ),

        # RIGHT: Live plot
        mainPanel(
          width = 9,
          plotOutput("plot", height = "600px", click = "plot_click"),
          verbatimTextOutput("info")
        )
      )
    ),

    server = function(input, output, session) {
      # Reactive data loading
      data <- reactive({
        if (input$dataset == "mtcars") mtcars
        else if (input$dataset == "iris") iris
        else if (input$dataset == "critstats::africa_data_all") {
          if (!requireNamespace("critstats", quietly = TRUE)) {
            showNotification("Install critstats: remotes::install_github('professornaite/critstats')",
                             type = "warning")
            return(NULL)
          }
          critstats::africa_data_all
        }
      })

      # Update variable choices when data changes
      observe({
        req(data())
        vars <- names(data())
        updateSelectInput(session, "xvar", choices = vars, selected = vars[1])
        updateSelectInput(session, "yvar", choices = vars, selected = vars[2])
        updateSelectInput(session, "zvar", choices = c("None" = "", vars))
      })

      # Warning for critstats
      output$critstats_warning <- renderUI({
        if (!requireNamespace("critstats", quietly = TRUE)) {
          div(style = "color: orange; font-weight: bold;",
              "Install critstats first: remotes::install_github('professornaite/critstats')")
        }
      })

      # Main plot
      output$plot <- renderPlot({
        req(input$xvar, input$yvar, data())

        p <- ggplot(data(), aes_string(x = input$xvar, y = input$yvar)) +
          geom_point(alpha = 0.7, size = 3) +
          theme_minimal(base_size = 14) +
          labs(title = paste(input$yvar, "~", input$xvar),
               subtitle = paste("Dataset:", input$dataset))

        # Add model smooth
        method <- if (input$model == "lm") "lm" else "loess"
        p <- p + geom_smooth(method = method, se = TRUE, size = 1.2)

        # Add z variable
        if (input$zvar != "" && input$zvar %in% names(data())) {
          if (input$color_or_facet == "color") {
            p <- p + aes_string(color = input$zvar)
          } else {
            p <- p + facet_wrap(as.formula(paste("~", input$zvar)))
          }
        }

        p
      })

      # Plot click info
      output$info <- renderPrint({
        if (!is.null(input$plot_click)) {
          cat("Click coordinates:\n")
          print(input$plot_click)
        }
      })
    }
  )

  shinyApp(ui = app$ui, server = app$server)
}
