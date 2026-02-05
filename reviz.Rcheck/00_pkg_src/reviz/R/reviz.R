#' Launch interactive reviz app
#'
#' @export
launch_reviz <- function() {
  # SYSTEMS INEQUALITY DATASET - 500 North Carolina Counties
  inequality_systems <- {
    n <- 500
    set.seed(42)
    median_income_base <- rnorm(n, mean = 55000, sd = 20000)
    median_income <- pmax(30000, pmin(120000, median_income_base))

    data.frame(
      county = paste("NC_County", sprintf("%03d", 1:n)),
      median_income = round(median_income, 0),
      gini_index = round(pmax(0.35, pmin(0.55, 0.45 - 0.0000005*(median_income-55000) + rnorm(n, 0, 0.04))), 3),
      property_tax_rate = round(pmax(0.01, pmin(0.04, 0.025 - 0.0000002*(median_income-55000) + rnorm(n, 0, 0.006))), 4),
      bachelor_pct = round(pmax(5, pmin(50, 25 + 0.0004*(median_income-55000) + rnorm(n, 0, 8))), 1),
      elderly_pct = round(pmax(10, pmin(35, 22 + rnorm(n, 0, 5))), 1),
      violent_crime_rate = round(pmax(1, pmin(15, 8 - 0.00004*(median_income-55000) + rnorm(n, 0, 2.5))), 1),
      median_home_value = round(pmax(100000, pmin(600000, 300000 + 3*(median_income-55000) + rnorm(n, 0, 50000))), 0),
      uninsured_pct = round(pmax(5, pmin(25, 15 - 0.00008*(median_income-55000) + rnorm(n, 0, 3))), 1),
      local_rev_per_capita = round(pmax(2000, pmin(8000, 5000 + 0.06*(median_income-55000) + rnorm(n, 0, 1000))), 0),
      stringsAsFactors = FALSE
    )
  }

  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::titlePanel("reviz: Variable Relationship Explorer"),

      shiny::sidebarLayout(
        shiny::sidebarPanel(
          width = 3,
          # File upload section
          shiny::fileInput("file_upload", "Upload CSV File",
                           accept = c(".csv", ".txt"),
                           buttonLabel = "Browse...",
                           placeholder = "No file selected"),

          shiny::radioButtons("data_source", "Data Source",
                              choices = c("Built-in Datasets" = "builtin",
                                          "Uploaded File" = "uploaded"),
                              selected = "builtin"),

          # Only show package selector when using built-in
          shiny::conditionalPanel(
            condition = "input.data_source == 'builtin'",
            shiny::selectInput("data_pkg", "Data Package",
                               choices = c("Base R" = "base",
                                           "Systems" = "systems",
                                           "critstats" = "critstats"),
                               selected = "base")
          ),

          shiny::conditionalPanel(
            condition = "input.data_source == 'builtin'",
            shiny::selectInput("dataset", "Dataset",
                               choices = c("mtcars" = "mtcars", "iris" = "iris"),
                               selected = "mtcars")
          ),

          shiny::selectInput("x", "X Variable", choices = names(mtcars), selected = "wt"),
          shiny::selectInput("y", "Y Variable", choices = names(mtcars), selected = "mpg"),

          shiny::radioButtons("model", "Model Type",
                              choices = c("Linear" = "lm",
                                          "LOESS (local)" = "loess",
                                          "Quadratic" = "poly2",
                                          "Cubic" = "poly3",
                                          "Robust Linear" = "rlm"),
                              selected = "lm")
        ),

        shiny::mainPanel(
          width = 9,
          shiny::plotOutput("plot", height = "650px"),
          shiny::h4("Reproducible R Code:"),
          shiny::verbatimTextOutput("r_code"),
          shiny::verbatimTextOutput("info")
        )
      )
    ),

    server = function(input, output, session) {

      # Reactive uploaded data
      uploaded_data <- shiny::reactive({
        req(input$file_upload)
        file <- input$file_upload
        tryCatch({
          data <- read.csv(file$datapath, stringsAsFactors = FALSE)
          if(ncol(data) < 2) stop("File must have at least 2 columns")
          if(nrow(data) < 5) stop("File must have at least 5 rows")
          data
        }, error = function(e) {
          shiny::showNotification(paste("Error reading file:", e$message), type = "error")
          NULL
        })
      })

      # Get data based on source
      get_data <- shiny::reactive({
        if(input$data_source == "uploaded" && !is.null(uploaded_data())) {
          uploaded_data()
        } else {
          if (input$data_pkg == "systems" && input$dataset == "inequality_systems") {
            return(inequality_systems)
          } else if (input$data_pkg == "base") {
            return(get(input$dataset, "package:datasets"))
          } else if (input$data_pkg == "critstats") {
            if(requireNamespace("critstats", quietly = TRUE)) {
              return(get(input$dataset, "package:critstats"))
            } else {
              return(mtcars)
            }
          }
          mtcars
        }
      })

      # Update datasets when package changes (built-in only)
      shiny::observeEvent(input$data_pkg, {
        if(input$data_source == "builtin") {
          if (input$data_pkg == "base") {
            shiny::updateSelectInput(session, "dataset",
                                     choices = c("mtcars" = "mtcars",
                                                 "iris" = "iris",
                                                 "USArrests" = "USArrests",
                                                 "ChickWeight" = "ChickWeight",
                                                 "ToothGrowth" = "ToothGrowth"))
          } else if (input$data_pkg == "systems") {
            shiny::updateSelectInput(session, "dataset",
                                     choices = c("inequality_systems" = "inequality_systems"))
          } else if (input$data_pkg == "critstats") {
            choices <- if (requireNamespace("critstats", quietly = TRUE)) {
              c("africa_data_all" = "africa_data_all",
                "africa_data_2020" = "africa_data_2020")
            } else {
              c("africa_data_all" = "africa_data_all (install critstats)")
            }
            shiny::updateSelectInput(session, "dataset", choices = choices)
          }
        }
      })

      # Update variables when data changes
      shiny::observeEvent(list(input$data_source, get_data()), {
        data <- get_data()
        shiny::req(data)
        vars <- names(data)
        vars <- vars[!(vars %in% c("county"))]  # Remove non-numeric identifiers
        if(length(vars) >= 2) {
          shiny::updateSelectInput(session, "x", choices = vars, selected = vars[1])
          shiny::updateSelectInput(session, "y", choices = vars, selected = vars[2])
        }
      })

      # Main plot
      output$plot <- shiny::renderPlot({
        data <- get_data()
        shiny::req(data, input$x, input$y)

        p <- ggplot2::ggplot(data, ggplot2::aes_string(x = input$x, y = input$y)) +
          ggplot2::geom_point(alpha = 0.6, size = 2.5, color = "#2E86AB")

        # Add model smooth
        if (input$model == "lm") {
          p <- p + ggplot2::geom_smooth(method = "lm", se = TRUE, size = 1.1, color = "#A23B72")
        } else if (input$model == "loess") {
          p <- p + ggplot2::geom_smooth(method = "loess", se = TRUE, size = 1.1, color = "#A23B72")
        } else if (input$model %in% c("poly2", "poly3")) {
          degree <- ifelse(input$model == "poly2", 2, 3)
          p <- p + ggplot2::geom_smooth(method = "lm", formula = y ~ poly(x, degree),
                                        se = TRUE, size = 1.1, color = "#A23B72")
        } else if (input$model == "rlm") {
          p <- p + ggplot2::geom_smooth(method = MASS::rlm, formula = y ~ x,
                                        se = TRUE, size = 1.1, color = "#A23B72")
        }

        p <- p + ggplot2::theme_minimal(base_size = 13) +
          ggplot2::labs(
            title = paste(input$y, "vs", input$x),
            subtitle = paste("Source:", ifelse(input$data_source == "uploaded", "User Upload", paste(input$dataset, "(", input$data_pkg, ")")), " | N =", nrow(data)),
            x = input$x,
            y = input$y
          ) +
          ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 16),
            plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12),
            axis.title = ggplot2::element_text(size = 12)
          )
        p
      })

      # Reproducible R code
      output$r_code <- shiny::renderText({
        data_source <- input$data_source
        x <- input$x
        y <- input$y

        model_code <- switch(input$model,
                             "lm" = "geom_smooth(method = 'lm', se = TRUE)",
                             "loess" = "geom_smooth(method = 'loess', se = TRUE)",
                             "poly2" = "geom_smooth(method = 'lm', formula = y ~ poly(x, 2), se = TRUE)",
                             "poly3" = "geom_smooth(method = 'lm', formula = y ~ poly(x, 3), se = TRUE)",
                             "rlm" = "geom_smooth(method = MASS::rlm, formula = y ~ x, se = TRUE)"
        )

        data_comment <- if(data_source == "uploaded") {
          "# Load your CSV file\n# data <- read.csv('your_file.csv')"
        } else {
          paste0("# Load ", input$dataset, "\n# data <- ", input$dataset)
        }

        paste0(
          "# REVIZ: Exact reproducible code\n",
          "library(ggplot2)\n\n",
          data_comment, "\n\n",
          "# Create identical plot\n",
          "p <- ggplot(data, aes(x = ", x, ", y = ", y, ")) +\n",
          "  geom_point(alpha = 0.6, size = 2.5, color = '#2E86AB') +\n",
          "  ", model_code, " +\n",
          "  theme_minimal(base_size = 13) +\n",
          "  labs(\n",
          "    title = '", y, " vs ", x, "',\n",
          "    x = '", x, "', y = '", y, "'\n",
          "  ) +\n",
          "  theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 16))\n\n",
          "print(p)"
        )
      })

      # Dataset info
      output$info <- shiny::renderText({
        data <- get_data()
        source_label <- ifelse(input$data_source == "uploaded", "User Upload", paste(input$data_pkg, input$dataset))
        paste("Data Source:", source_label,
              "\nVariables:", paste(names(data), collapse = ", "),
              "\nObservations:", nrow(data), "| Model:", input$model)
      })
    }
  )
}
