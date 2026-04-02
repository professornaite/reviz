#' Launch interactive reviz app
#'
#' @export
launch_reviz <- function() {
  inequality_systems <- {
    n <- 500
    set.seed(42)
    median_income_base <- rnorm(n, mean = 55000, sd = 20000)
    median_income <- pmax(30000, pmin(120000, median_income_base))

    data.frame(
      county = paste("NC_County", sprintf("%03d", 1:n)),
      median_income = round(median_income, 0),
      gini_index = round(pmax(0.35, pmin(0.55, 0.45 - 0.0000005 * (median_income - 55000) + rnorm(n, 0, 0.04))), 3),
      property_tax_rate = round(pmax(0.01, pmin(0.04, 0.025 - 0.0000002 * (median_income - 55000) + rnorm(n, 0, 0.006))), 4),
      bachelor_pct = round(pmax(5, pmin(50, 25 + 0.0004 * (median_income - 55000) + rnorm(n, 0, 8))), 1),
      elderly_pct = round(pmax(10, pmin(35, 22 + rnorm(n, 0, 5))), 1),
      violent_crime_rate = round(pmax(1, pmin(15, 8 - 0.00004 * (median_income - 55000) + rnorm(n, 0, 2.5))), 1),
      median_home_value = round(pmax(100000, pmin(600000, 300000 + 3 * (median_income - 55000) + rnorm(n, 0, 50000))), 0),
      uninsured_pct = round(pmax(5, pmin(25, 15 - 0.00008 * (median_income - 55000) + rnorm(n, 0, 3))), 1),
      local_rev_per_capita = round(pmax(2000, pmin(8000, 5000 + 0.06 * (median_income - 55000) + rnorm(n, 0, 1000))), 0),
      stringsAsFactors = FALSE
    )
  }

  ui <- shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$script(shiny::HTML("
        function copyRevizCode() {
          var el = document.getElementById('r_code_text');
          if (!el) return;
          var txt = el.innerText || el.textContent;
          navigator.clipboard.writeText(txt).then(function() {
            Shiny.setInputValue('copy_code_clicked', Math.random());
          });
        }
      "))
    ),

    shiny::titlePanel("reviz: Statistical Test Explorer"),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,

        shiny::fileInput(
          "file_upload",
          "Upload CSV File",
          accept = c(".csv", ".txt"),
          buttonLabel = "Browse...",
          placeholder = "No file selected"
        ),

        shiny::radioButtons(
          "data_source",
          "Data Source",
          choices = c("Built-in Datasets" = "builtin", "Uploaded File" = "uploaded"),
          selected = "builtin"
        ),

        shiny::radioButtons(
          "data_pkg",
          "Dataset Library",
          choices = c(
            "Base R datasets" = "base",
            "Inequality Systems (reviz)" = "systems",
            "critstats datasets" = "critstats"
          ),
          selected = "base"
        ),

        shiny::selectInput(
          "dataset",
          "Dataset",
          choices = c(
            "mtcars",
            "iris",
            "USArrests",
            "ChickWeight",
            "ToothGrowth"
          )
        ),

        shiny::selectInput(
          "analysis_type",
          "Analysis Type",
          choices = c(
            "Correlation Test" = "correlation",
            "ANOVA" = "anova",
            "MANOVA" = "manova",
            "Linear Regression" = "linear_regression"
          ),
          selected = "correlation"
        ),

        shiny::uiOutput("analysis_controls"),

        shiny::actionButton("run_test", "Run Analysis", class = "btn-primary")
      ),

      shiny::mainPanel(
        width = 9,
        shiny::uiOutput("test_info"),

        shiny::h4("Dataset Structure"),
        shiny::verbatimTextOutput("data_structure"),

        shiny::h4("Five Number Summary"),
        shiny::verbatimTextOutput("five_number_summary"),

        shiny::h4("Results"),
        shiny::verbatimTextOutput("result_summary"),

        shiny::plotOutput("plot", height = "550px"),

        shiny::h4("Reproducible R Code"),
        shiny::actionButton("copy_code", "Copy Code", onclick = "copyRevizCode()"),
        shiny::tags$pre(id = "r_code_text", shiny::textOutput("r_code", container = shiny::span)),
        shiny::verbatimTextOutput("info")
      )
    )
  )

  server <- function(input, output, session) {

    uploaded_data <- shiny::reactive({
      shiny::req(input$file_upload)
      tryCatch({
        data <- read.csv(input$file_upload$datapath, stringsAsFactors = FALSE)
        if (ncol(data) < 2) stop("File must have at least 2 columns.")
        if (nrow(data) < 5) stop("File must have at least 5 rows.")
        data
      }, error = function(e) {
        shiny::showModal(
          shiny::modalDialog(
            title = "File Upload Error",
            paste("Error reading file:", e$message),
            easyClose = TRUE,
            footer = shiny::modalButton("Close")
          )
        )
        NULL
      })
    })

    get_data <- shiny::reactive({

      shiny::req(input$data_source)

      if (input$data_source == "uploaded" && !is.null(uploaded_data())) {
        return(uploaded_data())
      }

      if (!is.null(input$data_pkg) &&
          !is.null(input$dataset) &&
          input$data_pkg == "systems" &&
          input$dataset == "inequality_systems") {
        return(inequality_systems)
      } else if (input$data_pkg == "base") {
        return(get(input$dataset, "package:datasets"))
      } else if (input$data_pkg == "critstats") {
        if (requireNamespace("critstats", quietly = TRUE)) {
          return(get(input$dataset, "package:critstats"))
        } else {
          return(mtcars)
        }
      }

      mtcars
    })

    shiny::observeEvent(input$data_pkg, {
      if (input$data_source == "builtin") {
        if (input$data_pkg == "base") {
          shiny::updateSelectInput(
            session, "dataset",
            choices = c(
              "mtcars" = "mtcars",
              "iris" = "iris",
              "USArrests" = "USArrests",
              "ChickWeight" = "ChickWeight",
              "ToothGrowth" = "ToothGrowth"
            )
          )
        } else if (input$data_pkg == "systems") {
          shiny::updateSelectInput(
            session, "dataset",
            choices = c("inequality_systems" = "inequality_systems")
          )
        } else if (input$data_pkg == "critstats") {
          choices <- if (requireNamespace("critstats", quietly = TRUE)) {
            c(
              "africa_data_all" = "africa_data_all",
              "africa_data_2020" = "africa_data_2020"
            )
          } else {
            c("africa_data_all" = "africa_data_all (install critstats)")
          }
          shiny::updateSelectInput(session, "dataset", choices = choices)
        }
      }
    }, ignoreInit = FALSE)

    shiny::observeEvent(get_data(), {
      data <- get_data()
      shiny::req(data)
      vars <- names(data)

      shiny::updateSelectInput(session, "x", choices = vars, selected = vars[1])
      shiny::updateSelectInput(session, "y", choices = vars, selected = vars[min(2, length(vars))])

      shiny::updateSelectInput(session, "anova_y", choices = vars, selected = vars[1])
      shiny::updateSelectInput(session, "anova_group", choices = vars, selected = vars[min(2, length(vars))])

      shiny::updateSelectizeInput(session, "manova_y", choices = vars, selected = vars[seq_len(min(2, length(vars)))], server = TRUE)
      shiny::updateSelectInput(session, "manova_group", choices = vars, selected = vars[min(3, length(vars))])

      shiny::updateSelectInput(session, "lm_y", choices = vars, selected = vars[1])
      shiny::updateSelectizeInput(session, "lm_x", choices = vars, selected = vars[min(2, length(vars))], server = TRUE)
    }, ignoreInit = FALSE)

    make_numeric <- function(x) {
      if (is.numeric(x)) {
        return(list(ok = TRUE, values = x, message = NULL))
      }

      if (is.logical(x)) {
        return(list(ok = TRUE, values = as.numeric(x), message = NULL))
      }

      if (is.factor(x)) {
        x <- as.character(x)
      }

      suppressWarnings(num <- as.numeric(x))

      valid_original <- sum(!is.na(x))
      valid_numeric <- sum(!is.na(num))

      if (valid_original == 0 || valid_numeric == 0) {
        return(list(ok = FALSE, values = NULL, message = "Could not convert to numeric."))
      }

      if (valid_numeric < valid_original * 0.6) {
        return(list(ok = FALSE, values = NULL, message = "Numeric conversion failed for too many values."))
      }

      list(ok = TRUE, values = num, message = "Converted to numeric with some coercion.")
    }

    make_factor <- function(x) {
      if (is.factor(x)) return(factor(x))
      if (is.character(x) || is.logical(x)) return(factor(x))
      if (is.numeric(x)) return(factor(x))
      factor(x)
    }

    dataset_diagnostics <- function(df) {

      structure_text <- capture.output(str(df))

      numeric_cols <- df[sapply(df, is.numeric)]

      five_num <- if (ncol(numeric_cols) > 0) {
        apply(numeric_cols, 2, function(x) {
          stats::fivenum(x, na.rm = TRUE)
        })
      } else {
        "No numeric variables available."
      }

      list(
        structure = paste(structure_text, collapse = "\n"),
        five_number = capture.output(print(five_num))
      )
    }

    output$analysis_controls <- shiny::renderUI({
      data <- get_data()
      shiny::req(data)
      vars <- names(data)

      if (input$analysis_type == "correlation") {
        shiny::tagList(
          shiny::selectInput("x", "Variable 1", choices = vars),
          shiny::selectInput("y", "Variable 2", choices = vars),

          shiny::selectInput(
            "cor_method",
            "Correlation Method",
            choices = c(
              "Pearson" = "pearson",
              "Spearman Rank" = "spearman",
              "Kendall Tau" = "kendall"
            )
          ),

          shiny::numericInput(
            "conf_level",
            "Confidence Level",
            value = 0.95,
            min = 0.8,
            max = 0.99,
            step = 0.01
          )
        )
      } else if (input$analysis_type == "anova") {
        shiny::tagList(
          shiny::selectInput("anova_y", "Numeric Outcome", choices = vars, selected = vars[1]),
          shiny::selectInput("anova_group", "Grouping Variable", choices = vars, selected = vars[min(2, length(vars))])
        )
      } else if (input$analysis_type == "manova") {
        shiny::tagList(
          shiny::selectizeInput("manova_y", "Numeric Outcomes (2+)", choices = vars, multiple = TRUE, selected = vars[1:min(2, length(vars))]),
          shiny::selectInput("manova_group", "Grouping Variable", choices = vars, selected = vars[min(3, length(vars))])
        )
      } else if (input$analysis_type == "linear_regression") {
        shiny::tagList(
          shiny::selectInput("lm_y", "Numeric Outcome", choices = vars, selected = vars[1]),
          shiny::selectizeInput("lm_x", "Predictors", choices = vars, multiple = TRUE, selected = vars[min(2, length(vars))])
        )
      }
    })

    analysis_result <- shiny::eventReactive(input$run_test, {
      data <- get_data()
      shiny::req(data)

      type <- input$analysis_type

      if (type == "correlation") {
        x_raw <- data[[input$x]]
        y_raw <- data[[input$y]]

        x_num <- make_numeric(x_raw)
        y_num <- make_numeric(y_raw)

        if (!x_num$ok || !y_num$ok) {
          bad <- c()
          if (!x_num$ok) bad <- c(bad, paste0(input$x, " (", x_num$message, ")"))
          if (!y_num$ok) bad <- c(bad, paste0(input$y, " (", y_num$message, ")"))

          shiny::showModal(
            shiny::modalDialog(
              title = "Invalid Variables for Correlation",
              paste("The following variables could not be used as numeric inputs:", paste(bad, collapse = "; ")),
              easyClose = TRUE,
              footer = shiny::modalButton("Close")
            )
          )
          return(NULL)
        }

        test_data <- data.frame(x = x_num$values, y = y_num$values)
        test_data <- stats::na.omit(test_data)

        if (nrow(test_data) < 3) {
          shiny::showModal(
            shiny::modalDialog(
              title = "Not Enough Data",
              "Not enough complete numeric observations are available to run the correlation test.",
              easyClose = TRUE,
              footer = shiny::modalButton("Close")
            )
          )
          return(NULL)
        }

        fit <- stats::cor.test(test_data$x, test_data$y, method = input$cor_method)

        plot_obj <- ggplot2::ggplot(test_data, ggplot2::aes(x = x, y = y)) +
          ggplot2::geom_point(alpha = 0.7, size = 2.5, color = "#2E86AB") +
          ggplot2::geom_smooth(method = "lm", se = TRUE, color = "#A23B72", linewidth = 1.1) +
          ggplot2::theme_minimal(base_size = 13) +
          ggplot2::labs(
            title = paste("Correlation:", input$x, "and", input$y),
            subtitle = paste("Method:", tools::toTitleCase(input$cor_method), "| r =", round(unname(fit$estimate), 3), "| p =", signif(fit$p.value, 3)),
            x = input$x,
            y = input$y
          )

        code_text <- paste0(
          "x <- as.numeric(data[['", input$x, "']])\n",
          "y <- as.numeric(data[['", input$y, "']])\n",
          "test_data <- na.omit(data.frame(x = x, y = y))\n",
          "cor.test(test_data$x, test_data$y, method = '", input$cor_method, "')\n\n",
          "ggplot(test_data, aes(x = x, y = y)) +\n",
          "  geom_point(alpha = 0.7, size = 2.5, color = '#2E86AB') +\n",
          "  geom_smooth(method = 'lm', se = TRUE, color = '#A23B72', linewidth = 1.1) +\n",
          "  theme_minimal()"
        )

        return(list(
          summary = paste0(
            "Correlation test completed.\n",
            "Method: ", tools::toTitleCase(input$cor_method), "\n",
            "Estimate: ", round(unname(fit$estimate), 4), "\n",
            "p-value: ", signif(fit$p.value, 4), "\n",
            "95% CI: ", paste(round(fit$conf.int, 4), collapse = " to "), "\n",
            "Complete cases used: ", nrow(test_data)
          ),
          plot = plot_obj,
          code = code_text,
          info = paste(
            "Correlation measures the strength and direction of association between two variables.",
            "Pearson correlation is commonly used for linear relationships between quantitative variables,",
            "while rank-based methods such as Spearman and Kendall are useful when the relationship is monotonic or less sensitive to strict normality assumptions."
          )
        ))
      }

      if (type == "anova") {
        y_num <- make_numeric(data[[input$anova_y]])
        if (!y_num$ok) {
          shiny::showModal(
            shiny::modalDialog(
              title = "Invalid Outcome for ANOVA",
              paste(input$anova_y, "could not be used as a numeric outcome."),
              easyClose = TRUE,
              footer = shiny::modalButton("Close")
            )
          )
          return(NULL)
        }

        group <- make_factor(data[[input$anova_group]])
        test_data <- data.frame(y = y_num$values, group = group)
        test_data <- stats::na.omit(test_data)

        if (length(unique(test_data$group)) < 2) {
          shiny::showModal(
            shiny::modalDialog(
              title = "Invalid Grouping Variable",
              "ANOVA requires at least two groups in the grouping variable.",
              easyClose = TRUE,
              footer = shiny::modalButton("Close")
            )
          )
          return(NULL)
        }

        fit <- stats::aov(y ~ group, data = test_data)
        fit_sum <- summary(fit)

        plot_obj <- ggplot2::ggplot(test_data, ggplot2::aes(x = group, y = y, fill = group)) +
          ggplot2::geom_boxplot(alpha = 0.8) +
          ggplot2::theme_minimal(base_size = 13) +
          ggplot2::theme(legend.position = "none") +
          ggplot2::labs(
            title = paste("ANOVA:", input$anova_y, "by", input$anova_group),
            x = input$anova_group,
            y = input$anova_y
          )

        code_text <- paste0(
          "data$y <- as.numeric(data[['", input$anova_y, "']])\n",
          "data$group <- as.factor(data[['", input$anova_group, "']])\n",
          "test_data <- na.omit(data.frame(y = data$y, group = data$group))\n",
          "fit <- aov(y ~ group, data = test_data)\n",
          "summary(fit)\n\n",
          "ggplot(test_data, aes(x = group, y = y, fill = group)) +\n",
          "  geom_boxplot(alpha = 0.8) +\n",
          "  theme_minimal()"
        )

        p_val <- fit_sum[[1]][["Pr(>F)"]][1]
        f_val <- fit_sum[[1]][["F value"]][1]

        return(list(
          summary = paste0(
            "ANOVA completed.\n",
            "F statistic: ", round(f_val, 4), "\n",
            "p-value: ", signif(p_val, 4), "\n",
            "Groups: ", length(unique(test_data$group)), "\n",
            "Complete cases used: ", nrow(test_data)
          ),
          plot = plot_obj,
          code = code_text,
          info = paste(
            "ANOVA tests whether the mean of one numeric outcome differs across two or more groups.",
            "It is useful when comparing group means and is commonly implemented through a linear-model framework."
          )
        ))
      }

      if (type == "manova") {
        if (length(input$manova_y) < 2) {
          shiny::showModal(
            shiny::modalDialog(
              title = "Invalid MANOVA Setup",
              "MANOVA requires at least two numeric outcome variables.",
              easyClose = TRUE,
              footer = shiny::modalButton("Close")
            )
          )
          return(NULL)
        }

        y_list <- lapply(input$manova_y, function(v) make_numeric(data[[v]]))
        bad_idx <- which(!vapply(y_list, function(x) x$ok, logical(1)))

        if (length(bad_idx) > 0) {
          bad_vars <- input$manova_y[bad_idx]
          shiny::showModal(
            shiny::modalDialog(
              title = "Invalid Outcomes for MANOVA",
              paste("These outcomes could not be used as numeric variables:", paste(bad_vars, collapse = ", ")),
              easyClose = TRUE,
              footer = shiny::modalButton("Close")
            )
          )
          return(NULL)
        }

        y_df <- as.data.frame(setNames(lapply(y_list, `[[`, "values"), input$manova_y))
        group <- make_factor(data[[input$manova_group]])
        test_data <- cbind(y_df, group = group)
        test_data <- stats::na.omit(test_data)

        if (length(unique(test_data$group)) < 2) {
          shiny::showModal(
            shiny::modalDialog(
              title = "Invalid Grouping Variable",
              "MANOVA requires at least two groups.",
              easyClose = TRUE,
              footer = shiny::modalButton("Close")
            )
          )
          return(NULL)
        }

        lhs <- paste(sprintf("`%s`", input$manova_y), collapse = ", ")
        formula_txt <- paste0("cbind(", lhs, ") ~ group")
        fit <- stats::manova(stats::as.formula(formula_txt), data = test_data)
        fit_sum <- summary(fit, test = "Pillai")

        long_data <- stats::reshape(
          test_data,
          direction = "long",
          varying = input$manova_y,
          v.names = "value",
          times = input$manova_y,
          timevar = "outcome"
        )

        plot_obj <- ggplot2::ggplot(long_data, ggplot2::aes(x = group, y = value, fill = group)) +
          ggplot2::geom_boxplot(alpha = 0.8) +
          ggplot2::facet_wrap(~ outcome, scales = "free_y") +
          ggplot2::theme_minimal(base_size = 13) +
          ggplot2::theme(legend.position = "none") +
          ggplot2::labs(
            title = paste("MANOVA:", paste(input$manova_y, collapse = ", "), "by", input$manova_group),
            x = input$manova_group,
            y = "Value"
          )

        code_text <- paste0(
          "outcomes <- c(", paste(sprintf("'%s'", input$manova_y), collapse = ", "), ")\n",
          "test_data <- data[, c(outcomes, '", input$manova_group, "')]\n",
          "test_data[outcomes] <- lapply(test_data[outcomes], as.numeric)\n",
          "test_data[['", input$manova_group, "']] <- as.factor(test_data[['", input$manova_group, "']])\n",
          "test_data <- na.omit(test_data)\n",
          "fit <- manova(cbind(", paste(input$manova_y, collapse = ", "), ") ~ ", input$manova_group, ", data = test_data)\n",
          "summary(fit, test = 'Pillai')"
        )

        pillai <- fit_sum$stats[1, "Pillai"]
        approx_f <- fit_sum$stats[1, "approx F"]
        p_val <- fit_sum$stats[1, "Pr(>F)"]

        return(list(
          summary = paste0(
            "MANOVA completed.\n",
            "Test statistic (Pillai): ", round(pillai, 4), "\n",
            "Approximate F: ", round(approx_f, 4), "\n",
            "p-value: ", signif(p_val, 4), "\n",
            "Outcomes: ", paste(input$manova_y, collapse = ", "), "\n",
            "Complete cases used: ", nrow(test_data)
          ),
          plot = plot_obj,
          code = code_text,
          info = paste(
            "MANOVA extends ANOVA to multiple numeric outcomes tested jointly across groups.",
            "It is useful when several related dependent variables may differ together as a multivariate profile."
          )
        ))
      }

      if (type == "linear_regression") {
        if (length(input$lm_x) < 1) {
          shiny::showModal(
            shiny::modalDialog(
              title = "Invalid Regression Setup",
              "Linear regression requires at least one predictor.",
              easyClose = TRUE,
              footer = shiny::modalButton("Close")
            )
          )
          return(NULL)
        }

        y_num <- make_numeric(data[[input$lm_y]])
        if (!y_num$ok) {
          shiny::showModal(
            shiny::modalDialog(
              title = "Invalid Outcome for Regression",
              paste(input$lm_y, "could not be used as a numeric outcome."),
              easyClose = TRUE,
              footer = shiny::modalButton("Close")
            )
          )
          return(NULL)
        }

        test_data <- data[, unique(c(input$lm_y, input$lm_x)), drop = FALSE]
        test_data[[input$lm_y]] <- y_num$values
        test_data <- stats::na.omit(test_data)

        formula_txt <- paste(input$lm_y, "~", paste(input$lm_x, collapse = " + "))
        fit <- stats::lm(stats::as.formula(formula_txt), data = test_data)
        fit_sum <- summary(fit)

        if (length(input$lm_x) == 1 && is.numeric(test_data[[input$lm_x[1]]])) {
          plot_obj <- ggplot2::ggplot(test_data, ggplot2::aes(x = .data[[input$lm_x[1]]], y = .data[[input$lm_y]])) +
            ggplot2::geom_point(alpha = 0.7, size = 2.5, color = "#2E86AB") +
            ggplot2::geom_smooth(method = "lm", se = TRUE, color = "#A23B72", linewidth = 1.1) +
            ggplot2::theme_minimal(base_size = 13) +
            ggplot2::labs(
              title = paste("Linear Regression:", input$lm_y, "on", input$lm_x[1]),
              x = input$lm_x[1],
              y = input$lm_y
            )
        } else {
          resid_df <- data.frame(
            fitted = stats::fitted(fit),
            residuals = stats::residuals(fit)
          )

          plot_obj <- ggplot2::ggplot(resid_df, ggplot2::aes(x = fitted, y = residuals)) +
            ggplot2::geom_point(alpha = 0.7, size = 2.5, color = "#2E86AB") +
            ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "#A23B72") +
            ggplot2::theme_minimal(base_size = 13) +
            ggplot2::labs(
              title = "Residual Plot",
              x = "Fitted values",
              y = "Residuals"
            )
        }

        code_text <- paste0(
          "test_data <- data[, c('", paste(unique(c(input$lm_y, input$lm_x)), collapse = "', '"), "')]\n",
          "test_data[['", input$lm_y, "']] <- as.numeric(test_data[['", input$lm_y, "']])\n",
          "test_data <- na.omit(test_data)\n",
          "fit <- lm(", formula_txt, ", data = test_data)\n",
          "summary(fit)"
        )

        return(list(
          summary = paste0(
            "Linear regression completed.\n",
            "Formula: ", formula_txt, "\n",
            "R-squared: ", round(fit_sum$r.squared, 4), "\n",
            "Adjusted R-squared: ", round(fit_sum$adj.r.squared, 4), "\n",
            "Model p-value: ", signif(stats::pf(fit_sum$fstatistic[1], fit_sum$fstatistic[2], fit_sum$fstatistic[3], lower.tail = FALSE), 4), "\n",
            "Complete cases used: ", nrow(test_data)
          ),
          plot = plot_obj,
          code = code_text,
          info = paste(
            "Linear regression estimates the relationship between a numeric outcome and one or more predictors.",
            "It is commonly used for explanation, prediction, and estimation of conditional mean differences under a linear model."
          )
        ))
      }

      NULL
    })

    output$result_summary <- shiny::renderText({
      res <- analysis_result()
      shiny::req(res)
      res$summary
    })

    output$plot <- shiny::renderPlot({
      res <- analysis_result()
      shiny::req(res)
      res$plot
    })

    output$r_code <- shiny::renderText({
      res <- analysis_result()
      shiny::req(res)
      res$code
    })

    output$test_info <- shiny::renderUI({
      res <- analysis_result()
      shiny::req(res)
      shiny::wellPanel(
        shiny::strong("About this test"),
        shiny::p(res$info)
      )
    })

    output$info <- shiny::renderText({
      data <- get_data()
      source_label <- ifelse(input$data_source == "uploaded", "User Upload", paste(input$data_pkg, input$dataset))
      paste(
        "Data Source:", source_label,
        "\nVariables:", paste(names(data), collapse = ", "),
        "\nObservations:", nrow(data),
        "\nSelected analysis:", input$analysis_type
      )
    })

    output$data_structure <- shiny::renderText({
      df <- get_data()
      shiny::req(df)
      diag <- dataset_diagnostics(df)
      diag$structure
    })

    output$five_number_summary <- shiny::renderText({
      df <- get_data()
      diag <- dataset_diagnostics(df)
      paste(diag$five_number, collapse = "\n")
    })

    shiny::observeEvent(input$copy_code_clicked, {
      shiny::showNotification("Code copied to clipboard.", type = "message")
    })
  }

  shiny::shinyApp(ui = ui, server = server)
}
