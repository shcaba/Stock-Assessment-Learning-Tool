library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(plotly)

# Von Bertalanffy growth function
vb_growth <- function(age, Linf, K, t0) {
  Linf * (1 - exp(-K * (age - t0)))
}

ui <- page_sidebar(
  title = "Von Bertalanffy Growth Function Estimator",
  sidebar = sidebar(
    fileInput("file", "Upload CSV File", accept = c(".csv")),
    helpText("CSV should have columns: age, length"),
    hr(),
    h5("Parameter Settings"),
    checkboxInput("fix_Linf", "Fix L∞ (asymptotic length)", FALSE),
    numericInput("Linf_value", "L∞ value:", 100, min = 0),
    checkboxInput("fix_K", "Fix K (growth coefficient)", FALSE),
    numericInput("K_value", "K value:", 0.3, min = 0, step = 0.01),
    checkboxInput("fix_t0", "Fix t₀ (theoretical age at length 0)", FALSE),
    numericInput("t0_value", "t₀ value:", -0.5, step = 0.1),
    hr(),
    actionButton("estimate", "Estimate Parameters", class = "btn-primary")
  ),

  navset_card_tab(
    id = "main_tabs",
    nav_panel(
      "Data Preview",
      layout_columns(
        col_widths = c(4, 8),
        card(
          card_header("Uploaded Data"),
          tableOutput("data_table")
        ),
        card(
          card_header("Growth Curve"),
          plotlyOutput("growth_data", height = "500px")
        )
      )
    ),
    nav_panel(
      "Results",
      layout_columns(
        col_widths = c(6, 6, 6, 6),
        card(
          card_header("Growth Curve"),
          plotlyOutput("growth_plot", height = "500px")
        ),
        #        layout_columns(
        #         col_widths = c(12, 12),
        card(
          card_header("Parameter Estimates"),
          verbatimTextOutput("estimates")
        ),
        card(
          card_header("Residual Plot"),
          plotlyOutput("residual_plot", height = "400px")
        ),
        #),
        card(
          card_header("Model Statistics"),
          verbatimTextOutput("model_stats")
        )
      )
      #nav_panel(
      # "Model Diagnostics",

      #  )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$estimate, {
    req(data())
    nav_select("main_tabs", "Results")
  })

  # Reactive value to store uploaded data
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)

    # Validate that required columns exist
    if (!all(c("age", "length") %in% names(df))) {
      showNotification(
        "CSV must contain 'age' and 'length' columns",
        type = "error"
      )
      return(NULL)
    }

    df
  })

  # Display data preview
  output$data_table <- renderTable({
    req(data())
    data()
  })

  # Plot data
  output$growth_data <- renderPlotly({
    req(data())
    df <- data()

    p <- ggplot(df, aes(x = age, y = length)) +
      geom_point(size = 3, alpha = 0.6, color = "steelblue") +
      labs(
        title = "Von Bertalanffy Growth Curve",
        x = "Age",
        y = "Length"
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5))
    ggplotly(p)
  })

  # Fit model when button is clicked
  fitted_model <- eventReactive(input$estimate, {
    req(data())

    df <- data()

    # Prepare starting values and fixed parameters
    start_vals <- list()
    lower_bounds <- c()
    upper_bounds <- c()
    fixed <- list()

    if (input$fix_Linf) {
      fixed$Linf <- input$Linf_value
    } else {
      start_vals$Linf <- input$Linf_value
      lower_bounds <- c(lower_bounds, max(df$length) * 0.5)
      upper_bounds <- c(upper_bounds, max(df$length) * 3)
    }

    if (input$fix_K) {
      fixed$K <- input$K_value
    } else {
      start_vals$K <- input$K_value
      lower_bounds <- c(lower_bounds, 0.001)
      upper_bounds <- c(upper_bounds, 2)
    }

    if (input$fix_t0) {
      fixed$t0 <- input$t0_value
    } else {
      start_vals$t0 <- input$t0_value
      lower_bounds <- c(lower_bounds, -5)
      upper_bounds <- c(upper_bounds, 5)
    }

    # Create formula with fixed parameters
    if (length(fixed) > 0) {
      formula_str <- "length ~ vb_growth(age"
      if (!is.null(fixed$Linf)) {
        formula_str <- paste0(formula_str, ", ", fixed$Linf)
      } else {
        formula_str <- paste0(formula_str, ", Linf")
      }

      if (!is.null(fixed$K)) {
        formula_str <- paste0(formula_str, ", ", fixed$K)
      } else {
        formula_str <- paste0(formula_str, ", K")
      }

      if (!is.null(fixed$t0)) {
        formula_str <- paste0(formula_str, ", ", fixed$t0)
      } else {
        formula_str <- paste0(formula_str, ", t0")
      }

      formula_str <- paste0(formula_str, ")")
    } else {
      formula_str <- "length ~ vb_growth(age, Linf, K, t0)"
    }

    # Fit the model
    tryCatch(
      {
        fit <- nls(
          as.formula(formula_str),
          data = df,
          start = start_vals,
          lower = lower_bounds,
          upper = upper_bounds,
          algorithm = "port"
        )

        list(
          model = fit,
          fixed = fixed,
          data = df
        )
      },
      error = function(e) {
        showNotification(
          paste("Model fitting failed:", e$message),
          type = "error"
        )
        NULL
      }
    )
  })

  # Display parameter estimates
  output$estimates <- renderPrint({
    req(fitted_model())

    fm <- fitted_model()

    cat("Von Bertalanffy Growth Function Parameters\n")
    cat("===========================================\n\n")

    # Get estimated parameters
    if (!is.null(fm$model)) {
      params <- coef(fm$model)
      se <- summary(fm$model)$coefficients[, "Std. Error"]

      # Combine with fixed parameters
      all_params <- fm$fixed
      for (name in names(params)) {
        all_params[[name]] <- params[name]
      }

      cat("L∞ (Asymptotic Length):", sprintf("%.3f", all_params$Linf))
      if ("Linf" %in% names(params)) {
        cat(sprintf(" ± %.3f", se["Linf"]))
      }
      if (!is.null(fm$fixed$Linf)) {
        cat(" (FIXED)")
      }
      cat("\n")

      cat("K (Growth Coefficient): ", sprintf("%.4f", all_params$K))
      if ("K" %in% names(params)) {
        cat(sprintf(" ± %.4f", se["K"]))
      }
      if (!is.null(fm$fixed$K)) {
        cat(" (FIXED)")
      }
      cat("\n")

      cat("t₀ (Theoretical age at length 0):", sprintf("%.4f", all_params$t0))
      if ("t0" %in% names(params)) {
        cat(sprintf(" ± %.4f", se["t0"]))
      }
      if (!is.null(fm$fixed$t0)) {
        cat(" (FIXED)")
      }
      cat("\n\n")

      cat(
        "Residual Standard Error:",
        sprintf("%.4f", summary(fm$model)$sigma),
        "\n"
      )
    }
  })

  # Plot growth curve
  output$growth_plot <- renderPlotly({
    req(fitted_model())

    fm <- fitted_model()
    df <- fm$data

    # Get all parameters (estimated + fixed)
    params <- coef(fm$model)
    all_params <- fm$fixed
    for (name in names(params)) {
      all_params[[name]] <- params[name]
    }

    # Generate predicted values
    age_range <- seq(min(df$age), max(df$age), length.out = 100)
    predicted <- vb_growth(
      age_range,
      all_params$Linf,
      all_params$K,
      all_params$t0
    )

    pred_df <- data.frame(age = age_range, length = predicted)

    p <- ggplot(df, aes(x = age, y = length)) +
      geom_point(size = 3, alpha = 0.6, color = "steelblue") +
      geom_line(data = pred_df, color = "red", size = 1) +
      labs(
        title = "Von Bertalanffy Growth Curve",
        x = "Age",
        y = "Length"
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5))
    ggplotly(p)
  })

  # Residual plot
  output$residual_plot <- renderPlotly({
    req(fitted_model())

    fm <- fitted_model()
    residuals_df <- data.frame(
      fitted = fitted(fm$model),
      residuals = residuals(fm$model)
    )

    p <- ggplot(residuals_df, aes(x = fitted, y = residuals)) +
      geom_point(size = 3, alpha = 0.6, color = "seagreen") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(
        title = "Residual Plot",
        x = "Fitted Values",
        y = "Residuals"
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5))
    ggplotly(p)
  })

  # Model statistics
  output$model_stats <- renderPrint({
    req(fitted_model())

    fm <- fitted_model()

    cat("Model Diagnostics\n")
    cat("=================\n\n")

    summary(fm$model)
  })
}

shinyApp(ui, server)
