library(shiny)
library(bslib)
library(bsicons)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)
library(r4ss)
library(shinyFiles)
library(shinyWidgets)
library(shinyjs)

ui <- page_navbar(
  title = "Stock Assessment Learning Tool",
  theme = bs_theme(bootswatch = "flatly"),

  ####################
  # Landing page tab #
  ####################
  nav_panel(
    title = "Homeport",
    value = "home",
    div(
      class = "container-fluid mt-4",
      h1(
        "Welcome to the Stock Assessment Learning Tool (SALT)",
        class = "text-center mb-5"
      ),
      p(
        "Click on any of the below topics to enter the learning modules:",
        class = "text-center lead mb-5"
      ),

      # Grid of clickable cards
      div(
        class = "row g-4",

        ################################
        # Life History Parameters Card #
        ################################
        div(
          class = "col-md-4",
          actionButton(
            "goto_LHP",
            label = div(
              card(
                card_header(
                  div(
                    #bs_icon("gender-ambiguous", size = "3em", class = "text-primary mb-3"),
                    icon("heart-pulse", class = "fa-3x fa-sharp fa-solid"),
                    h3("Life History Parameters", class = "card-title")
                  )
                ),
                card_body(
                  p(
                    "Life history parameters are the foundation of stock assessments and fisheries management. Learn about natural mortality (M), growth, maturity, and weight-length relationships, and how they go together to make life history strategies."
                  )
                )
              )
            ),
            class = "btn btn-link p-0 w-100",
            style = "text-decoration: none; color: inherit;"
          )
        ),

        ####################
        # Selectivity Card #
        ####################
        div(
          class = "col-md-4",
          actionButton(
            "goto_selectivity",
            label = div(
              card(
                card_header(
                  div(
                    #bs_icon("gender-ambiguous", size = "3em", class = "text-primary mb-3"),
                    #icon(,class="fa-3x fa-sharp fa-solid"),
                    tags$img(
                      src = "fishing_boat.png",
                      height = "100px",
                      width = "200px"
                    ),
                    h3("Selectivity", class = "card-title")
                  )
                ),
                card_body(
                  p(
                    "Learn about forms of gear selectivity and how it samples the population."
                  )
                )
              )
            ),
            class = "btn btn-link p-0 w-100",
            style = "text-decoration: none; color: inherit;"
          )
        ),

        ################
        # Productivity #
        ################
        div(
          class = "col-md-4",
          actionButton(
            "goto_prod_eqyield",
            label = div(
              card(
                card_header(
                  div(
                    tags$img(
                      src = "Eq_curveII.png",
                      height = "100px",
                      width = "200px"
                    ),
                    h3(
                      "Stock Productivity and Fishery Yield",
                      class = "card-title"
                    )
                  )
                ),
                card_body(
                  p(
                    "Find out what steepness means and understand the influence of stock productivty on fishery yield by changing steepness, life history, and selectivity parameters."
                  )
                )
              )
            ),
            class = "btn btn-link p-0 w-100",
            style = "text-decoration: none; color: inherit;"
          )
        ),

        ################
        # Uncertainty #
        ################
        div(
          class = "col-md-4",
          actionButton(
            "goto_uncertainty",
            label = div(
              card(
                card_header(
                  div(
                    tags$img(
                      src = "bullseye.png",
                      height = "100px",
                      width = "200px"
                    ),
                    h3(
                      "Understanding and Quantifying Uncertainty",
                      class = "card-title"
                    )
                  )
                ),
                card_body(
                  p(
                    "Understand how to describe uncertainty in data and model output using the concepts of bias and imprecision. Learn about the difference sources and ways to estimate uncertainty in stock assessments."
                  )
                )
              )
            ),
            class = "btn btn-link p-0 w-100",
            style = "text-decoration: none; color: inherit;"
          )
        ),

        ######################
        # Abundance sampling #
        ######################
        div(
          class = "col-md-4",
          actionButton(
            "goto_abundance",
            label = div(
              card(
                card_header(
                  div(
                    tags$img(
                      src = "sampling.png",
                      height = "100px",
                      width = "300px"
                    ),
                    h3("Sampling abundance", class = "card-title")
                  )
                ),
                card_body(
                  p(
                    "Explore how different types of sampling of fish abundance affects the accuracy of abundance estimation. Simulate fishery-dependent and fishery-independent sampling approaches."
                  )
                )
              )
            ),
            class = "btn btn-link p-0 w-100",
            style = "text-decoration: none; color: inherit;"
          )
        ),

        ######################
        # Bio compositions #
        ######################
        div(
          class = "col-md-4",
          actionButton(
            "goto_biocomps",
            label = div(
              card(
                card_header(
                  div(
                    tags$img(
                      src = "biocomps.png",
                      height = "100px",
                      width = "300px"
                    ),
                    h3("Age and length data", class = "card-title")
                  )
                ),
                card_body(
                  p(
                    "Create life histories and a fishery, then sample age and length data to compare fished and unfished populations."
                  )
                )
              )
            ),
            class = "btn btn-link p-0 w-100",
            style = "text-decoration: none; color: inherit;"
          )
        ),

        ##############
        # Indicators #
        ##############
        div(
          class = "col-md-4",
          actionButton(
            "goto_indicators",
            label = div(
              card(
                card_header(
                  div(
                    tags$img(
                      src = "Indicator_MP.png",
                      height = "100px",
                      width = "300px"
                    ),
                    h3("Indicators", class = "card-title")
                  )
                ),
                card_body(
                  p(
                    "Learn the basics of indicators and how they are interpreted by reference points and controls rules to meet management objectives."
                  )
                )
              )
            ),
            class = "btn btn-link p-0 w-100",
            style = "text-decoration: none; color: inherit;"
          )
        ),

        ####################
        # Reference Points #
        ####################
        div(
          class = "col-md-4",
          actionButton(
            "goto_refpts",
            label = div(
              card(
                card_header(
                  div(
                    tags$img(
                      src = "RefPts.png",
                      height = "100px",
                      width = "300px"
                    ),
                    h3("Reference Points", class = "card-title")
                  )
                ),
                card_body(
                  p(
                    "Explore and design reference points and harvest control rules and how they determine catch limits."
                  )
                )
              )
            ),
            class = "btn btn-link p-0 w-100",
            style = "text-decoration: none; color: inherit;"
          )
        ),

        ###############################
        # Scale, Status, Productivity #
        ###############################
        div(
          class = "col-md-4",
          actionButton(
            "goto_ssp",
            label = div(
              card(
                card_header(
                  div(
                    tags$img(
                      src = "SSP.png",
                      height = "100px",
                      width = "300px"
                    ),
                    h3("Scale, Status, Productivity", class = "card-title")
                  )
                ),
                card_body(
                  p(
                    "Understand stock assessment output and sensitivity and learn how to communicate stock assessments change by understanding the three main dimensions of stock assessment output."
                  )
                )
              )
            ),
            class = "btn btn-link p-0 w-100",
            style = "text-decoration: none; color: inherit;"
          )
        ),

        ####################
        # Baseline shifter #
        ####################
        div(
          class = "col-md-4",
          actionButton(
            "goto_baseline",
            label = div(
              card(
                card_header(
                  div(
                    tags$img(
                      src = "baseline.png",
                      height = "100px",
                      width = "300px"
                    ),
                    h3(
                      "Stock Assessment Baseline shifter",
                      class = "card-title"
                    )
                  )
                ),
                card_body(
                  p(
                    "Explore our preception of stock abundance and health when we have longer or shorter data sets of and/or experiences with the population and its dyanmics."
                  )
                )
              )
            ),
            class = "btn btn-link p-0 w-100",
            style = "text-decoration: none; color: inherit;"
          )
        ),

        ##############################
        # Stock Assessment Continuum #
        ##############################
        div(
          class = "col-md-4",
          actionButton(
            "goto_SAC",
            label = div(
              card(
                card_header(
                  div(
                    tags$img(
                      src = "SAC.jpg",
                      height = "100px",
                      width = "300px"
                    ),
                    h3("Stock Assessment Continuum", class = "card-title")
                  )
                ),
                card_body(
                  p(
                    "Navigate through the Stock Assessment Continuum by seeing stock assessment options based on data availability."
                  )
                )
              )
            ),
            class = "btn btn-link p-0 w-100",
            style = "text-decoration: none; color: inherit;"
          )
        ),
      )
    )
  ),

  ####################
  ####################
  #Set-up side panel #
  ####################
  ####################

  #####################
  # Life History tab #
  #####################
  nav_panel(
    title = "Life History",
    value = "LHP",
    page_sidebar(
      title = "Life History Parameters: Choose values for each life history parameter and see how the relationships change",

      sidebar = sidebar(
        width = 350,
        #  Parameters

        #Natural Mortality & Growth (von Bertalanffy) Parameters
        card(
          card_header("Natural Mortality (M)"),
          h6(
            "Use ",
            tags$a(
              href = "https://connect.fisheries.noaa.gov/natural-mortality-tool/",
              "The Natural Mortality Tool",
              target = "_blank"
            ),
            " to use empiricial methods of estimating natural mortality."
          ),
          numericInput(
            "M",
            "Natural Mortality Rate (M)",
            value = 0.2,
            min = 0.01,
            max = 2,
            step = 0.01
          )
        ),
        card(
          card_header("Growth Parameters. Lengths in cm."),
          numericInput(
            "Linf",
            "L∞ (Asymptotic Length)",
            value = 100,
            min = 10,
            max = 500
          ),
          numericInput(
            "K",
            "Growth Rate (K)",
            value = 0.15,
            min = 0.01,
            max = 2,
            step = 0.01
          ),
          numericInput(
            "t0",
            "t₀ (Theoretical age at length 0)",
            value = -1,
            min = -10,
            max = 1,
            step = 0.1
          )
        ),

        # Maturity Parameters
        card(
          card_header("Maturity Parameters. Lengths in cm"),
          numericInput(
            "L50",
            "L₅₀ (Length at 50% maturity)",
            value = 66,
            min = 5,
            max = 200
          ),
          numericInput(
            "L95",
            "L₉₅ (Length at 95% maturity)",
            value = 80,
            min = 10,
            max = 300
          )
        ),

        # Weight-Length Parameters
        card(
          card_header("Weight (kg)-Length (cm) Parameters"),
          numericInput(
            "a",
            "Parameter 'a'",
            value = 0.00001,
            min = 0.001,
            max = 1,
            step = 0.001
          ),
          numericInput(
            "b",
            "Parameter 'b'",
            value = 3,
            min = 1,
            max = 5,
            step = 0.1
          )
        ),
      ),

      # Main panel with plots
      card(
        card_header("Biological Relationships"),
        layout_columns(
          card(
            card_header("Natural Mortality"),
            plotlyOutput("mortality_plot")
          ),
          card(
            card_header("Growth Curve"),
            plotlyOutput("growth_plot")
          ),
          col_widths = c(6, 6)
        ),
        layout_columns(
          card(
            card_header("Maturity"),
            plotlyOutput("maturity_plot")
          ),
          card(
            card_header("Weight-Length Relationship"),
            plotlyOutput("weight_length_plot")
          ),
          col_widths = c(6, 6)
        )
      )
    )
  ),

  ###################
  # Selectivity tab #
  ###################
  nav_panel(
    title = "Selectivity",
    value = "selectivity",
    page_sidebar(
      title = "Selectivity Curve Designer: Explore different forms of fishery selectivity by changing parameter values",
      sidebar = sidebar(
        width = 300,
        h5("Selectivity Parameters"),

        # Choose between length or age
        radioButtons(
          "bin_type",
          "Bin Type:",
          choices = list("Length" = "length", "Age" = "age"),
          selected = "length"
        ),

        # Number of bins
        numericInput(
          "n_bins",
          "Bin step:",
          value = 2,
          min = 1,
          step = 1
        ),

        # Bin range inputs (will be updated based on bin_type)
        conditionalPanel(
          condition = "input.bin_type == 'length'",
          numericInput(
            "min_length",
            "Minimum Length (cm):",
            value = 10,
            min = 1
          ),
          numericInput(
            "max_length",
            "Maximum Length (cm):",
            value = 80,
            min = 1
          )
        ),

        conditionalPanel(
          condition = "input.bin_type == 'age'",
          numericInput("min_age", "Minimum Age (years):", value = 1, min = 0),
          numericInput("max_age", "Maximum Age (years):", value = 15, min = 1)
        ),

        hr(),

        h5("Quick Presets:"),
        actionButton("preset_logistic", "Logistic", class = "btn-sm"),
        actionButton("preset_dome", "Dome-shaped", class = "btn-sm"),
        actionButton("preset_flat", "Knife-edged", class = "btn-sm"),
        actionButton(
          "reset_all",
          "Reset All",
          class = "btn-sm btn-outline-secondary"
        ),
        br(),
        h5("Smooth out custom curve?"),
        actionButton("smooth", "Apply Smoothing", class = "btn-info"),

        hr(),

        p(
          "Adjust individual bin selectivity values using the sliders on the right, or click and drag points on the plot."
        ),

        downloadButton("download_data", "Download Data", class = "btn-primary")
      ),

      # Main panel with plot and sliders
      layout_columns(
        col_widths = c(8, 4),

        # Plot panel
        card(
          card_header("Selectivity Curve"),
          plotOutput(
            "selectivity_plot_out",
            height = "500px",
            click = "plot_click",
            hover = "plot_hover"
          )
        ),

        # Sliders panel
        card(
          card_header("Bin Selectivity Values"),
          div(
            style = "max-height: 500px; overflow-y: auto;",
            uiOutput("selectivity_sliders")
          )
        )
      )
    )
  ),

  ################
  # Productivity #
  ################
  nav_panel(
    title = "Productivity",
    value = "productivity",

    page_sidebar(
      title = "Productivity and Fishery Yield Analysis: How fishery yield changes with stock productivity",

      sidebar = sidebar(
        width = 300,

        p(
          "Explore stock productivity (steepness) and the relationship between relative spawning stock biomass and yield per recruit, 
       incorporating selectivity and natural mortality."
        ),

        p(
          "Steepness (h) is the fraction of R₀ expected when spawning biomass is 20% of unfished spawning biomass (SB₀)."
        ),
        #p("• h = 0.2: Very low productivity"),
        #p("• h = 0.7: Moderate productivity"),
        #p("• h = 1.0: Maximum productivity"),

        # Steepness parameter
        sliderInput(
          "steepness",
          "Steepness (h):",
          min = 0.2,
          max = 1.0,
          value = 0.7,
          step = 0.01,
          width = "100%"
        ),

        # Natural mortality
        sliderInput(
          "natural_mortality",
          "Natural Mortality (M):",
          min = 0.01,
          max = 1,
          value = 0.2,
          step = 0.01,
          width = "100%"
        ),

        h5("Age at Maturity & Selectivity"),
        uiOutput("maturity.in"),

        # Selectivity parameters
        #    h5("Selectivity at Age"),
        uiOutput("selectivity.in"),

        # Pretty Good Yield
        #h5("Pretty Good Yield: What % of MSY is good enough?"),
        numericInput(
          "PGY",
          "Pretty Good Yield: What % of MSY is good enough?",
          value = 0.8,
          min = 0,
          max = 1,
          step = 0.01
        ),
        hr(),
      ),

      # Main panel with yield curve plot
      layout_columns(
        card(
          card_header("Stock Recruitment Curve"),
          plotOutput("sr_plot", height = "400px")
        ),

        card(
          card_header("Beverton-Holt Stock-Recruitment Equation"),
          withMathJax(),
          div(
            style = "text-align: center; font-size: 16px; margin: 20px 0;",
            "$$\\frac{R}{R_0} = \\frac{4h \\cdot \\frac{S}{S_0}}{(1-h) + \\frac{S}{S_0}(5h-1)}$$"
          ),
          p("Where:"),
          tags$ul(
            tags$li("R/R₀ = Relative recruitment"),
            tags$li("S/S₀ = Relative spawning stock biomass"),
            tags$li("h = Steepness")
          )
        ),
        col_widths = c(6, 6),
        row_heights = c(2, 2)
      ),

      layout_columns(
        card(
          card_header("Yield Per Recruit Curve"),
          plotOutput("yield_curve", height = "500px")
        ),

        # Additional information card
        card(
          card_header("Model Parameters"),
          tableOutput("parameters_table"),
        ),
        card(
          card_header("Model Outputs"),
          tableOutput("outputs_table")
        ),
        col_widths = c(6, 3, 3),
        row_heights = c(2, 2, 2)
      )
    )
  ),

  ###############
  # Uncertainty #
  ###############
  nav_panel(
    title = "Uncertainty",
    value = "uncertainty",

    page_sidebar(
      title = "Understanding Uncertainty: Bias and Precision",

      sidebar = sidebar(
        h4("Uncertainty is "),
        tags$ul(
          tags$li(
            strong("Unknown:"),
            "What we do not know or have trouble measuring."
          ),
          tags$li(
            strong("Unpredictable:"),
            "We know it, but it is hard to predict."
          ),
          tags$li(
            strong("Risky:"),
            "If it is hard to measure or predict, it increases decision-making risk."
          )
        ),

        # sliderInput("true_value",
        #             "True Value:",
        #             min = 25, max = 175, value = 100, step = 1),

        numericInput(
          "n_samples",
          "Number of Samples:",
          min = 1,
          max = 1000000,
          value = 200,
          step = 1
        ),

        numericInput(
          "bias",
          "Bias (% error of true value):",
          min = -100,
          max = 100,
          value = 0,
          step = 1
        ),

        numericInput(
          "CV",
          "Precision (coeff. of variation):",
          min = 0,
          max = 1,
          value = 0.1,
          step = 0.01
        ),

        actionButton("resample", "Generate New Sample", class = "btn-primary"),

        hr(),

        # h5("Predefined Scenarios:"),
        # actionButton("scenario1", "High Precision, No Bias",
        #              class = "btn-outline-success btn-sm"),
        # actionButton("scenario2", "Low Precision, No Bias",
        #              class = "btn-outline-warning btn-sm"),
        # actionButton("scenario3", "High Precision, High Bias",
        #              class = "btn-outline-danger btn-sm"),
        # actionButton("scenario4", "Low Precision, High Bias",
        #              class = "btn-outline-dark btn-sm")
      ),

      # Main content
      layout_columns(
        col_widths = c(8, 4, 12),

        # Top row - main visualization
        card(
          card_header("Distribution of Measurements"),
          plotOutput("main_plot", height = "400px")
        ),

        # Bottom left - statistics
        card(
          layout_columns(
            col_widths = c(12, 12),
            card_header("Statistical Summary"),
            tableOutput("statistics_table"),
            #card_header("Interpretation"),
            uiOutput("interp")
          )
        ),

        # Bottom right - explanation
        layout_columns(
          col_widths = c(4, 4, 4),
          card(
            card_header("Key concepts"),
            uiOutput("uncertainty")
          ),
          card(
            card_header("Sources of Uncertainty"),
            uiOutput("sources")
          ),
          card(
            card_header("Estimating Uncertainty"),
            uiOutput("estimation")
          )
        )
      )
    )
  ),

  ######################
  # Abundance sampling #
  ######################
  nav_panel(
    title = "Sampling abundance",
    value = "abundance",
    page_sidebar(
      title = "Fishery dependent and independent population sampling: Explore how different sampling designs and approaches measure the true population",
      sidebar = sidebar(
        h4("Setting population size"),
        fluidRow(
          column(
            width = 6,
            numericInput(
              "pop_size",
              "Starting population size",
              value = 1000,
              min = 1,
              max = 100000000000,
              step = 1
            )
          ),
          column(
            width = 6,
            numericInput(
              "zero_cells",
              "Probability of zero fish",
              value = 0.05,
              min = 0,
              max = 1,
              step = 1
            )
          )
        ),
        #numericInput("mortality", "Do you want to apply a mortality rate?", value = 0, min = 1, max = 1, step = 0.001),
        actionButton(
          "pick_pop",
          "Update cells",
          class = "btn-outline-secondary",
          style = "color: #fff; background-color: #5D9741; border-color: #5D9741"
        ),
        h4("Choosing the fishing spots"),
        p("Click on cells in the grid to select/deselect them for sampling."),
        p(
          "To mimic a",
          tags$b(" fishery "),
          "go to the",
          tags$b(" hot spots "),
          "(i.e., the cells with the most fish)"
        ),
        actionButton(
          "fish_hot",
          "Choose hot spot cells",
          class = "btn-outline-secondary",
          style = "color: #fff; background-color: red; border-color: #a4422e"
        ),
        p(
          "To mimic a",
          tags$b(" survey "),
          "push the",
          tags$b(" random sample "),
          "button to get cells to sample"
        ),
        actionButton(
          "random_cells",
          "Choose random cells",
          class = "btn-outline-secondary",
          style = "color: #fff; background-color: #005595; border-color: #2e6da4"
        ),
        numericInput(
          "cell_num",
          "How many cells to fish?",
          value = 5,
          min = 0,
          max = 25,
          step = 1
        ),
        #   actionButton("select_all", "Select All", class = "btn-outline-primary"),
        actionButton(
          "save_sample",
          "Save sample",
          class = "btn-outline-secondary",
          style = "color: #fff; background-color: #eb860c; border-color: #eb860c"
        ),
        actionButton(
          "clear_all",
          "Clear all selections",
          class = "btn-outline-secondary",
          style = "color: #fff; background-color: #585955; border-color: black"
        ),
        br(),
        #    verbatimTextOutput("random.cells.out"),
        #    br(), br(),
        #    h5("Selected Cells"),
        #    verbatimTextOutput("selected_cells_display"),
        br(),
        h4("Sample comparisons"),
        tableOutput("pop_samples_out"),
        actionButton(
          "clear_samples",
          "Clear saved samples",
          class = "btn-outline-secondary",
          style = "color: #fff; background-color: #585955; border-color: black"
        )
      ),

      # Main panel with fishing cells and comparison tables
      layout_columns(
        card(
          card_header(
            "Fish Population Grid (5x5) - Click cells to fish in (i.e., take samples)"
          ),
          card_body(
            plotOutput(
              "grid_plot",
              width = "800px",
              height = "800px",
              click = "plot_click"
            )
          ),
        ),
        layout_columns(
          card(
            card_header("Sample Statistics"),
            card_body(
              tableOutput("sample_table")
            )
          ),
          # card(
          #   card_header("Sample Statistics (Selected cells)"),
          #   card_body(
          #     verbatimTextOutput("sample_stats")
          #   )
          # ),
          # card(
          #   card_header("Population Statistics (All 25 cells)"),
          #   card_body(
          #     verbatimTextOutput("population_stats")
          #   )
          # ),
          # col_widths = c(3,3),
        ),
      ),
      layout_columns(
        card(
          card_header("Index: Sample vs Population"),
          card_body(
            plotlyOutput("index_plot")
          ),
        ),
        card(
          card_header("Sample vs Population Statistics"),
          card_body(
            verbatimTextOutput("comparison_stats")
          )
        ),
        col_widths = c(8, 4),
      )
    )
  ),

  #################
  # Bio comps tab #
  #################
  nav_panel(
    title = "Age and Length",
    value = "biocomps",
    page_sidebar(
      title = "Fish Biological Structure: Change life history parameters and selectivity to see how they influences fished age and length compositions compared to unfished populations",

      sidebar = sidebar(
        width = 350,

        card(
          card_header("Life History Parameters"),
          fluidRow(
            column(
              width = 6,
              numericInput(
                "M_bc",
                "Natural Mortality (M)",
                value = 0.2,
                min = 0.05,
                max = 0.5,
                step = 0.01
              )
            ),
            column(
              width = 6,
              numericInput(
                "Linf_bc",
                "Asymptotic Length (Linf)",
                value = 60,
                min = 5,
                max = 2000,
                step = 1
              )
            )
          ),
          fluidRow(
            column(
              width = 6,
              numericInput(
                "K_bc",
                "Growth Coefficient (K)",
                value = 0.13,
                min = 0.001,
                max = 2,
                step = 0.01
              )
            ),
            column(
              width = 6,
              numericInput(
                "t0_bc",
                "Age at Size 0 (t0)",
                value = -1,
                min = -10,
                max = 0,
                step = 0.1
              )
            )
          ),
          h6("Length at Maturity"),
          fluidRow(
            column(
              width = 6,
              numericInput(
                "L50_bc",
                "L50%",
                value = 60 * 0.65,
                min = 0.1,
                max = 10000,
                step = 0.1
              )
            ),
            column(
              width = 6,
              numericInput(
                "L95_bc",
                "L95%",
                value = 60 * 0.8,
                min = 0.2,
                max = 10000,
                step = 0.1
              )
            )
          )
        ),

        #    card(
        #      card_header("Population Parameters"),
        #      numericInput("M", "Natural Mortality (M)", value = 0.2, min = 0.05, max = 0.5, step = 0.01),
        #      numericInput("R0", "Recruitment (R0)", value = 1000, min = 100, max = 10000, step = 100),
        #      numericInput("max_age", "Maximum Age", value = 5.4/0.2, min = 1, max = 500, step = 1)
        #    ),

        card(
          card_header("Fishing Parameters"),
          numericInput(
            "F_mort",
            "Fishing Mortality (F)",
            value = 0.2,
            min = 0,
            max = 1,
            step = 0.01
          ),
          fluidRow(
            column(
              width = 6,
              numericInput(
                "L50_asc",
                "L50 (50% selectivity):",
                value = 30,
                min = 0,
                step = 0.1
              )
            ),
            column(
              width = 6,
              numericInput(
                "L95_asc",
                "L95 (95% selectivity):",
                value = 40,
                min = 0,
                step = 0.1
              )
            )
          ),
          helpText(
            "Selectivity at L95 should be greater than selectivity at L50 for ascending limb"
          ),
          fluidRow(
            column(
              width = 6,
              numericInput(
                "peak_length",
                "Peak Length (mode):",
                value = 60,
                min = 0,
                step = 0.1
              )
            ),
            column(
              width = 6,
              numericInput(
                "desc_sd",
                "Standard Deviation:",
                value = 100,
                min = 0.1,
                step = 0.1
              )
            )
          ),
          helpText("The standard deviation controls the width of the dome."),
          helpText(
            "To make logistic selectivity, the peak length can be made larger than the largest size in the population."
          )
        ),
        fluidRow(
          column(
            width = 5,
            actionButton(
              "save_results",
              "Save results",
              class = "btn-outline-secondary",
              style = "color: #fff; background-color: #eb860c; border-color: #eb860c"
            )
          ),
          column(
            width = 7,
            actionButton(
              "clear_results",
              "Clear output table",
              class = "btn-outline-secondary",
              style = "color: #fff; background-color: #142530; border-color: black"
            )
          )
        ),

        downloadButton(
          "download_pops",
          "Download population outputs",
          class = "btn-outline-secondary",
          style = "color: #fff; background-color: #5D9741; border-color: #5D9741"
        ),

        fluidRow(
          column(
            width = 6,
            numericInput(
              "lt_den_samples",
              "Length Samples:",
              min = 1,
              max = 1000000,
              value = 200,
              step = 1
            ),
          ),
          column(
            width = 6,
            downloadButton(
              "download_lengths",
              "Download length samples",
              class = "btn-outline-secondary",
              style = "color: #fff; background-color: #005595; border-color: #005595"
            )
          )
        ),

        card(
          card_header("Reference Points"),
          fluidRow(
            column(
              width = 6,
              numericInput(
                "TRP",
                "Target RP",
                value = 0.4,
                min = 0,
                max = 1,
                step = 0.1
              )
            ),
            column(
              width = 6,
              numericInput(
                "LRP",
                "Limit RP",
                value = 0.25,
                min = 0,
                max = 1,
                step = 0.1
              )
            )
          )
        )
      ),

      layout_columns(
        col_widths = c(4, 4, 4),

        #    card(
        #      card_header("Age Structure"),
        #      plotlyOutput("age_plot")
        #    ),

        #    card(
        #      card_header("Length Structure"),
        #      plotlyOutput("length_plot")
        #    ),

        card(
          card_header("Selectivity and Maturity Curve"),
          plotOutput("selectivity_plot_lt")
        ),

        card(
          card_header("Growth and Mortality"),
          plotOutput("growth_M_plot", height = "500px")
        ),
        card(
          card_header("Stock Status"),
          tableOutput("stock_status_tab"),
          textOutput("stock_interpretation")
        )
      ),

      layout_columns(
        col_widths = c(4, 4, 4),
        card(
          card_header("Sampled age compositions: Fished vs Unfished"),
          plotlyOutput("age_sel_plot")
        ),

        card(
          card_header("Sampled length compositions: Fished vs Unfished"),
          plotlyOutput("length_sel_plot")
        ),

        card(
          card_header("Output table"),
          tableOutput("results_out"),
        )
      )
    )
  ),

  ##################
  # Indicators tab #
  ##################
  nav_panel(
    title = "Indicators",
    value = "indicators",

    page_sidebar(
      title = "Indicators: The essence of stock assessment",

      sidebar = sidebar(
        width = 300,
        h4("Data Options"),

        pickerInput(
          inputId = 'stock.choice',
          label = 'Choose A Stock',
          choices = c("A", "B", "C", "D"),
          options = list(`style` = "btn-info")
        ),

        downloadButton(
          "download_indicator_data",
          "Download Stock Data",
          class = "btn-primary"
        ),

        uiOutput("data.indicator"),

        # checkboxGroupButtons(
        #   inputId = "data.id",
        #   label = "Which data to consider: ",
        #   choices = c("Catch", "Index", "Mean Length"),
        #   size="sm"
        # ),

        # prettyCheckboxGroup( # or prettyRadioButtons
        #   inputId = "data.id",
        #   label = "Which data to consider",
        #   choices = c("Catch", "Index", "Mean Length"),
        #   outline = TRUE,
        #   plain = TRUE,
        #   status = "primary",
        #   icon = icon("check")
        # ),

        # Input for target value
        h5(
          "Select indicator (I), reference point (RP), and control rule (CR) values"
        ),
        h6("These are determined from analyzing the downloaded stock data"),

        h6(strong("Catch data")),
        fluidRow(
          column(
            width = 6,
            numericInput(
              "Ct_I_in",
              "I:",
              value = 0,
              min = NA,
              max = NA,
              step = 0.01
            )
          ),
          column(
            width = 6,
            numericInput(
              "Ct_RP_in",
              "RP:",
              value = 0,
              min = NA,
              max = NA,
              step = 0.01
            )
          )
        ),
        # numericInput(
        #   "Ct_CR_in",
        #   "CR:",
        #   value = 0,
        #   min = 0,
        #   max = 100000000000,
        #   step = 0.01
        #        ),

        selectInput(
          "ct_equation_type",
          "Control Rule Option:",
          choices = list(
            "Simple ratio (CR= I/RP)" = "ct_ratio",
            "Cubic (CR = 0.2*((I/RP)-1)^3)" = "ct_cubic",
            "Cubic polynomial (CR = 0.2*((I/RP)-1)^3+0.05*((I/RP)-1))" = "ct_cubicpoly",
            "Custom Equation" = "ct_custom"
          ),
          selected = "ratio"
        ),

        # Conditional input for custom equation
        conditionalPanel(
          condition = "input.ct_equation_type == 'ct_custom'",
          textInput(
            "ct_custom_cr",
            "Enter Custom Equation:",
            value = "(I/RP)*0.95",
            placeholder = "e.g., (I/RP)*0.95"
          ),
          helpText(
            "Use complete R function calls if using things like mean(), etc."
          )
        ),

        h6(strong("Index data")),
        fluidRow(
          column(
            width = 6,
            numericInput(
              "I_I_in",
              "I:",
              value = 0,
              min = NA,
              max = NA,
              step = 0.01
            )
          ),
          column(
            width = 6,
            numericInput(
              "I_RP_in",
              "RP:",
              value = 0,
              min = NA,
              max = NA,
              step = 0.01
            )
          )
        ),
        # numericInput(
        #           "I_CR_in",
        #           "CR:",
        #           value = 0,
        #           min = 0,
        #           max = 100000000000,
        #           step = 0.01
        #         ),

        selectInput(
          "ind_equation_type",
          "Control Rule Option:",
          choices = list(
            "Simple ratio (CR= I/RP)" = "ind_ratio",
            "Cubic (CR = 0.2*((I/RP)-1)^3)" = "ind_cubic",
            "Cubic polynomial (CR = 0.2*((I/RP)-1)^3+0.05*((I/RP)-1))" = "ind_cubicpoly",
            "Custom Equation" = "ind_custom"
          ),
          selected = "ratio"
        ),

        # Conditional input for custom equation
        conditionalPanel(
          condition = "input.ind_equation_type == 'ind_custom'",
          textInput(
            "ind_custom_cr",
            "Enter Custom Equation:",
            value = "(I/RP)*0.95",
            placeholder = "e.g., (I/RP)*0.95"
          ),
          helpText(
            "Use complete R function calls if using things like mean(), etc."
          )
        ),

        h6(strong("Mean length data")),
        fluidRow(
          column(
            width = 6,
            numericInput(
              "Lt_I_in",
              "I:",
              value = 0,
              min = NA,
              max = NA,
              step = 0.01
            )
          ),
          column(
            width = 6,
            numericInput(
              "Lt_RP_in",
              "RP:",
              value = 0,
              min = NA,
              max = NA,
              step = 0.01
            )
          )
        ),

        selectInput(
          "lt_equation_type",
          "Control Rule Option:",
          choices = list(
            "Simple ratio (CR= I/RP)" = "lt_ratio",
            "Cubic (CR = 0.2*((I/RP)-1)^3)" = "lt_cubic",
            "Cubic polynomial (CR = 0.2*((I/RP)-1)^3+0.05*((I/RP)-1))" = "lt_cubicpoly",
            "Custom Equation" = "lt_custom"
          ),
          selected = "ratio"
        ),

        # Conditional input for custom equation
        conditionalPanel(
          condition = "input.lt_equation_type == 'lt_custom'",
          textInput(
            "lt_custom_cr",
            "Enter Custom Equation:",
            value = "(I/RP)*0.95",
            placeholder = "e.g., (I/RP)*0.95"
          ),
          helpText(
            "Use complete R function calls if using things like mean(), etc."
          )
        ),

        # Calculate button
        # actionButton(
        #   "calculate_cr",
        #   "Run Control Rule(s)",
        #   class = "btn-primary",
        #   style = "width: 100%; margin-top: 10px;"
        # )
      ),

      # Main panel with results
      layout_columns(
        card(
          card_header("General Indicator-based Management Procedure"),
          withMathJax(),
          div(
            style = "text-align: center; font-size:20px; margin: 10px 0;",
            "$$\\ MM_{y} = \\ MM_{y-z} \\cdot \\ Modifier$$"
          ),
          p("Where:"),
          tags$ul(
            tags$li(
              "MM = Management Metric. Examples are catch limits, effort, etc."
            ),
            tags$li("y = current year"),
            tags$li("z = years before current year", tags$i("y")),
            tags$li("Modifer = "),
            tags$ol(
              tags$li(
                "Indicator (I) compared to reference point (RP). The indicator measures the current state. The reference point defines the desireable (e.g., target) and/or undesireable (e.g., limit) value of the stock, in the same metric as the indicator. For example, is the indicator more or less than the reference point?"
              ),
              tags$li(
                "Control rule (CR)= the rule to adjust the managaement metric based on comparison to reference point."
              )
            ),
          ),
          p(
            "The control rule would then be the Modifier used in the general equation."
          ),
          p(
            "An example of a simple control rule based on an Indicator (I) and Reference Point (RP) is:."
          ),
          div(
            style = "text-align: center; font-size:20px; margin: 10px 0;",
            "$$\\ CR = \\frac{I}{RP}$$"
          ),
          p(
            "Indicators can be model-free (using the data directly) or model-based (from a stock assessment)."
          ),
          p("Examples of indicators are:"),
          tags$ul(
            tags$li("Model free:"),
            tags$ol(
              tags$li("Average Catch"),
              tags$li("Indices of abundance (e.g., CPUE, density)"),
              tags$li(
                "Size or age-based metrics (mean values, SPR, relative to maturity, etc)"
              ),
              tags$li("Species composition"),
              tags$li("Habitat condition or availability"),
              tags$li(
                "Fishing behavior (e.g, Distance traveled to fishing ground)"
              ),
            ),
            tags$li("Model-based:"),
            tags$ol(
              tags$li("Spawning Potential Ratio"),
              tags$li("Relative stock size"),
              tags$li("Absolute biomass"),
              tags$li("Fishing rates (F or U)")
            ),
          ),
        ),

        layout_columns(
          # Time series plot
          card_header("Simple Indicators"),
          card(
            #plotlyOutput("stock_time_series_Ct", height = "200px")
            uiOutput("stock_time_series_Ct_ui")
          ),

          card(
            #plotlyOutput("stock_time_series_Index", height = "200px")
            uiOutput("stock_time_series_Index_ui")
          ),

          card(
            #plotlyOutput("stock_time_series_Lt", height = "200px")
            uiOutput("stock_time_series_Lt_ui")
          ),
          #LH and Summary statistics card
          card(
            layout_columns(
              card(
                card_header("Life History Values"),
                tableOutput("LH_values")
              ),
              card(
                card_header("Summary Statistics"),
                tableOutput("summary_stats"),
                actionButton("copy_btn", "Copy Table", class = "btn-primary")
              ),
              #col_widths = c(4, 8)
            )
          ),
          col_widths = c(12, 12, 12, 12)
        )
      )
    )
  ),

  ########################
  # Reference points tab #
  ########################
  nav_panel(
    title = "Reference Points",
    value = "refpts",
    page_sidebar(
      title = "Harvest Control Rule: Create control rules by defining reference points and explore their behavior by altering stock status",
      sidebar = sidebar(
        h5(
          tags$b("Set your harvest control rule"),
          style = "text-align: center;"
        ),

        # Stock status indicator

        # Biomass reference points
        #h6("",style="text-align: center;"),

        h6(
          "Biomass Reference Points relative to unfished",
          style = "text-align: center;"
        ),
        fluidRow(
          column(
            width = 6,
            numericInput(
              "b_target",
              "Target:",
              value = 0.4,
              min = 0.02,
              max = 0.1,
              step = 0.01
            )
          ),
          column(
            width = 6,
            numericInput(
              "b_limit",
              "Limit:",
              value = 0.25,
              min = 0.01,
              max = 1,
              step = 0.01
            )
          )
        ),

        fluidRow(
          column(
            width = 6,
            numericInput(
              "b_nocatch",
              "No Catch:",
              value = 0.1,
              min = 0.01,
              max = 1,
              step = 0.01
            )
          ),
          column(
            width = 6,
            numericInput(
              "buffer",
              "Buffer",
              value = 1,
              min = 0,
              max = 1,
              step = 0.001
            )
          )
        ),

        # Fishing mortality/catch parameters
        #h6("Harvest rate at MSY (or proxy). This is line slope and based on stock productivity.",style="text-align: center;"),
        numericInput(
          "E_msy",
          "Harvest rate at MSY (or proxy). This is the blue line slope and based on stock productivity",
          value = 0.3,
          min = 0.01,
          max = 1.0,
          step = 0.01
        ),

        #    numericInput("max_catch",
        #                 "Maximum Catch (relative units):",
        #                 value = 1.0,
        #                 min = 0.1,
        #                 max = 2.0,
        #                 step = 0.1),

        h5(
          tags$b(
            "Change this value to see what your catch is at a specific stock size"
          ),
          style = "text-align: center;"
        ),
        sliderInput(
          "current_stock",
          "Spawning Stock Size (SB/SB0):",
          value = 0.4,
          min = 0.01,
          max = 1,
          step = 0.01
        ),
        h6(
          "For more advanced explorations and creations of harvest control rules, please see ",
          tags$a(
            href = "https://bridgeenvironment.shinyapps.io/hcr_design_tool/",
            "The Harvest Control Design Tool",
            target = "_blank"
          )
        ),
        # Control rule shape
        #   h5("Control Rule Shape"),
        #   selectInput("rule_type",
        #               "Control Rule Type:",
        #               choices = list(
        #                 "Linear" = "linear",
        #                 "Hockey Stick" = "hockey",
        #                 "Smooth Transition" = "smooth"
        #               ),
        #               selected = "linear")
      ),

      # Main panel with plot and information
      layout_columns(
        card(
          card_header("Harvest Control Rule Visualization"),
          plotOutput("control_rule_plot", height = "500px")
        ),

        card(
          card_header("Stock Status Summary"),
          verbatimTextOutput("stock_status_RPs")
        ),

        card(
          card_header("Harvest Control Rule Summary"),
          verbatimTextOutput("stock_status")
        ),
        col_widths = c(12, 6, 6),
        row_heights = c(2, 1)
      )
    )
  ),

  ######################################
  # Scale, Status and Productivity tab #
  ######################################
  nav_panel(
    title = "SSP",
    value = "SSP",
    sidebarLayout(
      sidebarPanel(
        tags$head(
          tags$style(HTML(
            "
                    li {
                    font-size: 20px;

                    }
                    li span {
                    font-size: 18px;
                    }
                    ul {
                    list-style-type: square;
                    }

                    "
          ))
        ),

        tags$h3(
          "There are three main concepts that help us interpret stock assessments:"
        ),
        tags$ul(
          tags$li(
            tags$b("Stock scale or size"),
            "-- the absolute amount of the stock in either biomass or numbers-- allows for the understanding of fishing rates."
          ),
          tags$li(
            tags$b("Stock status"),
            "-- typically a relative size/percentage of the population to an unfished or size at maximum sustainable yield-- is a basic output of stock assessment. It provides the indicator of stock health."
          ),
          tags$li(
            tags$b("Productivity"),
            "-- how fast a population can ultimately growth in status and scale. This include mortality, growth, maturity, and recruitment capacity."
          )
        ),

        h4(
          "The combination of stock status (i.e., how much the population has declined) and size (i.e., how many are there) along with the productivity of the stock determines how much can be caught, and are used in the harvest control rules for setting catch limits."
        ),
        h4(
          "Understanding why stock status and scale may change across assessments, and what causes them to change (e.g., stock productivity), is critical for developing fisheries management"
        ),
        br(),
        h4(
          "This tool allows you to explore these concepts by picking different model configurations."
        ),
        h4(
          "All models are compared to a stock that is at 40% of unfished in the final year and has a certain catch time series and life history."
        ),
        h4("You can choose to change from the following options"),
        tags$ul(
          tags$li("Change the ending", tags$b("stock status"), "value."),
          tags$li(
            "Change the",
            tags$b("stock scale"),
            "by estimate the intial stock size or changing the catch history."
          ),
          tags$li(
            "Change the",
            tags$b("stock productivity"),
            "via natural mortality or recruitment steepness."
          )
        ),

        br(),
        h4("Choose changes to the reference model to explore"),
        uiOutput("SSP_model_picks_groupedII"),
        br(),
        actionButton(
          "run_SSP_comps",
          strong("Run Comparisons"),
          width = "100%",
          icon("circle-play"),
          style = "font-size:120%;border:2px solid;color:#FFFFFF; background:#236192"
        ),
      ),

      # Show a plot of the generated distribution
      mainPanel(
        #          fluidRow(column(width=6,plotlyOutput("Scale"),
        #                          column(width = 6,plotlyOutput("Status"))))
        plotlyOutput("Catches"),
        plotlyOutput("Scale"),
        plotlyOutput("Status"),
        plotlyOutput("Proj")
      )
    )
  ),

  ########################
  # Baseline Shifter tab #
  ########################
  nav_panel(
    title = "Baseline",
    value = "baseline",
    fluidPage(
      # Application title
      # Application title
      titlePanel(
        "How does our perception of current stock status change depending on the year we compare it to?"
      ),

      # Sidebar with a slider input for number of bins
      sidebarLayout(
        sidebarPanel(
          h4(strong(em("Choose a stock Report file"))),
          shinyDirButton(
            id = "Report_dir",
            label = "Select folder",
            title = "Choose folder containing the SS3 Report file"
          ),
          br(),
          h5(strong(textOutput("ReportPath", inline = TRUE))),

          h4(strong(em("Choose a year to compare all values"))),
          fluidRow(column(
            width = 6,
            numericInput(
              "Year_comp",
              "Year for comparison",
              value = 2000,
              min = 0,
              max = 2030,
              step = 1
            )
          )),
          actionButton(
            "run_baseline_comps",
            strong("Update Baseline Comparisons"),
            width = "100%",
            icon("circle-play"),
            style = "font-size:120%;border:2px solid;color:#FFFFFF; background:#236192"
          ),
        ),

        # Show a plot of the generated distribution
        mainPanel(
          h4(strong(
            "Time series of population outputs relative to a chosen year"
          )),
          h4("Horizontal and vertical lines intersect at the chosen year"),
          h4(
            "Top panel is all years relative to the chosen years (i.e., Year/Chosen_Year); Bottom panel is the percent difference from the chosen year)"
          ),
          h4(
            "Hover the pointer over any series and point to get the specific values"
          ),
          plotlyOutput("CompPlot"),
          plotlyOutput("CompPlotRE")
          #plotlyOutput("DepPlot"),
          #plotlyOutput("SpawnOutPlot"),
          #plotlyOutput("SummaryBPlot"),
          #plotlyOutput("TotalBPlot")
        )
      )
    )
  ),

  ###########
  # SAC tab #
  ###########
  nav_panel(
    title = "SAC",
    value = "sac",

    fluidPage(
      # Define the decision tree structure

      page_sidebar(
        title = "Navigating through the Stock Assessment Continuum",

        sidebar = div(
          # Reset button
          actionButton(
            "reset",
            "Begin Again",
            class = "btn-outline-primary mb-3",
            style = "width: 50%;"
          ),

          # Path display
          conditionalPanel(
            condition = "output.show_path",
            card(
              card_header("Your Data Pathway"),
              card_body(
                htmlOutput("path_display")
              )
            )
          )
        ),

        # Main content area
        div(
          id = "main_content",

          # Welcome message
          conditionalPanel(
            condition = "output.at_root",
            card(
              card_header(
                h1(
                  "Welcome to the Stock Assessment Continuum navigator",
                  class = "text-center"
                )
              ),
              card_body(
                p(
                  "This interactive decision tree will help you navigate through different stock assessment options.",
                  class = "text-center"
                ),
                p(
                  "Click 'Start' to traverse the Stock Assessment Continuum (SAC).",
                  class = "text-center"
                ),
                div(
                  actionButton(
                    "start",
                    "Start SAC Decision Tree",
                    class = "btn-outline-primary mb-3"
                  ),
                  #class = "btn-primary btn-lg"),
                  class = "text-center"
                ),
                imageOutput("SACImage_init")
              )
            )
          ),

          # Decision node display
          conditionalPanel(
            condition = "!output.at_root && !output.at_outcome",
            card(
              card_header(
                h4(textOutput("question_text"))
              ),
              card_body(
                uiOutput("choices_ui"),
                actionButton(
                  "go_back",
                  "Go Back",
                  width = "10%",
                  #                       style='padding:4px; font-size:100%'),
                  class = "text-center",
                  class = "btn-outline-secondary"
                ),
                #                       class = "btn-outline-secondary"),
                align = "center",
                imageOutput("SACImage")
              ),
            )
          ),

          # Outcome display
          conditionalPanel(
            condition = "output.at_outcome",
            card(
              card_header(
                h3("Recommendation", class = "text-success")
              ),
              card_body(
                div(
                  h5(textOutput("outcome_text")),
                  br(),
                  actionButton(
                    "start_over",
                    "Begin Again",
                    class = "btn-primary"
                  ),
                  actionButton(
                    "go_back_outcome",
                    "Go Back",
                    class = "btn-outline-secondary"
                  ),
                  imageOutput("SACImage_out")
                )
              ),
              card_body(
                h6(
                  "All scale- and model-based options, as well as the length and/or age only models, can be done in ",
                  tags$a(
                    href = "https://github.com/shcaba/SS-DL-tool",
                    "The Stock Assessment Continuum Tool",
                    target = "_blank"
                  )
                ),
                h6(
                  tags$a(
                    href = "https://connect.fisheries.noaa.gov/psa/",
                    "The Productivity-Susceptibility Analysis",
                    target = "_blank"
                  ),
                  " is a form of Risk Analysis that can be used when all other data are unavailable."
                ),
                h6(
                  "Model-free indicator approaches can be explored using the Indicator module of the Stock Assessment Learning Tool"
                )
              )
            )
          )
        ),

        # Add custom CSS
        tags$head(
          tags$style(HTML(
            "
      .choice-button {
        margin: 5px;
        width: 25%;
        text-align: left;
      }
      .path-item {
        padding: 2px 8px;
        margin: 2px;
        background-color: #6aa84f;
        border-radius: 15px;
        display: inline-block;
        font-size: 0.9em;
      }
    "
          ))
        )
      )
    )
  ),

  id = "navbar"
)
