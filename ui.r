library(shiny)
library(bslib)
library(bsicons)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)

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
      h1("Welcome to the Stock Assessment Learning Tool (SALT)", class = "text-center mb-5"),
      p("Click on any of the below topics to explore different features:", 
        class = "text-center lead mb-5"),
      
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
                    icon("heart-pulse",class="fa-3x fa-sharp fa-solid"),
                    h3("Life History Parameters", class = "card-title")
                  )
                ),
                card_body(
                  p("Life history parameters are the foundation of stock assessments and fisheries management. Learn about natural mortality (M), growth, maturity, and weight-length relationships, and how they go together to make life history strategies.")
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
                    tags$img(src = "fishing_boat.png", height = "100px", width = "200px"),
                    h3("Selectivty", class = "card-title")
                  )
                ),
                card_body(
                  p("Learn about forms of gear selectivity and how it samples the population.")
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
                  tags$img(src = "sampling.png", height = "100px", width = "300px"),
                  h3("Sampling abundance", class = "card-title")
                )
              ),
              card_body(
                p("Explore how different types of sampling of fish abundance affects the accuracy of abundance estimation. Simulate fishery-dependent and fishery-independent sampling approaches.")
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
                  tags$img(src = "SSP.png", height = "100px", width = "300px"),
                  h3("Scale, Status, Productivity", class = "card-title")
                )
              ),
              card_body(
                p("Understand stock assessment output and sensitivity and learn how to communicate stock assessments change by understanding the three main dimensions of stock assessment output.")
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
            tags$img(src = "RefPts.png", height = "100px", width = "300px"),
            h3("Reference Points", class = "card-title")
          )
        ),
        card_body(
          p("Explore and design reference points and harvest control rules and how they determine catch limits.")
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
                    bs_icon("file-earmark-bar-graph", size = "3em", class = "text-success mb-3"),
                    h3("Baseline shifter", class = "card-title")
                  )
                ),
                card_body(
                  p("Explore our preception of stock abundance and health when we have longer or shorter data sets of and/or experiences with the population and its dyanmics.")
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
      title = "Life History Parameters",
      
      sidebar = sidebar(
        width = 350,
        
        #  Parameters
        
        #Natural Mortality & Growth (von Bertalanffy) Parameters 
        card(
          card_header("Natural Mortality (M)"),
          numericInput("M", "Natural Mortality Rate (M)", value = 0.2, min = 0.01, max = 2, step = 0.01)
        ),
        card(
          card_header("Growth Parameters. Lengths in cm."),
          numericInput("Linf", "L∞ (Asymptotic Length)", value = 100, min = 10, max = 500),
          numericInput("K", "Growth Rate (K)", value = 0.15, min = 0.01, max = 2, step = 0.01),
          numericInput("t0", "t₀ (Theoretical age at length 0)", value = -1, min = -10, max = 1, step = 0.1)
        ),
        
        # Maturity Parameters
        card(
          card_header("Maturity Parameters. Lengths in cm"),
          numericInput("L50", "L₅₀ (Length at 50% maturity)", value = 66, min = 5, max = 200),
          numericInput("L95", "L₉₅ (Length at 95% maturity)", value = 80, min = 10, max = 300)
        ),
        
        # Weight-Length Parameters
        card(
          card_header("Weight (kg)-Length (cm) Parameters"),
          numericInput("a", "Parameter 'a'", value = 0.00001, min = 0.001, max = 1, step = 0.001),
          numericInput("b", "Parameter 'b'", value = 3, min = 1, max = 5, step = 0.1)
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
    title = "Selectivity Curve Designer",
    sidebar = sidebar(
      width = 300,
      h4("Selectivity Parameters"),
      
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
        numericInput("min_length", "Minimum Length (cm):", value = 10, min = 1),
        numericInput("max_length", "Maximum Length (cm):", value = 80, min = 1)
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
      actionButton("reset_all", "Reset All", class = "btn-sm btn-outline-secondary"),
      br(),
      h5("Smooth out custom curve?"),
      actionButton("smooth", "Apply Smoothing", class = "btn-info"),
      
      hr(),
      
      p("Adjust individual bin selectivity values using the sliders on the right, or click and drag points on the plot."),
      
      downloadButton("download_data", "Download Data", class = "btn-primary")
    ),
    
    # Main panel with plot and sliders
    layout_columns(
      col_widths = c(8, 4),
      
      # Plot panel
      card(
        card_header("Selectivity Curve"),
        plotOutput("selectivity_plot", 
                   height = "500px",
                   click = "plot_click",
                   hover = "plot_hover")
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

######################
# Abundance sampling #
######################
nav_panel(
  title = "Sampling abundance",
  value = "abundance",
  page_sidebar(
    title = "Fishery dependent and independent population sampling",
    sidebar = sidebar(
      h4("Setting population size"),
      fluidRow(
        column(width = 6,numericInput("pop_size", "Starting population size", value = 1000, min = 1, max = 100000000000, step = 1)),
        column(width = 6,numericInput("zero_cells", "Probability of zero fish", value = 0.05, min = 0, max = 1, step = 1))),
      #numericInput("mortality", "Do you want to apply a mortality rate?", value = 0, min = 1, max = 1, step = 0.001),
      actionButton("pick_pop", "Update cells", class = "btn-outline-secondary",style="color: #fff; background-color: #5D9741; border-color: #5D9741"),
      h4("Choosing the fishing spots"),
      p("Click on cells in the grid to select/deselect them for sampling."),
      p("To mimic a", tags$b(" fishery "), "go to the", tags$b(" hot spots ") ,"(i.e., the cells with the most fish)"),
      actionButton("fish_hot", "Choose hot spot cells", class = "btn-outline-secondary",style="color: #fff; background-color: red; border-color: #a4422e"),
      p("To mimic a" , tags$b(" survey "), "push the",tags$b(" random sample "),"button to get cells to sample"),
      actionButton("random_cells", "Choose random cells", class = "btn-outline-secondary",style="color: #fff; background-color: #005595; border-color: #2e6da4"),
      numericInput("cell_num", "How many cells to fish?", value = 5, min = 0, max = 25, step = 1),
      #   actionButton("select_all", "Select All", class = "btn-outline-primary"),
      actionButton("save_sample", "Save sample", class = "btn-outline-secondary",style="color: #fff; background-color: #eb860c; border-color: #eb860c"),
      actionButton("clear_all", "Clear all selections", class = "btn-outline-secondary",style="color: #fff; background-color: #585955; border-color: black"),
      br(),
      #    verbatimTextOutput("random.cells.out"),
      #    br(), br(),
      #    h5("Selected Cells"),
      #    verbatimTextOutput("selected_cells_display"),
      br(),
      h4("Sample comparisons"),
      tableOutput("pop_samples_out"),
      actionButton("clear_samples", "Clear saved samples", class = "btn-outline-secondary",style="color: #fff; background-color: #585955; border-color: black")
    ),
    
    # Main panel with fishing cells and comparison tables
    layout_columns(  
      card(
        card_header("Fish Population Grid (5x5) - Click cells to fish in (i.e., take samples)"),
        card_body(
          plotOutput("grid_plot",width="800px", height = "800px", click = "plot_click")
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
        card_header("Sample vs Population"),
        card_body(
          plotlyOutput("index_plot")
        ),
      ),
      card(
        card_header("Sample vs Population"),
        card_body(
          verbatimTextOutput("comparison_stats")
        )
      ),
      col_widths = c(8,4),
    )
   )
  ),

###########
# SSP tab #
###########
nav_panel(
  title = "SSP",
  value = "SSP",
  sidebarLayout(
    sidebarPanel(
      
      tags$head(
        tags$style(HTML("
                    li {
                    font-size: 20px;

                    }
                    li span {
                    font-size: 18px;
                    }
                    ul {
                    list-style-type: square;
                    }

                    "))
      ),
      
      tags$h3("There are three main concepts that help us interpret stock assessments:"),
      tags$ul(
        tags$li(tags$b("Stock scale or size"),"-- the absolute amount of the stock in either biomass or numbers-- allows for the understanding of fishing rates."),
        tags$li(tags$b("Stock status"),"-- typically a relative size/percentage of the population to an unfished or size at maximum sustainable yield-- is a basic output of stock assessment. It provides the indicator of stock health."),
        tags$li(tags$b("Productivity"),"-- how fast a population can ultimately growth in status and scale. This include mortality, growth, maturity, and recruitment capacity.")
      ),
      
      h4("The combination of stock status (i.e., how much the population has declined) and size (i.e., how many are there) along with the productivity of the stock determines how much can be caught, and are used in the harvest control rules for setting catch limits."),
      h4("Understanding why stock status and scale may change across assessments, and what causes there change (e.g., stock productivity), is critical for developing fisheries management"),
      br(),
      h4("This tool allows you to explore these concepts by picking different model configurations."),
      h4("All models are compared to a stock that is at 40% of unfished in the final year and has a certain catch time series and life history."),
      h4("You can choose to change from the following options"),
      tags$ul(
        tags$li("Change the ending", tags$b("stock status") ,"value."),
        tags$li("Change the", tags$b("stock scale") ,"by estimate the intial stock size or changing the catch history."),
        tags$li("Change the", tags$b("stock productivity") ,"via natural mortality or recruitment steepness.")
      ),
      
      br(),
      h4("Choose changes to the reference model to explore"),
      uiOutput("SSP_model_picks_groupedII"),
      br(),
      actionButton("run_SSP_comps", strong("Run Comparisons"),
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
# Reference points tab #
########################
nav_panel(
  title = "Reference Points",
  value = "refpts",
  page_sidebar(
    title = "Fisheries Threshold Harvest Control Rule",
    sidebar = sidebar(
      h5(tags$b("Set your harvest control rule"),style="text-align: center;"),
      
      # Stock status indicator
      
      # Biomass reference points
      #h6("",style="text-align: center;"),
      
      h6("Biomass Reference Points relative to unfished",style="text-align: center;"),
      fluidRow(
        column(width = 6,
               numericInput("b_target", 
                            "Target:", 
                            value = 0.4, 
                            min = 0.02, 
                            max = 0.1, 
                            step = 0.01)),
        column(width = 6,
               numericInput("b_limit", 
                            "Limit:", 
                            value = 0.25, 
                            min = 0.01, 
                            max = 1, 
                            step = 0.01))),
      
      fluidRow(
        column(width = 6,
               numericInput("b_nocatch", 
                            "No Catch:", 
                            value = 0.1, 
                            min = 0.01, 
                            max = 1, 
                            step = 0.01)),
        column(width = 6,
               numericInput("buffer", 
                            "Buffer", 
                            value = 1, 
                            min = 0, 
                            max = 1, 
                            step = 0.001))),
      
      
      # Fishing mortality/catch parameters
      #h6("Harvest rate at MSY (or proxy). This is line slope and based on stock productivity.",style="text-align: center;"),
      numericInput("E_msy", 
                   "Harvest rate at MSY (or proxy). This is the blue line slope and based on stock productivity", 
                   value = 0.3, 
                   min = 0.01, 
                   max = 1.0, 
                   step = 0.01),
      
      
      #    numericInput("max_catch", 
      #                 "Maximum Catch (relative units):", 
      #                 value = 1.0, 
      #                 min = 0.1, 
      #                 max = 2.0, 
      #                 step = 0.1),
      
      h5(tags$b("Change this value to see what your catch is at a specific stock size"),style="text-align: center;"),
      sliderInput("current_stock", 
                  "Spawning Stock Size (SB/SB0):", 
                  value = 0.4, 
                  min = 0.01, 
                  max = 1, 
                  step = 0.01),
      
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
      col_widths = c(12,6,6),
      row_heights = c(2,1)
    ))
  ),
  
########################
# Baseline Shifter tab #
########################
  nav_panel(
    title = "Basline",
    value = "baseline",
    fluidPage(
      
      # Application title
      titlePanel("What is our perception of stock status?"),
      
      # Sidebar with a slider input for number of bins
      sidebarLayout(
        sidebarPanel(
          h4(strong(em("Choose a stock"))),
          
          h4(strong(em("Choose a year to compare all values"))),
          fluidRow(column(width = 6, numericInput("Year_comp", "Year for comparison", value = 2000, min = 0, max = 2030, step = 1)))
          
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          h4(strong("Time series of population outputs relative to a chosen year")),
          h4("Horizontal and vertical lines intersect at the chosen year (i.e., a relative value of 1)"),
          h4("Hover the pointer over any series and point to get the specific values"),
          plotlyOutput("CompPlot")
          #          plotlyOutput("DepPlot"),
          #          plotlyOutput("SpawnOutPlot"),
          #         plotlyOutput("SummaryBPlot"),
          #          plotlyOutput("TotalBPlot")
        )
      )
    )
  ),

  id = "navbar"
 )


