library(shiny)
library(bslib)
library(DT)
library(plotly)

ui <- page_sidebar(
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
#  layout_columns(
#  )  
#  card(
 #   card_header("Selected Cell Data"),
  #  card_body(
  #    DTOutput("cell_table")
  #  )
#  )
)
