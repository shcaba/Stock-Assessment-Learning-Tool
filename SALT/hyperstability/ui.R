library(shiny)
library(bslib)
library(DT)


ui <- page_sidebar(
  title = "Fishery dependent and independent population sampling",
  sidebar = sidebar(
    h4("Setting population size"),
    numericInput("pop_size", "What is the starting population size?", value = 1000, min = 1, max = 100000000000, step = 1),
    #numericInput("mortality", "Do you want to apply a mortality rate?", value = 0, min = 1, max = 1, step = 0.001),
    actionButton("pick_pop", "Update population starting size", class = "btn-outline-secondary"),
    br(),
    h4("Choosing the fishing spots"),
    p("Click on cells in the grid to select/deselect them for sampling."),
    p("To mimic a", tags$b(" fishery "), "go to the", tags$b(" hot spots ") ,"(i.e., the cells with the most fish)"),
    actionButton("fish_hot", "Choose cells to fish hot spots", class = "btn-outline-secondary"),
    p("To mimic a survey, push the random sample button to get cells to sample"),
    actionButton("random_cells", "Choose cells to randomly select", class = "btn-outline-secondary"),
    numericInput("cell_num", "How many cells?", value = 5, min = 0, max = 25, step = 1),
    #   actionButton("select_all", "Select All", class = "btn-outline-primary"),
    actionButton("clear_all", "Clear All", class = "btn-outline-secondary"),
    br(),
    #    verbatimTextOutput("random.cells.out"),
        #    br(), br(),
#    h5("Selected Cells"),
#    verbatimTextOutput("selected_cells_display"),
#    br(),
#    h5("Sample vs Population"),
#    verbatimTextOutput("comparison_stats")
  ),
  
  # Main panel with grid visualization and comparison tables
layout_columns(  
card(
    card_header("Fish Population Grid (5x5) - Click cells to fish in (i.e., take samples)"),
    card_body(
      plotOutput("grid_plot",width="800px", height = "800px", click = "plot_click")
    )
  ),
  layout_columns(
    card(
      card_header("Sample vs Population"),
      card_body(
        verbatimTextOutput("comparison_stats")
      )
    ),
    card(
      card_header("Population Statistics (All 25 cells)"),
      card_body(
        verbatimTextOutput("population_stats")
      )
    ),
    card(
      card_header("Sample Statistics (Selected cells)"),
      card_body(
        verbatimTextOutput("sample_stats")
      )
    ),
    col_widths = c(12,12,12)
    ),
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
