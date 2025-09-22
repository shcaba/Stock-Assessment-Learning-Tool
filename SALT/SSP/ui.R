#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(plotly)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Stock assessment interpretation fundamentals: Scale, Status, and Productivity"),

    # Sidebar with a slider input for number of bins
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
            tags$li(tags$b("Stock status"),"-- typically a relative size/percentage of the population to an unfished or size at maximum sustainable yield-- is a basic output of stock assessment. It provides the indicator of stock health."),
            tags$li(tags$b("Stock scale or size"),"-- the absolute amount of the stock in either biomass or numbers-- allows for the understanding of fishing rates."),
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
          uiOutput("SSP_model_picks_grouped"),
#          fluidRow(
#            column(width = 2, uiOutput("SSP_model_picks_status")),
#            column(width = 2, uiOutput("SSP_model_picks_scale")),
#            column(width = 2, uiOutput("SSP_model_picks_prod")),
#            column(width = 3, uiOutput("SSP_model_picks_status_prod")),
#            column(width = 3, uiOutput("SSP_model_picks_scale_prod"))
#            ),
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
)
