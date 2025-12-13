library(shiny)
library(bslib)
library(htmltools)
library(shinyjs)

ui <- page_sidebar(
  title = "Interactive Flowchart",
  sidebar = sidebar(
    h4("Instructions"),
    p("Click on any node in the flowchart to see detailed information about that step."),
    br(),
    h5("Process Overview:"),
    p("This flowchart shows a typical data analysis workflow with 5 main steps."),
    br(),
    actionButton("reset_btn", "Reset Selection", class = "btn-secondary")
  ),
  
  # Main content area
  card(
    card_header("Basics of Stock Assessment"),
    card_body(
      # CSS for styling the flowchart
      tags$style(HTML("
        .flowchart {
          display: flex;
          flex-direction: column;
          align-items: center;
          padding: 20px;
        }
        
        .flowchart-row {
          display: flex;
          justify-content: center;
          align-items: center;
          margin: 10px 0;
          width: 100%;
        }
        
        .flowchart-node {
          background: #e7f3ff;
          border: 2px solid #4a90e2;
          border-radius: 10px;
          padding: 15px 20px;
          margin: 10px;
          cursor: pointer;
          transition: all 0.3s ease;
          text-align: center;
          min-width: 150px;
          font-weight: bold;
        }
        
        .flowchart-node:hover {
          background: #d1e8ff;
          border-color: #357abd;
          transform: scale(1.05);
        }
        
        .flowchart-node.selected {
          background: #4a90e2;
          color: white;
          border-color: #357abd;
        }
        
        .arrow {
          font-size: 24px;
          color: #4a90e2;
          margin: 5px;
        }
        
        .decision-node {
          background: #fff3cd;
          border-color: #ffc107;
          transform: rotate(45deg);
          width: 120px;
          height: 120px;
          display: flex;
          align-items: center;
          justify-content: center;
        }
        
        .decision-node:hover {
          background: #ffeaa7;
        }
        
        .decision-node.selected {
          background: #ffc107;
        }
        
        .decision-text {
          transform: rotate(-45deg);
          font-size: 12px;
          text-align: center;
        }
        
        .parallel-nodes {
          display: flex;
          justify-content: space-around;
          width: 100%;
          max-width: 600px;
        }
      ")),
      
      # Flowchart structure
      div(class = "flowchart",
          # Start node
          div(class = "flowchart-row",
              div(class = "flowchart-node", 
                  id = "node1",
                  onclick = "Shiny.setInputValue('selected_node', 'node1')",
                  "1. Data Collection")
          ),
          
          # Arrow down
          div(class = "flowchart-row",
              div(class = "arrow", "↓")
          ),
          
          # Processing node
          div(class = "flowchart-row",
              div(class = "flowchart-node", 
                  id = "node2",
                  onclick = "Shiny.setInputValue('selected_node', 'node2')",
                  "2. Data Cleaning")
          ),
          
          # Arrow down
          div(class = "flowchart-row",
              div(class = "arrow", "↓")
          ),
          
          # Decision node
          div(class = "flowchart-row",
              div(class = "flowchart-node decision-node", 
                  id = "node3",
                  onclick = "Shiny.setInputValue('selected_node', 'node3')",
                  div(class = "decision-text", "3. Quality Check"))
          ),
          
          # Arrows for decision outcomes
          div(class = "flowchart-row",
              div(style = "display: flex; justify-content: space-between; width: 300px;",
                  div(style = "text-align: center;",
                      div(class = "arrow", "↙")
                      #small("Needs work")
                  ),
                  div(style = "text-align: center;",
                      div(class = "arrow", "↘")
                      #small("Good quality")
                  )
              )
          ),
          
          # Parallel processing nodes
          div(class = "flowchart-row parallel-nodes",
              div(class = "flowchart-node", 
                  id = "node4a",
                  onclick = "Shiny.setInputValue('selected_node', 'node4a')",
                  "4a. Data Revision"),
              div(class = "flowchart-node", 
                  id = "node4b",
                  onclick = "Shiny.setInputValue('selected_node', 'node4b')",
                  "4b. Analysis")
          ),
          
          # Arrows converging
          div(class = "flowchart-row",
              div(style = "display: flex; justify-content: center; align-items: center;",
                  div(class = "arrow", "↘"),
                  div(class = "arrow", "↙")
              )
          ),
          
          # Final node
          div(class = "flowchart-row",
              div(class = "flowchart-node", 
                  id = "node5",
                  onclick = "Shiny.setInputValue('selected_node', 'node5')",
                  "5. Results & Report")
          )
      )
    )
  ),
  
  # Information panel
  card(
    card_header("Node Information"),
    card_body(
      uiOutput("node_info")
    )
  )
)

server <- function(input, output, session) {
  # Store selected node
  selected_node <- reactiveVal(NULL)
  
  # Node information content
  node_details <- list(
    "node1" = list(
      title = "1. Data Collection",
      description = "The first step in any data analysis process involves gathering data from various sources.",
      details = c(
        "• Identify data sources and requirements",
        "• Collect data from databases, APIs, files, or surveys",
        "• Ensure data completeness and accessibility",
        "• Document data sources and collection methods"
      ),
      icon = "📊"
    ),
    "node2" = list(
      title = "2. Data Cleaning",
      description = "Raw data often contains errors, inconsistencies, or missing values that need to be addressed.",
      details = c(
        "• Handle missing values (imputation or removal)",
        "• Remove duplicates and outliers",
        "• Standardize formats and naming conventions",
        "• Validate data types and ranges"
      ),
      icon = "🧹"
    ),
    "node3" = list(
      title = "3. Quality Check (Decision Point)",
      description = "A critical decision point to evaluate whether the data meets quality standards for analysis.",
      details = c(
        "• Assess data completeness and accuracy",
        "• Check for consistency across variables",
        "• Validate business rules and constraints",
        "• Decide: proceed to analysis or return for revision"
      ),
      icon = "✅"
    ),
    "node4a" = list(
      title = "4a. Data Revision",
      description = "When quality check fails, additional data cleaning and preprocessing is required.",
      details = c(
        "• Address identified quality issues",
        "• Re-collect data if necessary",
        "• Apply additional cleaning techniques",
        "• Loop back to quality check"
      ),
      icon = "🔄"
    ),
    "node4b" = list(
      title = "4b. Analysis",
      description = "When data passes quality check, proceed with the main analytical work.",
      details = c(
        "• Apply statistical methods and algorithms",
        "• Create visualizations and summaries",
        "• Test hypotheses and build models",
        "• Extract insights and patterns"
      ),
      icon = "📈"
    ),
    "node5" = list(
      title = "5. Results & Report",
      description = "The final step involves compiling findings and communicating results to stakeholders.",
      details = c(
        "• Summarize key findings and insights",
        "• Create visualizations and dashboards",
        "• Write comprehensive reports",
        "• Present recommendations and next steps"
      ),
      icon = "📋"
    )
  )
  
  # Update selected node when clicked
  observeEvent(input$selected_node, {
    selected_node(input$selected_node)
  })
  
  # Reset selection
  observeEvent(input$reset_btn, {
    selected_node(NULL)
  })
  
  # Update node styling based on selection
  observe({
    # Remove previous selection styling
    runjs("$('.flowchart-node').removeClass('selected');")
    
    # Add selection styling to current node
    if (!is.null(selected_node())) {
      runjs(paste0("$('#", selected_node(), "').addClass('selected');"))
    }
  })
  
  # Display node information
  output$node_info <- renderUI({
    if (is.null(selected_node())) {
      div(
        style = "text-align: center; padding: 40px; color: #666;",
        h4("👆 Click on any node above to learn more"),
        p("Select a step in the flowchart to see detailed information about that process.")
      )
    } else {
      node_data <- node_details[[selected_node()]]
      
      div(
        div(
          style = "display: flex; align-items: center; margin-bottom: 15px;",
          span(style = "font-size: 24px; margin-right: 10px;", node_data$icon),
          h4(node_data$title, style = "margin: 0;")
        ),
        
        p(node_data$description, style = "font-size: 16px; margin-bottom: 20px;"),
        
        h5("Key Activities:"),
        div(
          style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; border-left: 4px solid #4a90e2;",
          HTML(paste(node_data$details, collapse = "<br>"))
        ),
        
        br(),
        div(
          style = "text-align: center;",
          actionButton("close_info", "Close Details", 
                       onclick = "Shiny.setInputValue('selected_node', null)",
                       class = "btn-outline-secondary btn-sm")
        )
      )
    }
  })
}

# Add JavaScript for interactivity
addResourcePath("js", ".")
shinyApp(ui = ui, server = server)
