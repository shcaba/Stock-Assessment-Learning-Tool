library(shiny)
library(bslib)
library(shinyjs)

# Define the decision tree structure
decision_tree <- list(
  "root" = list(
    question = "Do you have a removal time series AND/OR an measure of absolute abundance?",
    choices = list(
      "YES" = "scale_node",
      "NO " = "status_node"
    )
  ),

  ##############  
  #Scale models#
  ##############
  "scale_node" = list(
    question = "Do you have biological (lengths or ages) compositions?",
    choices = list(
      "YES: biological data are available" = "scale_bio_node",
      "NO: biological data are NOT available" = "scale_nobio_node"
    ),
    path = c("Catch: YES")
  ),

  "scale_bio_node" = list(
    question = "Do you have a relative index of abundance?",
    choices = list(
      "YES: at least one abundance index is available" = "scale_bio_index_node",
      "NO: no abudance indices are available" = "scale_bio_noindex_node"
    ),
    path = c("Catch: YES","Bio comps: YES")
  ),
  
  "scale_bio_index_node" = list(
    outcome = "You have the all the data types to do a traditional FULLY INTEGRATED stock assessment model.",
    path = c("Catch: YES","Bio comps: YES","Index of abundance: YES")
  ),

  "scale_bio_noindex_node" = list(
    outcome = "You have the the data types to do a CATCH + LENGTH and/or AGE model.",
    path = c("Catch: YES","Bio comps: YES","Index of abundance: NO")
  ),
  
  "scale_nobio_node" = list(
    question = "Do you have a relative index of abundance?",
    choices = list(
      "YES: at least one abundance index is available" = "scale_nobio_index_node",
      "NO: no abudance indices are available" = "scale_nobio_noindex_node"
    ),
    path = c("Catch: YES","Bio comps: NO")
  ),
  
  "scale_nobio_index_node" = list(
    outcome = "You have the the data types to do a SURPLUS PRODUCTION model. This can be age-based if using an age-structured model.",
    path = c("Catch: YES","Bio comps: NO","Index of abundance: YES")
  ),
  
  "scale_nobio_noindex_node" = list(
    outcome = "You have the data to do a CATCH ONLY model.",
    path = c("Catch: YES","Bio comps: NO","Index of abundance: NO")
  ),
  
  ###############
  #Status models#
  ###############
  "status_node" = list(
    question = "Do you have biological (lengths or ages) compositions?",
    choices = list(
      "YES: biological data are available" = "status_bio_node",
      "NO: biological data are NOT available" = "status_nobio_node"
    ),
    path = c("Catch: NO")
  ),
  
  "status_bio_node" = list(
    question = "Do you have a relative index of abundance?",
    choices = list(
      "YES: at least one abundance index is available" = "status_bio_index_node",
      "NO: no abudance indices are available" = "status_bio_noindex_node"
    ),
    path = c("Catch: NO","Bio comps: YES")
  ),
  
  "status_bio_index_node" = list(
    outcome = "You have the data types to do a MULTI-INDICATOR approach.",
    path = c("Catch: NO","Bio comps: YES","Index of abundance: YES")
  ),
  
  "status_bio_noindex_node" = list(
    outcome = "You have the the data types to do a LENGTH and/or AGE ONLY model.",
    path = c("Catch: NO","Bio comps: YES","Index of abundance: NO")
  ),
  
  "status_nobio_node" = list(
    question = "Do you have a relative index of abundance?",
    choices = list(
      "YES: at least one abundance index is available" = "status_nobio_index_node",
      "NO: at least one abundance index is available" = "status_nobio_noindex_node"
    ),
    path = c("Catch: NO","Bio comps: NO")
  ),
  
  "status_nobio_index_node" = list(
    outcome = "You have the the data types to do an ABUNDANCE INDICATOR approach.",
    path = c("Catch: NO","Bio comps: NO","Index of abundance: YES")
  ),
  
  "status_nobio_noindex_node" = list(
    outcome = "Lacking the big three data types, a RISK ANALYSIS seems a good option.",
    path = c("Catch: NO","Bio comps: NO","Index of abundance: NO")
  )
  
)

ui <- page_sidebar(
  title = "Navigating through the Stock Assessment Continuum",
  
  sidebar = div(
    # Reset button
    actionButton("reset", "Begin Again", 
                 class = "btn-outline-primary mb-3", 
                 style = "width: 50%;"),
    
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
          h1("Welcome to the Stock Assessment Continuum navigator", class = "text-center")
        ),
        card_body(
          p("This interactive decision tree will help you navigate through different stock assessment options.", 
            class = "text-center"),
          p("Click 'Start' to traverse the Stock Assessment Continuum (SAC).", class = "text-center"),
          div(
            actionButton("start", "Start SAC Decision Tree", 
                         class = "btn-outline-primary mb-3"),
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
          actionButton("go_back", "Go Back", 
                       width="10%",
#                       style='padding:4px; font-size:100%'),
                        class = "text-center",
                        class = "btn-outline-secondary"),
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
            actionButton("start_over", "Begin Again", 
                         class = "btn-primary"),
            actionButton("go_back_outcome", "Go Back", 
                         class = "btn-outline-secondary"),
            imageOutput("SACImage_out")
          )
                  ),
        card_body(
          h6("All scale-based options, as well as the length and/or age only models, can be done in ",tags$a(href = "https://github.com/shcaba/SS-DL-tool", "The Stock Assessment Continuum Tool", target = "_blank")),
          h6(tags$a(href = "https://connect.fisheries.noaa.gov/psa/", "The Productivity-Susceptibility Analysis", target = "_blank")," is a form of Risk Analysis that can be used when all other data are unavailable."),
          h6("Indicator approaches can be explored using the Indicator module of the Stock Assessment Learning Tool")
        )
      )
    )
  ),
  
  # Add custom CSS
  tags$head(
    tags$style(HTML("
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
    "))
  )
)

server <- function(input, output, session) {
  # Reactive values to track state
  current_node <- reactiveVal("welcome")
  node_history <- reactiveVal(character(0))
  
  # Initialize useShinyjs
  useShinyjs()
  
  output$SACImage_init <- renderImage({
    
    list(
      src = file.path(getwd(),"SAC_pics/SAC.jpg"),
      alt = "SAC decision tree",
      width = "50%",
      height = "100%",
      style = "display: block; margin-left: auto; margin-right: auto;"
    )
  }, deleteFile = FALSE)
  
  output$SACImage <- renderImage({

      list.out<-list(
        src = file.path(getwd(),"SAC_pics/SAC.jpg"),
        alt = "SAC decision tree",
        width = "50%",
        height = "100%",
        style = "display: block; margin-left: auto; margin-right: auto;"
      )
  
    if(current_node()=="root"){list.out$src = src = file.path(getwd(),"SAC_pics/SAC.jpg")}
    if(current_node()=="scale_node"){list.out$src = file.path(getwd(),"SAC_pics/Scale_bio.jpg")}
    if(current_node()=="scale_bio_node"){list.out$src = file.path(getwd(),"SAC_pics/Scale_bioYES_Index.jpg")}
    if(current_node()=="scale_nobio_node"){list.out$src = file.path(getwd(),"SAC_pics/Scale_bioNO_Index.jpg")}
    if(current_node()=="status_node"){list.out$src = file.path(getwd(),"SAC_pics/Status_bio.jpg")}
    if(current_node()=="status_bio_node"){list.out$src = file.path(getwd(),"SAC_pics/Status_bioYES_Index.jpg")}
    if(current_node()=="status_nobio_node"){list.out$src = file.path(getwd(),"SAC_pics/Status_bioNO_Index.jpg")}
    
    list.out
    
    }, deleteFile = FALSE)
  
  output$SACImage_out <- renderImage({

    list.end<-list(
      src = file.path(getwd(),"SAC_pics/SAC.jpg"),
      alt = "SAC decision tree out",
      width = "50%",
      height = "100%",
      style = "display: block; margin-left: auto; margin-right: auto;"
    )

    if(current_node()=="scale_bio_index_node"){list.end$src = file.path(getwd(),"SAC_pics/Scale_bioYES_IndexYES_IA.jpg")}
    if(current_node()=="scale_bio_noindex_node"){list.end$src = file.path(getwd(),"SAC_pics/Scale_bioYES_IndexNO_CL.jpg")}
    if(current_node()=="scale_nobio_index_node"){list.end$src = file.path(getwd(),"SAC_pics/Scale_bioNO_IndexYES_SP.jpg")}
    if(current_node()=="scale_nobio_noindex_node"){list.end$src = file.path(getwd(),"SAC_pics/Scale_bioNO_IndexNO_CtO.jpg")}
    if(current_node()=="status_bio_index_node"){list.end$src = file.path(getwd(),"SAC_pics/Status_bioYES_IndexYES_MInd.jpg")}
    if(current_node()=="status_bio_noindex_node"){list.end$src = file.path(getwd(),"SAC_pics/Status_bioYES_IndexNO_LAO.jpg")}
    if(current_node()=="status_nobio_index_node"){list.end$src =  file.path(getwd(),"SAC_pics/Status_bioNO_IndexYES_Indicator.jpg")}
    if(current_node()=="status_nobio_noindex_node"){list.end$src = file.path(getwd(),"SAC_pics/Status_bioNO_IndexNO_RA.jpg")}
    
    list.end

    }, deleteFile = FALSE)
  
    # Track if we're at root
  output$at_root <- reactive({
    current_node() == "welcome"
  })
  outputOptions(output, "at_root", suspendWhenHidden = FALSE)
  
  # Track if we're at an outcome
  output$at_outcome <- reactive({
    node <- current_node()
    node != "welcome" && !is.null(decision_tree[[node]]$outcome)
  })
  outputOptions(output, "at_outcome", suspendWhenHidden = FALSE)
  
  # Show path when not at welcome
  output$show_path <- reactive({
    current_node() != "welcome"
  })
  outputOptions(output, "show_path", suspendWhenHidden = FALSE)
  
  # Display current question
  output$question_text <- renderText({
    node <- current_node()
    if (node != "welcome" && !is.null(decision_tree[[node]]$question)) {
      decision_tree[[node]]$question
    } else {
      ""
    }
  })
  
  # Display outcome text
  output$outcome_text <- renderText({
    node <- current_node()
    if (!is.null(decision_tree[[node]]$outcome)) {
      decision_tree[[node]]$outcome
    } else {
      ""
    }
  })
  
  # Generate choice buttons
  output$choices_ui <- renderUI({
    node <- current_node()
    if (node != "welcome" && !is.null(decision_tree[[node]]$choices)) {
      choices <- decision_tree[[node]]$choices
      choice_buttons <- lapply(names(choices), function(choice_name) {
        actionButton(
          inputId = paste0("choice_", gsub("[^A-Za-z0-9]", "_", choice_name)),
          label = choice_name,
          width = "25%",
          class = "text-center",
          class = "btn-outline-info choice-button",
          onclick = paste0("Shiny.setInputValue('selected_choice', '", choice_name, "');")
        )
      })
      do.call(div, choice_buttons)
    }
  })
  
  # Display path taken
  output$path_display <- renderText({
    node <- current_node()
    if (node != "welcome" && !is.null(decision_tree[[node]]$path)) {
      path_items <- decision_tree[[node]]$path
      path_html <- paste(
        sapply(path_items, function(item) {
          paste0('<span class="path-item">', item, '</span>')
        }),
        collapse = ' → '
      )
      path_html
    } else {
      ""
    }
  })
  
  # Start the decision tree
  observeEvent(input$start, {
    current_node("root")
    node_history(character(0))
  })
  
  # Handle choice selection
  observeEvent(input$selected_choice, {
    current <- current_node()
    if (!is.null(decision_tree[[current]]$choices)) {
      choice_name <- input$selected_choice
      next_node <- decision_tree[[current]]$choices[[choice_name]]
      
      # Update history and move to next node
      node_history(c(node_history(), current))
      current_node(next_node)
      # print(current_node)
      # print(node_history)
    }
  })
  
  # Go back functionality
  observeEvent(input$go_back, {
    history <- node_history()
    if (length(history) > 0) {
      # Go back to previous node
      current_node(history[length(history)])
      node_history(history[-length(history)])
    } else {
      # Go back to welcome if no history
      current_node("welcome")
    }
  })
  
  # Go back from outcome
  observeEvent(input$go_back_outcome, {
    history <- node_history()
    if (length(history) > 0) {
      current_node(history[length(history)])
      node_history(history[-length(history)])
    } else {
      current_node("welcome")
    }
  })
  
  # Reset to beginning
  observeEvent(c(input$reset, input$start_over), {
    current_node("welcome")
    node_history(character(0))
  })
}

shinyApp(ui = ui, server = server)
