library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(shinyWidgets)
library(viridis)
library(plotly)

ui <- page_sidebar(
  title = "Indicators: The essence of stock assessment",
  
  sidebar = sidebar(width=300,
    h4("Data Options"),
    
             pickerInput(inputId = 'stock.choice',
                label = 'Choose A Stock',
                choices = c("A","B","C","D"),
                options = list(`style` = "btn-info")),

            downloadButton("download_indicator_data", "Download Stock Data", class = "btn-primary"),
    
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
    h5("Select indicator (I), reference point (RP), and control rule (CR) values"),
    h6("These are determined from analyzing the downloaded stock data"),
    
    h6(strong("Catch data")),
    fluidRow(
      column(width = 6,
    numericInput(
      "Ct_I_in",
      "I:",
      value = 0,
      min = NA, 
      max = NA,
      step = 0.01
      )),
    column(width = 6,
           numericInput(
             "Ct_RP_in",
             "RP:",
             value = 0,
             min = NA, 
             max = NA,
             step = 0.01
           ))),
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
      helpText("Use complete R function calls if using things like mean(), etc.")
    ),
    
    
    h6(strong("Index data")),
    fluidRow(
      column(width = 6,
             numericInput(
               "I_I_in",
               "I:",
               value = 0,
               min = NA, 
               max = NA,
               step = 0.01
             )),
      column(width = 6,
             numericInput(
               "I_RP_in",
               "RP:",
               value = 0,
               min = NA, 
               max = NA,
               step = 0.01
             ))),
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
      helpText("Use complete R function calls if using things like mean(), etc.")
    ),
    
    
    
    h6(strong("Mean length data")),
    fluidRow(
      column(width = 6,
             numericInput(
               "Lt_I_in",
               "I:",
               value = 0,
               min = NA, 
               max = NA,
               step = 0.01
             )),
      column(width = 6,
             numericInput(
               "Lt_RP_in",
               "RP:",
               value = 0,
               min = NA, 
               max = NA,
               step = 0.01
             ))),


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
      helpText("Use complete R function calls if using things like mean(), etc.")
    ),
    
        
    # Calculate button
    actionButton(
      "calculate_cr",
      "Run Control Rule(s)",
      class = "btn-primary",
      style = "width: 100%; margin-top: 10px;"
      )  
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
        tags$li("MM = Management Metric. Examples are catch limits, effort, etc."),
        tags$li("y = current year"),
        tags$li(paste0("z = years before current year",em("y"))),
        tags$li("Modifer = "),
        tags$ol(
          tags$li("Indicator (I) compared to reference point (RP). The indicator measures the current state. The reference point defines the desireable (e.g., target) and/or undesireable (e.g., limit) value of the stock, in the same metric as the indicator. For example, is the indicator more or less than the reference point?"),
          tags$li("Control rule (CR)= the rule to adjust the managaement metric based on comparison to reference point.")
        ),
      ),
      p("The control rule would then be the Modifier used in the general equation."),
      p("An example of a simple control rule based on an Indicator (I) and Reference Point (RP) is:."),
      div(
        style = "text-align: center; font-size:20px; margin: 10px 0;",
        "$$\\ CR = \\frac{I}{RP}$$"
      ),
      p("Indicators can be model-free (using the data directly) or model-based (from a stock assessment)."),
     p("Examples of indicators are:"),
      tags$ul(
        tags$li("Model free:"),
        tags$ol(
          tags$li("Average Catch"),
          tags$li("Indices of abundance (e.g., CPUE, density)"),
          tags$li("Size or age-based metrics (mean values, SPR, relative to maturity, etc)"),
          tags$li("Species composition"),
          tags$li("Habitat condition or availability"),
          tags$li("Fishing behavior (e.g, Distance traveled to fishing ground)"),
          tags$li("Species composition")
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
    #Summary statistics card
    card(
      card_header("Summary Statistics"),
      tableOutput("summary_stats")
    ),

        col_widths = c(12,12,12,12)
    )
  )

  
  
)





server <- function(input, output, session) {
  
  # Reactive function to parse and process time series data
  
  
  stock.data <- readRDS("stock_data.RDS")
  data.colors<-viridis(3)
  
  data.sub<-reactive({
        data.sub<-subset(stock.data,Stock==input$stock.choice)
      })
  
  
  # observeEvent(input$calculate_cr, {
  #   # Code here runs ONLY when 'event_expression' changes or is triggered
  #   print("An event occurred!")
  #   showModal(modalDialog(title = "Triggered", "The event happened!"))
  # })
  #Equation choices

  #CR_calc_Ct<-eventReactive(input$calculate_cr, {
    CR_calc_Ct<-reactive({
      
      CR_calc_Ct<-NA
        if (input$ct_equation_type == "ct_custom") {
              cr.calc.ct<-input$ct_custom_cr
      } 
      else {
        cr.calc.ct<-switch(input$ct_equation_type,
               "ct_ratio" = "I/RP",
               "ct_cubic" = "0.2*((I/RP)-1)^3",
               "ct_cubicpoly" = "0.2*((I/RP)-1)^3+0.05*((I/RP)-1)" 
              )
      }
#      if(!is.null(input$data.id) & any(input$data.id=="Catch"))
#      {
      I=input$Ct_I_in 
      RP= input$Ct_RP_in 
      if(I!=0 & RP != 0){CR_calc_Ct<-eval(parse(text=cr.calc.ct))}
 #     }
      CR_calc_Ct
    })
  
    CR_calc_Ind<-reactive({
      
      CR_calc_Ind<-NA
      if (input$ind_equation_type == "ind_custom") {
        cr.calc.ind<-input$ind_custom_cr
      } 
      else {
        cr.calc.ind<-switch(input$ind_equation_type,
                           "ind_ratio" = "I/RP",
                           "ind_cubic" = "0.2*((I/RP)-1)^3",
                           "ind_cubicpoly" = "0.2*((I/RP)-1)^3+0.05*((I/RP)-1)" 
        )
      }
      #      if(!is.null(input$data.id) & any(input$data.id=="Catch"))
      #      {
      I=input$I_I_in 
      RP= input$I_RP_in 
      if(I!=0 & RP != 0){CR_calc_Ind<-eval(parse(text=cr.calc.ind))}
      #     }
      CR_calc_Ind
    })
    
    CR_calc_Lt<-reactive({
    
      CR_calc_Lt<-NA
      if (input$lt_equation_type == "lt_custom") {
        cr.calc.lt<-input$lt_custom_cr
      } 
      else {
        cr.calc.lt<-switch(input$lt_equation_type,
                           "lt_ratio" = "I/RP",
                           "lt_cubic" = "0.2*((I/RP)-1)^3",
                           "lt_cubicpoly" = "0.2*((I/RP)-1)^3+0.05*((I/RP)-1)" 
        )
      }
      #      if(!is.null(input$data.id) & any(input$data.id=="Catch"))
      #      {
      I=input$Lt_I_in 
      RP= input$Lt_RP_in 
      if(I!=0 & RP != 0){CR_calc_Lt<-eval(parse(text=cr.calc.lt))}
      #     }
      CR_calc_Lt
    })
    
    
  output$data.indicator <- renderUI({
    checkboxGroupButtons(
      inputId = "data.id",
      label = "Which data to consider: ",
      choices = c("Catch", "Index", "Mean Length"),
      size="sm"
    )
  })
 
  output$download_indicator_data <- downloadHandler(
    filename = function() {
      paste("stock_",input$stock.choice,"_indicator_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(data.sub())
      write.csv(data.sub(), file, row.names = FALSE)
    }
  )
  
#Indicator plots
  output$stock_time_series_Ct_ui<-renderUI({

    if(!is.null(input$Ct_I_in)){I_in<-NA}
    if(!is.null(input$Ct_RP_in)){RP_in<-NA}
    if(input$Ct_I_in>=0){I_in<-input$Ct_I_in}
    if(input$Ct_RP_in>=0){RP_in<-input$Ct_RP_in}
    
    ct.label <- data.frame(
      Year = c(76, 76),
      Catch = c(I_in, RP_in),
      label = c("Indicator", "Ref. Pt.")
    )
  
    if(!is.null(input$data.id) & any(input$data.id=="Catch"))
    {
      output$stock_time_series_Ct <- renderPlotly({
        data.plot<-data.sub()
        ggplot(data.plot,aes(Year, Catch))+
          geom_line(col=data.colors[1])+
          geom_point(col=data.colors[1])+
          geom_point(data=ct.label,aes(Year,Catch,fill=label),shape=c(22,23),size=3)+
          scale_fill_manual(values=c("orange","red"))+
          labs(fill="")+
          theme_bw()+
          theme(legend.position = "bottom")+
          xlab("Year")+
          ylab("Total Catch")
      })
      
    }
  })

  output$stock_time_series_Index_ui<-renderUI({

    if(!is.null(input$I_I_in)){I_in<-NA}
    if(!is.null(input$I_RP_in)){RP_in<-NA}
    if(input$I_I_in>=0){I_in<-input$I_I_in}
    if(input$I_RP_in>=0){RP_in<-input$I_RP_in}
    
    ind.label <- data.frame(
    Year = c(76, 76),
    Index = c(I_in, RP_in),
    label = c("Indicator", "Ref. Pt.")
    )
      
    if(!is.null(input$data.id) & any(input$data.id=="Index"))
    {
      output$stock_time_series_Ct <- renderPlotly({
        data.plot<-data.sub()
        ggplot(data.plot,aes(Year, Index))+
          geom_line(col=data.colors[2])+
          geom_point(col=data.colors[2])+
          geom_point(data=ind.label,aes(Year,Index,fill=label),shape=c(22,23),size=3)+
          scale_fill_manual(values=c("orange","red"))+
          labs(fill="")+
          theme_bw()+
          theme(legend.position = "bottom")+
          xlab("Year")+
          ylab("Index")
      })
      
    }
  })
  
  output$stock_time_series_Lt_ui<-renderUI({
    
    if(!is.null(input$Lt_I_in)){I_in<-NA}
    if(!is.null(input$Lt_RP_in)){RP_in<-NA}
    if(input$Lt_I_in>=0){I_in<-input$Lt_I_in}
    if(input$Lt_RP_in>=0){RP_in<-input$Lt_RP_in}
    
    lt.label <- data.frame(
    Year = c(76, 76),
    Mean.Length = c(I_in, RP_in),
    label = c("Indicator", "Ref. Pt.")
    )
      
    
    if(!is.null(input$data.id) & any(input$data.id=="Mean Length"))
    {
      if(input$stock.choice=="A")
      {
       M.in<-0.0375
       Linf.in<-60.1
       k.in<-0.08
       t0.in<- -0.55
       L50.in<- 46.5
      }

      if(input$stock.choice=="B")
      {
        M.in<-0.068
        Linf.in<-42.8
        k.in<-0.13
        t0.in<- -0.94
        L50.in<- 29
      }
      
      if(input$stock.choice=="C")
      {
        M.in<-0.145
        Linf.in<-53
        k.in<-0.143
        t0.in<- -0.07
        L50.in<- 42
      }
      
      if(input$stock.choice=="D")
      {
        M.in<-0.099
        Linf.in<-57.38
        k.in<-0.128
        t0.in<- -2.4
        L50.in<- 39.4
      }
      
      
      output$stock_time_series_Ct <- renderPlotly({
        data.plot<-data.sub()
        ggplot(data.plot,aes(Year, Mean.Length))+
          geom_line(col=data.colors[3])+
          geom_point(col=data.colors[3])+
          geom_point(data=lt.label,aes(Year,Mean.Length,fill=label),shape=c(22,23),size=3)+
          scale_fill_manual(values=c("orange","red"))+
          labs(fill="")+
          theme_bw()+
          theme(legend.position = "bottom")+
          xlab("Year")+
          ylab("Mean Length")+
          geom_hline(aes(yintercept = Linf.in),col="blue",linetype = "longdash")+
          geom_hline(aes(yintercept = L50.in),col="purple",linetype = "dash")+
          ylim(0,NA)+
          annotate("text",x=1,y=Linf.in+0.05*Linf.in,label="Linf",col="blue")+
          annotate("text",x=1,y=L50.in+0.05*L50.in,label="Lmat50%",col="purple")
            })
      
    }
  })
  
  # Calculate control rule

  # Calculate statistics
  # stats <- reactive({
  #   data <- processed_data()
  #   if(is.null(data) || nrow(data) == 0) return(NULL)
  #   
  #   values <- data$value
  #   avg <- mean(values)
  #   
  #   list(
  #     count = length(values),
  #     mean = avg,
  #     median = median(values),
  #     sd = sd(values),
  #     min = min(values),
  #     max = max(values),
  #     target = input$target_value,
  #     difference = avg - input$target_value,
  #     percent_diff = ((avg - input$target_value) / input$target_value) * 100
  #   )
  # })
  
  output$summary_stats <- renderTable({
    
    CR.in_Ct<-CR_calc_Ct()
    CR.in_I<-CR_calc_Ind()
    CR.in_Lt<-CR_calc_Lt()
    data.frame(
        Statistic = c("Ct I", "Ct RP", "Control Rule", "CR value"),
        Catch_CR = c(
          input$Ct_I_in,
          input$Ct_RP_in,
          input$ct_equation_type,
          round(CR.in_Ct,3)
        ),
        Index_CR = c(
          input$I_I_in,
          input$I_RP_in,
          input$ind_equation_type,
          round(CR.in_I,3)
        ),
        Length_CR = c(
          input$Lt_I_in,
          input$Lt_RP_in,
          input$lt_equation_type,
          round(CR.in_Lt,3)
        )
        # Length_CR = c(
        #   input$Lt_I_in,
        #   input$Lt_RP_in,
        #   input$lt_equation_type,
        #   CR.in_Lt
        # )
      )
    }, striped = TRUE)
  
  # Summary statistics table
  # output$summary_stats <- renderTable({
  #   s <- stats()
  #   if(is.null(s)) return(data.frame(Statistic = "No valid data", Value = ""))
  #   
  #   data.frame(
  #     Statistic = c("Count", "Mean", "Median", "Std Dev", "Min", "Max"),
  #     Value = c(
  #       s$count,
  #       round(s$mean, 3),
  #       round(s$median, 3),
  #       round(s$sd, 3),
  #       round(s$min, 3),
  #       round(s$max, 3)
  #     )
  #   )
  # }, striped = TRUE)
  # 
  # # Comparison results table
  # output$comparison_results <- renderTable({
  #   s <- stats()
  #   if(is.null(s)) return(data.frame(Metric = "No valid data", Value = ""))
  #   
  #   status <- ifelse(s$difference > 0, "Above Target", 
  #                    ifelse(s$difference < 0, "Below Target", "On Target"))
  #   
  #   data.frame(
  #     Metric = c("Average", "Target", "Difference", "% Difference", "Status"),
  #     Value = c(
  #       round(s$mean, 3),
  #       round(s$target, 3),
  #       round(s$difference, 3),
  #       paste0(round(s$percent_diff, 2), "%"),
  #       status
  #     )
  #   )
  # }, striped = TRUE)
  
}

shinyApp(ui = ui, server = server)
