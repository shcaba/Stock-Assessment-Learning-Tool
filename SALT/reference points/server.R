library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive function to calculate control rule
  control_rule_data <- reactive({
    # Validate inputs
    req(input$b_nocatch, input$b_target, input$E_msy, 1)
    
    # Ensure b_target > b_nocatch
#    if (input$b_target <= input$b_nocatch) {
#      updateNumericInput(session, "b_target", value = input$b_nocatch + 0.1)
#    }
    
    # Create sequence of stock sizes
    stock_ratio <- round(seq(0, 1, by = 0.01),2)
    data.frame(
      stock_ratio = stock_ratio,
      catch = stock_ratio*input$E_msy
    )
    
    
    # Create linear models
    
    # Calculate catch based on control rule type
  #   catch_values <- sapply(stock_ratio, function(b) {
  #     browser()
  #     if (b <= input$b_target) {
  #       # Below limit: no fishing
  #       return(stock_ratio*input$E_msy)
  #       } 
  #     else if (b >= input$b_target) {
  #       # Above target: maximum sustainable catch
  #       #return(input$max_catch)
  #       #return(1)
  #     #} else {
  #       # Between limit and target: depends on rule type
  #       #ratio <- (b - input$b_nocatch) / (input$b_target - input$b_nocatch)
  #       
  #       if(input$rule_type == "linear"){
  #         #return(input$max_catch * ratio)
  #         #return(lm(c(0,input$E_msy)~c(0,input$b_target))$coeff[2]*ratio)
  #         return(stock_ratio*input$E_msy)
  #       } 
  #       
  #       if(input$rule_type == "hockey"){
  #         #return(ifelse(ratio > 0.5, input$max_catch, input$max_catch * ratio * 2))
  #         return(ifelse(stock_ratio>=input$b_target,input$b_target*input$E_msy))
  #       } 
  #       
  #       #else if (input$rule_type == "smooth") {
  #         # Smooth S-curve transition
  #       #  smooth_ratio <- 1 / (1 + exp(-10 * (ratio - 0.5)))
  #       #  return(input$max_catch * smooth_ratio)
  #       #}
  #     }
  #   })
  # 
  #       data.frame(
  #     stock_ratio = stock_ratio,
  #     catch = catch_values
  #   )
   })
  
  # Generate the control rule plot
  output$control_rule_plot <- renderPlot({
    data <- control_rule_data()
    #Add threshhold option  
    data$thresh<-data$constant<-NA
    thresh.coefs<-coef(lm(c(0,data[data$stock_ratio==input$b_target,]$catch)~c(input$b_nocatch,input$b_target)))
    data$thresh[data$stock_ratio>=input$b_nocatch & data$stock_ratio<=input$b_target]<-thresh.coefs[2]*data$stock_ratio[data$stock_ratio>=input$b_nocatch & data$stock_ratio<=input$b_target]+thresh.coefs[1]
    data$constant[data$stock_ratio>=input$b_target]<-thresh.coefs[2]*data$stock_ratio[data$stock_ratio==input$b_target]+thresh.coefs[1]

    p <- ggplot(data, aes(x = stock_ratio, y = catch)) +
      geom_line(color = "blue", size = 2) +
      geom_line(aes(x = stock_ratio, y = catch*input$buffer),color = "#390878", size = 2,linetype="dotted") +
      geom_line(aes(x = stock_ratio, y = thresh),color = "orange", size = 2) +
      geom_line(aes(x = stock_ratio, y = thresh*input$buffer),color = "#390878", size = 2,linetype="dotted") +
      geom_line(aes(x = stock_ratio, y = constant),color = "#005595", size = 2) +
      geom_line(aes(x = stock_ratio, y = constant*input$buffer),color = "#390878", size = 2,linetype="dotted") +
      geom_point(aes(data[data$stock_ratio==input$current_stock,1],data[data$stock_ratio==input$current_stock,2]),size=5, color="black",fill="white")+
      #geom_abline(intercept = thresh.coefs[1],slope = thresh.coefs[2], 
      #           color = "orange", linetype = "dashed", size = 1) +
      geom_vline(xintercept = input$b_limit, 
                 color = "red", linetype = "dashed", size = 1) +
      geom_vline(xintercept = input$b_target, 
                 color = "#5D9741", linetype = "dashed", size = 1) +
      #geom_vline(xintercept = input$current_stock, 
      #           color = "orange", linetype = "solid", size = 1.5) +
      geom_hline(yintercept = 0, color = "black", linetype = "solid", alpha = 0.3) +
      coord_cartesian(clip = "off", ylim = c(-0.025*input$E_msy, input$E_msy)) +
      xlim(0, 1)+ 
    
      # Add reference point labels
      annotate("text", x = input$b_limit, y = 0.025*input$E_msy, 
               label = paste("Limit RP =", input$b_limit), 
               color = "red", hjust = -0.1) +
      annotate("text", x = input$b_target, y = 0.025*input$E_msy,
               label = paste("Target RP =", input$b_target), 
               color = "#5D9741", hjust = -0.1) +
      annotate("text", x = data$stock_ratio[93], y =input$E_msy ,
               label = paste("Constant fishing rate"), 
               color = "blue", hjust = 0.1) +
      annotate("text", x = data$stock_ratio[97], y = max(data$constant,na.rm = TRUE),
               label = paste("Constant catch"), 
               color = "#005595", vjust = -1.5) +
      annotate("text", x = input$b_nocatch, y = -0.025*input$E_msy,
               label = paste("No catch =", input$b_nocatch), 
               color = "black", hjust = -0.1) +
      annotate("text", x = input$current_stock, y = data[data$stock_ratio==input$current_stock,2] * 1, 
               label = "Current Stock", 
               color = "black", hjust = 0.5,vjust =-2.5) +
      
      # Styling
      labs(
        #title = paste("Fisheries Control Rule -", stringr::str_to_title(input$rule_type), "Type"),
        title = paste("Harvest Control Rule"),
        x = "Relative Stock Size (SB/SB₀)",
        y = "Relative Catch",
        subtitle = "Red = Limit Reference Point; Green = Target Reference Point; Black dot = Current Stock; Purple dots= Buffered catches rule"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray60"),
        panel.grid.minor = element_blank()
      ) 
      #ylim(0, input$E_msy)
    
    # Add zone coloring
    p <- p + 
      annotate("rect", xmin = 0, xmax = input$b_limit, 
               ymin = 0, ymax = input$E_msy, 
               alpha = 0.1, fill = "red") +
      annotate("rect", xmin = input$b_limit, xmax = input$b_target, 
               ymin = 0, ymax = input$E_msy, 
               alpha = 0.1, fill = "yellow") +
      annotate("rect", xmin = input$b_target, xmax = 1, 
               ymin = 0, ymax = input$E_msy, 
               alpha = 0.1, fill = "#5D9741")
    
    print(p)
  })
  
  # Generate stock status summary
  output$stock_status <- renderText({
    current_catch <- control_rule_data() %>%
      filter(abs(stock_ratio - input$current_stock) == min(abs(stock_ratio - input$current_stock))) %>%
      pull(catch) %>%
      first()
    #browser()
    
    data <- control_rule_data()
    #Add threshhold option  
    data$thresh<-data$constant<-NA
    thresh.coefs<-coef(lm(c(0,data[data$stock_ratio==input$b_target,]$catch)~c(input$b_nocatch,input$b_target)))
    data$thresh[data$stock_ratio>=input$b_nocatch & data$stock_ratio<=input$b_target]<-thresh.coefs[2]*data$stock_ratio[data$stock_ratio>=input$b_nocatch & data$stock_ratio<=input$b_target]+thresh.coefs[1]
    data$constant[data$stock_ratio>=input$b_target]<-thresh.coefs[2]*data$stock_ratio[data$stock_ratio==input$b_target]+thresh.coefs[1]
    
    curr_stock_catch<-data[data$stock_ratio==input$current_stock,]
    
    paste0(
      #"Overfishing limit at current stock size: ", round(current_catch, 3), " (relative units)\n",
      "Overfishing limit (OFL) at current stock size: ", round(curr_stock_catch[2],3), " (relative units)\n",
      "Threshhold control rule catch: ", round(curr_stock_catch[4],3), " (relative units)\n",
      "Buffered catch (e.g., ABC): ", round(curr_stock_catch[4]*input$buffer,3), " (relative units)\n",
      "Constant catch (catch at FMSY proxy at target biomass): ", round(curr_stock_catch[3]*input$buffer,3), " (relative units)\n"
    )
  })

  output$stock_status_RPs <- renderText({
    status <- if (input$current_stock <= input$b_nocatch) {
      "CRITICAL - Below Limit Reference Point"
    } else if (input$current_stock < input$b_target) {
      "CAUTIOUS - Between Limit and Target"
    } else {
      "HEALTHY - At or above Target Reference Point"
    }
    
    
    paste0(
      "Current Stock Size (SB/SB₀): ", round(input$current_stock, 3), "\n",
      "Current Stock Status: ", status, "\n",
      "No Catch Point: ", input$b_nocatch, "\n",
      "Limit (Overfished) Reference Point: ", input$b_limit, "\n",
      "Target Reference Point: ", input$b_target, "\n\n",
      "Management Zones:\n",
      "• RED (0 - ", input$b_limit, "): Overfished - rebuilding plan\n",
      "• YELLOW (", input$b_limit, " - ", input$b_target, "): Precautionary - Reduced fishing\n",
      "• GREEN (", input$b_target, "+): Healthy - Full fishing allowed"
    )
  })  
}