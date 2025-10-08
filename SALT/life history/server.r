library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(dplyr)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive data generation
  plot_data <- reactive({
    maxage<-5.4/input$M+0.2*(5.4/input$M)
    ages <- seq(0, maxage, by = 0.1)
    lengths <- seq(1,input$Linf+0.2*input$Linf , by = 1)
    
    # Growth curve (von Bertalanffy)
    length_at_age <- input$Linf * (1 - exp(-input$K * (ages - input$t0)))
    
    # Natural mortality
      mortality <- rep(input$M, length(ages))  # Constant mortality
      survival <- exp(-input$M * ages)  # Survival probability

    # Maturity ogive (logistic function)
    maturity_prob <- 1 / (1 + exp(-log(19) * (lengths - input$L50) / (input$L95 - input$L50)))
    
    # Weight-length relationship
    weight <- input$a * lengths^input$b
    
    list(
      ages = ages,
      lengths = lengths,
      length_at_age = length_at_age,
      mortality = mortality,
      survival = survival,
      maturity_prob = maturity_prob,
      weight = weight
    )
  })
  
  # Natural Mortality Plot
  output$mortality_plot <- renderPlotly({
    data <- plot_data()
    maxage<-5.4/input$M+0.2*(5.4/input$M)
       p <- ggplot(data.frame(Age = data$ages, Survival = data$survival), 
                  aes(x = Age, y = Survival)) +
        geom_line(color = "red", size = 1.2) +
        labs(title = paste("Natural mortality (M =", input$M, ")"),
             x = "Age (years)", y = "Survival Probability") +
        xlim(c(0,maxage))+
         theme_minimal()+
         annotate("text",x=maxage*0.8,y=0.9,label=paste0("Max age = ",5.4/input$M),col="red",size=unit(5, "pt"))
    
    ggplotly(p)
  })
  
  # Growth Plot
  output$growth_plot <- renderPlotly({
    data <- plot_data()
    maxage<-5.4/input$M+0.2*(5.4/input$M)
    age_at_size<-input$t0-((log(1-(c(input$L50,input$L95)/input$Linf))/input$K))
    size_at_maxage<-input$Linf * (1 - exp(-input$K * (maxage - input$t0)))

    p <- ggplot(data.frame(Age = data$ages, Length = data$length_at_age), 
                aes(x = Age, y = Length)) +
      geom_line(color = "blue", size = 1.2) +
      geom_hline(yintercept = input$Linf, linetype = "dashed", color = "gray") +
      xlim(c(0,maxage))+
      geom_point(aes(x=age_at_size[1],y=input$L50),col="purple",size=4)+
      geom_point(aes(x=age_at_size[2],y=input$L95),col="purple",size=4)+
      geom_point(aes(x=5.4/input$M,y=size_at_maxage),col="red",size=4)+
      #geom_point(aes(x=c(age_at_size,5.4/input$M),y=c(input$L50,input$L95,size_at_maxage)))+
      labs(title = "von Bertalanffy Growth Curve",
           x = "Age (years)", y = "Length (cm)") +
      annotate("text", x = maxage * 0.7, y = input$Linf + 5, 
               label = paste("L∞ =", input$Linf), color = "gray") +
      annotate("text", x = c(age_at_size+age_at_size*0.1,5.4/input$M), y = c(y=input$L50-input$L50*0.1,y=input$L95-input$L95*0.1,size_at_maxage-0.1*size_at_maxage), 
               label = c("L50","L95","Max age"), color = c("purple","purple","red"),hjust = 0) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Maturity Plot
  output$maturity_plot <- renderPlotly({
    data <- plot_data()
    
    p <- ggplot(data.frame(Length = data$lengths, Maturity = data$maturity_prob), 
                aes(x = Length, y = Maturity)) +
      geom_line(color = "green", size = 1.2) +
      geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
      geom_vline(xintercept = input$L50, linetype = "dashed", color = "gray") +
      labs(title = "Proportion Mature at Length",
           x = "Length (cm)", y = "Proportion Mature") +
      ylim(0, 1) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Weight-Length Plot
  output$weight_length_plot <- renderPlotly({
    data <- plot_data()
    
    p <- ggplot(data.frame(Length = data$lengths, Weight = data$weight), 
                aes(x = Length, y = Weight)) +
      geom_line(color = "purple", size = 1.2) +
      labs(title = paste("Weight-Length: W =", input$a, "× L^", input$b),
           x = "Length (cm)", y = "Weight (kg)") +
      theme_minimal()
    
    ggplotly(p)
  })
}
