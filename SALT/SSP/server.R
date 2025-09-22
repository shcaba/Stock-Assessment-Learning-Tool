#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinyWidgets)
library(r4ss)
library(viridis)
library(reshape2)
library(ggplot2)
library(plotly)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  #Folder names
  ssp.folder.names<-c("Status20%",
                      "Status60%",
                      "Status20%_Scale40%_estCt",
                      "Status60%_Scale40%_estCt",
                      "Status40%_h_low",
                      "Status40%_h_hi",
                      "Status40%_M_low",
                      "Status40%_M_hi",
                      "Status40%_Scale40%_estCt_h_low",
                      "Status40%_Scale40%_estCt_h_hi",
                      "Status40%_Scale40%_estCt_M_low",
                      "Status40%_Scale40%_estCt_M_hi",
                      "Status_est_Scale40%_h_low",
                      "Status_est_Scale40%_h_ref",
                      "Status_est_Scale40%_h_hi",
                      "Status_est_Scale40%_M_low",
                      "Status_est_Scale40%_M_ref",
                      "Status_est_Scale40%_M_hi")
    ssp.folder.names.in.status<-ssp.folder.names[1:2]
    ssp.folder.names.in.scale<-ssp.folder.names[3:4]
    ssp.folder.names.in.prod<-ssp.folder.names[5:8]
    ssp.folder.names.in.status_prod<-ssp.folder.names[13:18]
    ssp.folder.names.in.scale_prod<-ssp.folder.names[9:12]
    
    #SSP choices for user
    SSP_choices<-c(
                    "Status 20%",
                    "Status 60%",
                    "Status 20%, Scale estimate through catch",
                    "Status 60%, Scale estimate through catch",
                    "Steepness low",
                    "Steepness high",
                    "M low",
                    "M high",
                    "Scale estimate through catch, Steepness low",
                    "Scale estimate through catch, Steepness high",
                    "Scale estimate through catch, M low",
                    "Scale estimate through catch, M high",
                    "Estimate status, M low",
                    "Estimate status, M reference",
                    "Estimate status, M high",
                    "Estimate status, Steepness low",
                    "Estimate status, Steepness reference",
                    "Estimate status, Steepness high")
    
    SSP_choices.in.status<-SSP_choices[1:2]
    SSP_choices.in.scale<-SSP_choices[3:4]
    SSP_choices.in.prod<-SSP_choices[5:8]
    SSP_choices.in.status_prod<-SSP_choices[13:18]
    SSP_choices.in.scale_prod<-SSP_choices[9:12]
    
    output$SSP_model_picks_status<-renderUI({
      pickerInput(
        inputId = "myPicker_SSP_status",
        label = "Status",
        choices = SSP_choices.in.status,
        options = list(
          `actions-box` = TRUE,
          size = 12,
          `selected-text-format` = "count > 3"
        ),
        multiple = TRUE
      )
    })
  
    output$SSP_model_picks_scale<-renderUI({
      pickerInput(
        inputId = "myPicker_SSP_scale",
        label = "Scale",
        choices = SSP_choices.in.scale,
        options = list(
          `actions-box` = TRUE,
          size = 12,
          `selected-text-format` = "count > 3"
        ),
        multiple = TRUE
      )
    })
    
    output$SSP_model_picks_prod<-renderUI({
      pickerInput(
        inputId = "myPicker_SSP_prod",
        label = "Productivity",
        choices = SSP_choices.in.prod,
        options = list(
          `actions-box` = TRUE,
          size = 12,
          `selected-text-format` = "count > 3"
        ),
        multiple = TRUE
      )
    })

    output$SSP_model_picks_status_prod<-renderUI({
      pickerInput(
        inputId = "myPicker_SSP_status_prod",
        label = "Status and productivity",
        choices = SSP_choices.in.status_prod,
        options = list(
          `actions-box` = TRUE,
          size = 12,
          `selected-text-format` = "count > 3"
        ),
        multiple = TRUE
      )
    })

    output$SSP_model_picks_scale_prod<-renderUI({
      pickerInput(
        inputId = "myPicker_SSP_scale_prod",
        label = "Scale and productivity",
        choices = SSP_choices.in.scale_prod,
        options = list(
          `actions-box` = TRUE,
          size = 12,
          `selected-text-format` = "count > 3"
        ),
        multiple = TRUE
      )
    })
    
    output$SSP_model_picks_grouped<-renderUI({
      pickerInput(
        inputId = "myPicker_SSP_grouped",
        label = "Organized by change in the model",
        choices = list(
          "Status"= SSP_choices.in.status,
          "Scale"= SSP_choices.in.scale,
          "Productivity"= SSP_choices.in.prod,
          "Status and productivity"= SSP_choices.in.status_prod,
          "Scale and productivity" = SSP_choices.in.scale_prod
          ),
        options = list(
          `actions-box` = TRUE,
          size = 12,
          `selected-text-format` = "count > 3"
        ),
        multiple = TRUE
      )
    })
    
    
      
    
        
    #load model summaries
  
    
  observeEvent(req(input$run_SSP_comps),{
  load(paste0(getwd(),"/mod_summary.RDS"))  
  load(paste0(getwd(),"/Catches.RDS"))
  #myPicker_SSP<-c(input$myPicker_SSP_status,input$myPicker_SSP_scale,input$myPicker_SSP_prod,input$myPicker_SSP_status_prod,input$myPicker_SSP_scale_prod)
  #Dir_SSP<-getwd()  
  #SSP_mod_dir <-NULL
  #SSP_mod_dir[1]<-paste0(Dir_SSP,"/Models/Status40%")
  #browser()
  #for(i in 1:(length(input$myPicker_SSP)))
  #{SSP_mod_dir[i+1] <- paste0(Dir_SSP,"/Models/" ,paste(ssp.folder.names[SSP_choices%in%input$myPicker_SSP][i],collapse="_"))}
  #ssp.mod.prep<-r4ss::SSgetoutput(
  #dirvec=SSP_mod_dir,
#  keyvec=1:length(SSP.model.list), 
#  getcovar=FALSE
  #)

  #ssp_summary<- SSsummarize(ssp.mod.prep)
  #
    mod_indices<-c(2:ssp_summary$n)[SSP_choices%in%input$myPicker_SSP_grouped]
    SpawnOutput<-melt(id.vars=c("Yr"),ssp_summary$SpawnBio[-nrow(ssp_summary$SpawnBio),c(1,mod_indices,ncol(ssp_summary$SpawnBio))],value.name="Scale")
    Bratio<-melt(id.vars=c("Yr"),ssp_summary$Bratio[-nrow(ssp_summary$Bratio),c(1,mod_indices,ncol(ssp_summary$Bratio))],value.name="Status")
    #Catches<-Catches[Catches$Model%in%c("Status40%",ssp.folder.names[SSP_choices%in%input$myPicker_SSP]),]
    Catches<-as.data.frame(Catches)
    Catches<-Catches[Catches$Model%in%c("Status 40%",input$myPicker_SSP_grouped),]
    Catches$Model<-c("Status40%",ssp.folder.names[SSP_choices%in%input$myPicker_SSP_grouped])
      #try(SSplotComparisons(ssp_summary, subplots=c(1,3),legendlabels = c("Status40%",input$myPicker_SSP),endyrvec=2020, ylimAdj = 1.30, new = FALSE,plot=FALSE,print=TRUE, legendloc = 'topleft',uncertainty=TRUE,plotdir=paste0(Dir_SSP,"/Comparisons"),btarg=0.4,minbthresh=0.25))

    #Pull future catch values
    OFL<-ssp_summary$quants[ssp_summary$quants$Label=="OFLCatch_2021",c(-(ncol(ssp_summary$quants)-1),-ncol(ssp_summary$quants))]
    colnames(OFL)<-c("Status 40%",SSP_choices)
    OFL<-OFL[c(1,mod_indices)]
    OFL.rel<-(OFL-as.numeric(OFL[1]))/as.numeric(OFL[1])
    Forecatch<-ssp_summary$quants[ssp_summary$quants$Label=="ForeCatch_2021",c(-(ncol(ssp_summary$quants)-1),-ncol(ssp_summary$quants))]
    colnames(Forecatch)<-c("Status 40%",SSP_choices)
    Forecatch<-Forecatch[c(1,mod_indices)]
    Forecatch.rel<-(Forecatch-as.numeric(Forecatch[1]))/as.numeric(Forecatch[1])
    MSY<-ssp_summary$quants[ssp_summary$quants$Label=="Dead_Catch_MSY",c(-(ncol(ssp_summary$quants)-1),-ncol(ssp_summary$quants))]
    colnames(MSY)<-c("Status 40%",SSP_choices)
    MSY<-MSY[c(1,mod_indices)]
    MSY.rel<-(MSY-as.numeric(MSY[1]))/as.numeric(MSY[1])
    Proj.rel<-rbind(MSY.rel,OFL.rel,Forecatch.rel)
    Proj.rel$metric<-c("MSY","OFL","ABC")
    Proj.rel<-Proj.rel[,-1]
    Proj.rel.gg<-melt(Proj.rel)
    Proj.rel.gg$metric<-factor(Proj.rel.gg$metric,levels=c("MSY","OFL","ABC"))

    #Create plots
    output$Catches <- renderPlotly({
      p_catch<-ggplot(Catches,aes(Yr,dead_bio,color=Model))+
        geom_line()+
        xlab("Year")+
        ylab("Remvoals (in biomass)")+
        ylim(0,NA)+
        scale_color_viridis_d()+
        theme_bw()+
        labs(color = "Models")+
        ggtitle("Removals (Scale measure)")+
        theme(plot.title = element_text(size = 40, face = "bold"))
      
      ggplotly(p_catch)
    })
    
    output$Scale <- renderPlotly({
      p_scale<-ggplot(SpawnOutput,aes(Yr,Scale,color=variable))+
        geom_line(lwd=1.25)+
        xlab("Year")+
        ylab("Scale (Spawning Output)")+
        ylim(0,NA)+
        scale_color_viridis_d()+
        theme_bw()+
        labs(color = "Models")+
        ggtitle("Scale")+
        theme(plot.title = element_text(size = 40, face = "bold"))
      
      ggplotly(p_scale)
    })
    
    output$Status <- renderPlotly({
      p_status<-ggplot(Bratio,aes(Yr,Status,color=variable))+
        geom_line(lwd=1.25)+
        xlab("Year")+
        ylab("Status (Size relative to unfished)")+
        ylim(0,NA)+
        scale_color_viridis_d()+
        theme_bw()+
        labs(color = "Models")+
        ggtitle("Status")+
        theme(plot.title = element_text(size = 40, face = "bold"))
      
      ggplotly(p_status)
    })

    output$Proj <- renderPlotly({
      p_proj<-ggplot(Proj.rel.gg,aes(variable,value*100,color=metric))+
        geom_point(aes(shape=metric))+
        xlab("Model")+
        ylab("% change relative to the 40% stock status model")+
        geom_hline(yintercept=0)+
        scale_color_viridis_d()+
        theme_bw()+
        labs(color = "Catch metric",shape="")+
        ggtitle("Projected catch")+
        theme(plot.title = element_text(size = 40, face = "bold"))+
        coord_flip()
      
      ggplotly(p_proj)
    })
    

    #     output$SSP_SSBcomp_plot <- renderImage({
#     image.path<-normalizePath(file.path(paste0(Dir_SSP,"/Comparisons/compare1_spawnbio.png")),mustWork=FALSE)
#     return(list(
#       src = image.path,
#       contentType = "image/png",
#       width = 400,
#       height = 600,
#       style='height:60vh'))
#   },deleteFile=FALSE)
#  
#   output$SSP_relSSBcomp_plot <- renderImage({
#     image.path<-normalizePath(file.path(paste0(Dir_SSP,"/Comparisons/compare3_Bratio.png")),mustWork=FALSE)
#     return(list(
#       src = image.path,
#       contentType = "image/png",
#       width = 400,
#       height = 600,
#       style='height:60vh'))
#   },deleteFile=FALSE)
 })

  }
