library(shiny)
library(shinyjs)
library(gridExtra)
# Source helpers ----
source("helpers.R")

ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("Evaluation of Structural Variation (SV) callers"),
  sidebarLayout(
    sidebarPanel(conditionalPanel(condition="input.tabselected==2",
                                  checkboxGroupInput(inputId = "SVCaller2",
                                                     label=("Choose a SV caller"),
                                                     choices = c("Manta","Lumpy","GRIDSS","BreakDancer","CNVKit","Pindel","SvABA"),
                                                     selected = "Manta"),
                                  #uiOutput(outputId = "dynamicVAF2"),
                                  #uiOutput(outputId = "dynamicT_coverage2"),
                                  checkboxGroupInput("VAF2",
                                                     label="Tumor purity",
                                                     choices = c("100%"=1,"80%"=0.8,"50%"=0.5,"20%"=0.2,"10%"=0.1,"5%"=0.05),
                                                     selected = 1),
                                  checkboxGroupInput("T_coverage2",
                                                     label="Tumor coverage",
                                                     choices = c("20x","30x","45x","60x","75x","90x"),
                                                     selected = "60x"),
                                  checkboxGroupInput("N_coverage2",
                                                     label="Normal coverage",
                                                     choices = c("15x","30x","45x","60x","75x","90x"),
                                                     selected = "30x"),
                                  radioButtons("BND_threshold2",
                                               label="BND threshold",
                                               choices = c("2bp","5bp","50bp","150bp","200bp"),
                                               selected = "5bp"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Existing evaluation2",value = 2,
                 checkboxGroupInput("measurements2",
                                    label="Please choose the evaluation measurements",
                                    choices = list("Sensitivity","Precision"),
                                    selected = "Sensitivity",inline=TRUE),
                 radioButtons("X_axis2",
                              label="Evaluation across",
                              choices = c("Tumor Purity" = "VAF","Tumor coverage" = "T_coverage","Normal coverage" = "N_coverage","Breakpoint resolution" = "BND_threshold"),
                              selected = "VAF",inline=TRUE),
                 textOutput("txtOutput2"),
                 plotOutput("Plot2")),
        id = "tabselected"
      )
    )
  )
)

server <- function(input,output,session)({
  MeasureInput2 <- reactive({c("Sensitivity","Precision") %in% input$measurements2
  })
  output$txtOutput2 <- renderText({
    paste("Show the",input$measurements2,MeasureInput2()[1],
          "of",input$SVCaller2,
          'across',input$X_axis2,", with",
          " VAF=",input$VAF2,
          
          " Tumor coverage =",input$T_coverage2,
          " Normal coverage=",input$N_coverage2,
          " breakend resolution=",input$BND_threshold2)
  })

  Sensitivity_plot2 <- reactive({
    if(!MeasureInput2()[1])return(NULL)
    measurement <- "Sensitivity"
    
    if(input$X_axis2 == "VAF"){
      df <- df_sensitivity[df_sensitivity$Measurement == measurement & 
                             df_sensitivity$SVCaller %in% input$SVCaller2 &
                             df_sensitivity$BND_threshold == input$BND_threshold2 &
                             df_sensitivity$T_coverage %in% input$T_coverage2 &
                             df_sensitivity$N_coverage %in% input$N_coverage2,]
      line_type <- c("solid","longdash","twodash","dashed","dotdash","dotted")
      names(line_type) <- c("90x","75x","60x","45x","30x","20x")
      point_shape <- c(15,16,17,0,1,2,3)
      names(point_shape) <- c("120x","90x","75x","60x","45x","30x","15x")
      ggplot(data=df, aes(x=VAF, y=SUM, colour=SVCaller, group = interaction(SVCaller,T_coverage,N_coverage))) +
        geom_point(aes(shape=N_coverage),size=3)+
        geom_line(aes(linetype=T_coverage))+
        scale_y_continuous(breaks = seq(0, 1, by = 0.2),limits = c(0,1))+
        scale_color_manual(values=color_SVCaller[names(color_SVCaller) %in% df$SVCaller])+
        scale_linetype_manual(values=line_type[names(line_type) %in% df$T_coverage])+
        scale_shape_manual(values=point_shape[names(point_shape) %in% df$N_coverage])+
        labs(title = paste("SUM",measurement,"v.s.",input$X_axis2),x=input$X_axis2, y = measurement)+
        theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"),plot.title = element_text(size=18),
              legend.title=element_text(size=14),legend.text=element_text(size=14))
    }else if(input$X_axis2 == "T_coverage"){
      df <- df_sensitivity[df_sensitivity$Measurement == measurement & 
                             df_sensitivity$SVCaller %in% input$SVCaller2 &
                             df_sensitivity$BND_threshold == input$BND_threshold2 &
                             df_sensitivity$N_coverage %in% input$N_coverage2 &
                             df_sensitivity$VAF %in% input$VAF2,]
      line_type <- c("solid","longdash","twodash","dashed","dotdash","dotted")
      names(line_type) <- c(1,0.8,0.5,0.2,0.1,0.05)
      point_shape <- c(15,16,17,0,1,2,3)
      names(point_shape) <- c("120x","90x","75x","60x","45x","30x","15x")
      
      ggplot(data=df, aes(x=T_coverage, y=SUM, colour=SVCaller, group = interaction(SVCaller,VAF,N_coverage))) +
        geom_point(aes(shape=N_coverage),size=3)+
        geom_line(aes(linetype=VAF))+
        scale_y_continuous(breaks = seq(0, 1, by = 0.2),limits = c(0,1))+
        scale_color_manual(values=color_SVCaller[names(color_SVCaller) %in% df$SVCaller])+
        scale_linetype_manual(values=line_type[names(line_type) %in% df$VAF])+
        scale_shape_manual(values=point_shape[names(point_shape) %in% df$N_coverage])+
        labs(title = paste("SUM",measurement,"v.s.",input$X_axis2),x=input$X_axis2, y = measurement)+
        theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"),plot.title = element_text(size=18),
              legend.title=element_text(size=14),legend.text=element_text(size=14))
      
    }else if(input$X_axis2 == "N_coverage"){
      df <- df_sensitivity[df_sensitivity$Measurement == measurement & 
                             df_sensitivity$SVCaller %in% input$SVCaller2 &
                             df_sensitivity$BND_threshold == input$BND_threshold2 &
                             df_sensitivity$T_coverage %in% input$T_coverage2 &
                             df_sensitivity$VAF %in% input$VAF2,]
      line_type <- c("solid","longdash","twodash","dashed","dotdash","dotted")
      names(line_type) <- c(1,0.8,0.5,0.2,0.1,0.05)
      point_shape <- c(15,16,17,0,1,2)
      names(point_shape) <- c("90x","75x","60x","45x","30x","20x")
      
      ggplot(data=df, aes(x=N_coverage, y=SUM, colour=SVCaller, group = interaction(SVCaller,VAF,T_coverage))) +
        geom_point(aes(shape=T_coverage),size=3)+
        geom_line(aes(linetype=VAF))+
        scale_y_continuous(breaks = seq(0, 1, by = 0.2),limits = c(0,1))+
        scale_color_manual(values=color_SVCaller[names(color_SVCaller) %in% df$SVCaller])+
        scale_linetype_manual(values=line_type[names(line_type) %in% df$VAF])+
        scale_shape_manual(values=point_shape[names(point_shape) %in% df$T_coverage])+
        labs(title = paste("SUM",measurement,"v.s.",input$X_axis2),x=input$X_axis2, y = measurement)+
        theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"),plot.title = element_text(size=18),
              legend.title=element_text(size=14),legend.text=element_text(size=14))
    }else if(input$X_axis2 == "BND_threshold"){
      df <- df_sensitivity[df_sensitivity$Measurement == measurement & 
                             df_sensitivity$SVCaller %in% input$SVCaller2 &
                             df_sensitivity$T_coverage %in% input$T_coverage2 &
                             df_sensitivity$N_coverage %in% input$N_coverage2 &
                             df_sensitivity$VAF %in% input$VAF2,]
      line_type <- c("solid","longdash","twodash","dashed","dotdash","dotted")
      names(line_type) <- c(1,0.8,0.5,0.2,0.1,0.05)
      point_shape <- c(15,16,17,0,1,2)
      names(point_shape) <- c("90x","75x","60x","45x","30x","20x")
      
      ggplot(data=df, aes(x=BND_threshold, y=SUM, colour=SVCaller, group = interaction(SVCaller,VAF,T_coverage))) +
        geom_point(aes(shape=T_coverage),size=3)+
        geom_line(aes(linetype=VAF))+
        scale_y_continuous(breaks = seq(0, 1, by = 0.2),limits = c(0,1))+
        scale_color_manual(values=color_SVCaller[names(color_SVCaller) %in% df$SVCaller])+
        scale_linetype_manual(values=line_type[names(line_type) %in% df$VAF])+
        scale_shape_manual(values=point_shape[names(point_shape) %in% df$T_coverage])+
        labs(title = paste("SUM",measurement,"v.s.",input$X_axis2),x=input$X_axis2, y = measurement)+
        theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"),plot.title = element_text(size=18),
              legend.title=element_text(size=14),legend.text=element_text(size=14))
    }
  })
  Precision_plot2 <- reactive({
    if(!MeasureInput2()[2])return(NULL)
    measurement <- "Precision"

    if(input$X_axis2 == "VAF"){
      df <- df_precision[df_precision$Measurement == measurement & 
                             df_precision$SVCaller %in% input$SVCaller2 &
                             df_precision$BND_threshold == input$BND_threshold2 &
                             df_precision$T_coverage %in% input$T_coverage2 &
                             df_precision$N_coverage %in% input$N_coverage2,]
      line_type <- c("solid","longdash","twodash","dashed","dotdash","dotted")
      names(line_type) <- c("90x","75x","60x","45x","30x","20x")
      point_shape <- c(15,16,17,0,1,2,3)
      names(point_shape) <- c("120x","90x","75x","60x","45x","30x","15x")
      ggplot(data=df, aes(x=VAF, y=SUM, colour=SVCaller, group = interaction(SVCaller,T_coverage,N_coverage))) +
        geom_point(aes(shape=N_coverage),size=3)+
        geom_line(aes(linetype=T_coverage))+
        scale_y_continuous(breaks = seq(0, 1, by = 0.2),limits = c(0,1))+
        scale_color_manual(values=color_SVCaller[names(color_SVCaller) %in% df$SVCaller])+
        scale_linetype_manual(values=line_type[names(line_type) %in% df$T_coverage])+
        scale_shape_manual(values=point_shape[names(point_shape) %in% df$N_coverage])+
        labs(title = paste("SUM",measurement,"v.s.",input$X_axis2),x=input$X_axis2, y = measurement)+
        theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"),plot.title = element_text(size=18),
              legend.title=element_text(size=14),legend.text=element_text(size=14))
    }else if(input$X_axis2 == "T_coverage"){
      df <- df_precision[df_precision$Measurement == measurement & 
                             df_precision$SVCaller %in% input$SVCaller2 &
                             df_precision$BND_threshold == input$BND_threshold2 &
                             df_precision$N_coverage %in% input$N_coverage2 &
                             df_precision$VAF %in% input$VAF2,]
      line_type <- c("solid","longdash","twodash","dashed","dotdash","dotted")
      names(line_type) <- c(1,0.8,0.5,0.2,0.1,0.05)
      point_shape <- c(15,16,17,0,1,2,3)
      names(point_shape) <- c("120x","90x","75x","60x","45x","30x","15x")
      
      ggplot(data=df, aes(x=T_coverage, y=SUM, colour=SVCaller, group = interaction(SVCaller,VAF,N_coverage))) +
        geom_point(aes(shape=N_coverage),size=3)+
        geom_line(aes(linetype=VAF))+
        scale_y_continuous(breaks = seq(0, 1, by = 0.2),limits = c(0,1))+
        scale_color_manual(values=color_SVCaller[names(color_SVCaller) %in% df$SVCaller])+
        scale_linetype_manual(values=line_type[names(line_type) %in% df$VAF])+
        scale_shape_manual(values=point_shape[names(point_shape) %in% df$N_coverage])+
        labs(title = paste("SUM",measurement,"v.s.",input$X_axis2),x=input$X_axis2, y = measurement)+
        theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"),plot.title = element_text(size=18),
              legend.title=element_text(size=14),legend.text=element_text(size=14))
      
    }else if(input$X_axis2 == "N_coverage"){
      df <- df_precision[df_precision$Measurement == measurement & 
                             df_precision$SVCaller %in% input$SVCaller2 &
                             df_precision$BND_threshold == input$BND_threshold2 &
                             df_precision$T_coverage %in% input$T_coverage2 &
                             df_precision$VAF %in% input$VAF2,]
      line_type <- c("solid","longdash","twodash","dashed","dotdash","dotted")
      names(line_type) <- c(1,0.8,0.5,0.2,0.1,0.05)
      point_shape <- c(15,16,17,0,1,2)
      names(point_shape) <- c("90x","75x","60x","45x","30x","20x")
      
      ggplot(data=df, aes(x=N_coverage, y=SUM, colour=SVCaller, group = interaction(SVCaller,VAF,T_coverage))) +
        geom_point(aes(shape=T_coverage),size=3)+
        geom_line(aes(linetype=VAF))+
        scale_y_continuous(breaks = seq(0, 1, by = 0.2),limits = c(0,1))+
        scale_color_manual(values=color_SVCaller[names(color_SVCaller) %in% df$SVCaller])+
        scale_linetype_manual(values=line_type[names(line_type) %in% df$VAF])+
        scale_shape_manual(values=point_shape[names(point_shape) %in% df$T_coverage])+
        labs(title = paste("SUM",measurement,"v.s.",input$X_axis2),x=input$X_axis2, y = measurement)+
        theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"),plot.title = element_text(size=18),
              legend.title=element_text(size=14),legend.text=element_text(size=14))
    }else if(input$X_axis2 == "BND_threshold"){
      df <- df_precision[df_precision$Measurement == measurement & 
                             df_precision$SVCaller %in% input$SVCaller2 &
                             df_precision$T_coverage %in% input$T_coverage2 &
                             df_precision$N_coverage %in% input$N_coverage2 &
                             df_precision$VAF %in% input$VAF2,]
      line_type <- c("solid","longdash","twodash","dashed","dotdash","dotted")
      names(line_type) <- c(1,0.8,0.5,0.2,0.1,0.05)
      point_shape <- c(15,16,17,0,1,2)
      names(point_shape) <- c("90x","75x","60x","45x","30x","20x")
      
      ggplot(data=df, aes(x=BND_threshold, y=SUM, colour=SVCaller, group = interaction(SVCaller,VAF,T_coverage))) +
        geom_point(aes(shape=T_coverage),size=3)+
        geom_line(aes(linetype=VAF))+
        scale_y_continuous(breaks = seq(0, 1, by = 0.2),limits = c(0,1))+
        scale_color_manual(values=color_SVCaller[names(color_SVCaller) %in% df$SVCaller])+
        scale_linetype_manual(values=line_type[names(line_type) %in% df$VAF])+
        scale_shape_manual(values=point_shape[names(point_shape) %in% df$T_coverage])+
        labs(title = paste("SUM",measurement,"v.s.",input$X_axis2),x=input$X_axis2, y = measurement)+
        theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"),plot.title = element_text(size=18),
              legend.title=element_text(size=14),legend.text=element_text(size=14))
    }
  })
  output$Plot2 <- renderPlot({
    ptlist <- list(Sensitivity_plot2(),Precision_plot2())
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete]
    
    if (length(ptlist)==0) return(NULL)
    #grid.arrange(grobs=ptlist)
    grid.arrange(grobs=ptlist,ncol=length(ptlist))
  })
  
  
})
shinyApp(ui=ui,server=server)

