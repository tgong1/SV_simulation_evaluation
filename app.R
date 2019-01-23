library(shiny)
library(shinyjs)
# Source helpers ----
source("helpers.R")

ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("Evaluation of Structural Variation (SV) callers"),
  
  sidebarLayout(
    sidebarPanel(conditionalPanel(condition="input.tabselected==1", 
                                  checkboxGroupInput(inputId = "SVCaller1",
                                                     label=("Choose a SV caller"),
                                                     choices = c("Manta","Lumpy","GRIDSS","BreakDancer","CNVKit","Pindel","SvABA"),
                                                     selected = "Manta"),
                                  radioButtons(inputId = "Tumor_coverage1",
                                               label=("Choose depth of coverage for tumor sample"),
                                               choices = c("60x","30x"),
                                               selected = "60x"),
                                  radioButtons(inputId = "Normal_coverage1",
                                               label=("Choose depth of coverage for normal sample"),
                                               choices = c("60x","30x","15x"),
                                               selected = "60x"),
                                  radioButtons(inputId = "VAF1",
                                               label=("Choose tumor purity"),
                                               choices = c("100%","50%","20%","10%"),
                                               selected = "100%"),
                                  radioButtons(inputId = "bkpt_diff",
                                               label=("Choose the breakpoint resolution"),
                                               choices = c("5 bp","50 bp","100 bp","200 bp"),
                                               selected = "5 bp")),
                 conditionalPanel(condition="input.tabselected==2",
                                  checkboxGroupInput(inputId = "SVCaller2",
                                                     label=("Choose a SV caller"),
                                                     choices = c("Manta","Lumpy","GRIDSS","BreakDancer","CNVKit","Pindel","SvABA"),
                                                     selected = "Manta"),
                                  numericInput(inputId = "Tumor_coverage2", label = ("Type in depth of coverage for tumor sample"), value = 60 ,min = 10, max = 200,step=0.01),
                                  numericInput(inputId = "Normal_coverage2", label = ("Type in depth of coverage for normal sample"), value = 60 ,min = 10, max = 200,step=0.01),
                                  numericInput(inputId = "VAF2", label = ("Type in estimated tumor purity (%)"), value = 50 ,min = 10, max = 100,step=0.1,width='100%')),
                 conditionalPanel(condition="input.tabselected==3",
                                  fileInput("file",label = "Please input the bed file"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Existing evaluation",value = 1, 
               checkboxGroupInput("measurements1",
                                  label="Please choose the evaluation measurements",
                                  choices = list("Sensitivity","Precision"),
                                  selected = "Sensitivity",inline=TRUE),
               radioButtons("type1",
                            label="Evaluation across",
                            choices = list("SV type","SV length"),
                            selected = "SV type",inline=TRUE),
               plotOutput("Sensitivity_plot1"),
               plotOutput("Precision_plot1")),
        tabPanel("Predicted evaluation",value = 2, 
                 checkboxGroupInput("measurements2",
                                    label="Please choose the evaluation measurements",
                                    choices = list("Sensitivity","Precision"),
                                    selected = "Sensitivity",inline=TRUE),
                 radioButtons("type2",
                              label="Evaluation across",
                              choices = list("SV type","SV length"),
                              selected = "SV type",inline=TRUE),
                 plotOutput("Sensitivity_plot2"),
                 plotOutput("Precision_plot2")),
        tabPanel("New SV caller evaluation",value = 3, 
                 plotOutput("plot3")),
        id = "tabselected")
      )
  )
)

server <- function(input,output)({
  observe({
    if(!("60x" %in% input$Normal_coverage1)){
      shinyjs::disable(selector = paste("#SVCaller1 input[value=",c("BreakDancer","CNVKit","Pindel","SvABA"),"]"))
    }else{
      shinyjs::enable(id="SVCaller1")
    }
  })
  observe({
    shinyjs::disable(selector = paste("#SVCaller2 input[value=",c("BreakDancer","CNVKit","Pindel","SvABA"),"]"))
  })
  ### get data
  dataInput <- reactive({
    if(length(input$measurements1)==0){
      return(c(0,0))
    }else if(length(input$measurements1)==2){
      return(input$measurements1)
    }else if(input$measurements1 == "Sensitivity"){
      return(c("Sensitivity",0))
    }else if(input$measurements1 == "Precision"){
      return(c(0,"Precision"))
    }
  })
  
  typeInput <- reactive({switch(input$type1, "SV type" = "SVTYPE", "SV length" = "SVLEN") })
  VAFInput <- reactive({switch(input$VAF1, "100%" = "", "50%" = "_0.5", "20%" = "_0.2", "10%" = "_0.1") })
  
  
  #data <- reactive({return(input$SVCaller)})
  #output$Manta <- renderPrint(paste(data(),"_TP_Sensitivity_T"))
  
  #output$'Manta' <- renderPrint({
  #  paste("VAF value",input$VAF,"changed to",VAFInput())
  #})
  #output$'60x' <- renderPrint({
  #  paste("VAF value",input$VAF,"changed to",VAFInput(),"coverage",input$Tumor_coverage)
  #})
  
  output$Sensitivity_plot1 <- renderPlot({
    if(dataInput()[1] == "Sensitivity"){
      data <- eval(parse(text=paste0(typeInput(),"_TP_Sensitivity_T",input$Tumor_coverage1,"_N",input$Normal_coverage1,VAFInput())))
      Sensitivity_plot(data,caller=input$SVCaller1,typeInput())
    }
    
  })
  output$Precision_plot1 <- renderPlot({
    if(dataInput()[2] == "Precision"){
      data <- eval(parse(text=paste0(typeInput(),"_TP_Precision_T",input$Tumor_coverage1,"_N",input$Normal_coverage1,VAFInput())))
      Precision_plot(data,caller=input$SVCaller1,typeInput())
    }
  })
  
  
})
shinyApp(ui=ui,server=server)
