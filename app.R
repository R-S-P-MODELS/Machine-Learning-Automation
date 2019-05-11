#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
require(caret)
require(data.table)
require(plotly)
options(warn=-1)
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Graphical Machine Learning"),

   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        fileInput(inputId = "Arquivo",label = "Selecione um arquivo de entrada",multiple = FALSE)
         
      ),
      conditionalPanel(
        condition = "is.null(input.Arquivo) == true",
      #  checkboxInput("histogram","Show histogram of x")),
      selectInput(inputId = "Areas",label = "Selecione tipo de classificação",choices = c('Classificação','Regressão')),
      uiOutput("Eixox"),
      uiOutput("Remover"),
      uiOutput("EscolherModelo"),
      #uiOutput("cores"),
      #uiOutput("tamanhos"),
      actionButton("Treinamento","Inicializar treinamento"),
      downloadButton("downloadData", "Download do modelo")
   )
   ), 
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("distPlot")
      )
   )


# Define server logic required to draw a histogram
server <- function(input, output) {
  options(shiny.maxRequestSize=1000000*1024^2)
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("ModeloFinal", ".Rdata", sep = "")
    },
    content = function(file) {
      a=load("ModeloFinal.Rdata")
      
      save(ModeloFinal, file=file)
    }
  )
  
  LeituraArquivo<-reactive(
    {
      if(!is.null(input$Arquivo)){
        #funcoes_reativas()
        #w=fread(input$Arquivo$datapath,header=TRUE)
        w=read.csv(input$Arquivo$datapath,header=TRUE)
        return(w)
      }
      
      
    }
    
  )
  
  output$Eixox = renderUI({
    if(!is.null(input$Arquivo)){
      w=LeituraArquivo()
      #w=read.csv(input$file1$datapath,header=TRUE)
    #  if(input$Referenciador=="PCA Visualization"){
     #   step=which(lapply(w,class) %in% c("numeric","integer"))
    #    selectInput("X", label = "X",choices = names(w[,..step]))
        #selectInput("X", label = "X",choices = names(w[,which(lapply(w,class) %in% c("numeric","integer"))]))
    #  }
     # else{
      if(input$Areas=="Regressão")
        Rejeita=as.numeric( which(sapply(w,class) %in% c('character','factor') ) )
      if(input$Areas=="Classificação")
        Rejeita=as.numeric( which(sapply(w,class) %in% c('numeric','integer') ) )
      
        selectInput("X", label = "Variavel a ser predita",choices = names(w)[-Rejeita])
    #  }
      
      
      
    }
  })
  
  output$Remover = renderUI({
    if(!is.null(input$Arquivo)){
      w=LeituraArquivo()
      #w=read.csv(input$file1$datapath,header=TRUE)
      #  if(input$Referenciador=="PCA Visualization"){
      #   step=which(lapply(w,class) %in% c("numeric","integer"))
      #    selectInput("X", label = "X",choices = names(w[,..step]))
      #selectInput("X", label = "X",choices = names(w[,which(lapply(w,class) %in% c("numeric","integer"))]))
      #  }
      # else{
      selectInput("REM", label = "Elementos não usados no processo de machine learning",choices = names(w),multiple = TRUE)
      #  }
      
      
      
    }
  })
  
  output$EscolherModelo = renderUI({
    if(!is.null(input$Arquivo)){
    if(input$Areas=="Classificação")
        escolhas=c('Bayes','KNN','Logistic Regression','Decision Tree','Random Forest','SVM')
    if(input$Areas=="Regressão")
      escolhas=c('Linear Regression','Regression Tree','Random Forest')
    
      selectInput(inputId = "Modelo",label = "Tipo de modelo a prever variavel x",choices = escolhas)
    }
  })
  
  TrainModel<- eventReactive(input$Treinamento,{
    if(!is.null(input$Arquivo)){
     Objetivo <- input$X 
     trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
     Dataset=LeituraArquivo()
     print(length(input$REM))
     if(length(input$REM)>0){
      Removiveis=which(names(Dataset) %in% input$REM)
      Dataset=Dataset[-Removiveis,]
     }  
     Index=sample(1:nrow(Dataset),0.7*nrow(Dataset))
     Training=Dataset[Index,]
     Validation=Dataset[-Index,]
     if(input$Modelo=='Bayes')
       MetodoUtilizado='nb'
     if(input$Modelo=='KNN')
       MetodoUtilizado='knn'
     if(input$Modelo=='Logistic Regression')
       MetodoUtilizado='glm'
     if(input$Modelo=='Decision Tree')
       MetodoUtilizado='rpart'
     if(input$Modelo=='Regression Tree')
       MetodoUtilizado='rpart'
     if(input$Modelo=='Random Forest')
       MetodoUtilizado='rf'
     if(input$Modelo=='SVM')
       MetodoUtilizado='svmLinear'
     if(input$Modelo=='Linear Regression')
       MetodoUtilizado='lm'
     print(dim(Training))
     print(Objetivo)
     Indice=which(Objetivo==names(Training))
     print(Indice)
     #vecob=names(Training)[Indice]
     #vectest=names(Training)[-Indice]
     showModal(modalDialog("The code is currently running please wait", footer=NULL))
     
     ModeloFinal <- train(as.formula(paste(input$X, "~ .")),data=Training,method=MetodoUtilizado,trControl=trctrl,tuneLength = 10)
     #print("huebr")
     removeModal()
     
     #print(ModeloFinal)
     return(ModeloFinal)
    }  
  })
  
   output$distPlot <- renderPlotly({
     if(!is.null(input$Arquivo)){
       require(plotly)
       ModeloFinal=TrainModel()
       if(input$Areas=="Classificação"){
         Metricas=ModeloFinal$results$Accuracy
         Nome= ModeloFinal$method
 
       }
       if(input$Areas=='Regressão'){
         Metricas=ModeloFinal$results$RMSE
         Nome=ModeloFinal$method
         
       }
         
       save(ModeloFinal,file = "ModeloFinal.Rdata")
       plotter=data.frame(1:length(Metricas),Metricas)
       if(input$Areas=="Classificação")
        names(plotter)=c('Model','Accuracy')
       if(input$Areas=="Regressão")
         names(plotter)=c('Model','RMSE')
       ggplotly(ggplot(data=plotter,aes(x=Model,y=plotter[,2])) + geom_point() + labs(x=names(plotter)[1],y=names(plotter)[2],title = input$Modelo )   )
       
     }
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

