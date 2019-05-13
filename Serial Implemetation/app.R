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
        fileInput(inputId = "Arquivo",label = "Selecione um arquivo de entrada",multiple = FALSE),
         
      
      conditionalPanel(
        condition="output.csv_import_ready",
      #  checkboxInput("histogram","Show histogram of x")),
      selectInput(inputId = "Areas",label = "Selecione tipo de classificação",choices = c('Classificação','Regressão')),
      uiOutput("Eixox"),
      uiOutput("Remover"),
      
      uiOutput("EscolherModelo"),
      numericInput("NumeroDeTestes","Quantos testes serão feitos sobre este modelo?",min = 1,max=100,value = 1),
      #uiOutput("cores"),
      #uiOutput("tamanhos"),
      actionButton("Treinamento","Inicializar treinamento"),
      downloadButton("downloadData", "Download do modelo")
   )),
    
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(id="Principal",
         tabPanel(id="SelecioneModelo",title = "Selecione seu Modelo",plotlyOutput("distPlot")),
         tabPanel(id="ComparacaoSupervisionada",title="Comparacao Supervisionada",tableOutput("Supervised"))
        )
      ))
   )


# Define server logic required to draw a histogram
server <- function(input, output) {
  options(shiny.maxRequestSize=1000000*1024^2)
  
  output$csv_import_ready <- reactive({
    return(!is.null(input$Arquivo))
  })
  
  outputOptions(output, "csv_import_ready", suspendWhenHidden = FALSE)
  
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
        w=fread(input$Arquivo$datapath,header=TRUE)
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
      print(input$Principal)
      if(input$Principal=="Selecione seu Modelo"){
    if(input$Areas=="Classificação")
        escolhas=c('Bayes','KNN','Logistic Regression','Decision Tree','Random Forest','SVM')
    if(input$Areas=="Regressão")
      escolhas=c('Linear Regression','Regression Tree','Random Forest')
    
      selectInput(inputId = "Modelo",label = "Tipo de modelo a prever variavel x",choices = escolhas)
    } }
  })
  
  TrainEnsemble<-eventReactive(input$Treinamento,{
    if(!is.null(input$Arquivo)){
     # print(input$NumeroDeTestes)
      Ensemble=list()
    for(i in 1:input$NumeroDeTestes){
      ModeloFinal=TrainModel(input$Modelo)
      Ensemble[[i]]=ModeloFinal
    }
    return(Ensemble)
    }
  })
  
  SupervisedEnsemble<-eventReactive(input$Treinamento,{
    if(!is.null(input$Arquivo)){
      # print(input$NumeroDeTestes)
      if(input$Areas=="Classificação"){
       EnsembleBayes=list()
       AcuraciaBayes=c()
       for(i in 1:input$NumeroDeTestes){
        ModeloFinal=TrainModel('Bayes')
        EnsembleBayes[[i]]=ModeloFinal
        AcuraciaBayes[i]=max(ModeloFinal$results$Accuracy)
       }
       EnsembleKNN=list()
       AcuraciaKnn=c()
       for(i in 1:input$NumeroDeTestes){
         ModeloFinal=TrainModel('KNN')
         EnsembleKNN[[i]]=ModeloFinal
         AcuraciaKnn[i]=max(ModeloFinal$results$Accuracy)
       }
       EnsembleLogistic=list()
       AcuraciaLogistic=c()
       for(i in 1:input$NumeroDeTestes){
         ModeloFinal=TrainModel('Logistic Regression')
         EnsembleLogistic[[i]]=ModeloFinal
         AcuraciaLogistic[i]=max(ModeloFinal$results$Accuracy)
       }
       EnsembleTree=list()
       AcuraciaTree=c()
       for(i in 1:input$NumeroDeTestes){
         ModeloFinal=TrainModel('Decision Tree')
         EnsembleTree[[i]]=ModeloFinal
         AcuraciaTree[i]=max(ModeloFinal$results$Accuracy)
       }
       EnsembleSVM=list()
       AcuraciaSVM=c()
       for(i in 1:input$NumeroDeTestes){
         ModeloFinal=TrainModel('SVM')
         EnsembleSVM[[i]]=ModeloFinal
         AcuraciaSVM[i]=max(ModeloFinal$results$Accuracy)
       }
       EnsembleForest=list()
       AcuraciaForest=c()
       for(i in 1:input$NumeroDeTestes){
         ModeloFinal=TrainModel('Random Forest')
         EnsembleForest[[i]]=ModeloFinal
         AcuraciaForest[i]=max(ModeloFinal$results$Accuracy)
       }
       
       Modelos=data.frame(AcuraciaBayes,AcuraciaKnn,AcuraciaLogistic,AcuraciaSVM,AcuraciaTree,AcuraciaForest)
       names(Modelos)=c('Bayes','KNN','Logistic Regression','SVM','Decision Tree','Random Forest')
       return(Modelos)
       #return(Ensemble)
      }
      
      if(input$Areas=="Regressão"){
        Ensemblelm=list()
        Acuracialm=c()
        for(i in 1:input$NumeroDeTestes){
          ModeloFinal=TrainModel('Linear Regression')
          Ensemblelm[[i]]=ModeloFinal
          Acuracialm[i]=min(ModeloFinal$results$RMSE)
        }
        
        
        EnsembleTree=list()
        AcuraciaTree=c()
        for(i in 1:input$NumeroDeTestes){
          ModeloFinal=TrainModel('Regression Tree')
          EnsembleTree[[i]]=ModeloFinal
          AcuraciaTree[i]=min(ModeloFinal$results$RMSE)
        }
        
        EnsembleForest=list()
        AcuraciaForest=c()
        for(i in 1:input$NumeroDeTestes){
          ModeloFinal=TrainModel('Random Forest')
          EnsembleForest[[i]]=ModeloFinal
          AcuraciaForest[i]=min(ModeloFinal$results$RMSE)
        }
        
        Modelos=data.frame(Acuracialm,AcuraciaTree,AcuraciaForest)
        names(Modelos)=c('Linear Regression','Regression Tree','Random Forest')
        return(Modelos)
        #return(Ensemble)
      }
    }
  })
  
  TrainModel<- function(ModeloEscolhido){
    if(!is.null(input$Arquivo)){
     Objetivo <- input$X 
     trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
     Dataset=LeituraArquivo()
    # print(length(input$REM))
     if(length(input$REM)>0){
      Removiveis=which(names(Dataset) %in% input$REM)
      Dataset=Dataset[-Removiveis,]
     }  
     Index=sample(1:nrow(Dataset),0.7*nrow(Dataset))
     Training=Dataset[Index,]
     Validation=Dataset[-Index,]
     if(ModeloEscolhido=='Bayes')
       MetodoUtilizado='nb'
     if(ModeloEscolhido=='KNN')
       MetodoUtilizado='knn'
     if(ModeloEscolhido=='Logistic Regression')
       MetodoUtilizado='glm'
     if(ModeloEscolhido=='Decision Tree')
       MetodoUtilizado='rpart'
     if(ModeloEscolhido=='Regression Tree')
       MetodoUtilizado='rpart'
     if(ModeloEscolhido=='Random Forest')
       MetodoUtilizado='rf'
     if(ModeloEscolhido=='SVM')
       MetodoUtilizado='svmLinear'
     if(ModeloEscolhido=='Linear Regression')
       MetodoUtilizado='lm'
   #  print(dim(Training))
   #  print(Objetivo)
     Indice=which(Objetivo==names(Training))
    # print(Indice)
     #vecob=names(Training)[Indice]
     #vectest=names(Training)[-Indice]
     showModal(modalDialog("The code is currently running please wait", footer=NULL))
     
     ModeloFinal <- train(as.formula(paste(input$X, "~ .")),data=Training,method=MetodoUtilizado,trControl=trctrl,tuneLength = 10)
     #print("huebr")
     removeModal()
     
     #print(ModeloFinal)
     return(ModeloFinal)
    }  
  }
  
   output$distPlot <- renderPlotly({
     if(!is.null(input$Arquivo)){
       require(plotly)
       Ensemble=list()
       Metricas=c()
       Ensemble=TrainEnsemble()
       ModeloFinal=Ensemble[[1]]
       if(input$Areas=="Classificação"){
         for(i in 1:input$NumeroDeTestes){
         #  print(max(Ensemble[[i]]$results$Accuracy))
          Metricas[i]=max(Ensemble[[i]]$results$Accuracy)
         # print(Ensemble[[i]])
         }
       #  print(Ensemble)

         Nome= ModeloFinal$method
         Indice=min(which(max(Metricas)==Metricas) )
       }
       if(input$Areas=='Regressão'){
         for(i in 1:input$NumeroDeTestes){
           
           Metricas[i]=min(Ensemble[[i]]$results$RMSE)
         }

         Nome=ModeloFinal$method
         Indice=min(which(min(Metricas)==Metricas) )
       }
        ModeloFinal=Ensemble[[Indice]] 
       save(ModeloFinal,file = "ModeloFinal.Rdata")
      # print(Metricas)
       plotter=data.frame(1:length(Metricas),Metricas)
       if(input$Areas=="Classificação")
        names(plotter)=c('Model','Accuracy')
       if(input$Areas=="Regressão")
         names(plotter)=c('Model','RMSE')
       ggplotly(ggplot(data=plotter,aes(x=Model,y=plotter[,2])) + geom_point() + labs(x=names(plotter)[1],y=names(plotter)[2],title = input$Modelo )   )
       
     }
     
   })
   
   output$Supervised <- renderTable({
     if(!is.null(input$Arquivo)){
       Tabela=SupervisedEnsemble()
       print(Tabela)
       for(i in 1:ncol(Tabela)){
         index=which(Tabela[,i]=='NaN')
         if(length(index)>0)
           Tabela[index,i]=NA
       }
       
       DF=Tabela[1,]
       
       for(i in 1:ncol(Tabela)){
         if(sum(is.na(Tabela[,i]))==ncol(Tabela)   ){
           
           DF[1,i]=NA
           DF[2,i]=NA
           DF[3,i]=NA
           DF[4,i]=NA
           DF[5,i]=NA 
         }
         else{
           print(c(i,Tabela[,i]))
           DF[1,i]=mean(Tabela[,i],na.rm=TRUE)
           DF[2,i]=median(Tabela[,i],na.rm=TRUE)
           if(sum(!is.na(Tabela[,i])) > 1)
            DF[3,i]=sd(Tabela[,i],na.rm=TRUE)
           else
             DF[3,i]=0
           DF[4,i]=min(Tabela[,i],na.rm=TRUE)
           DF[5,i]=max(Tabela[,i],na.rm=TRUE)
         }
       }
       vec=c('Media','Mediana','Desvio Padrão','Minimo','Maximo')
       names(DF)=names(Tabela)
       DF=data.frame(vec,DF)
       names(DF)[1]='Métrica'
       
       #rownames(DF)=c('Mean','Median','Standart Deviation','Minimum','Maximum')
       return(DF)
       #return(DF)
     }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

