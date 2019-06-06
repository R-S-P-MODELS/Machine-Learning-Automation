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
require(klaR)

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
      conditionalPanel(condition="input.Principal!='Aprendizado não supervisionado'",
      selectInput(inputId = "Areas",label = "Selecione tipo de classificação",choices = c('Classificação','Regressão')),
      uiOutput("Eixox"),
      uiOutput("Remover"),
      
      uiOutput("EscolherModelo"),
      numericInput("NumeroDeTestes","Quantos testes serão feitos sobre este modelo?",min = 1,max=100,value = 1),
      actionButton("Treinamento","Inicializar treinamento")
      
      ),
      conditionalPanel(condition="input.Principal=='Aprendizado não supervisionado'",
      selectInput(inputId="UnsupervisedChoice",label ="Metodo de aprendizado",choices=c('Kmeans','Kmodes') ),
      numericInput(inputId ="Clusters",label = "Numero de clusters",min = 1,max=10,value = 2),
      uiOutput("ClusterVariables"),
      actionButton("AtivarTreinamento","Inicializar Treinamento")
      ),
      #uiOutput("cores"),
      #uiOutput("tamanhos"),
   
      downloadButton("downloadData", "Download do modelo")
   )
  
   ),
    
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(id="Principal",
         tabPanel(id="SelecioneModelo",title = "Selecione seu Modelo",plotlyOutput("distPlot")),
         tabPanel(id="ComparacaoSupervisionada",title="Comparacao Supervisionada",tableOutput("Supervised")),
         tabPanel(id="Unsupervised",title="Aprendizado não supervisionado",plotOutput("NonSupervisedPlot"))
        ),
        h3("Aplicativo Desenvolvivo por Rafael Silva Pereira"),
        h3("Contato: r.s.p.models@gmail.com"),
        h3("Inicie o processo subindo um arquivo csv"),
        h3("Processos de machine learning costumam ser demorados, utilize a implementação paralela em sua maquina")
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
  output$ClusterVariables <- renderUI({
    w=LeituraArquivo()
    w=as.data.frame(w)
    if(input$UnsupervisedChoice=='Kmeans')
      w1=w[,which(sapply(w,class) %in% c('numeric','integer'))]
    else if(input$UnsupervisedChoice=='Kmodes')
      w1=w[,which(sapply(w,class) %in% c('factor','character'))]
      selectInput(inputId = "EliminaCluster",label = "Variaveis que não serão utilizadas para clusterização",choices = names(w1),multiple = TRUE)

  })
  
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
      string=paste("The model",input$Modelo,sep=" ")
      string=paste(string,"is being trained on instance",sep=" ")
	string=paste(string,i,sep=" ")
	string=paste(string,"of",sep=" ")
	string=paste(string,input$NumeroDeTestes,sep=" ")
      showModal(modalDialog(string, footer=NULL))
      ModeloFinal=TrainModel(input$Modelo)
      Ensemble[[i]]=ModeloFinal
      removeModal()
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
	string=paste("The model Bayes is being trained on instance",i,"of",input$NumeroDeTestes,sep=" ")
        showModal(modalDialog(string, footer=NULL))
        ModeloFinal=TrainModel('Bayes')
        EnsembleBayes[[i]]=ModeloFinal
        AcuraciaBayes[i]=max(ModeloFinal$results$Accuracy)
	removeModal()
       }
       EnsembleKNN=list()
       AcuraciaKnn=c()
       for(i in 1:input$NumeroDeTestes){
	string=paste("The model KNN is being trained on instance",i,"of",input$NumeroDeTestes,sep=" ")
        showModal(modalDialog(string, footer=NULL))

         ModeloFinal=TrainModel('KNN')
         EnsembleKNN[[i]]=ModeloFinal
         AcuraciaKnn[i]=max(ModeloFinal$results$Accuracy)
	removeModal()
       }
       EnsembleLogistic=list()
       AcuraciaLogistic=c()
       for(i in 1:input$NumeroDeTestes){
	string=paste("The model Logistic Regression is being trained on instance",i,"of",input$NumeroDeTestes,sep=" ")
        showModal(modalDialog(string, footer=NULL))

         ModeloFinal=TrainModel('Logistic Regression')
         EnsembleLogistic[[i]]=ModeloFinal
         AcuraciaLogistic[i]=max(ModeloFinal$results$Accuracy)
	removeModal()
       }
       EnsembleTree=list()
       AcuraciaTree=c()
       for(i in 1:input$NumeroDeTestes){
	string=paste("The model Decision Tree is being trained on instance",i,"of",input$NumeroDeTestes,sep=" ")
        showModal(modalDialog(string, footer=NULL))

         ModeloFinal=TrainModel('Decision Tree')
         EnsembleTree[[i]]=ModeloFinal
         AcuraciaTree[i]=max(ModeloFinal$results$Accuracy)
	removeModal()
       }
       EnsembleSVM=list()
       AcuraciaSVM=c()
       for(i in 1:input$NumeroDeTestes){
	string=paste("The model SVM is being trained on instance",i,"of",input$NumeroDeTestes,sep=" ")
        showModal(modalDialog(string, footer=NULL))

         ModeloFinal=TrainModel('SVM')
         EnsembleSVM[[i]]=ModeloFinal
         AcuraciaSVM[i]=max(ModeloFinal$results$Accuracy)
	removeModal()
       }
       EnsembleForest=list()
       AcuraciaForest=c()
       for(i in 1:input$NumeroDeTestes){
	string=paste("The model Random Forest is being trained on instance",i,"of",input$NumeroDeTestes,sep=" ")
        showModal(modalDialog(string, footer=NULL))

         ModeloFinal=TrainModel('Random Forest')
         EnsembleForest[[i]]=ModeloFinal
         AcuraciaForest[i]=max(ModeloFinal$results$Accuracy)
	removeModal()
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
string=paste("The model Linear Regression is being trained on instance",i,"of",input$NumeroDeTestes,sep=" ")
        showModal(modalDialog(string, footer=NULL))

          ModeloFinal=TrainModel('Linear Regression')
          Ensemblelm[[i]]=ModeloFinal
          Acuracialm[i]=min(ModeloFinal$results$RMSE)
	removeModal()
        }
        
        
        EnsembleTree=list()
        AcuraciaTree=c()
        for(i in 1:input$NumeroDeTestes){
string=paste("The model Regression Tree is being trained on instance",i,"of",input$NumeroDeTestes,sep=" ")
        showModal(modalDialog(string, footer=NULL))

          ModeloFinal=TrainModel('Regression Tree')
          EnsembleTree[[i]]=ModeloFinal
          AcuraciaTree[i]=min(ModeloFinal$results$RMSE)
	removeModal()
        }
        
        EnsembleForest=list()
        AcuraciaForest=c()
        for(i in 1:input$NumeroDeTestes){
string=paste("The model Random Forest is being trained on instance",i,"of",input$NumeroDeTestes,sep=" ")
        showModal(modalDialog(string, footer=NULL))

          ModeloFinal=TrainModel('Random Forest')
          EnsembleForest[[i]]=ModeloFinal
          AcuraciaForest[i]=min(ModeloFinal$results$RMSE)
	removeModal()
        }
        
        Modelos=data.frame(Acuracialm,AcuraciaTree,AcuraciaForest)
        names(Modelos=c('Linear Regression','Regression Tree','Random Forest'))
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
     #showModal(modalDialog("The code is currently running please wait", footer=NULL))
     
     ModeloFinal <- train(as.formula(paste(input$X, "~ .")),data=Training,method=MetodoUtilizado,trControl=trctrl,tuneLength = 10)
     #print("huebr")
     #removeModal()
     
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
       nomes=names(Tabela)
       metricas=c('Média','Mediana','Desvio Padrão','Minimo','Maximo')
       Tabela1=data.frame(matrix(NA,nrow=5,ncol=ncol(Tabela)) )
       Tabela1[1,]=apply(Tabela,2,mean,na.rm=TRUE)
       Tabela1[2,]=apply(Tabela,2,median,na.rm=TRUE)
       Tabela1[3,]=apply(Tabela,2,sd,na.rm=TRUE)
       Tabela1[4,]=apply(Tabela,2,min,na.rm=TRUE)
       Tabela1[5,]=apply(Tabela,2,max,na.rm=TRUE)
       Tabela1=data.frame(metricas,Tabela1)
       names(Tabela1)[1]='Metricas'
       names(Tabela1)[2:ncol(Tabela1)]=names(Tabela)
       return(Tabela1)
       
     }
   })
   
   UnsupervisedTraining <- eventReactive(input$AtivarTreinamento,{
     if(!is.null(input$Arquivo)){
       w=LeituraArquivo()
       Listas=list()
       
       w=as.data.frame(w)
       if(length(input$EliminaCluster)>0){
        Remover=which(names(w) %in% input$EliminaCluster)
        w=w[,-Remover]
       }
       print("Comecou Clusterizacao")
     if(input$UnsupervisedChoice=="Kmodes"){
       Categoricos=w[,which(sapply(w,class) %in% c('factor','character'))]
       require(klaR)
       kmo=kmodes(Categoricos,modes=input$Clusters,iter.max=100)
       Listas[[1]]=w
       print(class(Categoricos))
       
       Listas[[2]]=kmo
       print("Terminou Clusterizacao")
       return(Listas)
     }
     else if(input$UnsupervisedChoice=="Kmeans"){
       Numericos=w[,which(sapply(w,class) %in% c('numeric','integer'))]
       
       kmo=kmeans(Numericos,centers = input$Clusters,iter.max=100)
       Listas=list()
       print(class(Numericos))
       Listas[[1]]=w
       Listas[[2]]=kmo
       print("Terminou Clusterizacao")
       return(Listas)
     }
     }
   })
   
   output$NonSupervisedPlot <- renderPlot({
     if(!is.null(input$Arquivo)){
       print("Testando")
      w=LeituraArquivo()
      source("PlotPairs.R")
      Lista=UnsupervisedTraining()
      if(length(Lista)==2){
        Pontos=Lista[[1]]
        print(class(Pontos))
        kmo=Lista[[2]]
        p1<-PairPlot(w,kmo$cluster)
        print(p1)
        #plot(Pontos,col=kmo$cluster)
        #plot(Lista[[1]],Lista[[2]])
        
      }  
     }
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

