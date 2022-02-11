library(dplyr)
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(shinyjs)
library(shinythemes)
library(bslib)
thematic::thematic_shiny(font = "auto")


#estructura jerárquica que empieza con shinyUI
ui <- fluidPage(
  theme = bs_theme(),
  useShinyjs(),
  titlePanel("Monty Hall Paradox"),
  img(src='logo.png', align = "right", height = '150px', width = '150px'),
  tabsetPanel(
    tabPanel("Single Experiment", fluidRow(
      # Columna de 3 para los selects, y columna de 9 para los botones/dibujos
      column(width = 4,
             # conditional panels que cambian en el UI
             conditionalPanel( "input.select % 3 == 0",
                               br(),
                               numericInput('num', label="Elige el numero de puertas",value=3, min=3,max=500)
             ),  
             conditionalPanel("input.select % 3 == 2",
                              br(),
                              htmlOutput("final"),
                              uiOutput("image")
                              
             )
      ),
      column(width = 8,
             br(),
             uiOutput('my_buttons'),
             conditionalPanel( "input.select % 3 == 0",
                               htmlOutput("elige_p")),
             conditionalPanel( "input.select % 3 == 1",
                               selectInput("strategy", label = 'Elige tu estrategia de juego, ¿quieres cambiar?',
                                           choices = list("Mantengo la puerta" = "mantener", "Cambio la puerta" = "cambiar"))),
             
             actionButton("select", "Continuar"),
             br(),
             verbatimTextOutput("summary"),
             br(),
             uiOutput('last'))
    )),
    tabPanel("Multiple Experiment", fluidRow(
      column(width = 4,
             br(),
             numericInput('doors', label="Numero de puertas",value=3, min=3,max=500),
             sliderInput("n_iter", label = "Numero de simulaciones", min = 1,
                         max = 200, value = 25),
             actionButton("generar", "Ejecutar")),
      column(width = 8, plotOutput("plot"))
    ))
  )
)

server <- function(input, output) {
  
  mybox <- reactiveValues()
  mybox$df <- data.frame()
  
  observeEvent(input$num, {
    prize<-as.character(floor(runif(1,1,(input$num+1))))
    door_num <- as.character(c(1:input$num))
    doors <- data.frame(door_num, prize)
    mybox$df <- doors %>%
      mutate(words=if_else(prize==door_num, "Coche", "Cabra"),
             step_1= paste0('Puerta ',door_num))
  })
  
  
  observeEvent(input$Id_Paso1, {
    mybox$selectDoor <- input$Id_Paso1
  })
  
  observeEvent(input$strategy, {
    mybox$strategy <- input$strategy
  })
  
  observeEvent(input$last, {
    refresh()
  })
  
  output$summary <- renderText({
    if (input$select %% 3 == 2) {
      paste('Resumen de la partida:',
            '\nNumero de puertas escogidas: ',nrow(mybox$df),
            '\nPuerta elegida al principio: ',mybox$selectDoor,
            '\nEstrategia escogida: ',mybox$strategy,
            '\nPuerta elegida al final: ', mybox$final,
            '\nPuerta con Premio: ',mybox$df$prize[1],
            '\nResultado: ',mybox$resultado,'\n\n',sep='')
    }
  })
  
  observeEvent(input$select, {
    if (input$select %% 3 == 2) {
      removeUI(selector='#select', immediate=TRUE)}
  }, autoDestroy=TRUE)
  
  
  
  output$last <- renderUI({
    if (input$select %% 3 == 2) {
      actionButton("last", "Jugar de Nuevo")
    }
  })
  
  
  output$my_buttons <-renderUI({
    if (input$select %% 3 == 0) {
      radioGroupButtons(
        inputId = 'Id_Paso1',
        label = 'Toca un botón para elegir una puerta',
        choices= c(mybox$df$door_num),
        status = "primary"
      ) 
      
    } else if (input$select %% 3 == 1) {
      if(mybox$df$prize[1]!=mybox$selectDoor){
        mybox$df <- mybox$df %>% 
          mutate(reveal= case_when(door_num==prize ~ door_num,
                                   door_num==mybox$selectDoor ~ door_num,
                                   TRUE ~ "Cabra"))
        
        mask1 <- mybox$df$reveal!="Cabra"
        mybox$a <- c(mybox$df$reveal[mask1])
        
        radioGroupButtons(
          inputId = 'Id_Paso2',
          label = HTML(paste('<h6><b>Tu puerta escogida al inicio es la --', input$Id_Paso1, '</b></h6>')),
          choices= c(mybox$df$reveal),
          status = "primary")
        
      }else{
        mybox$mi_vector <- c(sample(mybox$df$door_num[-which(c(mybox$df$door_num== mybox$df$prize[1]))],
                                    nrow(mybox$df)-2))
        mybox$df <- mybox$df %>% 
          mutate(reveal= if_else(door_num %in% mybox$mi_vector, "Cabra", door_num))
        
        mask1 <- mybox$df$reveal!="Cabra"
        mybox$a <- c(mybox$df$reveal[mask1])
        
        radioGroupButtons(
          inputId = 'Id_Paso2',
          label = HTML(paste('<h6><b>Tu puerta escogida al inicio es la --', input$Id_Paso1, '</b></h6>')),
          choices= c(mybox$df$reveal),
          status = "primary")
      }
      
    } else if (input$select %% 3 == 2) {
      if(mybox$strategy=='cambiar'){
        
        algo <- mybox$a!=mybox$selectDoor
        mybox$final <-mybox$a[algo]
        
      }else{
        mybox$final<-mybox$selectDoor
      }
      
      if(mybox$final==mybox$df$prize[1]){
        mybox$resultado <- "¡Ganas el Coche!"
      }else{
        mybox$resultado <- "Pierdes"
      }
      
      radioGroupButtons(
        inputId = 'Id_Final',
        label = 'Puertas Descubiertas',
        choices= c(mybox$df$words),
        status = "primary"
      ) 
    }  
    
  })
  
  output$final <- renderText({
    # resultado
    if(mybox$final==mybox$df$prize[1]){
      HTML(paste('<h6><b>!Ganas el Premio!</b></h6>'))
    }else{
      HTML(paste('<h6><b>...Pierdes...</b></h6>'))
    }
  })
  
  output$image <- renderUI({
    if(mybox$final==mybox$df$prize[1]){
      img(src = "coche.png", align ="left")
    }else{
      img(src = "cabra.jpeg",align ="left")
    }
  })
  
  
  output$elige_p <- renderText({
    # resultado
    if (input$select %% 3 == 0) {
      HTML(paste('<h6><b>Tu puerta escogida actualmente es la --', 
                 input$Id_Paso1, '</b></h6>'))
    }
  })
  
  
  # simulation in final tab
  
  observeEvent(input$generar, {
    output$plot <- renderPlot({
      
      wins<-c(0,0) #contador de las veces que se gana
      
      for (i in c(0:isolate({input$n_iter}))){
        
        prize <-floor(runif(1,1,isolate({input$doors+1})))
        doors <- c(1:isolate({input$doors}))
        selected <- sample(doors,1,replace=TRUE)
        strategy <- sample(c(TRUE,FALSE), 1)
        mi_df <- data.frame(doors, prize)
        df <- mi_df %>%
          mutate(prizes=if_else(prize==doors, "Coche", "Cabra"))
        
        show <- min(which(df[,1] != selected & df[,3] != "Coche"))
        if (strategy) {
          selected <- df[,1] != selected & df[,1] != show
        }
        if (df[selected,3][1] == "Coche") {
          strategy <- strategy + 1
          wins[strategy] <- wins[[strategy]] + 1
        }
      }
      
      barplot(wins/sum(wins), names.arg = c("Mantener", "Cambiar"), ylim = c(0, 1), 
              main = "Porcentajes Estrategia Ganadora", col = "sky blue")
      
      
    })
  })
  
  
  
}

# Lanza la aplicacion
shinyApp(ui=ui, server = server)