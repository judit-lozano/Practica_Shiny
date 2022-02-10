library(dplyr)
library(shiny)
library(ggplot2)
library(shinyjs)
library(shinythemes)
library(bslib)

#estructura jerárquica que empieza con shinyUI
ui <- fluidPage(theme = shinytheme("united"),
  useShinyjs(),
  titlePanel("Monty Hall Paradox"), 
  tabsetPanel(
    tabPanel("Single Experiment", fluidRow(
      # Columna de 3 para los selects, y columna de 9 para los botones/dibujos
      column(width = 3,
             # conditional panels que cambian en el UI
             conditionalPanel( "input.select % 3 == 0",
                               numericInput('num', label="Elige el numero de puertas",value=3, min=3,max=500),
                               uiOutput('door_choice')
             ), conditionalPanel( "input.select % 3 == 1", 
                                  htmlOutput("current"),
                                  selectInput("strategy", label = "¿Quieres cambiar de puerta?",
                                              choices = list("Mantengo la puerta" = "stay", "Cambio la puerta" = "switch"))
             ), conditionalPanel("input.select % 3 == 2",
                                 htmlOutput("final")
                                 
             ), actionButton("select", "Continuar")
      ),
      column(width = 9, 
             br(),br(),
             uiOutput('my_buttons'),
             br(),
             verbatimTextOutput("summary"),
             br(),
             uiOutput('last'))
    )),
    tabPanel("Multiple Experiment", fluidRow(
      column(width = 4,
             textOutput("monte_about"),
             sliderInput("n_iter", label = "Number of Trials", min = 1,
                         max = 200, value = 25)),
      column(width = 8, plotOutput("Monte_Carlo"))
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
  
  output$door_choice <- renderUI({
    selectInput("selectDoor", label = "Elige una puerta", 
                choices = c(mybox$df$door_num))
    
  })
  
  observeEvent(input$selectDoor, {
    mybox$selectDoor <- input$selectDoor
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
  
  output$last <- renderUI({
    if (input$select %% 3 == 2) {
      actionButton("last", "Jugar de Nuevo")
    }
  })
  
  
  output$my_buttons <-renderUI({
    if (input$select %% 3 == 0) {
      actionGroupButtons(
        inputIds = c(mybox$df$step_1),
        labels = c(mybox$df$step_1),
        status = "primary")
      
    } else if (input$select %% 3 == 1) {
      if(mybox$df$prize[1]!=mybox$selectDoor){
        mybox$df <- mybox$df %>% 
          mutate(reveal= case_when(door_num==prize ~ door_num,
                                   door_num==mybox$selectDoor ~ door_num,
                                   TRUE ~ "Cabra"))
        
        mask1 <- mybox$df$reveal!="Cabra"
        mybox$a <- c(mybox$df$reveal[mask1])
        
        actionGroupButtons(
          inputIds = c(mybox$df$reveal),
          labels = c(mybox$df$reveal),
          status = "primary")
      }else{
        mybox$mi_vector <- c(sample(mybox$df$door_num[-which(c(mybox$df$door_num== mybox$df$prize[1]))],
                                    nrow(mybox$df)-2))
        mybox$df <- mybox$df %>% 
          mutate(reveal= if_else(door_num %in% mybox$mi_vector, "Cabra", door_num))
        
        mask1 <- mybox$df$reveal!="Cabra"
        mybox$a <- c(mybox$df$reveal[mask1])
        
        actionGroupButtons(
          inputIds = c(mybox$df$reveal),
          labels = c(mybox$df$reveal),
          status = "primary") }
      
    } else if (input$select %% 3 == 2) {
      if(mybox$strategy=='switch'){
        
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
      
      actionGroupButtons(
        inputIds = c(mybox$df$words),
        labels = c(mybox$df$words),
        status = "primary"
      ) 
    }  
    
  })
  
  output$current <- renderText({
    HTML(paste('<h6><b>Tu puerta escogida es la --', input$selectDoor, '</b></h6>'))
  })
  
  output$final <- renderText({
    # resultado
    if(mybox$final==mybox$df$prize[1]){
      HTML(paste('<h6><b>!Ganas el Premio!</b></h6>'))
      #mybox$resultado <- "¡Ganas el Coche!"
    }else{
      HTML(paste('<h6><b>...Pierdes...</b></h6>'))
      #mybox$resultado <- "Pierdes"
    }
  })
  
  
  # monte-carlo simulation in final tab
  output$Monte_Carlo <- renderPlot({
    
    wins <- c(0, 0)  # no switch, switch
    for (i in c(0:input$n_iter)) {
      prizes <- sample(c("car", "goat", "goat"), size = 3)
      door_num <- c(1:3)
      doors <- cbind(door_num, prizes)
      
      selected <- sample(c(1:3), 1)
      switch <- sample(c(TRUE, FALSE), 1)
      show <- min(which(doors[,1] != selected & doors[,2] != "car"))
      if (switch) {
        selected <- doors[,1] != selected & doors[,1] != show
      }
      if (doors[selected,2] == "car") {
        switch <- switch + 1
        wins[switch] <- wins[[switch]] + 1
      }
    }
    
    barplot(wins/sum(wins), names.arg = c("Stayed", "Switched"), ylim = c(0, 1), 
            main = "Percentage of total wins", col = "sky blue")
    
  })
}

# Lanza la aplicacion
shinyApp(ui=ui, server = server)