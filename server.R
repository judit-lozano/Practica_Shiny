library(shiny)
library(ggplot2)

shinyServer(#una funcion dentro de shinyServer
  function(input, output) {
    
    funcion_base <- function(num=3, chosen, strategy){
      doors<-1:num
      guess<-chosen
      prize<-floor(runif(1,1,(num+1))) #randomize which door has the good prize
      ## Reveal one of the doors you didn't pick which has a bum prize
      if(prize!=guess){
        reveal<-doors[-c(prize,guess)]
      }else{
        reveal<-sample(doors[-c(prize,guess)],length(doors)-2)
      }
      ## Stay with your initial guess or switch
      if(strategy=='switch'){
        select<-doors[-c(reveal,guess)]
      }else{
        select<-guess
      }
      # resultado
      if(select==prize)
      {
        outcome<-'Ganas el premio!'
      }else{
        outcome<-'Pierdes!'
      }
      
      return(cat(paste('Numero de puertas para jugar: ',num,
                       '\nPuerta elegida al principio: ',guess,
                       '\nEstrategia escogida: ',strategy,
                       '\nPuerta elegida al final: ',select,
                       '\nPuerta con Premio: ',prize,
                       '\n',outcome,'\n\n',sep='')))
    }
    
    generate_game <- function() {
      # creates the initual random game setup
      prizes <- sample(c("car", "goat", "goat"), size = 3)
      door_num <- as.character(c(1:3))
      doors <- cbind(door_num, prizes)
      return(doors)
    }
    
    values <- reactiveValues()
    values$door_choice <- c(0, 0, 0)
    
    # the names of the image files for html rendering
    images <- list("door" = "images/transparent_door.png",
                   "car" = "images/car.jpg",
                   "goat" = "images/goat.jpg")
    
    game_update <- function() {
      # the main reactive function
      # updates the game matrix based on user selection and uses .rda's to
      # maintain state; majority of game logic is here
      if (input$select %% 3 == 0) {
        isolate({
          values$game <- generate_game()
          values$game <- cbind(values$game, rep(images[["door"]], 3))
        })
      } else if (input$select %% 3 == 1) {
        show_door <- as.character(min(which(c(values$game[,1] != input$selectDoor & values$game[,2] != "car"))))
        new_im <- images[[values$game[as.numeric(show_door),2]]]
        isolate({
          d <- as.numeric(input$selectDoor)
          values$door_choice[d] <- values$door_choice[[d]] + 1/2
          values$show_door <- show_door
          values$game[as.numeric(show_door),3] <- new_im
        })
      } else if (input$select %% 3 == 2) {
        if (!(as.logical(input$stay))) {
          picked_door <- values$game[,1] != values$show_door & values$game[,1] != input$selectDoor
        } else {
          picked_door <- values$game[,1] == input$selectDoor
        }
        isolate({
          values$picked_door <- picked_door
          values$game[,3] <- sapply(values$game[,2], function(x){images[[x]]})
        })
      }
      values$game
    }
    
    output$doors <- renderText({
      # renders the html for the doors and prizes; simpler than independent renders
      values$game <- game_update()
      HTML(paste0('<div><img src="', values$game[1,3],'" height="200" width="200">',
                  '<img src="', values$game[2,3],'" height="200" width="200">',
                  '<img src="', values$game[3,3],'" height="200" width="200">'))
    })
    
    output$current <- renderText({
      HTML(paste('<h6><b>Your current door --', input$selectDoor, '</b></h6>'))
    })
    
    output$winner <- renderText({
      door <- which(values$picked_door)[[1]]
      prize <- values$game[values$picked_door, 2]
      HTML(paste('<h5>You\'ve picked', door, 'and won a', prize, '!</h5>'))
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
    
    
    
    
    
    
    
    
    
    observeEvent(input$Cross, {
      global$years <- input$Cross
    })
    
    output$value <- renderUI({
      radioButtons(
        inputId = "Cross",
        label = "Elige una muerta",
        choiceNames = c(1:input$sample_size),
        choiceValues = c(1:input$sample_size),
        selected = 1
      )
    })

    #   output$value <- renderUI({
    #     b <- 1:5
    #     checkboxGroupInput(
    #     inputId = "Chk",
    #     label = "Estrategia",
    #     choices = c(1,2,3,4,5),
    #     inline = TRUE, width = "100%")
    # #algo <- eventReactive(input$refresh, {input$Chk}, ignoreNULL = FALSE)
    #  })
    observeEvent(input$Cross,{
        output$summary <- renderPrint({ #genero el sumario descriptivo que se actualiza según se actualice el set de datos mybox$df
        #y se traslada al UI a traves del ID "summary"
        funcion_base(input$sample_size, input$Cross, input$vary)
        })
      })
  }
)