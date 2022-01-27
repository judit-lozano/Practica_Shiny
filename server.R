library(shiny)
library(ggplot2)

shinyServer(#una funcion dentro de shinyServer
  function(input, output) {
    
    funcion_base <- function(num, chosen, strategy){
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
    

 
    
    output$summary <- renderPrint({ #genero el sumario descriptivo que se actualiza según se actualice el set de datos mybox$df
      #y se traslada al UI a traves del ID "summary"
      funcion_base(input$sample_size, input$choice, input$vary)
    })
 
  }
)