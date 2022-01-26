
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

#Test
#Si yo elijo:

num <- 3 #numero de puertas
chosen <- 1 #puerta elegida
strategy <-'stay' #'switch' or 'stay'

funcion_base(num, chosen, strategy)    

#Referencias:
#https://www.r-bloggers.com/2012/02/monty-hall-by-simulation-in-r/