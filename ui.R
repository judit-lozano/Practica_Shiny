library(shiny)
library(ggplot2)

variables = list("Cambiar la puerta"="switch", "Mantener la puerta"="stay")

#estructura jerárquica que empieza con shinyUI
shinyUI(fluidPage(
  titlePanel("Monty Hall Paradox"), #hijo de fuidPage
  sidebarLayout(#hijo de fuidPage
    sidebarPanel(#hijo de sidebarLayout
      numericInput('sample_size', 'Numero de puertas',
                   3, min = 3, max = 10000),#hijo de sidebarPanel, aquí se le pide al usuario un input numérico, en este caso que elija el tamaño de la muestra.
      numericInput('choice', 'Elige una puerta para empezar',
                   3, min = 1, max = 10000),
      selectInput("vary", #aquí se le pide al usuario un input de seleccionar, en este caso que elija el filtrado de cut para el que quiere ver el gráfico.
                  label="Estrategia",
                  choices=variables)#hijo de sidebarPanel
    ),
    mainPanel(
      verbatimTextOutput("summary") #aqui quiero colocar el gráfico "graph" que genero desde mi server file.
    )
  )
)
)