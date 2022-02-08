library(shiny)
library(ggplot2)
library(shinyWidgets)


#estructura jerárquica que empieza con shinyUI
shinyUI(fluidPage(
  titlePanel("Monty Hall Paradox"), 
  tabsetPanel(
              tabPanel("Single Experiment", fluidRow(
                # A column of 3 and column of 9
                # 3 is for buttons and 9 for rendering images
                column(width = 3,
                       # conditional panels change the UI for various points in the game
                       conditionalPanel( "input.select % 3 == 0",
                                         selectInput("selectDoor", label = "Select Door", 
                                                     choices = list("Door 1" = "1", "Door 2" = "2", "Door 3" = "3"))
                       ), conditionalPanel( "input.select % 3 == 1", 
                                            htmlOutput("current"),
                                            selectInput("stay", label = "Select Strategy",
                                                        choices = list("Stay" = TRUE, "Switch" = FALSE))
                       ), conditionalPanel("input.select % 3 == 2", 
                                           htmlOutput("winner")
                       ), actionButton("select", "Continue")
                ),
                column(width = 9, htmlOutput("doors"))
              )),
              tabPanel("Multiple Experiment", fluidRow(
                column(width = 4,
                       textOutput("monte_about"),
                       sliderInput("n_iter", label = "Number of Trials", min = 1,
                                   max = 200, value = 25)),
                column(width = 8, plotOutput("Monte_Carlo"))
              ))
  )
))