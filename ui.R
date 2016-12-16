#Layout ui
library(shiny)
library(shinydashboard)
library(ggplot2)
require(plyr)
library(DT)
library(Rcpp)
sourceCpp('rcpp_examples.cpp')
library(invgamma)

#transit <- read.csv("\Transit_demand.csv",header=TRUE,sep=",")



ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "Estadistica 2016"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inversa Exp", tabName = "Tarea1", icon = icon("fa fa-bar-chart")),
      menuItem("Int. MC", tabName = "Tarea2", icon = icon("fa fa-calculator")),
      menuItem("Cadena de Markov", tabName = "Tarea3", icon = icon("fa fa-eye")),
      menuItem("Metropolis", tabName = "Tarea4", icon = icon("fa fa-bar-chart"))
    )
  ),
  dashboardBody(
    tabItems(

##############################################    Tarea 1     #########################################################################################
            
      # First tab content
      tabItem(tabName = "Tarea1",
              fluidRow(
                box(
                  width = 6,
                  numericInput("lambda", "Lambda igual a", 1,min=0)
                ),
                box(
                   width = 6, 
                   numericInput("Simulaciones", "Numero de simulaciones", 1000)
                )
               
              ),
              fluidRow(
                box(width=12, title = "Simulacion de una variable exponencial",
                    plotOutput("plot1", height = 250))
                     ),
              
              fluidRow(box(width=12,
                       numericInput("alp", "Alpha", .05,step=.01,min=0,max=1),
                       verbatimTextOutput("value"))
              )
              
              ),
      
##############################################    Tarea 2     #########################################################################################

      # Second tab content
      tabItem(tabName = "Tarea2",
              box(width=6,textInput(inputId="expresion1", label="Funcion",value="function(x) x")),
              box(width=6,numericInput("NN", "Muestra", 1000)),
              box(width=4,numericInput("N1", "Lim Inf", 0)),
              box(width=4,numericInput("N2", "Lim sup", 1000)),
              box(width=4,numericInput("alpha", "Alfa", .05,step=.01,min=0,max=1)),
              fluidRow(box(width=12,verbatimTextOutput("Integracion"))),
              fluidRow( box(width=6, title = "Grafico de la funcion",plotOutput("plot2")),
                        box(width=6, title = "IC para diferentes tamanos de muestra",plotOutput("plot3"))
                        )

      ),

##############################################    Tarea 3     #########################################################################################

      #Third tab content
      tabItem(tabName = "Tarea3",
              fluidRow(box(width=12,fileInput("archi", label = h3("Matriz") ))),
              fluidRow(box(width=6,numericInput("inicio", "Estado inical", 1,min=0,max=1)),
                       box(width=6,numericInput("trans", "Numero de transiciones", 100,min=1,max=100000))),
              fluidRow(box(width=12,title="Matriz de transicion",dataTableOutput('tabla'))),
              fluidRow(box(width=12,title="Trayectoria",verbatimTextOutput("corrida")))
              ),

##############################################    Tarea 4     #########################################################################################

#Third tab content
tabItem(tabName = "Tarea4",
        fluidRow(box(width=12,fileInput("archivo", label = h3("Carga Primero un .CSV") ))),
        fluidRow(box(width=12,title="Transit demand",dataTableOutput('mytrans'))),
        fluidRow(box(width=6,selectInput("variableX", "Variable_Independiente:",
                                         choices=list("Price.per.week"=2 ,"Population.of.city"=3 ,
                                                      "Monthly.income.of.riders"=4 ,"Average.parking.rates.per.month"=5,
                                                      "Number.of.weekly.riders"=6 ),selected=3
                                                     )),
                 box(width=6,selectInput("variableY", "Variable_Dependiente:",
                                         choices=list("Price.per.week"=2 ,"Population.of.city"=3 ,
                                                      "Monthly.income.of.riders"=4 ,"Average.parkig.rates.per.month"=5,
                                                      "Number.of.weekly.riders"=6 ),selected=6))),
                fluidRow(box(width=12,title = "Diagrama de dispersion",plotOutput("dispersion"))),
                fluidRow(box(width=12,title="Aprioris",textOutput("aprioris"))),
                fluidRow(box(width=12,plotOutput("apriori1"))),
                fluidRow(box(width=12,plotOutput("apriori2"))),
                fluidRow(box(width=12,plotOutput("apriori3"))),
                fluidRow(box(width=4,numericInput("longcad", "Numero de cadenas por simular", 1,min=1)),box(width=4,numericInput("numcad", "Longitud de las cadenas", 10000,min=100)),box(width=4,actionButton("button", "Corre MCMC"))),
                fluidRow(box(width=12,title="Simulacion MCMC",dataTableOutput('cadenas'))),
                fluidRow(box(width=12,title="Graficos de parametros",plotOutput("histos"))),
                fluidRow(box(width=12,title="Graficos posterioris",plotOutput("distos")))
        
        
)
      
    )
  ))
