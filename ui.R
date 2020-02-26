library(shinydashboard)
library(DT)
library(shinythemes)


navbarPage(theme=shinytheme("sandstone"), "Fantasy Oracle", fluid = T, 
           tabPanel("Rankings",
                    box(width="100%",
                        selectInput("Position", "Select Position",choices=c("RB", "WR", "QB", "TE", "FLEX", "K", "DEF"), width= "100%"),           
                        textOutput("txt"),
                        plotOutput("plot", width = "100%", height="700"),
                        htmlOutput("Rankings"),
                        DTOutput("table", width = "100%"),
                        htmlOutput("DefDesc"),
                        DTOutput("Defense", width= "100%"))),
           tabPanel("Research",
                    box(width="100%",

                        fluidRow(
                          column(6,
                                 uiOutput("giff"),
                                 imageOutput("img", width = "100%"),
                          br(),
                          br(),
                          br(),
                          br(),
                          uiOutput("blog"))))),
           tabPanel("Optimizer",
                    uiOutput("Optimizer"),
                    DTOutput("Picks", width="100%"),
                    selectInput("removeplyr", "Remove Player: ", choices=NULL),
                    actionButton("ReOptimize", "ReOptimize:", icon=icon("refresh")))
           # tabPanel("Donate",
           #          box(width="100%",
           #              fluidRow(
           #                column(4,
           #                       htmlOutput("btcAddy"),
           #                       imageOutput("btc"))))
           #          )
           
)
