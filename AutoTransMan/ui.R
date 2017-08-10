

library(shiny)

shinyUI(fluidPage(
  

  titlePanel("SaDaT - Semi-automated Data Transformation Manager"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput('file_VarDef', 'Choose variable definitions CSV file',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      
      fileInput('file_Data', 'Choose Data CSV file',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      #lists
      fluidPage(
        #before after lists
        fluidRow(column(12,checkboxInput("checkbox_sort_by_yule", "Sort By Yule", TRUE))),
        fluidRow(column(12,selectInput('ui_list_Before', 'Before:', NULL , multiple=TRUE, selectize=FALSE))),
        fluidRow(column(12,selectInput('ui_list_After', 'After:', NULL, multiple=TRUE, selectize=FALSE)))
        ),
      
      verbatimTextOutput("StatusLine")
    ),
    
    mainPanel(
      #plotOutput("output_Graph_1")
      fluidPage(
        #fluidRow(plotOutput("output_Graph_1"))
        fluidRow(
                 column(4,plotOutput("output_Graph_1",height = "300px")),
                 column(4,plotOutput("output_Graph_2",height = "300px")),
                 column(4,plotOutput("output_Graph_3",height = "300px"))
                ),
        fluidRow(
                 column(4,plotOutput("output_Graph_4",height = "300px")),
                 column(4,plotOutput("output_Graph_5",height = "300px")),
                 column(4,plotOutput("output_Graph_6",height = "300px"))
        )
        
        
      )
    )
  )
))