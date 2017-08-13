

library(shiny)

shinyUI(
  navbarPage(div(img(src="hbp.jpg"), "AutoNeta"),
             
  ###**************************************
  ### Transform window, main window for app
  ###**************************************
  tabPanel("Transform",
  fluidPage(
    ###**************************************
    ### Transform window: main window for app
    ###**************************************
    # upper toolbar, buttons are disabled until data and vardef are loaded
    fluidRow(column(2,HTML("Save current work space:"), downloadButton("button_Save","Save")),
             column(4,HTML("Load previous work space:"),fileInput("button_Load","")),
             column(2,HTML("Export transformed dataset:"),downloadButton("button_Export","Export Transformed Data")),
             column(2,HTML("Export Transformation report:"),downloadButton("button_ExportTransReport","Transformation Report"))
    ),
    HTML('<hr style="color: gray;">'),
    ###**************************************
    ### Transform window: main body of transform window - sidebar and main window
    ###**************************************
    fluidRow(column(12,
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
                        verbatimTextOutput("StatusLine"),
                        htmlOutput("button_Apply")
                      ),
                      
                      mainPanel(
                        #plotOutput("output_Graph_1")
                        fluidPage(
                          #fluidRow(plotOutput("output_Graph_1"))
                          fluidRow(
                            column(4,plotOutput("output_Graph_1",height = "300px",click = "click_Graph_1")),
                            column(4,plotOutput("output_Graph_2",height = "300px",click = "click_Graph_2")),
                            column(4,plotOutput("output_Graph_3",height = "300px",click = "click_Graph_3"))
                          ),
                          fluidRow(
                            column(4,plotOutput("output_Graph_4",height = "300px",click = "click_Graph_4")),
                            column(4,plotOutput("output_Graph_5",height = "300px",click = "click_Graph_5")),
                            column(4,plotOutput("output_Graph_6",height = "300px",click = "click_Graph_6"))
                          )
                          
                          
                        )
                      )# end of main panel
                    )# end of sidebar layout
                    ) 
    ),
    ###**************************************
    ### Transform window: this is the graphical parameters row
    ###**************************************
    fluidRow(
      sliderInput("graphicalparameter_BinSize", "Bin Size:",
                          min = 0, max = 1000, value = 500
      ),
      sliderInput("graphicalparameter_KernelWidth", "KDE Width:",
                          min = 0, max = 1000, value = 500
      )
    )
  )# end of fluid page (upper toolbar, sidebar layout, advanced panel)
), #end of tabPanel - Transform

###**************************************
### Variable Definition window
###**************************************
tabPanel("Generate Variable Defenition",sidebarLayout(sidebarPanel(),mainPanel())),

###**************************************
### Help and About windows
###**************************************

navbarMenu("Help",
           tabPanel("Help",
              htmlOutput("Page_Help")      
           ),
           tabPanel("About",
              htmlOutput("Page_About")            
           )
)
         
) # end of navbarPage
)#end of ShinyUI