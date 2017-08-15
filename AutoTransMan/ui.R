

library(shiny)

# definitions:
source('definitions.r')



shinyUI(
  navbarPage(div(img(src="hbp.jpg"), UI_LABELS$TITLE),selected = UI_LABELS$TAB_TRANSFORM,
  
             ###**************************************
             ### File window: save load, exports
             ###**************************************
             
            tabPanel(UI_LABELS$TAB_FILE,
                fluidPage(fluidRow(column(2,HTML(UI_LABELS$SAVE), downloadButton("button_Save",UI_LABELS$BUTTON_LABEL_SAVE)),
                                   column(4,HTML(UI_LABELS$LOAD),fileInput("button_Load",UI_LABELS$BUTTON_LABEL_LOAD)),
                                   column(2,HTML(UI_LABELS$EXPORT),downloadButton("button_Export",UI_LABELS$BUTTON_LABEL_EXPORT_DATA)),
                                   column(2,HTML(UI_LABELS$EXPORT_TRANS),downloadButton("button_ExportTransReport",UI_LABELS$BUTTON_LABEL_EXPORT_TRANS))
                ),
                HTML('<hr style="color: gray;">')
            )
          ),
                        
  ###**************************************
  ### Transform window, main window for app
  ###**************************************
  tabPanel(UI_LABELS$TAB_TRANSFORM,
  fluidPage(
    
    
    ###**************************************
    ### Transform window: main body of transform window - sidebar and main window
    ###**************************************
    fluidRow(column(12,
                    sidebarLayout(
                      sidebarPanel(
                        fileInput('file_VarDef', UI_LABELS$UPLOAD_VARDEF_LABEL,
                                  accept=c('text/csv', 
                                           'text/comma-separated-values,text/plain', 
                                           '.csv')),
                        
                        fileInput('file_Data', UI_LABELS$UPLOAD_DATA_LABEL,
                                  accept=c('text/csv', 
                                           'text/comma-separated-values,text/plain', 
                                           '.csv')),
                        #lists
                        fluidPage(
                          #before after lists
                          fluidRow(column(12,checkboxInput("checkbox_sort_by_yule", UI_LABELS$SORT_BY_YULE , TRUE))),
                          fluidRow(column(12,selectInput('ui_list_Before', UI_LABELS$BEFORE_LIST, NULL , multiple=TRUE, selectize=FALSE))),
                          fluidRow(column(12,selectInput('ui_list_After', UI_LABELS$AFTER_LIST, NULL, multiple=TRUE, selectize=FALSE)))
                        ),
                        verbatimTextOutput("StatusLine"),
                        htmlOutput("button_Apply")
                      ),
                      
                      mainPanel(
                        #plotOutput("output_Graph_1")
                        fluidPage(
                          #fluidRow(plotOutput("output_Graph_1"))
                          fluidRow(
                            column(4,plotOutput("output_Graph_1",height = PLOT_HEIGHT,click = "click_Graph_1")),
                            column(4,plotOutput("output_Graph_2",height = PLOT_HEIGHT,click = "click_Graph_2")),
                            column(4,plotOutput("output_Graph_3",height = PLOT_HEIGHT,click = "click_Graph_3"))
                          ),
                          fluidRow(
                            column(4,plotOutput("output_Graph_4",height = PLOT_HEIGHT,click = "click_Graph_4")),
                            column(4,plotOutput("output_Graph_5",height = PLOT_HEIGHT,click = "click_Graph_5")),
                            column(4,plotOutput("output_Graph_6",height = PLOT_HEIGHT,click = "click_Graph_6"))
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
      column(6,sliderInput("graphicalparameter_BinSize", UI_LABELS$SLIDER_BIN_SIZE,
                           min = 0, max = 1000, value = 500,width = '85%'
      )),
      column(6,sliderInput("graphicalparameter_KernelWidth", UI_LABELS$SLIDER_KDE_WIDTH,
                          min = 0, max = 1000, value = 500,width = '85%'
      ))
    )
  )# end of fluid page (upper toolbar, sidebar layout, advanced panel)
), #end of tabPanel - Transform


###**************************************
### Variable Definition window
###**************************************
tabPanel(UI_LABELS$TAB_GENERATE_VARDEF, 
         sidebarLayout(
           sidebarPanel(
             fileInput('file_varGuess', UI_LABELS$UPLOAD_DATA_LABEL,
                                               accept=c('text/csv', 
                                                        'text/comma-separated-values,text/plain', 
                                                        '.csv')),
             uiOutput("ui")),
                       mainPanel())),

#
###**************************************
### Help and About windows
###**************************************

navbarMenu(UI_LABELS$HELP_MENU,
           tabPanel(UI_LABELS$HELP_MENU_HELP_ITEM,
              htmlOutput("Page_Help")      
           ),
           tabPanel(UI_LABELS$HELP_MENU_ABOUT_ITEM,
              htmlOutput("Page_About")            
           )
)
         
) # end of navbarPage
)#end of ShinyUI