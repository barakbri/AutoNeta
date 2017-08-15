

library(shiny)

# definitions:
source('definitions.r')



shinyUI(
  navbarPage(div(img(src="hbp.jpg"), UI_LABELS$TITLE),selected = UI_LABELS$TAB_TRANSFORM,
  
             ###**************************************
             ### File window: save load, exports
             ###**************************************
             
            tabPanel(UI_LABELS$TAB_FILE,
                fluidPage(
                  
                  fluidRow(column(1,img(src="disk.png",height = '40px',width = '40px',style="margin-top: 25px;display: block; margin-left: auto; margin-right: auto;")),
                           column(4,h4(UI_LABELS$SAVE,style="padding:20px;vertical-align:center;"), downloadButton("button_Save",UI_LABELS$BUTTON_LABEL_SAVE))),
                  fluidRow(HTML('</br>')),
                  
                  fluidRow(column(1,img(src="folder.png",height = '40px',width = '40px',style="margin-top: 25px; display: block; margin-left: auto; margin-right: auto;")),
                           column(4,h4(UI_LABELS$LOAD,style="padding:20px;"),fileInput("button_Load",UI_LABELS$BUTTON_LABEL_LOAD))),
                  fluidRow(HTML('</br>')),
                  
                  fluidRow(column(1,img(src="book.png",height = '40px',width = '40px',style="margin-top: 25px;display: block; margin-left: auto; margin-right: auto;")),
                           column(4,h4(UI_LABELS$EXPORT,style="padding:20px;"),uiOutput('ui_export_trans_data'))),
                  fluidRow(HTML('</br>')),
                  
                  fluidRow(column(1,img(src="checklist.png",height = '40px',width = '40px',style="margin-top: 25px;display: block; margin-left: auto; margin-right: auto;")),
                           column(4,h4(UI_LABELS$EXPORT_TRANS,style="padding:20px;"),uiOutput('ui_export_trans_report')))
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
                           min = SLIDER_BINSIZE_MIN_MULTIPLIER, max = SLIDER_BINSIZE_MAX_MULTIPLIER, value = 1,width = '85%',sep='',step = 0.1
      )),
      column(6,sliderInput("graphicalparameter_KernelWidth", UI_LABELS$SLIDER_KDE_WIDTH,
                          min = SLIDER_KERNELWIDTH_MIN_MULTIPLIER, max = SLIDER_KERNELWIDTH_MAX_MULTIPLIER, value = 1,width = '85%',sep='',step = 0.1
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
             uiOutput("ui_download_generated_vardef")),
                       mainPanel())),


###**************************************
### Help and About windows
###**************************************

navbarMenu(UI_LABELS$HELP_MENU,
           tabPanel(UI_LABELS$HELP_MENU_HELP_ITEM,
              tags$iframe(style="height:900px; width:100%", src="project_help.pdf")      
           ),
           tabPanel(UI_LABELS$HELP_MENU_ABOUT_ITEM,
              h3('Semi-Automatic Data Transformation Manager',style = 'align:center;'),
              h4('Developed by Tzviel Frostig, <tfrostig at gmail dot com> ; '),
              h4('and Barak Brill, <barakbri at mail dot tau dot ac dot il> ;  '),
              h4('with additional code by Neta Shachar and Tal Kozlovski'),
              h4('github page: https://github.com/barakbri/AutoNeta'),
              h4(paste0('Version: ',VERSION_SERVER))
           )
)
         
) # end of navbarPage
) # end of ShinyUI