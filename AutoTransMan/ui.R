

library(shiny)
if (!require("DT")) install.packages('DT')
library(DT)

# definitions:
source('definitions.r')

ICON_SIZE = '35px'

shinyUI(
  navbarPage(title = UI_LABELS$TITLE,selected = UI_LABELS$TAB_FILE,
  
             ###**************************************
             ### File window: save load, exports
             ###**************************************
             
             tabPanel(UI_LABELS$TAB_FILE,
                      sidebarLayout(
                        # Sidebar with controls
                        sidebarPanel(
                          fluidPage(
                            fluidRow(column(1,img(src="upload.png",height = ICON_SIZE,width = ICON_SIZE,style="margin-top: 25px;display: block; margin-left: auto; margin-right: auto;")),
                                     column(10,h4(UI_LABELS$UPLOAD_VARDEF_LABEL,style="padding:20px;vertical-align:center;"),
                                            uiOutput('ui_load_var_def'))),
                            fluidRow(column(1,img(src="upload.png",height = ICON_SIZE,width = ICON_SIZE,style="margin-top: 25px;display: block; margin-left: auto; margin-right: auto;")),
                                     column(10,h4(UI_LABELS$UPLOAD_DATA_LABEL,style="padding:20px;vertical-align:center;"),
                                            uiOutput('ui_load_data'))),
                            fluidRow(column(1,img(src="disk.png",height = ICON_SIZE,width = ICON_SIZE,style="margin-top: 25px;display: block; margin-left: auto; margin-right: auto;")),
                                     column(10,h4(UI_LABELS$SAVE,style="padding:20px;vertical-align:center;"), downloadButton("button_Save",UI_LABELS$BUTTON_LABEL_SAVE))),
                            #fluidRow(HTML('</br>')),
                            
                            fluidRow(column(1,img(src="folder.png",height = ICON_SIZE,width = ICON_SIZE,style="margin-top: 25px; display: block; margin-left: auto; margin-right: auto;")),
                                     column(10,h4(UI_LABELS$LOAD,style="padding:20px;"),fileInput("button_Load",UI_LABELS$BUTTON_LABEL_LOAD))),
                            #fluidRow(HTML('</br>')),
                            
                            fluidRow(column(1,img(src="book.png",height = ICON_SIZE,width = ICON_SIZE,style="margin-top: 25px;display: block; margin-left: auto; margin-right: auto;")),
                                     column(10,h4(UI_LABELS$EXPORT,style="padding:20px;"),uiOutput('ui_export_trans_data'))),
                            #fluidRow(HTML('</br>')),
                            
                            fluidRow(column(1,img(src="checklist.png",height = ICON_SIZE,width = ICON_SIZE,style="margin-top: 25px;display: block; margin-left: auto; margin-right: auto;")),
                                     column(10,h4(UI_LABELS$EXPORT_TRANS,style="padding:20px;"),uiOutput('ui_export_trans_report')))
                          )  
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          fluidPage(
                            fluidRow(
                              DT::dataTableOutput("part.table"), title = 'Summary')
                            ,
                            fluidRow(
                              DT::dataTableOutput("full.table"), title = 'Output Variables')
                          )
                        )
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
      column(6,uiOutput('ui_Slider_BinSize')),
      column(6,htmlOutput('ui_Slider_KernelWidth'))
    )
  )# end of fluid page (upper toolbar, sidebar layout, advanced panel)
), #end of tabPanel - Transform


###**************************************
### Variable Definition window
###**************************************
tabPanel(UI_LABELS$TUTORIAL, 
         tags$iframe(style="height:900px; width:100%", src="Tutorial.pdf")
         ),


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
              h4('with additional code by Netta Shachar and Tal Kozlovski'),
              h4('github page: https://github.com/barakbri/AutoNeta'),
              h4(paste0('Version: ',VERSION_SERVER))
           )
)
         
) # end of navbarPage
) # end of ShinyUI