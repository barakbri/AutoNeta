library(shiny)

#installing packages
#if(!("HHG" %in% rownames(installed.packages()))){
#  install.packages('HHG')
#}

# load source for transformation library:
 source('Function Project.R')


# Define server logic
shinyServer(function(input, output, session){
  
  ###
  # Variable Defenitions:
  ###
  SystemVariables <- reactiveValues(
    
    StatusLineString = "INIT",
    ErrorLineString = "",
    
  # Data/Model variables
    
    Data_File = NULL,
    Data_FileName = NULL,
    Data_Is_Loaded = F,
    Data_Is_Error = F,
    
    VarDef_File = NULL,
    VarDef_FileName = NULL,
    VarDef_Is_Loaded = F,
    VarDef_Is_Error = F,
  
    
  
  # Data variables
  
    Data_Original = NULL,
    Data_Transformed = NULL,
    Original_Yule = NULL,
    New_Yule = NULL,
    hasBeenTransformed = NULL,
    Transformation_Used = NULL,
  
    VarDef_table = NULL,
    VarDef_label = NULL,
    VarDef_a = NULL,
    VarDef_b = NULL, 
    VarDef_type = NULL,
  
  #Lists variables
  
    Lists_RefreshNeeded = F,  
  
    BeforeList_Indices_of_var = NULL,
    BeforeList_Color_Code = NULL,
    BeforeList_OrderBy_Yule = F,
    BeforeList_Labels = NULL,
    BeforeList_IndexSelected = -1,
    BeforeList_HasFocus = F,
  
    AfterList_Indices_of_var = NULL,
    AfterList_HasBeenTransformed = NULL,
    AfterList_Labels = NULL,
    AfterList_IndexSelected = -1,
    AfterList_HasFocus = F,
    
    Variable_Selected = F,
    Variable_Selected_IndexOf = -1,
  
  
  ## GGPLOT items
    
    Graphs_RefreshNeeded = F,  
    Graphs_Nr_Displayed = NULL,
    Graphs_ggplot2_obj_list = NULL,
    Graphs_display_transformed_data = NULL,
    Graphs_display_transformed_yule = NULL,
    Graphs_Nr_Selected = -1
  )
  
  ###
  # Definition of Reactive Observers
  ###
  
  # Observer - Refresh Lists, on any refresh needed
  observe({
    #Lists_RefreshNeeded
    if(SystemVariables$Lists_RefreshNeeded){
      Controller_ComputeList()
    }
  })
  
  # Observer - Refresh Lists, on yule checkbox change
  observe({
    #Lists_RefreshNeeded
    if('checkbox_sort_by_yule' %in% names(input))
      if(length(input$checkbox_sort_by_yule) >0)
        if(SystemVariables$BeforeList_OrderBy_Yule != input$checkbox_sort_by_yule){
          if(SystemVariables$Data_Is_Loaded  & SystemVariables$VarDef_Is_Loaded)
            SystemVariables$Lists_RefreshNeeded = T
        }
  })
  
  #Observer - check if data is loaded
  observe({
    #Check if Data is loaded
    
    #get current references to files
    inFile_Data <- input$file_Data
    inFile_VarDef <- input$file_VarDef
    
    
    
    # check if data has not been loaded, and can be loaded
    if(!SystemVariables$Data_Is_Loaded & !is.null(inFile_Data) ){
      
      SystemVariables$Data_Original = read.csv(inFile_Data$datapath)
      SystemVariables$Data_FileName = inFile_Data$datapath
      SystemVariables$Data_File = inFile_Data
      #need to implement checks on file structure
      
      #call initialization of data variables:
      Controller_LoadData() 
      
      SystemVariables$Data_Is_Loaded = T
      SystemVariables$StatusLineString = "Data Loaded"
    }
    
    
    # check if var def has not been loaded, and can be loaded
    if(!SystemVariables$VarDef_Is_Loaded & !is.null(inFile_VarDef)){
      SystemVariables$VarDef_table = read.csv(inFile_VarDef$datapath)
      SystemVariables$VarDef_FileName = inFile_VarDef$datapath
      SystemVariables$VarDef_File = inFile_VarDef
      #need to implement checks on file structure
      
      
      #call initialization of var def variables
      Controller_LoadVarDef()
      
      SystemVariables$VarDef_Is_Loaded = T
      SystemVariables$StatusLineString = "Variable Defenition Loaded"
    }
    
    if(SystemVariables$VarDef_Is_Loaded & SystemVariables$Data_Is_Loaded){
      SystemVariables$Lists_RefreshNeeded = T
    }
    
    
    #note:
    # need to handle different seperators and qoutes:
    #  inFile <- input$file1
    #  if (is.null(inFile))
    #    return(NULL)
    #  read.csv(inFile$datapath, header=input$header, sep=input$sep, 
    #           quote=input$quote)
    
    
  })
  
  #Observer -  Refresh Graphs, on any refresh needed (listens to change in variable selection)
  observe({
    current_index_before_list = -1
    current_index_after_list = -1
    #get current selected index of Before/After list
    if('ui_list_Before' %in% names(input))
      current_index_before_list = which(input$ui_list_Before == SystemVariables$BeforeList_Labels)
    if('ui_list_After' %in% names(input))
      current_index_after_list = which(input$ui_list_After == SystemVariables$AfterList_Labels)

    if(length(current_index_before_list)>0)
      if(SystemVariables$BeforeList_IndexSelected != current_index_before_list){
        SystemVariables$BeforeList_IndexSelected = current_index_before_list
        SystemVariables$Graphs_RefreshNeeded = T
        SystemVariables$BeforeList_HasFocus = T
        SystemVariables$AfterList_HasFocus = F
      }
    
    if(length(current_index_after_list)>0)
      if(SystemVariables$AfterList_IndexSelected != current_index_after_list){
        SystemVariables$AfterList_IndexSelected = current_index_after_list
        SystemVariables$Graphs_RefreshNeeded = T
        SystemVariables$BeforeList_HasFocus = F
        SystemVariables$AfterList_HasFocus = T
      }
    
    #Graphs_RefreshNeeded
    if(SystemVariables$Graphs_RefreshNeeded){
      Controller_VariableSelected()
    }
  })
  
  ###
  # Controller Logic
  ###
  
  # load function used for loading and checking data:
  # load into data transformed from  data original
  # compute all yule indices
  # if an error occured, display error
  Controller_LoadData = function(){
    
    data_nvar = ncol(SystemVariables$Data_Original)
    SystemVariables$Original_Yule = abs(apply(SystemVariables$Data_Original,2,yuleIndex))
    SystemVariables$New_Yule = SystemVariables$Original_Yule
    SystemVariables$hasBeenTransformed = rep(F,data_nvar)
    SystemVariables$Transformation_Used = rep("None",data_nvar)
    SystemVariables$Data_Transformed = SystemVariables$Data_Original
    
  }
  
  
  # load function used for loading variable defenitions:
  # load into vardef variables
  # if an error occured, display error message
  Controller_LoadVarDef = function(){
    SystemVariables$VarDef_File
    VarDef_label = (SystemVariables$VarDef_File$Variable)
    VarDef_a = SystemVariables$VarDef_File$a
    VarDef_b = SystemVariables$VarDef_File$b
    VarDef_type = SystemVariables$VarDef_File$type
  }
  
  
  # Function for computation of list variables,
  # from current data state:
  # generate lists of vars, colors, labels,
  # order lists
  # populate lists
  Controller_ComputeList = function(){
    SystemVariables$BeforeList_OrderBy_Yule  = input$checkbox_sort_by_yule
    ind_before = which(SystemVariables$hasBeenTransformed == F)
    ind_after = which(SystemVariables$hasBeenTransformed == T)
    
    SystemVariables$BeforeList_Indices_of_var = ind_before
    SystemVariables$BeforeList_Labels = colnames(SystemVariables$Data_Original)[ind_before]
    
    if(SystemVariables$BeforeList_OrderBy_Yule == T){
      #need to do sort
      before_original_yules = SystemVariables$Original_Yule[ind_before]
      ord = order(before_original_yules,decreasing = T)
      SystemVariables$BeforeList_Indices_of_var = SystemVariables$BeforeList_Indices_of_var[ord]  
      SystemVariables$BeforeList_Labels = SystemVariables$BeforeList_Labels[ord]
      before_original_yules = before_original_yules[ord]
      
      for(i in 1:length(ord)){
        SystemVariables$BeforeList_Labels[i]  = paste0(SystemVariables$BeforeList_Labels[i], "(yule:", before_original_yules[i],")")
      }
      
    }
    
    
    SystemVariables$AfterList_Indices_of_var = ind_after
    SystemVariables$AfterList_HasBeenTransformed = SystemVariables$hasBeenTransformed[ind_after]
    AfterList_Labels = colnames(SystemVariables$Data_Original)[ind_after]
    
    
    #populate lists
    updateSelectInput(session, "ui_list_Before",label = "Before:",
                      choices = SystemVariables$BeforeList_Labels,selected = 1
    )
    
    updateSelectInput(session, "ui_list_After", label = "After:",
                      choices = SystemVariables$AfterList_Labels,selected = 1
    )

    SystemVariables$StatusLineString = paste0("List Drawn at:  ",as.character(Sys.time()))
    SystemVariables$Lists_RefreshNeeded = F
  }
  
  
  # Function for computation of tranformation, storing them in data etc
  # from current data state:
  # - by var type select list of transformations
  # - compute transformations + yule
  # - order them by yule improvments
  # - put in display variables
  Controller_VariableSelected = function(){
    if(SystemVariables$BeforeList_HasFocus & SystemVariables$BeforeList_IndexSelected!=-1){
      SystemVariables$Variable_Selected = T
      SystemVariables$Variable_Selected_IndexOf = SystemVariables$BeforeList_Indices_of_var[SystemVariables$BeforeList_IndexSelected]
    }
    if(SystemVariables$AfterList_HasFocus & SystemVariables$AfterList_IndexSelected!=-1){
      SystemVariables$Variable_Selected =T
      SystemVariables$Variable_Selected_IndexOf = SystemVariables$AfterList_Indices_of_var[SystemVariables$AfterList_IndexSelected]
    }
    
    
    
    SystemVariables$StatusLineString = paste0(
      colnames(SystemVariables$Data_Original)[SystemVariables$Variable_Selected_IndexOf],
      " selected")
    
  }
  
  
  # get selected index
  # change transformed data to selected transformation
  # recompute list, reselect index from list
  Controller_TransformationApproved = function(index_of_button){
    
  }
  
  ###
  # Renderers:
  ###
  
  # Graph Renderers:
  output$output_Graph_1 = renderPlot({plot(1:100,1:100)})
  
  output$output_Graph_2 = renderPlot({plot(1:100,1:100)})
  
  output$output_Graph_3 = renderPlot({plot(1:100,1:100)})
  
  output$output_Graph_4 = renderPlot({plot(1:100,1:100)})
  
  output$output_Graph_5 = renderPlot({plot(1:100,1:100)})
  
  output$output_Graph_6 = renderPlot({plot(1:100,1:100)})
  
  # Select Button Renderers:
  #Display buttons if graph is present and varible selected
  
  #Status Display Renderer
  #Used Also for checking of uploads
  output$StatusLine = renderText({ 
    SystemVariables$StatusLineString
  })
  
  
  ###
  # Click Handlers
  ###
  
  # Handle Approve Transformation Selection (handlers_per_button)
  
  
  
})
