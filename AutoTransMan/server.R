library(shiny)

FIELDS_TO_SAVE_LIST = c('Data_File','Data_FileName','Data_Is_Loaded','Data_Is_Error',
                        'VarDef_File','VarDef_FileName','VarDef_Is_Loaded','VarDef_Is_Error',
                        'Data_Original','Data_Transformed','Original_Yule','New_Yule',
                        'hasBeenTransformed','Transformation_Used','Transformation_Used_Index',
                        'VarDef_table','VarDef_label','VarDef_a','VarDef_b','VarDef_type')

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
    Transformation_Used_Index = NULL,
  
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
  
  # Observer for button click
  observeEvent(input$click_Graph_1,{handle_Click(1)})
  observeEvent(input$click_Graph_2,{handle_Click(2)})
  observeEvent(input$click_Graph_3,{handle_Click(3)})
  observeEvent(input$click_Graph_4,{handle_Click(4)})
  observeEvent(input$click_Graph_5,{handle_Click(5)})
  observeEvent(input$click_Graph_6,{handle_Click(6)})
  
  #observer - when the apply transformation button is clicked
  observeEvent(input$button_Apply,{
    if('button_Apply' %in% names(input)){
      nr_trans_selected = SystemVariables$Graphs_Nr_Selected
      nr_var_viewed = SystemVariables$Variable_Selected_IndexOf
      SystemVariables$Data_Transformed[,nr_var_viewed] = SystemVariables$Graphs_display_transformed_data[[nr_trans_selected]]
      SystemVariables$New_Yule[nr_var_viewed] = SystemVariables$Graphs_display_transformed_yule[[1]][nr_trans_selected]
      SystemVariables$Transformation_Used[nr_var_viewed] = names(SystemVariables$Graphs_display_transformed_yule[[1]])[nr_trans_selected]
      SystemVariables$Transformation_Used_Index[nr_var_viewed] = nr_trans_selected
      SystemVariables$hasBeenTransformed[nr_var_viewed] = T
      
      
      
      #zeroize graph display
        
      SystemVariables$Graphs_RefreshNeeded = F
      SystemVariables$Graphs_Nr_Displayed = NULL
      SystemVariables$Graphs_ggplot2_obj_list = NULL
      SystemVariables$Graphs_display_transformed_data = NULL
      SystemVariables$Graphs_display_transformed_yule = NULL
      SystemVariables$Graphs_Nr_Selected = -1
      
      SystemVariables$Variable_Selected = F
      SystemVariables$Variable_Selected_IndexOf = -1
      
      SystemVariables$BeforeList_IndexSelected = -1
      SystemVariables$AfterList_IndexSelected = 1
      SystemVariables$BeforeList_HasFocus = F
      SystemVariables$AfterList_HasFocus = T
      
      #refresh lists
      
      Controller_ComputeList()
      Controller_VariableSelected()
      
      #SystemVariables$Lists_RefreshNeeded  = T # call for a refresh of lists
      SystemVariables$StatusLineString = "Transformation Applied"
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
    
    #temp = s
    
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
    SystemVariables$Transformation_Used_Index = rep(-1,data_nvar)
    SystemVariables$Data_Transformed = SystemVariables$Data_Original
    
  }
  
  
  # load function used for loading variable defenitions:
  # load into vardef variables
  # if an error occured, display error message
  Controller_LoadVarDef = function(){
    SystemVariables$VarDef_label = (SystemVariables$VarDef_table$Variable)
    SystemVariables$VarDef_a = SystemVariables$VarDef_table$a
    SystemVariables$VarDef_b = SystemVariables$VarDef_table$b
    SystemVariables$VarDef_type = SystemVariables$VarDef_table$Type
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
        SystemVariables$BeforeList_Labels[i]  = paste0(SystemVariables$BeforeList_Labels[i], "(yule:", round(before_original_yules[i],3),")")
      }
      
    }
    
    
    SystemVariables$AfterList_Indices_of_var = ind_after
    SystemVariables$AfterList_HasBeenTransformed = SystemVariables$hasBeenTransformed[ind_after]
    SystemVariables$AfterList_Labels = colnames(SystemVariables$Data_Original)[ind_after]
    
    
    #populate lists
    updateSelectInput(session, "ui_list_Before",label = "Before:",
                      choices = SystemVariables$BeforeList_Labels,selected = NULL
    )
    
    updateSelectInput(session, "ui_list_After", label = "After:",
                      choices = SystemVariables$AfterList_Labels,selected = NULL
    )

    SystemVariables$StatusLineString = paste0("List Drawn at:  ",as.character(Sys.time()))
    SystemVariables$Lists_RefreshNeeded = F
  }
  
  
  # Function for computation of tranformation, storing them in data etc
  # from current data state:
  # - get current index of selected from before or after lists/ need to find out
  # - call transofrmation manager:
  #     by var type select list of transformations
  #     compute transformations + yule
  #     order them by yule improvments
  # - put in display variables
  # - zeroize, selection and redraw falgs.
  # - put label in status bar
  Controller_VariableSelected = function(){
    

    if(SystemVariables$BeforeList_HasFocus & SystemVariables$BeforeList_IndexSelected!=-1){
      SystemVariables$Variable_Selected = T
      SystemVariables$Variable_Selected_IndexOf = SystemVariables$BeforeList_Indices_of_var[SystemVariables$BeforeList_IndexSelected]
      #remember to zeroize selection on the before list, otherwise we will not bet able to reselect it
      updateSelectInput(session, "ui_list_After", label = "After:",
                        choices = SystemVariables$AfterList_Labels,selected = NULL
      )
    }
    if(SystemVariables$AfterList_HasFocus & SystemVariables$AfterList_IndexSelected!=-1){
      SystemVariables$Variable_Selected =T
      SystemVariables$Variable_Selected_IndexOf = SystemVariables$AfterList_Indices_of_var[SystemVariables$AfterList_IndexSelected]
      SystemVariables$Graphs_Nr_Selected   =  SystemVariables$Transformation_Used_Index[SystemVariables$Variable_Selected_IndexOf]
      
      #remember to zeroize selection on the before list, otherwise we will not bet able to reselect it
      updateSelectInput(session, "ui_list_Before",label = "Before:",
                        choices = SystemVariables$BeforeList_Labels,selected = NULL
      )
      
    }
    
    #calling transformation functions, and storing results:
    ind_selected = SystemVariables$Variable_Selected_IndexOf
    transformations_obj = NULL
    transformations_obj = try(wrapTypes(
      SystemVariables$Data_Original[,ind_selected],
      as.character(SystemVariables$VarDef_type[ind_selected]),
      SystemVariables$VarDef_a[ind_selected],
      SystemVariables$VarDef_b[ind_selected]))
    if(!is.null(transformations_obj)){
      transformation_names = names(transformations_obj$Transformations$Transformations)
      #save into System variables
      SystemVariables$Graphs_ggplot2_obj_list  = transformations_obj$Plots
      SystemVariables$Graphs_display_transformed_data  = transformations_obj$Transformations$Transformations
      SystemVariables$Graphs_display_transformed_yule   = transformations_obj$Transformations$`Yule Index` 
      
      #no graph selected after redraw
      if(SystemVariables$BeforeList_HasFocus)
      SystemVariables$Graphs_Nr_Selected = -1
      
      SystemVariables$Graphs_RefreshNeeded = F
      
      SystemVariables$StatusLineString = paste0(
        colnames(SystemVariables$Data_Original)[SystemVariables$Variable_Selected_IndexOf],
        " selected")  
    }else{
      SystemVariables$StatusLineString = ""
      SystemVariables$ErrorLineString = paste0("Cannot transform ",colnames(SystemVariables$Data_Original)[SystemVariables$Variable_Selected_IndexOf])
    }
    
    
  }
  
  
  # get selected index
  # change transformed data to selected transformation
  # recompute list, reselect index from list
  Controller_TransformationApproved = function(index_of_button){
    
  }
  
  
  ###
  # state handlers - download and upload
  ###
  output$button_Save <- downloadHandler(
    filename = function() {
      paste("workspace-", Sys.Date(), ".Rdata", sep="")
    },
    content = function(file) {
      save_list = isolate(reactiveValuesToList(SystemVariables))
      save(save_list,file = file)  
      #dt = data.frame(Variable = names(SystemVariables),Value = NA)
      #for(i in 1:nrow(dt)){
      #  dt$Value[i] = SystemVariables[[dt$Variable[i]]]
      #}
      #write.csv(dt, file,quote = F,row.names = F)
      #if(!is.null(file)){
      #  save_list = list()
      #  for(i in 1:length(FIELDS_TO_SAVE_LIST)){
      #    save_list[[ FIELDS_TO_SAVE_LIST[i] ]] = SystemVariables[ FIELDS_TO_SAVE_LIST[i] ]
      #  }
      #  save(save_list,file = file)  
      #}
      
    }
  )
 
  output$button_Export <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data, file)
    }
  )
  
  output$button_ExportTransReport <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data, file)
    }
  )
  
  ###
  # Renderers:
  ###
  
  # Graph Renderers:
  output$output_Graph_1 = renderPlot({get_plot(1)})
  
  output$output_Graph_2 = renderPlot({get_plot(2)})
  
  output$output_Graph_3 = renderPlot({get_plot(3)})
  
  output$output_Graph_4 = renderPlot({get_plot(4)})
  
  output$output_Graph_5 = renderPlot({get_plot(5)})
  
  output$output_Graph_6 = renderPlot({get_plot(6)})
  
  get_plot = function(index){
    return_obj = ggplot()+theme_light()
    return_graph = F
    if(SystemVariables$Variable_Selected)
      if(!is.null(SystemVariables$Graphs_ggplot2_obj_list))
        if(length(SystemVariables$Graphs_ggplot2_obj_list )>=index)
          return_graph = T
    if(return_graph){
      return_obj = SystemVariables$Graphs_ggplot2_obj_list[[index]]
      if(SystemVariables$Graphs_Nr_Selected == index){
        # need to add selection
        return_obj = return_obj + theme(panel.border = element_rect(
                                          size = 3,
                                          colour = "red",
                                          fill = NA
                                        ))
      }
    }
    return(return_obj)
  }
  
  # Select Button Renderers:
  #Display buttons if graph is present and transformation has been selected
  output$button_Apply = renderUI({
    if (SystemVariables$Graphs_Nr_Selected > 0){
      actionButton("button_Apply", "Apply Transformation")
    }
  })
 
  #Status Display Renderer
  output$StatusLine = renderText({ 
    s = SystemVariables$StatusLineString
    if(length(SystemVariables$ErrorLineString) > 1){
     s = paste0(s," , Error: ",SystemVariables$ErrorLineString) 
    }
    s
  })
  
  
  ###
  # Click Handlers
  ###
  
  # Handle Approve Transformation Selection (handlers_per_button)
  
  handle_Click = function(index){
    if(length(SystemVariables$Graphs_ggplot2_obj_list) >= index)
      SystemVariables$Graphs_Nr_Selected  = index
  }
  
  ###
  # Additional Pages & Actions
  ###
  
  
  output$Page_Help = renderUI({
    renderUI("HELP PAGE HERE")
  })
  
  output$Page_About = renderUI({
    renderUI("HELP PAGE HERE")
  })
  
  
  
})
