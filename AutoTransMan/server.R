library(shiny)

# definitions:
source('definitions.r')
#version:
VERSION_SERVER = '20170814'

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
    Version_Server = VERSION_SERVER,
    StatusLineString = STATUS_LINE_MSGS$INIT,
    ErrorLineString = "",
    
  # reference to files being loaded
    WorkSpaceFileName = NULL,
    workSpaceIsLoaded = F,
  
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
    isExcluded = NULL,
    isExcluded_Reason = NULL,
    
    VarDef_table = NULL,
    VarDef_label = NULL,
    VarDef_a = NULL,
    VarDef_b = NULL, 
    VarDef_type = NULL,
    VarDef_reverse = NULL,
  
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
    Graphs_RefreshInProgress = F,  
    Graphs_Nr_Displayed = NULL,
    Graphs_ggplot2_obj_list = NULL,
    Graphs_display_transformed_data = NULL,
    Graphs_display_transformed_yule = NULL,
    Graphs_Nr_Selected = -1,
  
  ## Sliders:
  
    Sliders_need_to_update = F,
    Slider_BinSize_current_value = NULL,
    Slider_BinSize_max_value = NULL,
    Slider_BinSize_min_value = NULL,
    
    Slider_KernelWidth_current_value = NULL,
    Slider_KernelWidth_max_value = NULL,
    Slider_KernelWidth_min_value = NULL
    
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
      SystemVariables$Graphs_RefreshInProgress = F
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
      
      SystemVariables$StatusLineString = STATUS_LINE_MSGS$TRANSFORMATION_APPLIED
    }
  })
  
  #observer - check if workspace has been loaded
  observe({
    
    inFile_WorkSpace = input$button_Load
    if(!SystemVariables$workSpaceIsLoaded & !is.null(inFile_WorkSpace) ){
      SystemVariables$WorkSpaceFileName = inFile_WorkSpace$datapath
      
      #Call Controller Logic Function for loading saved file
      Controller_LoadWorkSpace()
      
      SystemVariables$workSpaceIsLoaded = T
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
      SystemVariables$StatusLineString = STATUS_LINE_MSGS$DATA_LOADED
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
      SystemVariables$StatusLineString = STATUS_LINE_MSGS$VARDEF_LOADED
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
      if(!SystemVariables$Graphs_RefreshInProgress){
        SystemVariables$Graphs_RefreshInProgress = T
        Controller_VariableSelected()
        SystemVariables$Graphs_RefreshInProgress = F  
      }
    }
  })
  
  observe({
    
    if(!is.null(input$graphicalparameter_BinSize) & !is.null(SystemVariables$Slider_BinSize_current_value))
      if(SystemVariables$Slider_BinSize_current_value != input$graphicalparameter_BinSize){
        SystemVariables$Sliders_need_to_update = F
        SystemVariables$Graphs_RefreshNeeded = T
      }
    if(!is.null(input$graphicalparameter_KernelWidth) & !is.null(SystemVariables$Slider_KernelWidth_current_value))
      if(SystemVariables$Slider_KernelWidth_current_value != input$graphicalparameter_KernelWidth){
        SystemVariables$Sliders_need_to_update = F
        SystemVariables$Graphs_RefreshNeeded = T
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
    SystemVariables$Original_Yule = rep(NA,data_nvar) #abs(apply(SystemVariables$Data_Original,2,yuleIndex))
    SystemVariables$New_Yule = rep(NA,data_nvar) #SystemVariables$Original_Yule
    SystemVariables$hasBeenTransformed = rep(F,data_nvar)
    SystemVariables$isExcluded = rep(F,data_nvar)
    SystemVariables$isExcluded_Reason = rep("",data_nvar)
    SystemVariables$Transformation_Used = rep("None",data_nvar)
    SystemVariables$Transformation_Used_Index = rep(-1,data_nvar)
    SystemVariables$Data_Transformed = SystemVariables$Data_Original
    
    
    # check which dimensions are not castable
    for(i in 1:data_nvar){
      vec = SystemVariables$Data_Original[,i]
      is_numeric = is.numeric(vec)
      is_NA = is.na(vec)
      is_NaN = is.nan(vec)
      
      is_not_castable = (!is_numeric & !is_NaN & !is_NA)
      if(sum(is_not_castable)>0){
        SystemVariables$isExcluded[i] = T  
        SystemVariables$isExcluded_Reason[i] = UI_LABELS$EXCLUDE_VARIABLE_NOT_NUMERIC
      }
    }
    
    #cast NANs into NA's
    for(i in 1:data_nvar){
      if(!SystemVariables$isExcluded[i]){
        which_nan = which(is.nan(SystemVariables$Data_Original[,i]))
        if(length(which_nan>0)){
          SystemVariables$Data_Original[which_nan,i] = NA
        }  
      }
    }
    
    #at least three non NA
    for(i in 1:data_nvar){
      if(!SystemVariables$isExcluded[i]){
        nr_non_NA_values = sum(!is.na(SystemVariables$Data_Original[,i]))
        if(nr_non_NA_values<3){
          SystemVariables$isExcluded[i] = T
          SystemVariables$isExcluded_Reason[i] = UI_LABELS$EXCLUDE_VARIABLE_NOT_ENOUGH_VALUES
        }
      }
    }
    
    #now we can safely compute yule, but only for variables which have not been excluded
    for(i in 1:data_nvar){
      if(!SystemVariables$isExcluded[i]){
        SystemVariables$Original_Yule[i] = abs(yuleIndex(SystemVariables$Data_Original[,i]))
        SystemVariables$New_Yule[i] = SystemVariables$Original_Yule[i]
      }
    }
    
    
    
  }
  
  
  # load function used for loading variable defenitions:
  # load into vardef variables
  # if an error occured, display error message
  Controller_LoadVarDef = function(){
    SystemVariables$VarDef_label = (SystemVariables$VarDef_table$Variable)
    SystemVariables$VarDef_a = SystemVariables$VarDef_table$a
    SystemVariables$VarDef_b = SystemVariables$VarDef_table$b
    SystemVariables$VarDef_type = SystemVariables$VarDef_table$Type
    SystemVariables$VarDef_reverse = as.numeric(SystemVariables$VarDef_table$To.Reverse)
    
    #checks on vardef:
    if(any(!(SystemVariables$VarDef_type %in% VARIABLE_TYPES))){
      
    }
    
    if(any(!is.finite(as.numeric(SystemVariables$VarDef_a)))){
      
    }
    
    if(any(!is.finite(as.numeric(SystemVariables$VarDef_b)))){
      
    }
    
    if(sum(as.numeric(SystemVariables$VarDef_b)<as.numeric(SystemVariables$VarDef_b), na.rm = T )){
      
    }
    
    if(any(!(SystemVariables$VarDef_reverse %in% c(0,1)))){
      
      
    }
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
        if(SystemVariables$isExcluded[SystemVariables$BeforeList_Indices_of_var[i]]){
          SystemVariables$BeforeList_Labels[i]  = paste0(SystemVariables$BeforeList_Labels[i], UI_LABELS$LIST_EXCLUDED)  
        }else{
          SystemVariables$BeforeList_Labels[i]  = paste0(SystemVariables$BeforeList_Labels[i], "( Yule:", round(before_original_yules[i],3),")")  
        }
        
      }
      
    }
    
    
    SystemVariables$AfterList_Indices_of_var = ind_after
    SystemVariables$AfterList_HasBeenTransformed = SystemVariables$hasBeenTransformed[ind_after]
    SystemVariables$AfterList_Labels = colnames(SystemVariables$Data_Original)[ind_after]
    
    if(length(SystemVariables$AfterList_Labels)>0){
      for(i in 1:length(SystemVariables$AfterList_Labels)){
        if(SystemVariables$isExcluded[SystemVariables$AfterList_Indices_of_var[i]]){
          SystemVariables$AfterList_Labels[i] = paste0(SystemVariables$AfterList_Labels[i], UI_LABELS$LIST_EXCLUDED)
        }
      }  
    }
    
    
    
    #populate lists
    Controller_Update_BeforeList()
    Controller_Update_AfterList()
   

    SystemVariables$StatusLineString = paste0(UI_LABELS$LIST_REFRESH_MSG)
    SystemVariables$Lists_RefreshNeeded = F
  }
  
  Controller_Update_BeforeList = function(){
    updateSelectInput(session, "ui_list_Before",label = UI_LABELS$BEFORE_LIST,
                      choices = SystemVariables$BeforeList_Labels,selected = NULL
    )
  }
  
  Controller_Update_AfterList = function(){
    updateSelectInput(session, "ui_list_After", label = UI_LABELS$AFTER_LIST,
                      choices = SystemVariables$AfterList_Labels,selected = NULL
    )
  }
  
  #Controller function called on workspace load, handles the laoding from file
  # along with zeroizing of graphic variables, and recomputing lists
  Controller_LoadWorkSpace = function(){
    load(file = SystemVariables$WorkSpaceFileName) #=> save_list
    for(i in 1:length(FIELDS_TO_SAVE_LIST)){
      SystemVariables[[ FIELDS_TO_SAVE_LIST[i] ]] = save_list[[ FIELDS_TO_SAVE_LIST[i] ]]
    }
    if(SystemVariables$Version_Server != VERSION_SERVER){
      showModal(modalDialog(
        title = "Warning: Possible Save File Mismatch!",
        paste0('Server Version:',VERSION_SERVER,',',' Save File Version: ',SystemVariables$Version_Server)
      ))
    }
    #zeroize the reset of the memory space
    SystemVariables$ErrorLineString = ""
    SystemVariables$Lists_RefreshNeeded = F
    
    SystemVariables$BeforeList_Indices_of_var = NULL
    SystemVariables$BeforeList_Color_Code = NULL
    SystemVariables$BeforeList_OrderBy_Yule = F
    SystemVariables$BeforeList_Labels = NULL
    SystemVariables$BeforeList_IndexSelected = -1
    SystemVariables$BeforeList_HasFocus = F
    
    SystemVariables$AfterList_Indices_of_var = NULL
    SystemVariables$AfterList_HasBeenTransformed = NULL
    SystemVariables$AfterList_Labels = NULL
    SystemVariables$AfterList_IndexSelected = -1
    SystemVariables$AfterList_HasFocus = F
        
    SystemVariables$Variable_Selected = F
    SystemVariables$Variable_Selected_IndexOf = -1
    
    SystemVariables$Graphs_RefreshNeeded = F
    SystemVariables$Graphs_Nr_Displayed = NULL
    SystemVariables$Graphs_ggplot2_obj_list = NULL
    SystemVariables$Graphs_display_transformed_data = NULL
    SystemVariables$Graphs_display_transformed_yule = NULL
    SystemVariables$Graphs_Nr_Selected = -1
    
    SystemVariables$Sliders_need_to_update = F
    SystemVariables$Slider_BinSize_current_value = NULL
    SystemVariables$Slider_KernelWidth_current_value = NULL

    # recompute lists, also set them to be not selected
    Controller_ComputeList()
    
    SystemVariables$StatusLineString = STATUS_LINE_MSGS$WORKSPACE_LOAD
  }
  
  
  # Function for computation of tranformation, storing them in data etc
  # from current data state:
  # - get current index of selected from before or after lists/ need to find out
  # - if needed update sliders, else, get kernel width and bin size from sliders
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
      
      
      if(SystemVariables$Variable_Selected_IndexOf != SystemVariables$BeforeList_Indices_of_var[SystemVariables$BeforeList_IndexSelected]){
        SystemVariables$Sliders_need_to_update = T
      }else{
        #changes by the two modes of accessing this function, lists or slider
      }
      
      SystemVariables$Variable_Selected_IndexOf = SystemVariables$BeforeList_Indices_of_var[SystemVariables$BeforeList_IndexSelected]
            
      #remember to zeroize selection on the after list, otherwise we will not bet able to reselect it
      Controller_Update_AfterList()
      
    }
    if(SystemVariables$AfterList_HasFocus & SystemVariables$AfterList_IndexSelected!=-1){
      if(SystemVariables$Variable_Selected_IndexOf != SystemVariables$AfterList_Indices_of_var[SystemVariables$AfterList_IndexSelected]){
        SystemVariables$Sliders_need_to_update = T  
        
      }else{
        
      }
      
      SystemVariables$Graphs_Nr_Selected   =  SystemVariables$Transformation_Used_Index[SystemVariables$Variable_Selected_IndexOf]  
      SystemVariables$Variable_Selected_IndexOf = SystemVariables$AfterList_Indices_of_var[SystemVariables$AfterList_IndexSelected]
      
      SystemVariables$Variable_Selected =T
      
      
      #remember to zeroize selection on the before list, otherwise we will not bet able to reselect it
      Controller_Update_BeforeList()
    }
    
    
    #calling transformation functions, and storing results:
    ind_selected = SystemVariables$Variable_Selected_IndexOf
    ind_of_var_in_vardef = which(SystemVariables$VarDef_label == colnames(SystemVariables$Data_Original)[ind_selected])
    transformations_obj = NULL
    bin_type_parameter = NULL
    kernel_width_parameter = NULL
    
    if(SystemVariables$isExcluded[ind_selected]){
      SystemVariables$Graphs_ggplot2_obj_list = NULL
      SystemVariables$Graphs_display_transformed_data = NULL
      SystemVariables$Graphs_display_transformed_yule = NULL
      SystemVariables$Graphs_RefreshNeeded = F
      showModal(modalDialog(
        title = MSGS$MSG_VARIABLE_EXCLUDED_TITLE,
        paste0(MSGS$MSG_VARIABLE_EXCLUDED_BODY, '\n',
               SystemVariables$isExcluded_Reason[ind_selected])
      ))
      return(FALSE)
    }
    
    #do not take dependecy on slider varibles!
    isolate({
          SystemVariables$Slider_BinSize_current_value = input$graphicalparameter_BinSize
          SystemVariables$Slider_KernelWidth_current_value = input$graphicalparameter_KernelWidth
    })
    bin_type_parameter = input$graphicalparameter_BinSize
    kernel_width_parameter = input$graphicalparameter_KernelWidth
       
    
    if(length(ind_of_var_in_vardef) == 1){
      transformations_obj = tryCatch(
        wrapTypes(
          target.vec = SystemVariables$Data_Original[,ind_selected],
          type = as.character(SystemVariables$VarDef_type[ind_of_var_in_vardef]),
          a = SystemVariables$VarDef_a[ind_of_var_in_vardef],
          b = SystemVariables$VarDef_b[ind_of_var_in_vardef],
          to.reverse  = SystemVariables$VarDef_reverse[ind_of_var_in_vardef], 
          bin.width   = bin_type_parameter,
          window.size = kernel_width_parameter,
          var.name    = colnames(SystemVariables$Data_Original)[ind_selected],
          index.type = INDICES_FOR_ASYMMETRY$YULE
        ),error = function(e){SystemVariables$ErrorLineString = "Error in Transformations";NULL}
      )
      if(!is.null(transformations_obj)){
        SystemVariables$ErrorLineString = ''
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
    if(length(ind_of_var_in_vardef) != 1){
      SystemVariables$Graphs_ggplot2_obj_list = NULL
      SystemVariables$Graphs_display_transformed_data = NULL
      SystemVariables$Graphs_display_transformed_yule = NULL
      SystemVariables$Graphs_RefreshNeeded = F
      showModal(modalDialog(
        title = MSGS$MSG_VAR_NOT_IN_VARDEF_TITLE,
        MSGS$MSG_VAR_NOT_IN_VARDEF_BODY
      ))
    }
  }
  
  
  # get selected index
  # change transformed data to selected transformation
  # recompute list, reselect index from list
  Controller_TransformationApproved = function(index_of_button){
    
  }
  
  
  ###
  # download handlers for save and export
  ###
  output$button_Save <- downloadHandler(
    filename = function() {
      paste("workspace-", Sys.Date(), ".Rdata", sep="")
    },
    content = function(file) {
      save_list = isolate(reactiveValuesToList(SystemVariables))
      save(save_list,file = file)  
    }
  )
 
  
  output$button_Export <- downloadHandler(
    filename = function() {
      paste("data-export-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      if(!SystemVariables$Data_Is_Loaded || !SystemVariables$VarDef_Is_Loaded){
        showModal(modalDialog(
          title = MSGS$MSG_CANNOT_EXPORT_TITLE,
          MSGS$MSG_CANNOT_EXPORT_BODY
        ))
      }else{
        write.csv(SystemVariables$Data_Transformed, file = file,quote = F,row.names = F)  
      }
      
    }
  )
  
  output$button_ExportTransReport <- downloadHandler(
    filename = function() {
      paste("trans-report-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      if(!SystemVariables$Data_Is_Loaded || !SystemVariables$VarDef_Is_Loaded){
        showModal(modalDialog(
          title = MSGS$MSG_CANNOT_EXPORT_TITLE,
          MSGS$MSG_CANNOT_EXPORT_BODY
        ))
      }else{
        transformation_report = data.frame( variables = colnames(SystemVariables$Data_Original),
                                            Original_Yule = SystemVariables$Original_Yule,
                                            New_Yule = SystemVariables$New_Yule,
                                            hasBeenTransformed = SystemVariables$hasBeenTransformed,
                                            Transformation_Used = SystemVariables$Transformation_Used )
        write.csv(transformation_report, file = file,quote = F,row.names = F)  
      }
      
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
      actionButton("button_Apply", UI_LABELS$BUTTON_LABEL_APPLY_TRANSFORMATION)
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
    HTML("HELP PAGE HERE")
  })
  
  output$Page_About = renderUI({
    HTML("HELP ABOUT HERE")
  })

})


