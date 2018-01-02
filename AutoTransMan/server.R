## Libraries 
library(shiny)
library(MASS)
# definitions:
source('definitions.r')


# load source for transformation library:
 source('Function Project.R')


# Define server logic
shinyServer(function(input, output, session){
  
  ###
  # Variable Defenitions:
  ###
  SystemVariables <- reactiveValues(
    
    Version_Server = VERSION_SERVER, #Version for server, taken on load
    StatusLineString = STATUS_LINE_MSGS$INIT, #status line string
    ErrorLineString = "", #used for displaying error messages
    
  # reference to files being loaded
    WorkSpaceFileName = NULL,
    workSpaceIsLoaded = F,
  
  # Data/Model variables - This section is a part of the 'Model' part of the program
  # this part is saved and loaded on workspace save/load actions (except for XXX_FILE fields)
    Data_File = NULL,
    Data_FileName = NULL,
    Data_Is_Loaded = F,
    Data_Is_Error = F,
    
    VarDef_File = NULL,
    VarDef_FileName = NULL,
    VarDef_Is_Loaded = F,
    VarDef_Is_Error = F,
    
    
  
  # Data variables - This section is a part of the 'Model' part of the program
  # this part is saved and loaded on workspace save/load actions
    Data_Original = NULL,
    Data_Type = environment(),
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
    
    # used to hold the table for auto-generated variable definitions
    VarDef_Guess = NULL, 
  
  #Lists variables  - This section is used to update the viewer (what the lists holds)
  # also contains controller variables: indicators for if refresh and selection
  
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
   # contains the list of displayed graphs, along with a flag (lock) if plot is in progress.
   # contains the list of current transformations displayed, along with their recomputed yule
    
    Graphs_RefreshNeeded = F,
    Graphs_RefreshInProgress = F,  
    Graphs_Nr_Displayed = NULL,
    Graphs_ggplot2_obj_list = NULL,
    Graphs_display_transformed_data = NULL,
    Graphs_display_transformed_yule = NULL,
    Graphs_Nr_Selected = -1,
  
  ## Sliders:
  
    Sliders_need_to_update = F, # deprecated? I think this could be removed
    Slider_BinSize_current_value = NULL, #current displayed value of slider - used to check if a change has occured
    Slider_BinSize_max_value = NULL,# deprecated? I think this could be removed
    Slider_BinSize_min_value = NULL,# deprecated? I think this could be removed
    
    Slider_KernelWidth_current_value = NULL, #current displayed value of slider - used to check if a change has occured
    Slider_KernelWidth_max_value = NULL,# deprecated? I think this could be removed
    Slider_KernelWidth_min_value = NULL# deprecated? I think this could be removed
    
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
      #store the current transformation and values (yule, trans number and name)
      #for the variable viewed
      nr_trans_selected = SystemVariables$Graphs_Nr_Selected
      nr_var_viewed = SystemVariables$Variable_Selected_IndexOf
      SystemVariables$Data_Transformed[,nr_var_viewed] = SystemVariables$Graphs_display_transformed_data[[nr_trans_selected]]
      SystemVariables$New_Yule[nr_var_viewed] = SystemVariables$Graphs_display_transformed_yule[nr_trans_selected]
      SystemVariables$Transformation_Used[nr_var_viewed] = names(SystemVariables$Graphs_display_transformed_yule)[nr_trans_selected]
      SystemVariables$Transformation_Used_Index[nr_var_viewed] = nr_trans_selected
      SystemVariables$hasBeenTransformed[nr_var_viewed] = T
      
      
      
      #zeroize graph display

       SystemVariables$Graphs_RefreshNeeded = F
       SystemVariables$Graphs_RefreshInProgress = F
       SystemVariables$Graphs_Nr_Displayed = NULL
       SystemVariables$Graphs_ggplot2_obj_list = NULL
       SystemVariables$Graphs_display_transformed_data = NULL
       SystemVariables$Graphs_display_transformed_yule = NULL
      
       
       #refresh lists
       Controller_ComputeList()
       
       #show the top of the before list
       #SystemVariables$BeforeList_IndexSelected = 1 #This can be used for resetting the before list after selection to the top item
       SystemVariables$AfterList_IndexSelected = -1
       SystemVariables$BeforeList_HasFocus = F
       if(length(SystemVariables$BeforeList_Labels)>0)  
        SystemVariables$BeforeList_HasFocus = T
       SystemVariables$AfterList_HasFocus = F
       
       SystemVariables$Variable_Selected = T
       SystemVariables$Variable_Selected_IndexOf = SystemVariables$BeforeList_Indices_of_var[1]
       SystemVariables$Graphs_Nr_Selected = -1
       
       #select the top item in the before list
       Controller_Update_BeforeList(1)
       Controller_Update_AfterList()
       
       #replot - but only if there are remaining in the before list
       if(length(SystemVariables$BeforeList_Labels)>0)
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
    
    
    current_index_before_list = -1
    current_index_after_list = -1
    #get current selected index of Before/After list
    if('ui_list_Before' %in% names(input))
      current_index_before_list = which(input$ui_list_Before == SystemVariables$BeforeList_Labels)
    if('ui_list_After' %in% names(input))
      current_index_after_list = which(input$ui_list_After == SystemVariables$AfterList_Labels)

    #check for a change on the before list
    if(length(current_index_before_list)>0)
      if(SystemVariables$BeforeList_IndexSelected != current_index_before_list){
        SystemVariables$BeforeList_IndexSelected = current_index_before_list
        SystemVariables$Graphs_RefreshNeeded = T
        SystemVariables$BeforeList_HasFocus = T
        SystemVariables$AfterList_HasFocus = F
      }
    
    #check for a change on the after list
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
    
    ####### Wrote Here 
    ## Summary dashboard
    
    output$part.table <- DT::renderDataTable({
      ind <- SystemVariables$AfterList_Indices_of_var 
      if (length(ind) > 0) {
      # var.ind.vardef <- which(SystemVariables$VarDef_label == colnames(SystemVariables$Data_Original)[ind])
      # 'New Yule' = SystemVariables$New_Yule[ind],
      # 'Old Yule' = SystemVariables$Original_Yule[ind],
      # 
      
      var.trans.tab  <- table('Var Type' = SystemVariables$VarDef_type[ind],
                              'Transform' = SystemVariables$Transformation_Used[ind])
      left.var.sum  <- table('Left Variables' = SystemVariables$VarDef_type[-ind])
      
      temp.tab      <- data.frame(cbind(var.trans.tab, 
                                        'Left Variables' =left.var.sum[match(rownames(var.trans.tab), 
                                                                             rownames(left.var.sum))]))
      data      <- data.frame(temp.tab, 'Total' = apply(temp.tab, 1, sum, na.rm = T))
      DT::datatable(data, caption = 'Summary')
      }
    }
    ) 
    
    output$full.table <- DT::renderDataTable({
      ind <- SystemVariables$AfterList_Indices_of_var
      if (length(ind) > 0) {
      data <- data.frame('Name'= gsub("^(.*?),.*", "\\1", SystemVariables$AfterList_Labels),
                         'Var Type' = SystemVariables$VarDef_type[ind],
                         'Transform' = SystemVariables$Transformation_Used[ind],
                         'Old Yule' = round(SystemVariables$Original_Yule[ind], 3),
                         'New Yule' = round(SystemVariables$New_Yule[ind], 3)) 
      DT::datatable(data, caption = 'Transformed Variables')
      }
    }
    )
  })
  
  #handle a replot, on a change of sliders
  observe({
    #check for change on the Bin Size Slider
    if(!is.null(input$graphicalparameter_BinSize) & !is.null(SystemVariables$Slider_BinSize_current_value))
      if(SystemVariables$Slider_BinSize_current_value != input$graphicalparameter_BinSize){
        #SystemVariables$Sliders_need_to_update = F
        SystemVariables$Graphs_RefreshNeeded = T
      }
    #check for change on the Kernel Width Slider
    if(!is.null(input$graphicalparameter_KernelWidth) & !is.null(SystemVariables$Slider_KernelWidth_current_value))
      if(SystemVariables$Slider_KernelWidth_current_value != input$graphicalparameter_KernelWidth){
        #SystemVariables$Sliders_need_to_update = F
        SystemVariables$Graphs_RefreshNeeded = T
      }
    
  })
  
  ###
  # Controller Logic
  ###
  
  # load function used for loading and checking data:
  # load into data transformed from  data original
  # compute all yule indices
  # exclude variables which are not purely numeric or have less than three numeric values
  # converts NaNs to NAs
  Controller_LoadData = function(){
    
    #init model variables
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
    
    #Check variable types - display MSG on problem
    if(any(!(SystemVariables$VarDef_type %in% VARIABLE_TYPES))){
      showModal(modalDialog(
        title = MSGS$MSG_VARDEF_CHECK_TYPENOTFOUND_TITLE,
        MSGS$MSG_VARDEF_CHECK_TYPENOTFOUND_BODY
      ))
    }
    
    #Check a's - to be numeric
    if(any(!is.finite(as.numeric(SystemVariables$VarDef_a)))){
      showModal(modalDialog(
        title = MSGS$MSG_VARDEF_CHECK_NOT_NUMERIC_A_TITLE,
        MSGS$MSG_VARDEF_CHECK_NOT_NUMERIC_A_BODY
      ))
      SystemVariables$VarDef_Is_Error  = T 
    }
    
    #Check b's - to be numeric
    if(any(!is.finite(as.numeric(SystemVariables$VarDef_b)))){
      showModal(modalDialog(
        title = MSGS$MSG_VARDEF_CHECK_NOT_NUMERIC_B_TITLE,
        MSGS$MSG_VARDEF_CHECK_NOT_NUMERIC_B_BODY
      ))
      SystemVariables$VarDef_Is_Error  = T 
    }
    
    #check b>a
    if(sum(as.numeric(SystemVariables$VarDef_b)<as.numeric(SystemVariables$VarDef_b), na.rm = T )){
      showModal(modalDialog(
        title = MSGS$MSG_VARDEF_CHECK_NOT_NUMERIC_ABORDER_TITLE,
        MSGS$MSG_VARDEF_CHECK_NOT_NUMERIC_ABORDER_BODY
      ))
      SystemVariables$VarDef_Is_Error  = T 
    }
    
    #check to.reverse is in c(0,1)
    if(any(!(SystemVariables$VarDef_reverse %in% c(0,1)))){
      showModal(modalDialog(
        title = MSGS$MSG_VARDEF_CHECK_NOT_NUMERIC_TOREVERSE01_TITLE,
        MSGS$MSG_VARDEF_CHECK_NOT_NUMERIC_TOREVERSE01_BODY
      ))
      SystemVariables$VarDef_Is_Error  = T 
    }
  }
  
  # Function for computation of list variables,
  # from current data state:
  # generate lists of vars, labels,
  # order lists
  # populate lists
  # labels contain the yule value for the before list, and
  # chosen transformation of the after list
  Controller_ComputeList = function(){
    SystemVariables$BeforeList_OrderBy_Yule  = input$checkbox_sort_by_yule
    ind_before = which(SystemVariables$hasBeenTransformed == F)
    ind_after = which(SystemVariables$hasBeenTransformed == T)
    
    #organize variables types in hashlist
    vec_of_names = colnames(SystemVariables$Data_Original)
    for(i in 1:length(vec_of_names)){
      if(is.null(SystemVariables$Data_Type[[vec_of_names[i] ]])){
        ind_of_type = which(as.character(SystemVariables$VarDef_label) == vec_of_names[i])
        if(length(ind_of_type)>0){
          SystemVariables$Data_Type[[vec_of_names[i] ]] = SystemVariables$VarDef_type[ind_of_type[1]] #can we encounter a case of more than one?
        }
      }
    }
    
    SystemVariables$BeforeList_Indices_of_var = ind_before
    if(length(SystemVariables$BeforeList_Indices_of_var) >= 0){
      
    }
    SystemVariables$BeforeList_Labels = colnames(SystemVariables$Data_Original)[ind_before]
    
    if(length(ind_before)>0){
      for(i in 1:length(ind_before)){
        if(!is.null(SystemVariables$Data_Type[[ SystemVariables$BeforeList_Labels[i] ]])){
          SystemVariables$BeforeList_Labels[i] = paste0(SystemVariables$BeforeList_Labels[i], ', ',SystemVariables$Data_Type[[SystemVariables$BeforeList_Labels[i] ]],' ')    
        }
        
      }
      
      #order by yule indexon the before list, if needed
      if(SystemVariables$BeforeList_OrderBy_Yule == T){
        #need to do sort
        before_original_yules = SystemVariables$Original_Yule[ind_before]
        before_original_yules_order_by = before_original_yules
        #here we can reorder groups, for example,
        #show binary variables at the end of the list
        
        binary_vars = list()
        for(i in 1:ncol(SystemVariables$Data_Original)){
          current_name = SystemVariables$Data_Type[[colnames(SystemVariables$Data_Original)[i] ]]
          if(!is.null(current_name)){
            if((current_name %in% c("Binary (categories)","Category") |
                length(unique(SystemVariables$Data_Original[,i]))<=2))
              binary_vars[[length(binary_vars) + 1]] = i
          }
        }
        binary_vars = unlist(binary_vars)
        
        #binary_vars = which(SystemVariables$VarDef_type[ind_before] %in%
        #                      c("Binary (categories)","Category") |
        #                      abs(abs(before_original_yules) - 1) <= 10^(-4))
        if(length(binary_vars) > 0)
          before_original_yules_order_by[binary_vars] = -Inf
        
        
        ord = order(before_original_yules_order_by,decreasing = T)
        
        
        SystemVariables$BeforeList_Indices_of_var = SystemVariables$BeforeList_Indices_of_var[ord]  
        SystemVariables$BeforeList_Labels = SystemVariables$BeforeList_Labels[ord]
        before_original_yules = before_original_yules[ord]
        
        for(i in 1:length(ord)){
          SystemVariables$BeforeList_Labels[i]  = paste0(SystemVariables$BeforeList_Labels[i], "(Abs. Yule:", round(before_original_yules[i],3),")")  
        }
        
      }
      
      #add type - excluded
      for(i in 1:length(SystemVariables$BeforeList_Labels)){
        if(!is.na(SystemVariables$BeforeList_Indices_of_var[i]))
          if(SystemVariables$isExcluded[SystemVariables$BeforeList_Indices_of_var[i]]){
            SystemVariables$BeforeList_Labels[i]  = paste0(SystemVariables$BeforeList_Labels[i], UI_LABELS$LIST_EXCLUDED)  
          }
      }
    } # end of check if ind before is of length >0
      
    
    
    SystemVariables$AfterList_Indices_of_var = ind_after
    SystemVariables$AfterList_HasBeenTransformed = SystemVariables$hasBeenTransformed[ind_after]
    SystemVariables$AfterList_Labels = colnames(SystemVariables$Data_Original)[ind_after]
    
    if(length(ind_after))
      for(i in 1:length(ind_after)){
        if(!is.null(SystemVariables$Data_Type[[ SystemVariables$AfterList_Labels[i] ]])){
          SystemVariables$AfterList_Labels[i] = paste0(SystemVariables$AfterList_Labels[i], ', ',SystemVariables$Data_Type[[ SystemVariables$AfterList_Labels[i] ]],' ')            
        }

      }
    
    #add transformation labels, and if excluded
    if(length(SystemVariables$AfterList_Labels)>0){
      for(i in 1:length(SystemVariables$AfterList_Labels)){
        ind_of_var = SystemVariables$AfterList_Indices_of_var[i]
        SystemVariables$AfterList_Labels[i] = paste0(SystemVariables$AfterList_Labels[i],
                                                     ' - ',
                                                     SystemVariables$Transformation_Used[ind_of_var],
                                                     ' (Abs Yule -> Yule: ',
                                                     round(SystemVariables$Original_Yule[ind_of_var],3),
                                                     ' -> ',
                                                     round(SystemVariables$New_Yule[ind_of_var],3),')')
        
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
  
  #wrapper for updating the 'before' list according to system variables
  Controller_Update_BeforeList = function(selected_item = NULL){
    updateSelectInput(session, "ui_list_Before",label = UI_LABELS$BEFORE_LIST,
                      choices = SystemVariables$BeforeList_Labels,selected = selected_item
    )
  }
  
  #wrapper for updating the 'after' list according to system variables
  Controller_Update_AfterList = function(selected_item = NULL){
    updateSelectInput(session, "ui_list_After", label = UI_LABELS$AFTER_LIST,
                      choices = SystemVariables$AfterList_Labels,selected = selected_item
    )
  }
  
  #Controller function called on workspace load, handles the laoding from file
  # along with zeroizing of graphic variables, and recomputing lists
  Controller_LoadWorkSpace = function(){
    load(file = SystemVariables$WorkSpaceFileName) #=> save_list
    for(i in 1:length(FIELDS_TO_SAVE_LIST)){
      SystemVariables[[ FIELDS_TO_SAVE_LIST[i] ]] = save_list[[ FIELDS_TO_SAVE_LIST[i] ]]
    }
    #check version
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
    #check which if the two lists has caused the change
    #note: it could be that a slider, and not lists caused the redraw
    if(SystemVariables$BeforeList_HasFocus & SystemVariables$BeforeList_IndexSelected!=-1){
      SystemVariables$Variable_Selected = T
      if(SystemVariables$BeforeList_IndexSelected<=length(SystemVariables$BeforeList_Indices_of_var))
        SystemVariables$Variable_Selected_IndexOf = SystemVariables$BeforeList_Indices_of_var[SystemVariables$BeforeList_IndexSelected]
      else{
        SystemVariables$Variable_Selected = F
        SystemVariables$BeforeList_HasFocus = F
      }
            
      #remember to zeroize selection on the after list, otherwise we will not bet able to reselect it
      Controller_Update_AfterList()
      
    }
    if(SystemVariables$AfterList_HasFocus & SystemVariables$AfterList_IndexSelected!=-1){
     
      SystemVariables$Variable_Selected_IndexOf = SystemVariables$AfterList_Indices_of_var[SystemVariables$AfterList_IndexSelected]      
      SystemVariables$Graphs_Nr_Selected   =  SystemVariables$Transformation_Used_Index[SystemVariables$Variable_Selected_IndexOf]  
      
      
      SystemVariables$Variable_Selected = T
      
      
      #remember to zeroize selection on the before list, otherwise we will not bet able to reselect it
      Controller_Update_BeforeList()
    }
    
    
    #calling transformation functions, and storing results:
    ind_selected = SystemVariables$Variable_Selected_IndexOf
    ind_of_var_in_vardef = which(SystemVariables$VarDef_label == colnames(SystemVariables$Data_Original)[ind_selected])
    transformations_obj = NULL
    bin_type_parameter = NULL
    kernel_width_parameter = NULL
    
    #if variable is excluded, tell the user!
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
    
    #this handles a case where the the KDE slider is not initialized yet
    if(is.null(kernel_width_parameter))
      kernel_width_parameter = 1
    if(is.null(bin_type_parameter))
      bin_type_parameter = 1
    
    # this is where the magic happens:
    if(length(ind_of_var_in_vardef) == 1){
      #here we check A and B are valid
      current_a = SystemVariables$VarDef_a[ind_of_var_in_vardef]
      current_b = SystemVariables$VarDef_b[ind_of_var_in_vardef]
      if(is.null(current_a) | is.na(current_a) |!is.numeric(current_a)){
        new_current_a = min(SystemVariables$Data_Original[,ind_selected],na.rm = T)
        showModal(modalDialog(
          title = "Invalid a parameter",
          paste0("Taking a to be minimum of variable: a= ", format(new_current_a, scientific = TRUE, digits = 4))))
        current_a = new_current_a
      }
      
      if(is.null(current_b) | is.na(current_b) |!is.numeric(current_b)){
        new_current_b = max(SystemVariables$Data_Original[,ind_selected],na.rm = T)
        showModal(modalDialog(
          title = "Invalid a parameter",
          paste0("Taking a to be maximum of variable: b= ", format(new_current_b, scientific = TRUE, digits = 4))))
        current_b = new_current_b
      }
      #here we call the actual engine
      transformations_obj = tryCatch(
        wrapTypes(
          target.vec = SystemVariables$Data_Original[,ind_selected],
          type = as.character(SystemVariables$VarDef_type[ind_of_var_in_vardef]),
          a = current_a,
          b = current_b,
          to.reverse  = SystemVariables$VarDef_reverse[ind_of_var_in_vardef], 
          bin.width   = bin_type_parameter,
          window.size = kernel_width_parameter,
          var.name    = colnames(SystemVariables$Data_Original)[ind_selected],
          index.type = INDICES_FOR_ASYMMETRY$YULE
        ),error = function(e){SystemVariables$ErrorLineString = paste0("Error in Transformations: ",as.character(e));NULL}
      )
      if(!is.null(transformations_obj)){
        SystemVariables$ErrorLineString = ''
        transformation_names = names(transformations_obj$Transformations)
        #save into System variables
        SystemVariables$Graphs_ggplot2_obj_list  = transformations_obj$Plots
        SystemVariables$Graphs_display_transformed_data  = transformations_obj$Transformations
        SystemVariables$Graphs_display_transformed_yule   = transformations_obj$Index$Index 
        
        #no graph selected after redraw
        if(SystemVariables$BeforeList_HasFocus)
          SystemVariables$Graphs_Nr_Selected = -1
        
        
        
        SystemVariables$StatusLineString = paste0(
          colnames(SystemVariables$Data_Original)[SystemVariables$Variable_Selected_IndexOf],
          " selected")
        SystemVariables$Sliders_need_to_update = T
      }else{
        SystemVariables$StatusLineString = ""
        SystemVariables$ErrorLineString = paste0(SystemVariables$ErrorLineString,"Cannot transform ",colnames(SystemVariables$Data_Original)[SystemVariables$Variable_Selected_IndexOf])
        SystemVariables$Sliders_need_to_update = F
        showModal(modalDialog(
          title = MSGS$MSG_CANNOT_TRANSFORM_TITLE,
          SystemVariables$ErrorLineString
        ))
      }
    }
    
    SystemVariables$Graphs_RefreshNeeded = F
    
    #if variable is not found in vardef, report to user
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
        return_obj = return_obj + theme(plot.background = element_rect( #panel.border
                                          size = 3,
                                          colour = "#bcb7a6",
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
  
  
  #renderer for data upload
  output$ui_load_data <- renderUI({
    should_show = !SystemVariables$workSpaceIsLoaded
    if(should_show){
      fileInput('file_Data', '', accept=c('text/csv',  'text/comma-separated-values,text/plain', '.csv'))
    }else{
      h4(UI_LABELS$WORKSPACE_LOADED_NO_UPLOAD)
    }
  })
  
  #renderer for var def upload
  output$ui_load_var_def <- renderUI({
    should_show = !SystemVariables$workSpaceIsLoaded
    if(should_show){
      fileInput('file_VarDef', '', accept=c('text/csv',  'text/comma-separated-values,text/plain',  '.csv'))
    }else{
      h4(UI_LABELS$WORKSPACE_LOADED_NO_UPLOAD)
    }
  })
  
  #renderer for export data button:
  output$ui_export_trans_data <- renderUI({
    should_show = SystemVariables$Data_Is_Loaded & SystemVariables$VarDef_Is_Loaded
    if(should_show){
      downloadButton("button_Export",UI_LABELS$BUTTON_LABEL_EXPORT_DATA)
    }else{
      h4(UI_LABELS$EXPORT_DATA_NOT_READY)
    }
    
  })
  
  #renderer for export transformation button:
  output$ui_export_trans_report <- renderUI({
    should_show = SystemVariables$Data_Is_Loaded & SystemVariables$VarDef_Is_Loaded 
    if(should_show){
      downloadButton("button_ExportTransReport",UI_LABELS$BUTTON_LABEL_EXPORT_TRANS)
    }else{
      h4(UI_LABELS$EXPORT_TRANS_NOT_READY)
    }
  })
  
  #renderers for sliders:
  #Bin size slider:
  output$ui_Slider_BinSize <- renderUI({
    sliderInput("graphicalparameter_BinSize", UI_LABELS$SLIDER_BIN_SIZE,
                min = SLIDER_BINSIZE_MIN_MULTIPLIER, max = SLIDER_BINSIZE_MAX_MULTIPLIER, value = 1,width = '85%',sep='',step = 0.1
    )
  })
  
  #KDE slider:
  output$ui_Slider_KernelWidth <- renderUI({
    need_to_show = F

    if(!is.null(SystemVariables$VarDef_type))
        if(SystemVariables$Variable_Selected_IndexOf>=1 & SystemVariables$Variable_Selected_IndexOf <= ncol(SystemVariables$Data_Original)){
          ind_selected = SystemVariables$Variable_Selected_IndexOf
          current_var_type = NULL
          if(colnames(SystemVariables$Data_Original)[ind_selected] %in% names(SystemVariables$Data_Type))
            current_var_type = SystemVariables$Data_Type[[colnames(SystemVariables$Data_Original)[ind_selected]]]
          #ind_of_var_in_vardef = which(SystemVariables$VarDef_label == colnames(SystemVariables$Data_Original)[ind_selected])
          if(!is.null(current_var_type))
            if(tolower(current_var_type) %in%
               tolower(VARIABLE_TYPES_WITH_DENSITY)){
              need_to_show = T
            }
        }


    if(SystemVariables$Sliders_need_to_update){
      SystemVariables$Sliders_need_to_update = F
    }    
    if(need_to_show ){
      if(is.null(SystemVariables$Slider_KernelWidth_current_value))
        SystemVariables$Slider_KernelWidth_current_value = 1
      sliderInput("graphicalparameter_KernelWidth",
                    UI_LABELS$SLIDER_KDE_WIDTH,
                    min = SLIDER_KERNELWIDTH_MIN_MULTIPLIER,
                    max = SLIDER_KERNELWIDTH_MAX_MULTIPLIER,
                    value = SystemVariables$Slider_KernelWidth_current_value,
                    width = '85%',sep='',step = 0.1
      )
    }
    #   

    # 
    
  })
  ###
  ## Click Handlers
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

  
  ### Var guesser 
  ## Disable download button 
  #Observer - check if data is loaded
  observe({
    #Check if Data is loaded
    #get current references to files
    File_Data_Guess <- input$file_varGuess
    if (!is.null(input$file_varGuess)) {
      Temp_File  <- read.csv(File_Data_Guess$datapath)
      SystemVariables$VarDef_Guess <- WrapGuess(Temp_File)

    }
  })
   
   
  output$downloadGuess <- downloadHandler( 
    filename = function() {
      paste("GuessVarType_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
        write.csv(SystemVariables$VarDef_Guess, file = file)
    },
    contentType = "text/csv"
    ) 
  
  output$ui_download_generated_vardef <- renderUI({
    if (!is.null(input$file_varGuess)) {
      downloadButton('downloadGuess', 'Download')
      
    }
  })


  
})


