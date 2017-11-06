
#Constants:
##############
#This is the land of the creatures known as constants:

#version:
VERSION_SERVER = '20171003'

#This is the list of fields to be loaded, on workspace load
FIELDS_TO_SAVE_LIST = c('Version_Server','Data_File','Data_FileName','Data_Is_Loaded','Data_Is_Error',
                        'VarDef_File','VarDef_FileName','VarDef_Is_Loaded','VarDef_Is_Error',
                        'Data_Original','Data_Transformed','Original_Yule','New_Yule',
                        'hasBeenTransformed','Transformation_Used','Transformation_Used_Index','isExcluded','isExcluded_Reason',
                        'VarDef_table','VarDef_label','VarDef_a','VarDef_b','VarDef_type','VarDef_reverse')

VARIABLE_TYPES = c("Amounts","Counts","Ratio","Proportion","Counted Fraction",
                   "Bounded Amounts","Bounded Counts","Ranks","Ordered Categories",
                   "Binary (categories)","Category")

VARIABLE_TYPES_WITH_DENSITY = VARIABLE_TYPES[which(VARIABLE_TYPES %in% c("Amounts","Ratio","Proportion","Bounded Amounts"))]

INDICES_FOR_ASYMMETRY = list()
INDICES_FOR_ASYMMETRY$YULE = 'Yule'

SLIDER_BINSIZE_MIN_MULTIPLIER = 0.1
SLIDER_BINSIZE_MAX_MULTIPLIER = 4
SLIDER_KERNELWIDTH_MIN_MULTIPLIER = 0.1
SLIDER_KERNELWIDTH_MAX_MULTIPLIER = 4

#Messages:
MSGS = list()
MSGS$MSG_CANNOT_EXPORT_TITLE = "Cannot Export Data"
MSGS$MSG_CANNOT_EXPORT_BODY = "Cannot Export Data - No Data Has Been Loaded"
MSGS$MSG_CANNOT_DOWNLOAD_TITLE = "Cannot Download VarType Estimation"
MSGS$MSG_CANNOT_DOWNLOAD_BODY = "Cannot Download VarType Estimation - No Data Was Uploaded"
MSGS$DISCLAIMER = "Note: The suggested variable types are in the sole responsability of the user, and require their discretion. Please consult the user manual for the meaning of each variable type definition."

MSGS$MSG_VAR_NOT_IN_VARDEF_TITLE = "Cannot display variable"
MSGS$MSG_VAR_NOT_IN_VARDEF_BODY = "Cannot display variable, no line \n found for variable in vardef file"

MSGS$MSG_VARIABLE_EXCLUDED_TITLE = "Cannot display variable"
MSGS$MSG_VARIABLE_EXCLUDED_BODY = "Variable has been excluded, reason:"


MSGS$MSG_VARDEF_CHECK_TYPENOTFOUND_TITLE = "Some Variable Types Invalid"
MSGS$MSG_VARDEF_CHECK_TYPENOTFOUND_BODY = "Some variable types in selected variable \n definition dont match known types."

MSGS$MSG_VARDEF_CHECK_NOT_NUMERIC_A_TITLE = "Parameter \'a\' invalid"
MSGS$MSG_VARDEF_CHECK_NOT_NUMERIC_A_BODY = "Values for parameter \'a\' are not \n strictly numeric."

MSGS$MSG_VARDEF_CHECK_NOT_NUMERIC_B_TITLE = "Parameter \'b\' invalid"
MSGS$MSG_VARDEF_CHECK_NOT_NUMERIC_B_BODY = "Values for parameter \'b\' are not \n strictly numeric"

MSGS$MSG_VARDEF_CHECK_NOT_NUMERIC_ABORDER_TITLE = "Parameter \'b\' must be >= \'a\' "
MSGS$MSG_VARDEF_CHECK_NOT_NUMERIC_ABORDER_BODY = "Values for parameters must be \'b\' >= \'a\' ."

MSGS$MSG_VARDEF_CHECK_NOT_NUMERIC_TOREVERSE01_TITLE = "Field \'To.Reverse\' must be 0 or 1"
MSGS$MSG_VARDEF_CHECK_NOT_NUMERIC_TOREVERSE01_BODY = "Values for field \'To.Reverse\' must be 0 or 1 ."

MSGS$MSG_CANNOT_TRANSFORM_TITLE = "Cannot Transform Variable"


STATUS_LINE_MSGS = list()
STATUS_LINE_MSGS$INIT = "INIT"
STATUS_LINE_MSGS$TRANSFORMATION_APPLIED = "Transformation Applied"
STATUS_LINE_MSGS$DATA_LOADED = "Data Loaded"
STATUS_LINE_MSGS$VARDEF_LOADED = "Variable Definition Loaded"
STATUS_LINE_MSGS$WORKSPACE_LOAD =  "Workspace File Loaded!"

UI_LABELS = list()
UI_LABELS$BEFORE_LIST = "Before Transformation:"
UI_LABELS$AFTER_LIST = "After Transformation:"
UI_LABELS$LIST_REFRESH_MSG = "Variable List Refreshed"

UI_LABELS$SAVE =   "Save current work space:"
UI_LABELS$LOAD = "Load previous work space:"
UI_LABELS$EXPORT = "Export transformed dataset:"
UI_LABELS$EXPORT_TRANS = "Export Transformation report:"
UI_LABELS$TITLE = 'AutoNeta - Transformation Mananger'
UI_LABELS$TAB_FILE = "File"
UI_LABELS$TAB_TRANSFORM = "Transform"
UI_LABELS$TAB_GENERATE_VARDEF = "Generate Variable Defenition"
UI_LABELS$UPLOAD_VARDEF_LABEL = 'Choose variable definitions CSV file'
UI_LABELS$UPLOAD_DATA_LABEL = 'Choose Data CSV file'
UI_LABELS$SORT_BY_YULE = "Sort by Yule (Binary Categories appear last)"

UI_LABELS$BUTTON_LABEL_SAVE = "Save"
UI_LABELS$BUTTON_LABEL_LOAD = ""
UI_LABELS$BUTTON_LABEL_EXPORT_DATA = "Export Transformed Data"
UI_LABELS$BUTTON_LABEL_EXPORT_TRANS = "Transformation Report"
UI_LABELS$BUTTON_LABEL_APPLY_TRANSFORMATION = "Apply Transformation"

UI_LABELS$SLIDER_BIN_SIZE = "Bin Size: (Scale is 0-2)"
UI_LABELS$SLIDER_KDE_WIDTH = "KDE Width: (Scale is 0-2)"

UI_LABELS$HELP_MENU = "Help"
UI_LABELS$HELP_MENU_HELP_ITEM = "Help"
UI_LABELS$HELP_MENU_ABOUT_ITEM = "About"

UI_LABELS$EXCLUDE_VARIABLE_NOT_NUMERIC = "Variable is not purely numeric."
UI_LABELS$EXCLUDE_VARIABLE_NOT_ENOUGH_VALUES = " Variable has less then three valid values."

UI_LABELS$LIST_EXCLUDED = " (Excluded)"

UI_LABELS$EXPORT_DATA_NOT_READY = 'Upload data and var. def.  to allow export'
UI_LABELS$EXPORT_TRANS_NOT_READY = 'Upload data and var. def.  to allow export'
UI_LABELS$WORKSPACE_LOADED_NO_UPLOAD = 'Workspace reloaded, refresh to load new data'
UI_LABELS$TUTORIAL = 'Tutorial'

#UX/UI constants:
PLOT_HEIGHT = "280px"