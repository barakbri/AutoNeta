
#Constants:
##############
#This is the land of the creatures known as constants:

#This is the list of fields to be loaded, on workspace load
FIELDS_TO_SAVE_LIST = c('Version_Server','Data_File','Data_FileName','Data_Is_Loaded','Data_Is_Error',
                        'VarDef_File','VarDef_FileName','VarDef_Is_Loaded','VarDef_Is_Error',
                        'Data_Original','Data_Transformed','Original_Yule','New_Yule',
                        'hasBeenTransformed','Transformation_Used','Transformation_Used_Index','isExcluded','isExcluded_Reason',
                        'VarDef_table','VarDef_label','VarDef_a','VarDef_b','VarDef_type','VarDef_reverse')

VARIABLE_TYPES = c("Amounts","Counts","Ratio","Proportion","Counted Fraction",
                   "Bounded Amounts","Bounded Counts","Ranks","Ordered Categories",
                   "Binary (categories)","Category")

INDICES_FOR_ASYMMETRY = list()
INDICES_FOR_ASYMMETRY$YULE = 'Yule'

SLIDER_BINSIZE_MIN_MULTIPLIER = 0.25
SLIDER_BINSIZE_MAX_MULTIPLIER = 2
SLIDER_KERNELWIDTH_MIN_MULTIPLIER = 0.25
SLIDER_KERNELWIDTH_MAX_MULTIPLIER = 2

#Messages:
MSGS = list()
MSGS$MSG_CANNOT_EXPORT_TITLE = "Cannot Export Data"
MSGS$MSG_CANNOT_EXPORT_BODY = "Cannot Export Data - No Data Has Been Loaded"
MSGS$MSG_CANNOT_DOWNLOAD_TITLE = "Cannot Download VarType Estimation"
MSGS$MSG_CANNOT_DOWNLOAD_BODY = "Cannot Download VarType Estimation - No Data Was Uploaded"
MSGS$DISCLAIMER = "The suggested var types are in the sole responsability of the user, and require their discretion."

MSGS$MSG_VAR_NOT_IN_VARDEF_TITLE = "Cannot display variable"
MSGS$MSG_VAR_NOT_IN_VARDEF_BODY = "Cannot display variable, no line \n found for variable in vardef file"

MSGS$MSG_VARIABLE_EXCLUDED_TITLE = "Cannot display variable"
MSGS$MSG_VARIABLE_EXCLUDED_BODY = "Variable has been excluded, reason:"


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
UI_LABELS$TITLE = "AutoNeta"
UI_LABELS$TAB_FILE = "File"
UI_LABELS$TAB_TRANSFORM = "Transform"
UI_LABELS$TAB_GENERATE_VARDEF = "Generate Variable Defenition"
UI_LABELS$UPLOAD_VARDEF_LABEL = 'Choose variable definitions CSV file'
UI_LABELS$UPLOAD_DATA_LABEL = 'Choose Data CSV file'
UI_LABELS$SORT_BY_YULE = "Sort By Yule"

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

#UX/UI constants:
PLOT_HEIGHT = "280px"