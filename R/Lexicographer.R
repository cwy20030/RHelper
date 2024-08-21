#' Project-oriented Dictionary Composition: Lexicographer
#'
#' This function will automatically document all the variables embeded in the project.
#' When calling Lexicographer for the first time, Lexicographer will create a dictionary template at where the
#' When activate Lexicographer, the
#'
#' @import readxl writexl readr utils stats
#' @param Directory Directory of where the dictionary was stored (e.g. "C:/Users/___YOUR USERNAME___/UPSTREAM FOLDER/.../FOLDER NAME/"). If not specified, the default is set to the working directory.  Note for first time user or new project setup, a new dictionary will be automatically generated if not found.
#' @param Data a data.frame
#' @param Commit A logical indicator to specify whether to save the update (default = TRUE)
#' @param Version_Control A logical indicator to specify whether to create a version history of the Dictionary. (default = TRUE)
#' @return Import files into your working enviroment in R (i.e., Global Enviroment in R Studio)
#' @keywords Dictionary, Lexicographer
#' @export
#' @examplesIf interactive()
#'
#' # Load infert Dataset
#' data(infert)
#'
#' ## Add NA
#' for(i in names(infert)){
#'   a = sample(15,1)
#'   b = sample(nrow(infert),a)
#'
#'   infert[[i]][b] = NA
#' }
#'
#' # Change the Working Directory to the Default Temporary Directory
#' Current.Dir <- getwd(); setwd(tempdir())
#'
#'
#' Lexicographer(Directory = tempdir(), Data = infert)
#'
#'
#'
#' # Reset Working Directory
#' setwd(Current.Dir)
#' rm(Current.Dir)
#'
#'

Lexicographer <- function(Directory=NULL, Data = NULL, Commit = F, Version_Control = T ){


# Check Pre-requisit --------
  for (x in c("readxl", "writexl", "readr", "utils", "stats"))
    if (!requireNamespace(x, quietly = T)) {
      install.packages(x)
      requireNamespace(x, quietly = T)
    }



# Check if Directory Grammar ----------
  if (is.null(Directory)) Trigger = TRUE
  Directory = DirSetting(Dir = Directory)


# Decision on the Dictionary -------------
# Default Decision = 1

  Decision = 1


  if (Trigger & "Dictionary" %in% Who_is("List")) {
    message("A Dictionary has been found in the Global Environment.
            Do you wish to use this existing Dictionary?")
    warning("To create a new Dictionary, the existing one will be removed from the Global Environment!")
    Options = c("Default: Import/Create a Dictionary","Use Existing Dictionary in the Global Environment")
    Decision = QnA(Options,"action")
  }


# Check if Dictionary exists in the directory --------------

  ClerkLog = Clerk(Directory)


## Decision == 2  --------------
### If to keep the existing Dictionary ---------------

  if (Decision == 2) {
    Dictionary = get("Dictionary",envir = .GlobalEnv)

    if (!"Dictionary" %in% ClerkLog$File_Name) writexl::write_xlsx(list(Variable = Dictionary$Variable, Value = Dictionary$Value), path = paste0(Directory,"/Dictionary.xlsx"))

  }
## Decision == 1  --------------
### First Time ---------------------
if (!"Dictionary" %in% ClerkLog$File_Name) {

    Variable = data.frame(matrix(ncol = 5))
    Value =  data.frame(matrix(ncol = 6))
    names(Variable)  = c("Variable", "Type",	"Unit",	"Definition", "Note")
    names(Value)  =   c("Variable",	"Type",	"Unit", "Value", "Definition", "Note")

    Dictionary = list(Variable = Variable, Value = Value)

  message("No Dictionary was found in the directory. Dictionary now is created.

<Note>
Currently, there is no additional option to individualize the dictionary name.
Therefore, please, do NOT manually modify the file name of the dictionary.
This however, does not restrict manual edits on the content within the dictionary.
In fact, we encourage users to go through the dictionary.

As a gentle reminder, Lexicographer can also be used to document newly created variable.")

  writexl::write_xlsx(list(Variable = Variable, Value = Value), path = paste0(Directory,"/Dictionary.xlsx"))

  Path = paste0(Directory,"/Dictionary.xlsx") # Save Path

 # if(nrow(df) > 1024^2){
 #   stop("the xlsx format does not support tables with 1M+ rows")
 #  }

} else {

### If there is a dictionary already, import it -------------

  Path = ClerkLog$File_Path[ClerkLog$File_Name == "Dictionary"]

  Importer(Path)
  Dictionary = get("Dictionary",envir = .GlobalEnv)
}



# Documentation  ------------------
  if (!is.null(Data)) {

 #   df_name <- deparse(substitute(Data)) # Extract Data Name


 ## Step 1: Check Existance of Variable Names within the Existing Dictionary -------
   VNames = names(Data)

   Exclude = VNames[VNames %in% Dictionary$Variable$Variable]

   if (length(Exclude) > 0) {
     warning(paste0("Following variables ", paste0(Exclude,collapse = ", ") ," had been documented in prior session. "))

     SubData = Data[VNames[!VNames %in% Exclude]] # Subset Data for New Variable Documentation

   } else {

     SubData = Data
   }



  ## Step 2: Check Existance of Variable Names within the Existing Dictionary ----
   if (nrow(SubData) > 0) {
   Update = VDocument(SubData)


   ### Version Control ------
   if (!Trigger & isTRUE(Commit)) {
      if (isTRUE(Version_Control)) {
        VControl(Dir = Directory,
                 dfName = "Dictionary",
                 extension = ".xlsx",
                 category = "Dictionary")
      }
   }


   Dictionary$Variable = RowBind(Dictionary$Variable,Update$Variable)
   Dictionary$Variable = Dictionary$Variable[complete.cases(Dictionary$Variable),]


   Dictionary$Value = RowBind(Dictionary$Value,Update$Value)
   Dictionary$Value = Dictionary$Value[complete.cases(Dictionary$Value),]




  assign("Dictionary",Dictionary,envir = .GlobalEnv)



  if (isTRUE(Commit))
  writexl::write_xlsx(list(Variable = Dictionary$Variable, Value = Dictionary$Value), path = paste0(Directory,"/Dictionary.xlsx"))

  }



}


}
