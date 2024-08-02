#' A Function to Automatically Import Files in a Folder
#'
#' This function imports files with the same file type as specified in bulk. Users are required to provide the Directory to the folder containing files to be imported.
#'
#' Importer supports most of the commonly used data format, including  "txt", "tsv", "csv", "xls(x)","sas", and "sav".
#'
#' All sheets contained within an excel file will be imported into a data list structure while keeping the assigned sheet names for each sub-list.
#'
#' Users can also specify if the imported files should be merged into a data list structure or as separate data.frame.
#' If unsure, users may be able to determine by simply opening one of the files in notepad++. Common Delims include tabs "/t" , comma "," , semicolon ";" , and period "." .
#'
#' @import readxl haven utils readr stats
#' @param Directory Directory of where the files that you want to import (e.g. "C:/Users/___YOUR USERNAME___/UPSTREAM FOLDER/.../FOLDER NAME/"). If not specified, the default is set to the working directory.
#' @param File_Type The type of file to import. Currently, supports  "txt", "tsv", "csv", "xls(x)","sas", and "sav". <default: "csv">
#' @param Encoding If csv files would be imported, one can specify
#' @param Delim If the data is stored in csv file and the Delim is not comma as default, please, specify it. Common deliminators include comma "," semicolon ";" colon ":" tab "/t" .  <default: ",">
#' @param Keyword Specify keywords to import only specific files based on file names <Default: FALSE, i.e., import all>
#' @param Exclude Specify keywords to exclude specific files from importation <Default: FALSE, i.e., exclude none>
#' @param inList Binary operator identify if to import all files into a large list <Default: FALSE, i.e., import each separately into individual dataframe>
#' @param Meta_List_Name The name assigned to the meta data list if inList is TRUE <Default: df_list>
#' @return Import files into your working enviroment in R (i.e., Global Enviroment in R Studio)
#' @keywords import, read
#' @export
#' @seealso \code{\link{Merger}}
#' @examples
#'
#' # Load the Simulated Sleep Datasets
#' data("SimSleep")
#'
#' # Change the Working Directory to the Default Temporary Directory
#' Current.Dir <- getwd(); setwd(tempdir())
#'
#' # Export ALL 20 Simulated Data to Individual Files
#' for(i in names(SimSleep)){
#' write.table(SimSleep[[i]], file = paste0(i,".txt"), sep = "," )
#' # sep allows users to specify the desired deliminater.
#' }
#'
#' # Import All 20 Files at the Same Time
#' # < Option 1 > One data.frame per file (i.e., default option)
#' Importer(Directory = getwd(),
#'               File_Type = "txt",
#'               Delim = ",")
#'
#' # < Option 2 > Store All 20 files into a data.list
#' Importer(Directory = getwd(),
#'               File_Type = "txt",
#'               Delim = ",",
#'               inList = TRUE,
#'               Meta_List_Name = "SimSleep2")
#'
#' # Reset Working Directory
#' setwd(Current.Dir)
#' rm(Current.Dir)
#'


Importer = function(Directory = NULL, File_Type = "csv", Encoding = "", Delim = ",", Keyword =  NULL, Exclude = NULL, inList = FALSE, Meta_List_Name = "df_list", Progress = FALSE, ...) {


  # Check Pre-requisit --------
  for (x in c("readxl" ,"haven","readr", "utils", "stats"))
    if (!requireNamespace(x, quietly = T)) {
      install.packages(x)
      requireNamespace(x, quietly = T)
    }


  # Check & Correct Directory -------------------
  Directory = DirSetting(Dir = Directory)

  ## Check if the Directory is a file already -------------

  Directory = DirClass(Dir = Directory)
  if (!class(Directory) == "Folder") {
    File_Type = class(Directory) # Update File Type

    Keyword = gsub("^.*/(.*?)\\..*$","\\1",Directory) # Update Keyword

    Directory =  as.character(gsub(paste0(Keyword,".*"),"\\1",Directory)) # Update File Type
  }


  File_Type = paste0(".",File_Type) # Add "." before the file type



  # Get Files --------------------------
  ############################################
  ### Read All files in the Directory ###
  ############################################

  if (length(File_Type) > 1) warnings("Sorry, for the current version, please keep it to one type of files at a time. Please, don`t over-feed me.")



  files = list.files(path = Directory,pattern = paste0("*",File_Type))
  file_dir = list.files(path = Directory,pattern = paste0("*",File_Type),full.names = TRUE)

  # Keyword Control ---------------------

  if (!is.null(Keyword)) {
    files = sapply(Keyword,function(ln){
      ToInclu <- grep(pattern = ln,files)
      files[ToInclu]
    })

    file_dir = sapply(Keyword,function(ln){
      ToInclu <- grep(pattern = ln,file_dir)
      file_dir[ToInclu]
    })
  }

  file_names = gsub(x = files,pattern = File_Type ,replacement = "") # File Names

  # Exclusion List Control ---------------------
  if (!is.null(Exclude)) {
    Exclude = c("^~\\$","^~")
  } else {
    Exclude = c("^~\\$","^~",Exclude)
  }

  Exclude_names <- unlist(file_names)
  Exclude_names <- Exclude_names[grep(paste0(Exclude,collapse = "|"),Exclude_names)]

  if (length(Exclude_names) > 0) {
    for (i in 1:length(files)) {
      if (file_names[i] %in% Exclude_names) {
        files[i] <- NA
        file_dir <- NA
        file_names <- NA
      }}
    files <- na.omit(files)
    file_dir <- na.omit(file_dir)
    file_names <- na.omit(file_names)
  }



  # inList Control ---------------------
  if (isTRUE(inList)) {
    df_list <- rep(list(list()),length(file_names))
    names(df_list) <- file_names
  }


  if (File_Type == ".csv" | File_Type == ".tsv" | File_Type == ".txt") {
    for (i in 1:length(files)) {
      if (Progress) print(files[i])


      if (File_Type == ".csv" | File_Type == ".txt") {
        if (grepl( "UTF",Encoding, fixed = TRUE)) {
          files_df <- read.csv(file_dir[i],sep = Delim)
        } else {
          files_df = read.csv(file_dir[i], encoding = Encoding, sep = Delim)
        }
      }

      if (File_Type == ".tsv") files_df <- read_tsv(file_dir[i])

      # Bind to a list or import files individually
      if (isFALSE(inList)) {
        assign( x = file_names[i], value = files_df,.GlobalEnv)
      } else {
        df_list[[file_names[i]]] <- files_df
      }
    }

  }  else if (File_Type == ".xls" | File_Type == ".xlsx") {

    for (i in 1:length(files)) {

      if (Progress) print(files[i])

      sheets <- excel_sheets(file_dir[i])




      if (length(sheets) > 1) {
        files_df <- lapply(sheets, function(X) read_excel(file_dir[i], sheet = X))
        files_df <- lapply(files_df, as.data.frame)
        names(files_df) <- sheets

      } else {
        files_df <- read_excel(file_dir[i])
        files_df <- files_df
      }



      # Bind to a list or import files individually
      if (isFALSE(inList)) {
        assign( x = file_names[i], value = files_df,.GlobalEnv)
      } else {
        df_list[[file_names[i]]] <- files_df
      }

    }


  } else if (File_Type == ".sas") {

    for (i in 1:length(files)) {
      if (Progress) print(files[i])

      files_df <- read_sas(file_dir[i])
      files_df <- files_df

      # Bind to a list or import files individually
      if (isFALSE(inList)) {
        assign( x = file_names[i], value = files_df,.GlobalEnv)
      } else {
        df_list[[file_names[i]]] <- files_df
      }
    }


  }  else if (File_Type == ".sav") {

    for (i in 1:length(files)) {
      if (Progress) print(files[i])

      files_df <- read_spss(file_dir[i])
      files_df <- files_df

      # Bind to a list or import files individually
      if (isFALSE(inList)) {
        assign( x = file_names[i], value = files_df,.GlobalEnv)
      } else {
        df_list[[file_names[i]]] <- files_df
      }
    }
  } else {
    print("Currently, we do not support ",File_Type," !")
  }


  if (isTRUE(inList)) assign( x = Meta_List_Name, value = df_list,.GlobalEnv)
  ############################################
  ###                 End                  ###
  ############################################

}
