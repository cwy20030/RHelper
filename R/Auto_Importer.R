#' A Function to Automatically Import Files in a Folder
#'
#' This function imports files with the same file type as specified in bulk. Users are required to provide the Directory to the folder containing files to be imported. #' Auto_Importer supports most of the commonly used data format, including .txt .tsv .csv, .xls(x), .sas, .sav . All sheets contained within an excel file will be imported into a data list structure while keeping the assigned sheet names for each sub-list. Users can determine if the imported files should be merged into a data list structure or as separate data.frame.
#' If unsure, users may be able to determine by simply opening one of the files in notepad++. Common Delims include tabs "/t" , comma "," , semicolon ";" , and period "." .
#'
#' @import readxl haven readr
#' @param Directory Directory of where the files that you want to import <e.g. "C:/Users/___YOUR USERNAME___/UPSTREAM FOLDER/.../FOLDER NAME/">
#' @param File_Type The type of file to import. Currently, supports  ".txt .tsv  ".csv", ".xls(x)",".sas",".sav". <default: ".csv">
#' @param Encoding If .csv files would be imported, one can specify
#' @param Delim If the data is stored in .csv file and the Delim is not comma as default, please, specify it. Common deliminators include comma "," semicolon ";" colon ":" tab "/t" .  <default: ",">
#' @param Keyword Specify keywords to import only specific files based on file names <Default: FALSE, i.e., import all>
#' @param Exclude Specify keywords to exclude specific files from importation <Default: FALSE, i.e., exclude none>
#' @param inList Binary operator identify if to import all files into a large list <Default: FALSE, i.e., import each separately into individual dataframe>
#' @param Meta_List_Name The name assigned to the meta data list if inList is TRUE <Default: df_list>
#' @return Import files into your working enviroment in R (i.e., Global Enviroment in R Studio)
#' @keywords Import, Read
#' @export
#' @examples
#' Download the folder from
#' Auto_Importer(Directory_to_Downloads_Folder)
#'
#'


Auto_Importer <- function(Directory,File_Type=".csv",Encoding="",Delim=",", Keyword= FALSE,Exclude= FALSE, inList=FALSE, Meta_List_Name="df_list",Progress=FALSE,...){


  if(File_Type==".xls" | File_Type==".xlsx") {
  if(!require("readxl",character.only = TRUE)) stop("Package 'readxl' not found")

  } else if(File_Type==".sas" | File_Type==".sav"){
  if(!require("haven",character.only = TRUE)) stop("Package 'haven' not found")

  } else if(File_Type==".tsv"){
    if(!require("readr",character.only = TRUE)) stop("Package 'readr' not found")
  }

############################################
### Read All files in the Directory ###
############################################

  if(length(File_Type)>1) warnings("Sorry, for the current version, please keep it to one type of files at a time. Please, don`t over-feed me.")


  if(!Keyword==FALSE) {

    files = iconv(list.files(path = Directory,pattern = paste0("*",File_Type)), from="UTF-8", to="LATIN1")

    if(length(Keyword)>1){
      files_original <- iconv(list.files(path = Directory,pattern = paste0("*",File_Type)), from="UTF-8", to="LATIN1")

      files <- rep(list(),length(Keyword))

      files <- lapply(Keyword,function(ln){
        Exclusion <- grep(pattern = ln,files_original)
        files_original[Exclusion]
      })
      file_dir = iconv(list.files(path=Directory,pattern = paste0("*",File_Type),full.names = TRUE), from="UTF-8", to="LATIN1")[Exclusion]


    } else {
      Exclusion <- grep(pattern = Keyword,files)
      files <- files[Exclusion]
      file_dir = iconv(list.files(path=Directory,pattern = paste0("*",File_Type),full.names = TRUE), from="UTF-8", to="LATIN1")[Exclusion]

    }



  } else {

files = iconv(list.files(path = Directory,pattern = paste0("*",File_Type)), from="UTF-8", to="LATIN1")
file_dir = iconv(list.files(path=Directory,pattern = paste0("*",File_Type),full.names = TRUE), from="UTF-8", to="LATIN1")



}

  file_names = gsub(x = files,pattern = File_Type ,replacement = "")


if(!isFALSE(Exclude)){
Exclude_names <- unlist(file_names)
Exclude_names <- Exclude_names[grep(Exclude,Exclude_names)]
  for(i in 1:length(files)){
  if(file_names[i] %in% Exclude_names) {
  files[i] <- NA
  file_dir <- NA
  file_names <- NA
  }
    files <- na.omit(files)
    file_dir <- na.omit(file_dir)
    file_names <- na.omit(file_names)
    }

}

  if(isTRUE(inList)){
    df_list <- rep(list(list()),length(file_names))
    names(df_list) <- file_names
  }


if(File_Type==".csv"| File_Type==".tsv" | File_Type==".txt"){
  for(i in 1:length(files)){
    if(Progress) print(files[i])


    if(File_Type==".csv"|File_Type==".txt"){
      if(grepl( "UTF",Encoding, fixed = TRUE)){
        files_df <- read.csv(file_dir[i],sep = Delim,show_col_types = FALSE)
    } else {
      files_df <- read.csv(file_dir[i],encoding=Encoding,sep = Delim,show_col_types = FALSE)
    }
      }

    if(File_Type==".tsv") files_df <- read_tsv(file_dir[i])

    # Bind to a list or import files individually
    if(isFALSE(inList)){
      assign( x = file_names[i], value = files_df,.GlobalEnv)
    } else {
      df_list[[file_names[i]]] <- files_df
    }
  }

}  else if(File_Type==".xls" | File_Type==".xlsx") {

  for(i in 1:length(files)){

    if(Progress) print(files[i])

    sheets <- excel_sheets(file_dir[i])

  if(length(sheets)>1) {
    files_df <- lapply(sheets, function(X) read_excel(file_dir[i], sheet = X))
    files_df <- lapply(files_df, as.data.frame)
    names(files_df) <- sheets

    # Bind to a list or import files individually
    if(isFALSE(inList)){
      assign( x = file_names[i], value = files_df,.GlobalEnv)
    } else {
      df_list[[file_names[i]]] <- files_df
    }


  } else {
    files_df <- read_excel(file_dir[i])
    files_df <- files_df

    # Bind to a list or import files individually
    if(isFALSE(inList)){
      assign( x = file_names[i], value = files_df,.GlobalEnv)
    } else {
      df_list[[file_names[i]]] <- files_df
    }
  }
  }


  } else if(File_Type==".sas") {

    for(i in 1:length(files)){
      if(Progress) print(files[i])

      files_df <- read_sas(file_dir[i])
      files_df <- files_df

      # Bind to a list or import files individually
      if(isFALSE(inList)){
        assign( x = file_names[i], value = files_df,.GlobalEnv)
      } else {
        df_list[[file_names[i]]] <- files_df
      }
    }


  }  else if(File_Type==".sav") {

    for(i in 1:length(files)){
      if(Progress) print(files[i])

      files_df <- read_spss(file_dir[i])
      files_df <- files_df

      # Bind to a list or import files individually
      if(isFALSE(inList)){
        assign( x = file_names[i], value = files_df,.GlobalEnv)
      } else {
        df_list[[file_names[i]]] <- files_df
      }
    }
   } else {
  print("Currently, we do not support ",File_Type," !")
   }


  if(isTRUE(inList)) assign( x = Meta_List_Name, value = df_list,.GlobalEnv)
############################################
###                 End                  ###
############################################

}
