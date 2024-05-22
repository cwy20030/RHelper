#' A function to automatically document files' properties contained in the directery/path specified.
#'
#' This function lists and documents all files contained within one or more directories (i.e., computer paths to folders of interest) specified by the user. This is suitable if one wish to import files from multiple folders into a data.list for analysis.
#'
#' @import readr
#' @param Directories Directory of where the files that you want to document <e.g. "C:/Users/___YOUR USERNAME___/UPSTREAM FOLDER/.../FOLDER NAME/">
#' @return Import files into your working enviroment in R (i.e., Global Enviroment in R Studio)
#' @keywords Import, Read
#' @export
#' @examples
#' Download the folder from
#' Clerk(Directory_to_Downloads_Folder)
#'
#'


Clerk <- function(Directories,...){



  ############################################
  ### List All files/folders in the Directories ###
  ############################################

  if(is.list(Directories)) Directories = unlist(Directories)

  if(length(Directories)>1) Directories = unique(Directories)

  FolderCheck = file.info(Directories)

  if(!any(FolderCheck$isdir))  Directories[!FolderCheck$isdir] <- gsub("/[^/]*$", "", Directories[!FolderCheck$isdir])
  Directories = unique(Directories)



  Out= lapply(Directories, function(Directory){

    files = iconv(list.files(path=Directory,full.names = TRUE), from="UTF-8", to="LATIN1")
    temp = data.frame(matrix(nrow = length(files),ncol=4))
    names(temp) = c("File_Name","File_Type","Folder","File_Path")

    FileCheck = file.info(files)

    temp$File_Name = gsub("^.*/(.*?)\\..*$","\\1",files)

    temp$File_Type = gsub("^.*\\.(.*?)$","\\1",files)
    temp$File_Type[FileCheck$isdir]  = "Folder"

    if("Folder" %in% temp$File_Type) temp$File_Name[FileCheck$isdir] = gsub("^.*/","",temp$File_Name[FileCheck$isdir])

    temp$File_Path = files
    temp$Folder = rep(gsub("^.*/","",Directory),length(files))

    temp
  })


  do.call(rbind,Out)

  ############################################
  ###                 End                  ###
  ############################################

}
