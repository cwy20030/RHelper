#' A Function to Document Executed Codes by Date with Continuous Time Stamps Separating Different Entries
#'
#' This function was designed to facilitate lab note documentation while help maintain a minimum clean history window. Executed codes will be stored automatically in the History folder in the directory assigned.
#'
#' @import readr utils
#' @param Directory Directory to where the executed codes will be documented. <e.g. "C:/Users/___YOUR USERNAME___/UPSTREAM FOLDER/.../FOLDER NAME/">
#' @return Current time stamp
#' @export
#' @examplesIf interactive()
#'
#' # Find working directory
#' getwd()
#'
#' # Add a documentation...
#' Historian()
#'



Historian <- function(Directory) {


  # Check Pre-requisit --------
  for (x in c("readr", "utils"))
    if (!requireNamespace(x, quietly = T)) {
      install.packages(x)
      requireNamespace(x, quietly = T)
    }


  # Check and Correct the directory --------------
  Directory = gsub("\\\\","/",Directory)

  # Generate a Path ----------
  Directory <- paste0(Directory,"/History")

  if (isFALSE(dir.exists(Directory) )) dir.create(Directory)


  # Generate a File ----------
  File_Name <- paste0(Sys.Date(),".txt")

  if (File_Name %in% list.files(Directory)) {
    History <- read.delim(paste0(Directory,"/",File_Name),header = FALSE)
    History[[1]] <- as.character(History[[1]])
    History[nrow(History) + 2,] <- as.character(timestamp())


    savehistory(file = paste0(Directory,"/","2_",File_Name))
    History2 <- read.delim(paste0(Directory,"/","2_",File_Name),header = FALSE)
    History2[[1]] <- as.character(History2[[1]])
    History <- rbind(History, History2)
    write_delim(History, paste0(Directory,"/",File_Name),col_names = FALSE)

    file.remove(paste0(Directory,"/","2_",File_Name))

  } else {
    savehistory(file = paste0(Directory,"/",File_Name))
  }

  write("", file = ".blank")
  loadhistory(".blank")
  unlink(".blank")
}
