
# Directory Correction and Setting


DirSetting = function(Dir = NULL){

  # Directory unspecified ------------
  if (is.null(Dir)) {


    if (!exists("RHSetting",envir = .GlobalEnv))  EnvSetUp()

    RHSetting = get("RHSetting",envir = .GlobalEnv)

    ## Check for "DefaultDir" -----------------
    ### with DefaultDir -----------------
    if ("DefaultDir" %in% RHSetting) {
      message("No directory was specified. Default directory will be used.")
      message(RHSetting$DefaultDir)

      Dir = RHSetting$DefaultDir # Assign Directory

    } else {
      ### without DefaultDir -----------------
      message("Default directory has yet been set.")


      ### Ask for confirmation
      choice <- menu(c("Yes/Oui", "No/Non"),
                     title = "No directory was specified!
The default will be set to the working directory using getwd().
Do you agree to proceed (reply with 1 or 2) ?")

      #### Check the choice
      ##### No
      if (choice == 2) { # Update Directory

        Dir <- readline(prompt = paste0("Please, manually enter the directory."))

        if (toupper(Dir) %in% c("ABORT","NA","EXIT","NO")) stop("Abort! Please, find the directory and try again.")
      }

      ##### Yes
      if (choice == 1) {

        Dir = getwd() # Working Directory as Directory
      }
    }
  }


  # Correct Directory Grammar ----------------
  Dir = gsub("\\\\","/",Dir)

  # Check if the last character is a slash
  Closing = substr(Dir, nchar(Dir), nchar(Dir)) == "/"
  if(!Closing) Dir = paste0(Dir,"/")

  ## Export Direcotry ----------
  return(Dir)





  # Set Default Directory ----------------
  #    if (!exists("DefaultDir",envir = .GlobalEnv)) {
  #      ### Ask for confirmation
  #     choice2 <- menu(c("Yes/Oui", "No/Non"),
  #                     title = "No default directory set!
  #Would you agree to set the last directory as the default (reply with 1 or 2) ?")

  ##### Yes
  #      if (choice2 == 1) {

  #        assign("DefaultDir",RHSetting$DefaultDir,envir = .GlobalEnv)# Working Directory as Directory
  #     }

  #      }

}


