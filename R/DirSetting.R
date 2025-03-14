#  File RHelper/R/DirSetting.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2020-2025  C. William Yao, PhD
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#
#
# Directory Correction and Setting


DirSetting = function(Dir = NULL){

  Trigger = FALSE

  # Directory unspecified ------------
  if (is.null(Dir)) {

    Trigger = TRUE
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

  # Check directory existance status -------------
  if (all(!dir.exists(Dir) & !file.exists(Dir))) stop(paste0(Dir," cannot be found. Please, check the spelling.") )

  # Check if the last character is a slash
  Closing = substr(Dir, nchar(Dir), nchar(Dir)) == "/"
  if (!Closing) Dir = paste0(Dir,"/")

  ### Update DefaultDir -------
  #### For initial commit
  if (Trigger)
    if (!"DefaultDir" %in% RHSetting)
      RHSetting$DefaultDir = Dir

  #### If default directory was not properly specified
  if (Trigger)
    if ("DefaultDir" %in% RHSetting & !Dir == RHSetting$DefaultDir){
      message("Improper directory grammar was found in default directory.
            Correction was made and assigned.")
      RHSetting$DefaultDir = Dir
    }



  ## Export Direcotry ----------
  return(Dir)


  assign("RHSetting", RHSetting, envir = .GlobalEnv)





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


