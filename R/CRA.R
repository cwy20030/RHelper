#  File RHelper/R/CRA.R
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
#' @title CRA
#'
#' @description
#' A function that check and install missing dependencies for packages.
#'
#' @details
#' The function - CRA (inspired by the acronym of Canadian Revenue Agency), will check if the dependent packages required are installed in the system. It is particularly helpful in the following scenarios.
#' 1. Installing a "developing" libraries from GitHub
#' 2. Sourcing an in-house library from the author
#' 3. Check dependencies and repair missing libraries after downgrading/upgrading R environment without performing automatic transfer for libraries.
#'
#' @import utils stats
#' @param Package The name of the package <e.g. "RHelper">
#' @param Install_Suggests Specify if the suggested libraries will be installed <default: FALSE>
#' @param Silent A logical indicator for reporting summary.
#' @export
#' @examplesIf interactive()
#'
#  # if(!require("stats", character.only = TRUE)) install.packages("stats")
#
#' CRA("stats") # ***It is very important to keep the double quotation marks!
#'
#'
#'
#'



CRA <- function(Package, Install_Suggests=FALSE, Silent=FALSE ){

  # Check Pre-requisit --------
  for(x in c("utils"))
    if(!requireNamespace(x, quietly = T)){
      install.packages(x)
      requireNamespace(x, quietly = T)
    }


  # Get All Avaialble Packages on the Official R Depository
  All_Packs <- available.packages()[,"Package"]

  # Get all installed packages in the system
  Installed <- installed.packages()


  # Prepare Initial Report
  ## Merge items based on the specified actions for the non-essential packages.
  Report = lapply(c("Depends","Imports","Suggests"), function(x){
    temp <- available.packages()[Package,x]

    temp  <- unlist(strsplit(paste0(temp),split = ","))
    tempclean <- trimws(temp) # Remove White Space
    tempclean  <- gsub("\n\\("," (",tempclean) # Remove other type of spacing

    df = data.frame(Name = gsub(" .*\\(.*", "\\1",tempclean))

    df$Type = ifelse(x == "Depends", "Depedent",
                     ifelse(x == "Imports", "Required",
                            ifelse(x == "Suggests", "Suggested","")))

    df$Version = trimws(ifelse(grepl(" \\(",tempclean),gsub(".*\\((.*?)\\).*", "\\1",tempclean),""))

    as.data.frame(df)
  })


  Report = do.call(rbind,Report)

  # Step 2: Find out what to install
  ToInstall <- Report$Name[Report$Name %in% All_Packs & !Report$Name %in% Installed]

  ## Report Existing Status
  Report$Installed = ifelse(Report$Name %in% Installed, "Yes", "No")

  ## Generate To-be-Installed list
  ### Remove Suggested Package from the list if needed
  if(isFALSE(Install_Suggests)) ToInstall = ToInstall[!ToInstall %in% Report$Name[Report$Version=="Suggested"]]

  ### Generate New Installation Report
  Report$Action = ifelse(Report$Name %in% ToInstall, "New Installation", "")

  ## R Version Report
  if("R" %in% Report$Name){
    Report = rbind(Report[Report$Name=="R",], Report[!Report$Name=="R",] )

    Report$Installed[Report$Name=="R"] = ""
  }


  # Step 3: Final Action and Installation
  if(length(ToInstall)>0){
    lapply(ToInstall,install.packages)
  } else {
    message("None to be installed.")
  }

  if(isFALSE(Silent)) return(Report)

}




