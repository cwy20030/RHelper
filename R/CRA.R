#' A function that check and install missing dependencies for packages.
#'
#' The function - CRA, will check if the dependent packages required are installed in the system. It is particularliy helpful in the following scenarios.
#' 1. Installing a "developing" libraries from GitHub
#' 2. Sourcing an in-house library from the author
#' 3. Check dependencies and repair missing libraries after downgrading/upgrading R environment without performing automatic transfer for libraries.
#'
#' @import readxl haven readr
#' @param Package The name of the package <e.g. "RHelper">
#' @param Install_Suggests Specify if the suggested libraries will be installed <default: FALSE>
#' @return Import files into your working enviroment in R (i.e., Global Enviroment in R Studio)
#' @export
#' @examples
#' CRA("ggplot2") ***It is very important to keep the double quotation marks!
#'
#'
#'



CRA <- function(Package, Install_Suggests=FALSE,...){


  All_Packs <- available.packages()[,"Package"]
  Installed <- installed.packages()


  DP <- available.packages()[Package,"Depends"]
  IM <- available.packages()[Package,"Imports"]
  SG <- available.packages()[Package,"Suggests"]



  if(isTRUE(Install_Suggests)){
    All <- c(DP,IM,SG)
  } else {
    All <- c(DP,IM)
  }



  All <- unlist(strsplit(paste0(All),split = ","))
  All.1 <- unlist(strsplit(paste0(All),split = " "))
  All.2 <- unlist(strsplit(paste0(All.1),split = "[[:punct:]]"))
  All.Final <- All.2[grepl("^[A-Za-z]",All.2)]

  To.Install <- All.Final[All.Final %in% All_Packs & !All.Final %in% Installed]

  if(length(To.Install)>0){
    lapply(To.Install,install.packages)
  } else {
    message("None to be installed.")
  }




}




