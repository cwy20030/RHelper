
.onAttach <- function(libname, pkgname) {

  packageStartupMessage(

"
      _ _ _    ____   ____
    ||  _  ||   ||     ||
    || |_| ||   ||     ||   _ _ _           _ _ _   _ _ _   _ _ _
    ||_ _ _||   ||- - -||  |       |       |    |  |       |     |
    ||   \\\\     ||     ||  |_ _    |       |_ _ |  |_ _    |- - -|
    ||    \\\\    ||     ||  |       |       |       |       |   \\
   _||_   _\\\\_ _||_   _||_ |_ _ _  |_ _ _  |       |_ _ _  |    \\

======================================================================
                             Version 1.9.3
"

  )
}




.onLoad <- function(libname, pkgname) {
  if (getRversion() >= "3.1.0") {
    utils::globalVariables(c("RHSetting"))
  }
}





.onUnload = function(libname, pkgname){

  if(getRversion() >= "3.1.0")  EnvSetUp()


  if (exists("RHSetting",envir = .GlobalEnv)) {

    message("Removing RHelper Setting...")

    rm(RHSetting,envir = .GlobalEnv)

    message("'RHSetting' is now removed
            Thank you for using RHelper
            Please, remember to cite our package if used in publication.")

  }
}

