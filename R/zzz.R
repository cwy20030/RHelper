
.onAttach <- function(libname, pkgname) {



    cat("
      _ _ _    ____   ____
    ||  _  ||   ||     ||
    || |_| ||   ||     ||   _ _ _           _ _ _   _ _ _   _ _ _
    ||_ _ _||   ||- - -||  |       |       |    |  |       |     |
    ||   \\\\     ||     ||  |_ _    |       |_ _ |  |_ _    |- - -|
    ||    \\\\    ||     ||  |       |       |       |       |   \\
   _||_   _\\\\_ _||_   _||_ |_ _ _  |_ _ _  |       |_ _ _  |    \\

======================================================================
                                Version 1.9
"

  )
}


.onUnload = function(libname, pkgname){

  if (exists("RHSetting",envir = .GlobalEnv)) {

    message("Removing RHelper Setting...")

    rm(RHSetting,envir = .GlobalEnv)

    message("'RHSetting' is now removed
            Thank you for using RHelper
            Please, remember to cite our package if used in publication.")

  }
}

