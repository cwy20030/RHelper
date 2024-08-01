
#' Setting up Setting Files


EnvSetUp = function(){
  if (!exists("RHSetting",envir = .GlobalEnv)) {

    message("Initiating RHelper Settings...")

    assign("RHSetting",list(),envir = .GlobalEnv)

    message("'RHSetting' is now created.")

  }
}

