#' A Function to bind data.frame by row
#'
#' This function is an extension rbind (i.e., row bind or merge) without the traditional limits of column names and their orders. Users can choose to either keep only the columns with the same names ("SAME"), only based on one of the two data.frames ("x", "y", "either") or all existing columns ("ALL").
#'
#'
#' @param DFx First data.frame
#' @param DFy Second data.frame
#' @param Keep A logical indicator for the rwo binding condition. There are Five indicators pre-programmed: "SAME" (keeping only the shared variables), "x" (keeping only the variables existed in the first data.frame), "y" (keeping only the variables existed in the second data.frame), "either" (keeping only the variables existed in one but not both data.frame) and "ALL" (keeping all available varialbes found in either data.frame)
#' @return a new row bond data.frame
#' @keywords bind, row, rbind, RowBind
#' @export
#' @examplesIf interactive()
#'
#' # Create Sample Data
#' data("sleep")
#'
#' # Extract variable names
#' Variables = names(sleep)
#'
#' # Split the data by half based on the extra hours of sleep
#' sleep.1 = subset.data.frame(sleep,subset = sleep$extra<2)
#' # less than 2-hour increase on total sleep time
#' sleep.2 = subset.data.frame(sleep,subset = sleep$extra>=2)
#' # more than 2-hour increase on total sleep time
#'
#' # Generate a variable in sleep.1 dataset to identify negative changes in
#' # sleep time
#' sleep.1$SleepReduction  = factor(ifelse(sleep.1$extra < 0, 1, 0))
#'
#' # First, try using the base code rbind (remove the # sign before executing
#' # the following line.)
#' #rbind(sleep.1,sleep.2)
#'
#' # Now, let's try RowBind
#' RowBind(sleep.1,sleep.2)
#'


RowBind = function(DFx, DFy, Keep = "SAME" ) {

  # Prepare Process -----

  Namesx = names(DFx)
  Namesy = names(DFy)

  ## Van Diagram -----------
  Common_Name = Namesx[Namesx %in% Namesy] # Same names
  Only_x = Namesx[!Namesx %in% Namesy] # Names exisitng in only DFx
  Only_y = Namesy[!Namesy %in% Namesx] # Names exisitng in only DFy
  Either_xy = c(Only_x,Only_y) # Names exisitng in either DFx or DFy


  ### Prepare overlapped columns -----------
  if (!toupper(Keep) == "EITHER")  DFSame = rbind(DFx[Common_Name],DFy[Common_Name])

  if (length(Common_Name) == 0) warning("No common columns detected.")



  ### Prepare non-overlapped columns ------------
  if (toupper(Keep) %in% c("EITHER","X","Y","ALL")) {
    DFy[Only_x] = NA
    DFx[Only_y] = NA
  }


  # Prepare Output ------
  ## SAME -------
  if (toupper(Keep) == "SAME") {
    DFOut = DFSame

  ## Either ----------
  } else if (toupper(Keep) == "EITHER") {
    DFOut = rbind(DFx[Either_xy],DFy[Either_xy])

  ## Only x ----------
  } else if (toupper(Keep) == "X") {
    DFeither = rbind(DFx[Only_x],DFy[Only_x])
    DFOut = cbind(DFSame,DFeither)

  ## Only y ----------
  } else if (toupper(Keep) == "Y") {
    DFeither = rbind(DFx[Only_y],DFy[Only_y])
    DFOut = cbind(DFSame,DFeither)

  ## All Variables --------
  } else if (toupper(Keep) == "ALL") {
    DFeither = rbind(DFx[Either_xy],DFy[Either_xy])
    DFOut = cbind(DFSame,DFeither)
  }



  # Return ---------
  return(DFOut)

}
