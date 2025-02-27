#  File RHelper/R/ReOrder.R
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
#' @title ReOrder
#'
#' @description
#' A function to reorder data.frame by column or row
#'
#' @details
#' Based on the pre-specified order of column names or the levels of a factorial variable, ReOrder will reorganize the data.frame.
#'
#' @param Data a data.frame
#' @param Order To reorder data.frame by column, provide a vector of column names in the desired sequential order. To reorder data.frame by row, provide the name of the variable/column to be used to reorder the data.frame
#' @return a reordered data.frame
#' @keywords order reorder
#' @export
#'
#'


ReOrder = function(Data, Order) {

  # Check Data class
  if ( !inherits(Data, "data.frame") ){

    message(paste0(deparse(substitute(Data)) , " is not a data.frame.
                  Commencing conversion to a data.frame-class object."))

    Data = as.data.frame(Data)

  }




  # Deciding Task
  if (length(Order) == 1) {

    Task = "Row" # When there is only one variable specified.

  } else {

    Task = "Column"

  }







  # Reorder Column ---------
  if (Task == "Column") {

  ## Extract names of all columns
  ColName = names(Data)

  ## Identnify missing columns
  Missing = Order[!Order %in% ColName]

  if (length(Missing) > 0) Data[Missing] = NA

  ## Identnify 'new' columns
  NewCol = ColName[!ColName %in% Order]

  if (length(NewCol) > 0) Order = c(Order, NewCol)

  ## Reordered Data
  Data = Data[Order]
  }







  # Reorder Row ---------
  if (Task == "Row") {

    ## Check Point
    if (!Order %in% names(Data)) stop(paste0(deparse(substitute(Order)) , " is not in ", deparse(substitute(Data)), "!"))

    ## Get Reference
    ### Factorial
    if (inherits(Data[[Order]], "factor")) {

      Ref = c(levels(Data[[Order]]), NA, NaN)

      ### Other class
    } else if (inherits(Data[[Order]], c("character","string","POSIXct","POSIXt","numeric","double","integer"))) {

      Ref = sort(unique(Data[[Order]], na.last = T))

    }

    ## Order Data by Reference
    Data = Data[match(Ref, Data[[Order]]), ]

  }

  # Return Reordered Data
  return(Data)


}
