#  File RHelper/R/Butler.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2025  C. William Yao, PhD
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
#' @title Butler
#'
#' @description
#' A function to help keeping the global environment organized
#'
#' @import utils methods
#' @param Task A logical operator to assign one of the pre-specified tasks to the butler (this function). The tasks include
#' 1. "Book", to document all the items in the current global environment
#' 2. "Clean", to clean up everything not documented in the butler's book (a.k.a., BBook). The Butler will also update the BBook by removing manually deleted objects previously documented.
#' 3. "Clear", to clear the output and the console.
#' @param Keep A place-holding text vector, that allows users to specify the new/undocumented items to keep while cleaning. Besides the names of the items, users may also passed class categories to restrict the cleaning range. For instance, when "table" is passed, the Butler will keep everything previous documented and any new table-class item.
#' @keywords clean document
#' @export
#' @examples
#'
#' # First, show the variables listed in the global environment
#' ListAll = ls()
#'
#' ## If the Global Environment is clean, then create some things
#' if(length(ListAll)<1){
#'
#'   data("SimSleep") # A data.list
#'
#'   WD = getwd() # A Directory
#'
#'   Date = Sys.Date() # A Variable
#'
#'   Hello_World = function(){ print("Hello_World") } # A Function
#' }
#'
#' # Generate a Documentation, called "BBook", which stands for Butler's Book.
#' Butler(Task="Book")
#'
#' # Add new items to the environment
#' data("iris")
#'
#' AVector = stats::rnorm(100)
#'
#' # Clean-up and Go to the Documented Environment
#' Butler(Task="Clean") # To keep certain "new" items, specify the name(s) of
#' # the object(s) or item class(es) via Keep.
#'
#'
#'



Butler = function(Task,Keep=NULL){



  # Check Pre-requisit --------
  for (x in c("methods", "utils"))
    if (!requireNamespace(x, quietly = T)) {
      install.packages(x)
      requireNamespace(x, quietly = T)
    }

  CANTFind = NULL   # Place Holder

  # Clear ------------
  if (toupper(Task) == "CLEAR") cat("\014")


  # Booking ------------
  if (toupper(Task) == "BOOK") {
    if (is.null(Keep)) {
      assign("BBook",value = ls(envir = .GlobalEnv),envir = .GlobalEnv)
    } else {
      ## Add Additional Items to Document -----------
      ToDocument = Keep[Keep %in% ls(envir = .GlobalEnv)]
      CANTFind = Keep[!Keep %in% ls(envir = .GlobalEnv)]
      assign("BBook",value = ToDocument,envir = .GlobalEnv)
    }
  }


  # Cleaning ---------
  if (toupper(Task) == "CLEAN") {

    ## First check the existance of Butler's Book (a.k.a., BBook) --------
    if (!"BBook" %in% ls(envir = .GlobalEnv)) stop("The newly hired Butler has never documented any items in the Global Environment.
                                  Please, ask the Buatler to Book items specified in Keep before cleaning.")


    ## Retrieve Butler's Book----
    BBook = get("BBook",envir = .GlobalEnv)

    ## List Everything Stored ------

    AllItems = ls(envir = .GlobalEnv)
    AllItems = AllItems[!AllItems %in% "BBook"]
    BBook = BBook[BBook %in% AllItems]
    ## Next, Handling Keep Parameters ------

    if (!is.null(Keep)) {

      ### Add Keep Items' Names ----------
      ToAdd = Keep[Keep %in% AllItems]
      CANTFind = Keep[!Keep %in% AllItems]


      ### Check for class items ------
      if (any(isClass(Keep))) {
        CLASS = names(which(sapply(Keep,isClass)))

        ItemClass = sapply(AllItems,function(x){
          class(get(x,envir = .GlobalEnv))
        })

        ToAdd = AllItems[which(ItemClass %in% CLASS)]

        #### Update BBook and Keep --------
        BBook = c(BBook,ToAdd)
        Keep = Keep[!Keep %in% CLASS]
      }

      ### Add Keep Items' Names ----------
      ToAdd = Keep[Keep %in% AllItems]
      CANTFind = Keep[!Keep %in% AllItems]

      #### Update BBook --------
      BBook = c(BBook,ToAdd)
    }

    ## Commence Cleaning
    assign("BBook",value = BBook,envir = .GlobalEnv)
    rm(list = AllItems[!AllItems %in% BBook],envir = .GlobalEnv)

    gc()
  }


  if (length(CANTFind) > 0)
    warning(paste0("Some items were not documented because the Butler cannot find them. Please, check the spelling for ",paste0(CANTFind,collapse = ", "),"."))



}


