#' A function listing out variables to faciliate writing codes or functions
#'
#' This is a simple function that will print out values (e.g., strings, names or
#' numbers) while keeping the quotation marks, which were otherwise not
#' retained via paste or cat. This is particularly useful when manually
#' organizing data files or passing co-variates while performing regression
#' models. It can also be helpful when moving large numbers of files via Bash
#' shell (Linux)
#'
#' @param x Values to be passed through and printed out in a string with quotes.
#' @param Delim Deliminators to be used to separate individual items within the value x.
#' @return A long string with characters in quotation marks
#' @export
#' @examples
#' Naming_Variables(names(iris))
#' #"Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species"
#'
#' Naming_Variables(ls())
#' # List everything stored in the global environment
#' # (i.e., within the "Environment" tab)
#'
#' Naming_Variables(list.files(getwd()))
#' # List all the files within the current working directory
#'
#'


Naming_Variables <- function(x, Delim = ","){
  return(cat(paste0(shQuote(x),collapse = Delim)))
}
