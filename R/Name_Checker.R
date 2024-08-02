#' A Function to Check for Potentially Problematic Variable Names and Correct Them
#'
#' This function will loop over all data frames in the work environment. If a dataframe starts with a digit or a
#' punctuation, the pipeline will add a "x" in front of it so the user will not need to wrap `` around
#' the name of a data.frame during computation.
#'
#' @importFrom stringi stri_trans_general
#' @param Names The name of variables/columns within a dataframe.
#' @param Silent A logical indicator for reporting summary.
#' @return 'Corrected' variable names
#' @export
#' @examplesIf interactive()
#'
#' # Create a dummy data.frame
#' df = data.frame(x = rnorm(100))
#'
#' df$`Hours in Bed` = abs(rnorm(100)*10)
#' df$`10 Hrs of Sleep ` = factor(ifelse(df$`Hours in Bed`>10,"Yes","No"))
#' df$`Subject who Forget to Sign Agreement!` = factor(rbinom(n = 100,size = 1,prob = 0.1))
#'
#' Name_Checker(names(df))
#'
#'
#' # The same principle can be applied to correct data.list documentation.
#'
#'
#'

Name_Checker <- function(Names, Silent = TRUE ){
  if (!require("stringi",character.only = TRUE)) stop("Package stringi not found")


  Names <- stri_trans_general(Names,id = "Latin-ASCII")

  # Prepare Output Field
  NewNames = Names

  # Process
  for (i in Names) {
    # Remove unwanted symbols
    Punct <- unlist(gregexpr('[[:punct:]]', i))
    if (!-1 %in% Punct) i <- gsub("[[:punct:]]", "\\s", i)


    # Deal with Spacing
    i = trimws(i) # Remove Space at the begining and at the end of the string

    Space <- grepl("\\s", i)
    if (isTRUE(Space)) {
      i_n <- gsub("\\s","_",i)
    } else {
      i_n <- i
    }


    # Add a cap to the number beginning string
    Alphb <- unlist(gregexpr('[[:alpha:]]', i_n))
    if (!1 %in% Alphb) i_n <- paste0("x",i_n)


    # Return Results

    NewNames[NewNames %in% i] <- i_n
  }

 return(NewNames)

  if (!Silent) message(table(Original_Name = Names, New_Name = NewNames))
}

