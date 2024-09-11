#' A function to inject specified values based on the index
#'
#' By providing the index (e.g., sequential ID of a vector or rowID of a
#' data.frame/table), the function will add the specified values between i
#' indice and the indice before it.
#'
#' @param x A vector of numeric/integer values
#' @param Index A vector of index for either row ID or sequential ID of a vector
#' @param Value A value or a vector of values to be added.
#' @return A new vector with values injected
#' @keywords add, inject
#' @export
#' @examples
#'
#' # An example to inject NA into the vector x
#' x = sample(c(1:10), 1000, replace = TRUE)
#' ID = sample(1:1000, 10)
#'
#' x1 = Inject(x, Index = ID, Value = NA)
#'
#' # An example to inject specific values into the vector x
#' x2 = Inject(x, Index = ID, Value = sample(100:1000, 10))
#'
#'
#' print(x1[ID + 1:length(ID)])
#' print(x2[ID + 1:length(ID)])
#'

Inject = function(x, Index, Value) {


  # Check if the to-be-injected value should be duplicated
  if (length(Value) == 1) Value = rep(Value, length(Index))


  Head = FALSE
  HeadValue = NULL

  Tail = FALSE
  TailVALUE = NULL

  # Check if to inject value before the first value
  if (1 %in% Index) {

    Head = TRUE
    HeadValue = Value[Index == 1]

    Index = Index[!Index == 1]
    Value = Value[!Index == 1]
  }

  # Check if to inject value after the last value
  if (max(Index) > length(x)) {

    Tail = TRUE
    TailValue = Value[Index > length(x)]

    Index = Index[!Index > length(x)]
    Value = Value[!Index > length(x)]
  }


  # Create the beginning and the end index
    Beginning = c(1, Index)

    End = c((Index - 1), length(x))




 Out =  unlist(lapply(1:length(Index), function(n) {

          c(x[Beginning[[n]]:End[[n]]], Value[[n]])

        }))


 # Add the last epoch back into the vector
 IndexLN = length(Index)
 Out = c(Out, x[Beginning[[IndexLN + 1]]:End[[IndexLN + 1]]])


 if (Head) Out = c(HeadValue, Out)

 if (Tail) Out = c(Out, TailValue)


 return(Out)


}
