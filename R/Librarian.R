#' A function to help prepare a dataset for analysis
#'
#' Based on the dictionary prepared by Lexicographer, Librarian will prepare the variables within a dataset ready for analysis.
#'
#' @param Data a data.frame
#' @param Directory Directory for the project
#' @param Missing_Identifier Values identified as missing
#' @param KeyExclude Values to be excluded
#' @param NewName Name name for the modified data.
#' @return Import files into your working enviroment in R (i.e., Global Enviroment in R Studio)
#' @keywords librarian
#' @export
#'
#'


Librarian <- function(Data, Dictionary = NULL, Missing_Identifier = NA, KeyExclude = NULL, NewName = NULL,...){

  if (is.null(Dictionary)) {
    Lexicographer(Data = Data)
    Dictionary = get(Dictionary, envir = .GlobalEnv)
  }

# Handling Missing Value ---------------------
for (i in names(Data)) {

  Dictionary$Value$Type[Dictionary$Value$Value %in%  Missing_Identifier & Dictionary$Value$Variable == i] = "Missing"

  Missing = Dictionary$Value$Value[Dictionary$Value$Type == "Missing" & Dictionary$Value$Variable == i]

  Data[[i]][Data[[i]] %in% Missing] = NA

  if (!is.null(KeyExclude)) Data[[i]][Data[[i]] %in% KeyExclude] = NA
}


# Handling Factor ---------------------
  factor_list <- Dictionary$Variables[Dictionary$Variable$Type %in% c("factor","ordinal/categorical","binary")]
  for (i in factor_list) Data[[i]] = as.factor(as.character(Data[[i]]))


  # Handling Numeric---------------------
  Numeric_list <- Dictionary$Variables[Dictionary$Variable$Type %in% c("numeric","integer")]
  for (i in Numeric_list) Data[[i]] = as.numeric(Data[[i]])

Not_in_Ditionary <- names(Data)[!names(Data) %in% Dictionary$Variable$Variable]
if (!is.null(Not_in_Ditionary)) print(paste0("The following variables were not found in your dictionary:",Not_in_Ditionary))


if (is.null(NewName))  NewName = deparse(substitute(Data))

  assign(NewName,value = Data,.GlobalEnv)
 }
