#' A function to help prepare a dataset for analysis
#'
#' Based on the dictionary prepared by Lexicographer, Librarian will prepare the variables within a dataset ready for analysis.
#'
#' @param Data a data.frame
#' @param Dictionary the Dictionary to be used as a reference and data.frame preparation. If not specified, a new Dictionary will be created.
#' @param Missing_Identifier Values identified as missing
#' @param KeyExclude Values to be excluded
#' @param NewName Name name for the modified data.
#' @return Import files into your working enviroment in R (i.e., Global Enviroment in R Studio)
#' @keywords librarian
#' @export
#'
#'


Librarian <- function(Data, Dictionary = NULL, Missing_Identifier = NA, KeyExclude = NULL, NewName = NULL ){

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


  # Change the font of the letters in Variable type
  Dictionary$Variable$Type = tolower(Dictionary$Variable$Type)

# Handling Factor ---------------------
  factor_list <- Dictionary$Variables[Dictionary$Variable$Type %in% c("factor","ordinal/categorical","binary")]
  for (i in factor_list) Data[[i]] = as.factor(as.character(Data[[i]]))

  # Handling Ordinal Data ---------------------
  Ordinal_list <- Dictionary$Variables[Dictionary$Variable$Type %in% c("ordinal")]
  for (i in Ordinal_list) {

    typ = tolower(Dictionary$Value$Type[Dictionary$Value$Variable %in% i])
    # Set Reference level and order
    if ("ref" %in% typ){ # When Reference is specified.
      LEV = Dictionary$Value$Value[Dictionary$Value$Variable]
      REF = Dictionary$Value$Value[Dictionary$Value$Variable %in% i & typ == "ref"]
      Levels = c(REF, LEV[!LEV %in% REF])
    } else {  # When Reference is NOT specified.
      LEV = levels(factor(as.character(Data[[i]])))
      REF = LEV[1]
      Levels = c(REF, LEV[!LEV %in% REF])
    }

    message(paste0( REF, " is set as the reference level for ", i))

    Data[[i]] = factor(as.character(Data[[i]]),levels = Levels, ordered = T)
  }

  # Handling Numeric---------------------
  Numeric_list <- Dictionary$Variables[Dictionary$Variable$Type %in% c("numeric","integer")]
  for (i in Numeric_list) Data[[i]] = as.numeric(Data[[i]])

Not_in_Ditionary <- names(Data)[!names(Data) %in% Dictionary$Variable$Variable]
if (!is.null(Not_in_Ditionary)) print(paste0("The following variables were not found in your dictionary:",Not_in_Ditionary))


if (is.null(NewName))  NewName = deparse(substitute(Data))

  assign(NewName,value = Data,.GlobalEnv)
 }
