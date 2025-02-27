#  File RHelper/R/VDocument.R
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
#
#@param Data a data.frame
#@return A Filled Dictionary Template

VDocument = function(Data){


  ## Step 1: Go Through the Variables and Extract Information -----------
  ### Variable Class -------------------

  Class = sapply(Data,function(cl){

    # Check if the variable is all numbers
    ## If yes, turn them into numeric form
    if (all(grepl("^\\d+$", na.omit(cl)))) cl = as.numeric(as.character(cl))
    if (all(grepl("^\\d*\\.?\\d+$", na.omit(cl)))) cl = as.numeric(as.character(cl))


    # Non-character Variables
    if (!any(is.character(cl),is.factor(cl))) {

      UniqueValues = unique(na.omit(cl))

      ## Binary Charactersitics ------------------
      NUnique = length(UniqueValues)


      ## Check Ordinal Variables -------------------
      # Get differences between sorted unique values
      diffs = diff(sort(UniqueValues))


      if (NUnique == 2) { ## Binary
        "binary"

      } else if (all(diffs == diffs[1])) {   ## Ordinal:  Check if all differences are equal
        "ordinal/categorical"

      } else {
        "numeric"

      }

    } else {
      class(cl)
    }

  })



  #### Record Variables into the Dictionary Template --------------
  Variable = data.frame(matrix(nrow = length(names(Data)), ncol = 5))
  names(Variable)  = c("Variable", "Type",	"Unit",	"Definition", "Note")
  Variable[names(Variable)] = ""

  Variable$Variable = names(Data)
  Variable$Type = unname(Class)


  ### Unique Values -----------------------------
  UniqueValues = sapply(Data,function(cl){

    UniqueLevel = unique(cl)

    NAValue = UniqueLevel[which(is.na(UniqueLevel))]

    if (length(UniqueLevel) > 20) UniqueLevel = NAValue

    UniqueLevel

    })


  repLength = sapply(UniqueValues,length)


  ### Variable Names
  var = unlist(sapply(1:length(repLength), function(x){

            if (repLength[[x]] > 0) rep(names(repLength)[[x]],repLength[[x]])
  }))


  ### Variable Type
  # typ = unlist(sapply(1:length(repLength), function(x){
  #  if(repLength[[x]]>0) rep(Class[[x]],repLength[[x]])
  # }))



  #### Record Variables into the Dictionary Template --------------

  Value =  data.frame(matrix(nrow = length(var), ncol = 6))
  names(Value)  =   c("Variable",	"Type",	"Unit", "Value", "Definition", "Note")
  Value[names(Value)] = ""

  Value$Variable = names(unlist(UniqueValues))
  Value$Value = unname(unlist(UniqueValues))
  Value$Type = "Value"
  Value$Type[which(is.na(Value$Value))] = "Missing"


  ## Step 2: Create a Dictionary Template --------------

  Form = list(Variable = Variable,Value = Value)



  return(Form)

}
