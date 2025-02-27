#  File RHelper/R/Inject.Row.R
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
#
#' @title Inject.Row
#'
#' @description
#' A dynamic function to allow users to add rows to specific location within a data.frame with or without values.
#'
#'
#' @param df A data.frame
#' @param Index A vector of index for either row ID or sequential ID of a vector
#' @param Variable A variable or a vector of variable names where to values are to be modified.
#' @param Value A value or a vector of values to be added.
#' @return A new vector with values injected
#' @keywords add, inject
#' @export
#' @examples
#'
#' # To inject empty rows into the sleep data
#' Test = Inject.Row(sleep, sample(1:20,5))
#'
#' # To inject specific value at specific variable into the sleep data
#' Test1 = Inject.Row(sleep, sample(1:20,5), Variable = "group", Value = 1)
#'
#' # To dynamically inject values at variable
#' Test2 = Inject.Row(sleep, sample(1:20,3), Variable = names(sleep),
#'                    Value = c("a","b","c"))
#'
#'
#'

Inject.Row = function(df, Index, Variable = NULL, Value = NULL) {


  # Check if the to-be-injected value should be duplicated
  if (is.null(Value)) Value = NA
  if (length(Value) == 1) Value = rep(Value, length(Index))

  # Check if the to-be-injected Variable should be duplicated
  if (length(Variable) %in% c(0,1)) Variable = rep(Variable, length(Index))


  # Warning and Check Points
  ## When the two vectors (i.e., Variable and Value) do not share the same length
  if (!is.null(Variable))
  if (!length(Variable) == length(Value)) stop("For dynamic injection, the vector of variable names should have the same length as the values.")

  ## When the Value is not a missing value but no variable name is specified.
  if (all(is.null(Variable), !all(is.na(Value)))) warning("No variable specified. All variables at the injected row will share the same value.")


  # Check df class
  if (!inherits(df,c("table","matrix"))) df = as.data.frame(df)

  # Create Empty data.frame template
  Template = data.frame(matrix(nrow = 1, ncol = ncol(df)))
  names(Template) = names(df)


  Head = FALSE
  HeadValue = NULL

  Tail = FALSE
  TailValue = NULL

  # Check if to inject value before the first value

  Index = sort(Index)

  if (1 %in% Index) {

    Head = TRUE
    HeadValue = Value[Index == 1]
    HeadVariable = Variable[Index == 1]

    Index = Index[!Index == 1]
    Value = Value[!Index == 1]
    Variable = Variable[!Index == 1]
  }

  # Check if to inject value after the last value
  if (max(Index) > nrow(df)) {

    Tail = TRUE
    TailValue = Value[Index > nrow(df)]
    TailVariable = Variable[Index > nrow(df)]

    Index = Index[!Index > nrow(df)]
    Value = Value[!Index > nrow(df)]
    Variable = Variable[!Index > nrow(df)]
  }


  # Create the beginning and the end index
    Beginning = c(1, Index)

    End = c((Index - 1), nrow(df))




 Out = lapply(1:length(Index), function(i) {

          Temp = Template

          if (!is.null(Variable)) {

            Temp[[Variable[[i]]]] = Value[[i]]

          } else {
            Temp[1:length(Temp)] = Value[[i]]
            }

          rbind(df[Beginning[[i]]:End[[i]], ], Temp)

        })



 Out = do.call(rbind, Out)

 IndexLN = length(Index)
 Out = rbind(Out, df[Beginning[[IndexLN + 1]]:End[[IndexLN + 1]],])


 if (Head) {
   Temp = Template

   if (!is.null(HeadVariable)) {

     Temp[[HeadVariable]] = HeadValue

     } else {

      Temp[1:length(Temp)] = HeadValue

     }


   Out = rbind(Temp, Out)
 }

 if (Tail) {

   Temp = Template

   if (!is.null(TailVariable)) {

     Temp[[TailVariable]] = TailValue

   } else {

     Temp[1:length(Temp)] = TailValue

   }

   Out = rbind(Out, TailValue)

 }


 return(Out)


}
