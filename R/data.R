#' Subjective sleep report of 100 participants
#'
#' This dataset contains SIMULATED sleep reports of 100 participants interviewed in 1994.
#' Each row contains the information of an "interviewee" about their psychological distress
#' level in the past month and experiences of insomnia and apnea. Besides the purpose of
#' helping users familiarize with the RHelper package. The simulated dataset can also be used
#' as a practical tool for practicing/training research analysis in R.
#'
#' "Research" Questions:
#' 1. Is there an association between apnea symptom and participants' psychological distress level?
#' 2. Is biological sex a modifier for the association between age and insomnia?
#'
#'
#' @docType data
#' @keywords datasets
#' @name NCSM_205_1994_Modified_K
#' @usage data(NCSM_205_1994_Modified_K)
#' @format A simulated data with 100 rows and 5 variables:
#' \describe{
#' \item{Age}{Age at the recuitment}
#' \item{Sex}{Participant's biological sex}
#' \item{K10}{Psychological distress scale}
#' \item{Insomnia}{Insomnia symptom status}
#' \item{Apnea}{Apnea symptom status}
#' }
#'
"NCSM_205_1994_Modified_K"


#' A meta list of 20 waves of artificial sleep report following the NCSM_205_1994_Modified_K simulated dataset
#'
#' A 20-item data.list containing simulated datasets, which follow the same design as the
#' NCSM_205_1994_Modified_K simulated dataset.
#' Within each data.frame stored under the data.list, each row contains the information of an "interviewee"
#' about their psychological distress level in the past month and experiences of insomnia and apnea. Besides the
#' purpose of helping users familiarize with the RHelper package. The simulated dataset can also be used
#' as a practical tool for practising/training research analysis in R.
#'
#' Data Management Training Questions:
#' 1. Do all data.frames stored under the SimSleep data.list, have the same variables?
#' 2. How many waves of interview was conducted for each year?
#'
#' "Research" Questions:
#' 1. Assume the interviewees were randomly selected from the general public, is there a change in association
#' between psychological distress level and insomnia symptoms across years?
#' 2. Is there a potential bias in participant sampling for a specific year?
#'
#' @docType data
#' @keywords datalist
#' @name SimSleep
#' @usage data(SimSleep)
#' @format 20 simulated data with 100 rows and 5 variables:
#' \describe{
#' \item{NCSM_103_2021_Modified_K}{NCSM simulated interview response conducted in year 2021}
#' \item{NCSM_153_2021_Modified_K}{NCSM simulated interview response conducted in year 2021}
#' \item{NCSM_175_2005_Modified_K}{NCSM simulated interview response conducted in year 2005}
#' \item{NCSM_205_1994_Modified_K}{NCSM simulated interview response conducted in year 1994}
#' \item{NCSM_209_1989_Modified_K}{NCSM simulated interview response conducted in year 1989}
#' \item{NCSM_219_1991_Modified_K}{NCSM simulated interview response conducted in year 1991}
#' \item{NCSM_250_2007_Modified_K}{NCSM simulated interview response conducted in year 2007}
#' \item{NCSM_255_2013_Modified_K}{NCSM simulated interview response conducted in year 2013}
#' \item{NCSM_272_2019_Modified_K}{NCSM simulated interview response conducted in year 2019}
#' \item{NCSM_288_1985_Modified_K}{NCSM simulated interview response conducted in year 1985}
#' \item{NCSM_309_2000_Modified_K}{NCSM simulated interview response conducted in year 2000}
#' \item{NCSM_310_1991_Modified_K}{NCSM simulated interview response conducted in year 1991}
#' \item{NCSM_319_1990_Modified_K}{NCSM simulated interview response conducted in year 1990}
#' \item{NCSM_337_1987_Modified_K}{NCSM simulated interview response conducted in year 1987}
#' \item{NCSM_391_1999_Modified_K}{NCSM simulated interview response conducted in year 1999}
#' \item{NCSM_424_1991_Modified_K}{NCSM simulated interview response conducted in year 1991}
#' \item{NCSM_431_1993_Modified_K}{NCSM simulated interview response conducted in year 1993}
#' \item{NCSM_462_1988_Modified_K}{NCSM simulated interview response conducted in year 1988}
#' \item{NCSM_471_2009_Modified_K}{NCSM simulated interview response conducted in year 2009}
#' \item{NCSM_478_2021_Modified_K}{NCSM simulated interview response conducted in year 2021}
#' \item{Age}{Age at the recuitment}
#' \item{Sex}{Participant's biological sex}
#' \item{K10}{Psychological distress scale}
#' \item{Insomnia}{Insomnia symptom status}
#' \item{Apnea}{Apnea symptom status}
#' }
#'
"SimSleep"



