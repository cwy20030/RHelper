# R/RHelper

![version](https://img.shields.io/badge/version-1.9.2-blue)
[![R-CMD-check](https://github.com/cwy20030/RHelper/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cwy20030/RHelper/actions/workflows/R-CMD-check.yaml)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/RHelper)](https://cran.r-project.org/package=RHelper)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](code_of_conduct.md)


<div align="center">
  <img src="RHelper-logos.jpeg" width="300px" />
</div>

## Description
RHelper is a "grocery store" library containing various functions designed to simplify/document coding in R. 

### For all users (including beginners)
RHelper has simplified various data management procedures into one-line commands. These include mapping folder contents (<i>Clerk</i>), bulk data importation across multiple formats (<i>Importer</i>), and organizing the global environment (e.g., <i>Who_is</i>, <i>ListTree</i> and <i>Merger</i>). 

### For experienced users
RHelper features code logging (<i>Historian</i>), systematic documentation for data preparation (<i>Lexicographer</i>, <i>Librarian</i>) and cleaning up code environment (<i>Butler</i>). These functions can help users not only track their code execution but also improve reproducibility for future projects.  

## Installation
You can install the released version of RHelper from Github with the following code.

``` r
install.packages("devtools")
devtools::install_github("cwy20030/RHelper")
``` 

## How to update RHelper?
To update the full-feature package, hosted on GitHub, use the internal update function. 

``` r
RHelper::update()
``` 


