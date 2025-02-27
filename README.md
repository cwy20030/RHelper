# R/RHelper

![version](https://img.shields.io/badge/version-1.9.4-blue)
[![R-CMD-check](https://github.com/cwy20030/RHelper/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cwy20030/RHelper/actions/workflows/R-CMD-check.yaml)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/RHelper)](https://cran.r-project.org/package=RHelper)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](code_of_conduct.md)

<div align="center">
  <img src="RHelper-logos.jpeg" width="300px" />
</div>

## Description
RHelper is a "grocery store" library containing various functions designed to simplify/document coding in R. 

### For all users (including beginners)
RHelper has simplified various data management procedures into one-line commands. These include mapping folder contents (Clerk), bulk data importation across multiple formats (Importer), and organizing the global environment (e.g., Who_is and Merger). 

### For experienced users
RHelper features code logging (Historian), systematic documentation for data preparation (Lexicographer, Librarian) and cleaning up code environment (Butler). These functions can help users not only track their code execution but also improve reproducibility for future projects.  

## Installation
You can install the released version of RHelper from Github with the following code.
``` r
install.packages("devtools")
devtools::install_github("cwy20030/RHelper")
``` 

## Update
To update RHelper, please, run <i> update() </i>
``` r
library("RHelper")
update()
```
*** Note, if the installed RHelper version is too old, it may be necessary to reinstall it.

## Citation
To cite this package, please, run the following code in R.
``` r
citation("RHelper")
``` 

