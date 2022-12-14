% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CRA.R
\name{CRA}
\alias{CRA}
\title{A function that check and install missing dependencies for packages.}
\usage{
CRA(Package, Install_Suggests = FALSE, ...)
}
\arguments{
\item{Package}{The name of the package <e.g. "RHelper">}

\item{Install_Suggests}{Specify if the suggested libraries will be installed <default: FALSE>}
}
\value{
Import files into your working enviroment in R (i.e., Global Enviroment in R Studio)
}
\description{
The function - CRA, will check if the dependent packages required are installed in the system. It is particularliy helpful in the following scenarios.
1. Installing a "developing" libraries from GitHub
2. Sourcing an in-house library from the author
3. Check dependencies and repair missing libraries after downgrading/upgrading R environment without performing automatic transfer for libraries.
}
\examples{
CRA("ggplot2") ***It is very important to keep the double quotation marks!



}
