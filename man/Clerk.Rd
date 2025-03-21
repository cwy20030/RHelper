% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Clerk.R
\name{Clerk}
\alias{Clerk}
\title{Clerk}
\usage{
Clerk(Dirs)
}
\arguments{
\item{Dirs}{Directory of where the files that you want to document <e.g. "C:/Users/\emph{\strong{YOUR USERNAME}}/UPSTREAM FOLDER/.../FOLDER NAME/">}
}
\value{
Import files into your working enviroment in R (i.e., Global Enviroment in R Studio)
}
\description{
A function to automatically document files' properties contained in the directery/path specified.
}
\details{
This function lists and documents all files contained within one or more directories (i.e., computer paths to folders of interest) specified by the user. This is suitable if one wish to import files from multiple folders into a data.list for analysis.
}
\examples{

HOME = Sys.getenv("HOME")

print(HOME)

Clerk(HOME)

}
\keyword{Import,}
\keyword{Read}
