% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_text.R
\name{clean_text}
\alias{clean_text}
\title{Clean text files to facilitate citation case extraction.}
\usage{
clean_text(folder, number = NULL, encoding = "ASCII")
}
\arguments{
\item{folder}{Name of folder within working directory in which the citing documents (.txt files) are located, e.g. "Beck 1995".}

\item{number}{Number of .txt files in folder the function should be applied to. Default is "all .txt files in folder".}
}
\value{
Returns and saves text files that have been cleaned (e.g. abbreviations replaced)
}
\description{
Clean text files to facilitate citation case extraction.
}
\examples{
\dontrun{
 setwd("C:/Users/paul/Google Drive/Research/2016_Quality_of_citations/data")
 folder <- "Acemoglu 2001"
 clean_text(folder)
}
}
