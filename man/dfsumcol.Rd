% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rrepast-helper.R
\name{dfsumcol}
\alias{dfsumcol}
\title{dfsumcol}
\usage{
dfsumcol(d, lst = c(), invert = FALSE)
}
\arguments{
\item{d}{The data frame}

\item{lst}{Skip columns included. Sum columns NOT included}

\item{invert}{Sum only the columns included in \code{lst}}
}
\value{
The original data frame with a new column (sum) holding the sum
}
\description{
Sum data frame columns but tho
}
