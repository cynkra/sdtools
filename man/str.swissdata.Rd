% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.R
\name{str.swissdata}
\alias{str.swissdata}
\title{Compact Display of the Swissdata Object}
\usage{
\method{str}{swissdata}(object, language = "en", ...)
}
\arguments{
\item{object}{swissdata object}

\item{language}{language of the labels to be used for displaying the structure}

\item{...}{further arguments passed to or from methods}
}
\value{
Does not return anything but displays the swissdata structure.
}
\description{
Compactly displays the internal hierarchy and labels of swissdata objects.
}
\examples{
z <- adecco
str(z)
str(z, language = "de")

}
\author{
Karolis Koncevičius
}
