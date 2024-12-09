% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/display_consort.R
\name{display_consort}
\alias{display_consort}
\title{Display consort}
\usage{
display_consort(consort, fdim = list(width = 9, height = 5, dpi = 600))
}
\arguments{
\item{consort}{An object with class Consort.}

\item{fdim}{A list of 3 elements 'width', 'height' and 'dpi', respectively, with 3 numeric values associated. Only used for type = 'F'.}
}
\value{
A consort.
}
\description{
Display consort
}
\examples{
x <- create_consort(100, 150) |>
 add_box('box1', x = c(10, 5), y = c(50, 3), align = 'c', text = 'Box 1') |>
 display_consort()
}
