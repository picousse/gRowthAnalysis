% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generic-functions.R
\name{\%>\%}
\alias{\%>\%}
\title{Pipe symbol}
\arguments{
\item{lhs, rhs}{A visualisation and a function to apply to it}
}
\description{
Pipe symbol
}
\examples{
# Instead of
layer_points(ggvis(mtcars, ~mpg, ~wt))
# you can write
mtcars \%>\% ggvis(~mpg, ~wt) \%>\% layer_points()
}
