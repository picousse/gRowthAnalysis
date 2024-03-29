% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyse.R
\name{calc.exp.time}
\alias{calc.exp.time}
\title{calculate the exponential time of the curve. This is done by fitting a spline, and calculating when the first derivative goes below 0.}
\usage{
calc.exp.time(df = NULL, input_time = "time", input_value = "value",
  treshold = 0.3, lambda = 1e-05)
}
\arguments{
\item{df}{(dataframe). Dataframe containing the data}

\item{input_time}{(string). Column name that contains the time value (in hours!). (Default = "time")}

\item{input_value}{(string). Column name that contains the measured value. (Default = "value")}

\item{treshold}{(double). End of exponential time is only calculated if this treshold has been surpassed. This mainly filters out the noise often seen in the beginnen of the curce.}

\item{lamda}{(double), Smoothing paraneter. this needed to filter out outliers.}
}
\value{
double. End of the exponential phase
}
\description{
calculate the exponential time of the curve. This is done by fitting a spline, and calculating when the first derivative goes below 0.
}
\author{
Pieter Coussement, \email{coussementpieter@gmail.com}
}
