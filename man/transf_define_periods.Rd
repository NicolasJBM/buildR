% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transf_define_periods.R
\name{transf_define_periods}
\alias{transf_define_periods}
\title{Define periods}
\usage{
transf_define_periods(x, range = 2, cutoff = c(-0.5, 0.5))
}
\arguments{
\item{x}{Numeric. Vector of chronological values to be transformed into periods}

\item{range}{Integer. Number of years after the focal value used to compute slopes.}

\item{cutoff}{Double. Absolute slope below which the period is considered stable.}
}
\value{
A tibble with phases identified: phase, level, evolution.
}
\description{
Find period characteristics: phase, level, evolution.
}
\author{
Nicolas Mangin
}
