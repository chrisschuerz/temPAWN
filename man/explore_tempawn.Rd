% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_tempawn.R
\name{explore_tempawn}
\alias{explore_tempawn}
\title{Plot analysis results of temoral PAWN analysis}
\usage{
explore_tempawn(
  tempawn,
  variable = NULL,
  significance = NULL,
  date_range = NULL
)
}
\arguments{
\item{tempawn}{The list object that is returned by \code{tempawn()}.}
}
\value{
Returns a ggplot object with the visualization of the simulation ranges
and the individual temoral parameter sensitivities.
}
\description{
Plot analysis results of temoral PAWN analysis
}
