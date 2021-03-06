% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/orgDatMsm.R
\name{org_dat_msm}
\alias{org_dat_msm}
\title{Organizes the data so that a Hidden Markov Model can be fit}
\usage{
org_dat_msm(best_detects, t_star = 1)
}
\arguments{
\item{best_detects}{Output of \code{\link{get_locations}} or \code{\link{get_best_locations}}.}

\item{t_star}{The distance in km at which a fish is considered to have stayed in place between detection periods.}
}
\value{
returns a data.frame that will be taken as input into \code{\link{fit_hmm}}.
}
\description{
Organizes the data so that a Hidden Markov Model can be fit
}
\examples{
org_dat <- org_dat_msm(best_detects)
}
