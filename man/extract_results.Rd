% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extractResults.R
\name{extract_results}
\alias{extract_results}
\title{Summarize the output of a fit Hidden Markov Model}
\usage{
extract_results(fit_mod, cl = 0.95)
}
\arguments{
\item{fit_mod}{Output of \code{\link{fit_hmm}}.}

\item{cl}{The desired confidence level.}
}
\value{
Returns fish survival, detection, and mortality signal related information.
}
\description{
Summarize the output of a fit Hidden Markov Model
}
\examples{
extract_results(fit_mod, cl=0.90)
}
