% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Pab.R
\name{Pab}
\alias{Pab}
\title{Function used by the TagLos model.}
\usage{
Pab(pin, t)
}
\arguments{
\item{pin}{A vector of input parameters (length two):
Instant tag retension (proportion)
Chronic tag loss (proportion)}

\item{t}{A vector of recapture liberties.}
}
\value{
The probability of both tags remaining after time t.
}
\description{
Used by the TagLoss model when animals have been tagged with two tags which are expected to have diffeent probabilities of retension.
}
\examples{
Pab(c(0.5,0.1,1.0,0.2),1:10)
}
