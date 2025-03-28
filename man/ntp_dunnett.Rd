% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NTP.R
\name{ntp_dunnett}
\alias{ntp_dunnett}
\title{ntp_dunett Dunnett's test}
\usage{
ntp_dunnett(formula, data, dose_name = "dose")
}
\arguments{
\item{formula}{An equation of the form \eqn{Y \sim X.} Here the variable
\eqn{Y} is the response of interest, and \eqn{X} represents discrete
experimental conditions. For example, if weight is the dependent variable,
and you are interested in looking at the trend across sex one would have
'weight ~ sex'.}

\item{data}{A data frame with column names in the formula.}

\item{dose_name}{The name of the variable containing the doses in the data
frame \eqn{data}. It is expected multiple doses for each of the experimental
conditions \eqn{X}.}
}
\value{
An S4 object with results of Dunnet's test for each level in
\eqn{dose\_name} in the S4 slot results.
}
\description{
ntp_dunett Dunnett's test
}
\examples{
data(ntp_599_hemotology)
a <- ntp_dunnett(
    response ~ sex + response_type,
    data = ntp_599_hemotology,
    dose_name = "concentration"
)
a
}
