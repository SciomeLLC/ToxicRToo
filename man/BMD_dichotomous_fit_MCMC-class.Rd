% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fits.R
\docType{class}
\name{BMD_dichotomous_fit_MCMC-class}
\alias{BMD_dichotomous_fit_MCMC-class}
\alias{BMD_dichotomous_fit_MCMC}
\title{Single dichotomous model MCMC fit class}
\value{
A BMD dichotomous fit (MCMC) object
}
\description{
Methods include plot and predict
}
\section{Slots}{

\describe{
\item{\code{full_model}}{The model along with the likelihood distribution.}

\item{\code{parameters}}{The parameter estimates produced by the procedure, which
are relative to the model given in \code{full_model}.  The last parameter
is always the estimate for \eqn{\log(sigma^2)}.}

\item{\code{covariance}}{The variance-covariance matrix for the parameters.}

\item{\code{bmd_dist}}{Quantiles for the BMD distribution.}

\item{\code{bmd}}{A vector containing the benchmark dose (BMD) and
\eqn{100\times(1-2\alpha)} confidence intervals.}

\item{\code{maximum}}{The maximum value of the likelihood/posterior.}

\item{\code{prior}}{Prior information}

\item{\code{model}}{String specifying the mean model used.}

\item{\code{data}}{Matrix of input data}

\item{\code{mcmc_result}}{List of 2 matrices: BMD and parameter samples}

\item{\code{options}}{Numeric vector of options used in the fitting procedure.}
}}

\examples{
mData <- matrix(c(
    0, 2, 50,
    1, 2, 50,
    3, 10, 50,
    16, 18, 50,
    32, 18, 50,
    33, 17, 50
), nrow = 6, ncol = 3, byrow = TRUE)
D <- mData[, 1]
Y <- mData[, 2]
N <- mData[, 3]
model <- single_dichotomous_fit(
    D, Y, N,
    model_type = "hill", fit_type = "mcmc"
)
model
}
