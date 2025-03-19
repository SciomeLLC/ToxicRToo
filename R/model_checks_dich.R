################################################################################
# Copyright 2020  NIEHS <matt.wheeler@nih.gov>
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation the
# rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
# sell copies of the Software, and to permit persons to whom the Software
# is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
# IN THE SOFTWARE.
################################################################################

.check_d_gamma <- function(prior) {
    temp <- prior[[1]]

    if (nrow(temp) != 3) {
        stop("Dichotomous Gamma model prior requires 3 parameters.")
    }

    if (temp[2, 4] < 0) {
        stop("The prior on b (parameter 2)
    can not have a lower bound less than zero.")
    }

    if (temp[3, 4] < 0) {
        stop("The prior on d (parameter 3)
    can not have a lower bound less than zero.")
    }

    prior$model <- "Gamma Model [binomial]"
    prior$mean <- "gamma"
    prior$parameters <- c("logit(g)", "a", "b")
    prior
}

.check_d_weibull <- function(prior) {
    temp <- prior[[1]]

    if (nrow(temp) != 3) {
        stop("Dichotomous Weibull model prior requires 3 parameters.")
    }

    if (temp[2, 4] < 0) {
        stop("The prior on b (parameter 2)
    can not have a lower bound less than zero.")
    }

    if (temp[3, 4] < 0) {
        stop("The prior on d (parameter 3)
    can not have a lower bound less than zero.")
    }

    prior$model <- "Weibull Model [binomial]"
    prior$mean <- "weibull"
    prior$parameters <- c("logit(g)", "a", "b")
    prior
}

.check_d_lprobit <- function(prior) {
    temp <- prior[[1]]

    if (nrow(temp) != 3) {
        stop("Dichotomous Log-Probit model prior requires 3 parameters.")
    }

    if (temp[3, 4] < 0) {
        stop("The prior on b1 (parameter 3)
    can not have a lower bound less than zero.")
    }

    prior$model <- "Log-Probit Model [binomial]"
    prior$mean <- "log-probit"
    prior$parameters <- c("logit(g)", "b0", "b1")
    prior
}

.check_d_llogistic <- function(prior) {
    temp <- prior[[1]]

    if (nrow(temp) != 3) {
        stop("Dichotomous Log-Logistic model prior requires 3 parameters.")
    }

    if (temp[3, 4] < 0) {
        stop("The prior on b1 (parameter 3)
    can not have a lower bound less than zero.")
    }

    prior$model <- "Log-Logistic Model [binomial]"
    prior$mean <- "log-logistic"
    prior$parameters <- c("logit(g)", "b0", "b1")
    prior
}

.check_d_logistic <- function(prior) {
    temp <- prior[[1]]

    if (nrow(temp) != 2) {
        stop("Dichotomous logistic model prior requires 2 parameters.")
    }

    if (temp[2, 4] < 0) {
        stop("The prior on b (parameter 2)
    can not have a lower bound less than zero.")
    }

    prior$model <- "Logistic Model [binomial]"
    prior$mean <- "logistic"
    prior$parameters <- c("a", "b")
    prior
}

.check_d_probit <- function(prior) {
    temp <- prior[[1]]

    if (nrow(temp) != 2) {
        stop("Dichotomous probit model prior requires 2 parameters.")
    }

    if (temp[2, 4] < 0) {
        stop("The prior on b (parameter 2)
    can not have a lower bound less than zero.")
    }

    prior$model <- "Probit Model [binomial]"
    prior$mean <- "probit"
    prior$parameters <- c("a", "b")
    prior
}

.check_d_qlinear <- function(prior) {
    temp <- prior[[1]]

    if (nrow(temp) != 2) {
        stop("Dichotomous Quantal model prior requires 2 parameters.")
    }

    if (temp[2, 4] < 0) {
        stop("The prior on b (parameter 2)
    can not have a lower bound less than zero.")
    }

    prior$model <- "Quantal Linear Model [binomial]"
    prior$mean <- "qlinear"
    prior$parameters <- c("logit(g)", "b")
    prior
}

.check_d_multistage <- function(prior) {
    temp <- prior[[1]]

    if (nrow(temp) < 2) {
        stop("Multistage model prior requires 2 or more parameters.")
    }

    names <- c("logit(g)")
    for (ii in seq(2, nrow(temp))) {
        names <- c(names, sprintf("b%d", ii - 1))
    }

    if (sum(temp[seq(2, nrow(temp)), 4] < 0) > 0) {
        stop("The prior on bx can not have a lower bound less than zero.")
    }

    # Build a list
    rV <- list()
    rV$priors <- temp
    rV$model <- sprintf("Multistage-%d Model [binomial]", nrow(temp) - 1)
    rV$mean <- "multistage"
    rV$degree <- nrow(temp) - 1
    rV$parameters <- names
    rV
}

.check_d_hill <- function(prior) {
    temp <- prior[[1]]

    if (nrow(temp) != 4) {
        stop("Dichotomous Hill  model prior requires 3 parameters.")
    }

    if (temp[4, 4] < 0) {
        stop("The prior on d (parameter 4)
    can not have a lower bound less than zero.")
    }

    prior$model <- "Hill Model [binomial]"
    prior$mean <- "hill"
    prior$parameters <- c("logit(g)", "b", "c", "d")
    prior
}
