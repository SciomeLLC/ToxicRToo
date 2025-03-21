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

#' Fit a single dichotomous dose-response model to data.
#'
#' @param D A numeric vector or matrix of doses.
#' @param Y A numeric vector or matrix of responses.
#' @param N A numeric vector or matrix of the number of replicates at a dose.
#' @param model_type The mean model for the dichotomous model fit.  It can be
#' one of the following: \cr "hill","gamma","logistic", "log-logistic",
#' "log-probit","multistage"  ,"probit","qlinear","weibull"
#' @param fit_type the method used to fit (laplace, mle, or mcmc)
#' @param prior Used if you want to specify a prior for the data.
#' @param BMR This option specifies the benchmark response BMR. The BMR is
#' defined in relation to the BMD calculation requested (see BMD). By
#' default, the "BMR = 0.1."
#' @param alpha Alpha is the specified nominal coverage rate for computation of
#' the lower bound on the BMDL and BMDU, i.e., one computes a
#' \eqn{100\times(1-\alpha)\%} .  For the interval (BMDL,BMDU) this is a
#' \eqn{100\times(1-2\alpha)\% } confidence interval.  By default, it is set to
#' 0.05.
#' @param degree the number of degrees of a polynomial model. Only used for
#' polynomial models.
#' @param samples the number of samples to take (MCMC only)
#' @param burnin the number of burnin samples to take (MCMC only)
#' @param threads specify the number of OpenMP threads to use for the
#' calculations. Default = 2
#' @param seed specify the GSL seed. Default = 12331
#'
#' @return Returns a fitted S4 model class (either
#' \code{\link{BMD_dichotomous_fit_MCMC}} or
#' \code{\link{BMD_dichotomous_fit_maximized}}) with a subset of the following
#' slots:
#' \itemize{
#'    \item \code{full_model}: The model along with the likelihood distribution.
#'    \item \code{parameters}:
#'      The parameter estimates produced by the procedure, which are relative
#'      to the model given in \code{full_model}.  The last parameter is always
#'      the estimate for \eqn{\log(\sigma^2)}.
#'    \item \code{covariance}: The variance-covariance matrix for the
#'      parameters.
#'    \item \code{bmd_dist}:  Quantiles for the BMD distribution.
#'    \item \code{bmd}: A vector containing the benchmark dose (BMD) and
#'      \eqn{100\times(1-2\alpha)} confidence intervals.
#'    \item \code{maximum}: The maximum value of the likelihod/posterior.
#'    \item \code{gof_p_value}: GOF p-value for the Pearson
#'      \eqn{\chi^2} GOF test.
#'    \item \code{gof_chi_sqr_statistic}: The GOF statistic. (Not with MCMC)
#'    \item \code{prior}: This value gives the prior for the Bayesian analysis.
#'    \item \code{model}: String specifying the mean model used.
#'    \item \code{options}: Numeric vector of options used in the fitting
#'      procedure.
#'    \item \code{data}: The data used in the fit.
#'    \itemize{
#'        When MCMC is specified, an additional variable \code{mcmc_result}
#'        has the following two variables:
#'        \item \code{PARM_samples}: matrix of parameter samples.
#'        \item \code{BMD_samples}: vector of BMD sampled values.
#'    }
#' }
#'
#' @examples
#' mData <- matrix(c(
#'     0, 2, 50,
#'     1, 2, 50,
#'     3, 10, 50,
#'     16, 18, 50,
#'     32, 18, 50,
#'     33, 17, 50
#' ), nrow = 6, ncol = 3, byrow = TRUE)
#' D <- mData[, 1]
#' Y <- mData[, 2]
#' N <- mData[, 3]
#' model <- single_dichotomous_fit(
#'     D, Y, N,
#'     model_type = "hill", fit_type = "laplace"
#' )
#' model
#'
single_dichotomous_fit <- function(
    D, Y, N, model_type,
    fit_type = "laplace",
    prior = NULL,
    BMR = 0.1,
    alpha = 0.05,
    degree = 2,
    samples = 21000,
    burnin = 1000,
    threads = 2,
    seed = 12331) {
    # Convert inputs to matrix
    Y <- as.matrix(Y)
    D <- as.matrix(D)
    N <- as.matrix(N)

    .setseedGSL(seed)
    .set_threads(threads)

    DATA <- cbind(D, Y, N)
    test <- .check_for_na(DATA)
    Y <- Y[test == TRUE, , drop = FALSE]
    D <- D[test == TRUE, , drop = FALSE]
    N <- N[test == TRUE, , drop = FALSE]

    # Rebuild DATA
    DATA <- cbind(D, Y, N)

    # Build or check prior
    if (is.null(prior)) {
        prior <- .bayesian_prior_dich(model_type, degree)
    } else {
        if (!isClass(prior, "priorClass")) {
            stop(
                "Prior is not the correct form.
        Please create a priorClass object with create_prior_list."
            )
        }
        model_type <- prior@mean
        if (model_type == "multistage") {
            degree <- prior@degree
        }
        if (fit_type == "mle") {
            stop("A Bayesian prior model was specified, but MLE was requested.")
        }
    }

    # map model_type to internal code
    dmodel <- which(model_type == c(
        "hill", "gamma", "logistic", "log-logistic", "log-probit",
        "multistage", "probit", "qlinear", "weibull"
    ))
    if (identical(dmodel, integer(0))) {
        stop(
            'Please specify one of: "hill","gamma","logistic","log-logistic",',
            '"log-probit","multistage","probit","qlinear","weibull"'
        )
    }

    # Some bounds
    o1 <- c(BMR, alpha, -9999)
    o2 <- c(1, degree)

    # If multistage, check degree constraints
    if (dmodel == 6) { # multistage
        if ((o2[2] < 2) || (o2[2] > (nrow(DATA) - 1))) {
            stop(
                "Multistage must have degree between 2 and nrow(DATA)-1.
        If degree=1, use qlinear."
            )
        }
    }

    fit_type <- tolower(fit_type)
    fitter <- which(fit_type == c("mle", "laplace", "mcmc"))
    if (identical(fitter, integer(0))) {
        stop('fit_type must be "laplace","mle", or "mcmc"')
    }
    ret_obj <- NULL

    if (fitter == 1) {
        # MLE fit
        bounds <- .bmd_default_frequentist_settings(model_type, degree)
        temp <- .run_single_dichotomous(dmodel, DATA, bounds, o1, o2, seed)

        # Post-process bmd_dist -> bmd with alpha bounds
        temp_me <- temp$bmd_dist
        temp_me <- temp_me[!is.infinite(temp_me[, 1]), , drop = FALSE]
        temp_me <- temp_me[!is.na(temp_me[, 1]), , drop = FALSE]
        temp_me <- temp_me[!is.nan(temp_me[, 1]), , drop = FALSE]
        if (nrow(temp_me) > 5) {
            te <- splinefun(
                temp_me[, 2],
                temp_me[, 1],
                method = "monoH.FC",
                ties = mean
            )
            bmd_val <- c(temp$bmd, te(alpha), te(1 - alpha))
        } else {
            bmd_val <- c(temp$bmd, NA, NA)
        }
        names(bmd_val) <- c("BMD", "BMDL", "BMDU")

        # Build the S4 object
        ret_obj <- BMD_dichotomous_fit_maximized(
            full_model = temp$full_model,
            parameters = temp$parameters,
            covariance = temp$covariance,
            bmd_dist = temp$bmd_dist,
            bmd = bmd_val,
            maximum = temp$maximum,
            gof_p_value = temp$gof_p_value,
            gof_chi_sqr_statistic = temp$gof_chi_sqr_statistic,
            prior = NULL, # MLE => no prior
            model = model_type,
            data = DATA,
            mcmc_result = NULL,
            options = c(BMR, alpha, samples, burnin)
        )
    } else if (fitter == 2) {
        # Laplace
        temp <- .run_single_dichotomous(dmodel, DATA, prior@prior, o1, o2, seed)
        temp_me <- temp$bmd_dist
        temp_me <- temp_me[!is.infinite(temp_me[, 1]), , drop = FALSE]
        temp_me <- temp_me[!is.na(temp_me[, 1]), , drop = FALSE]
        temp_me <- temp_me[!is.nan(temp_me[, 1]), , drop = FALSE]
        if (nrow(temp_me) > 5) {
            te <- splinefun(
                temp_me[, 2],
                temp_me[, 1],
                method = "monoH.FC",
                ties = mean
            )
            bmd_val <- c(temp$bmd, te(alpha), te(1 - alpha))
        } else {
            bmd_val <- c(temp$bmd, NA, NA)
        }
        names(bmd_val) <- c("BMD", "BMDL", "BMDU")

        ret_obj <- BMD_dichotomous_fit_maximized(
            full_model = temp$full_model,
            parameters = temp$parameters,
            covariance = temp$covariance,
            bmd_dist = temp$bmd_dist,
            bmd = bmd_val,
            maximum = temp$maximum,
            gof_p_value = temp$gof_p_value,
            gof_chi_sqr_statistic = temp$gof_chi_sqr_statistic,
            prior = prior, # store the prior
            model = model_type,
            data = DATA,
            mcmc_result = NULL,
            options = c(BMR, alpha, samples, burnin)
        )
    } else if (fitter == 3) {
        # MCMC
        temp <- .run_dichotomous_single_mcmc(
            dmodel, DATA[, 2:3, drop = FALSE], DATA[, 1, drop = FALSE],
            prior@prior, c(BMR, alpha, samples, burnin), seed
        )
        # build bmd_dist from BMD_samples
        my_samps <- temp$mcmc_result$BMD_samples
        bmd_med <- mean(my_samps, na.rm = TRUE)
        bmd_l <- stats::quantile(my_samps, alpha, na.rm = TRUE)
        bmd_u <- stats::quantile(my_samps, 1 - alpha, na.rm = TRUE)
        bmd_val <- c(bmd_med, bmd_l, bmd_u)
        names(bmd_val) <- c("BMD", "BMDL", "BMDU")

        bmd_grid <- seq(0.005, 0.995, 0.005)
        bmd_vals <- stats::quantile(my_samps, bmd_grid, na.rm = TRUE)
        temp_bmd_dist <- cbind(bmd_vals, bmd_grid)

        ret_obj <- BMD_dichotomous_fit_MCMC(
            full_model   = temp$fitted_model$full_model,
            parameters   = temp$fitted_model$parameters,
            covariance   = temp$fitted_model$covariance,
            bmd_dist     = temp_bmd_dist,
            bmd          = bmd_val,
            maximum      = temp$fitted_model$maximum,
            # gof_p_value  = temp$gof_p_value,
            # gof_chi_sqr_statistic = temp$gof_chi_sqr_statistic,
            prior        = prior,
            model        = model_type,
            data         = DATA,
            mcmc_result  = temp$mcmc_result,
            options      = c(BMR, alpha, samples, burnin)
        )
    }

    return(ret_obj)
}



.bmd_default_frequentist_settings <- function(model, degree = 2) {
    dmodel <- which(model == c(
        "hill", "gamma", "logistic", "log-logistic",
        "log-probit", "multistage", "probit",
        "qlinear", "weibull"
    ))
    if (dmodel == 1) { # HILL
        prior <- matrix(c(
            0, 0, 2, -18, 18,
            0, 0, 2, -18, 18,
            0, 0, 0.5, -18, 18,
            0, 1, 0.250099980007996, 1.00E+00, 18
        ), nrow = 4, ncol = 5, byrow = TRUE)
    }
    if (dmodel == 2) { # GAMMA
        prior <- matrix(c(
            0, 0, 2, -18, 18,
            0, 1.5, 0.424264068711929, 1, 18,
            0, 1, 1, 0, 1000
        ), nrow = 3, ncol = 5, byrow = TRUE)
    }
    if (dmodel == 3) { # LOGISTIC
        prior <- matrix(c(
            0, -2, 2, -18, 18,
            0, 0.1, 1, 0.00E+00, 1e4
        ), nrow = 2, ncol = 5, byrow = TRUE)
    }
    if (dmodel == 4) { # LOG-LOGISTIC
        prior <- matrix(c(
            0, -1.65, 2, -18, 18,
            0, -2, 1, -18, 18,
            0, 1.2, 0.5, 0, 1e4
        ), nrow = 3, ncol = 5, byrow = TRUE)
    }
    if (dmodel == 5) { # LOG-PROBIT
        prior <- matrix(c(
            0, 0.01, 2, -18, 18,
            0, 0, 1, -8, 8,
            0, 1, 0.5, 0, 1000
        ), nrow = 3, ncol = 5, byrow = TRUE)
    }

    if (dmodel == 6) { # MULTISTAGE
        temp <- matrix(c(
            0, -2, 2, -18, 18,
            0, 1, 0.25, 0, 1e4,
            0, 0.1, 1, 0, 1.00E+06
        ), nrow = 3, ncol = 5, byrow = TRUE)
        prior <- matrix(
            c(0, 0.1, 1, 0, 1.00E+06),
            nrow = 1 + degree,
            ncol = 5,
            byrow = TRUE
        )
        prior[seq_len(3), ] <- temp
    }
    if (dmodel == 7) { # PROBIT
        prior <- matrix(c(
            0, -2, 2, -8, 8,
            0, 0.1, 1, 0.00E+00, 1000
        ), nrow = 2, ncol = 5, byrow = TRUE)
    }
    if (dmodel == 8) { # QLINEAR
        prior <- matrix(c(
            0, -2, 2, -18, 18,
            0, 1, 1, 0, 1000
        ), nrow = 2, ncol = 5, byrow = TRUE)
    }
    if (dmodel == 9) { # WEIBULL
        prior <- matrix(c(
            0, 0, 2, -18, 18,
            0, 1, 1, 0, 50,
            0, 1, 0.424264068711929, 1.00E-06, 1000
        ), nrow = 3, ncol = 5, byrow = TRUE)
    }

    return(prior)
}
