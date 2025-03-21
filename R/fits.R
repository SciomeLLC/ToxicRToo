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

###############################################################################
# Set a Generic for `plot()`
###############################################################################
setGeneric(
    "plot",
    function(x, y, ...) standardGeneric("plot")
)

###############################################################################
# S4 Class Definitions
###############################################################################
# For MCMC-based continuous fits

#' Single continuous model MCMC fit class
#'
#' Methods include plot and predict
#'
#' @slot bmd A vector containing the benchmark dose (BMD) and
#' \eqn{100\times(1-2\alpha)} confidence intervals.
#' @slot data Matrix of input data
#' @slot prior Prior information
#' @slot model String specifying the mean model used.
#' @slot options Numeric vector of options used in the fitting procedure.
#' @slot mcmc_result List of 2 matrices: BMD and parameter samples
#' @slot covariance The variance-covariance matrix for the parameters.
#' @slot maximum Maximum value of likelihood/posterior
#' @slot bmd_dist Quantiles for the BMD distribution.
#' @slot fitted_model A list with a simplified S3 object
#' @slot transformed Are the data \eqn{\log(x+\sqrt{x^2+1})} transformed?
#' @slot full_model The model along with the likelihood distribution.
#' @slot parameters The parameter estimates produced by the procedure, which
#' are relative to the model given in \code{full_model}.  The last parameter
#' is always the estimate for \eqn{\log(sigma^2)}.
#'
#' @return A BMD continuous fit (MCMC) object
#' @name BMD_continuous_fit_MCMC-class
#' @aliases BMD_continuous_fit_MCMC
#' @examples
#' M2 <- matrix(0, nrow = 5, ncol = 4)
#' colnames(M2) <- c("Dose", "Resp", "N", "StDev")
#' M2[, 1] <- c(0, 25, 50, 100, 200)
#' M2[, 2] <- c(6, 5.2, 2.4, 1.1, 0.75)
#' M2[, 3] <- c(20, 20, 19, 20, 20)
#' M2[, 4] <- c(1.2, 1.1, 0.81, 0.74, 0.66)
#' model <- single_continuous_fit(
#'     M2[, 1, drop = FALSE],
#'     M2[, 2:4],
#'     BMR_TYPE = "sd",
#'     BMR = 1, ewald = TRUE,
#'     distribution = "normal",
#'     fit_type = "mcmc",
#'     model_type = "hill",
#'     threads = 2,
#'     seed = 12331
#' )
#'
#' model
#' @export
setClass(
    "BMD_continuous_fit_MCMC",
    slots = c(
        bmd = "numeric",
        data = "matrix",
        prior = "ANY",
        model = "character",
        options = "numeric",
        mcmc_result = "list",
        covariance = "matrix",
        maximum = "numeric",
        bmd_dist = "matrix",
        fitted_model = "ANY",
        transformed = "logical",
        full_model = "character",
        parameters = "numeric"
    )
)

# For MLE / Laplace-based continuous fits

#' Single continuous model maximized (Laplace or MLE) fit class
#'
#' Methods include plot and predict
#'
#' @slot bmd A vector containing the benchmark dose (BMD) and
#' \eqn{100\times(1-2\alpha)} confidence intervals.
#' @slot data Matrix of input data
#' @slot prior Prior information
#' @slot model String specifying the mean model used.
#' @slot options Numeric vector of options used in the fitting procedure.
#' @slot covariance The variance-covariance matrix for the parameters.
#' @slot maximum Maximum value of likelihood/posterior
#' @slot bmd_dist Quantiles for the BMD distribution.
#' @slot fitted_model A list with a simplified S3 object
#' @slot transformed Are the data \eqn{\log(x+\sqrt{x^2+1})} transformed?
#' @slot full_model The model along with the likelihood distribution.
#' @slot parameters The parameter estimates produced by the procedure, which
#' are relative to the model given in \code{full_model}.  The last parameter
#' is always the estimate for \eqn{\log(sigma^2)}.
#'
#' @return A BMD continuous fit (Maximized) object
#' @name BMD_continuous_fit_maximized-class
#' @aliases BMD_continuous_fit_maximized
#' @examples
#' M2 <- matrix(0, nrow = 5, ncol = 4)
#' colnames(M2) <- c("Dose", "Resp", "N", "StDev")
#' M2[, 1] <- c(0, 25, 50, 100, 200)
#' M2[, 2] <- c(6, 5.2, 2.4, 1.1, 0.75)
#' M2[, 3] <- c(20, 20, 19, 20, 20)
#' M2[, 4] <- c(1.2, 1.1, 0.81, 0.74, 0.66)
#' model <- single_continuous_fit(
#'     M2[, 1, drop = FALSE],
#'     M2[, 2:4],
#'     BMR_TYPE = "sd",
#'     BMR = 1, ewald = TRUE,
#'     distribution = "normal",
#'     fit_type = "laplace",
#'     model_type = "hill",
#'     threads = 2,
#'     seed = 12331
#' )
#'
#' model
#' @export
setClass(
    "BMD_continuous_fit_maximized",
    slots = c(
        bmd = "numeric",
        data = "matrix",
        prior = "ANY",
        model = "character",
        options = "numeric",
        covariance = "matrix",
        maximum = "numeric",
        bmd_dist = "matrix",
        fitted_model = "ANY",
        transformed = "logical",
        full_model = "character",
        parameters = "numeric"
    )
)

BMD_continuous_fit_MCMC <- function(
    bmd = numeric(3),
    data = matrix(),
    prior = NULL,
    model = character(),
    options = numeric(),
    mcmc_result = list(),
    covariance = matrix(),
    maximum = NA_real_,
    bmd_dist = matrix(),
    fitted_model = NULL,
    transformed = FALSE,
    full_model = character(),
    parameters = numeric()) {
    new("BMD_continuous_fit_MCMC",
        bmd = bmd,
        data = data,
        prior = prior,
        model = model,
        options = options,
        mcmc_result = mcmc_result,
        covariance = covariance,
        maximum = maximum,
        bmd_dist = bmd_dist,
        fitted_model = fitted_model,
        transformed = transformed,
        full_model = full_model,
        parameters = parameters
    )
}

BMD_continuous_fit_maximized <- function(
    bmd = numeric(3),
    data = matrix(),
    prior = NULL,
    model = character(),
    options = numeric(),
    covariance = matrix(),
    maximum = NA_real_,
    bmd_dist = matrix(),
    fitted_model = NULL,
    transformed = FALSE,
    full_model = character(),
    parameters = numeric()) {
    new("BMD_continuous_fit_maximized",
        bmd = bmd,
        data = data,
        prior = prior,
        model = model,
        options = options,
        covariance = covariance,
        maximum = maximum,
        bmd_dist = bmd_dist,
        fitted_model = fitted_model,
        transformed = transformed,
        full_model = full_model,
        parameters = parameters
    )
}

#' Single dichotomous model MCMC fit class
#'
#' Methods include plot and predict
#'
#' @slot full_model The model along with the likelihood distribution.
#' @slot parameters The parameter estimates produced by the procedure, which
#' are relative to the model given in \code{full_model}.  The last parameter
#' is always the estimate for \eqn{\log(sigma^2)}.
#' @slot covariance The variance-covariance matrix for the parameters.
#' @slot bmd_dist Quantiles for the BMD distribution.
#' @slot bmd A vector containing the benchmark dose (BMD) and
#' \eqn{100\times(1-2\alpha)} confidence intervals.
#' @slot maximum The maximum value of the likelihood/posterior.
#' @slot prior Prior information
#' @slot model String specifying the mean model used.
#' @slot data Matrix of input data
#' @slot mcmc_result List of 2 matrices: BMD and parameter samples
#' @slot options Numeric vector of options used in the fitting procedure.
#'
#' @return A BMD dichotomous fit (MCMC) object
#' @name BMD_dichotomous_fit_MCMC-class
#' @aliases BMD_dichotomous_fit_MCMC
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
#'     model_type = "hill", fit_type = "mcmc"
#' )
#' model
#' @export
setClass(
    "BMD_dichotomous_fit_MCMC",
    slots = c(
        full_model = "character",
        parameters = "numeric",
        covariance = "matrix",
        bmd_dist = "matrix",
        bmd = "numeric",
        maximum = "numeric",
        # gof_p_value = "numeric",
        # gof_chi_sqr_statistic = "numeric",
        prior = "ANY",
        model = "character",
        data = "matrix",
        mcmc_result = "list", # has PARM_samples, BMD_samples
        options = "numeric"
    )
)

BMD_dichotomous_fit_MCMC <- function(
    full_model = "",
    parameters = numeric(),
    covariance = matrix(0, 0, 0),
    bmd_dist = matrix(0, 0, 0),
    bmd = numeric(3),
    maximum = NA_real_,
    # gof_p_value  = NA_real_,
    # gof_chi_sqr_statistic = NA_real_,
    prior = NULL,
    model = "",
    data = matrix(),
    mcmc_result = list(),
    options = numeric()) {
    new("BMD_dichotomous_fit_MCMC",
        full_model   = full_model,
        parameters   = parameters,
        covariance   = covariance,
        bmd_dist     = bmd_dist,
        bmd          = bmd,
        maximum      = maximum,
        # gof_p_value  = gof_p_value,
        # gof_chi_sqr_statistic = gof_chi_sqr_statistic,
        prior        = prior,
        model        = model,
        data         = data,
        mcmc_result  = mcmc_result,
        options      = options
    )
}

#' Single dichotomous model maximized (Laplace or MLE) fit class
#'
#' Methods include plot and predict
#'
#' @slot full_model The model along with the likelihood distribution.
#' @slot parameters The parameter estimates produced by the procedure, which
#' are relative to the model given in \code{full_model}. The last parameter is
#' always the estimate for \eqn{\log(\sigma^2)}.
#' @slot covariance The variance-covariance matrix for the parameters.
#' @slot bmd_dist Quantiles for the BMD distribution.
#' @slot bmd A vector containing the benchmark dose (BMD) and
#' \eqn{100\times(1-2\alpha)} confidence intervals.
#' @slot maximum The maximum value of the likelihood/posterior.
#' @slot gof_p_value GOF p-value for the Pearson \eqn{\chi^2} GOF test.
#' @slot gof_chi_sqr_statistic The GOF statistic.
#' @slot prior Prior information
#' @slot model String specifying the mean model used.
#' @slot data Matrix of input data
#' @slot options Numeric vector of options used in the fitting procedure.
#'
#' @return A BMD continuous fit (Maximized) object
#' @name BMD_dichotomous_fit_maximized-class
#' @aliases BMD_dichotomous_fit_maximized
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
#' @export
setClass(
    "BMD_dichotomous_fit_maximized",
    slots = c(
        full_model = "character",
        parameters = "numeric",
        covariance = "matrix",
        bmd_dist = "matrix",
        bmd = "numeric", # length=3: (BMD, BMDL, BMDU)
        maximum = "numeric",
        gof_p_value = "numeric",
        gof_chi_sqr_statistic = "numeric",
        prior = "ANY", # for Bayesian laplace (or NA if MLE)
        model = "character",
        data = "matrix", # [Dose, Y, N, ...]
        #    mcmc_result = "ANY",
        options = "numeric" # BMR, alpha, samples, burnin, etc.
    )
)

BMD_dichotomous_fit_maximized <- function(
    full_model = "",
    parameters = numeric(),
    covariance = matrix(0, 0, 0),
    bmd_dist = matrix(0, 0, 0),
    bmd = numeric(3),
    maximum = NA_real_,
    gof_p_value = NA_real_,
    gof_chi_sqr_statistic = NA_real_,
    prior = NULL,
    model = "",
    data = matrix(),
    mcmc_result = NULL,
    options = numeric()) {
    new("BMD_dichotomous_fit_maximized",
        full_model = full_model,
        parameters = parameters,
        covariance = covariance,
        bmd_dist = bmd_dist,
        bmd = bmd,
        maximum = maximum,
        gof_p_value = gof_p_value,
        gof_chi_sqr_statistic = gof_chi_sqr_statistic,
        prior = prior,
        model = model,
        data = data,
        #      mcmc_result  = mcmc_result,
        options = options
    )
}

#' BMD Bayesian Dichotomous Model class
#'
#' Methods include plot, cleveland_plot
#'
#' @slot bmd Named vector of model averaged BMD, BMDL, BMDU
#' @slot posterior_probs Named vector of posterior probabilities of each model
#' @slot submodels list of all of models used in model averaging
#' @slot fit_type string value specifying fit type "mcmc" or "laplace"
#' @slot full_model Full model name
#'
#' @return A BMD_dichotomous_fit_MA object
#' @name BMD_dichotomous_fit_MA-class
#' @aliases BMD_dichotomous_fit_MA
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
#' model <- ma_dichotomous_fit(D, Y, N)
#'
#' model
#' @export
# BMD_dichotomous_fit_MA: for model-averaged fits
setClass(
    "BMD_dichotomous_fit_MA",
    slots = c(
        bmd = "numeric",
        posterior_probs = "numeric",
        submodels = "list", # each element is a single-model fit
        fit_type = "character", # "mcmc" or "laplace"
        full_model = "character"
    )
)

BMD_dichotomous_fit_MA <- function(
    bmd = numeric(3),
    posterior_probs = numeric(),
    submodels = list(),
    fit_type = "mcmc", # or "laplace"
    full_model = "BMDdichotomous_MA") {
    new("BMD_dichotomous_fit_MA",
        bmd = bmd,
        posterior_probs = posterior_probs,
        submodels = submodels,
        fit_type = fit_type,
        full_model = full_model
    )
}


setMethod("show", "BMD_continuous_fit_MCMC", function(object) {
    cat("BMD Continuous Fit (MCMC)\n")
    cat("  Model:          ", object@full_model, "\n")
    cat("  BMD:            ", paste(object@bmd, collapse = ", "), "\n")
    # cat("  Maximum Posterior: ", object@maximum, "\n")
    cat("  Parameters: ", paste(object@parameters, collapse = ", "), "\n")
    invisible(object)
})

setMethod("show", "BMD_continuous_fit_maximized", function(object) {
    cat("BMD Continuous Fit (Laplace)\n")
    cat("  Model:          ", object@full_model, "\n")
    cat("  BMD:            ", paste(object@bmd, collapse = ", "), "\n")
    # cat("  Maximum Posterior: ", object@maximum, "\n")
    cat("  Parameters: ", paste(object@parameters, collapse = ", "), "\n")
    invisible(object)
})

###############################################################################
# S4 Methods
###############################################################################
setMethod("show", "BMD_dichotomous_fit_maximized", function(object) {
    cat(
        "BMD Dichotomous Fit (Maximized with",
        ifelse(is.null(object@prior), "MLE", "Laplace"),
        ") \n"
    )
    cat("  Model:   ", object@full_model, "\n")
    cat("  BMD:     ", paste(object@bmd, collapse = ", "), "\n")
    # cat("  Maximum: ", object@maximum, "\n")
    cat("  Parameters: ", paste(object@parameters, collapse = ", "), "\n")
    invisible(object)
})

setMethod("show", "BMD_dichotomous_fit_MCMC", function(object) {
    cat("BMD Dichotomous Fit (MCMC)\n")
    cat("  Model:   ", object@full_model, "\n")
    cat("  BMD:     ", paste(object@bmd, collapse = ", "), "\n")
    # cat("  Maximum: ", object@maximum, "\n")
    cat("  Parameters: ", paste(object@parameters, collapse = ", "), "\n")
    invisible(object)
})

#' Plot method for BMD_continuous_fit_MCMC objects
#'
#' @title Plot method for BMD_continuous_fit_MCMC objects
#' @param x S4 object BMD_continuous_fit_MCMC
#' @param y NA / missing
#' @param ... additional parameters (not used)
#' @return ggplot object
#' @rdname plot
#' @aliases plot,BMD_continuous_fit_MCMC,missing-method
#' @exportMethod plot
setMethod(
    "plot",
    signature = signature(x = "BMD_continuous_fit_MCMC", y = "missing"),
    function(x, y, ...) {
        temp_args <- list(...)
        if (!"qprob" %in% names(temp_args)) {
            qprob <- 0.05
        } else {
            qprob <- temp_args$qprob
        }
        # to suppress "no visible binding" NOTE in CRAN checks, etc.
        Dose <- NULL
        X1 <- X2 <- X3 <- X4 <- X5 <- NULL

        isLogNormal <- (grepl("Log-Normal", x@full_model) == 1)
        IS_tranformed <- x@transformed
        density_col <- "blueviolet"
        credint_col <- "azure2"
        BMD_DENSITY <- TRUE
        data_d <- x@data

        if (ncol(data_d) == 4) {
            # sufficient stats
            IS_SUFFICIENT <- TRUE
            mean_val <- data_d[, 2, drop = FALSE]
            se <- data_d[, 4, drop = FALSE] / sqrt(data_d[, 3, drop = FALSE])
            doses <- data_d[, 1, drop = FALSE]
            uerror <- mean_val + 2 * se
            lerror <- mean_val - 2 * se
            lm_fit <- lm(mean_val ~ doses, weights = 1 / (se * se))
        } else {
            IS_SUFFICIENT <- FALSE
            Response <- data_d[, 2, drop = FALSE]
            doses <- data_d[, 1, drop = FALSE]
            lm_fit <- lm(Response ~ doses)
        }

        # determine if data is decreasing or not
        if (coefficients(lm_fit)[2] < 0) {
            decrease <- TRUE
        } else {
            decrease <- FALSE
        }

        # Build a test_doses vector
        test_doses <- seq(
            min(doses), max(doses) * 1.03,
            (max(doses) * 1.03 - min(doses)) / 500
        )
        if (IS_tranformed) {
            test_doses <- asinh(test_doses)
        }

        Q <- switch(x@model,
            "exp-aerts" = apply(
                x@mcmc_result$PARM_samples,
                1,
                .cont_exp_aerts_f,
                d = test_doses,
                decrease = decrease
            ),
            "invexp-aerts" = apply(
                x@mcmc_result$PARM_samples,
                1,
                .cont_invexp_aerts_f,
                d = test_doses,
                decrease = decrease
            ),
            "gamma-aerts" = apply(
                x@mcmc_result$PARM_samples,
                1,
                .cont_gamma_aerts_f,
                d = test_doses,
                decrease = decrease
            ),
            "invgamma-aerts" = apply(
                x@mcmc_result$PARM_samples,
                1,
                .cont_invgamma_aerts_f,
                d = test_doses,
                decrease = decrease
            ),
            "hill-aerts" = apply(
                x@mcmc_result$PARM_samples,
                1,
                .cont_hill_aerts_f,
                d = test_doses,
                decrease = decrease
            ),
            "lomax-aerts" = apply(
                x@mcmc_result$PARM_samples,
                1,
                .cont_lomax_aerts_f,
                d = test_doses,
                decrease = decrease
            ),
            "invlomax-aerts" = apply(
                x@mcmc_result$PARM_samples,
                1,
                .cont_invlomax_aerts_f,
                d = test_doses,
                decrease = decrease
            ),
            "lognormal-aerts" = apply(
                x@mcmc_result$PARM_samples,
                1,
                .cont_lognormal_aerts_f,
                d = test_doses,
                decrease = decrease
            ),
            "logskew-aerts" = apply(
                x@mcmc_result$PARM_samples,
                1,
                .cont_logskew_aerts_f,
                d = test_doses,
                decrease = decrease
            ),
            "invlogskew-aerts" = apply(
                x@mcmc_result$PARM_samples,
                1,
                .cont_invlogskew_aerts_f,
                d = test_doses,
                decrease = decrease
            ),
            "logistic-aerts" = apply(
                x@mcmc_result$PARM_samples,
                1,
                .cont_logistic_aerts_f,
                d = test_doses,
                decrease = decrease
            ),
            "probit-aerts" = apply(
                x@mcmc_result$PARM_samples,
                1,
                .cont_probit_aerts_f,
                d = test_doses,
                decrease = decrease
            ),
            "LMS" = apply(
                x@mcmc_result$PARM_samples,
                1,
                .cont_LMS_f,
                d = test_doses,
                decrease = decrease
            ),
            "gamma-efsa" = apply(
                x@mcmc_result$PARM_samples,
                1,
                .cont_gamma_efsa_f,
                d = test_doses,
                decrease = decrease
            ),
            NULL
        )
        if (isLogNormal && !is.null(Q)) {
            Q <- exp(
                Q + exp(
                    x@mcmc_result$PARM_samples[
                        , ncol(x@mcmc_result$PARM_samples)
                    ]
                ) / 2
            )
        }
        if (x@model == "FUNL") {
            Q <- apply(
                x@mcmc_result$PARM_samples, 1, .cont_FUNL_f,
                d = test_doses, decrease = decrease
            )
        }
        if (x@model == "hill") {
            Q <- apply(
                x@mcmc_result$PARM_samples, 1, .cont_hill_f,
                d = test_doses, decrease = decrease
            )
        }
        if (x@model == "exp-3") {
            Q <- apply(
                x@mcmc_result$PARM_samples, 1, .cont_exp_3_f,
                d = test_doses, decrease = decrease
            )
            if (isLogNormal) {
                # var in last col
                varCol <- ncol(x@mcmc_result$PARM_samples)
                Q <- exp(log(Q) + exp(x@mcmc_result$PARM_samples[, varCol]) / 2)
            }
        }
        if (x@model == "exp-5") {
            Q <- apply(
                x@mcmc_result$PARM_samples, 1, .cont_exp_5_f,
                d = test_doses, decrease = decrease
            )
            if (isLogNormal) {
                varCol <- ncol(x@mcmc_result$PARM_samples)
                Q <- exp(log(Q) + exp(x@mcmc_result$PARM_samples[, varCol]) / 2)
            }
        }
        if (x@model == "power") {
            Q <- apply(
                x@mcmc_result$PARM_samples, 1, .cont_power_f,
                d = test_doses, decrease = decrease
            )
        }
        if (x@model == "polynomial") {
            # "degree" might be ncol(...) - 2 if normal-ncv, etc.
            if (length(grep(": normal-ncv", tolower(x@full_model))) > 0) {
                degree <- ncol(x@mcmc_result$PARM_samples) - 2
            } else {
                degree <- ncol(x@mcmc_result$PARM_samples) - 1
            }
            Q <- apply(
                x@mcmc_result$PARM_samples[
                    ,
                    seq_len(degree)
                ],
                1,
                .cont_polynomial_f,
                d = test_doses,
                decrease = decrease
            )
        }

        if (IS_tranformed) {
            test_doses <- sinh(test_doses)
        }
        Q <- t(Q)
        me <- apply(Q, 2, quantile, probs = 0.5)
        lq <- apply(Q, 2, quantile, probs = qprob)
        uq <- apply(Q, 2, quantile, probs = 1 - qprob)

        # Build the ggplot
        plot_gg <- ggplot() +
            xlim(-max(test_doses) * 5, max(test_doses) * 5) +
            geom_line(
                aes(x = test_doses, y = me),
                color = "blue", linewidth = 2
            ) +
            labs(
                x = "Dose", y = "Response",
                title = paste(x@full_model, "MCMC", sep = ",  Fit Type: ")
            ) +
            theme_minimal()

        # If the test_doses are not NaN / Inf, add BMD lines
        if (sum(!is.nan(test_doses) + !is.infinite(test_doses)) == 0) {
            plot_gg <- plot_gg +
                geom_segment(
                    aes(
                        x = x@bmd[2], y = me[1], # me[1] ~ me at bmd[1]?
                        xend = x@bmd[3], yend = me[1]
                    ),
                    color = "darkslategrey", linewidth = 1.2, alpha = 0.9
                ) +
                annotate(
                    geom = "text", x = x@bmd[2], y = me[1], label = "[",
                    size = 10,
                    color = "darkslategrey", alpha = 0.9
                ) +
                annotate(
                    geom = "text", x = x@bmd[3], y = me[1], label = "]",
                    size = 10,
                    color = "darkslategrey", alpha = 0.9
                ) +
                annotate(
                    geom = "point", x = x@bmd[1], y = me[1], size = 5,
                    color = "darkslategrey", shape = 17, alpha = 0.9
                )
        }

        if (BMD_DENSITY) {
            temp <- x@mcmc_result$BMD_samples
            temp <- temp[!is.nan(temp)]
            temp <- temp[!is.na(temp)]
            temp <- temp[!is.infinite(temp)]
            errorFun <- function(e) {
                y <- rep(1, 512)
                xx <- seq(
                    min(test_doses),
                    max(test_doses),
                    (max(test_doses) - min(test_doses)) / 511
                )
                data.frame(x = xx, y = y)
            }
            Dens <- tryCatch(
                density(temp,
                    cut = c(max(test_doses)), adjust = 1.5, n = 512,
                    from = min(test_doses), to = max(test_doses)
                ),
                error = errorFun
            )
            Dens$y <- Dens$y / max(Dens$y) * (max(Q) - min(Q)) * 0.6
            # keep points < max(test_doses)
            temp_idxs <- which(Dens$x < max(test_doses))
            D1_y <- Dens$y[temp_idxs]
            D1_x <- Dens$x[temp_idxs]
            scale <- (max(Q) - min(Q)) / max(D1_y) * 0.40
            plot_gg <- plot_gg +
                geom_polygon(aes(
                    x = c(0, D1_x, max(doses)),
                    y = c(min(Q), min(Q) + D1_y * scale, min(Q))
                ), fill = "blueviolet", alpha = 0.6)
        }

        # Add raw data
        width <- 3
        if (IS_SUFFICIENT) {
            data_in <- data.frame(
                doses = doses,
                mean_val = mean_val[, 1],
                uerror = uerror[, 1],
                lerror = lerror[, 1]
            )
            plot_gg <- plot_gg +
                geom_errorbar(
                    data = data_in, aes(
                        x = doses, ymin = lerror, ymax = uerror
                    ),
                    color = "black", linewidth = 0.8, width = width
                ) +
                geom_point(
                    aes(x = doses, y = mean_val),
                    size = 3,
                    shape = 21,
                    fill = "white",
                    data = data_in
                )
        } else {
            data_in <- data.frame(doses = doses[, 1], Response = data_d[, 2])
            plot_gg <- plot_gg +
                geom_point(aes(x = doses, y = Response), data = data_in)
        }

        plot_gg <- plot_gg +
            geom_polygon(
                aes(
                    x = c(
                        test_doses,
                        test_doses[rev(seq_len(length(test_doses)))]
                    ),
                    y = c(uq, lq[rev(seq_len(length(test_doses)))])
                ),
                fill = "blue", alpha = 0.1
            )

        plot_gg <- plot_gg + coord_cartesian(
            xlim = c(
                min(test_doses),
                max(test_doses)
            ),
            expand = FALSE
        )

        # return final ggplot
        plot_gg
    }
)


#' Plot method for BMD_continuous_fit_maximized objects
#'
#' @title Plot method for BMD_continuous_fit_maximized objects
#' @param x S4 object BMD_continuous_fit_maximized
#' @param y NA / missing
#' @param ... additional parameters (not used)
#' @rdname plot
#' @aliases plot,BMD_continuous_fit_maximized,missing-method
#' @exportMethod plot
setMethod(
    "plot",
    signature = signature(x = "BMD_continuous_fit_maximized", y = "missing"),
    function(x, y, ...) {
        temp_args <- list(...)
        if (!"qprob" %in% names(temp_args)) {
            qprob <- 0.05
        } else {
            qprob <- temp_args$qprob
        }

        isLogNormal <- (grepl("Log-Normal", x@full_model) == 1)
        IS_transformed <- x@transformed

        data_d <- x@data
        if (ncol(data_d) == 4) {
            IS_SUFFICIENT <- TRUE
            mean_val <- data_d[, 2, drop = FALSE]
            se <- data_d[, 4, drop = FALSE] / sqrt(data_d[, 3, drop = FALSE])
            doses <- data_d[, 1, drop = FALSE]
            uerror <- mean_val + 2 * se
            lerror <- mean_val - 2 * se
            lm_fit <- lm(mean_val ~ doses, weights = 1 / (se * se))
        } else {
            IS_SUFFICIENT <- FALSE
            Response <- data_d[, 2, drop = FALSE]
            doses <- data_d[, 1, drop = FALSE]
            lm_fit <- lm(Response ~ doses)
        }

        decrease <- (coefficients(lm_fit)[2] < 0)
        test_doses <- seq(
            min(doses),
            max(doses) * 1.03,
            (max(doses) - min(doses)) / 500
        )
        if (IS_transformed) {
            test_doses <- asinh(test_doses)
        }

        me <- switch(x@model,
            "exp-aerts" = .cont_exp_aerts_f(x@parameters, test_doses),
            "invexp-aerts" = .cont_invexp_aerts_f(x@parameters, test_doses),
            "gamma-aerts" = .cont_gamma_aerts_f(x@parameters, test_doses),
            "invgamma-aerts" = .cont_invgamma_aerts_f(x@parameters, test_doses),
            "hill-aerts" = .cont_hill_aerts_f(x@parameters, test_doses),
            "lomax-aerts" = .cont_lomax_aerts_f(x@parameters, test_doses),
            "invlomax-aerts" = .cont_invlomax_aerts_f(x@parameters, test_doses),
            "lognormal-aerts" = .cont_lognormal_aerts_f(
                x@parameters, test_doses
            ),
            "logskew-aerts" = .cont_logskew_aerts_f(x@parameters, test_doses),
            "invlogskew-aerts" = .cont_invlogskew_aerts_f(
                x@parameters, test_doses
            ),
            "logistic-aerts" = .cont_logistic_aerts_f(x@parameters, test_doses),
            "probit-aerts" = .cont_probit_aerts_f(x@parameters, test_doses),
            "LMS" = .cont_LMS_f(x@parameters, test_doses),
            "gamma-efsa" = .cont_gamma_efsa_f(x@parameters, test_doses)
        )

        if (isLogNormal && !is.null(me)) {
            me <- exp(me)
        }
        if (x@model == "FUNL") {
            me <- .cont_FUNL_f(x@parameters, test_doses)
        }
        if (x@model == "hill") {
            me <- .cont_hill_f(x@parameters, test_doses)
        }
        if (x@model == "exp-3") {
            me <- .cont_exp_3_f(x@parameters, test_doses, decrease)
        }
        if (x@model == "exp-5") {
            me <- .cont_exp_5_f(x@parameters, test_doses)
        }
        if (x@model == "power") {
            me <- .cont_power_f(x@parameters, test_doses)
        }
        if (x@model == "polynomial") {
            # if distribution is normal-ncv vs normal, etc.
            if (length(grep(": normal-ncv", tolower(x@full_model))) > 0) {
                deg <- length(x@parameters) - 2
            } else {
                deg <- length(x@parameters) - 1
            }
            me <- .cont_polynomial_f(x@parameters[seq_len(deg)], test_doses)
        }
        if (isLogNormal) {
            var_ <- exp(x@parameters[length(x@parameters)])
            me <- exp(log(me) + var_ / 2)
        }

        if (IS_transformed) {
            test_doses <- sinh(test_doses)
        }
        ma_mean <- splinefun(test_doses, me)

        # Build ggplot
        plot_gg <- ggplot() +
            geom_line(
                aes(x = test_doses, y = me),
                color = "blue", linewidth = 2
            ) +
            xlim(-max(test_doses) * 5, max(test_doses) * 5) +
            labs(
                x = "Dose",
                y = "Response",
                title = paste(x@full_model, "Maximized", sep = ",  Fit Type: ")
            ) +
            theme_minimal()

        # If test_doses aren’t infinite/NaN, add BMD lines
        if (sum(!is.nan(test_doses) + !is.infinite(test_doses)) == 0) {
            if (!any(is.na(x@bmd))) {
                plot_gg <- plot_gg +
                    geom_segment(
                        aes(
                            x = x@bmd[2], y = ma_mean(x@bmd[1]),
                            xend = x@bmd[3], yend = ma_mean(x@bmd[1])
                        ),
                        color = "darkslategrey", linewidth = 1.2, alpha = 0.9
                    ) +
                    annotate("text",
                        x = x@bmd[2], y = ma_mean(x@bmd[1]),
                        label = "[", size = 10, color = "darkslategrey",
                        alpha = 0.9
                    ) +
                    annotate("text",
                        x = x@bmd[3], y = ma_mean(x@bmd[1]),
                        label = "]", size = 10, color = "darkslategrey",
                        alpha = 0.9
                    ) +
                    annotate("point",
                        x = x@bmd[1], y = ma_mean(x@bmd[1]),
                        size = 5, color = "darkslategrey", shape = 17,
                        alpha = 0.9
                    )
            }
        }

        # Sufficient stats or raw data
        width <- 3
        if (IS_SUFFICIENT) {
            data_in <- data.frame(
                doses = doses[, 1],
                mean_val = mean_val[, 1],
                uerror = uerror[, 1],
                lerror = lerror[, 1]
            )
            plot_gg <- plot_gg +
                geom_errorbar(aes(x = doses, ymin = lerror, ymax = uerror),
                    data = data_in, color = "grey", linewidth = 0.5, width = 3
                ) +
                geom_point(aes(x = doses, y = mean_val),
                    data = data_in, size = 3, shape = 21, fill = "white"
                )
        } else {
            data_in <- data.frame(doses = doses[, 1], Response = data_d[, 2])
            plot_gg <- plot_gg +
                geom_point(aes(x = doses, y = Response), data = data_in)
        }

        # Return final ggplot
        plot_gg + coord_cartesian(
            xlim = c(
                min(test_doses),
                max(test_doses)
            ),
            expand = FALSE
        )
    }
)

# MCMC single-model

#' Plot method for BMD_dichotomous_fit_MCMC objects
#'
#' @title Plot method for BMD_dichotomous_fit_MCMC objects
#' @param x S4 object BMD_dichotomous_fit_MCMC
#' @param y NA / missing
#' @param ... additional parameters (not used)
#' @rdname plot
#' @aliases plot,BMD_dichotomous_fit_MCMC,missing-method
#' @exportMethod plot
setMethod(
    "plot",
    signature = signature(x = "BMD_dichotomous_fit_MCMC", y = "missing"),
    function(x, y, ...) {
        temp_args <- list(...)
        if (!"qprob" %in% names(temp_args)) {
            qprob <- 0.05
        } else {
            qprob <- temp_args$qprob
        }
        density_col <- "red"
        credint_col <- "azure2"
        BMD_DENSITY <- TRUE

        dose <- x_axis <- y_axis <- NULL
        if (qprob < 0 || qprob > 0.5) {
            stop("Quantile probability must be between 0 and 0.5")
        }

        data_d <- x@data
        probs <- (0.5 + data_d[, 2]) / (1.0 + data_d[, 3])
        se <- sqrt(probs * (1 - probs) / data_d[, 3])
        doses <- data_d[, 1]

        uerror <- apply(cbind(probs * 0 + 1, probs + se), 1, min, na.rm = TRUE)
        lerror <- apply(cbind(probs * 0, probs - se), 1, max, na.rm = TRUE)

        # Build test doses
        test_doses <- seq(min(doses), max(doses) * 1.03, length.out = 100)

        # Switch on x@model to compute Q from MCMC param samples
        if (x@model == "hill") {
            Q <- apply(
                x@mcmc_result$PARM_samples, 1, .dich_hill_f,
                d = test_doses
            )
        } else if (x@model == "gamma") {
            Q <- apply(
                x@mcmc_result$PARM_samples, 1, .dich_gamma_f,
                d = test_doses
            )
        } else if (x@model == "logistic") {
            Q <- apply(
                x@mcmc_result$PARM_samples, 1, .dich_logist_f,
                d = test_doses
            )
        } else if (x@model == "log-logistic") {
            Q <- apply(
                x@mcmc_result$PARM_samples, 1, .dich_llogist_f,
                d = test_doses
            )
        } else if (x@model == "probit") {
            Q <- apply(
                x@mcmc_result$PARM_samples, 1, .dich_probit_f,
                d = test_doses
            )
        } else if (x@model == "log-probit") {
            Q <- apply(
                x@mcmc_result$PARM_samples, 1, .dich_lprobit_f,
                d = test_doses
            )
        } else if (x@model == "multistage") {
            Q <- apply(
                x@mcmc_result$PARM_samples,
                1,
                .dich_multistage_f,
                d = test_doses
            )
        } else if (x@model == "qlinear") {
            Q <- apply(
                x@mcmc_result$PARM_samples, 1, .dich_qlinear_f,
                d = test_doses
            )
        } else if (x@model == "weibull") {
            Q <- apply(
                x@mcmc_result$PARM_samples, 1, .dich_weibull_f,
                d = test_doses
            )
        } else {
            stop("Unknown model:", x@model)
        }

        # BMD samples for density
        temp <- x@mcmc_result$BMD_samples
        temp <- temp[!is.nan(temp)]
        temp <- temp[!is.infinite(temp)]
        # build Q matrix
        Q <- t(Q)

        # me = colMeans(Q)
        me <- colMeans(Q, na.rm = TRUE)
        lq <- apply(Q, 2, quantile, probs = qprob, na.rm = TRUE)
        uq <- apply(Q, 2, quantile, probs = 1 - qprob, na.rm = TRUE)

        # build ggplot
        plot_gg <- ggplot() +
            geom_errorbar(
                aes(x = doses, ymin = lerror, ymax = uerror),
                color = "grey"
            ) +
            labs(
                x = "Dose", y = "Proportion",
                title = paste(x@full_model, sep = ",  Fit Type: ")
            ) +
            theme_minimal() +
            xlim(0 - 5 * max(test_doses), 5 * max(test_doses))

        # Spline
        temp_fit <- splinefun(test_doses, me)

        # Add ribbon + line
        df_plot <- data.frame(dose = test_doses, me = me, lq = lq, uq = uq)
        plot_gg <- plot_gg +
            geom_ribbon(
                aes(x = dose, ymin = lq, ymax = uq),
                fill = "blue",
                alpha = 0.1,
                data = df_plot
            ) +
            geom_line(
                aes(x = dose, y = me),
                col = "blue",
                linewidth = 2,
                data = df_plot
            ) +
            geom_point(
                aes(x = doses, y = probs),
                color = "black"
            )

        # BMD lines
        bmd_vec <- x@bmd
        plot_gg <- plot_gg +
            geom_segment(aes(
                x = bmd_vec[2], y = temp_fit(bmd_vec[1]),
                xend = bmd_vec[3], yend = temp_fit(bmd_vec[1])
            ), color = "darkslategrey", linewidth = 1.2, alpha = 0.9) +
            annotate(
                geom = "text", x = bmd_vec[2], y = temp_fit(bmd_vec[1]),
                label = "[", size = 10, color = "darkslategrey", alpha = 0.9
            ) +
            annotate(
                geom = "text", x = bmd_vec[3], y = temp_fit(bmd_vec[1]),
                label = "]", size = 10, color = "darkslategrey", alpha = 0.9
            ) +
            annotate(
                geom = "point", x = bmd_vec[1], y = temp_fit(bmd_vec[1]),
                size = 5, color = "darkslategrey", shape = 17, alpha = 0.9
            )

        # Density polygon
        if (BMD_DENSITY) {
            Dens <- density(
                temp,
                cut = c(5 * max(test_doses)),
                n = 1000,
                from = 0,
                to = max(test_doses),
                na.rm = TRUE
            )
            # scale
            # (max(uerror)-min(lerror))*0.6
            max_u <- max(uerror, na.rm = TRUE)
            min_l <- min(lerror, na.rm = TRUE)
            Dens$y <- Dens$y / max(Dens$y) * (max_u - min_l) * 0.6
            idx2 <- which(Dens$x < max(test_doses))
            D1_y <- Dens$y[idx2]
            D1_x <- Dens$x[idx2]
            scale <- (max_u - min_l) / max(D1_y) * 0.40
            plot_gg <- plot_gg +
                geom_polygon(aes(
                    x = c(max(0, min(D1_x)), D1_x, max(D1_x)),
                    y = c(min_l, min_l + D1_y * scale, min_l)
                ), fill = "blueviolet", alpha = 0.6)
        }

        plot_gg + coord_cartesian(
            xlim = c(min(doses), max(doses)),
            expand = FALSE
        )
    }
)

# Maximized single-model

#' Plot method for BMD_dichotomous_fit_maximized objects
#'
#' @title Plot method for BMD_dichotomous_fit_maximized objects
#' @param x S4 object BMD_dichotomous_fit_maximized
#' @param y NA / missing
#' @param ... additional parameters (not used)
#' @rdname plot
#' @aliases plot,BMD_dichotomous_fit_maximized,missing-method
#' @exportMethod plot
setMethod(
    "plot",
    signature = signature(x = "BMD_dichotomous_fit_maximized", y = "missing"),
    function(x, y, ...) {
        temp_args <- list(...)
        if (!"qprob" %in% names(temp_args)) {
            qprob <- 0.05
        } else {
            qprob <- temp_args$qprob
        }
        dose <- NULL
        density_col <- "red"
        credint_col <- "azure2"

        data_d <- x@data
        probs <- (0.5 + data_d[, 2]) / (1.0 + data_d[, 3])
        se <- sqrt(probs * (1 - probs) / data_d[, 3])

        doses <- data_d[, 1]
        uerror <- apply(cbind(probs * 0 + 1, probs + se), 1, min)
        lerror <- apply(cbind(probs * 0, probs - se), 1, max)

        test_doses <- seq(min(doses), max(doses) * 1.03, length.out = 100)

        # Build me from x@parameters
        if (x@model == "hill") {
            me <- .dich_hill_f(x@parameters, test_doses)
        } else if (x@model == "gamma") {
            me <- .dich_gamma_f(x@parameters, test_doses)
        } else if (x@model == "logistic") {
            me <- .dich_logist_f(x@parameters, test_doses)
        } else if (x@model == "log-logistic") {
            me <- .dich_llogist_f(x@parameters, test_doses)
        } else if (x@model == "probit") {
            me <- .dich_probit_f(x@parameters, test_doses)
        } else if (x@model == "log-probit") {
            me <- .dich_lprobit_f(x@parameters, test_doses)
        } else if (x@model == "multistage") {
            me <- .dich_multistage_f(x@parameters, test_doses)
        } else if (x@model == "qlinear") {
            me <- .dich_qlinear_f(x@parameters, test_doses)
        } else if (x@model == "weibull") {
            me <- .dich_weibull_f(x@parameters, test_doses)
        } else {
            stop("Unknown model:", x@model)
        }

        plot_gg <- ggplot() +
            geom_errorbar(
                aes(x = doses, ymin = lerror, ymax = uerror),
                color = "grey"
            ) +
            xlim(
                c(min(doses) - 5 * max(doses), max(doses) * 5)
            ) +
            labs(
                x = "Dose",
                y = "Proportion",
                title = paste(x@full_model, sep = ",  Fit Type: ")
            ) +
            theme_minimal()

        # Add line
        df_plot <- data.frame(dose = test_doses, me = me)
        plot_gg <- plot_gg +
            geom_line(
                aes(x = dose, y = me),
                col = "blue",
                linewidth = 1.2,
                data = df_plot
            ) +
            geom_point(aes(x = doses, y = probs))

        # BMD lines
        temp_fit <- splinefun(test_doses, me)
        bmd_vec <- x@bmd
        plot_gg <- plot_gg +
            geom_segment(aes(
                x = bmd_vec[2], y = temp_fit(bmd_vec[1]),
                xend = bmd_vec[3], yend = temp_fit(bmd_vec[1])
            ), color = "darkslategrey", linewidth = 1.2, alpha = 0.9) +
            annotate(
                geom = "text", x = bmd_vec[2], y = temp_fit(bmd_vec[1]),
                label = "[", size = 10, color = "darkslategrey", alpha = 0.9
            ) +
            annotate(
                geom = "text", x = bmd_vec[3], y = temp_fit(bmd_vec[1]),
                label = "]", size = 10, color = "darkslategrey", alpha = 0.9
            ) +
            annotate(
                geom = "point", x = bmd_vec[1], y = temp_fit(bmd_vec[1]),
                size = 5, color = "darkslategrey", shape = 17, alpha = 0.9
            )

        plot_gg + coord_cartesian(
            xlim = c(min(doses), max(doses)),
            expand = FALSE
        )
    }
)

#' Predict values for a maximized dichotomous fit model
#'
#' This method provides predictions for objects of class
#' \code{BMD_dichotomous_fit_maximized}. It computes the predicted response
#' values based on the fitted model parameters and optionally new dose values.
#'
#' @param object An object of class \code{BMD_dichotomous_fit_maximized}.
#' @param new_doses An optional vector or matrix of new dose values at which to
#'   predict the response. If \code{NULL}, the original dose values from the
#'   fitted data are used.
#' @param ... Additional arguments (currently not used).
#'
#' @return A list with components \code{X} (the dose levels) and \code{Y} (the
#'   predicted response values).
#'
#' @rdname predict-methods
#' @aliases predict,BMD_dichotomous_fit_maximized-method
#' @exportMethod predict
setMethod(
    "predict",
    signature = signature(object = "BMD_dichotomous_fit_maximized"),
    function(object, new_doses = NULL, ...) {
        # Extract from slots
        data_d <- object@data # old fit$data
        pars <- object@parameters
        model_name <- object@model

        # if user doesn't supply new_doses, default to the original data
        if (is.null(new_doses)) {
            test_doses <- data_d[, 1] # first column is dose
        } else {
            test_doses <- new_doses
        }

        # replicate the logic from .dichotomous_predict_model:
        if (model_name == "hill") f <- .dich_hill_f(pars, test_doses)

        if (model_name == "gamma") f <- .dich_gamma_f(pars, test_doses)

        if (model_name == "logistic") f <- .dich_logist_f(pars, test_doses)

        if (model_name == "log-logistic") f <- .dich_llogist_f(pars, test_doses)

        if (model_name == "probit") f <- .dich_probit_f(pars, test_doses)

        if (model_name == "log-probit") f <- .dich_lprobit_f(pars, test_doses)

        if (model_name == "multistage") {
            f <- .dich_multistage_f(pars, test_doses)
        }

        if (model_name == "qlinear") f <- .dich_qlinear_f(pars, test_doses)

        if (model_name == "weibull") f <- .dich_weibull_f(pars, test_doses)

        return(list(X = test_doses, Y = f))
    }
)

#' Predict values for a MCMC dichotomous fit model
#'
#' This method provides predictions for objects of class
#' \code{BMD_dichotomous_fit_MCMC}. It computes the predicted response
#' values based on the fitted model parameters and optionally new dose values.
#'
#' @param object An object of class \code{BMD_dichotomous_fit_MCMC}.
#' @param new_doses An optional vector or matrix of new dose values at which to
#'   predict the response. If \code{NULL}, the original dose values from the
#'   fitted data are used.
#' @param ... Additional arguments (currently not used).
#'
#' @return A list with components \code{X} (the dose levels) and \code{Y} (the
#'   predicted response values).
#'
#' @rdname predict-methods
#' @aliases predict,BMD_dichotomous_fit_MCMC-method
#' @exportMethod predict
setMethod(
    "predict",
    signature = signature(object = "BMD_dichotomous_fit_MCMC"),
    function(object, new_doses = NULL, ...) {
        data_d <- object@data
        model_name <- object@model
        # MCMC samples in object@mcmc_result$PARM_samples
        param_mat <- object@mcmc_result$PARM_samples

        if (is.null(new_doses)) {
            test_doses <- data_d[, 1]
        } else {
            test_doses <- new_doses
        }

        # replicate .dichotomous_predict_model_mcmc
        if (model_name == "hill") {
            f <- apply(param_mat, 1, .dich_hill_f, d = test_doses)
        }
        if (model_name == "gamma") {
            f <- apply(param_mat, 1, .dich_gamma_f, d = test_doses)
        }
        if (model_name == "logistic") {
            f <- apply(param_mat, 1, .dich_logist_f, d = test_doses)
        }
        if (model_name == "log-logistic") {
            f <- apply(param_mat, 1, .dich_llogist_f, d = test_doses)
        }
        if (model_name == "probit") {
            f <- apply(param_mat, 1, .dich_probit_f, d = test_doses)
        }
        if (model_name == "log-probit") {
            f <- apply(param_mat, 1, .dich_lprobit_f, d = test_doses)
        }
        if (model_name == "multistage") {
            f <- apply(param_mat, 1, .dich_multistage_f, d = test_doses)
        }
        if (model_name == "qlinear") {
            f <- apply(param_mat, 1, .dich_qlinear_f, d = test_doses)
        }
        if (model_name == "weibull") {
            f <- apply(param_mat, 1, .dich_weibull_f, d = test_doses)
        }

        # f is matrix, each col = dose, each row = sample => transpose if needed
        return(list(X = test_doses, Y = f))
    }
)

#' Predict values for a maximized continuous fit model
#'
#' This method provides predictions for objects of class
#' \code{BMD_continuous_fit_maximized}. It computes the predicted response
#' values based on the fitted model parameters and optionally new dose values.
#'
#' @param object An object of class \code{BMD_continuous_fit_maximized}.
#' @param new_doses An optional vector or matrix of new dose values at which to
#'   predict the response. If \code{NULL}, the original dose values from the
#'   fitted data are used.
#' @param ... Additional arguments (currently not used).
#'
#' @return A list with components \code{X} (the dose levels) and \code{Y} (the
#'   predicted response values).
#'
#' @rdname predict-methods
#' @aliases predict,BMD_continuous_fit_maximized-method
#' @exportMethod predict
setMethod(
    "predict",
    signature = signature(object = "BMD_continuous_fit_maximized"),
    function(object, new_doses = NULL, ...) {
        data_d <- object@data
        pars <- object@parameters
        model <- object@model
        full_mod <- object@full_model

        if (ncol(data_d) == 4) {
            mean_v <- data_d[, 2, drop = FALSE]
            se <- data_d[, 4, drop = FALSE] / sqrt(data_d[, 3, drop = FALSE])
            doses <- data_d[, 1, drop = FALSE]
            lm_fit <- lm(mean_v ~ doses, weights = 1 / (se * se))
        } else {
            # single-col Y
            Response <- data_d[, 2, drop = FALSE]
            doses <- data_d[, 1, drop = FALSE]
            lm_fit <- lm(Response ~ doses)
        }

        # decide new_doses
        if (is.null(new_doses)) {
            test_doses <- data_d[, 1]
        } else {
            test_doses <- new_doses
        }

        decrease <- (coefficients(lm_fit)[2] < 0)

        f <- NULL
        # Check model name
        if (model == "FUNL") {
            f <- .cont_FUNL_f(pars, test_doses)
        }
        if (model == "hill") {
            f <- .cont_hill_f(pars, test_doses)
        }
        if (model == "exp-3") {
            f <- .cont_exp_3_f(pars, test_doses, decrease)
        }
        if (model == "exp-5") {
            f <- .cont_exp_5_f(pars, test_doses)
        }
        if (model == "power") {
            f <- .cont_power_f(pars, test_doses)
        }
        if (model == "exp-aerts") {
            f <- .cont_exp_aerts_f(pars, test_doses)
        }
        if (model == "invexp-aerts") {
            f <- .cont_invexp_aerts_f(pars, test_doses)
        }
        if (model == "gamma-aerts") {
            f <- .cont_gamma_aerts_f(pars, test_doses)
        }
        if (model == "invgamma-aerts") {
            f <- .cont_invgamma_aerts_f(pars, test_doses)
        }
        if (model == "hill-aerts") {
            f <- .cont_hill_aerts_f(pars, test_doses)
        }
        if (model == "lomax-aerts") {
            f <- .cont_lomax_aerts_f(pars, test_doses)
        }
        if (model == "invlomax-aerts") {
            f <- .cont_invlomax_aerts_f(pars, test_doses)
        }
        if (model == "lognormal-aerts") {
            f <- .cont_lognormal_aerts_f(pars, test_doses)
        }
        if (model == "logskew-aerts") {
            f <- .cont_logskew_aerts_f(pars, test_doses)
        }
        if (model == "invlogskew-aerts") {
            f <- .cont_invlogskew_aerts_f(pars, test_doses)
        }
        if (model == "logistic-aerts") {
            f <- .cont_logistic_aerts_f(pars, test_doses)
        }
        if (model == "probit-aerts") {
            f <- .cont_probit_aerts_f(pars, test_doses)
        }
        if (model == "LMS") {
            f <- .cont_LMS_f(pars, test_doses)
        }
        if (model == "polynomial") {
            # check degree
            if (length(grep(": normal-ncv", tolower(full_mod))) > 0) {
                degree <- length(pars) - 2
            } else {
                degree <- length(pars) - 1
            }
            f <- .cont_polynomial_f(pars[seq_len(degree)], test_doses)
        }

        # handle lognormal case
        if (grepl("Log-Normal", full_mod)) {
            # check if it's an aerts model
            if (model %in% c(
                "exp-aerts", "invexp-aerts", "gamma-aerts", "invgamma-aerts",
                "hill-aerts", "lomax-aerts", "invlomax-aerts",
                "lognormal-aerts", "logskew-aerts", "invlogskew-aerts",
                "logistic-aerts", "probit-aerts", "LMS"
            )) {
                retY <- exp(as.numeric(f) + 0.5 * exp(pars[length(pars)]))
            } else {
                retY <- exp(log(as.numeric(f)) + 0.5 * exp(pars[length(pars)]))
            }
            return(list(X = test_doses, Y = retY))
        } else {
            return(list(X = test_doses, Y = as.numeric(f)))
        }
    }
)

#' Predict values for a MCMC continuous fit model
#'
#' This method provides predictions for objects of class
#' \code{BMD_continuous_fit_MCMC}. It computes the predicted response
#' values based on the fitted model parameters and optionally new dose values.
#'
#' @param object An object of class \code{BMD_continuous_fit_MCMC}.
#' @param new_doses An optional vector or matrix of new dose values at which to
#'   predict the response. If \code{NULL}, the original dose values from the
#'   fitted data are used.
#' @param ... Additional arguments (currently not used).
#'
#' @return A list with components \code{X} (the dose levels) and \code{Y} (the
#'   predicted response values).
#'
#' @rdname predict-methods
#' @aliases predict,BMD_continuous_fit_MCMC-method
#' @exportMethod predict
setMethod(
    "predict",
    signature = signature(object = "BMD_continuous_fit_MCMC"),
    function(object, new_doses = NULL, ...) {
        data_d <- object@data
        model <- object@model
        full_mod <- object@full_model
        param_mat <- object@mcmc_result$PARM_samples

        if (ncol(data_d) == 4) {
            mean_v <- data_d[, 2, drop = FALSE]
            se <- data_d[, 4, drop = FALSE] / sqrt(data_d[, 3, drop = FALSE])
            doses <- data_d[, 1, drop = FALSE]
            lm_fit <- lm(mean_v ~ doses, weights = 1 / (se * se))
        } else {
            Response <- data_d[, 2, drop = FALSE]
            doses <- data_d[, 1, drop = FALSE]
            lm_fit <- lm(Response ~ doses)
        }

        if (is.null(new_doses)) {
            test_doses <- data_d[, 1]
        } else {
            test_doses <- new_doses
        }
        decrease <- (coefficients(lm_fit)[2] < 0)

        # replicate .continuous_predict_model_mcmc:
        if (model == "FUNL") {
            f <- apply(param_mat, 1, .cont_FUNL_f, test_doses)
        }
        if (model == "hill") {
            f <- apply(param_mat, 1, .cont_hill_f, test_doses)
        }
        if (model == "exp-3") {
            f <- apply(param_mat, 1, .cont_exp_3_f, test_doses)
        }
        if (model == "exp-5") {
            f <- apply(param_mat, 1, .cont_exp_5_f, test_doses)
        }
        if (model == "power") {
            f <- apply(param_mat, 1, .cont_power_f, test_doses)
        }
        if (model == "exp-aerts") {
            f <- apply(param_mat, 1, .cont_exp_aerts_f, test_doses)
        }
        if (model == "invexp-aerts") {
            f <- apply(param_mat, 1, .cont_invexp_aerts_f, test_doses)
        }
        if (model == "gamma-aerts") {
            f <- apply(param_mat, 1, .cont_gamma_aerts_f, test_doses)
        }
        if (model == "invgamma-aerts") {
            f <- apply(param_mat, 1, .cont_invgamma_aerts_f, test_doses)
        }
        if (model == "hill-aerts") {
            f <- apply(param_mat, 1, .cont_hill_aerts_f, test_doses)
        }
        if (model == "lomax-aerts") {
            f <- apply(param_mat, 1, .cont_lomax_aerts_f, test_doses)
        }
        if (model == "invlomax-aerts") {
            f <- apply(param_mat, 1, .cont_invlomax_aerts_f, test_doses)
        }
        if (model == "lognormal-aerts") {
            f <- apply(param_mat, 1, .cont_lognormal_aerts_f, test_doses)
        }
        if (model == "logskew-aerts") {
            f <- apply(param_mat, 1, .cont_logskew_aerts_f, test_doses)
        }
        if (model == "invlogskew-aerts") {
            f <- apply(param_mat, 1, .cont_invlogskew_aerts_f, test_doses)
        }
        if (model == "logistic-aerts") {
            f <- apply(param_mat, 1, .cont_logistic_aerts_f, test_doses)
        }
        if (model == "probit-aerts") {
            f <- apply(param_mat, 1, .cont_probit_aerts_f, test_doses)
        }
        if (model == "LMS") {
            f <- apply(param_mat, 1, .cont_LMS_f, test_doses)
        }
        if (model == "polynomial") {
            if (length(grep(": normal-ncv", tolower(full_mod))) > 0) {
                degree <- ncol(param_mat) - 2
            } else {
                degree <- ncol(param_mat) - 1
            }

            f <- apply(
                param_mat[, seq_len(degree)],
                1,
                .cont_polynomial_f,
                test_doses
            )
        }
        # handle lognormal
        if (grepl("Log-Normal", full_mod)) {
            if (model %in% c(
                "exp-aerts", "invexp-aerts", "gamma-aerts", "invgamma-aerts",
                "hill-aerts", "lomax-aerts", "invlomax-aerts",
                "lognormal-aerts", "logskew-aerts", "invlogskew-aerts",
                "logistic-aerts", "probit-aerts", "LMS"
            )) {
                retY <- exp(
                    f + 0.5 * exp(object@parameters[length(object@parameters)])
                )
            } else {
                retY <- exp(
                    log(f) +
                        0.5 * exp(object@parameters[length(object@parameters)])
                )
            }
            return(list(X = test_doses, Y = retY))
        } else {
            return(list(X = test_doses, Y = f))
        }
    }
)

#' Gets the prior of a BMD_continuous_fit
#'
#' This is equivalent to object@prior, but the accessor is
#' needed to avoid BiocCheck warnings for the vignette.
#'
#' @param object A single continuous fit object
#' @return Returns the included priorClass
#' @examples
#' M2 <- matrix(0, nrow = 5, ncol = 4)
#' colnames(M2) <- c("Dose", "Resp", "N", "StDev")
#' M2[, 1] <- c(0, 25, 50, 100, 200)
#' M2[, 2] <- c(6, 5.2, 2.4, 1.1, 0.75)
#' M2[, 3] <- c(20, 20, 19, 20, 20)
#' M2[, 4] <- c(1.2, 1.1, 0.81, 0.74, 0.66)
#' model <- single_continuous_fit(
#'     M2[, 1, drop = FALSE],
#'     M2[, 2:4],
#'     BMR_TYPE = "sd",
#'     BMR = 1, ewald = TRUE,
#'     distribution = "normal",
#'     fit_type = "laplace",
#'     model_type = "hill",
#'     threads = 2,
#'     seed = 12331
#' )
#'
#' get_prior(model)
#' @rdname get_prior
#' @name get_prior
#' @export
setGeneric("get_prior", function(object) standardGeneric("get_prior"))
#' @rdname get_prior
#' @aliases get_prior,BMD_continuous_fit_maximized-method
#' @exportMethod get_prior
setMethod(
    "get_prior",
    signature(object = "BMD_continuous_fit_maximized"),
    function(object) object@prior
)
#' @rdname get_prior
#' @aliases get_prior,BMD_continuous_fit_MCMC-method
#' @exportMethod get_prior
setMethod(
    "get_prior",
    signature(object = "BMD_continuous_fit_MCMC"),
    function(object) object@prior
)


#' Gets the model of a BMD_continuous_fit
#'
#' This is equivalent to object@full_model, but the accessor is
#' needed to avoid BiocCheck warnings for the vignette.
#'
#' @param object A single continuous fit object
#' @return Returns a string description of the model
#' @examples
#' M2 <- matrix(0, nrow = 5, ncol = 4)
#' colnames(M2) <- c("Dose", "Resp", "N", "StDev")
#' M2[, 1] <- c(0, 25, 50, 100, 200)
#' M2[, 2] <- c(6, 5.2, 2.4, 1.1, 0.75)
#' M2[, 3] <- c(20, 20, 19, 20, 20)
#' M2[, 4] <- c(1.2, 1.1, 0.81, 0.74, 0.66)
#' model <- single_continuous_fit(
#'     M2[, 1, drop = FALSE],
#'     M2[, 2:4],
#'     BMR_TYPE = "sd",
#'     BMR = 1, ewald = TRUE,
#'     distribution = "normal",
#'     fit_type = "laplace",
#'     model_type = "hill",
#'     threads = 2,
#'     seed = 12331
#' )
#'
#' get_model(model)
#' @rdname get_model
#' @name get_model
#' @export
setGeneric("get_model", function(object) standardGeneric("get_model"))

#' @rdname get_model
#' @aliases get_model,BMD_continuous_fit_maximized-method
#' @exportMethod get_model
setMethod(
    "get_model",
    signature(object = "BMD_continuous_fit_maximized"),
    function(object) object@full_model
)

#' @rdname get_model
#' @aliases get_model,BMD_continuous_fit_MCMC-method
#' @exportMethod get_model
setMethod(
    "get_model",
    signature(object = "BMD_continuous_fit_MCMC"),
    function(object) object@full_model
)
