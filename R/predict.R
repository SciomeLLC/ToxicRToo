# Copyright 2021  NIEHS <matt.wheeler@nih.gov>
#
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software
# and associated documentation files (the "Software"), to deal in the Software without restriction,
# including without limitation the rights to use, copy, modify, merge, publish, distribute,
# sublicense, and/or sell copies of the Software, and to permit persons to whom the Software
# is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all copies
# or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
# INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
# PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
# CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
# OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

# #' Predict function for ToxicRToo
# #'
# #' @param object Model object (of a single_continuous_fit or 
# #' single_dichotomous_fit)
# #' @param new_doses New doses to predict the response at
# #' @param ... Other unused arguments
# #'
# #' @return Predicted Values for the model
# setGeneric("predict", function(object, new_doses = NULL, ...) {
#   standardGeneric("predict")
# })

#' Predict Class Helper Function
#' 
#' While simply using predict() should work on single fits, this 
#' function provides the documentation for specifying new data with
#' a named parameter called new_doses.
#'
#' @param x Single continuous/dichotomous model fit object
#' @param new_doses New doses to predict the response at
#' @param ... Other unused arguments
#'
#' @return Predicted response values for the model
#' @export
predict.ToxicRToo <- function(x, new_doses = NULL, ...){
  #show("See documentation!")
  predict(x, new_doses = new_doses)
}

# setMethod(
#   "predict",
#   signature = signature(object = "BMD_dichotomous_fit_maximized"),
#   function(object, new_doses = NULL, ...) {

#     # Extract from slots
#     data_d <- object@data # old fit$data
#     pars <- object@parameters
#     model_name <- object@model

#     # if user doesn't supply new_doses, default to the original data
#     if (is.null(new_doses)) {
#       test_doses <- data_d[, 1] # first column is dose
#     } else {
#       test_doses <- new_doses
#     }

#     # replicate the logic from .dichotomous_predict_model:
#     if (model_name == "hill") f <- .dich_hill_f(pars, test_doses)

#     if (model_name == "gamma") f <- .dich_gamma_f(pars, test_doses)

#     if (model_name == "logistic") f <- .dich_logist_f(pars, test_doses)

#     if (model_name == "log-logistic") f <- .dich_llogist_f(pars, test_doses)

#     if (model_name == "probit") f <- .dich_probit_f(pars, test_doses)

#     if (model_name == "log-probit") f <- .dich_lprobit_f(pars, test_doses)

#     if (model_name == "multistage") f <- .dich_multistage_f(pars, test_doses)

#     if (model_name == "qlinear") f <- .dich_qlinear_f(pars, test_doses)

#     if (model_name == "weibull") f <- .dich_weibull_f(pars, test_doses)

#     return(list(X = test_doses, Y = f))
#   }
# )

# setMethod(
#   "predict",
#   signature = signature(object = "BMD_dichotomous_fit_MCMC"),
#   function(object, new_doses = NULL, ...) {

#     data_d <- object@data
#     model_name <- object@model
#     # MCMC samples in object@mcmc_result$PARM_samples
#     param_mat <- object@mcmc_result$PARM_samples

#     if (is.null(new_doses)) {
#       test_doses <- data_d[, 1]
#     } else {
#       test_doses <- new_doses
#     }

#     # replicate .dichotomous_predict_model_mcmc
#     if (model_name == "hill") {
#       f <- apply(param_mat, 1, .dich_hill_f, d = test_doses)
#     }
#     if (model_name == "gamma") {
#       f <- apply(param_mat, 1, .dich_gamma_f, d = test_doses)
#     }
#     if (model_name == "logistic") {
#       f <- apply(param_mat, 1, .dich_logist_f, d = test_doses)
#     }
#     if (model_name == "log-logistic") {
#       f <- apply(param_mat, 1, .dich_llogist_f, d = test_doses)
#     }
#     if (model_name == "probit") {
#       f <- apply(param_mat, 1, .dich_probit_f, d = test_doses)
#     }
#     if (model_name == "log-probit") {
#       f <- apply(param_mat, 1, .dich_lprobit_f, d = test_doses)
#     }
#     if (model_name == "multistage") {
#       f <- apply(param_mat, 1, .dich_multistage_f, d = test_doses)
#     }
#     if (model_name == "qlinear") {
#       f <- apply(param_mat, 1, .dich_qlinear_f, d = test_doses)
#     }
#     if (model_name == "weibull") {
#       f <- apply(param_mat, 1, .dich_weibull_f, d = test_doses)
#     }

#     # f is matrix, each col = dose, each row = sample => transpose if needed
#     return(list(X = test_doses, Y = f))
#   }
# )

# setMethod(
#   "predict",
#   signature = signature(object = "BMD_continuous_fit_maximized"),
#   function(object, new_doses = NULL, ...) {

#     data_d <- object@data
#     pars <- object@parameters
#     model <- object@model
#     full_mod <- object@full_model

#     if (ncol(data_d) == 4) {
#       mean_v <- data_d[, 2, drop = FALSE]
#       se <- data_d[, 4, drop = FALSE] / sqrt(data_d[, 3, drop = FALSE])
#       doses <- data_d[, 1, drop = FALSE]
#       lm_fit <- lm(mean_v ~ doses, weights = 1 / (se * se))
#     } else {
#       # single-col Y
#       Response <- data_d[, 2, drop = FALSE]
#       doses <- data_d[, 1, drop = FALSE]
#       lm_fit <- lm(Response ~ doses)
#     }

#     # decide new_doses
#     if (is.null(new_doses)) {
#       test_doses <- data_d[, 1]
#     } else {
#       test_doses <- new_doses
#     }

#     decrease <- (coefficients(lm_fit)[2] < 0)

#     f <- NULL
#     # Check model name
#     if (model == "FUNL") {
#       f <- .cont_FUNL_f(pars, test_doses)
#     }
#     if (model == "hill") {
#       f <- .cont_hill_f(pars, test_doses)
#     }
#     if (model == "exp-3") {
#       f <- .cont_exp_3_f(pars, test_doses, decrease)
#     }
#     if (model == "exp-5") {
#       f <- .cont_exp_5_f(pars, test_doses)
#     }
#     if (model == "power") {
#       f <- .cont_power_f(pars, test_doses)
#     }
#     if (model == "exp-aerts") {
#       f <- .cont_exp_aerts_f(pars, test_doses)
#     }
#     if (model == "invexp-aerts") {
#       f <- .cont_invexp_aerts_f(pars, test_doses)
#     }
#     if (model == "gamma-aerts") {
#       f <- .cont_gamma_aerts_f(pars, test_doses)
#     }
#     if (model == "invgamma-aerts") {
#       f <- .cont_invgamma_aerts_f(pars, test_doses)
#     }
#     if (model == "hill-aerts") {
#       f <- .cont_hill_aerts_f(pars, test_doses)
#     }
#     if (model == "lomax-aerts") {
#       f <- .cont_lomax_aerts_f(pars, test_doses)
#     }
#     if (model == "invlomax-aerts") {
#       f <- .cont_invlomax_aerts_f(pars, test_doses)
#     }
#     if (model == "lognormal-aerts") {
#       f <- .cont_lognormal_aerts_f(pars, test_doses)
#     }
#     if (model == "logskew-aerts") {
#       f <- .cont_logskew_aerts_f(pars, test_doses)
#     }
#     if (model == "invlogskew-aerts") {
#       f <- .cont_invlogskew_aerts_f(pars, test_doses)
#     }
#     if (model == "logistic-aerts") {
#       f <- .cont_logistic_aerts_f(pars, test_doses)
#     }
#     if (model == "probit-aerts") {
#       f <- .cont_probit_aerts_f(pars, test_doses)
#     }
#     if (model == "LMS") {
#       f <- .cont_LMS_f(pars, test_doses)
#     }
#     if (model == "polynomial") {
#       # check degree
#       if (length(grep(": normal-ncv", tolower(full_mod))) > 0) {
#         degree <- length(pars) - 2
#       } else {
#         degree <- length(pars) - 1
#       }
#       f <- .cont_polynomial_f(pars[1:degree], test_doses)
#     }

#     # handle lognormal case
#     if (grepl("Log-Normal", full_mod)) {
#       # check if it's an aerts model
#       if (model %in% c("exp-aerts", "invexp-aerts", "gamma-aerts", "invgamma-aerts", "hill-aerts",
#                         "lomax-aerts", "invlomax-aerts", "lognormal-aerts", "logskew-aerts",
#                         "invlogskew-aerts", "logistic-aerts", "probit-aerts", "LMS")) {
#         retY <- exp(as.numeric(f) + 0.5 * exp(pars[length(pars)]))
#       } else {
#         retY <- exp(log(as.numeric(f)) + 0.5 * exp(pars[length(pars)]))
#       }
#       return(list(X = test_doses, Y = retY))
#     } else {
#       return(list(X = test_doses, Y = as.numeric(f)))
#     }
#   }
# )

# setMethod(
#   "predict",
#   signature = signature(object = "BMD_continuous_fit_MCMC"),
#   function(object, new_doses = NULL, ...) {

#     data_d <- object@data
#     model <- object@model
#     full_mod <- object@full_model
#     param_mat <- object@mcmc_result$PARM_samples

#     if (ncol(data_d) == 4) {
#       mean_v <- data_d[, 2, drop = FALSE]
#       se <- data_d[, 4, drop = FALSE] / sqrt(data_d[, 3, drop = FALSE])
#       doses <- data_d[, 1, drop = FALSE]
#       lm_fit <- lm(mean_v ~ doses, weights = 1 / (se * se))
#     } else {
#       Response <- data_d[, 2, drop = FALSE]
#       doses <- data_d[, 1, drop = FALSE]
#       lm_fit <- lm(Response ~ doses)
#     }

#     if (is.null(new_doses)) {
#       test_doses <- data_d[, 1]
#     } else {
#       test_doses <- new_doses
#     }
#     decrease <- (coefficients(lm_fit)[2] < 0)

#     # replicate .continuous_predict_model_mcmc:
#     if (model == "FUNL") {
#       f <- apply(param_mat, 1, .cont_FUNL_f, test_doses)
#     }
#     if (model == "hill") {
#       f <- apply(param_mat, 1, .cont_hill_f, test_doses)
#     }
#     if (model == "exp-3") {
#       f <- apply(param_mat, 1, .cont_exp_3_f, test_doses)
#     }
#     if (model == "exp-5") {
#       f <- apply(param_mat, 1, .cont_exp_5_f, test_doses)
#     }
#     if (model == "power") {
#       f <- apply(param_mat, 1, .cont_power_f, test_doses)
#     }
#     if (model == "exp-aerts") {
#       f <- apply(param_mat, 1, .cont_exp_aerts_f, test_doses)
#     }
#     if (model == "invexp-aerts") {
#       f <- apply(param_mat, 1, .cont_invexp_aerts_f, test_doses)
#     }
#     if (model == "gamma-aerts") {
#       f <- apply(param_mat, 1, .cont_gamma_aerts_f, test_doses)
#     }
#     if (model == "invgamma-aerts") {
#       f <- apply(param_mat, 1, .cont_invgamma_aerts_f, test_doses)
#     }
#     if (model == "hill-aerts") {
#       f <- apply(param_mat, 1, .cont_hill_aerts_f, test_doses)
#     }
#     if (model == "lomax-aerts") {
#       f <- apply(param_mat, 1, .cont_lomax_aerts_f, test_doses)
#     }
#     if (model == "invlomax-aerts") {
#       f <- apply(param_mat, 1, .cont_invlomax_aerts_f, test_doses)
#     }
#     if (model == "lognormal-aerts") {
#       f <- apply(param_mat, 1, .cont_lognormal_aerts_f, test_doses)
#     }
#     if (model == "logskew-aerts") {
#       f <- apply(param_mat, 1, .cont_logskew_aerts_f, test_doses)
#     }
#     if (model == "invlogskew-aerts") {
#       f <- apply(param_mat, 1, .cont_invlogskew_aerts_f, test_doses)
#     }
#     if (model == "logistic-aerts") {
#       f <- apply(param_mat, 1, .cont_logistic_aerts_f, test_doses)
#     }
#     if (model == "probit-aerts") {
#       f <- apply(param_mat, 1, .cont_probit_aerts_f, test_doses)
#     }
#     if (model == "LMS") {
#       f <- apply(param_mat, 1, .cont_LMS_f, test_doses)
#     }
#     if (model == "polynomial") {
#       if (length(grep(": normal-ncv", tolower(full_mod))) > 0) {
#         degree <- ncol(param_mat) - 2
#       } else {
#         degree <- ncol(param_mat) - 1
#       }

#       f <- apply(param_mat[, 1:degree], 1, .cont_polynomial_f, test_doses)
#     }
#     # handle lognormal
#     if (grepl("Log-Normal", full_mod)) {
#       if (model %in% c("exp-aerts", "invexp-aerts", "gamma-aerts", "invgamma-aerts", "hill-aerts",
#                         "lomax-aerts", "invlomax-aerts", "lognormal-aerts", "logskew-aerts",
#                         "invlogskew-aerts", "logistic-aerts", "probit-aerts", "LMS")) {
#         retY <- exp(f + 0.5 * exp(object@parameters[length(object@parameters)]))
#       } else {
#         retY <- exp(log(f) + 0.5 * exp(object@parameters[length(object@parameters)]))
#       }
#       return(list(X = test_doses, Y = retY))
#     } else {
#       return(list(X = test_doses, Y = f))
#     }
#   }
# )
# .dichotomous_predict_model <- function(object, ...) {
#   fit <- object
#   tmp_args <- list(...)
#   if (!exists("new_doses", where = tmp_args)) {
#     new_doses <- NULL
#   } else {
#     new_doses <- tmp_args$new_doses
#   }
#   if (is.null(new_doses)) {
#     test_doses <- fit$data[, 1]
#   } else {
#     test_doses <- new_doses
#   }

#   if (fit$model == "hill") {
#     f <- .dich_hill_f(fit$parameters, test_doses)
#   }
#   if (fit$model == "gamma") {
#     f <- .dich_gamma_f(fit$parameters, test_doses)
#   }
#   if (fit$model == "logistic") {
#     f <- .dich_logist_f(fit$parameters, test_doses)
#   }
#   if (fit$model == "log-logistic") {
#     f <- .dich_llogist_f(fit$parameters, test_doses)
#   }
#   if (fit$model == "probit") {
#     f <- .dich_probit_f(fit$parameters, test_doses)
#   }
#   if (fit$model == "log-probit") {
#     f <- .dich_lprobit_f(fit$parameters, test_doses)
#   }
#   if (fit$model == "multistage") {
#     f <- .dich_multistage_f(fit$parameters, test_doses)
#   }
#   if (fit$model == "qlinear") {
#     f <- .dich_qlinear_f(fit$parameters, test_doses)
#   }
#   if (fit$model == "weibull") {
#     f <- .dich_weibull_f(fit$parameters, test_doses)
#   }

#   returnV <- list(X = test_doses, Y = f)
#   return(returnV)
# }


# .dichotomous_predict_model_mcmc <- function(object, ...) {
#   fit <- object
#   tmp_args <- list(...)

#   if (!exists("new_doses", where = tmp_args)) {
#     new_doses <- NULL
#   } else {
#     new_doses <- tmp_args$new_doses
#   }

#   if (is.null(new_doses)) {
#     test_doses <- fit$data[, 1]
#   } else {
#     test_doses <- new_doses
#   }

#   if (fit$model == "hill") {
#     f <- apply(fit$mcmc_result$PARM_samples, 1, .dich_hill_f, d = test_doses)
#   }
#   if (fit$model == "gamma") {
#     f <- apply(fit$mcmc_result$PARM_samples, 1, .dich_gamma_f, d = test_doses)
#   }
#   if (fit$model == "logistic") {
#     f <- apply(fit$mcmc_result$PARM_samples, 1, .dich_logist_f, d = test_doses)
#   }
#   if (fit$model == "log-logistic") {
#     f <- apply(fit$mcmc_result$PARM_samples, 1, .dich_llogist_f, d = test_doses)
#   }
#   if (fit$model == "probit") {
#     f <- apply(fit$mcmc_result$PARM_samples, 1, .dich_probit_f, d = test_doses)
#   }
#   if (fit$model == "log-probit") {
#     f <- apply(fit$mcmc_result$PARM_samples, 1, .dich_lprobit_f, d = test_doses)
#   }
#   if (fit$model == "multistage") {
#     f <- apply(fit$mcmc_result$PARM_samples, 1, .dich_multistage_f, d = test_doses)
#   }
#   if (fit$model == "qlinear") {
#     f <- apply(fit$mcmc_result$PARM_samples, 1, .dich_qlinear_f, d = test_doses)
#   }
#   if (fit$model == "weibull") {
#     f <- apply(fit$mcmc_result$PARM_samples, 1, .dich_weibull_f, d = test_doses)
#   }

#   returnV <- list(X = test_doses, Y = f)
#   return(returnV)
# }

# .continuous_predict_model <- function(object, ...) {
#   fit <- object
#   tmp_args <- list(...)
#   if (!exists("new_doses", where = tmp_args)) {
#     new_doses <- NULL
#   } else {
#     new_doses <- tmp_args$new_doses
#   }


#   data_d <- fit$data

#   if (ncol(data_d) == 4) {
#     # sufficient statistics
#     mean <- data_d[, 2, drop = F]
#     se <- data_d[, 4, drop = F] / sqrt(data_d[, 3, drop = F])
#     doses <- data_d[, 1, drop = F]
#     lm_fit <- lm(mean ~ doses, weights = 1 / (se * se))
#   } else {
#     Response <- data_d[, 2, drop = F]
#     doses <- data_d[, 1, drop = F]
#     lm_fit <- lm(Response ~ doses)
#   }

#   if (is.null(new_doses)) {
#     test_doses <- fit$data[, 1]
#   } else {
#     test_doses <- new_doses
#   }

#   if (coefficients(lm_fit)[2] < 0) {
#     decrease <- TRUE
#   } else {
#     decrease <- FALSE
#   }

#   if (fit$model == "FUNL") {
#     f <- .cont_FUNL_f(fit$parameters, test_doses)
#   }
#   if (fit$model == "hill") {
#     f <- .cont_hill_f(fit$parameters, test_doses)
#   }
#   if (fit$model == "exp-3") {
#     f <- .cont_exp_3_f(fit$parameters, test_doses, decrease)
#   }
#   if (fit$model == "exp-5") {
#     f <- .cont_exp_5_f(fit$parameters, test_doses)
#   }
#   if (fit$model == "power") {
#     f <- .cont_power_f(fit$parameters, test_doses)
#   }
#   if (fit$model == "exp-aerts") {
#     f <- .cont_exp_aerts_f(fit$parameters, test_doses)
#   }
#   if (fit$model == "invexp-aerts") {
#     f <- .cont_invexp_aerts_f(fit$parameters, test_doses)
#   }
#   if (fit$model == "gamma-aerts") {
#     f <- .cont_gamma_aerts_f(fit$parameters, test_doses)
#   }
#   if (fit$model == "invgamma-aerts") {
#     f <- .cont_invgamma_aerts_f(fit$parameters, test_doses)
#   }
#   if (fit$model == "hill-aerts") {
#     f <- .cont_hill_aerts_f(fit$parameters, test_doses)
#   }
#   if (fit$model == "lomax-aerts") {
#     f <- .cont_lomax_aerts_f(fit$parameters, test_doses)
#   }
#   if (fit$model == "invlomax-aerts") {
#     f <- .cont_invlomax_aerts_f(fit$parameters, test_doses)
#   }
#   if (fit$model == "lognormal-aerts") {
#     f <- .cont_lognormal_aerts_f(fit$parameters, test_doses)
#   }
#   if (fit$model == "logskew-aerts") {
#     f <- .cont_logskew_aerts_f(fit$parameters, test_doses)
#   }
#   if (fit$model == "invlogskew-aerts") {
#     f <- .cont_invlogskew_aerts_f(fit$parameters, test_doses)
#   }
#   if (fit$model == "logistic-aerts") {
#     f <- .cont_logistic_aerts_f(fit$parameters, test_doses)
#   }
#   if (fit$model == "probit-aerts") {
#     f <- .cont_probit_aerts_f(fit$parameters, test_doses)
#   }
#   if (fit$model == "LMS") {
#     f <- .cont_LMS_f(fit$parameters, test_doses)
#   }
#   if (fit$model == "polynomial") {
#     if (length(grep(": normal-ncv", tolower(fit$full_model))) > 0) {
#       degree <- length(fit$parameters) - 2
#     } else {
#       degree <- length(fit$parameters) - 1
#     }

#     f <- .cont_polynomial_f(fit$parameters[1:degree], test_doses)
#   }

#   if (grepl("Log-Normal", fit$full_model)) {
#     if (fit$model %in% c("exp-aerts", "invexp-aerts", "gamma-aerts", "invgamma-aerts", "hill-aerts",
#                         "lomax-aerts", "invlomax-aerts", "lognormal-aerts", "logskew-aerts",
#                         "invlogskew-aerts", "logistic-aerts", "probit-aerts", "LMS")) {
#       returnV <- list(X = test_doses, Y = exp(as.numeric(f) + 0.5 * exp(fit$parameters[length(fit$parameters)]))) # lognormal mean  # nolint
#     } else {
#       returnV <- list(X = test_doses, Y = exp(log(as.numeric(f)) + 0.5 * exp(fit$parameters[length(fit$parameters)]))) # lognormal mean  # nolint
#     }
#   } else {
#     returnV <- list(X = test_doses, Y = as.numeric(f))
#   }
#   return(returnV)
# }


# .continuous_predict_model_mcmc <- function(object, ...) {
#   fit <- object
#   tmp_args <- list(...)
#   if (!exists("new_doses", where = tmp_args)) {
#     new_doses <- NULL
#   } else {
#     new_doses <- tmp_args$new_doses
#   }
#   data_d <- fit$data

#   if (ncol(data_d) == 4) {
#     # sufficient statistics
#     mean <- data_d[, 2, drop = F]
#     se <- data_d[, 4, drop = F] / sqrt(data_d[, 3, drop = F])
#     doses <- data_d[, 1, drop = F]
#     lm_fit <- lm(mean ~ doses, weights = 1 / (se * se))
#   } else {
#     Response <- data_d[, 2, drop = F]
#     doses <- data_d[, 1, drop = F]
#     lm_fit <- lm(Response ~ doses)
#   }

#   if (is.null(new_doses)) {
#     test_doses <- fit$data[, 1]
#   } else {
#     test_doses <- new_doses
#   }

#   if (coefficients(lm_fit)[2] < 0) {
#     decrease <- TRUE
#   } else {
#     decrease <- FALSE
#   }

#   if (fit$model == "FUNL") {
#     f <- apply(fit$mcmc_result$PARM_samples, 1, .cont_FUNL_f, test_doses)
#   }
#   if (fit$model == "hill") {
#     f <- apply(fit$mcmc_result$PARM_samples, 1, .cont_hill_f, test_doses)
#   }
#   if (fit$model == "exp-3") {
#     f <- apply(fit$mcmc_result$PARM_samples, 1, .cont_exp_3_f, test_doses)
#   }
#   if (fit$model == "exp-5") {
#     f <- apply(fit$mcmc_result$PARM_samples, 1, .cont_exp_5_f, test_doses)
#   }
#   if (fit$model == "power") {
#     f <- apply(fit$mcmc_result$PARM_samples, 1, .cont_power_f, test_doses)
#   }
#   if (fit$model == "exp-aerts") {
#     f <- apply(fit$mcmc_result$PARM_samples, 1, .cont_exp_aerts_f, test_doses)
#   }
#   if (fit$model == "invexp-aerts") {
#     f <- apply(fit$mcmc_result$PARM_samples, 1, .cont_invexp_aerts_f, test_doses)
#   }
#   if (fit$model == "gamma-aerts") {
#     f <- apply(fit$mcmc_result$PARM_samples, 1, .cont_gamma_aerts_f, test_doses)
#   }
#   if (fit$model == "invgamma-aerts") {
#     f <- apply(fit$mcmc_result$PARM_samples, 1, .cont_invgamma_aerts_f, test_doses)
#   }
#   if (fit$model == "hill-aerts") {
#     f <- apply(fit$mcmc_result$PARM_samples, 1, .cont_hill_aerts_f, test_doses)
#   }
#   if (fit$model == "lomax-aerts") {
#     f <- apply(fit$mcmc_result$PARM_samples, 1, .cont_lomax_aerts_f, test_doses)
#   }
#   if (fit$model == "invlomax-aerts") {
#     f <- apply(fit$mcmc_result$PARM_samples, 1, .cont_invlomax_aerts_f, test_doses)
#   }
#   if (fit$model == "lognormal-aerts") {
#     f <- apply(fit$mcmc_result$PARM_samples, 1, .cont_lognormal_aerts_f, test_doses)
#   }
#   if (fit$model == "logskew-aerts") {
#     f <- apply(fit$mcmc_result$PARM_samples, 1, .cont_logskew_aerts_f, test_doses)
#   }
#   if (fit$model == "invlogskew-aerts") {
#     f <- apply(fit$mcmc_result$PARM_samples, 1, .cont_invlogskew_aerts_f, test_doses)
#   }
#   if (fit$model == "logistic-aerts") {
#     f <- apply(fit$mcmc_result$PARM_samples, 1, .cont_logistic_aerts_f, test_doses)
#   }
#   if (fit$model == "probit-aerts") {
#     f <- apply(fit$mcmc_result$PARM_samples, 1, .cont_probit_aerts_f, test_doses)
#   }
#   if (fit$model == "LMS") {
#     f <- apply(fit$mcmc_result$PARM_samples, 1, .cont_LMS_f, test_doses)
#   }
#   if (fit$model == "polynomial") {
#     if (length(grep(": normal-ncv", tolower(fit$full_model))) > 0) {
#       degree <- ncol(fit$mcmc_result$PARM_samples) - 2
#     } else {
#       degree <- ncol(fit$mcmc_result$PARM_samples) - 1
#     }

#     f <- apply(fit$mcmc_result$PARM_samples[, 1:degree], 1, .cont_polynomial_f, test_doses)
#   }

#   if (grepl("Log-Normal", fit$full_model)) {
#     if (fit$model %in% c("exp-aerts", "invexp-aerts", "gamma-aerts", "invgamma-aerts", "hill-aerts",
#                         "lomax-aerts", "invlomax-aerts", "lognormal-aerts", "logskew-aerts",
#                         "invlogskew-aerts", "logistic-aerts", "probit-aerts", "LMS")) {
#       returnV <- list(X = test_doses, Y = exp(f + 0.5 * exp(fit$parameters[length(fit$parameters)]))) # lognormal mean  # nolint
#     } else {
#       returnV <- list(X = test_doses, Y = exp(log(f) + 0.5 * exp(fit$parameters[length(fit$parameters)]))) # lognormal mean  # nolint
#     }
#   } else {
#     returnV <- list(X = test_doses, Y = f)
#   }
#   return(returnV)
# }
