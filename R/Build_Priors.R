# Copyright 2020  NIEHS <matt.wheeler@nih.gov>
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

# .parse_prior <- function(prior) {
#   rV <- list()
#   rV$prior <- prior$prior

#   temp_a <- regexpr("[[][a-zA-Z]+-*[a-zA-Z]+[]]", prior$model)
#   start <- temp_a[1] + 1
#   end <- start + attr(temp_a, "match.length") - 3
#   if (temp_a == -1) {
#     stop("Could not find a distribution for analysis.")
#   }
#   rV$distribution <- substr(prior$model, start, end)
#   rV$model <- prior$mean

#   class(rV) <- "BMD_Bayes_continuous_model"
  
#   return(rV)
# }

#' Build a list of default priors for model averaging
#'
#' @title ma_build_priors - Build a list of default priors for model averaging
#' @param Y Response matrix (optional) but preferred to fix variance prior
#' @param model_list Vector of models to use. See \code{\link{single_continuous_fit}} for details on the models. Overrides ma_type
#' @param distribution_list Vector of corresponding underlying distribution to use ("normal", "normal-ncv", "lognormal"). Defaults to normal-ncv
#' @param ma_type Models to average over ("original", "EFSA", or "all"). All corresponds to all implemented models; 
#' EFSA and original are described in \code{\link{ma_continuous_fit}} under the EFSA parameter.
#'
#' @return A list of configurations for the single models (priors, model type). Can be fed as the model_list to ma_continuous_fit
#'
#' @examples
#' #get EFSA model priors
#' prior_list <- ma_build_priors(ma_type = "EFSA")
#' 
# ma_build_priors <- function(Y = NA, model_list = NA, distribution_list = NA, ma_type = "EFSA"){
#   #build model/distribution lists by ma_type
#   if(is.na(model_list[1])){
#     if(ma_type == "EFSA"){
#       model_list <- c(rep("exp-aerts", 2), rep("invexp-aerts",2), rep("hill-aerts", 2), rep("lognormal-aerts",2),
#                       rep("gamma-efsa", 2), rep("LMS", 2), rep("probit-aerts", 2), rep("logistic-aerts",2))
#       distribution_list <- rep(c("normal", "lognormal"), 8)
#     }
#     if(ma_type == "original"){
#       model_list <- c(rep("hill", 2), rep("exp-3", 3), rep("exp-5", 3), rep("power", 2))
#       distribution_list <- c(
#         "normal", "normal-ncv", rep(c("normal", "normal-ncv", "lognormal"), 2),
#         "normal", "normal-ncv"
#       )
#     }
#     if(ma_type == "all"){
#       model_list <- rep(.continuous_models, each=length(.continuous_distributions))
#       distribution_list <- rep(.continuous_distributions, length(.continuous_models))
#       badd_inds <- c(3, 12, 13:18)
#       model_list <- model_list[-badd_inds]
#       distribution_list <- distribution_list[-badd_inds]
#     }
#   }else{
#     #check that the models are all valid
#     if(!all(model_list %in% .continuous_models[-(5:6)])){
#       stop('Please specify only the following model types: \n
#             "hill","exp-3","exp-5","power", "exp-aerts", "invexp-aerts", "gamma-aerts", "invgamma-aerts", "hill-aerts",
#             "lomax-aerts", "invlomax-aerts", "lognormal-aerts", "logskew-aerts", "invlogskew-aerts", "logistic-aerts", "probit-aerts", "LMS",
#             "gamma-efsa"')
#     }
#     #if no distributions, default to normal-ncv
#     if(is.na(distribution_list[1])){
#       temp <- unique(model_list)
#       if(length(temp) != length(model_list)){
#         warning("Removing duplicate models. Please specify distribution_list to avoid this behavior.")
#         model_list <- temp
#       }
#       distribution_list <- rep("normal-ncv", length(temp))
#     }else{
#       #check size compatibility
#       if(length(model_list) != length(distribution_list)){
#         stop("Please specify a distribution for each model.")
#       }
#       #check distribution validity
#       if(!all(distribution_list %in% .continuous_distributions)){
#         stop('Please specify only the following distribution types: "normal", "normal-ncv", "lognormal"')
#       }
#     }
#   }
#   #check for negative values if response provided
#   if(!is.na(Y[1])){
#     is_neg = .check_negative_response(Y)
#     if(is_neg){
#       tmpIdx <- which(distribution_list == "lognormal")
#       model_list <- model_list[-tmpIdx]
#       distribution_list <- distribution_list[-tmpIdx]
#       if(identical(tmpIdx, integer(0))){
#         warning("Negative response values were found in the data.  All lognormal
#             models were removed from the analysis.")
#       }
#     }
#   }
#   prior_list <- list()
#   for (ii in 1:length(model_list)) {
#     PR <- .bayesian_prior_continuous_default(model_list[ii], distribution_list[ii], 2)
#     # specify variance of last parameter to variance of response if response provided
#     if(!is.na(Y[1])){
#       if (distribution_list[ii] == "lognormal") {
#         if (ncol(Y) > 1) {
#           PR$priors[nrow(PR$priors), 2] <- log(mean(Y[, 3])^2)
#         } else {
#           PR$priors[nrow(PR$priors), 2] <- log(var(log(Y)))
#         }
#       } else {
#         if (ncol(Y) > 1) {
#           if (distribution_list[ii] == "normal") {
#             PR$priors[nrow(PR$priors), 2] <- log(mean(Y[, 3])^2)
#           } else {
#             PR$priors[nrow(PR$priors), 2] <- log(abs(mean(Y[1, ])) / mean(Y[, 3])^2)
#           }
#         } else {
#           if (distribution_list[ii] == "normal") {
#             PR$priors[nrow(PR$priors), 2] <- log(var(Y))
#           } else {
#             PR$priors[nrow(PR$priors), 2] <- log(abs(mean(Y)) / var(Y))
#           }
#         }
#       }
#     }
#     t_prior_result <- create_continuous_prior(PR, model_list[ii], distribution_list[ii], 2)
#     # PR <- t_prior_result$prior
#     prior_list[[ii]] <- t_prior_result#list(prior = PR, model_tye = model_list[ii], dist = distribution_list[ii])
#   }
#   return(prior_list)
# }

#' @title create_continuous_prior  Given priorlist, a model,
#'        and a distribution. Create a prior for a given analysis.
#' @param prior_list First Prior
#' @param model Model to be used
#' one of \code{"hill", "exp-3", "exp-5", "power", "polynomial", "FUNL", "exp-aerts", "invexp-aerts", "gamma-aerts", "invgamma-aerts", "hill-aerts", 
#' "lomax-aerts", "invlomax-aerts", "lognormal-aerts", "logskew-aerts", "invlogskew-aerts", "logistic-aerts", "probit-aerts", "gamma-efsa"}
#' @param distribution - Normal "normal", Normal non-constant variance "normal-ncv", or
#'                       log-normal "lognormal"
#' @param deg - For polynomial models only, the degree of the polynomial.
#' @return new BMDprior list. This object is essentially a prior list constructed by
#' \code{create_prior_lists} with a model type and variance type.
#'
#' @examples
#' plist <- create_prior_list(
#'   normprior(0, 0.1, -100, 100), # a
#'   normprior(0, 1, -1e2, 1e2), # b
#'   lnormprior(1, 0.2, 0, 18), # k
#'   normprior(0, 1, -18, 18)
#' )
#'
#' power_normal <- create_continuous_prior(plist, "power", "normal")
#'
# create_continuous_prior <- function(prior_list, model, distribution, deg = 2) {
#   if (!("BMDmodelprior" %in% class(prior_list))) {
#     stop("Prior is not of a 'BMDmodelprior' class. A probable solution is to
#           define the prior using function `create_prior_list`.")
#   }
#   if (!(model %in% .continuous_models)) {
#     stop(cat("Model Type must be one of:", .continuous_models, "\n"))
#   }
#   if (!(distribution %in% .continuous_distributions)) {
#     stop(cat("Distribution must be one of:", .continuous_distributions, "\n"))
#   }
#   temp <- floor(deg)

#   if (deg < 1) {
#     stop("Polynomial degree must be greater than or equal to 1.")
#   }
#   if (temp != deg) {
#     stop("Polynomial degree must be an integer.")
#   }

#   p <- NA
#   if(grepl("aerts", model)){
#     if(model %in% c("exp-aerts", "invexp-aerts", "hill-aerts", "lognormal-aerts")){
#       p <- .check_4param(prior_list, distribution)
#     }
#     if(model %in% c("logistic-aerts", "probit-aerts")){
#       p <- .check_4param_sigmoidal(prior_list, distribution)
#     }
#     if(model %in% c("gamma-aerts", "invgamma-aerts", "lomax-aerts", "invlomax-aerts",
#                     "logskew-aerts", "invlogskew-aerts")){
#       p <- .check_5param(prior_list, distribution)
#     }
#     if(model %in% c("gamma-aerts","invgamma-aerts")){
#       p<- .check_5param_gamma(prior_list,distribution)
#     }
#     p$mean <- model
#   }else{
#     if("LMS" == model){
#       p <- .check_4param(prior_list, distribution)
#       p$mean <- model
#     }
#     if("gamma-efsa" == model){
#       p <- .check_4param_gamma(prior_list, distribution)
#       p$mean <- model
#     }
#     if ("hill" == model) {
#       p <- .check_hill(prior_list, distribution)
#     }
    
#     if ("FUNL" == model) {
#       p <- .check_FUNL(prior_list, distribution)
#     }
    
#     if ("exp-5" == model) {
#       p <- .check_exp5(prior_list, distribution)
#     }
    
#     if ("exp-3" == model) {
#       p <- .check_exp3(prior_list, distribution)
#     }
    
#     if ("polynomial" == model) {
#       p <- .check_polynomial(prior_list, distribution)
#     }
    
#     if ("power" == model) {
#       p <- .check_power(prior_list, distribution)
#     }
#   }

#   class(p) <- "BMD_Bayes_continuous_model"

#   return(p)
# }


#' @title create_dichotomous_prior  Given priorlist, a model,
#'        and a distribution. Create a prior for a given analysis.
#' @param prior First Prior
#' @param model Model to be used should be one of"hill","gamma","logistic","log-logistic","log-probit","multistage", "probit", "qlinear", or "weibull"
#' @return new BMDprior list that can be used in a dichotomous fit.
#'
#' @examples
#' plist <- create_prior_list(
#'   normprior(0, 0.1, -100, 100), # a
#'   lnormprior(1, 0.2, 0, 18)
#' )
#'
#' power_normal <- create_dichotomous_prior(plist, "logistic")
#'
# create_dichotomous_prior <- function(prior, model) {
#   if (!("BMDmodelprior" %in% class(prior))) {
#     stop("Prior is not of a 'BMDmodelprior' class. A probable solution is to
#           define the prior using function `create_prior_list`.")
#   }
#   if (!(model %in% .dichotomous_models)) {
#     stop(cat("Model Type must be one of:", .dichotomous_models, "\n"))
#   }


#   p <- NA
#   temp <- prior[[1]]

#   if (sum(temp[, 4] > temp[, 5]) > 0) {
#     stop("One of the parameter's lower bounds is greater than the upper bound.")
#   }

#   if ("hill" == model) {
#     p <- .check_d_hill(prior)
#   }

#   if ("gamma" == model) {
#     p <- .check_d_gamma(prior)
#   }

#   if ("logistic" == model) {
#     p <- .check_d_logistic(prior)
#   }

#   if ("log-logistic" == model) {
#     p <- .check_d_llogistic(prior)
#   }

#   if ("multistage" == model) {
#     p <- .check_d_multistage(prior)
#   }

#   if ("probit" == model) {
#     p <- .check_d_probit(prior)
#   }

#   if ("log-probit" == model) {
#     p <- .check_d_lprobit(prior)
#   }

#   if ("qlinear" == model) {
#     p <- .check_d_qlinear(prior)
#   }

#   if ("weibull" == model) {
#     p <- .check_d_weibull(prior)
#   }

#   class(p) <- "BMD_Bayes_dichotomous_model"

#   return(p)
# }

