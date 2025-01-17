setGeneric("parse_prior", function(prior) standardGeneric("parse_prior"))
setGeneric("create_continuous_prior",
  function(prior_list, model, distribution, deg = 2) {
    standardGeneric("create_continuous_prior")
  }
)
setGeneric("create_dichotomous_prior", function(prior, model) {
  standardGeneric("create_dichotomous_prior")
})

setMethod("parse_prior", "list", function(prior) {
  # Build the distribution from the string
  rV <- list()
  rV$prior <- prior$prior

  temp_a <- regexpr("[[][a-zA-Z]+-*[a-zA-Z]+[]]", prior$model)
  start <- temp_a[1] + 1
  end <- start + attr(temp_a, "match.length") - 3
  if (temp_a == -1) {
    stop("Could not find a distribution for analysis.")
  }
  rV$distribution <- substr(prior$model, start, end)
  rV$model <- prior$mean
  BMD_Bayes_continuous_model(
    prior = rV$prior,
    model = rV$model,
    distribution = rV$distribution,
    parameters = character(),
    mean = rV$model
  )
})

setMethod("create_continuous_prior",
  signature(
    prior_list = "ANY",
    model = "character",
    distribution = "character"
  ),
  function(prior_list, model, distribution, deg = 2) {
    if (!("BMDmodelprior" %in% class(prior_list))) {
      stop("Prior is not of a 'BMDmodelprior' class. 
           Define the prior with `create_prior_list()` or similar.")
    }

    p <- NA
    if (grepl("aerts", model)) {
      if (model %in% c("exp-aerts", "invexp-aerts", "hill-aerts", "lognormal-aerts")) {
        p <- .check_4param(prior_list, distribution)
      } else if (model %in% c("logistic-aerts", "probit-aerts")) {
        p <- .check_4param_sigmoidal(prior_list, distribution)
      } else if (model %in% c("gamma-aerts", "invgamma-aerts", "lomax-aerts",
                              "invlomax-aerts", "logskew-aerts", "invlogskew-aerts")) {
        p <- .check_5param(prior_list, distribution)
      }
      if (model %in% c("gamma-aerts", "invgamma-aerts")) {
        p <- .check_5param_gamma(prior_list, distribution)
      }
      p$mean <- model
    } else {
      # handle the non-aerts
      if (model == "LMS") {
        p <- .check_4param(prior_list, distribution)
        p$mean <- model
      } else if (model == "gamma-efsa") {
        p <- .check_4param_gamma(prior_list, distribution)
        p$mean <- model
      } else if (model == "hill") {
        p <- .check_hill(prior_list, distribution)
      } else if (model == "FUNL") {
        p <- .check_FUNL(prior_list, distribution)
      } else if (model == "exp-5") {
        p <- .check_exp5(prior_list, distribution)
      } else if (model == "exp-3") {
        p <- .check_exp3(prior_list, distribution)
      } else if (model == "polynomial") {
        p <- .check_polynomial(prior_list, distribution)
      } else if (model == "power") {
        p <- .check_power(prior_list, distribution)
      }
    }

    ret_obj <- BMD_Bayes_continuous_model(
      prior = p$prior,
      distribution = distribution,
      model = p$model,
      parameters = p$parameters,
      mean = p$mean,
      degree = ifelse(!is.null(p$degree), p$degree, deg)
    )
    return(ret_obj)
  }
)

ma_build_priors <- function(Y = NA, model_list = NA, distribution_list = NA, ma_type = "EFSA") {
  if (is.na(model_list[1])) {
    if (ma_type == "EFSA") {
      model_list <- c(
        rep("exp-aerts", 2), rep("invexp-aerts", 2), rep("hill-aerts", 2),
        rep("lognormal-aerts", 2), rep("gamma-efsa", 2), rep("LMS", 2),
        rep("probit-aerts", 2), rep("logistic-aerts", 2)
      )
      distribution_list <- rep(c("normal", "lognormal"), 8)
    }
    else if (ma_type == "original") {
      model_list <- c(
        rep("hill", 2), rep("exp-3", 3),
        rep("exp-5", 3), rep("power", 2)
      )
      distribution_list <- c(
        "normal", "normal-ncv",
        rep(c("normal", "normal-ncv", "lognormal"), 2),
        "normal", "normal-ncv"
      )
    }
    else if (ma_type == "all") {
      model_list <- rep(.continuous_models, each = length(.continuous_distributions))
      distribution_list <- rep(.continuous_distributions, length(.continuous_models))
      badd_inds <- c(3, 12, 13:18)
      model_list <- model_list[-badd_inds]
      distribution_list <- distribution_list[-badd_inds]
    }
  } else {
    # check that the models are valid
    if (!all(model_list %in% .continuous_models[-(5:6)])) {
      stop("Please specify only the following model types: \n
            \"hill\",\"exp-3\",\"exp-5\",\"power\", \"exp-aerts\", \"invexp-aerts\", \"gamma-aerts\", \"invgamma-aerts\", \"hill-aerts\",
            \"lomax-aerts\", \"invlomax-aerts\", \"lognormal-aerts\", \"logskew-aerts\", \"invlogskew-aerts\", \"logistic-aerts\", \"probit-aerts\", \"LMS\",
            \"gamma-efsa\"")
    }
    # if no distributions, default to normal-ncv
    if (is.na(distribution_list[1])) {
      temp <- unique(model_list)
      if (length(temp) != length(model_list)) {
        warning("Removing duplicate models. Please specify distribution_list to avoid this behavior.")
        model_list <- temp
      }
      distribution_list <- rep("normal-ncv", length(temp))
    } else {
      # check size compatibility
      if (length(model_list) != length(distribution_list)) {
        stop("Please specify a distribution for each model.")
      }
      # check distribution validity
      if (!all(distribution_list %in% .continuous_distributions)) {
        stop("Please specify only the following distribution types: \"normal\", \"normal-ncv\", \"lognormal\"")
      }
    }
  }

  if (!is.na(Y[1])) {
    is_neg <- .check_negative_response(Y)
    if (is_neg) {
      tmp_idx <- which(distribution_list == "lognormal")
      model_list <- model_list[-tmp_idx]
      distribution_list <- distribution_list[-tmp_idx]
      if (identical(tmp_idx, integer(0))) {
        warning("Negative response values found. All lognormal models were removed.")
      }
    }
  }

  prior_list <- list()
  for (ii in seq_along(model_list)) {
    PR <- .bayesian_prior_continuous_default(model_list[ii], distribution_list[ii], 2)

    if (!is.na(Y[1])) {
      if (distribution_list[ii] == "lognormal") {
        if (ncol(Y) > 1) {
          PR$priors[nrow(PR$priors), 2] <- log(mean(Y[, 3]) ^ 2)
        } else {
          PR$priors[nrow(PR$priors), 2] <- log(var(log(Y)))
        }
      } else {
        if (ncol(Y) > 1) {
          if (distribution_list[ii] == "normal") {
            PR$priors[nrow(PR$priors), 2] <- log(mean(Y[, 3]) ^ 2)
          } else {
            PR$priors[nrow(PR$priors), 2] <- log(abs(mean(Y[1,])) / mean(Y[, 3]) ^ 2)
          }
        } else {
          if (distribution_list[ii] == "normal") {
            PR$priors[nrow(PR$priors), 2] <- log(var(Y))
          } else {
            PR$priors[nrow(PR$priors), 2] <- log(abs(mean(Y)) / var(Y))
          }
        }
      }
    }

    t_prior_result <- create_continuous_prior(PR, model_list[ii], distribution_list[ii], 2)
    prior_list[[ii]] <- t_prior_result
  }

  return(prior_list)
}
