###############################################################################
# Set a Generic for `plot()`
###############################################################################
setGeneric("plot",
  function(x, y, ...) standardGeneric("plot")
)

###############################################################################
# S4 Class Definitions
###############################################################################
# For MCMC-based continuous fits
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
    transformed = "logical"
  )
)

# For MLE / Laplace-based continuous fits
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
    transformed = "logical"
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
  transformed = FALSE
) {
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
    transformed = transformed
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
  transformed = FALSE
) {
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
    transformed = transformed
  )
}

setClass(
  "BMD_dichotomous_fit_MCMC",
  slots = c(
    full_model = "character",
    parameters = "numeric",
    covariance = "matrix",
    bmd_dist = "matrix",
    bmd = "numeric",
    maximum = "numeric",
    gof_p_value = "numeric",
    gof_chi_sqr_statistic = "numeric",
    prior = "ANY",
    model = "character",
    data = "matrix",
    mcmc_result = "list", # has PARM_samples, BMD_samples
    options = "numeric"
  )
)

BMD_dichotomous_fit_MCMC <- function(
  full_model   = "",
  parameters   = numeric(),
  covariance   = matrix(0,0,0),
  bmd_dist     = matrix(0,0,0),
  bmd          = numeric(3),
  maximum      = NA_real_,
  gof_p_value  = NA_real_,
  gof_chi_sqr_statistic = NA_real_,
  prior        = NULL,
  model        = "",
  data         = matrix(),
  mcmc_result  = list(),
  options      = numeric()
) {
  new("BMD_dichotomous_fit_MCMC",
      full_model   = full_model,
      parameters   = parameters,
      covariance   = covariance,
      bmd_dist     = bmd_dist,
      bmd          = bmd,
      maximum      = maximum,
      gof_p_value  = gof_p_value,
      gof_chi_sqr_statistic = gof_chi_sqr_statistic,
      prior        = prior,
      model        = model,
      data         = data,
      mcmc_result  = mcmc_result,
      options      = options
  )
}

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
    mcmc_result = "ANY",
    options = "numeric" # BMR, alpha, samples, burnin, etc.
  )
)

BMD_dichotomous_fit_maximized <- function(
  full_model   = "",
  parameters   = numeric(),
  covariance   = matrix(0,0,0),
  bmd_dist     = matrix(0,0,0),
  bmd          = numeric(3),
  maximum      = NA_real_,
  gof_p_value  = NA_real_,
  gof_chi_sqr_statistic = NA_real_,
  prior        = NULL,
  model        = "",
  data         = matrix(),
  mcmc_result  = NULL,
  options      = numeric()
) {
  new("BMD_dichotomous_fit_maximized",
      full_model   = full_model,
      parameters   = parameters,
      covariance   = covariance,
      bmd_dist     = bmd_dist,
      bmd          = bmd,
      maximum      = maximum,
      gof_p_value  = gof_p_value,
      gof_chi_sqr_statistic = gof_chi_sqr_statistic,
      prior        = prior,
      model        = model,
      data         = data,
      mcmc_result  = mcmc_result,
      options      = options
  )
}

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
  full_model = "BMDdichotomous_MA"
) {
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
  cat("  Model:          ", object@model, "\n")
  cat("  BMD:            ", paste(object@bmd, collapse = ", "), "\n")
  cat("  Maximum Posterior: ", object@maximum, "\n")
  # etc.
  invisible(object)
})

###############################################################################
# S4 Methods
###############################################################################
setMethod("show", "BMD_continuous_fit_maximized", function(object) {
  cat("BMD Continuous Fit (Maximized)\n")
  cat("  Model:   ", object@model, "\n")
  cat("  BMD:     ", paste(object@bmd, collapse = ", "), "\n")
  cat("  Maximum: ", object@maximum, "\n")
  invisible(object)
})

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

    Dose <- NULL # to suppress "no visible binding" NOTE in CRAN checks, etc.
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
    test_doses <- seq(min(doses), max(doses) * 1.03,
                      (max(doses) * 1.03 - min(doses)) / 500)
    if (IS_tranformed) {
      test_doses <- asinh(test_doses)
    }

    Q <- switch(
      x@model,
      "exp-aerts" = apply(x@mcmc_result$PARM_samples, 1, .cont_exp_aerts_f, d = test_doses, decrease = decrease),
      "invexp-aerts" = apply(x@mcmc_result$PARM_samples, 1, .cont_invexp_aerts_f, d = test_doses, decrease = decrease),
      "gamma-aerts" = apply(x@mcmc_result$PARM_samples, 1, .cont_gamma_aerts_f, d = test_doses, decrease = decrease),
      "invgamma-aerts" = apply(x@mcmc_result$PARM_samples, 1, .cont_invgamma_aerts_f, d = test_doses, decrease = decrease),
      "hill-aerts" = apply(x@mcmc_result$PARM_samples, 1, .cont_hill_aerts_f, d = test_doses, decrease = decrease),
      "lomax-aerts" = apply(x@mcmc_result$PARM_samples, 1, .cont_lomax_aerts_f, d = test_doses, decrease = decrease),
      "invlomax-aerts" = apply(x@mcmc_result$PARM_samples, 1, .cont_invlomax_aerts_f, d = test_doses, decrease = decrease),
      "lognormal-aerts" = apply(x@mcmc_result$PARM_samples, 1, .cont_lognormal_aerts_f, d = test_doses, decrease = decrease),
      "logskew-aerts" = apply(x@mcmc_result$PARM_samples, 1, .cont_logskew_aerts_f, d = test_doses, decrease = decrease),
      "invlogskew-aerts" = apply(x@mcmc_result$PARM_samples, 1, .cont_invlogskew_aerts_f, d = test_doses, decrease = decrease),
      "logistic-aerts" = apply(x@mcmc_result$PARM_samples, 1, .cont_logistic_aerts_f, d = test_doses, decrease = decrease),
      "probit-aerts" = apply(x@mcmc_result$PARM_samples, 1, .cont_probit_aerts_f, d = test_doses, decrease = decrease),
      "LMS" = apply(x@mcmc_result$PARM_samples, 1, .cont_LMS_f, d = test_doses, decrease = decrease),
      "gamma-efsa" = apply(x@mcmc_result$PARM_samples, 1, .cont_gamma_efsa_f, d = test_doses, decrease = decrease),
      NULL
    )
    if (isLogNormal && !is.null(Q)) {
      # original: Q <- exp(Q + exp(fit$mcmc_result$PARM_samples[,ncol(...)]) / 2)
      Q <- exp(Q + exp(x@mcmc_result$PARM_samples[, ncol(x@mcmc_result$PARM_samples)]) / 2)
    }
    if (x@model == "FUNL") {
      Q <- apply(x@mcmc_result$PARM_samples, 1, .cont_FUNL_f, d = test_doses, decrease = decrease)
    }
    if (x@model == "hill") {
      Q <- apply(x@mcmc_result$PARM_samples, 1, .cont_hill_f, d = test_doses, decrease = decrease)
    }
    if (x@model == "exp-3") {
      Q <- apply(x@mcmc_result$PARM_samples, 1, .cont_exp_3_f, d = test_doses, decrease = decrease)
      if (isLogNormal) {
        # var in last col
        varCol <- ncol(x@mcmc_result$PARM_samples)
        Q <- exp(log(Q) + exp(x@mcmc_result$PARM_samples[, varCol]) / 2)
      }
    }
    if (x@model == "exp-5") {
      Q <- apply(x@mcmc_result$PARM_samples, 1, .cont_exp_5_f, d = test_doses, decrease = decrease)
      if (isLogNormal) {
        varCol <- ncol(x@mcmc_result$PARM_samples)
        Q <- exp(log(Q) + exp(x@mcmc_result$PARM_samples[, varCol]) / 2)
      }
    }
    if (x@model == "power") {
      Q <- apply(x@mcmc_result$PARM_samples, 1, .cont_power_f, d = test_doses, decrease = decrease)
    }
    if (x@model == "polynomial") {
      # "degree" might be ncol(...) - 2 if normal-ncv, etc. 
      if (length(grep(": normal-ncv", tolower(x@full_model))) > 0) {
        degree <- ncol(x@mcmc_result$PARM_samples) - 2
      } else {
        degree <- ncol(x@mcmc_result$PARM_samples) - 1
      }
      Q <- apply(x@mcmc_result$PARM_samples[, 1:degree], 1, .cont_polynomial_f, d = test_doses, decrease = decrease)
    }

    if (IS_tranformed) {
      test_doses <- sinh(test_doses)
    }
    Q <- t(Q)
    me <- apply(Q, 2, quantile, probs = 0.5)
    lq <- apply(Q, 2, quantile, probs = qprob)
    uq <- apply(Q, 2, quantile, probs = 1 - qprob)

    # Build the ggplot
    library(ggplot2)
    plot_gg <- ggplot() +
      xlim(-max(test_doses) * 5, max(test_doses) * 5) +
      geom_line(aes(x = test_doses, y = me), color = "blue", linewidth = 2) +
      labs(
        x = "Dose", y = "Response",
        title = paste(x@full_model, "MCMC", sep = ",  Fit Type: ")
      ) +
      theme_minimal()

    # If the test_doses are not NaN / Inf, add BMD lines
    if (sum(!is.nan(test_doses) + !is.infinite(test_doses)) == 0) {
      plot_gg <- plot_gg +
        geom_segment(aes(x = x@bmd[2], y = me[1], # me[1] ~ me at bmd[1]?
                         xend = x@bmd[3], yend = me[1]),
                     color = "darkslategrey", linewidth = 1.2, alpha = 0.9) +
        annotate(
          geom = "text", x = x@bmd[2], y = me[1], label = "[", size = 10,
          color = "darkslategrey", alpha = 0.9
        ) +
        annotate(
          geom = "text", x = x@bmd[3], y = me[1], label = "]", size = 10,
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
        xx <- seq(min(test_doses), max(test_doses), (max(test_doses) - min(test_doses)) / 511)
        return(data.frame(x = xx, y = y))
      }
      Dens <- tryCatch(
        density(temp, cut = c(max(test_doses)), adjust = 1.5, n = 512,
                from = min(test_doses), to = max(test_doses)),
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
      data_in <- data.frame(doses = doses, mean_val = mean_val[, 1], uerror = uerror[, 1], lerror = lerror[, 1])
      plot_gg <- plot_gg +
        geom_errorbar(
          data = data_in, aes(x = doses, ymin = lerror, ymax = uerror),
          color = "black", linewidth = 0.8, width = width
        ) +
        geom_point(aes(x = doses, y = mean_val), size = 3, shape = 21, fill = "white", data = data_in)
    } else {
      data_in <- data.frame(doses = doses[, 1], Response = data_d[, 2])
      plot_gg <- plot_gg +
        geom_point(aes(x = doses, y = Response), data = data_in)
    }

    plot_gg <- plot_gg +
      geom_polygon(aes(
        x = c(test_doses, test_doses[length(test_doses):1]),
        y = c(uq, lq[length(test_doses):1])
      ),
      fill = "blue", alpha = 0.1
      )

    plot_gg <- plot_gg + coord_cartesian(xlim = c(min(test_doses), max(test_doses)), expand = FALSE)

    # return final ggplot
    return(plot_gg)
  }
)


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
    test_doses <- seq(min(doses), max(doses) * 1.03, (max(doses) - min(doses)) / 500)
    if (IS_transformed) {
      test_doses <- asinh(test_doses)
    }

    me <- switch(x@model,
      "exp-aerts" = .cont_exp_aerts_f(fit$parameters, test_doses),
      "invexp-aerts" = .cont_invexp_aerts_f(fit$parameters, test_doses),
      "gamma-aerts" = .cont_gamma_aerts_f(fit$parameters, test_doses),
      "invgamma-aerts" = .cont_invgamma_aerts_f(fit$parameters, test_doses),
      "hill-aerts" = .cont_hill_aerts_f(fit$parameters, test_doses),
      "lomax-aerts" = .cont_lomax_aerts_f(fit$parameters, test_doses),
      "invlomax-aerts" = .cont_invlomax_aerts_f(fit$parameters, test_doses),
      "lognormal-aerts" = .cont_lognormal_aerts_f(fit$parameters, test_doses),
      "logskew-aerts" = .cont_logskew_aerts_f(fit$parameters, test_doses),
      "invlogskew-aerts" = .cont_invlogskew_aerts_f(fit$parameters, test_doses),
      "logistic-aerts" = .cont_logistic_aerts_f(fit$parameters, test_doses),
      "probit-aerts" = .cont_probit_aerts_f(fit$parameters, test_doses),
      "LMS" = .cont_LMS_f(fit$parameters, test_doses),
      "gamma-efsa" = .cont_gamma_efsa_f(fit$parameters, test_doses)
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
      me <- .cont_polynomial_f(x@parameters[1:deg], test_doses)
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
    library(ggplot2)
    plot_gg <- ggplot() +
      geom_line(aes(x = test_doses, y = me), color = "blue", linewidth = 2) +
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
          geom_segment(aes(
            x = x@bmd[2], y = ma_mean(x@bmd[1]),
            xend = x@bmd[3], yend = ma_mean(x@bmd[1])
          ),
          color = "darkslategrey", linewidth = 1.2, alpha = 0.9) +
          annotate("text", x = x@bmd[2], y = ma_mean(x@bmd[1]),
                   label = "[", size = 10, color = "darkslategrey", alpha = 0.9) +
          annotate("text", x = x@bmd[3], y = ma_mean(x@bmd[1]),
                   label = "]", size = 10, color = "darkslategrey", alpha = 0.9) +
          annotate("point", x = x@bmd[1], y = ma_mean(x@bmd[1]),
                   size = 5, color = "darkslategrey", shape = 17, alpha = 0.9)
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
                      data = data_in, color = "grey", linewidth = 0.5, width = 3) +
        geom_point(aes(x = doses, y = mean_val),
                   data = data_in, size = 3, shape = 21, fill = "white")
    } else {
      data_in <- data.frame(doses = doses[, 1], Response = data_d[, 2])
      plot_gg <- plot_gg +
        geom_point(aes(x = doses, y = Response), data = data_in)
    }

    # Return final ggplot
    return(plot_gg + coord_cartesian(xlim = c(min(test_doses), max(test_doses)), expand = FALSE))
  }
)

# MCMC single-model
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
      Q <- apply(x@mcmc_result$PARM_samples, 1, .dich_hill_f, d = test_doses)
    } else if (x@model == "gamma") {
      Q <- apply(x@mcmc_result$PARM_samples, 1, .dich_gamma_f, d = test_doses)
    } else if (x@model == "logistic") {
      Q <- apply(x@mcmc_result$PARM_samples, 1, .dich_logist_f, d = test_doses)
    } else if (x@model == "log-logistic") {
      Q <- apply(x@mcmc_result$PARM_samples, 1, .dich_llogist_f, d = test_doses)
    } else if (x@model == "probit") {
      Q <- apply(x@mcmc_result$PARM_samples, 1, .dich_probit_f, d = test_doses)
    } else if (x@model == "log-probit") {
      Q <- apply(x@mcmc_result$PARM_samples, 1, .dich_lprobit_f, d = test_doses)
    } else if (x@model == "multistage") {
      Q <- apply(x@mcmc_result$PARM_samples, 1, .dich_multistage_f, d = test_doses)
    } else if (x@model == "qlinear") {
      Q <- apply(x@mcmc_result$PARM_samples, 1, .dich_qlinear_f, d = test_doses)
    } else if (x@model == "weibull") {
      Q <- apply(x@mcmc_result$PARM_samples, 1, .dich_weibull_f, d = test_doses)
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
    library(ggplot2)
    plot_gg <- ggplot() +
      geom_errorbar(aes(x = doses, ymin = lerror, ymax = uerror), color = "grey") +
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
      geom_ribbon(aes(x = dose, ymin = lq, ymax = uq), fill = "blue", alpha = 0.1, data = df_plot) +
      geom_line(aes(x = dose, y = me), col = "blue", linewidth = 2, data = df_plot) +
      geom_point(aes(x = doses, y = probs), color = "black")

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
      Dens <- density(temp, cut = c(5 * max(test_doses)), n = 1000, from = 0, to = max(test_doses), na.rm = TRUE)
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

    return(plot_gg + coord_cartesian(xlim = c(min(doses), max(doses)), expand = FALSE))
  }
)

# Maximized single-model
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

    library(ggplot2)
    plot_gg <- ggplot() +
      geom_errorbar(aes(x = doses, ymin = lerror, ymax = uerror), color = "grey") +
      xlim(c(min(doses) - 5 * max(doses), max(doses) * 5)) +
      labs(x = "Dose", y = "Proportion", title = paste(x@full_model, sep = ",  Fit Type: ")) +
      theme_minimal()

    # Add line
    df_plot <- data.frame(dose = test_doses, me = me)
    plot_gg <- plot_gg +
      geom_line(aes(x = dose, y = me), col = "blue", linewidth = 1.2, data = df_plot) +
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

    return(plot_gg + coord_cartesian(xlim = c(min(doses), max(doses)), expand = FALSE))
  }
)

# 4C) Model-Averaged approach: replicate .plot.BMDdichotomous_MA
# We store submodels in x@submodels, each either MCMC or maximized
# We store x@fit_type == "mcmc" or "laplace" (like your old class checks).
setMethod(
  "plot",
  signature = signature(x = "BMD_dichotomous_fit_MA", y = "missing"),
  function(x, y, ...) {
    temp_args <- list(...)
    if (!"qprob" %in% names(temp_args)) {
      qprob <- 0.05
    } else {
      qprob <- temp_args$qprob
    }

    density_col <- "blueviolet"
    credint_col <- "azure2"
    submods <- x@submodels
    nSub <- length(submods)
    if (nSub < 1) {
      stop("No submodels found in this MA object.")
    }
    A <- x

    # assume all submodels share same data shape
    # take data from first submodel
    data_d <- submods[[1]]@data
    probs <- (0.5 + data_d[, 2]) / (1.0 + data_d[, 3])
    se <- sqrt(probs * (1 - probs) / data_d[, 3])
    doses <- data_d[, 1]
    uerror <- apply(cbind(probs * 0 + 1, probs + se), 1, min)
    lerror <- apply(cbind(probs * 0, probs - se), 1, max)

    library(ggplot2)
    plot_gg <- ggplot() +
      geom_errorbar(aes(x = doses, ymin = lerror, ymax = uerror), color = "grey") +
      xlim(c(-5 * max(doses)), 5 * max(doses)) +
      labs(x = "Dose", y = "Proportion", title = paste(x@full_model, "MA")) +
      theme_minimal()

    max_dose <- max(doses)
    min_dose <- min(doses)
    test_doses <- seq(min_dose, max_dose, (max_dose - min_dose) / 500)

    if (x@fit_type == "mcmc") {
      # -------------- "BMDdichotomous_MA_mcmc" logic --------------
      # sample submodels with prob A@posterior_probs,
      n_samps <- nrow(submods[[1]]@mcmc_result$PARM_samples)
      ma_samps <- sample(seq_along(submods), n_samps, replace = TRUE, prob = x@posterior_probs)
      temp_f <- matrix(0, n_samps, length(test_doses))
      temp_bmd <- rep(0, n_samps)

      for (ii in seq_len(n_samps)) {
        fit_i <- submods[[ma_samps[ii]]] # pick submodel
        if (fit_i@model == "hill") {
          temp_f[ii,] <- .dich_hill_f(fit_i@mcmc_result$PARM_samples[ii,], test_doses)
          temp_bmd[ii] <- fit_i@mcmc_result$BMD_samples[ii]
        } else if (fit_i@model == "gamma") {
          temp_f[ii,] <- .dich_gamma_f(fit_i@mcmc_result$PARM_samples[ii,], test_doses)
          temp_bmd[ii] <- fit_i@mcmc_result$BMD_samples[ii]
        } else if (fit_i@model == "logistic") {
          temp_f[ii,] <- .dich_logist_f(fit_i@mcmc_result$PARM_samples[ii,], test_doses)
          temp_bmd[ii] <- fit_i@mcmc_result$BMD_samples[ii]
        } else if (fit_i@model == "log-logistic") {
          temp_f[ii,] <- .dich_llogist_f(fit_i@mcmc_result$PARM_samples[ii,], test_doses)
          temp_bmd[ii] <- fit_i@mcmc_result$BMD_samples[ii]
        } else if (fit_i@model == "probit") {
          temp_f[ii,] <- .dich_probit_f(fit_i@mcmc_result$PARM_samples[ii,], test_doses)
          temp_bmd[ii] <- fit_i@mcmc_result$BMD_samples[ii]
        } else if (fit_i@model == "log-probit") {
          temp_f[ii,] <- .dich_lprobit_f(fit_i@mcmc_result$PARM_samples[ii,], test_doses)
          temp_bmd[ii] <- fit_i@mcmc_result$BMD_samples[ii]
        } else if (fit_i@model == "multistage") {
          temp_f[ii,] <- .dich_multistage_f(fit_i@mcmc_result$PARM_samples[ii,], test_doses)
          temp_bmd[ii] <- fit_i@mcmc_result$BMD_samples[ii]
        } else if (fit_i@model == "qlinear") {
          temp_f[ii,] <- .dich_qlinear_f(fit_i@mcmc_result$PARM_samples[ii,], test_doses)
          temp_bmd[ii] <- fit_i@mcmc_result$BMD_samples[ii]
        } else if (fit_i@model == "weibull") {
          temp_f[ii,] <- .dich_weibull_f(fit_i@mcmc_result$PARM_samples[ii,], test_doses)
          temp_bmd[ii] <- fit_i@mcmc_result$BMD_samples[ii]
        }
      }

      me <- colMeans(temp_f)
      lq <- apply(temp_f, 2, quantile, probs = qprob, na.rm = TRUE)
      uq <- apply(temp_f, 2, quantile, probs = 1 - qprob, na.rm = TRUE)

      df_ma <- data.frame(dose = test_doses, me = me, lq = lq, uq = uq)
      plot_gg <- plot_gg +
        geom_ribbon(aes(x = dose, ymin = lq, ymax = uq), fill = "blue", alpha = 0.1, data = df_ma) +
        geom_line(aes(x = dose, y = me), col = "blue", linewidth = 2, data = df_ma) +
        geom_point(aes(x = doses, y = probs))

      temp_fit <- splinefun(test_doses, me)
      # add BMD lines
      bmdv <- x@bmd
      plot_gg <- plot_gg +
        geom_segment(aes(
          x = bmdv[2], y = temp_fit(bmdv[1]),
          xend = bmdv[3], yend = temp_fit(bmdv[1])
        ), color = "darkslategrey", linewidth = 1.2, alpha = 0.9) +
        annotate(
          geom = "text", x = bmdv[2], y = temp_fit(bmdv[1]),
          label = "[", size = 10, color = "darkslategrey", alpha = 0.9
        ) +
        annotate(
          geom = "text", x = bmdv[3], y = temp_fit(bmdv[1]),
          label = "]", size = 10, color = "darkslategrey", alpha = 0.9
        ) +
        annotate(
          geom = "point", x = bmdv[1], y = temp_fit(bmdv[1]),
          size = 5, color = "darkslategrey", shape = 17, alpha = 0.9
        )

      # density for temp_bmd
      temp_bmd <- temp_bmd[!is.nan(temp_bmd)]
      temp_bmd <- temp_bmd[!is.infinite(temp_bmd)]
      temp_bmd <- temp_bmd[temp_bmd < 5 * max(doses)]
      Dens <- density(temp_bmd, cut = c(5 * max(test_doses)), n = 1000, from = 0, to = max(test_doses))
      max_u <- max(uerror)
      min_l <- min(lerror)
      Dens$y <- Dens$y / max(Dens$y) * (max_u - min_l) * 0.6
      idx2 <- which(Dens$x < max(test_doses))
      D1_y <- Dens$y[idx2]
      D1_x <- Dens$x[idx2]
      scale <- (max_u - min_l) / max(D1_y) * 0.40

      plot_gg <- plot_gg +
        geom_polygon(aes(
          x = c(max(0, min(D1_x)), D1_x, max(D1_x)),
          y = c(min_l, min_l + D1_y * scale, min_l)
        ),
        fill = "blueviolet", alpha = 0.6
        )

      # Add lines for each submodel if posterior_probs > 0.05
      df <- NULL
      for (ii in seq_along(submods)) {
        if (!is.finite(x@posterior_probs[ii])) {
          x@posterior_probs[ii] = 0
        }
        if (x@posterior_probs[ii] > 0.05) {
          fit_i <- submods[[ii]]
          # Single line
          if (fit_i@model == "hill") {
            f <- .dich_hill_f(fit_i@parameters, test_doses)
          } else if (fit_i@model == "gamma") {
            f <- .dich_gamma_f(fit_i@parameters, test_doses)
          } else if (fit_i@model == "logistic") {
            f <- .dich_logist_f(fit_i@parameters, test_doses)
          } else if (fit_i@model == "log-logistic") {
            f <- .dich_llogist_f(fit_i@parameters, test_doses)
          } else if (fit_i@model == "probit") {
            f <- .dich_probit_f(fit_i@parameters, test_doses)
          } else if (fit_i@model == "log-probit") {
            f <- .dich_lprobit_f(fit_i@parameters, test_doses)
          } else if (fit_i@model == "multistage") {
            f <- .dich_multistage_f(fit_i@parameters, test_doses)
          } else if (fit_i@model == "qlinear") {
            f <- .dich_qlinear_f(fit_i@parameters, test_doses)
          } else if (fit_i@model == "weibull") {
            f <- .dich_weibull_f(fit_i@parameters, test_doses)
          } else {
            next
          }
          alpha_val <- x@posterior_probs[ii]
          tmp_df <- data.frame(x_axis = test_doses, y_axis = f, alpha_lev = alpha_val, model_no = ii)
          plot_gg <- plot_gg +
            geom_line(data = tmp_df, aes(x = x_axis, y = y_axis), color = "coral3", alpha = alpha_val) +
            theme_minimal()
        }
      }

      return(plot_gg + coord_cartesian(xlim = c(min(doses), max(doses)), expand = FALSE))

    } else if (x@fit_type == "laplace") {
      # -------------- "BMDdichotomous_MA_laplace" logic --------------
      x@posterior_probs[!is.finite(x@posterior_probs)] <- 0
      num_model <- length(x@posterior_probs)
      test_doses <- seq(min_dose, max_dose, length.out = 500)
      temp_f <- matrix(0, num_model, length(test_doses))

      for (ii in seq_len(num_model)) {
        fit_loop <- submods[[ii]]
        w <- x@posterior_probs[ii]

        # Use "parameters" to get me
        if (fit_loop@model == "hill") {
          temp_f[ii,] <- .dich_hill_f(fit_loop@parameters, test_doses) * w
        } else if (fit_loop@model == "gamma") {
          temp_f[ii,] <- .dich_gamma_f(fit_loop@parameters, test_doses) * w
        } else if (fit_loop@model == "logistic") {
          temp_f[ii,] <- .dich_logist_f(fit_loop@parameters, test_doses) * w
        } else if (fit_loop@model == "log-logistic") {
          temp_f[ii,] <- .dich_llogist_f(fit_loop@parameters, test_doses) * w
        } else if (fit_loop@model == "probit") {
          temp_f[ii,] <- .dich_probit_f(fit_loop@parameters, test_doses) * w
        } else if (fit_loop@model == "log-probit") {
          temp_f[ii,] <- .dich_lprobit_f(fit_loop@parameters, test_doses) * w
        } else if (fit_loop@model == "multistage") {
          temp_f[ii,] <- .dich_multistage_f(fit_loop@parameters, test_doses) * w
        } else if (fit_loop@model == "qlinear") {
          temp_f[ii,] <- .dich_qlinear_f(fit_loop@parameters, test_doses) * w
        } else if (fit_loop@model == "weibull") {
          temp_f[ii,] <- .dich_weibull_f(fit_loop@parameters, test_doses) * w
        }
      }

      me <- colSums(temp_f)
      temp_fit <- splinefun(test_doses, me)

      # build plot
      plot_gg <- plot_gg +
        ylim(c(min(lerror, me) * 0.95, max(uerror, me) * 1.05)) +
        geom_line(aes(x = test_doses, y = me), col = "blue", linewidth = 1.2) +
        geom_point(aes(x = doses, y = probs))

      # bmd lines
      bmdv <- x@bmd
      plot_gg <- plot_gg +
        geom_segment(aes(
          x = bmdv[2], y = temp_fit(bmdv[1]),
          xend = bmdv[3], yend = temp_fit(bmdv[1])
        ), color = "darkslategrey", linewidth = 1.2, alpha = 0.9) +
        annotate(
          geom = "text", x = bmdv[2], y = temp_fit(bmdv[1]),
          label = "[", size = 10, color = "darkslategrey", alpha = 0.9
        ) +
        annotate(
          geom = "text", x = bmdv[3], y = temp_fit(bmdv[1]),
          label = "]", size = 10, color = "darkslategrey", alpha = 0.9
        ) +
        annotate(
          geom = "point", x = bmdv[1], y = temp_fit(bmdv[1]),
          size = 5, color = "darkslategrey", shape = 17, alpha = 0.9
        )

      # overlay submodels with posterior prob > 0.05
      df <- NULL
      for (ii in seq_len(num_model)) {
        if (!is.finite(x@posterior_probs[ii])) {
          x@posterior_probs[ii] = 0
        }
        if (x@posterior_probs[ii] > 0.05) {
          fit_i <- submods[[ii]]
          # single line
          if (fit_i@model == "hill") {
            f <- .dich_hill_f(fit_i@parameters, test_doses)
          } else if (fit_i@model == "gamma") {
            f <- .dich_gamma_f(fit_i@parameters, test_doses)
          } else if (fit_i@model == "logistic") {
            f <- .dich_logist_f(fit_i@parameters, test_doses)
          } else if (fit_i@model == "log-logistic") {
            f <- .dich_llogist_f(fit_i@parameters, test_doses)
          } else if (fit_i@model == "probit") {
            f <- .dich_probit_f(fit_i@parameters, test_doses)
          } else if (fit_i@model == "log-probit") {
            f <- .dich_lprobit_f(fit_i@parameters, test_doses)
          } else if (fit_i@model == "multistage") {
            f <- .dich_multistage_f(fit_i@parameters, test_doses)
          } else if (fit_i@model == "qlinear") {
            f <- .dich_qlinear_f(fit_i@parameters, test_doses)
          } else if (fit_i@model == "weibull") {
            f <- .dich_weibull_f(fit_i@parameters, test_doses)
          } else {
            next
          }
          alpha_val <- x@posterior_probs[ii]
          tmp_df <- data.frame(x_axis = test_doses, y_axis = f, alpha_lev = alpha_val, model_no = ii)
          plot_gg <- plot_gg +
            geom_line(data = tmp_df, aes(x = x_axis, y = y_axis), color = "coral3", alpha = alpha_val) +
            theme_minimal()
        }
      }

      return(plot_gg + coord_cartesian(xlim = c(min(doses), max(doses)), expand = FALSE))
    }

    # if fit_type is neither "mcmc" nor "laplace"
    stop("BMD_dichotomous_fit_MA with unknown fit_type:", x@fit_type)
  }
)