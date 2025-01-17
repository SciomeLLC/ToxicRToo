.check_hill <- function(prior, distribution) {
  temp <- prior[[1]]
  if (distribution == "normal") {
    if (nrow(temp) != 5) {
      stop("Normal Hill model prior requires 5 parameters.")
    }
    if (sum(temp[, 4] > temp[, 5]) > 0) {
      stop("One of the parameter's lower bounds is greater than the upper bound.")
    }
    prior$model <- "Hill Model [normal]"
    prior$parameters <- c("a", "b", "c", "d", "log(sigma^2)")
  }
  if (distribution == "normal-ncv") {
    if (nrow(temp) != 6) {
      stop("Normal-NCV Hill model prior requires 6 parameters.")
    }
    if (temp[5, 4] < 0) {
      stop("The prior on rho (parameter 5) can not have a lower bound < 0.")
    }
    prior$model <- "Hill Model [normal-ncv]"
    prior$parameters <- c("a", "b", "c", "d", "rho", "log(sigma^2)")
  }
  if (distribution == "lognormal") {
    stop("Log-Normal/Hill specification is not presently available.")
  }
  prior$mean <- "hill"
  return(prior)
}

.check_4param_gamma <- function(prior, distribution) {
  temp <- prior[[1]]
  if (sum(temp[, 4] > temp[, 5]) > 0) {
    stop("One of the parameter's lower bounds is greater than the upper bound.")
  }
  if (temp[2, 4] <= 0) {
    stop("The lower bound on b must be positive (for numerical stability).")
  }
  if (temp[4, 4] < 0.2) {
    stop("The lower bound on d must be >= 0.2 for numerical reasons.")
  }

  if (distribution == "normal") {
    if (nrow(temp) != 5) {
      stop("Normal Aerts model prior requires 5 parameters.")
    }
    prior$model <- "Aerts Model [normal]"
    prior$parameters <- c("a", "b", "c", "d", "log(sigma^2)")
  } else if (distribution == "normal-ncv") {
    if (nrow(temp) != 6) {
      stop("Normal-NCV Aerts model prior requires 6 parameters.")
    }
    if (temp[5, 4] < 0) {
      stop("The prior on rho (parameter 5) cannot have lower bound < 0.")
    }
    prior$model <- "Aerts Model [normal-ncv]"
    prior$parameters <- c("a", "b", "c", "d", "rho", "log(sigma^2)")
  } else if (distribution == "lognormal") {
    if (nrow(temp) != 5) {
      stop("Lognormal Aerts model prior requires 5 parameters.")
    }
    prior$model <- "Aerts Model [lognormal]"
    prior$parameters <- c("a", "b", "c", "d", "log(sigma^2)")
  }
  return(prior)
}

.check_4param <- function(prior, distribution) {
  temp <- prior[[1]]
  if (sum(temp[, 4] > temp[, 5]) > 0) {
    stop("One of the parameter's lower bounds is greater than the upper bound.")
  }
  if (temp[2, 4] <= 0) {
    stop("The lower bound on b must be positive (for numerical stability).")
  }
  if (temp[4, 4] < 0) {
    stop("The lower bound on d must be non-negative.")
  }
  if (distribution == "normal") {
    if (nrow(temp) != 5) {
      stop("Normal Aerts model prior requires 5 parameters.")
    }
    prior$model <- "Aerts Model [normal]"
    prior$parameters <- c("a", "b", "c", "d", "log(sigma^2)")
  } else if (distribution == "normal-ncv") {
    if (nrow(temp) != 6) {
      stop("Normal-NCV Aerts model prior requires 6 parameters.")
    }
    if (temp[5, 4] < 0) {
      stop("The prior on rho (parameter 5) can not have a lower bound < 0.")
    }
    prior$model <- "Aerts Model [normal-ncv]"
    prior$parameters <- c("a", "b", "c", "d", "rho", "log(sigma^2)")
  } else if (distribution == "lognormal") {
    if (nrow(temp) != 5) {
      stop("Lognormal Aerts model prior requires 5 parameters.")
    }
    prior$model <- "Aerts Model [lognormal]"
    prior$parameters <- c("a", "b", "c", "d", "log(sigma^2)")
  }
  return(prior)
}

.check_4param_sigmoidal <- function(prior, distribution) {
  temp <- prior[[1]]
  if (sum(temp[, 4] > temp[, 5]) > 0) {
    stop("One of the parameter's lower bounds is greater than the upper bound.")
  }
  if (temp[4, 4] < 0) {
    stop("The lower bound on d (exponent) must be non-negative.")
  }
  if (distribution == "normal") {
    if (nrow(temp) != 5) {
      stop("Normal Aerts model prior requires 5 parameters.")
    }
    prior$model <- "Aerts Model [normal]"
    prior$parameters <- c("a", "b", "c", "d", "log(sigma^2)")
  } else if (distribution == "normal-ncv") {
    if (nrow(temp) != 6) {
      stop("Normal-NCV Aerts model prior requires 6 parameters.")
    }
    if (temp[5, 4] < 0) {
      stop("The prior on rho (parameter 5) cannot have lower bound < 0.")
    }
    prior$model <- "Aerts Model [normal-ncv]"
    prior$parameters <- c("a", "b", "c", "d", "rho", "log(sigma^2)")
  } else if (distribution == "lognormal") {
    if (nrow(temp) != 5) {
      stop("Lognormal Aerts model prior requires 5 parameters.")
    }
    prior$model <- "Aerts Model [lognormal]"
    prior$parameters <- c("a", "b", "c", "d", "log(sigma^2)")
  }
  return(prior)
}

.check_5param <- function(prior, distribution) {
  temp <- prior[[1]]
  if (sum(temp[, 4] > temp[, 5]) > 0) {
    stop("One of the parameter's lower bounds is greater than the upper bound.")
  }
  if (temp[2, 4] <= 0 | temp[5, 4] <= 0) {
    stop("The lower bounds on b and xi must be positive (for numerical stability).")
  }
  if (temp[4, 4] < 0) {
    stop("The lower bound on d must be non-negative.")
  }
  if (temp[2, 4] < 0.1) {
    warning("Lower bound on b < 0.1 may cause numerical instability.")
  }
  if (distribution == "normal") {
    if (nrow(temp) != 6) {
      stop("Normal Aerts model prior requires 6 parameters.")
    }
    prior$model <- "Aerts Model [normal]"
    prior$parameters <- c("a", "b", "c", "d", "xi", "log(sigma^2)")
  } else if (distribution == "normal-ncv") {
    if (nrow(temp) != 7) {
      stop("Normal-NCV Aerts model prior requires 7 parameters.")
    }
    if (temp[6, 4] < 0) {
      stop("The prior on rho (parameter 6) cannot have lower bound < 0.")
    }
    prior$model <- "Aerts Model [normal-ncv]"
    prior$parameters <- c("a", "b", "c", "d", "xi", "rho", "log(sigma^2)")
  } else if (distribution == "lognormal") {
    if (nrow(temp) != 6) {
      stop("Lognormal Aerts model prior requires 6 parameters.")
    }
    prior$model <- "Aerts Model [lognormal]"
    prior$parameters <- c("a", "b", "c", "d", "xi", "log(sigma^2)")
  }
  prior$mean <- .continuous_models[1]
  return(prior)
}

.check_5param_gamma <- function(prior, distribution) {
  temp <- prior[[1]]
  if (sum(temp[, 4] > temp[, 5]) > 0) {
    stop("One of the parameter's lower bounds is greater than the upper bound.")
  }
  if (temp[2, 4] <= 0 | temp[5, 4] <= 0) {
    stop("The lower bounds on b and xi must be positive.")
  }
  if (temp[4, 4] < 0) {
    stop("The lower bound on d must be non-negative.")
  }
  if (temp[5, 4] < 0.2) {
    stop("The lower bound on xi must be >= 0.2.")
  }
  if (temp[2, 4] < 0.1) {
    warning("Lower bound on b < 0.1 may cause instability.")
  }
  if (distribution == "normal") {
    if (nrow(temp) != 6) {
      stop("Normal Aerts model prior requires 6 parameters.")
    }
    prior$model <- "Aerts Model [normal]"
    prior$parameters <- c("a", "b", "c", "d", "xi", "log(sigma^2)")
  } else if (distribution == "normal-ncv") {
    if (nrow(temp) != 7) {
      stop("Normal-NCV Aerts model prior requires 7 parameters.")
    }
    if (temp[6, 4] < 0) {
      stop("The prior on rho cannot have a lower bound < 0.")
    }
    prior$model <- "Aerts Model [normal-ncv]"
    prior$parameters <- c("a", "b", "c", "d", "xi", "rho", "log(sigma^2)")
  } else if (distribution == "lognormal") {
    if (nrow(temp) != 6) {
      stop("Lognormal Aerts model prior requires 6 parameters.")
    }
    prior$model <- "Aerts Model [lognormal]"
    prior$parameters <- c("a", "b", "c", "d", "xi", "log(sigma^2)")
  }
  prior$mean <- .continuous_models[1]
  return(prior)
}

.check_exp5 <- function(prior, distribution) {
  temp <- prior[[1]]
  if (distribution == "normal") {
    if (nrow(temp) != 5) {
      stop("Normal Exponential-5 model prior requires 5 parameters.")
    }
    if (sum(temp[, 4] > temp[, 5]) > 0) {
      stop("One of the parameter's lower bounds is greater than the upper bound.")
    }
    prior$model <- "Exponential-5 Model [normal]"
    prior$parameters <- c("a", "b", "c", "d", "log(sigma^2)")
  } else if (distribution == "normal-ncv") {
    if (nrow(temp) != 6) {
      stop("Normal Exponential-5 model prior requires 6 parameters.")
    }
    if (sum(temp[, 4] > temp[, 5]) > 0) {
      stop("One param lower bound > upper bound.")
    }
    if (temp[5, 4] < 0) {
      stop("rho (param 5) cannot have lower bound < 0.")
    }
    prior$model <- "Exponential-5 [normal-ncv]"
    prior$parameters <- c("a", "b", "c", "d", "rho", "log(sigma^2)")
  } else if (distribution == "lognormal") {
    if (nrow(temp) != 5) {
      stop("Lognormal Exponential-5 model prior requires 5 parameters.")
    }
    if (sum(temp[, 4] > temp[, 5]) > 0) {
      stop("One param lower bound > upper bound.")
    }
    prior$model <- "Exponential-5 Model [lognormal]"
    prior$parameters <- c("a", "b", "c", "d", "log(sigma^2)")
  }
  prior$mean <- .continuous_models[3]
  return(prior)
}

.check_power <- function(prior, distribution) {
  if (distribution == "normal") {
    temp <- prior[[1]]
    if (nrow(temp) != 4) {
      stop("Normal Power model prior requires 4 parameters.")
    }
    if (sum(temp[, 4] > temp[, 5]) > 0) {
      stop("One param lower bound > upper bound.")
    }
    if (temp[3, 4] < 0) {
      stop("The power parameter d cannot have lower bound < 0.")
    }
    prior$model <- "Power Model [normal]"
    prior$parameters <- c("a", "b", "d", "log(sigma^2)")
  } else if (distribution == "normal-ncv") {
    temp <- prior[[1]]
    if (nrow(temp) != 5) {
      stop("Normal-NCV Power model requires 5 parameters.")
    }
    if (sum(temp[, 4] > temp[, 5]) > 0) {
      stop("One param lower bound > upper bound.")
    }
    if (temp[3, 4] < 0) {
      stop("d cannot have lower bound < 0.")
    }
    if (temp[4, 4] < 0) {
      stop("rho cannot have lower bound < 0.")
    }
    prior$model <- "Power Model [normal-ncv]"
    prior$parameters <- c("a", "b", "d", "rho", "log(sigma^2)")
  } else if (distribution == "lognormal") {
    stop("Log-Normal/Power specification not available.")
  }
  prior$mean <- .continuous_models[4]
  return(prior)
}

.check_FUNL <- function(prior, distribution) {
  if (distribution == "normal") {
    temp <- prior[[1]]
    if (nrow(temp) != 7) {
      stop("Normal FUNL model prior requires 7 parameters.")
    }
    if (sum(temp[, 4] > temp[, 5]) > 0) {
      stop("One param lower bound > upper bound.")
    }
    prior$model <- "FUNL Model [normal]"
    prior$parameters <- c("a", "b", "lm", "ls", "nm", "ns", "log(sigma^2)")
  } else if (distribution == "normal-ncv") {
    temp <- prior[[1]]
    if (nrow(temp) != 8) {
      stop("Normal-NCV FUNL model requires 8 parameters.")
    }
    prior$model <- "FUNL Model [normal-ncv]"
    prior$parameters <- c("a", "b", "lm", "ls", "nm", "ns", "rho", "log(sigma^2)")
  } else if (distribution == "lognormal") {
    stop("Log-Normal/FUNL specification not available.")
  }
  prior$mean <- .continuous_models[5]
  return(prior)
}

.check_exp3 <- function(prior, distribution) {
  temp <- prior[[1]]
  if (distribution == "normal") {
    if (nrow(temp) != 4) {
      stop("Normal Exponential-3 model requires 4 parameters.")
    }
    if (sum(temp[, 4] > temp[, 5]) > 0) {
      stop("One param lower bound > upper bound.")
    }
    prior$model <- "Exponential-3 Model [normal]"
    prior$parameters <- c("a", "b", "c", "d", "log(sigma^2)")
  } else if (distribution == "normal-ncv") {
    if (nrow(temp) != 5) {
      stop("Normal Exponential-3 model requires 5 parameters.")
    }
    if (sum(temp[, 4] > temp[, 5]) > 0) {
      stop("One param lower bound > upper bound.")
    }
    if (temp[4, 4] < 0) {
      stop("rho cannot have lower bound < 0.")
    }
    prior$model <- "Exponential-3 [normal-ncv]"
    prior$parameters <- c("a", "b", "c", "d", "rho", "log(sigma^2)")
  } else if (distribution == "lognormal") {
    if (nrow(temp) != 4) {
      stop("Lognormal Exponential-3 model requires 4 parameters.")
    }
    if (sum(temp[, 4] > temp[, 5]) > 0) {
      stop("One param lower bound > upper bound.")
    }
    prior$model <- "Exponential-3 Model [lognormal]"
    prior$parameters <- c("a", "b", "c", "d", "log(sigma^2)")
  }
  prior$mean <- .continuous_models[2]
  # Insert extra parameter if needed (like your code does)
  temp2 <- prior$prior
  prior$prior <- matrix(NA, nrow = nrow(temp2) + 1, ncol = 5)
  prior$prior[1:2,] <- temp2[1:2,]
  prior$prior[3,] <- c(1, 0, 1, -100, 100) # an example row
  prior$prior[4:nrow(prior$prior),] <- temp2[3:nrow(temp2),]
  cat("NOTE: Parameter 'c' added to prior list. It is not used in the analysis.\n")
  return(prior)
}

.check_polynomial <- function(prior, distribution) {
  temp <- prior[[1]]
  if (sum(temp[, 4] > temp[, 5]) > 0) {
    stop("One param lower bound > upper bound.")
  }
  temp_p <- c("b0")
  for (ii in 2:(nrow(temp) - 1)) {
    temp_p <- c(temp_p, sprintf("b%s", ii - 1))
  }
  if (distribution == "normal") {
    if (nrow(temp) < 3) {
      stop("Normal Polynomial models require 3 or more parameters.")
    }
    prior$model <- "Polynomial Model [normal]"
    prior$parameters <- c(temp_p, "log(sigma^2)")
    prior$degree <- nrow(temp) - 2
  } else if (distribution == "normal-ncv") {
    if (nrow(temp) < 4) {
      stop("Normal-ncv polynomial models require 4 or more parameters.")
    }
    if (temp[nrow(temp) - 1, 4] < 0) {
      stop("rho cannot have lower bound < 0.")
    }
    prior$model <- "Polynomial Model [normal-ncv]"
    temp_p[length(temp_p)] <- "rho"
    prior$parameters <- c(temp_p, "log(sigma^2)")
    prior$degree <- nrow(temp) - 3
  } else if (distribution == "lognormal") {
    stop("Log-Normal/Polynomial specification not available.")
  }
  prior$mean <- .continuous_models[6]
  return(prior)
}

