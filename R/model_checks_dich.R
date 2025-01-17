.check_d_gamma <- function(prior) {
  temp <- prior[[1]]

  if (nrow(temp) != 3) {
    stop("Dichotomous Gamma model prior requires 3 parameters.")
  }

  if (temp[2, 4] < 0) {
    stop("The prior on b (parameter 2) can not have a lower bound less than zero.")
  }

  if (temp[3, 4] < 0) {
    stop("The prior on d (parameter 3) can not have a lower bound less than zero.")
  }

  prior$model <- "Gamma Model [binomial]"
  prior$mean <- "gamma"
  prior$parameters <- c("logit(g)", "a", "b")
  return(prior)
}

.check_d_weibull <- function(prior) {
  temp <- prior[[1]]

  if (nrow(temp) != 3) {
    stop("Dichotomous Weibull model prior requires 3 parameters.")
  }

  if (temp[2, 4] < 0) {
    stop("The prior on b (parameter 2) can not have a lower bound less than zero.")
  }

  if (temp[3, 4] < 0) {
    stop("The prior on d (parameter 3) can not have a lower bound less than zero.")
  }

  prior$model <- "Weibull Model [binomial]"
  prior$mean <- "weibull"
  prior$parameters <- c("logit(g)", "a", "b")
  return(prior)
}

.check_d_lprobit <- function(prior) {
  temp <- prior[[1]]

  if (nrow(temp) != 3) {
    stop("Dichotomous Log-Probit model prior requires 3 parameters.")
  }

  if (temp[3, 4] < 0) {
    stop("The prior on b1 (parameter 3) can not have a lower bound less than zero.")
  }

  prior$model <- "Log-Probit Model [binomial]"
  prior$mean <- "log-probit"
  prior$parameters <- c("logit(g)", "b0", "b1")
  return(prior)
}

.check_d_llogistic <- function(prior) {
  temp <- prior[[1]]

  if (nrow(temp) != 3) {
    stop("Dichotomous Log-Logistic model prior requires 3 parameters.")
  }

  if (temp[3, 4] < 0) {
    stop("The prior on b1 (parameter 3) can not have a lower bound less than zero.")
  }

  prior$model <- "Log-Logistic Model [binomial]"
  prior$mean <- "log-logistic"
  prior$parameters <- c("logit(g)", "b0", "b1")
  return(prior)
}

.check_d_logistic <- function(prior) {
  temp <- prior[[1]]

  if (nrow(temp) != 2) {
    stop("Dichotomous logistic model prior requires 2 parameters.")
  }

  if (temp[2, 4] < 0) {
    stop("The prior on b (parameter 2) can not have a lower bound less than zero.")
  }

  prior$model <- "Logistic Model [binomial]"
  prior$mean <- "logistic"
  prior$parameters <- c("a", "b")
  return(prior)
}

.check_d_probit <- function(prior) {
  temp <- prior[[1]]

  if (nrow(temp) != 2) {
    stop("Dichotomous probit model prior requires 2 parameters.")
  }

  if (temp[2, 4] < 0) {
    stop("The prior on b (parameter 2) can not have a lower bound less than zero.")
  }

  prior$model <- "Probit Model [binomial]"
  prior$mean <- "probit"
  prior$parameters <- c("a", "b")
  return(prior)
}

.check_d_qlinear <- function(prior) {
  temp <- prior[[1]]

  if (nrow(temp) != 2) {
    stop("Dichotomous Quantal model prior requires 2 parameters.")
  }

  if (temp[2, 4] < 0) {
    stop("The prior on b (parameter 2) can not have a lower bound less than zero.")
  }

  prior$model <- "Quantal Linear Model [binomial]"
  prior$mean <- "qlinear"
  prior$parameters <- c("logit(g)", "b")
  return(prior)
}

.check_d_multistage <- function(prior) {
  temp <- prior[[1]]

  if (nrow(temp) < 2) {
    stop("Multistage model prior requires 2 or more parameters.")
  }

  names <- c("logit(g)")
  for (ii in 2:nrow(temp)) {
    names <- c(names, sprintf("b%d", ii - 1))
  }

  if (sum(temp[2:nrow(temp), 4] < 0) > 0) {
    stop("The prior on bx can not have a lower bound less than zero.")
  }

  # Build a list
  rV <- list()
  rV$priors <- temp
  rV$model <- sprintf("Multistage-%d Model [binomial]", nrow(temp) - 1)
  rV$mean <- "multistage"
  rV$degree <- nrow(temp) - 1
  rV$parameters <- names
  return(rV)
}

.check_d_hill <- function(prior) {
  temp <- prior[[1]]

  if (nrow(temp) != 4) {
    stop("Dichotomous Hill  model prior requires 3 parameters.")
  }

  if (temp[4, 4] < 0) {
    stop("The prior on d (parameter 4) can not have a lower bound less than zero.")
  }

  prior$model <- "Hill Model [binomial]"
  prior$mean <- "hill"
  prior$parameters <- c("logit(g)", "b", "c", "d")
  return(prior)
}