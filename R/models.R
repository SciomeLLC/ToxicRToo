setClass("priorClass",
slots = c(
  prior = "ANY",
  model = "character",
  mean = "character",
  parameters = "character"
))

## MA model class
setClass(
  "BMD_continuous_MA",
  slots = c(
    models = "list",
    bmd = "numeric",
    ma_bmd = "matrix",
    posterior_probs = "numeric",
    type = "character"
  )
)

setClass(
  "BMD_dichotomous_MA",
  slots = c(
    models = "list",
    bmd = "numeric",
    ma_bmd = "matrix",
    posterior_probs = "numeric",
    type = "character",
    BMD_CDF = "matrix"
  )
)
## Continuous model class
setClass(
  "BMD_Bayes_continuous_model",
  slots = c(
    prior = "ANY",
    model = "character",
    distribution = "character",
    parameters = "character",
    mean = "character",
    degree = "numeric",
    bmd = "numeric",
    posterior_probs = "numeric",
    submodels = "list"
  ),
  validity = function(object) {
    if (!length(object@model)) {
      return("Slot 'model' cannot be empty.")
    }
    if (!length(object@distribution)) {
      return("Slot 'distribution' cannot be empty.")
    }
    TRUE
  }
)

## Dichotomous model class
setClass(
  "BMD_Bayes_dichotomous_model",
  slots = c(
    prior = "ANY",
    model = "character",
    parameters = "character",
    mean = "character",
    degree = "numeric",
    bmd = "numeric",
    posterior_probs = "numeric",
    submodels = "list"
  ),
  validity = function(object) {
    if (!length(object@model)) {
      return("Slot 'model' cannot be empty for dichotomous objects.")
    }
    TRUE
  }
)

priorClass <- function(prior, model = character(), mean = character(), parameters = character()) {
  new("priorClass", prior = prior, model = model, mean = mean, parameters = parameters)
}

BMD_continuous_MA <- function(models = list(), bmd = numeric(), ma_bmd = matrix(), posterior_probs = numeric(), type = character()) {
  new("BMD_continuous_MA", 
    models = models,
    bmd = bmd, 
    ma_bmd = ma_bmd,
    posterior_probs = posterior_probs,
    type = type
  )
} 

BMD_dichotomous_MA <- function(models = list(), bmd = numeric(), ma_bmd = matrix(), posterior_probs = numeric(), type = character(), BMD_CDF = matrix()) {
  new("BMD_dichotomous_MA", 
    models = models,
    bmd = bmd, 
    ma_bmd = ma_bmd,
    posterior_probs = posterior_probs,
    type = type,
    BMD_CDF = BMD_CDF
  )
}

BMD_Bayes_continuous_model <- function(prior, model, distribution,
                                       parameters = character(),
                                       mean = character(),
                                       degree = NA_real_,
                                       bmd = numeric(),
                                       posterior_probs = numeric(),
                                       submodels = list()) {
  new("BMD_Bayes_continuous_model",
      prior = prior,
      model = model,
      distribution = distribution,
      parameters = parameters,
      mean = mean,
      degree = degree,
      bmd = bmd,
      posterior_probs = posterior_probs,
      submodels = submodels
  )
}

BMD_Bayes_dichotomous_model <- function(prior, model,
                                        parameters = character(),
                                        mean = character(),
                                        degree = NA_real_,
                                        bmd = numeric(),
                                        posterior_probs = numeric(),
                                        submodels = list()) {
  new("BMD_Bayes_dichotomous_model",
      prior = prior,
      model = model,
      parameters = parameters,
      mean = mean,
      degree = degree,
      bmd = bmd,
      posterior_probs = posterior_probs,
      submodels = submodels
  )
}

setMethod("show", "BMD_Bayes_continuous_model", function(object) {
  cat("BMD Bayesian Continuous Model\n")
  cat("  Model:        ", object@model, "\n")
  cat("  Distribution: ", object@distribution, "\n")
  cat("  Mean Label:   ", object@mean, "\n")
  cat("  Parameters:   ", paste(object@parameters, collapse = ", "), "\n")
  cat("  Degree:       ", object@degree, "\n")
  cat("  Prior:\n")
  str(object@prior)
  cat("\n")
})

setMethod("show", "BMD_Bayes_dichotomous_model", function(object) {
  cat("BMD Bayesian Dichotomous Model\n")
  cat("  Model:       ", object@model, "\n")
  cat("  Mean Label:  ", object@mean, "\n")
  cat("  Parameters:  ", paste(object@parameters, collapse = ", "), "\n")
  cat("  Degree:      ", object@degree, "\n")
  cat("  Prior:\n")
  str(object@prior)
  cat("\n")
})