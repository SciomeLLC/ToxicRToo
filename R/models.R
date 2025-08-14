#' The prior class defining a prior for a specific model
#'
#' @slot prior A matrix of prior information (see \code{\link{create_prior_list}}) 
#' @slot model Character string of model description
#' @slot mean Character string of model type (e.g. hill)
#' @slot parameters Character vector of parameter names
#' @slot distribution Character string of the error distribution
#'
#' @return A priorClass object
#' @export
setClass("priorClass",
slots = c(
  prior = "ANY",
  model = "character",
  mean = "character",
  parameters = "character",
  distribution = "character"
))

#' Model averaged continuous fit class
#' 
#' Methods include plot, MAdensity_plot, and cleveland_plot
#'
#' @slot models List of BMD_continuous_fit_* objects (one per model)
#' @slot bmd Named vector of model averaged BMD, BMDL, BMDU
#' @slot ma_bmd Matrix of spline BMD CDF
#' @slot posterior_probs Named vector of posterior probabilities of each model
#' @slot type Character string of fit_type
#'
#' @return A BMD_continuous_MA object
#' @export
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

#' Model averaged dichotomous fit class
#' 
#' Methods include plot, MAdensity_plot, and cleveland_plot
#'
#' @slot models List of BMD_dichotomous_fit_* objects (one per model)
#' @slot bmd Named vector of model averaged BMD, BMDL, BMDU
#' @slot ma_bmd Matrix of spline BMD CDF 
#' @slot posterior_probs Named vector of posterior probabilities of each model
#' @slot type Character string of fit_type
#' @slot BMD_CDF Matrix of spline BMD CDF
#'
#' @return A BMD_dichotomous_MA object
#' @export
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
setClass("prior",
  slots = c(
    prior = "ANY",
    model = "character",
    mean = "character",
    parameters = "character"
  )
)

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

priorClass <- function(prior, model = character(), mean = character(), parameters = character(), distribution = character()) {
  new("priorClass", prior = prior, model = model, mean = mean, parameters = parameters, distribution = distribution)
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
  invisible(object)
})

##follows old .print.BMD_Bayes_model
setMethod("show", "priorClass", function(object){
  if (!is.null(object@model)) {
    cat(object@model, " Parameter Priors\n")
  } else {
    cat("Model Parameter Priors\n ")
  }

  cat("------------------------------------------------------------------------\n")

  if(is.null(object@prior)){
    cat("Null prior!")
  }else{
    X = object@prior
    for (ii in 1:nrow(X)) {
      V <- X[ii, ]
      if (!is.null(object@parameters)) {
        temp_text <- sprintf("Prior [%s]:", object@parameters[ii])
      } else {
        temp_text <- "Prior: "
      }
      if (V[1] == 1) {
        cat(sprintf(
          "%sNormal(mu = %1.2f, sd = %1.3f) 1[%1.2f,%1.2f]\n", temp_text, V[2],
          V[3], V[4], V[5]
        ))
      }
      if (V[1] == 2) {
        cat(sprintf(
          "%sLog-Normal(log-mu = %1.2f, log-sd = %1.3f) 1[%1.2f,%1.2f]\n", temp_text, V[2],
          V[3], V[4], V[5]
        ))
      }
      if (V[1] == 3) {
        cat(sprintf(
          "%sCauchy(mu = %1.2f, sd = %1.3f) 1[%1.2f,%1.2f]\n", temp_text, V[2],
          V[3], V[4], V[5]
        ))
      }
      if (V[1] == 4) {
        cat(sprintf(
          "%sGamma(mu = %1.2f, sd = %1.3f) 1[%1.2f,%1.2f]\n", temp_text, V[2],
          V[3], V[4], V[5]
        ))
      }
      if (V[1] == 5) {
        cat(sprintf(
          "%sPERT(mu = %1.2f, sd = %1.3f) 1[%1.2f,%1.2f]\n", temp_text, V[2],
          V[3], V[4], V[5]
        ))
      }
    }
  }
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

# Define S4 generic and methods for parse_prior after classes exist
setGeneric("parse_prior", function(prior) standardGeneric("parse_prior"))

## Return the object; downstream code reads slots via @distribution, @mean, @prior
setMethod("parse_prior", signature(prior = "BMD_Bayes_continuous_model"), function(prior) {
  prior
})

setMethod("parse_prior", signature(prior = "BMD_Bayes_dichotomous_model"), function(prior) {
  prior
})