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
  invisible(object)
})


setMethod("show", "BMD_continuous_MA", function(object) {
  cat("Model Averaged BMD Continuous Fit \n")
  cat("Fit Type:", ifelse(object@type == "mcmc", "MCMC", "Laplace"), "\n")
  temp = max(nchar(names(model@posterior_probs)))
  bmds = unlist(lapply(model@models, function(x) paste(round(x@bmd[1],3), "(",round(x@bmd[2],2), ",", round(x@bmd[3],2), ")")))
  cat(format("Models:", temp+5, justify='left'), "\t\t BMDs \t\t\t Posterior Probabilities \n")
  cat(paste(format(names(model@posterior_probs), temp + 5, justify='left'), bmds, round(model@posterior_probs,3), sep="\t\t", collapse="\n"))
  #cat("  Maximum: ", object@maximum, "\n")
  cat("\n \n") #separation before model averaged BMD values
  cat(format("Model Averaged BMD:", temp+5, justify='left'), round(object@bmd[1],3), "(", round(object@bmd[2],2),",", round(object@bmd[3],2) ,")\n")
  invisible(object)
})

setMethod("show", "BMD_dichotomous_MA", function(object) {
  cat("Model Averaged BMD Dichotomous Fit \n")
  cat("Fit Type:", ifelse(object@type == "mcmc", "MCMC", "Laplace"), "\n")
  temp = max(nchar(names(model@posterior_probs)))
  bmds = unlist(lapply(model@models, function(x) paste(round(x@bmd[1],3), "(",round(x@bmd[2],2), ",", round(x@bmd[3],2), ")")))
  cat(format("Models:", temp+5, justify='left'), "\t\t BMDs \t\t\t Posterior Probabilities \n")
  cat(paste(format(names(model@posterior_probs), temp + 5, justify='left'), bmds, round(model@posterior_probs,3), sep="\t\t", collapse="\n"))
  #cat("  Maximum: ", object@maximum, "\n")
  cat("\n \n") #separation before model averaged BMD values
  cat(format("Model Averaged BMD:", temp+5, justify='left'), round(object@bmd[1],3), "(", round(object@bmd[2],2),",", round(object@bmd[3],2) ,")\n")

  invisible(object)
})

#' Create a density plot from a model averaged model fit with MCMC.
#'
#' @title MAdensity_plot - Create a density plot from a model averaged model.
#' @param A The model averaged model to plot. An \code{\link{BMD_continuous_MA}} 
#' or \code{\link{BMD_dichotomous_MA}} object.
#' @return Returns a \code{ggplot2} graphics object.
#' @examples
#' \donttest{
#' doses <- cbind(c(0, 25, 50, 100, 200))
#' y <- cbind(
#'   c(6, 5.2, 2.4, 1.1, 0.75),
#'   c(20, 20, 19, 20, 20),
#'   c(1.2, 1.1, 0.81, 0.74, 0.66)
#' )
#' model <- ma_continuous_fit(doses, y,
#'   fit_type = "mcmc", BMR_TYPE = "sd", BMR = 1
#' )
#' try(MAdensity_plot(model))
#' }
#' @export
setGeneric("MAdensity_plot", function(A) standardGeneric("MAdensity_plot"))

setMethod("MAdensity_plot", "BMD_continuous_MA", function(A) {
  # Construct bmd sample plots for mcmc
  if(A@type == 'laplace'){
    stop("Density plots only available for model averaged MCMC fits!")
  }
  X1 <- X2 <- X3 <- NULL
  class_list <- names(A)
  # fit_idx <- grep("Indiv_", class_list)
  fit_idx <- 1:length(A@models)
  qprob <- 0.05

  # Dose levels
  data <- A@models[[fit_idx[1]]]@data
  doses <- data[, 1]
  t_combine <- NA

  post_probs = A@posterior_probs;
  post_probs[is.na(post_probs)] = 0.0;

  for (i in fit_idx) {
    # Loop for the model
    fit <- A@models[[i]]

    temp <- fit@mcmc_result$BMD_samples[!is.nan(fit@mcmc_result$BMD_samples)]
    temp <- temp[!is.infinite(temp)]

    # Try to run the density function.  
    # If there is a problem, return null

    temp_density<-data.frame(matrix(0,length(temp),3))
    temp_density[,2]=substr(fit@full_model,8,999)
    temp_density[,1]=temp
    temp_density[,3]=post_probs[i]

    t <- temp_density

    t_combine <- rbind(t_combine, t)
  }

  t_combine <- t_combine[-1, ]

  idx <- sample(1:length(fit_idx), length(A@models[[fit_idx[1]]]@mcmc_result$BMD_samples), replace = TRUE, prob = post_probs)

  df <- NA
  ##
  for (i in 1:length(fit_idx)) {
    m <- A@models[[i]]
    c <- m@mcmc_result$BMD_samples
    # df <- data.frame(cbind(df, c))
    df<-cbind(df,c)
  }

  # Compute the model average density
  df <- as.matrix(df[,-1])
  result_idx = sample(1:ncol(df),dim(df)[1],replace=T,prob= post_probs )
  BMD_MA = matrix(NA,dim(df)[1],3)
  for (ii in 1:(dim(BMD_MA)[1])){
    BMD_MA[ii,1] = df[ii,result_idx[ii]] 
  }

  BMD_MA<-data.frame(BMD_MA)
  BMD_MA[,2] = "Model Average"
  BMD_MA[,3] = 1
  #clean up t_combine for the plot
  t_combine <- t_combine %>% filter(as.numeric(X3)>0.05 , as.numeric(X1)!=Inf)
  t_combine <-rbind(t_combine,BMD_MA)
  t_combine3 <- t_combine %>% filter(!is.infinite(as.numeric(X3)), 
                                     !is.na(as.numeric(X1)))

  # John's comment- I want to fill the color as
  p <- ggplot() +
    stat_density_ridges(
      data = t_combine3, aes(x = X1, y = fct_reorder(X2, X3, .desc = T), group = X2, alpha = sqrt(X3), fill = cut(X3, c(0, 0.99, 1))),
      calc_ecdf = TRUE, quantiles = c(0.025, 0.975), na.rm = T, quantile_lines = T, scale = 0.9
    ) +
    xlim(c(0, quantile(t_combine$X1, 0.99))) +
    geom_vline(xintercept = A@bmd[1], linetype = "longdash") +
    scale_fill_manual(name = "X3", values = c("(0,0.99]" = "darkgrey", "(0.99,1]" = "red")) +
    labs(x = "Dose Level (Dotted Line : MA BMD)", y = "", title = "Density plots for each fitted model (Fit type: MCMC)") +
    theme_classic()

  p2 <- p + theme(legend.position = "none", axis.text.y = element_text(size = 12))
  return(p2) # Return output
})

setMethod("MAdensity_plot", "BMD_dichotomous_MA", function(A) {
  # Construct bmd sample plots for mcmc
  if(A@type == 'laplace'){
    stop("Density plots only available for model averaged MCMC fits!")
  }
  X1 <- X2 <- X3 <- NULL
  class_list <- names(A)
  # fit_idx <- grep("Indiv_", class_list)
  fit_idx <- 1:length(A@models)
  qprob <- 0.05

  # Dose levels
  data <- A@models[[fit_idx[1]]]@data
  doses <- data[, 1]
  t_combine <- NA

  post_probs = A@posterior_probs;
  post_probs[is.na(post_probs)] = 0.0;

  for (i in fit_idx) {
    # Loop for the model
    fit <- A@models[[i]]
    test_doses <- seq(min(doses), max(doses) * 1.03, (max(doses) * 1.03 - min(doses)) / 100)
    probs <- (0.5 + fit@data[, 2, drop = T]) / (1.0 + fit@data[, 3, drop = T])



    if (fit@model == "hill") {
      Q <- apply(fit@mcmc_result$PARM_samples, 1, .dich_hill_f, d = test_doses)
    }
    if (fit@model == "gamma") {
      Q <- apply(fit@mcmc_result$PARM_samples, 1, .dich_gamma_f, d = test_doses)
    }
    if (fit@model == "logistic") {
      Q <- apply(fit@mcmc_result$PARM_samples, 1, .dich_logist_f, d = test_doses)
    }
    if (fit@model == "log-logistic") {
      Q <- apply(fit@mcmc_result$PARM_samples, 1, .dich_llogist_f, d = test_doses)
    }
    if (fit@model == "probit") {
      Q <- apply(fit@mcmc_result$PARM_samples, 1, .dich_probit_f, d = test_doses)
    }
    if (fit@model == "log-probit") {
      Q <- apply(fit@mcmc_result$PARM_samples, 1, .dich_lprobit_f, d = test_doses)
    }
    if (fit@model == "multistage") {
      Q <- apply(fit@mcmc_result$PARM_samples, 1, .dich_multistage_f, d = test_doses)
    }
    if (fit@model == "qlinear") {
      Q <- apply(fit@mcmc_result$PARM_samples, 1, .dich_qlinear_f, d = test_doses)
    }

    temp <- fit@mcmc_result$BMD_samples[!is.nan(fit@mcmc_result$BMD_samples)]
    temp <- temp[!is.infinite(temp)]




    Dens <- density(temp, cut = c(max(doses)))
    # what is this 0.4 means? Scale?

    # normalize it?-- We don't need it actually here
    # Dens$y = Dens$y/max(Dens$y) * max(probs)
    # temp = which(Dens$x < max(doses))
    # D1_y = Dens$y[temp]
    # D1_x = Dens$x[temp]

    # Do I need to stack up the dataset?
    temp_density <- data.frame(matrix(0, length(temp), 3))
    temp_density[, 2] <- fit@model
    temp_density[, 1] <- temp
    temp_density[, 3] <- post_probs[i]

    # assign(paste("t",i,sep="_"),temp_density)
    # 06/21/21 Update
    t <- temp_density

    t_combine <- rbind(t_combine, t)
  }

  t_combine <- t_combine[-1, ]

  #
  # t_combine<-rbind(t_1,t_2,t_3,t_4,t_5,t_6,t_7,t_8,t_9)
  #
  # This part is needed to get MA density plots
  #
  # idx <- sample(1:9, length(A$Individual_Model_1$mcmc_result$BMD_samples),replace=TRUE,prob=A$posterior_probs)

  idx <- sample(1:length(fit_idx), length(A@models[[fit_idx[1]]]@mcmc_result$BMD_samples), replace = TRUE, prob = post_probs)

  df <- NA

  # How should I initialize this?
  for (i in 1:length(fit_idx)) {
    m <- A@models[[i]]
    c <- m@mcmc_result$BMD_samples
    df <- data.frame(cbind(df, c))
  }

  df_samples <- data.frame(df[, -1])

  # Select MA values
  BMD_MA <- matrix(NA, length(A@models[[fit_idx[1]]]@mcmc_result$BMD_samples), 1)

  for (i in 1:length(A@models[[fit_idx[1]]]@mcmc_result$BMD_samples)) {
    # BMD_MA[i,1]<-combine_samples[sample(nrow(combine_samples), size=1, replace=TRUE),idx[i]]
    j <- sample(nrow(df_samples), size = 1, replace = TRUE)
    BMD_MA[i, 1] <- df_samples[j, idx[i]]
  }

  BMD_MA <- data.frame(BMD_MA)

  t_ma <- BMD_MA %>%
    mutate(X1 = BMD_MA, X2 = "Model Average", X3 = 1)
  # BMD_CDF should be shown here - it
  t_ma2 <- t_ma %>%
    select(X1, X2, X3)

  t_combine2 <- rbind(t_combine, t_ma2)
  t_combine3 <- t_combine2 %>%
    filter(as.numeric(X3) > 0.05, as.numeric(X1) != Inf)

  p <- ggplot() +
    stat_density_ridges(
      data = t_combine3, aes(x = X1, y = fct_reorder(X2, X3, .desc = T), group = X2, alpha = sqrt(X3), fill = cut(X3, c(0, 0.99, 1))),
      calc_ecdf = TRUE, quantiles = c(0.025, 0.975), na.rm = T, quantile_lines = T, scale = 0.9
    ) +
    xlim(c(0, quantile(t_combine$X1, 0.99))) +
    geom_vline(xintercept = A@bmd[1], linetype = "longdash") +
    scale_fill_manual(name = "X3", values = c("(0,0.99]" = "darkgrey", "(0.99,1]" = "red")) +
    labs(x = "Dose Level (Dotted Line : MA BMD)", y = "", title = "Density plots for each fitted model (Fit type: MCMC)") +
    theme_classic()

  p2 <- p + theme(legend.position = "none", axis.text.y = element_text(size = 12))
  p2
  return(p2) # Return output
})

#' Create a Cleveland plot from a model averaged model.
#'
#' @title cleveland_plot - Create a Cleveland plot from a model averaged model.
#' @param A the model averaged model to plot. An \code{\link{BMD_continuous_MA}} 
#' or \code{\link{BMD_dichotomous_MA}} object.
#' @return Returns a \code{ggplot2} graphics object.
#' @examples
#' \donttest{
#' mData <- matrix(c(
#'   0, 2, 50,
#'   1, 2, 50,
#'   3, 10, 50,
#'   16, 18, 50,
#'   32, 18, 50,
#'   33, 17, 50
#' ), nrow = 6, ncol = 3, byrow = TRUE)
#' D <- mData[, 1]
#' Y <- mData[, 2]
#' N <- mData[, 3]
#'
#' model <- ma_dichotomous_fit(D, Y, N)
#' cleveland_plot(model)
#' }
#' @export
setGeneric("cleveland_plot", function(A) standardGeneric("cleveland_plot"))

setMethod("cleveland_plot", "BMD_dichotomous_MA", function(A) {
  # 'A@submodels' is assumed to be a list of submodel objects,
  # each with a bmd slot (length 3), a model slot, etc.
  #submods <- A@models
  n_sub <- length(A@models)
  bmd_ind <- matrix(0, n_sub + 1, 5)

  for (i in seq_len(n_sub)) {
    # Submodel's BMD vector: median, 5%, 95%
    bmd_ind[i, 1] <- A@models[[i]]@bmd[1] # median
    bmd_ind[i, 2] <- A@models[[i]]@bmd[2] # 5%
    bmd_ind[i, 3] <- A@models[[i]]@bmd[3] # 95%
    bmd_ind[i, 4] <- A@models[[i]]@model
    bmd_ind[i, 5] <- A@posterior_probs[i] # posterior probability
  }

  bmd_ind[n_sub + 1, 1] <- A@bmd[1]
  bmd_ind[n_sub + 1, 2] <- A@bmd[2]
  bmd_ind[n_sub + 1, 3] <- A@bmd[3]
  bmd_ind[n_sub + 1, 4] <- "Model Average"
  bmd_ind[n_sub + 1, 5] <- 1

  bmd_ind_df <- data.frame(bmd_ind)

  # Filter out submodels with posterior prob <= 0.05
  bmd_ind_df <- dplyr::filter(bmd_ind_df, as.numeric(X5) > 0.05)

  # Drop any NA rows
  bmd_ind_df2 <- bmd_ind_df[!is.na(bmd_ind_df[, 1]),]

  # Build the ggplot
  out <- ggplot() +
    geom_point(
      data = bmd_ind_df2,
      aes(
        x = as.numeric(X1),
        y = forcats::fct_reorder(X4, as.numeric(X5), .desc = TRUE),
        size = sqrt(as.numeric(X5) + 0.01)
      ),
      color = "red"
    ) +
    theme_minimal() +
    labs(
      x = "Dose Level",
      y = "",
      title = "BMD Estimates by Each Model (Sorted by Posterior Probability)",
      size = "Posterior Probability"
    ) +
    theme(legend.position = "none") +
    geom_errorbar(
      data = bmd_ind_df2,
      width = 0.2,
      aes(
        xmin = as.numeric(X2),
        xmax = as.numeric(X3),
        y = forcats::fct_reorder(X4, X5, .desc = TRUE)
      ),
      color = "black",
      alpha = 0.3
    )

  return(out)
})

setMethod("cleveland_plot", "BMD_continuous_MA", function(A) {
  # submodels is a list of submodel objects, each with a bmd, model, etc.
  #submods <- A@models
  n_sub <- length(A@models)

  bmd_ind <- matrix(0, n_sub + 1, 5)

  for (i in seq_len(n_sub)) {
    bmd_ind[i, 1] <- A@models[[i]]@bmd[1] # median
    bmd_ind[i, 2] <- A@models[[i]]@bmd[2] # 5%
    bmd_ind[i, 3] <- A@models[[i]]@bmd[3] # 95%
    bmd_ind[i, 4] <- names(A@posterior_probs)[i]#A@models[[i]]@full_model
    bmd_ind[i, 5] <- A@posterior_probs[i]
  }

  # Add row for the "Model Average"
  bmd_ind[n_sub + 1, 1] <- A@bmd[1]
  bmd_ind[n_sub + 1, 2] <- A@bmd[2]
  bmd_ind[n_sub + 1, 3] <- A@bmd[3]
  bmd_ind[n_sub + 1, 4] <- "Model Average"
  bmd_ind[n_sub + 1, 5] <- 1

  bmd_ind_df <- data.frame(bmd_ind)

  # Filter out any submodels with posterior prob <= 0.05, etc.
  bmd_ind_df <- dplyr::filter(bmd_ind_df, as.numeric(X5) > 0.05)

  bmd_ind_df2 <- bmd_ind_df[!is.na(bmd_ind_df[, 1]),]

  out <- ggplot() +
    geom_point(
      data = bmd_ind_df2,
      aes(
        x = as.numeric(X1),
        y = forcats::fct_reorder(X4, as.numeric(X5), .desc = TRUE),
        size = sqrt(as.numeric(X5) + 0.01)
      ),
      color = "red"
    ) +
    theme_minimal() +
    labs(
      x = "Dose Level",
      y = "",
      title = "BMD Estimates by Each Model (Sorted by Posterior Probability)",
      size = "Posterior Probability"
    ) +
    theme(legend.position = "none") +
    geom_errorbar(
      data = bmd_ind_df2,
      width = 0.2,
      aes(
        xmin = as.numeric(X2),
        xmax = as.numeric(X3),
        y = forcats::fct_reorder(X4, X5, .desc = TRUE)
      ),
      color = "black",
      alpha = 0.3
    )

  return(out)
})

setMethod(
  "plot", 
  signature = signature(x = "BMD_continuous_MA", y = "missing"),
  function(x, y, ...) {
    A = x
  model_no <- x_axis <- y_axis <-cols <- NULL
  temp_args = list(...)
  if (!exists("qprob",temp_args)){
    qprob = 0.05
  }else{
    qprob = temp_args$qprob
  }
  # Should be matched with BMD_MA plots
  # SL 06/02 Updated 
  # Later, we'll have it 
     density_col="blueviolet"
     credint_col="azure2"
     class_list <- names(A)
     fit_idx    <- 1:length(A@models)
  
     A@posterior_probs[!is.finite( A@posterior_probs)] = 0.0
     #plot the model average curve
     if (A@type == "mcmc"){ # mcmc run
          n_samps <- nrow(A@models[[1]]@mcmc_result$PARM_samples)
          data_d   <-  A@models[[1]]@data
          max_dose <- max(data_d[,1])
          min_dose <- min(data_d[,1])
          test_doses <- seq(min_dose,max_dose,(max_dose-min_dose)/500) 
          ma_samps <- sample(fit_idx,n_samps, replace=TRUE,prob = A@posterior_probs)
          temp_f   <- matrix(0,n_samps,length(test_doses))
          temp_bmd <- rep(0,length(test_doses))
          width= (max_dose-min_dose)/20
          
          # 06/07/21 SL Update
          IS_SUFFICIENT=FALSE
   
          if (ncol(data_d) == 4 ){ #sufficient statistics
            IS_SUFFICIENT = TRUE
            mean <- data_d[,2,drop=F]
            se   <- data_d[,4,drop=F]/sqrt(data_d[,3,drop=F])
            doses = data_d[,1,drop=F]
            uerror <- mean+2*se
            lerror <- mean-2*se
            dose = c(doses,doses)
            Response = c(uerror,lerror)
            lm_fit = lm(mean ~ doses,weights = 1/(se*se))
          }else{
            Response <- data_d[,2,drop=F]
            doses = data_d[,1,drop=F]
            lm_fit = lm(Response~doses)
          }
          
          if (coefficients(lm_fit)[2] < 0){
            decrease = TRUE
          }else{
            decrease = FALSE
          }
          
          for (ii in 1:n_samps){
               fit <- A@models[[fit_idx[ma_samps[ii]]]]
               isLogNormal = (grepl("Log-Normal",fit@full_model) == 1)
               asdf <- switch(fit@model,
                           "exp-aerts" = .cont_exp_aerts_f(fit@mcmc_result$PARM_samples[ii,],test_doses),
                           "invexp-aerts" = .cont_invexp_aerts_f(fit@mcmc_result$PARM_samples[ii,],test_doses),
                           "gamma-aerts" = .cont_gamma_aerts_f(fit@mcmc_result$PARM_samples[ii,],test_doses),
                           "invgamma-aerts" = .cont_invgamma_aerts_f(fit@mcmc_result$PARM_samples[ii,],test_doses),
                           "hill-aerts" = .cont_hill_aerts_f(fit@mcmc_result$PARM_samples[ii,],test_doses),
                           "lomax-aerts" = .cont_lomax_aerts_f(fit@mcmc_result$PARM_samples[ii,],test_doses), 
                           "invlomax-aerts" = .cont_invlomax_aerts_f(fit@mcmc_result$PARM_samples[ii,],test_doses), 
                           "lognormal-aerts" = .cont_lognormal_aerts_f(fit@mcmc_result$PARM_samples[ii,],test_doses), 
                           "logskew-aerts" = .cont_logskew_aerts_f(fit@mcmc_result$PARM_samples[ii,],test_doses), 
                           "invlogskew-aerts" = .cont_invlogskew_aerts_f(fit@mcmc_result$PARM_samples[ii,],test_doses), 
                           "logistic-aerts" = .cont_logistic_aerts_f(fit@mcmc_result$PARM_samples[ii,],test_doses), 
                           "probit-aerts" = .cont_probit_aerts_f(fit@mcmc_result$PARM_samples[ii,],test_doses),
                           "LMS" = .cont_LMS_f(fit@mcmc_result$PARM_samples[ii,],test_doses),
                           "gamma-efsa" = .cont_gamma_efsa_f(fit@mcmc_result$PARM_samples[ii,],test_doses))
               if(isLogNormal & !is.null(asdf)){
                 asdf <- exp(asdf)
               }
               if(!is.null(asdf)){
                 temp_f[ii,] <- asdf
                 temp_bmd[ii] <- fit@mcmc_result$BMD_samples[ii]
                 rm(asdf)
               }
               if (fit@model=="FUNL"){
                    temp_f[ii,] <- .cont_FUNL_f(fit@mcmc_result$PARM_samples[ii,],test_doses)
                    temp_bmd[ii] <- fit@mcmc_result$BMD_samples[ii]
               }  
               if (fit@model=="hill"){
                    temp_f[ii,] <- .cont_hill_f(fit@mcmc_result$PARM_samples[ii,],test_doses)
                    temp_bmd[ii] <- fit@mcmc_result$BMD_samples[ii]
               }
               if (fit@model=="exp-3"){
                    temp_f[ii,] <- .cont_exp_3_f(fit@mcmc_result$PARM_samples[ii,],test_doses,decrease)
                    temp_bmd[ii] <- fit@mcmc_result$BMD_samples[ii]
               }
               if (fit@model=="exp-5"){
                    temp_f[ii,] <- .cont_exp_5_f(fit@mcmc_result$PARM_samples[ii,],test_doses)
                    temp_bmd[ii] <- fit@mcmc_result$BMD_samples[ii]
               }
               if (fit@model=="power"){
                    temp_f[ii,] <- .cont_power_f(fit@mcmc_result$PARM_samples[ii,],test_doses)
                    temp_bmd[ii] <- fit@mcmc_result$BMD_samples[ii]
               }
          }
          
          temp_f[is.infinite(temp_f)] = NA
          temp_f[abs(temp_f) > 1e10] = NA
          
          # If temp_bmd== Inf then delete;
          # Updated 06/02/21 SL
          temp_bmd[is.infinite(temp_bmd)] = NA
          
          me <- apply(temp_f,2,quantile, probs = 0.5,na.rm = TRUE) # BMD
          lq <- apply(temp_f,2,quantile, probs = qprob,na.rm = TRUE) # BMDL
          uq <- apply(temp_f,2,quantile, probs = 1-qprob,na.rm = TRUE) # BMDU

          
          # 06/02/21 SL update
          if (IS_SUFFICIENT){
         
              plot_gg<-ggplot()+xlim(-max(test_doses)*5,min(test_doses)*5)+
                  geom_point(aes(x=data_d[,1],y=data_d[,2]))+
                  geom_errorbar(aes(x=data_d[,1], ymin=lerror, ymax=uerror),color="grey",linewidth=0.8,width=width)+
                  xlim(c(min(data_d[,1])-width,max(data_d[,1])*1.03))+
                  labs(x="Dose", y="Response",title="Continuous MA fitting")+
                  theme_minimal()
          
          }else{
            plot_gg<-ggplot()+xlim(-max(test_doses)*5,min(test_doses)*5)+
              geom_point(aes(x=doses,y=Response))+
              xlim(c(min(doses),max(doses)*1.03))+
              labs(x="Dose", y="Response",title="Continuous MA fitting")+
              theme_minimal()
          }
          
          
          plot_gg<-plot_gg+
                   geom_ribbon(aes(x=test_doses,ymin=lq,ymax=uq),fill="blue",alpha=0.1)
          
          plot_gg<-plot_gg+
                   geom_line(aes(x=test_doses,y=me),col="blue",linewidth=2)
         
          bmd <- quantile(temp_bmd,c(qprob,0.5,1-qprob),na.rm = TRUE)
  
          
          
          if(sum(!is.nan(test_doses) + !is.infinite(test_doses)) == 0){ 
            temp = temp_bmd[temp_bmd < 10*max(test_doses)]
            temp = temp[!is.infinite(temp_bmd)]
            temp = temp[!is.na(temp)]
       
            # Density only creates few data points SL
            
            # Fixed part 06/04/21
            Dens =  density(temp,cut=c(5*max(test_doses)), n=1000, from=0, to=max(test_doses))
          
            Dens$y = Dens$y/max(Dens$y) * (max(Response)-min(Response))*0.6
            temp = which(Dens$x < max(test_doses))
            D1_y = Dens$y[temp]
            D1_x = Dens$x[temp]
            qm = min(Response)
            scale = (max(Response)-min(Response))/max(D1_y) *.40
            
          
             plot_gg<-plot_gg+
                    geom_polygon(aes(x=c(max(0,min(D1_x)),D1_x,max(D1_x)),
                                     y=c(min(Response),min(Response)+D1_y*scale,min(Response))),
                                     fill = "blueviolet", alpha=0.6)

          }
          
          ## 
          # Add lines to the BMD
          ma_mean <- splinefun(test_doses,me)
          ma_BMD = A@bmd
       
          df<-NULL
           
          # Problem of the loop using this case- the ggplot is not added automatically, 
          # It replaces the last one;
          
          for (ii in 1:length(fit_idx)){
            if (!is.finite(A@posterior_probs[ii])){
              A@posterior_probs[ii] = 0
            }
            if (A@posterior_probs[ii]>0.05){
               fit <- A@models[[fit_idx[ii]]]
               isLogNormal = (grepl("Log-Normal",fit@full_model) == 1)
               f <- switch(fit@model,
                            "exp-aerts" = .cont_exp_aerts_f(fit@parameters,test_doses),
                            "invexp-aerts" = .cont_invexp_aerts_f(fit@parameters,test_doses),
                            "gamma-aerts" = .cont_gamma_aerts_f(fit@parameters,test_doses),
                            "invgamma-aerts" = .cont_invgamma_aerts_f(fit@parameters,test_doses),
                            "hill-aerts" = .cont_hill_aerts_f(fit@parameters,test_doses),
                            "lomax-aerts" = .cont_lomax_aerts_f(fit@parameters,test_doses), 
                            "invlomax-aerts" = .cont_invlomax_aerts_f(fit@parameters,test_doses), 
                            "lognormal-aerts" = .cont_lognormal_aerts_f(fit@parameters,test_doses), 
                            "logskew-aerts" = .cont_logskew_aerts_f(fit@parameters,test_doses), 
                            "invlogskew-aerts" = .cont_invlogskew_aerts_f(fit@parameters,test_doses), 
                            "logistic-aerts" = .cont_logistic_aerts_f(fit@parameters,test_doses), 
                            "probit-aerts" = .cont_probit_aerts_f(fit@parameters,test_doses),
                            "LMS" = .cont_LMS_f(fit@parameters,test_doses),
                            "gamma-efsa" = .cont_gamma_efsa_f(fit@parameters,test_doses))
               if(isLogNormal & !is.null(f)){
                 f <- exp(f)
               }
               if (fit@model=="FUNL"){
                    f <- .cont_FUNL_f(fit@parameters,test_doses)
               }  
               if (fit@model=="hill"){
                    f <- .cont_hill_f(fit@parameters,test_doses)
               }
               if (fit@model=="exp-3"){
                   temp = fit@parameters
                    f <- .cont_exp_3_f(temp,test_doses,decrease)
               }
               if (fit@model=="exp-5"){
                    f <- .cont_exp_5_f(fit@parameters,test_doses)
               }
               if (fit@model=="power"){
                    f <- .cont_power_f(fit@parameters,test_doses)
               }
               col = 'coral3'
               temp_df<-data.frame(x_axis=test_doses,y_axis=f,cols=col,model_no=ii, alpha_lev=unname(A@posterior_probs[ii]))
               # # 06/19/21 SL update 
               df     <-data.frame(x_axis=test_doses,y_axis=f,cols=col,model_no=ii, alpha_lev=unname(A@posterior_probs[ii]))
               
               df <-rbind(df,temp_df)
        
               #SL Updated 06/18/21 -- Transparency update based on posterior probability and Y scale for dichotomous case
               temp_data<- df %>% filter(model_no==ii)
               
               plot_gg<- plot_gg+
                 geom_line(data=temp_data, aes(x=x_axis,y=y_axis,color=cols),alpha=unique(temp_data$alpha_lev),show.legend=F)+
                 theme_minimal()
               
               plot_gg <- plot_gg +
                         geom_segment(aes(x=A@bmd[2], y=ma_mean(A@bmd[1]), xend=min(max(doses),A@bmd[3]),
                                          yend=ma_mean(A@bmd[1])),color="darkslategrey",linewidth=1.2, alpha=0.9) +
                         annotate( geom = "text", x = A@bmd[2], y = ma_mean(A@bmd[1]),
                                   label = "[", size = 10,color="darkslategrey", alpha=0.9)+
                         annotate(geom = "text", x = A@bmd[3], y = ma_mean(A@bmd[1]),
                                  label = "]", size = 10,color="darkslategrey", alpha=0.9) +
                         annotate(geom = "point", x = A@bmd[1], y = ma_mean(A@bmd[1]),
                                  size = 5, color="darkslategrey",shape=17, alpha=0.9)
            }
            
          
          }
          
          

     }else{ #laplace run
       
       data_d   <-  A@models[[1]]@data
       max_dose <- max(data_d[,1])
       min_dose <- min(data_d[,1])
       width= (max_dose-min_dose)/20
       test_doses <- seq(min_dose,max_dose,(max_dose-min_dose)/200); 
       temp_bmd <- rep(0,length(test_doses))
       IS_SUFFICIENT = F
       if (ncol(data_d) == 4 ){ #sufficient statistics
         mean <- data_d[,2,drop=F]
         se   <- data_d[,4,drop=F]/sqrt(data_d[,3,drop=F])
         doses = data_d[,1,drop=F]
         uerror <- mean+2*se
         lerror <- mean-2*se
         dose = c(doses,doses)
         Response = c(uerror,lerror)
       
         lm_fit = lm(mean ~ doses,weights = 1/(se*se))
         IS_SUFFICIENT = T
       }else{
         Response <- data_d[,2,drop=F]
         doses = data_d[,1,drop=F]
         lm_fit = lm(Response~doses)
       }
       
       
       if (coefficients(lm_fit)[2] < 0){
         decrease = TRUE
       }else{
         decrease = FALSE
       }
       me = test_doses*0   
       for (ii in 1:length(fit_idx)){
         fit <- A@models[[fit_idx[ii]]]
         isLogNormal = (grepl("Log-Normal",fit@full_model) == 1)
         t <- switch(fit@model,
                      "exp-aerts" = .cont_exp_aerts_f(fit@parameters,test_doses),
                      "invexp-aerts" = .cont_invexp_aerts_f(fit@parameters,test_doses),
                      "gamma-aerts" = .cont_gamma_aerts_f(fit@parameters,test_doses),
                      "invgamma-aerts" = .cont_invgamma_aerts_f(fit@parameters,test_doses),
                      "hill-aerts" = .cont_hill_aerts_f(fit@parameters,test_doses),
                      "lomax-aerts" = .cont_lomax_aerts_f(fit@parameters,test_doses), 
                      "invlomax-aerts" = .cont_invlomax_aerts_f(fit@parameters,test_doses), 
                      "lognormal-aerts" = .cont_lognormal_aerts_f(fit@parameters,test_doses), 
                      "logskew-aerts" = .cont_logskew_aerts_f(fit@parameters,test_doses), 
                      "invlogskew-aerts" = .cont_invlogskew_aerts_f(fit@parameters,test_doses), 
                      "logistic-aerts" = .cont_logistic_aerts_f(fit@parameters,test_doses), 
                      "probit-aerts" = .cont_probit_aerts_f(fit@parameters,test_doses),
                      "LMS" = .cont_LMS_f(fit@parameters,test_doses),
                      "gamma-efsa" = .cont_gamma_efsa_f(fit@parameters,test_doses))
         if (!is.finite(A@posterior_probs[ii])){
           A@posterior_probs[ii] = 0
         }
         if(isLogNormal & !is.null(t)){
           t <- exp(t)
         }
         if(!is.null(t)){
           if (!is.finite(A@posterior_probs[ii])){
             A@posterior_probs[ii] = 0
           }
           if(A@posterior_probs[ii] > 0){
             me = t*A@posterior_probs[ii] + me
           }
         }
         if (fit@model=="FUNL"){
           t <- .cont_FUNL_f(fit@parameters,test_doses)
           if (!is.finite(A@posterior_probs[ii])){
             A@posterior_probs[ii] = 0
           }
           if(A@posterior_probs[ii] > 0){
             me = t*A@posterior_probs[ii] + me
           }
          
         }  
         if (fit@model=="hill"){
            
           t <- .cont_hill_f(fit@parameters,test_doses)
           
           # SL comment - why the name of object is BB? At the beginning it was declared as A-  05/28/21
           # I guess this part should be A as well 
           if (!is.finite(A@posterior_probs[ii])){
             A@posterior_probs[ii] = 0
           }
           if(A@posterior_probs[ii] > 0){
             me = t*A@posterior_probs[ii] + me
           }
         }
         if (fit@model=="exp-3"){
           t <- .cont_exp_3_f(fit@parameters,test_doses,decrease)
          
           if(A@posterior_probs[ii] > 0){
             me = t*A@posterior_probs[ii] + me
           }
         }
         if (fit@model=="exp-5"){
           t <- .cont_exp_5_f(fit@parameters,test_doses)
           if(A@posterior_probs[ii] > 0){
             me = t*A@posterior_probs[ii] + me
           }
         }
         if (fit@model=="power"){
           t <- .cont_power_f(fit@parameters,test_doses)
           if(A@posterior_probs[ii] > 0){
             me = t*A@posterior_probs[ii] + me
           }
         }
       }

       if (IS_SUFFICIENT){

         plot_gg<-ggplot()+xlim(-max(test_doses)*5,min(test_doses)*5)+
           geom_point(aes(x=data_d[,1],y=data_d[,2]))+
           geom_errorbar(aes(x=data_d[,1], ymin=lerror, ymax=uerror),color="grey",linewidth=0.8,width=width)+
           xlim(c(min(data_d[,1])-width,max(data_d[,1])*1.03))+
           labs(x="Dose", y="Response",title="Continous MA fitting")+
           theme_minimal()
           y_min = min(lerror)
           y_max = max(uerror)
       }else{
         plot_gg<-ggplot()+xlim(-max(test_doses)*5,min(test_doses)*5)+
           geom_point(aes(x=doses,y=Response))+
           xlim(c(min(doses),max(doses)*1.03))+
           labs(x="Dose", y="Response",title="Continous MA fitting")+
           theme_minimal()
           y_min = min(Response)
           y_max = max(Response)
       }
       
        
       plot_gg<-plot_gg+
         geom_line(aes(x=test_doses,y=me),col="blue",linewidth=2)
 
       ## 
       # Add lines to the BMD
       ma_mean <- splinefun(test_doses,me)
       ma_BMD = A@bmd

       # Not sure about this part - SL 05/28/21
       #Plot only level >2
       
       
       for (ii in 1:length(fit_idx)){
       df<-NULL
         if (!is.finite(A@posterior_probs[ii])){
           A@posterior_probs[ii] = 0
         }
         if (A@posterior_probs[ii]>0.05){
           fit <- A@models[[fit_idx[ii]]]
           isLogNormal = (grepl("Log-Normal",fit@full_model) == 1)
           f <- switch(fit@model,
                       "exp-aerts" = .cont_exp_aerts_f(fit@parameters,test_doses),
                       "invexp-aerts" = .cont_invexp_aerts_f(fit@parameters,test_doses),
                       "gamma-aerts" = .cont_gamma_aerts_f(fit@parameters,test_doses),
                       "invgamma-aerts" = .cont_invgamma_aerts_f(fit@parameters,test_doses),
                       "hill-aerts" = .cont_hill_aerts_f(fit@parameters,test_doses),
                       "lomax-aerts" = .cont_lomax_aerts_f(fit@parameters,test_doses), 
                       "invlomax-aerts" = .cont_invlomax_aerts_f(fit@parameters,test_doses), 
                       "lognormal-aerts" = .cont_lognormal_aerts_f(fit@parameters,test_doses), 
                       "logskew-aerts" = .cont_logskew_aerts_f(fit@parameters,test_doses), 
                       "invlogskew-aerts" = .cont_invlogskew_aerts_f(fit@parameters,test_doses), 
                       "logistic-aerts" = .cont_logistic_aerts_f(fit@parameters,test_doses), 
                       "probit-aerts" = .cont_probit_aerts_f(fit@parameters,test_doses),
                       "LMS" = .cont_LMS_f(fit@parameters,test_doses),
                       "gamma-efsa" = .cont_gamma_efsa_f(fit@parameters,test_doses))
           if(isLogNormal & !is.null(f)){
             f <- exp(f)
           }
           if (fit@model=="FUNL"){
             f <- .cont_FUNL_f(fit@parameters,test_doses)
           }
           if (fit@model=="hill"){
             f <- .cont_hill_f(fit@parameters,test_doses)
           }
           if (fit@model=="exp-3"){
             temp = fit@parameters
             f <- .cont_exp_3_f(temp,test_doses,decrease)
           }
           if (fit@model=="exp-5"){
             f <- .cont_exp_5_f(fit@parameters,test_doses)
           }
           if (fit@model=="power"){
             f <- .cont_power_f(fit@parameters,test_doses)
           }

           col = 'coral3'
           # Not using loop, but save data in the external data and load it later
           temp_df<-data.frame(x_axis=test_doses,y_axis=f,cols=col,model_no=ii, alpha_lev=unname(A@posterior_probs[ii]))
           df<-temp_df #rbind(df,temp_df)
           plot_gg<- plot_gg+
                geom_line(data=df, aes(x=x_axis,y=y_axis,color=col),alpha=0.5,show.legend=F)
         }
       

         
                    
       }
       plot_gg <- plot_gg +
                   geom_segment(aes(x=A@bmd[2], y=ma_mean(A@bmd[1]), xend=min(max(doses),abs(A@bmd[3])),
                                    yend=ma_mean(A@bmd[1])),color="darkslategrey",linewidth=1.2, alpha=0.9) +
                   annotate( geom = "text", x = A@bmd[2], y = ma_mean(A@bmd[1]),
                             label = "[", size = 10,color="darkslategrey", alpha=0.9)+
                   annotate(geom = "text", x = A@bmd[3], y = ma_mean(A@bmd[1]),
                            label = "]", size = 10,color="darkslategrey", alpha=0.9) +
                   annotate(geom = "point", x = A@bmd[1], y = ma_mean(A@bmd[1]),
                            size = 5, color="darkslategrey",shape=17, alpha=0.9)
     }
   
     return(plot_gg + 
              coord_cartesian(xlim=c(min(test_doses)-width,max(test_doses)+width),expand=F))
  }
)

# 4C) Model-Averaged approach: replicate .plot.BMDdichotomous_MA
# We store submodels in x@submodels, each either MCMC or maximized
# We store x@fit_type == "mcmc" or "laplace" (like your old class checks).
setMethod(
  "plot",
  signature = signature(x = "BMD_dichotomous_MA", y = "missing"),
  function(x, y, ...) {
    temp_args <- list(...)
    if (!"qprob" %in% names(temp_args)) {
      qprob <- 0.05
    } else {
      qprob <- temp_args$qprob
    }

    density_col <- "blueviolet"
    credint_col <- "azure2"
    submods <- x@models
    nSub <- length(submods)
    if (nSub < 1) {
      stop("No submodels found in this MA object.")
    }
    # A <- x

    # assume all submodels share same data shape
    # take data from first submodel
    data_d <- submods[[1]]@data
    probs <- (0.5 + data_d[, 2]) / (1.0 + data_d[, 3])
    se <- sqrt(probs * (1 - probs) / data_d[, 3])
    doses <- data_d[, 1]
    uerror <- apply(cbind(probs * 0 + 1, probs + se), 1, min)
    lerror <- apply(cbind(probs * 0, probs - se), 1, max)

    library(ggplot2)
    temp_name = x@type
    if(temp_name == "laplace"){ #capitalize first letter
      temp_name = tools::toTitleCase(temp_name)
    }else{ #capitalize MLE/MCMC
      temp_name = toupper(temp_name)
    }
    plot_gg <- ggplot() +
      geom_errorbar(aes(x = doses, ymin = lerror, ymax = uerror), color = "grey") +
      xlim(c(-5 * max(doses)), 5 * max(doses)) +
      labs(x = "Dose", y = "Proportion", title = paste0("Model : Dichotomous MA, Fit type : ", temp_name)) +
      theme_minimal()

    max_dose <- max(doses)
    min_dose <- min(doses)
    test_doses <- seq(min_dose, max_dose, (max_dose - min_dose) / 500)

    if (x@type == "mcmc") {
      # -------------- "BMDdichotomous_MA_mcmc" logic --------------
      # sample submodels with prob A@posterior_probs,
      n_samps <- nrow(submods[[1]]@mcmc_result$PARM_samples)
      x@posterior_probs[!is.finite(x@posterior_probs)] <- 0
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
          alpha_val <- unname(x@posterior_probs[ii])
          tmp_df <- data.frame(x_axis = test_doses, y_axis = f, alpha_lev = alpha_val, model_no = ii)
          plot_gg <- plot_gg +
            geom_line(data = tmp_df, aes(x = x_axis, y = y_axis), color = "coral3", alpha = alpha_val) +
            theme_minimal()
        }
      }

      return(plot_gg + coord_cartesian(xlim = c(min(doses), max(doses)), expand = FALSE))

    } else if (x@type == "laplace") {
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
        # if (!is.finite(x@posterior_probs[ii])) {
        #   x@posterior_probs[ii] = 0
        # }
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
          alpha_val <- unname(x@posterior_probs[ii])
          tmp_df <- data.frame(x_axis = test_doses, y_axis = f, alpha_lev = alpha_val, model_no = ii)
          plot_gg <- plot_gg +
            geom_line(data = tmp_df, aes(x = x_axis, y = y_axis), color = "coral3", alpha = alpha_val) +
            theme_minimal()
        }
      }

      return(plot_gg + coord_cartesian(xlim = c(min(doses), max(doses)), expand = FALSE))
    }

    # if fit_type is neither "mcmc" nor "laplace"
    stop("BMD_dichotomous_fit_MA with unknown fit_type:", x@type)
  }
)

