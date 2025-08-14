setGeneric("cleveland_plot", function(A) standardGeneric("cleveland_plot"))
utils::globalVariables(c("X1","X2","X3","X4","X5","BMD","BMDL","BMDU","Model","PostProb"))

#' Create a Cleveland plot from a model averaged model.
#'
#' @title cleveland_plot - Create a Cleveland plot from a model averaged model.
#' @param A the model averaged model to plot
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
setMethod("cleveland_plot", "BMD_Bayes_dichotomous_model", function(A) {
  # 'A@submodels' is assumed to be a list of submodel objects,
  # each with a bmd slot (length 3), a model slot, etc.
  submods <- A@submodels
  n_sub <- length(submods)
  bmd_ind <- matrix(0, n_sub + 1, 5)

#   for (i in seq_len(n_sub)) {
#     # Submodel's BMD vector: median, 5%, 95%
#     bmd_ind[i, 1] <- A@models[[i]]@bmd[1] # median
#     bmd_ind[i, 2] <- A@models[[i]]@bmd[2] # 5%
#     bmd_ind[i, 3] <- A@models[[i]]@bmd[3] # 95%
#     bmd_ind[i, 4] <- A@models[[i]]@model
#     bmd_ind[i, 5] <- A@posterior_probs[i] # posterior probability
#   }

#   bmd_ind[n_sub + 1, 1] <- A@bmd[1]
#   bmd_ind[n_sub + 1, 2] <- A@bmd[2]
#   bmd_ind[n_sub + 1, 3] <- A@bmd[3]
#   bmd_ind[n_sub + 1, 4] <- "Model Average"
#   bmd_ind[n_sub + 1, 5] <- 1

  bmd_ind_df <- data.frame(bmd_ind)
  names(bmd_ind_df) <- c("BMD", "BMDL", "BMDU", "Model", "PostProb")

  # Filter out submodels with posterior prob <= 0.05
  bmd_ind_df <- dplyr::filter(bmd_ind_df, rlang::.data$PostProb > 0.05)

  # Drop any NA rows
  bmd_ind_df2 <- bmd_ind_df[!is.na(bmd_ind_df[, "BMD"]), ]

  # Build the ggplot
  out <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = bmd_ind_df2,
      ggplot2::aes(
        x = as.numeric(rlang::.data$BMD),
        y = forcats::fct_reorder(rlang::.data$Model, as.numeric(rlang::.data$PostProb), .desc = TRUE),
        size = sqrt(as.numeric(rlang::.data$PostProb) + 0.01)
      ),
      color = "red"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Dose Level",
      y = "",
      title = "BMD Estimates by Each Model (Sorted by Posterior Probability)",
      size = "Posterior Probability"
    ) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_errorbar(
      data = bmd_ind_df2,
      width = 0.2,
      ggplot2::aes(
        xmin = as.numeric(rlang::.data$BMDL),
        xmax = as.numeric(rlang::.data$BMDU),
        y = forcats::fct_reorder(rlang::.data$Model, rlang::.data$PostProb, .desc = TRUE)
      ),
      color = "black",
      alpha = 0.3
    )

#   return(out)
# })

# setMethod("cleveland_plot", "BMD_continuous_MA", function(A) {
#   # submodels is a list of submodel objects, each with a bmd, model, etc.
#   #submods <- A@models
#   n_sub <- length(A@models)

#   bmd_ind <- matrix(0, n_sub + 1, 5)

#   for (i in seq_len(n_sub)) {
#     bmd_ind[i, 1] <- A@models[[i]]@bmd[1] # median
#     bmd_ind[i, 2] <- A@models[[i]]@bmd[2] # 5%
#     bmd_ind[i, 3] <- A@models[[i]]@bmd[3] # 95%
#     bmd_ind[i, 4] <- names(A@posterior_probs)[i]#A@models[[i]]@full_model
#     bmd_ind[i, 5] <- A@posterior_probs[i]
#   }

#   # Add row for the "Model Average"
#   bmd_ind[n_sub + 1, 1] <- A@bmd[1]
#   bmd_ind[n_sub + 1, 2] <- A@bmd[2]
#   bmd_ind[n_sub + 1, 3] <- A@bmd[3]
#   bmd_ind[n_sub + 1, 4] <- "Model Average"
#   bmd_ind[n_sub + 1, 5] <- 1

  bmd_ind_df <- data.frame(bmd_ind)
  names(bmd_ind_df) <- c("BMD", "BMDL", "BMDU", "Model", "PostProb")

  # Filter out any submodels with posterior prob <= 0.05, etc.
  bmd_ind_df <- dplyr::filter(bmd_ind_df, rlang::.data$PostProb > 0.05)

  bmd_ind_df2 <- bmd_ind_df[!is.na(bmd_ind_df[, "BMD"]), ]

  out <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = bmd_ind_df2,
      ggplot2::aes(
        x = as.numeric(rlang::.data$BMD),
        y = forcats::fct_reorder(rlang::.data$Model, as.numeric(rlang::.data$PostProb), .desc = TRUE),
        size = sqrt(as.numeric(rlang::.data$PostProb) + 0.01)
      ),
      color = "red"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Dose Level",
      y = "",
      title = "BMD Estimates by Each Model (Sorted by Posterior Probability)",
      size = "Posterior Probability"
    ) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_errorbar(
      data = bmd_ind_df2,
      width = 0.2,
      ggplot2::aes(
        xmin = as.numeric(rlang::.data$BMDL),
        xmax = as.numeric(rlang::.data$BMDU),
        y = forcats::fct_reorder(rlang::.data$Model, rlang::.data$PostProb, .desc = TRUE)
      ),
      color = "black",
      alpha = 0.3
    )

#   return(out)
# })
