################################################################################
# Copyright 2020  NIEHS <matt.wheeler@nih.gov>
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation the
# rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
# sell copies of the Software, and to permit persons to whom the Software
# is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
# IN THE SOFTWARE.
################################################################################

.summary_dichotomous_max <- function(object, ...) {
    model <- object
    returnV <- list()
    # alpha <- .evaluate_alpha()
    alpha <- model$options[2]
    if (is.null(model$prior)) {
        returnV$fit_method <- "MLE"
        returnV$prior <- NA
    } else {
        returnV$fit_method <- "Bayesian:MAP"
        returnV$prior <- model$prior
    }
    returnV$fit <- model$full_model

    temp_function <- splinefun(
        model$bmd_dist[, 2],
        model$bmd_dist[, 1],
        method = "monoH.FC",
        ties = mean
    )
    returnV$BMD <- temp_function(1 - c(1 - alpha, 0.5, alpha))
    names(returnV$BMD) <- c("BMDL", "BMD", "BMDU")
    returnV$alpha <- alpha


    returnV$GOF <- cbind(model$gof_chi_sqr_statistic, model$gof_p_value)
    colnames(returnV$GOF) <- c("X^2", "P-Value")
    class(returnV) <- "summary_dichotomous_max"
    returnV
}


.print_summary_dichotomous_max <- function(x, ...) { # nolint
    s_fit <- x

    if (grepl("MLE", s_fit$fit_method)) {
        message(
            sprintf(
                "Summary of single model fit (%s) using ToxicRToo\n",
                "MLE"
            )
        )
        message(s_fit$fit, "\n")
    } else {
        message(
            sprintf(
                "Summary of single model fit (%s) using ToxicRToo\n\n",
                "Bayesian-MAP"
            )
        )
    }
    message("\n")

    message("BMD: ")
    message(
        sprintf(
            "%1.2f (%1.2f, %1.2f) %1.1f%% CI\n",
            s_fit$BMD[2],
            s_fit$BMD[1],
            s_fit$BMD[3],
            100 * (1 - 2 * s_fit$alpha)
        )
    )
    message("\n")
    message("Model GOF\n")
    message("--------------------------------------------------\n")
    s_fit$GOF <- round(s_fit$GOF, 3)
    rownames(s_fit$GOF) <- c("Test: X^2 GOF")
    message(s_fit$GOF)
}
