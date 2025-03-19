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

# Continuous functions are defined here

# FUNL
.cont_FUNL_f <- function(parms, doses) {
    b <- parms[1] +
        parms[2] *
            exp((doses - parms[5])^2 * (-parms[6])) *
            (1 / (1 + exp(-(doses - parms[3]) / parms[4])))
    b
}

# dichotomous hill
.cont_hill_f <- function(parms, d) {
    g <- parms[1]
    nu <- parms[2]

    k <- parms[3]
    n <- parms[4]
    rval <- g + nu * d^n / (k^n + d^n)
    rval
}

# dichotomous log-logistic
.cont_exp_5_f <- function(parms, d) {
    g <- parms[1]
    b <- parms[2]
    c <- parms[3]
    e <- parms[4]
    rval <- g * (exp(c) - (exp(c) - 1.0) * (exp(-(b * d)^e)))
    rval
}

# dichotomous log-probit
.cont_exp_3_f <- function(parms, d) {
    g <- parms[1]
    b <- parms[2]
    e <- parms[4]
    rval <- g * exp((b * d)^e)
    rval
}

.cont_power_f <- function(parms, d) {
    g <- parms[1]
    b <- parms[2]
    a <- parms[3]
    rval <- g + b * d^a
    rval
}
