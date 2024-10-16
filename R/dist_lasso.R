setClass("bmerLassoDist",
         representation(beta.0 = "numeric",
                        lambda = "numeric",
                        c = "numeric"),
         contains = "bmerDist")

toString.bmerLassoDist <- function(x, digits = getOption("digits"), ...) {
  meanString <- ""
  beta.0 <- x@beta.0
  if (length(beta.0) > 4L) {
    meanString <- paste0("mean = c(", toString(round(beta.0[seq_len(4L)], digits)), ", ...)")
  } else if (length(beta.0) == 1L) {
    meanString <- paste0("mean = ", toString(round(beta.0[1L], digits)))
  } else {
    meanString <- paste0("mean = c(", toString(round(beta.0, digits)), ")")
  }
  paste0("Lasso(", meanString, ", ",
         "lambda = ", round(x@lambda, digits), ", ",
         "c = ", round(x@c, digits), ", ",
         "common.scale = ", x@commonScale, ")")
}
setMethod("getDFAdjustment", "bmerLassoDist",
  function(object) {
    if (object@commonScale == TRUE) length(object@beta.0) else 0
  }
)
setMethod("getConstantTerm", "bmerLassoDist",
  function(object) {
    # TODO: add the correct constant term of the lasso prior
    0
  }
)
setMethod("getExponentialTerm", "bmerLassoDist",
  function(object, beta, sigma = NULL) {
    beta.0 <- object@beta.0
    lambda <- object@lambda
    c <- object@c

    # beta[-1] will exclude the intercept from the penalty
    # this approach is course not be ideal in models without an intercept
    result <- lambda * sum(sqrt((beta[-1] - beta.0)^2 + c))
    if (object@commonScale == TRUE && !is.null(sigma)) {
      result <- result / sigma^2
    }
    c(0, result)
  }
)

