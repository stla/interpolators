#' @title Barycentric rational interpolator
#' @description Barycentric rational interpolator.
#'
#' @param x,y numeric vectors giving the coordinates of the known points,
#'   without missing value
#' @param ao approximation order, an integer greater than or equal to 3
#'
#' @return An interpolator, for usage in \code{\link{evalInterpolator}}.
#' @export
#'
#' @examples
#' library(interpolators)
#' x <- c(1, 2, 4, 5)
#' y <- x^2
#' ipr <- iprBarycentricRational(x, y)
#' evalInterpolator(ipr, c(2, 3))
#' evalInterpolator(ipr, c(2, 3), derivative = 1)
iprBarycentricRational <- function(x, y, ao = 3) {
  stopifnot(ao >= 3)
  xy <- checkxy(x, y)
  ipr <- ipr_barycentricRational(xy[["x"]], xy[["y"]], as.integer(ao))
  attr(ipr, "ipr") <- "barycentricRational"
  ipr
}

#' @title Interpolator evaluation
#' @description Evaluation of an interpolator at some given values.
#'
#' @param ipr an interpolator
#' @param x numeric vector giving the values to be interpolated; missing
#'   values are not allowed
#'
#' @return Numeric vector of interpolated values.
#' @export
evalInterpolator <- function(ipr, x, derivative = 0) {
  stopifnot(isNumericVector(x))
  whichipr <- attr(ipr, "ipr")
  if(whichipr == "barycentricRational") {
    stopifnot(derivative %in% c(0, 1))
    eval_barycentricRational(ipr, as.double(x), as.integer(derivative))
  }
}
