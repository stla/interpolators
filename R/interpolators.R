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
#' @details See \href{https://www.boost.org/doc/libs/1_83_0/libs/math/doc/html/math_toolkit/barycentric.html}{Barycentric rational interpolation}.
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

#' @title Modified Akima interpolator
#' @description Modified Akima interpolator.
#'
#' @param x,y numeric vectors giving the coordinates of the known points,
#'   without missing value
#'
#' @return An interpolator, for usage in \code{\link{evalInterpolator}}.
#' @export
#'
#' @details See \href{https://www.boost.org/doc/libs/1_83_0/libs/math/doc/html/math_toolkit/makima.html}{Modified Akima interpolation}.
#'
#' @examples
#' library(interpolators)
#' x <- seq(0, 4*pi, length.out = 9L)
#' y <- x - sin(x)
#' ipr <- iprMakima(x, y)
#' curve(x - sin(x), from = 0, to = 4*pi, lwd = 2)
#' curve(
#'   evalInterpolator(ipr, x),
#'   add = TRUE, col = "blue", lwd = 2, lty = "dashed"
#' )
#' points(x, y, pch = 19)
iprMakima <- function(x, y) {
  xy <- checkxy(x, y)
  ipr <- ipr_Makima(xy[["x"]], xy[["y"]])
  attr(ipr, "ipr") <- "makima"
  ipr
}

#' @title PCHIP interpolator
#' @description PCHIP interpolator. It is monotonic.
#'
#' @param x,y numeric vectors giving the coordinates of the known points,
#'   without missing value
#'
#' @return An interpolator, for usage in \code{\link{evalInterpolator}}.
#' @export
#'
#' @details See \href{https://www.boost.org/doc/libs/1_83_0/libs/math/doc/html/math_toolkit/pchip.html}{PCHIP Interpolation}.
#'
#' @examples
#' library(interpolators)
#' x <- seq(0, 4*pi, length.out = 9L)
#' y <- x - sin(x)
#' ipr <- iprPCHIP(x, y)
#' curve(x - sin(x), from = 0, to = 4*pi, lwd = 2)
#' curve(
#'   evalInterpolator(ipr, x),
#'   add = TRUE, col = "blue", lwd = 2, lty = "dashed"
#' )
#' points(x, y, pch = 19)
iprPCHIP <- function(x, y) {
  xy <- checkxy(x, y)
  ipr <- ipr_PCHIP(xy[["x"]], xy[["y"]])
  attr(ipr, "ipr") <- "PCHIP"
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
  } else if(whichipr == "makima") {
    stopifnot(derivative %in% c(0, 1))
    eval_makima(ipr, as.double(x), as.integer(derivative))
  } else if(whichipr == "PCHIP") {
    stopifnot(derivative %in% c(0, 1))
    eval_PCHIP(ipr, as.double(x), as.integer(derivative))
  }
}
