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

#' @title Catmull-Rom interpolator
#' @description Catmull-Rom interpolator for 2-dimensional or 3-dimensional
#'   points.
#'
#' @param points numeric matrix of 2D or 3D points, one point per row
#' @param closed Boolean, whether the curve is closed
#' @param alpha parameter between 0 and 1; the default value 0.5 is recommended
#'
#' @return An interpolator, for usage in \code{\link{evalInterpolator}}.
#' @export
#'
#' @details See \href{https://www.boost.org/doc/libs/1_83_0/libs/math/doc/html/math_toolkit/catmull_rom.html}{Catmull-Rom splines}.
#'
#' @examples
#' library(interpolators)
#' points <- rbind(
#'   c(0, 2.5),
#'   c(2, 4),
#'   c(3, 2),
#'   c(4, 1.5),
#'   c(5, 6),
#'   c(6, 5),
#'   c(7, 3),
#'   c(9, 1),
#'   c(10, 2.5),
#'   c(11, 7),
#'   c(9, 5),
#'   c(8, 6),
#'   c(7, 5.5)
#' )
#' ipr <- iprCatmullRom(points)
#' s <- seq(0, 1, length.out = 400)
#' Curve <- evalInterpolator(ipr, s)
#' head(Curve)
#' plot(Curve, type = "l", lwd = 2)
#' points(points, pch = 19)
#'
#' # a closed example (pentagram) ####
#' rho <- sqrt((5 - sqrt(5))/10)
#' R <- sqrt((25 - 11*sqrt(5))/10)
#' points <- matrix(NA_real_, nrow = 10L, ncol = 2L)
#' points[c(1, 3, 5, 7, 9), ] <- t(vapply(0:4, function(i){
#'   c(rho*cospi(2*i/5), rho*sinpi(2*i/5))
#' }, numeric(2L)))
#' points[c(2, 4, 6, 8, 10), ] <- t(vapply(0:4, function(i){
#'   c(R*cospi(2*i/5 + 1/5), R*sinpi(2*i/5 + 1/5))
#' }, numeric(2L)))
#' ipr <- iprCatmullRom(points, closed = TRUE)
#' s <- seq(0, 1, length.out = 400L)
#' Curve <- evalInterpolator(ipr, s)
#' plot(Curve, type = "l", lwd = 2, asp = 1)
#' points(points, pch = 19)
iprCatmullRom <- function(points, closed = FALSE, alpha = 0.5) {
  stopifnot(is.matrix(points))
  stopifnot(ncol(points) %in% c(2L, 3L))
  storage.mode(points) <- "double"
  if(anyNA(points)) {
    stop("There are missing values in the `points` matrix.")
  }
  stopifnot(isBoolean(closed))
  stopifnot(isDouble(alpha))
  stopifnot(alpha >= 0, alpha <= 1)
  d <- ncol(points)
  if(d == 2L) {
    ipr <- ipr_catmullRom2(points, closed, as.double(alpha))
    attr(ipr, "ipr") <- "CatmullRom2"
  } else {
    ipr <- ipr_catmullRom3(points, closed, as.double(alpha))
    attr(ipr, "ipr") <- "CatmullRom3"
  }
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
#'   add = TRUE, col = "blue", lwd = 3, lty = "dashed"
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
#' @details See \href{https://www.boost.org/doc/libs/1_83_0/libs/math/doc/html/math_toolkit/pchip.html}{PCHIP interpolation}.
#'
#' @examples
#' library(interpolators)
#' x <- seq(0, 4*pi, length.out = 9L)
#' y <- x - sin(x)
#' ipr <- iprPCHIP(x, y)
#' curve(x - sin(x), from = 0, to = 4*pi, lwd = 2)
#' curve(
#'   evalInterpolator(ipr, x),
#'   add = TRUE, col = "blue", lwd = 3, lty = "dashed"
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
#'   values are not allowed; for Catmull-Rom splines, the values must be
#'   between 0 and 1
#' @param derivative order of differentiation, 0 or 1
#'
#' @return Numeric vector of interpolated values, or numeric matrix of
#'   interpolated points for the Catmull-Rom interpolator.
#' @export
evalInterpolator <- function(ipr, x, derivative = 0) {
  stopifnot(isNumericVector(x))
  whichipr <- attr(ipr, "ipr")
  if(whichipr == "barycentricRational") {
    stopifnot(derivative %in% c(0, 1))
    eval_barycentricRational(ipr, as.double(x), as.integer(derivative))
  } else if(whichipr == "CatmullRom2") {
    stopifnot(derivative %in% c(0, 1))
    stopifnot(all(x >= 0), all(x <= 1))
    eval_catmullRom2(ipr, as.double(x), as.integer(derivative))
  } else if(whichipr == "CatmullRom3") {
    stopifnot(derivative %in% c(0, 1))
    stopifnot(all(x >= 0), all(x <= 1))
    eval_catmullRom3(ipr, as.double(x), as.integer(derivative))
  } else if(whichipr == "makima") {
    stopifnot(derivative %in% c(0, 1))
    eval_makima(ipr, as.double(x), as.integer(derivative))
  } else if(whichipr == "PCHIP") {
    stopifnot(derivative %in% c(0, 1))
    eval_PCHIP(ipr, as.double(x), as.integer(derivative))
  }
}
