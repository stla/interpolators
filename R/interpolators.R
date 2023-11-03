#' Title
#'
#' @param x x
#' @param y x
#' @param ao x
#'
#' @return x
#' @export
iprBarycentricRational <- function(x, y, ao = 3) {
  stopifnot(ao >= 3)
  o <- order(x)
  x <- as.double(x[o]) # TODO check uniqueness
  y <- as.double(y[o])
  ipr_barycentricRational(x, y, as.integer(ao))
}

#' Title
#'
#' @param ipr x
#' @param x x
#'
#' @return x
#' @export
evalInterpolator <- function(ipr, x) {
  eval_barycentricRational(ipr, as.double(x))
}
