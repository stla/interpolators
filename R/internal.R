isNumericVector <- function(x) {
  is.numeric(x) && !anyNA(x) && length(x) >= 1L
}

isBoolean <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x)
}

isDouble <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x)
}

checkxy <- function(x, y) {
  stopifnot(isNumericVector(x), isNumericVector(y))
  stopifnot(length(x) == length(y))
  o <- order(x)
  x <- as.double(x[o])
  y <- as.double(y[o])
  if(any(diff(x) == 0)) {
    stop("Duplicated values in `x` are not allowed.")
  }
  list("x" = x, "y" = y)
}
