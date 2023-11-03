library(interpolators)

x <- 1:10
y <- x^2

ipr <- iprBarycentricRational(x, y)

evalInterpolator(ipr, c(2, 2.5))
