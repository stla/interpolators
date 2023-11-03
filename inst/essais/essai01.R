library(interpolators)

x <- 1:10
y <- x^2
ipr <- iprBarycentricRational(x, y)
evalInterpolator(ipr, c(2, 2.5))

x <- seq(0, 4*pi, length.out = 10L)
y <- x - sin(x)
ipr <- iprBarycentricRational(x, y)
curve(x - sin(x), from = 0, to = 4*pi)
curve(evalInterpolator(ipr, x), add = TRUE, col = "blue")

