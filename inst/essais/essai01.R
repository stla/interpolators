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


f <- function(x) {
  5*cos(2*x/(1-x)) / (25*(1-x)^2 + x^2)
}
x <- c(seq(0.6, 0.8, by = 0.025), seq(0.82, 0.91, by = 0.01))
y <- f(x)
ipr <- iprBarycentricRational(x, y, ao = 3)
ipr <- iprMakima(x, y)
svg("x.svg", width = 5, height = 5)
curve(
  f(x), from = 0.6, to = 0.91, n = 500, lwd = 2,
  main = "5*cos(2*x/(1-x)) / (25*(1-x)^2 + x^2)"
)
curve(
  evalInterpolator(ipr, x), n = 500, to = 0.909,
  add = TRUE, col = "blue", lwd = 3, lty = "dotted"
)
points(x, y, pch = 19)
dev.off()

rsvg::rsvg_png("x.svg", "oscillating.png", width = 512, height = 512)

# Makima marche mieux mais problème outside range
