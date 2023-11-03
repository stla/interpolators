library(interpolators)
x <- seq(0, 4*pi, length.out = 9L)
y <- x - sin(x)
ipr <- iprPCHIP(x, y)
curve(x - sin(x), from = 0, to = 4*pi, lwd = 2)
curve(
  evalInterpolator(ipr, x),
  add = TRUE, col = "blue", lwd = 2, lty = "dashed"
)
points(x, y, pch = 19)
