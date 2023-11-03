library(interpolators)
points <- rbind(
  c(0, 2.5),
  c(2, 4),
  c(3, 2),
  c(4, 1.5),
  c(5, 6),
  c(6, 5),
  c(7, 3),
  c(9, 1),
  c(10, 2.5),
  c(11, 7),
  c(9, 5),
  c(8, 6),
  c(7, 5.5)
)
ipr <- iprCatmullRom(points)
s <- seq(0, 1, length.out = 400)
Curve <- evalInterpolator(ipr, s)
head(Curve)
plot(Curve, type = "l", lwd = 2)
points(points, pch = 19)
