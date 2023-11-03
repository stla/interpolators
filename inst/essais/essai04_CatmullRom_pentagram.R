library(interpolators)

# a closed example (pentagram) ####
rho <- sqrt((5 - sqrt(5))/10)
R <- sqrt((25 - 11*sqrt(5))/10)
points <- matrix(NA_real_, nrow = 10L, ncol = 2L)
points[c(1, 3, 5, 7, 9), ] <- t(vapply(0:4, function(i){
  c(rho*cospi(2*i/5), rho*sinpi(2*i/5))
}, numeric(2L)))
points[c(2, 4, 6, 8, 10), ] <- t(vapply(0:4, function(i){
  c(R*cospi(2*i/5 + 1/5), R*sinpi(2*i/5 + 1/5))
}, numeric(2L)))
ipr <- iprCatmullRom(points, closed = TRUE)
s <- seq(0, 1, length.out = 400L)
Curve <- evalInterpolator(ipr, s)
plot(Curve, type = "l", lwd = 2, asp = 1)
points(points, pch = 19)

svg("x.svg", width = 5, height = 5)
opar <- par(mar = c(2, 2, 1, 1))
plot(Curve, type = "l", lwd = 2, asp = 1, xlab = NA, ylab = NA)
points(points, pch = 19)
par(opar)
dev.off()
rsvg::rsvg_png("x.svg", "pentagram.png", width = 512, height = 512)

