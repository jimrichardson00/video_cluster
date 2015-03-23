# Dimension_reduction_pca.r

x <- rnorm(n = 200, mean = 0, sd = 7)
y <- rnorm(n = 200, mean = 0, sd = 1.5)

x

plot(x, y, xlim = c(-10, 10), ylim = c(-10, 10))

cos(5)

R <- function(theta) {
	R <- c(cos(theta), -sin(theta), sin(theta), cos(theta))
	R <- matrix(R, nrow = 2, byrow = TRUE)
	return(R)
}

theta <- pi/4
x_t <- t(R(theta) %*% t(cbind(x, y)))[, 1]
y_t <- t(R(theta) %*% t(cbind(x, y)))[, 2]

proj <- function(x) {
	return(c(x[1], 0))
}

jpeg("Data.jpeg")
plot(x_t, y_t, xlim = c(-10, 10), ylim = c(-10, 10))
arrows(x0 = 0, y0 = 0, x1 = 10, y1 = 10)
arrows(x0 = 0, y0 = 0, x1 = -2, y1 = 2)
dev.off()

pr <- prcomp(cbind(x_t, y_t), retx = TRUE)

jpeg("Data_dimreduce.jpeg")
plot(pr$x, xlim = c(-10, 10), ylim = c(-10, 10))
segments(x0 = pr$x[, 1], y0 = pr$x[, 2], x1 = pr$x[, 1], y1 = 0)
# arrows(x0 = 0, y0 = 0, x1 = 10, y1 = 0)
# arrows(x0 = 0, y0 = 0, x1 = 0, y1 = 2)
points(pr$x[, 1], rep(0, nrow(pr$x)), col = 'red', pch = 16)
segments(x0 = pr$x[, 1], y0 = pr$x[, 2], x1 = pr$x[, 1], y1 = 0)
dev.off()
pr$x[1, ]
rep(0, ncol(pr$x))

# ------------------------------------------s

frameSTRS2013 <- list.files("/home/jim/Dropbox/REM/Tasks/video_cluster/frameSTRS2013")
frameSTRS2014 <- list.files("/home/jim/Dropbox/REM/Tasks/video_cluster/frameSTRS2014")
frameSKBO2014 <- list.files("/home/jim/Dropbox/REM/Tasks/video_cluster/frameSKBO2014")

require(stringr)
settrapSTRS2013 <- unique(str_match(frameSTRS2013, "(.+)_GOPR+.")[, 2])
settrapSTRS2014 <- unique(str_match(frameSTRS2014, "(.+)_GOPR+.")[, 2])
settrapSKBO2014 <- unique(str_match(frameSKBO2014, "(.+)_GOPR+.")[, 2])

require(stringr)
length(settrapSTRS2013)
length(settrapSTRS2014)
length(settrapSKBO2014)

0.22790*3
