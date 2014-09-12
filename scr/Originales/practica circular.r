library(circular)
x <- c(45, 55, 81, 96, 110, 117, 132, 154)
x <- as.circular(x)
x <- rad(x)
x <- as.circular(x)
x <- as.circular(x, units = "deg")
class(x)
mean.circular(x)
var.circular(x)
plot(x, col = "green", zero = pi/2, rotation = "clock")
rose.diag(x, col = "red", zero = pi/2, rotation = "clock")

summary(x)
`?`(circular)

deg(mean.circular(x)) 