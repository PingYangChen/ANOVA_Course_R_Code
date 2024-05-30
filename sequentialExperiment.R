library(DiceOptim)
library(lhs)


grlee12 <- function(x) {
  xx <- x*(2.5 - 0.5) + 0.5
  return( sin(10*pi*xx) / (2*xx) + (xx-1)^4 )
}


optimSol <- list(x = 0.02428307, y = -0.8690111)
x_grid <- as.matrix(seq(0, 1, length = 1000))
y_grid <- apply(x_grid, 1, grlee12)
plot(x_grid, y_grid, type = "l", col = "blue", lwd = 2, 
     xlab = "x (max-min scaled)", ylab = "f(x)")
points(optimSol$x, optimSol$y, pch = 17, col = "darkorange3")
legend("topleft", c("f(x)", "Optim"), bty = "n",
       pch = c(NA, 17), col = c("blue", "darkorange3"), 
       lty = c(1, NA), lwd = c(2, NA))