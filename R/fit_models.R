fitModel <- function() {
  x = runif(10)
  y = 2*x + rnorm(1)
  fit1 = lm(y ~ x)
  plot(x, fit1$fitted.values)
}
