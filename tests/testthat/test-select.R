context("Tests for select")

test_that("select() input arguments classes", {

  # create some testing objects - some common mistakes that might appear in inputs
  wrong_formula <- "y = x"
  df <- data.frame("x" = rnorm(10), "y" = rnorm(10))
  #matrix_dat <- matrix(rnorm(12), 3, 4)
  expect_error(select(wrong_formula, df),
               "A non-formula argument is passed in.")
  expect_error(select(y~x, df, m = -1),
               "Mutation rate out of range. Expecting 'm' between 0 and 1.")
  expect_error(select(y~x, df, m = "0.01"),
               "m is not a numeric or integer vector")
  expect_error(select(y~x, df, gap = 0.49),
               "Generation gap out of range.")
  expect_error(select(y~x, df, gap = "1"),
               "gap is not a numeric or integer vector")
  # wrong family
  expect_error(select(y~x, df, family = "gamma"))

  # fitness is not a function
  expect_error(select(y~x, df, fitness = 1), "fitness is not a function")

  fitness <- function(model) {
    return("aic")
  }

  expect_error(select(y~x, df, fitness = fitness), "fitness doesn't return a numeric value.")

})

test_that("select() works with valid user-defined fitness", {
  # use r square as fitness
  fitness <- function(model) {
    return(1-model$deviance/model$null.deviance)
  }

  result = select(Sepal.Length ~ ., data = iris, fitness = fitness, gap = 0.5)

  expect_type(result, "list")
  expect_length(result, 4)

})

test_that("select() works with numeric `y` and `x`s",{
  x = matrix(rnorm(400), ncol = 4)
  intercept = rep(1, 100)
  coeff = 5*runif(4)
  y = cbind(intercept, x[,-4]) %*% coeff
  df <- data.frame(cbind(y, x))
  colnames(df) <- c("y", "x1", "x2", "x3", "x4")

  result_1 = select(y~x1, df, gap = 0.5)
  result_2 = select(y~x4, df, gap = 0.5)
  result_3 = select(y~x2+x3, df, gap = 0.5)
  result_4 = select(y~x1+x2+x3, df, gap = 0.5)
  result_5 = select(y~x1+x2+x3+x4, df, gap = 0.5)

  expect_type(result_1, "list")
  expect_length(result_1, 4)
  expect_type(result_2, "list")
  expect_length(result_2, 4)
  expect_type(result_3, "list")
  expect_length(result_3, 4)
  expect_type(result_4, "list")
  expect_length(result_4, 4)
  expect_type(result_5, "list")
  expect_length(result_5, 4)

})



test_that("select() works with numeric independent variable with gaussian family",{
  result_1 = select(Sepal.Length ~ ., data = iris, gap = 0.5)
  result_2 = select(Sepal.Length ~ Sepal.Width, data = iris, gap = 0.5)
  result_3 = select(Sepal.Length ~ Species, data = iris, gap = 0.5)
  result_4 = select(Sepal.Length ~ Sepal.Width*Species, data = iris, gap = 0.5)
  result_5 = select(Sepal.Length ~ Petal.Width*Petal.Length, data = iris, gap = 0.5)

  expect_type(result_1, "list")
  expect_length(result_1, 4)
  expect_type(result_2, "list")
  expect_length(result_2, 4)
  expect_type(result_3, "list")
  expect_length(result_3, 4)
  expect_type(result_4, "list")
  expect_length(result_4, 4)

})

test_that("select() works with 0/1 independent variable with binomial family", {
  x = matrix(rnorm(400), ncol = 4)
  y = sample(0:1, 100, replace = TRUE)
  df <- data.frame(cbind(y, x))
  colnames(df) <- c("y", "x1", "x2", "x3", "x4")

  result_1 = select(y~x1, df, family = binomial, gap = 0.5)
  result_2 = select(y~x2*x3 + x4, df, family = binomial, gap = 0.5)

  expect_type(result_1, "list")
  expect_length(result_1, 4)
  expect_type(result_2, "list")
  expect_length(result_2, 4)
})

test_that("select() works with integer independent variable with poisson family", {
  x = matrix(abs(rnorm(400)), ncol = 4)
  intercept = rep(1, 100)
  coeff = 5*runif(4)
  y = rpois(100, lambda = cbind(intercept, x[,-4]) %*% coeff)
  df <- data.frame(cbind(y, x))
  colnames(df) <- c("y", "x1", "x2", "x3", "x4")

  result_1 = select(y~x1, df, family = poisson, gap = 0.5)
  result_2 = select(y~x4, df, family = poisson, gap = 0.5)
  result_3 = select(y~x2+x3, df, family = poisson, gap = 0.5)
  result_4 = select(y~x2*x3, df, family = poisson, gap = 0.5)
  result_5 = select(y~x1*x2+x3+x4, df, family = poisson, gap = 0.5)

  expect_type(result_1, "list")
  expect_length(result_1, 4)
  expect_type(result_2, "list")
  expect_length(result_2, 4)
  expect_type(result_3, "list")
  expect_length(result_3, 4)
  expect_type(result_4, "list")
  expect_length(result_4, 4)
  expect_type(result_5, "list")
  expect_length(result_5, 4)
})

test_that("select() works with numeric independent variable with Gamma family", {
  result_1 = select(Sepal.Length ~ ., data = iris, family = Gamma, gap = 0.5)
  result_2 = select(Sepal.Length ~ Sepal.Width, data = iris, family = Gamma, gap = 0.5)
  result_3 = select(Sepal.Length ~ Species, data = iris, family = Gamma, gap = 0.5)
  result_4 = select(Sepal.Length ~ Sepal.Width*Species, data = iris, family = Gamma, gap = 0.5)
  result_5 = select(Sepal.Length ~ Petal.Width*Petal.Length, data = iris, family = Gamma, gap = 0.5)

  expect_type(result_1, "list")
  expect_length(result_1, 4)
  expect_type(result_2, "list")
  expect_length(result_2, 4)
  expect_type(result_3, "list")
  expect_length(result_3, 4)
  expect_type(result_4, "list")
  expect_length(result_4, 4)
})

