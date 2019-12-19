context("Tests for mutation")

test_that('test for normal cases', {
  parent_1 <- c(1, 1, 1, 0, 0, 1)
  parent_2 <- c(0, 1, 1, 1, 1, 0)
  x <- c(1, 1, 1, 1, 1, 0)

  expect_identical(mutation(parent_1, parent_2, x, 1), c(1, 0, 0, 1, 1, 0))

  #test output format
  expect_true(is.numeric(mutation(parent_1, parent_2, x, 1)))
})


test_that('test for the two parents without same genes, offspring unchanged', {
  parent_1 <- c(1, 0, 0, 0, 0, 1)
  parent_2 <- c(0, 1, 1, 1, 1, 0)
  x <- c(1, 0, 0, 1, 1, 0)

  expect_identical(mutation(parent_1, parent_2, x, 1), x)
})


## test for errors
test_that('test for missing inputs', {
  parent_1 <- c(1, 1, 1, 0, 0, 1)
  parent_2 <- c(0, 1, 1, 1, 1, 0)
  x <- c(1, 1, 1, 1, 1, 0)

  #missing parent
  expect_error(mutation(parent_1))
  #missing offspring
  expect_error(mutation(parent_1, parent_2))
  #missing m
  expect_error(mutation(parent_1, parent_2, x))
})


test_that('test for invalid inputs', {
  parent_1 <- c(1, 1, 1, 0, 0, 1)
  parent_2 <- c("a", "b", "c", "d", "e")
  x <- c(1, 1, 1, 1, 1, 0)

  expect_error(mutation(parent_1, parent_2, x, 1))
})


test_that('test for unmatched lengths of inputs', {
  parent_1 <- c(1, 1, 1, 0, 0, 1)
  x <- c(1, 1, 1, 1, 1, 0)

  expect_error(mutation(parent_1, c(1, 0), x, 1))
})


