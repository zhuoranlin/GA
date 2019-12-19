context("Tests for crossover")
# create a function that checks if the result of crossover is possible
# this can be done by checking if the resulting chromosome belongs to one of the
# possible crossover scenarios

# testCrossover returns TRUE if the crossover result matches one of the possible outcomes;
# otherwise, it returns FALSE

testCrossover <- function(chromo_1, chromo_2){
  ## note that when the chromosomes only have 1 locus, crossover returns the second chromosome
  if (length(chromo_1) == 1) {
    return(crossover(chromo_1, chromo_2) == chromo_2)
  }
  ## when chromosomes have length > 1, check if the crossover result is a possible case
  for (pos in 1:(length(chromo_1)-1)){
    if (identical(c(chromo_2[1:pos], chromo_1[(pos+1):length(chromo_1)] ) ,crossover(chromo_1, chromo_2))) {
      return(TRUE)
    }

  }
  return(FALSE)
}

# testing on crossover outputs under different scenarios

test_that("crossover is valid", {

  # generate test chromosomes in general cases
  c1 <- c(1,0,0,1,1)
  c2 <- c(0,0,0,0,1)

  # check if crossover works
  expect_equal(testCrossover(c1, c2), TRUE)

})


test_that("crossover is valid in extreme cases", {

  # generate test chromosomes that only have one single locus
  c1_single <- c(1)
  c2_single <- c(0)

  # check if crossover works with chromosomes with a single locus
  expect_equal(testCrossover(c1_single, c2_single), TRUE)

  # generate test chromosomes that have identical loci
  c1_identical <- c(0,0,0,0,0)
  c2_identical <- c(0,0,0,0,0)

  # check if crossover works with identical chromosomes
  expect_equal(testCrossover(c1_identical, c2_identical), TRUE)

})


# testing error occurances

test_that("crossover prints out error when input is missing", {

  c1 <- c(1,0,0,1,1)
  c2 <- c(0,0,0,0,1)

  # check if taking NULL produces error
  expect_error(crossover(NULL), "chromo_1 is not a numeric or integer vector")
  expect_error(crossover(c1, NULL), "chromo_2 is not a numeric or integer vector")
  expect_error(crossover(NULL,c2), "chromo_1 is not a numeric or integer vector")
})


test_that("crossover takes numeric vectors as input arguments", {
  c1 <- c(1,0,0,1,1)
  c2 <- c(0,0,0,0,1)

  # create character vectors for testing
  char1 <- c('a','b','c','d')
  char2 <- c('e','f','g','h')

  # check whether taking character vectors will produce error
  expect_error(crossover(char1, char2), "chromo_1 is not a numeric or integer vector")
  expect_error(crossover(c1, char2), "chromo_2 is not a numeric or integer vector")
  expect_error(crossover(char1, c2), "chromo_1 is not a numeric or integer vector")

  # create logical vectors for testing
  logic1 <- c(TRUE, FALSE, TRUE)
  logic2 <- c(TRUE, FALSE, FALSE)

  # check whether taking logical vectors will produce error
  expect_error(crossover(logic1, logic2), "chromo_1 is not a numeric or integer vector")
  expect_error(crossover(c1, logic2), "chromo_2 is not a numeric or integer vector")
  expect_error(crossover(logic1, c2), "chromo_1 is not a numeric or integer vector")

})
