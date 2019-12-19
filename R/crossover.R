#' Crossover
#'
#' @description \code{crossover} is to perform the genetic operator crossover between two parent chromosomes, and return one offspring chromosome.
#' @param chromo_1,chromo_2 Numeric vectors with binary 0/1 elements. Should be of same length.
#' @details  To begin with, first select a random position between two adjacent loci,
#' and split both parent chromosomes at this position.
#' Then glue the left chromosome segment from one parent to the right segment
#' from the other parent to form an offspring chromosome
#' (the remaining segments are also combined to form a second offspring, but then discarded).

crossover <- function(chromo_1, chromo_2) {
  ## test whether the inputs are a vector, either character or numeric
  ## test whether the length of two chromosomes are the same

  ## test whether the inputs are numeric vectors
  assert_that(is.numeric(chromo_1), is.numeric(chromo_2))
  ## test whether the length of two chromosomes are the same
  assert_that(length(chromo_1) == length(chromo_2))
  ## test whether there are missing arguments
  assert_that(!is.null(chromo_1) && !is.null(chromo_2))

  C <- length(chromo_1)
  if (C == 1) {
    return(chromo_2)
  }
  pos <- sample(1:(C-1), 1)
  tmp_1 <- chromo_1[1:pos]
  chromo_1[1:pos] <- chromo_2[1:pos]
  chromo_2[1:pos] <- tmp_1

  return(chromo_1)
}
