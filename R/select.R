#' Variable Selection by Genetic Algorithm.
#'
#' @description \code{select} performs variable selection in regression problems (\code{lm}/\code{glm}) by genetic algorithm. The details of the algorithm used are based on section 3.4 of the Givens and Hoteing book on Computational Statistics.
#'
#' @usage select(formula, data, fitness = NULL, family = gaussian, m = 0.01, gap = 1)
#'
#'
#' @param formula an object of class \code{formula} (or one that can be coerced to that class) that specifies the regression model that needs variable selection.
#' @param data a data frame (or one that an be coerced to that class) containing the variables for regression.
#' @param family an optional family function, that gives the error distribution and link function to be used in the model. Gaussian by default if none specified.
#' @param m an optional number between 0 and 1 that specifies the mutation rate (the probability that mutation occurs) to be used in the process. Default takes the value 0.01
#' @param gap an optional number specifying the generation gap; default takes the value 1. Note that when \code{gap = 1}, all individuals in each generation would be replaced by the generated offspring.
#' Thus, \code{gap = 1} corresponds to distinct and nonoverlapping generations. (The details of generation gap are given under 'Details')
#' @param fitness an optional function to be used as the fitness function; Negative AIC is used by default.
#' A higher value should indicate greater fitness.
#' @return \code{select} returns a list of the selection results.
#'
#' Specifically, the returned list contains four elements:
#' \itemize{
#' \item \code{last}: a matrix that contains the chromosomes generated in the final iteration
#' \item \code{Neg}: an 100 by 2C matrix that contains the negative fitness (negative AIC if default adopted) of each chromesome in each iteration
#' \item \code{selected}: the selected independent variables
#' \item \code{fitness}: the corresponding fitness for the selected variables
#' }
#'
#' @details
#'
#'
#'
#' The Genetic Algorithm used can be broken down to five points:
#'
#' \itemize{
#' \item the first generation (initial population)
#' \item fitness
#' \item selection of parents
#' \item crossover
#' \item mutation
#' }
#'
#' \emph{The first generation} is created at random using binary values. This can be thought of as encoding the genes in a chromosome.
#' Specifically, the length of each chromosome \code{C} equals the number of terms in the formula \code{response~terms}, while the generation size \code{P} is taken to be twice this length.
#'
#'
#' \emph{The fitness function} measures how fit an indivudual is.
#' It should be noted that a typical fitness function should return a greater value when an individual is fitter.
#' By default, \code{select} uses the negative AIC in determining the fitness level of each individual.
#' Additionally, to avoid premature convergence when there exists large variations or other difficulties caused by the actual form of the function,
#' \code{select} adopts a rank-based method in which selectivity is based on relative fitness.
#' Having a higher rank of fitness among the population then indicates a higher probability of being chosen for reproduction.
#' Specifically, \code{probability = rank/sum(rank)}.
#'
#' The \emph{selection of parents} mechanism is to select parents based on their fitness rank.
#' Basically, the idea of selection is to let the fittest individuals pass their genes to the next generation.
#' \code{select} adopts a simple method of selection:
#' when selecting the parents, one is selected with probability proportional to the fitness rank, using methods specified in the above paragraph,
#' while the other is selected completely at random. This is to encourage variation among offsprings and mimic natural selection.
#' \code{select} repeats this selection process 100 times to generate a final population that in theory displays a greater fitness level compared to the first generation.
#' It should also be noted that, \code{select} does not allow duplicate individuals in the population as this potentially distorts the parent selection criterion by inflating the probability to produce offspring for duplicated chromosomes.
#' Specifically, \code{select} eliminates duplicates in this process by comparing each generation of offspring to the previous offsprings as well as
#' those that will move on to the next generation.
#'
#' Populations can be partially updated with the specification of the \emph{generation gap}.
#' The generation gap indicates the proportion of the generation to be replcaced by the generated offspring.
#' When \code{gap = 1}, each generation is distinct, though users should be aware of the potential disappearance of the fittest chromosomes in previous generations,
#' even if the following offsprings show no improvements in fitness. \code{gap < 1} avoids this possibility, though this might reduce variability in population.
#'
#' \emph{Crossover} is the fundamental genetic operator in the generic algorithm.
#' Here, it is done by the simplest method: select a random position between two adjacent loci and split both parent chromosomes at this position,
#' then glue the left chromosome segment from one parent to the right segment from the other parent to form an offspring chromosome.
#' For example, if the two parents are 100110001 and 110100110, and the random split point is between the third and fourth loci,
#' then the potential offspring are 100100110 and 110110001.
#' During crossover, \code{select} only keeps one offspring chromosome for each pair of parents; the second offspring that is formed by combining the remaining segments is discarded.
#'
#' \emph{Mutation}, being another important genetic operator, ensures diversity among the population.
#' Here, it is done with changing an offspring chromosome by randomly introducing one or more alleles in loci where those alleles are not seen in the corresponding loci of either parent chromosome.
#' This implies that some of the bits could be flipped.
#' The default probability of mutation is 0.01, though users can specify their own mutation rate as desired.
#' When choosing the mutation rate, it should be taken into account that an overly high rate would disturb the fitness selectivity, while an overly low rate would discourage revolution and miss potential improvements.
#'
#' The algorithm \emph{terminates} when it reaches the maximum number of iterations. \code{select} uses \code{n = 100} number of iterations.
#' If the number of independent variables in the formula is fewer than or equal to two, there will be no iterations.
#'
#'
#'
#'
#'
#'
#' @references Givens, Geof H., and Jennifer A. Hoeting. Computational Statistics. Wiley, 2013.
#'
#'
#' @examples
#' # An example with the built-in mtcars dataset
#' formula <- mpg~cyl+disp+hp+cyl:disp+wt+gear+carb+gear
#' result <- select(formula, mtcars)
#' selected_vars <- result$selected
#' max_negAIC <- result$fitness
#'
#'
#' # A plotting example with the built-in airquality dataset
#' formula <- Ozone~Solar.R+Wind+Temp+Month+Wind:Temp
#' result <- select(formula, airquality, m = 0.05)
#' AIC <- -result$Neg
#' averaged_AIC <- apply(AIC, 1, mean)
#' plot(averaged_AIC, type = 'l')
#'
#'
#' @export


select <- function(formula, data, fitness = NULL, family = gaussian, m = 0.01, gap = 1) {
  assert_that(is_formula(formula))
  if (is.null(fitness)) {
    fitness <- function(model) return(-model$aic)
  } else {
    assert_that(is.function(fitness))
  }
  assert_that(is.numeric(m))
  assert_that(in_range_m(m))
  assert_that(is.numeric(gap))

  fit_init = glm(formula, data, family = family, x = TRUE)
  assert_that(is_numeric(fitness(fit_init)))

  # user-specified independent variables in the original data
  x_vars_data = colnames(fit_init$model)[-1]

  # y_var in the formula
  y_var <- as.character(formula)[2]

  # convert factor variable to numeric to get the expanded formula
  data_numeric = data
  data_numeric[x_vars_data] = sapply(data[x_vars_data], as.numeric)

  fit_init = glm(formula, data_numeric, family = family, x = TRUE)

  # independent variables to be selected
  x_vars = colnames(fit_init$x)[-1]

  # length of the chromosomes
  C = length(x_vars)

  # size of each generation
  P = 2*C

  assert_that(in_range_gap(gap, 1/P, 1))

  # a maximum number of iterations chosen to limit computing time
  max = 100

  # random initial generation
  init_chromo = array(0, dim = c(C, P))
  init_chromo[,1] = as.numeric(sample(0:1, C, replace = TRUE))

  # how many unique chromos are generated
  k = 1

  while(k < P) {
    # candidate chromosomes
    chromo_cand = as.numeric(sample(0:1, C, replace = TRUE))

    # check for duplicates
    if (!sum(apply(as.matrix(init_chromo[, 1:k]), 2, function(x)
      identical(x, chromo_cand)))) {
      init_chromo[, k+1] = chromo_cand
      k = k + 1
    }
  }

  chromo = init_chromo
  neg_aic = array(0, dim = c(max, P))

  for (j in 1:max) {
    #print(j)
    fit_j = apply(chromo, 2, function(i) {
      x_vars_i = x_vars[as.logical(i)]
      x_formula_i = paste(x_vars_i, collapse = " + ")
      if (x_formula_i != "")
        x_formula_i = paste0(" + ", x_formula_i)

      fit_i = glm(formula(paste0(y_var, "~ 1", x_formula_i)), data, family = family)

      # larger fitness preferred
      return(fitness(fit_i))
    })

    if (C <= 2) {
      neg_aic[max,] = fit_j
      break
    }

    neg_aic[j,] = fit_j

    # how many unique offspring chromosomes are generated
    k = 0

    # update at most a percentage of `gap` of the current generation
    offsprings = array(0, dim = c(C, floor(gap*P)))

    # fitness rank of the current generation,
    rank = rank(fit_j)

    # choose the parents with the probability proportional to the
    # fitness rank
    prob = rank/sum(rank)

    while(k < ncol(offsprings)) {
      # 1st parent selected based on fitness ranks
      index_1 = sample(1:P, prob = prob, 1)

      # 2nd parent selected randomly
      index_2 = sample(c(1:P)[-index_1], 1)

      parent_1 = unlist(chromo[,index_1])
      parent_2 = unlist(chromo[,index_2])

      # crossover
      offspring_cand = crossover(parent_1, parent_2)

      ### additional operators

      # mutation
      offspring_cand = mutation(parent_1, parent_2, offspring_cand, m)

      # check duplicates with the chromosomes of the current generation
      # that will remain in the next generation
      diff_chromos = sum(apply(chromo[,which(rank > ncol(offsprings))], 2,
                               function(x) identical(x, offspring_cand)))

      if (k == 0){
        # for the first offspring generated only need to check
        # with chromosomes of the current generation
        if (diff_chromos == 0) {
          k = k + 1
          offsprings[, k] = offspring_cand
        }
      } else {
        # not the first offspring generated, needs to be checked for uniqueness
        # with previous offsprings and also chromosomes of the current generation
        # that will remain in the next generation
        diff_offs = sum(apply(as.matrix(offsprings[,1:k]), 2, function(x)
          identical(x, offspring_cand)))

        if (sum(diff_offs, diff_chromos) == 0) {
          offsprings[, k+1] = offspring_cand
          k = k + 1
        }
      }

    }

    # update the least fit organisms in the current generation
    # to form the next generation

    chromo[,order(fit_j)[1:ncol(offsprings)]] <- offsprings
  }

  # select the chromosomes with the largest negative aic
  result_chromo = chromo[,which.max(neg_aic[max,])]

  # variables selected
  vars = x_vars[as.logical(result_chromo)]
  if (length(vars) == 0)
    vars = NULL
  # the corresponding aic score
  result_aic = neg_aic[max,which.max(neg_aic[max,])]

  return(list("last" = chromo, "Neg" = neg_aic,
              "selected" = vars, "fitness" = result_aic))
}
