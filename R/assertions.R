library(assertthat)
# test whether a formula is passed in as input
# write function that returns TRUE if the argument 'formula' is indeed in the class formula
# and returns FALSE if otherwise
is_formula <- function(f){
  inherits(f, 'formula')
}

# customize useful error message if is_formula() returns FALSE
on_failure(is_formula) <- function(call, env){
  paste('A non-formula argument is passed in.')
}

in_range <- function(x, lower, upper){
  if (x >= lower && x <= upper)
    return(TRUE)
  return(FALSE)
}

in_range_m <- function(m){
  return(in_range(m, 0, 1))
}

on_failure(in_range_m) <- function(call, env){
  paste("Mutation rate out of range. Expecting 'm' between 0 and 1.")
}

in_range_gap <- function(gap, lower, upper){
  return(in_range(gap, lower, upper))
}

on_failure(in_range_gap) <- function(call, env){
  paste("Generation gap out of range.")
}

is_numeric <- function(x) {
  return(is.numeric(x))
}

on_failure(is_numeric) <- function(call, env){
  paste("fitness doesn't return a numeric value.")
}
