#' A Binomial Sample
#'
#' @description
#' A function to generate a binomial sample
#' This is a simple wrapper around the rbinom function to be called
#' by the \code{binned_binomial} function
#'
#' @param size_val A numeric value for the size of the binomial distribution
#' @param prob_val A numeric value for the probability of the binomial distribution
#'
#' @return A numeric value from the binomial distribution
#' @export
#'
#' @examples
#' binomial_size_prob(10,0.5)
binomial_size_prob <- function(size_val,prob_val){
  return(stats::rbinom(1, size = size_val, prob = prob_val))
}

#' Binned Binomial
#'
#' @param size_vect A numeric vector for the sizes of the binomial distribution
#' @param prob_vect A numeric vector for the probabilities of the binomial distribution
#'
#' @return A numeric vector from the binomial distribution
#' @export
#'
#' @examples
#' binned_binomial(c(10,20),c(0.5,0.6))
binned_binomial <- function(size_vect,prob_vect){
  return(purrr::map2_dbl(size_vect,prob_vect,binomial_size_prob))
}

