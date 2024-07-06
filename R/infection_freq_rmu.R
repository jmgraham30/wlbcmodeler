#' Random mu infection frequency model
#'
#' @param p_t A numeric value between 0 and 1
#' @param F_val A numeric value for the fitness
#' @param mu_vect A numeric vector of transmission rates for each group
#' @param bin_props A numeric vector of proportions for each group
#' @param N_val A numeric value for the total population size
#'
#' @return A numeric value for the infection frequency
#' @export
#'
#' @examples
#' infection_freq_rmu(0.5,1,c(0.1,0.2),c(0.5,0.5),100)
infection_freq_rmu <- function(p_t,F_val,mu_vect,bin_props,N_val){

  N_s <- round(F_val * N_val * p_t * bin_props)
  P_t <- binned_binomial(N_s,1-mu_vect)/N_val

  return(sum(P_t) / (1 + p_t*(F_val - 1)))

}
