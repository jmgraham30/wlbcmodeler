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

  N_s <- base::round(F_val * N_val * p_t * bin_props)
  P_t <- binned_binomial(N_s,1-mu_vect)/N_val

  return(base::sum(P_t) / (1 + p_t*(F_val - 1)))

}

#' Iterate Random mu infection frequency model
#'
#' @param F_val A numeric value for the fitness
#' @param mu_vect A numeric vector of transmission rates for each group
#' @param bin_props A numeric vector of proportions for each group
#' @param N_val A numeric value for the total population size
#' @param p_t_init A numeric value between 0 and 1 for the initial infection frequency
#' @param n_iter A numeric value for the maximum number of iterations
#' @param lwr_thresh A numeric value for the lower threshold
#'
#' @return A numeric vector of infection frequencies
#' @export
#'
#' @examples
#' ptlt <- infection_freq_rmu_iteration(1.2,c(0.01,0.9),c(0.99,0.01),1000)
#' plot_a_simulation(ptlt,alpha=0.8)
#' extract_stats(ptlt)
infection_freq_rmu_iteration <- function(F_val,mu_vect,bin_props,N_val,
                                         p_t_init=0.4,n_iter=10000,
                                         lwr_thresh = 0.0001){

  p_t <- p_t_init
  p_t_vec <- base::numeric(n_iter)
  p_t_vec[1] <- p_t
  i_ind <- 2

  while(i_ind <= n_iter && p_t > lwr_thresh){
    p_t <- infection_freq_rmu(p_t,F_val,mu_vect,bin_props,N_val)
    p_t_vec[i_ind] <- min(p_t,1.0)
    i_ind <- i_ind + 1
  }

  return(p_t_vec[p_t_vec > 0])

}


#' Main simulation for fixed F but random mu model
#'
#' @description
#' This function simulates the infection frequency
#' for a fixed fitness value but random mu values
#'
#' @param rep_num A numeric value for the number of replications
#' @param F_val A numeric value for the fitness
#' @param mu_vect A numeric vector of transmission rates for each group
#' @param bin_props A numeric vector of proportions for each group
#' @param N_val A numeric value for the total population size
#' @param p_t_init A numeric value between 0 and 1 for the initial infection frequency
#' @param ... Additional arguments
#'
#' @return A tibble with the simulation results
#' @export
#'
#' @examples
#' infection_freq_rmu_sim(1,1.05,c(0.01,0.9),c(0.99,0.01),1000)
infection_freq_rmu_sim <- function(rep_num,F_val,mu_vect,bin_props,N_val,p_t_init=0.4,...){

  a_sim <- infection_freq_rmu_iteration(F_val,mu_vect,bin_props,N_val,p_t_init=p_t_init)
  sim_res <- extract_stats(a_sim)
  parm_data <- tibble::tibble(rep_num=rep_num,F_val=F_val,mu_vect=mu_vect,bin_props=bin_props,N_val=N_val)
  return(dplyr::bind_cols(parm_data,sim_res))

}
