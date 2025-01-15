#' Iterate Random F and mu infection frequency model
#'
#' @param F_val_m A numeric value for the mean fitness
#' @param F_cv A numeric value for the coefficient of variation for the fitness
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
#' ptlt <- infection_freq_rf_rmu_iteration(1.2,0.1,c(0.01,0.9),c(0.99,0.01),1000)
#' plot_a_simulation(ptlt,alpha=0.8)
#' extract_stats(ptlt)
infection_freq_rf_rmu_iteration <- function(F_val_m,F_cv,mu_vect,bin_props,N_val,
                                         p_t_init=0.4,n_iter=10000,
                                         lwr_thresh = 0.0001){

  F_val_s <- stats::rnorm(n_iter, mean=log(F_val_m), sd=sqrt(log(F_cv^2 + 1))) |>
    exp()
  F_val <- base::ifelse(F_val_s < 0.001, 0.001, F_val_s)

  p_t <- p_t_init
  p_t_vec <- base::numeric(n_iter)
  p_t_vec[1] <- p_t
  i_ind <- 2

  while(i_ind <= n_iter && p_t > lwr_thresh){
    p_t <- infection_freq_rmu(p_t,F_val[i_ind-1],mu_vect,bin_props,N_val)
    p_t_vec[i_ind] <- min(p_t,1.0)
    i_ind <- i_ind + 1
  }

  return(p_t_vec[p_t_vec > 0])

}

#' Main simulation for random F and mu model
#'
#' @description
#' This function simulates the infection frequency
#' for random fitness and mu values
#'
#' @param rep_num A numeric value for the number of replications
#' @param F_val_m A numeric value for the mean fitness
#' @param F_cv A numeric value for the coefficient of variation for the fitness
#' @param mu_vect A numeric vector of transmission rates for each group
#' @param bin_props A numeric vector of proportions for each group
#' @param N_val A numeric value for the total population size
#' @param p_t_init A numeric value between 0 and 1 for the initial infection frequency
#' @param ... Additional arguments
#'
#' @return A tibble of simulation results
#' @export
#'
#' @examples
#' infection_freq_rf_rmu_sim(1,1.2,0.1,c(0.01,0.9),c(0.99,0.01),1000)
infection_freq_rf_rmu_sim <- function(rep_num,F_val_m,F_cv,mu_vect,bin_props,N_val,p_t_init=0.4,...){

  a_sim <- infection_freq_rf_rmu_iteration(F_val_m,F_cv,mu_vect,bin_props,N_val,p_t_init=p_t_init)
  sim_res <- extract_stats(a_sim)
  parm_data <- tibble::tibble(rep_num=rep_num,
                              F_val_m=F_val_m,F_cv=F_cv,mu_vect=mu_vect,
                              bin_props=bin_props,s_h=0.0,N_val=N_val,
                              prop_sim=list(a_sim))
  return(dplyr::bind_cols(parm_data,sim_res))

}
