#' Random mu infection frequency model with CI
#'
#' @param p_t A numeric value between 0 and 1
#' @param F_val A numeric value for the fitness
#' @param mu_vect A numeric vector of transmission rates for each group
#' @param bin_props A numeric vector of proportions for each group
#' @param s_h A numeric value for the CI
#' @param N_val A numeric value for the total population size
#'
#' @return A numeric value for the infection frequency
#' @export
#'
#' @examples
#' infection_freq_rmu_ci(0.4,1.2,c(0.01,0.8),c(0.99,0.01),0.1,1000)
infection_freq_rmu_ci <- function(p_t,F_val,mu_vect,bin_props,s_h,N_val){

  N_s_1 <- base::round(F_val * N_val * p_t^2 * bin_props)
  N_s_2 <- base::round(F_val * N_val * p_t * (1 - p_t) * bin_props)
  P_t_1 <- binned_binomial(N_s_1,1-mu_vect)/N_val
  P_t_2 <- binned_binomial(N_s_2,1-mu_vect)/N_val

  X_1 <- base::sum(P_t_1)
  X_2 <- base::sum(P_t_2)

  p_num <- X_1 + X_2
  p_denom <- s_h * (1 - F_val) * p_t^2 + (F_val - s_h - 1) * p_t + s_h * X_1 + 1

  return(p_num/p_denom)

}


#' Iterate Random mu infection frequency model with CI
#'
#' @param F_val A numeric value for the fitness
#' @param mu_vect A numeric vector of transmission rates for each group
#' @param bin_props A numeric vector of proportions for each group
#' @param s_h A numeric value for the CI
#' @param N_val A numeric value for the total population size
#' @param p_t_init A numeric value between 0 and 1
#' @param n_iter A numeric value for the number of iterations
#' @param lwr_thresh A numeric value for the lower threshold
#'
#' @return A numeric vector of infection frequencies
#' @export
#'
#' @examples
#' infection_freq_rmu_ci_iteration(1.2,c(0.01,0.8),c(0.99,0.01),0.1,1000)
infection_freq_rmu_ci_iteration <- function(F_val,mu_vect,bin_props,s_h,N_val,
                                         p_t_init=0.4,n_iter=10000,
                                         lwr_thresh = 0.0001){

  p_t <- p_t_init
  p_t_vec <- base::numeric(n_iter)
  p_t_vec[1] <- p_t
  i_ind <- 2

  while(i_ind <= n_iter && p_t > lwr_thresh){
    p_t <- infection_freq_rmu_ci(p_t,F_val,mu_vect,bin_props,s_h,N_val)
    if (is.na(p_t)){
      p_t <- 1.0
    }
    p_t_vec[i_ind] <- min(p_t,1.0)
    i_ind <- i_ind + 1
  }

  return(p_t_vec[p_t_vec > 0])

}


#' Main simulation for fixed F but random mu model with CI
#'
#' @description
#' This function simulates the infection frequency
#' for a fixed fitness value but random mu values
#' but includes CI
#'
#' @param rep_num A numeric value for the number of replications
#' @param F_val A numeric value for the fitness
#' @param mu_vect A numeric vector of transmission rates for each group
#' @param bin_props A numeric vector of proportions for each group
#' @param s_h A numeric value for the CI
#' @param N_val A numeric value for the total population size
#' @param p_t_init A numeric value between 0 and 1
#' @param ... Additional arguments
#'
#' @return A tibble of the simulation results
#' @export
#'
#' @examples
#' infection_freq_rmu_ci_sim(100,1.2,c(0.01,0.8),c(0.99,0.01),0.1,1000)
infection_freq_rmu_ci_sim <- function(rep_num,F_val,mu_vect,bin_props,s_h,N_val,p_t_init=0.4,...){

  a_sim <- infection_freq_rmu_ci_iteration(F_val,mu_vect,bin_props,s_h,N_val,p_t_init=p_t_init)
  sim_res <- extract_stats(a_sim)
  parm_data <- tibble::tibble(rep_num=rep_num,F_val=F_val,mu_vect=mu_vect,bin_props=bin_props,s_h=s_h,N_val=N_val)
  return(dplyr::bind_cols(parm_data,sim_res))

}
