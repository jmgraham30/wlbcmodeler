#' Simulation data for random mu and F model.
#'
#' Model-simulated data for multi-binomial distributed mu
#' and log-normal distributed F.
#' Single mu group scenario.
#'
#'
#'
#' @format ## `rand_mu_f_sim_data_1`
#' A data frame with 55,000 rows and 13 columns:
#' \describe{
#'   \item{rep_num}{Repitition number}
#'   \item{F_val}{F value}
#'   \item{mu_vect}{mu value}
#'   \item{bin_props}{group proportion for mu}
#'   \item{N_val}{Population size}
#'   \item{p_t_median}{median infection proportion}
#'   \item{p_t_mean}{mean infection proportion}
#'   \item{p_t_var}{variance of infection proportion}
#'   \item{p_t_min}{minimum infection proportion}
#'   \item{p_t_max}{maximum infection proportion}
#'   \item{gen_steps}{number of generations}
#'   \item{final_p_t}{final infection proportion}
#'   ...
#' }
#' @source Simulated from a model.
"rand_mu_f_sim_data_1"

#' Simulation data for random mu and F model.
#'
#' Model-simulated data for multi-binomial distributed mu
#' and log-normal distributed F.
#' Two mu groups scenario.
#'
#'
#' @format ## `rand_mu_f_sim_data_2`
#' A data frame with 145,600 rows and 13 columns:
#' \describe{
#'   \item{rep_num}{Repitition number}
#'   \item{F_val}{F value}
#'   \item{mu_vect}{mu value}
#'   \item{bin_props}{group proportion for mu}
#'   \item{N_val}{Population size}
#'   \item{p_t_median}{median infection proportion}
#'   \item{p_t_mean}{mean infection proportion}
#'   \item{p_t_var}{variance of infection proportion}
#'   \item{p_t_min}{minimum infection proportion}
#'   \item{p_t_max}{maximum infection proportion}
#'   \item{gen_steps}{number of generations}
#'   \item{final_p_t}{final infection proportion}
#'   ...
#' }
#' @source Simulated from a model.
"rand_mu_f_sim_data_2"
