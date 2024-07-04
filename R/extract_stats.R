#' Extract statistics from a simulation
#'
#' @description
#' Extracts statistics from a vector of p_t values
#'
#' @param p_t_vec A numeric vector
#' @param threshold A numeric value greater than 0
#'
#' @return A tibble
#' @export
#'
#' @examples
#' extract_stats(runif(1000))
extract_stats <- function(p_t_vec,threshold=0.0001){

  p_t_vec_trunc <- p_t_vec[p_t_vec > threshold]

  p_t_median <- stats::median(p_t_vec_trunc)
  p_t_mean <- base::mean(p_t_vec_trunc)
  p_t_var <- stats::var(p_t_vec_trunc)
  p_t_min <- base::min(p_t_vec_trunc)
  p_t_max <- base::max(p_t_vec_trunc)
  gen_steps <- base::length(p_t_vec_trunc)
  final_p_t <- p_t_vec_trunc[gen_steps]

  return(tibble::tibble(p_t_median=p_t_median,
                        p_t_mean=p_t_mean,
                        p_t_var=p_t_var,
                        p_t_min=p_t_min,
                        p_t_max=p_t_max,
                        gen_steps=gen_steps,
                        final_p_t=final_p_t))

}
