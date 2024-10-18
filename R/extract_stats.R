#' Extract statistics from a simulation
#'
#' @description
#' Extracts statistics from a vector of p_t values
#'
#' @param p_t_vec A numeric vector
#' @param threshold A numeric value greater than 0
#' @param burn_in A numeric value greater than 0
#'
#' @return A tibble
#' @export
#'
#' @examples
#' extract_stats(runif(1000))
extract_stats <- function(p_t_vec,threshold=0.0001,burn_in=500){

  p_t_vec_trunc <- p_t_vec[p_t_vec > threshold]

  gen_steps <- base::length(p_t_vec_trunc)

  if (gen_steps > burn_in) {
    p_t_vec_s <- p_t_vec_trunc[(burn_in+1):length(p_t_vec_trunc)]
    p_t_median <- stats::median(p_t_vec_s)
    p_t_mean <- base::mean(p_t_vec_s)
    p_t_var <- stats::var(p_t_vec_s)
    p_t_min <- base::min(p_t_vec_s)
    p_t_max <- base::max(p_t_vec_s)
    final_p_t <- p_t_vec_trunc[gen_steps]
  } else {
    p_t_median <- NA
    p_t_mean <- NA
    p_t_var <- NA
    p_t_min <- NA
    p_t_max <- NA
    final_p_t <- NA
  }


  return(tibble::tibble(p_t_median=p_t_median,
                        p_t_mean=p_t_mean,
                        p_t_var=p_t_var,
                        p_t_min=p_t_min,
                        p_t_max=p_t_max,
                        gen_steps=gen_steps,
                        final_p_t=final_p_t))

}
