#' Turelli Model Function
#'
#' @description
#' Implements the iteration
#'
#' \deqn{p_{t+1} = \frac{p_t F (1 - \mu)}{1 + p_t (F - 1 - s_h) + p_t^2 s_h (1 - \mu F)}}
#'
#'
#' @param p_t A numeric value between 0 and 1
#' @param F_val A numeric value greater than 0
#' @param mu_val A numeric value between 0 and 1
#' @param sh_val A numeric value between 0 and 1
#'
#' @return A numeric value between 0 and 1
#' @export
#'
#' @examples
#' turelli_model(0.4,1.021,0.1)
turelli_model <- function(p_t,F_val,mu_val,sh_val=0.0){

  num_exp <- p_t*F_val*(1-mu_val)
  den_exp <- 1 + p_t*(F_val - 1 - sh_val) + (p_t^2)*sh_val*(1 - mu_val*F_val)

  p_t1 <- num_exp/den_exp

  return(max(0.0,min(p_t1,1.0)))

}
