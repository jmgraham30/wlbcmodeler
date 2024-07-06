#' Turelli Model Function
#'
#' @description
#' Implements the iteration
#'
#' \deqn{p_{t+1} = \frac{p_t F (1 - \mu)}{1 + p_t (F - 1 - s_h) + p_t^2 s_h (1 - \mu F)}}
#'
#' This is the basic deterministic model.
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

#' Turelli Iteration
#'
#' @description
#' Implements the basic iteration for the Turelli model
#' assuming a finite population. This calls the function \code{turelli_model}.
#'
#' @param p_val A numeric value between 0 and 1
#' @param F_val A numeric value greater than 0
#' @param mu_val A numeric value between 0 and 1
#' @param sh_val A numeric value between 0 and 1
#' @param N A numeric value greater than 0
#'
#' @return A numeric value between 0 and 1
#' @export
#'
#' @examples
#' turelli_iteration(0.4,1.021,0.1,0.0,1000)
turelli_iteration <- function(p_val,F_val,
                              mu_val,sh_val,N){

  p_star <- turelli_model(p_val, F_val, mu_val, sh_val)

  return(stats::rbinom(1, N, p_star) / N )

}

#' Simulation of Turelli model
#'
#' @description
#' Simulates the Turelli model for a given number of generations
#' or until the frequency is below a given threshold.
#'
#' @param p_0 A numeric value between 0 and 1
#' @param F_val A numeric value greater than 0
#' @param mu_val A numeric value between 0 and 1
#' @param sh_val A numeric value between 0 and 1
#' @param N A numeric value greater than 0
#' @param max_iter A numeric value greater than 0
#' @param thresh A numeric value between 0 and 1
#'
#' @return A numeric vector with the frequency of infected over time
#' @export
#'
#' @examples
#' turelli_simulation(0.4,1.021,0.1,0.0,1000)
turelli_simulation <- function(p_0,F_val,
                                 mu_val,sh_val,N,
                                 max_iter=10^4, thresh = 10^(-5)){

  gen_i <- 1
  p <- numeric(max_iter)

  while (gen_i < max_iter & p_0 > thresh){
    p_0 <- turelli_iteration(p_0,F_val=F_val,mu_val=mu_val,
                             sh_val=sh_val,N=N)
    gen_i <- gen_i + 1
    p[gen_i] <- p_0
  }

  return(p[p > 0])

}

#' Simulation of Turelli model
#'
#' @description
#' Simulates the Turelli model for a given number of generations
#' or until the frequency is below a given threshold.
#'
#' @param p_0 A numeric value between 0 and 1
#' @param F_val A numeric value greater than 0
#' @param mu_val A numeric value between 0 and 1
#' @param sh_val A numeric value between 0 and 1
#' @param N A numeric value greater than 0
#' @param max_iter A numeric value greater than 0
#' @param thresh A numeric value between 0 and 1
#'
#' @return A numeric with the persistence time
#' @export
#'
#' @examples
#' turelli_simulation_tf(0.4,1.021,0.1,0.0,1000)
turelli_simulation_tf <- function(p_0,F_val,
                               mu_val,sh_val,N,
                               max_iter=10^4, thresh = 10^(-5)){

  gen_i <- 1

  while (gen_i < max_iter & p_0 > thresh){
    p_0 <- turelli_iteration(p_0,F_val=F_val,mu_val=mu_val,
                             sh_val=sh_val,N=N)
    gen_i <- gen_i + 1
  }

  return(gen_i)

}
