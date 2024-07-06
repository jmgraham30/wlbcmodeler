#' Simulation of Turelli et al. 2022 model
#'
#' @description
#' Simulates the Turelli et al. 2022 model for a given number of generations
#' or until the frequency is below a given threshold. This model
#' incorporates a randomly fluctuating fitness value.
#'
#' @param p_0 A numeric value between 0 and 1
#' @param F_val_m A numeric value greater than 0
#' @param F_cv A numeric value greater than 0
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
#' turelli_simulation_rf(0.4,1.021,0.4,0.1,0.0,1000)
turelli_simulation_rf <- function(p_0,F_val_m,F_cv,
                               mu_val,sh_val,N,
                               max_iter=10^8, thresh = 10^(-5)){

  gen_i <- 1
  p <- numeric(max_iter)

  while (gen_i < max_iter & p_0 > thresh){

    F_val_s <- stats::rnorm(1, mean=log(F_val_m), sd=sqrt(log(F_cv^2 + 1))) |>
      exp()

    F_val <- base::ifelse(F_val_s < 0.001, 0.001, F_val_s)

    p_0 <- turelli_iteration(p_0,F_val=F_val,mu_val=mu_val,
                             sh_val=sh_val,N=N)
    gen_i <- gen_i + 1
    p[gen_i] <- p_0
  }

  return(p[p > 0])

}

#' Simulation of Turelli et al. 2022 model
#'
#' @description
#' Simulates the Turelli et al. 2022 model for a given number of generations
#' or until the frequency is below a given threshold. This model
#' incorporates a randomly fluctuating fitness value.
#'
#' @param p_0 A numeric value between 0 and 1
#' @param F_val_m A numeric value greater than 0
#' @param F_cv A numeric value greater than 0
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
#' turelli_simulation_rf_tf(0.4,1.021,0.4,0.1,0.0,1000)
turelli_simulation_rf_tf <- function(p_0,F_val_m,F_cv,
                                  mu_val,sh_val,N,
                                  max_iter=10^8, thresh = 10^(-5)){


  gen_i <- 1

  while (gen_i < max_iter & p_0 > thresh){

    F_val_s <- stats::rnorm(1, mean=log(F_val_m), sd=sqrt(log(F_cv^2 + 1))) |>
      exp()

    F_val <- base::ifelse(F_val_s < 0.001, 0.001, F_val_s)

    p_0 <- turelli_iteration(p_0,F_val=F_val,mu_val=mu_val,
                             sh_val=sh_val,N=N)
    gen_i <- gen_i + 1
  }

  return(gen_i)

}
