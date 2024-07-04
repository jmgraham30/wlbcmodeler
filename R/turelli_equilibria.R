#' Curve where no-CI equilibria can be maintained
#'
#' @description
#' Implements
#' \deqn{\mu = 1 - \frac{1}{F}}
#'
#'
#' @param F_val A numeric value greater than 0
#'
#' @return A numeric value
#' @export
#'
#' @examples
#' mu_F_equality(1.1)
mu_F_equality <- function(F_val){

  return(1 - 1/F_val)

}



#' Stable equilibrium in case of no CI
#'
#' @description
#' Implements
#' \deqn{p^{\ast} = 1 - \frac{\mu F}{F - 1}}
#'
#' @param F_val A numeric value greater than 0
#' @param mu_val A numeric value between 0 and 1
#'
#' @return A numeric value between 0 and 1
#' @export
#'
#' @examples
#' no_ci(1.2,0.1)
no_ci <- function(F_val,mu_val){

  if (F_val * (1 - mu_val) > 1){
    return(1 - (mu_val * F_val)/(F_val - 1))
  } else {
    return(NA)
  }

}

#' Stable equilibrium in case of CI
#'
#' @description
#' Implements
#' \deqn{p^{\ast} = \frac{s_h + 1 - F + \sqrt{(s_h + 1 - F)^2 + 4s_h(F(1-\mu)-1)(1-F\mu)}}{2s_h(1 - F\mu)}}
#'
#' @param F_val A numeric value greater than 0
#' @param mu_val A numeric value between 0 and 1
#' @param sh_val A numeric value between 0 and 1
#'
#' @return A numeric value between 0 and 1
#' @export
#'
#' @examples
#' ci_stable(1.021,0.1,0.8)
ci_stable <- function(F_val,mu_val,sh_val){

  d_val <- (sh_val + 1 - F_val)^2 + 4*sh_val*(F_val*(1 - mu_val) - 1)*(1 - F_val*mu_val)
  if (d_val >= 0.0){
     return(min(1,(sh_val + 1 - F_val + sqrt(d_val))/(2*sh_val*(1-F_val*mu_val))))
  } else {
    return(NA)
  }

}

#' Unstable equilibrium in case of CI
#'
#' @description
#' Implements
#' \deqn{p^{\ast} = \frac{s_h + 1 - F - \sqrt{(s_h + 1 - F)^2 + 4s_h(F(1-\mu)-1)(1-F\mu)}}{2s_h(1 - F\mu)}}
#'
#' @param F_val A numeric value greater than 0
#' @param mu_val A numeric value between 0 and 1
#' @param sh_val A numeric value between 0 and 1
#'
#' @return A numeric value between 0 and 1
#' @export
#'
#' @examples
#' ci_unstable(1.021,0.1,0.8)
ci_unstable <- function(F_val,mu_val,sh_val){

  d_val <- (sh_val + 1 - F_val)^2 + 4*sh_val*(F_val*(1 - mu_val) - 1)*(1 - F_val*mu_val)

  if (d_val >= 0.0){
    return(max(0,(sh_val + 1 - F_val - sqrt(d_val))/(2*sh_val*(1-F_val*mu_val))))
  } else {
    return(NA)
  }

  }

