mu_F_equality <- function(F_val){

  return(1 - 1/F_val)

}

no_ci <- function(F_val,mu_val){

  if (F_val * (1 - mu_val) > 1){
    return(1 - (mu_val * F_val)/(F_val - 1))
  } else {
    return(NA)
  }

}

ci_stable <- function(F_val,mu_val,sh_val){

  d_val <- (sh_val + 1 - F_val)^2 + 4*sh_val*(F_val*(1 - mu_val) - 1)*(1 - F_val*mu_val)
  if (d_val >= 0.0){
     return(min(1,(sh_val + 1 - F_val + sqrt(d_val))/(2*sh_val*(1-F_val*mu_val))))
  } else {
    return(NA)
  }

}

ci_unstable <- function(F_val,mu_val,sh_val){

  d_val <- (sh_val + 1 - F_val)^2 + 4*sh_val*(F_val*(1 - mu_val) - 1)*(1 - F_val*mu_val)

  if (d_val >= 0.0){
    return(max(0,(sh_val + 1 - F_val - sqrt(d_val))/(2*sh_val*(1-F_val*mu_val))))
  } else {
    return(NA)
  }

  }

