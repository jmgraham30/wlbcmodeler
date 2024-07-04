turelli_model <- function(p_t,F_val,mu_val,sh_val=0.0){

  num_exp <- p_t*F_val*(1-mu_val)
  den_exp <- 1 + p_t*(F_val - 1 - sh_val) + (p_t^2)*sh_val*(1 - mu_val*F_val)

  p_t1 <- num_exp/den_exp

  return(max(0.0,min(p_t1,1.0)))

}
