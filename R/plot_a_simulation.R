#' Plot a simulation
#'
#' @description
#' Plots a simulation
#'
#' @param p_t_vec A numeric vector
#' @param threshold A numeric value
#' @param alpha_val A numeric value
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' plot_a_simulation(runif(1000))
plot_a_simulation <- function(p_t_vec,threshold=0.0001,alpha_val=0.2){

  y_1 <- base::min(p_t_vec)
  y_2 <- base::max(p_t_vec)

  p_t_vec_trunc <- p_t_vec[p_t_vec > threshold]

  p_t <- NULL

  plot_df <- tibble::tibble(p_t=p_t_vec_trunc)

  ggplot2::ggplot(data=plot_df,mapping=ggplot2::aes(x=1:length(p_t),y=p_t)) +
    ggplot2::geom_point(alpha=alpha_val,color="#4988BFFF") +
    ggplot2::labs(x="Generation",y=latex2exp::TeX("$p_{t}$")) +
    ggplot2::theme_bw(base_size=14) +
    ggplot2::ylim(c(y_1,y_2))

}
