
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wlbcmodeler <a href="https://github.com/jmgraham30/wlbcmodeler"><img src="man/figures/wlbcmodeler.png" align="right" height="138" /></a>

<!-- badges: start -->
<!-- badges: end -->

The goal of `wlbcmodeler` is to implement various models for
[wolbachia](https://en.wikipedia.org/wiki/Wolbachia) frequency dynamics.

## Installation

You can install the development version of `wlbcmodeler` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jmgraham30/wlbcmodeler")
```

## Background

$$p_{t+1} = \frac{p_{t}F(1 - \mu)}{1 + p_{t}(F - 1 - s_{h}) + p_{t}^{2}s_{h}(1 - \mu F)},$$
where

- $p_{t}$ is the frequency of Wolbachia infected females in the
  population at time $t$,

- $F$ is the relative fecundity of Wolbachia infected females,

- $\mu$ is the transmission rate, and

- $s_{h}$ is the cytoplasmic incompatibility (CI) rate.

For a finite population of size $N$, the iteration is given by

$$
\begin{align}
p_{t+1} &= \frac{p_{t}F(1 - \mu)}{1 + p_{t}(F - 1 - s_{h}) + p_{t}^{2}s_{h}(1 - \mu F)},  \\
q_{ij} &= \left(\begin{array}{c} N \\ j \end{array}\right)(p^{\ast})^{j}(1 - p^{\ast})^{N - j}. 
\end{align}
$$

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(wlbcmodeler)
## basic example code
```

``` r
library(ggplot2)
library(patchwork)

tp_1 <- turelli_simulation(0.4,1.2,0.1,0.0,1000)
p_1 <- plot_a_simulation(tp_1,alpha_val=0.2) + ylim(c(0,1))
#> Scale for y is already present.
#> Adding another scale for y, which will replace the existing scale.

tp_2 <- turelli_simulation(0.4,1.02,0.5,0.0,1000)
p_2 <- plot_a_simulation(tp_2,alpha_val=1.0) + ylim(c(0,1))
#> Scale for y is already present.
#> Adding another scale for y, which will replace the existing scale.

tp_3 <- turelli_simulation(0.1,1.02,0.1,0.8,1000)
p_3 <- plot_a_simulation(tp_3,alpha_val=0.8) + ylim(c(0,1))
#> Scale for y is already present.
#> Adding another scale for y, which will replace the existing scale.

tp_4 <- turelli_simulation(0.3,1.02,0.1,0.8,1000)
p_4 <- plot_a_simulation(tp_4,alpha_val=0.2) + ylim(c(0,1))
#> Scale for y is already present.
#> Adding another scale for y, which will replace the existing scale.

(p_1 + p_2) / (p_3 + p_4)
```

<img src="man/figures/README-examp_sims-1.png" width="100%" />

``` r
library(latex2exp)

data.frame(x=c(1.0,3.0)) |>
  ggplot(aes(x=x)) + 
  geom_function(fun = mu_F_equality, linewidth=1,color="#4988BFFF") + 
  labs(x = "F", y = TeX("$\\mu$")) + 
  theme_bw()
```

<img src="man/figures/README-fmu-1.png" width="100%" />
