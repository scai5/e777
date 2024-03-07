# About ------------------------------------------------------------------------

# GMM helper functions
# ECON 777 Problem Set 1
# Author:         Shirley Cai 
# Date created:   03/04/2024 
# Last edited:    03/07/2024 

# Functions --------------------------------------------------------------------

#' Returns the value of xsi(theta) from the inner loop / contraction mapping
#' 
#' @param theta_bar A vector of means of the random coefficient distributions
#' @param sigma A vector of std. dev. of the random coefficient distributions
#' @param plan_char A n x k matrix of k plan characteristics  
#' @returns A scalar
get_xsi <- function(theta_bar, sigma, plan_char){
  # Parameters and initial values
  ns <- 100                                     # Number of simulated draws 
  tol <- 10^(-8)                                # Tolerance 
  max_iter <- 500                               # Maximum number of iterations 
  
  delta_err <- 99                               # Initial delta error
  delta <- rep(0, dim(plan_char)[1])            # Initial delta values
  n_iter <- 0                                   # Initial number of iterations
  
  while(delta_err > tol & n_iter < max_iter){
    # Take ns random draws a, b, ... from independent N(theta_bar, sigma)
    rand_draws <- mvrnorm(ns, theta_bar, diag(sigma))
    rand_draws <- sweep(rand_draws, 2, theta_bar)
    rand_draws <- t(rand_draws)
    
    # Compute predicted shares 
    V <- replicate(ns, delta) + plan_char %*% rand_draws
    pred_share <- rowMeans(inv.logit(V))
    
    # Compute new delta via contraction mapping 
    new_delta <- delta + log(actual_share / pred_share)
    
    # Update for next iteration 
    delta_err <- sqrt(sum((new_delta - delta)^2))
    delta <- new_delta 
    n_iter <- n_iter + 1
  }
  
  # Calculate xsi based on delta, theta
  xsi <- delta - plan_char %*% theta_bar 
  return(xsi)
}
