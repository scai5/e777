# About ------------------------------------------------------------------------

# GMM helper functions
# ECON 777 Problem Set 1
# Author:         Shirley Cai 
# Date created:   03/07/2024 
# Last edited:    03/18/2024 

# Functions --------------------------------------------------------------------

#' Returns the value of xi(theta) from the inner loop / contraction mapping
#' 
#' @param beta A vector of means of the random coefficient distributions
#' @param sigma A vector of std. dev. of the random coefficient distributions
#' @param plan_char A n x k matrix of k plan characteristics  
#' @returns A vector of xi(theta)
get_xi <- function(beta, sigma, plan_char){
  # Parameters and initial values
  ns <- 100                                     # Number of simulated draws 
  tol <- 10^(-8)                                # Tolerance 
  max_iter <- 500                               # Maximum number of iterations 
  
  delta_err <- 99                               # Initial delta error
  delta <- rep(0, dim(plan_char)[1])            # Initial delta values
  n_iter <- 0                                   # Initial number of iterations
  
  n_rand <- length(beta)                        # Number of random coefficients
  
  # Initialize simulated draws from iid N(0,1)
  nu <- mvrnorm(ns, rep(0, n_rand), diag(rep(1, n_rand)))
  nu <- t(nu)
  
  while(delta_err > tol & n_iter < max_iter){
    # Take random draws from independent N(beta, sigma) - delta
    rand_draws <- nu * sigma
    
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
  
  # Calculate xi based on delta, theta
  xi <- delta - plan_char %*% beta 
  return(xi)
}

#' Returns the value of GMM objective function for given values of parameters
#' 
#' @param theta A vector theta = (beta, sigma)
#' @param plan_char A n x k matrix of k plan characteristics
#' @returns A scalar
get_GMM_obj <- function(theta, plan_char, W){
  
  # Split up theta to be more manageable
  beta <- theta_init[1:dim(plan_char)[2]]
  sigma <- theta_init[(dim(plan_char)[2] + 1):length(theta_init)]
  
  xi <- get_xi(beta, sigma, plan_char)
  
  # Compute and return GMM objective 
  Q <- t(xi) %*% Z
  GMM_obj <- Q %*% W %*% t(Q)
  return(drop(GMM_obj))
}
