# About ------------------------------------------------------------------------

# 2b. BLP
# ECON 777 Problem Set 1
# Author:         Shirley Cai 
# Date created:   03/04/2024 
# Last edited:    03/05/2024 

# Parameters  and initial values -----------------------------------------------

ns = 100                # Number of simulated draws 
tol = 10^(-8)           # Tolerance 
max_iter = 500          # Maximum number of iterations 

delta_init = 0          # Initial delta value
theta_init = c(0,1)     # Initial theta value = (beta bar, sigma)
error_init = 1          # Initial error value

# Helper functions -------------------------------------------------------------

# TODO: get_delta
#' Returns the value of delta from the inner loop / contraction mapping
#' 
#' @param theta A vector theta = (beta, sigma)
#' @returns A scalar
get_delta <- function(theta){
  delta_err <- error_init
  delta <- delta_init
  
  while(delta_err > tol){
    # TODO: Take ns random draws of beta from N(beta, sigma^2)
    # TODO: Compute predicted shares 
    # TODO: Compute new delta via contraction mapping 
    # TODO: Calculate new error 
  }
  
  return(delta)
}

# TODO: get_GMM_obj 
#' Returns the value of GMM objective function
#' 
#' @param theta A vector theta = (beta, sigma)
#' @returns A scalar
get_GMM_obj <- function(theta){
  
  # TODO: delta <- Call get_delta
  
  # TODO: Compute xsi hat 
  # TODO: Compute and return GMM objective 
}

# Run optimization routine -----------------------------------------------------

# TODO: res <- optim(theta_init, get_GMM_obj, method = "Nelder-Mead")