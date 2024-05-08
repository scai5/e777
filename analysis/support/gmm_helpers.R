# About ------------------------------------------------------------------------

# GMM helper functions
# ECON 777 Problem Set 1
# Author:         Shirley Cai 
#                 Adapted from Evan Saltzman's BLP code 
# Date created:   03/07/2024 
# Last edited:    05/08/2024 

# Functions --------------------------------------------------------------------

#' Returns predicted shares based on this iteration of delta and sigma.
#' Accesses nu, X_rand, and Dmktyr from the global environment. 
#' 
#' @param sigma A vector of std. dev. of the random coefficient distributions
#' @param delta A vector of delta 
#' @returns A vector of predicted shares
get_pred_share <- function(sigma, delta){
  
  # Compute numerator for each row
  rand_draws <- nu * sigma
  num <- exp(delta + X_rand %*% rand_draws)
  
  # Aggregate num over that row's market and year 
  denom <- 1 + Dmktyr %*% num
  
  pred_share <- rowMeans(num / denom)
  
  # Throw error if there are missing or infinite values 
  if(any(is.na(pred_share))){
    stop("Error: Missing or infinite predicted values")
  }
  
  return(pred_share)
}

#' Returns the value of delta from the inner loop / contraction mapping. 
#' Accesses nu, X_rand, Dmktyr, and n_total from the global environment. 
#' 
#' @param sigma A vector of std. dev. of the random coefficient distributions
#' @param tol Tolerance level for contraction mapping, defaults to 1E(-8)
#' @param max_iter Number of maximum iterations for contraction mapping, defaults to 500
#' @returns A vector of delta 
get_delta <- function(sigma, tol = 10^(-8), max_iter = 500){
  
  # Initial values 
  delta_err <- 99                               # Initial delta error
  delta <- rep(0, n_total)                      # Initial delta values
  n_iter <- 0                                   # Initial number of iterations
  
  while(delta_err > tol & n_iter < max_iter){
    # Compute predicted shares 
    pred_share <- get_pred_share(sigma, delta)
    
    # Compute new delta via contraction mapping 
    new_delta <- delta + log(actual_share / pred_share)
    
    # Update for next iteration 
    delta_err <- sqrt(sum((new_delta - delta)^2) / n_total)
    delta <- new_delta 
    n_iter <- n_iter + 1
  }
  
  return(delta)
}

#' Returns the value of GMM objective function for given values of parameters. 
#' Accesses X, X_rand, Z, and W from the global environment. 
#' 
#' @param sigma A vector of std. dev. of the random coefficient distributions
#' @returns A scalar
get_GMM_obj <- function(sigma){
  
  delta <- get_delta(sigma)
  temp <- t(X) %*% Z %*% W %*% t(Z)
  beta <- solve(temp %*% X) %*% temp %*% delta
  xi <- delta - X %*% beta 
  
  # Compute and return GMM objective 
  Q <- t(xi) %*% Z
  GMM_obj <- drop(Q %*% W %*% t(Q))
  message(paste0("Objective value:", GMM_obj))
  
  return(GMM_obj)
}

#' Runs the GMM routine via Nelder-Mead and returns a vector of converged parameters. 
#' Accesses X, Z, W from the global environment. 
#' 
#' @param param_init A vector of initial parameters sigma
#' @returns A list of beta and sigma
BLP <- function(param_init){
  
  # GMM optimization to get sigma
  res <- optim(par = param_init, fn = get_GMM_obj, method = "Nelder-Mead")
  sigma <- res$par
  
  # Get beta via the FOCs
  delta <- get_delta(sigma)
  temp <- t(X) %*% Z %*% W %*% t(Z)
  beta <- solve(temp %*% X) %*% temp %*% delta
  
  return(list(beta = beta, sigma = sigma))
}
