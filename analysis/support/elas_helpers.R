# About ------------------------------------------------------------------------

# Elasticity helper functions 
# ECON 777 Problem Set 1
# Author:         Shirley Cai 
# Date created:   03/05/2024 
# Last edited:    05/07/2024

# Functions --------------------------------------------------------------------

#' Returns whether a equals b
#' 
#' @param a 
#' @param b 
#' @returns A boolean 
is_equal <- function(a, b) {
  return(a == b)
}

#' Returns a matrix of logit partials (ds/dp)
#' 
#' @param alpha A scalar of estimated price parameter
#' @param share A vector of market shares 
#' @returns A matrix 
get_mnl_partials <- function(alpha, share){
  partials <- -alpha * tcrossprod(share, share) 
  diag(partials) <- alpha * share * (1 - share) 
  return(partials)
}

#' Calculates price elasticities from a logit price parameter. 
#' 
#' @param alpha A scalar of estimated price parameter
#' @param share A vector of market shares
#' @param price A vector of prices 
#' @param Dmarket A matrix that indicates which observations are in the same market
#' @returns A vector of own-price and cross-price elasticities. Own-price elasticity is returned as a positive value. 
compute_mnl_elas <- function(alpha, share, price, Dmarket){
  # Compute elasticity matrix 
  partials <- get_mnl_partials(alpha, share) * Dmarket
  elas_mat <- t(tcrossprod(price, 1/share)) * partials
  
  # Own-price elasticity 
  own_elas <- mean(diag(elas_mat), na.rm = TRUE)
  
  # Cross-price elasticity 
  diag(elas_mat) <- 0
  cross_elas <- sum(elas_mat) / sum(elas_mat > 0)
  
  return(c(-own_elas, cross_elas))
}

#' Calculates price elasticities from a nested logit price parameter. See http://www.econ.ucla.edu/ackerber/acnew5.pdf for derivations.
#' 
#' @param alpha A scalar that is the estimated price parameter
#' @param sigma A scalar that is the estimated nesting parameter
#' @param share A vector of market shares
#' @param nest_share A vector within nest shares
#' @param price A vector of prices 
#' @param Dmarket A matrix that indicates which observations are in the same market
#' @param Dnest A matrix that indicates which observations are in the same nest, excluding itself
#' @returns A vector of own-price elasticity and cross-price elasticities within and outside of nest. Own-price elasticity is returned as a positive value. 
compute_nested_elas <- function(alpha, sigma, share, nest_share, price, Dmarket, Dnest){
  Ddiff <- abs(1-Dnest)
  
  # Compute elasticity matrix 
  partials_same <- -alpha * (sigma/(1-sigma) * nest_share + share) %*% t(share) 
  partials_diff <- -alpha * tcrossprod(share, share) 
  partials <- (partials_same * Dnest + partials_diff * Ddiff) * Dmarket
  diag(partials) <- alpha * share * (1/(1-sigma) - sigma/(1-sigma)*nest_share - share) 
  elas_mat <- t(tcrossprod(price, 1/share)) * partials
  
  # Own-price elasticity 
  own_elas <- mean(diag(elas_mat), na.rm = TRUE)
  
  # Cross-price elasticity within nest 
  elas_mat_same <- elas_mat * Dnest
  diag(elas_mat_same) <- 0
  cross_elas_same <- sum(elas_mat_same) / sum(elas_mat_same > 0)
  
  # Cross-price elasticity outside of nest 
  elas_mat_diff <- elas_mat * Ddiff
  diag(elas_mat_diff) <- 0
  cross_elas_diff <- sum(elas_mat_diff) / sum(elas_mat_diff > 0)
  
  return(c(-own_elas, cross_elas_same, cross_elas_diff))
}

