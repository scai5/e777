# About ------------------------------------------------------------------------

# 2b. BLP
# ECON 777 Problem Set 1
# Author:         Shirley Cai 
# Date created:   03/04/2024 
# Last edited:    03/18/2024 

# Changing data for TESTING ONLY        # TODO: Remove -------------------------

rm(list=ls())
mkt.df <- read_tsv("data/temp/mkt_subset.txt")

# Format data as matrices ------------------------------------------------------

actual_share <- mkt.df$mkt_share_indv
plan_char <- as.matrix(mkt.df %>% dplyr::select(avg_price_pp, av, hmo))

# TODO: Is there anything else that goes into Z? 
Z <- mkt.df$hausman_pp

# Initial param values = (a, b, ..., sigma_a, sigma_b, ...)
theta_init <- c(rep(0, dim(plan_char)[2]), rep(1, dim(plan_char)[2]))

# Helper functions -------------------------------------------------------------

source("analysis/support/gmm_helpers.R")

# TODO: create GMM wrapper that asks if you want two-stage 
#' Runs the GMM routine via Nelder-Mead and returns a vector of converged parameters
#' 
#' @param theta_init A vector theta = (beta, sigma)
#' @param efficient A bool = TRUE if the function should run efficient GMM
#' @returns A vector theta 
GMM_optimize <- function(theta_init, efficient = TRUE){
  # GMM first step: Use W = 1
  res <- optim(par = theta_init, fn = get_GMM_obj, 
               method = "Nelder-Mead", control = list(maxit = 5), 
               plan_char = plan_char, W = 1)
  theta <- res$par
  
  # TODO: It's not working. I broke it :(
  
  if(!efficient) return(theta)
  
  # TODO: Figure out how to do global environment variables to keep xi from first run
  
  # Calculate consistent estimate for W 
  W <- solve((t(Z) %*% xi)^2)
  
  # GMM second step: Use consistent estimate for W
  res <- optim(theta_init, get_GMM_obj, 
               method = "Nelder-Mead", 
                  plan_char = plan_char, W = W)
  theta <- res$par
  
  return(theta)
}


# Run optimization routine -----------------------------------------------------

set.seed(1234)

