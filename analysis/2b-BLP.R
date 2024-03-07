# About ------------------------------------------------------------------------

# 2b. BLP
# ECON 777 Problem Set 1
# Author:         Shirley Cai 
# Date created:   03/04/2024 
# Last edited:    03/07/2024 

# Changing data for TESTING ONLY        # TODO: Remove -------------------------

rm(list=ls())
mkt.df <- read_tsv("data/temp/mkt_subset.txt")

# Format data as matrices ------------------------------------------------------

actual_share <- mkt.df$mkt_share_indv
plan_char <- as.matrix(mkt.df %>% dplyr::select(avg_price_pp, av, hmo))
Z <- mkt.df$hausman_pp

# Initial param values = (a, b, ..., sigma_a, sigma_b, ...)
theta_init <- c(rep(0, dim(plan_char)[2]), rep(1, dim(plan_char)[2]))

# Helper functions -------------------------------------------------------------

# TODO: Put these in a source file 
source("analysis/support/gmm_helpers.R")

# TODO: get_GMM_obj 
#' Returns the value of GMM objective function for given values of parameters
#' 
#' @param theta A vector theta = (beta, sigma)
#' @param plan_char A n x k matrix of k plan characteristics
#' @returns A scalar
get_GMM_obj <- function(theta, plan_char, W){
  
  # Split up theta to be more manageable
  theta_bar <- theta_init[1:dim(plan_char)[2]]
  sigma <- theta_init[(dim(plan_char)[2] + 1):length(theta_init)]
  
  xsi <- get_xsi(theta_bar, sigma, plan_char)
  
  # TODO: Compute and return GMM objective 
  
}

# TODO: create GMM wrapper that asks if you want two-stage 
#' Runs the GMM routine via Nelder-Mead and returns a vector of converged parameters
#' 
#' @param theta_init A vector theta = (beta, sigma)
#' @param efficient A bool = TRUE if the function should run efficient GMM
#' @returns A vector theta 
GMM_optimize <- function(theta_init, efficient = TRUE){
  # TODO: gmm with W=1
  
  if(!efficient) return(theta)
  
  # TODO: Calculate consistent estimate for W 
  # TODO: GMM with consistent estimate 
  
  return(theta)
}

# TODO: res <- optim(theta_init, get_GMM_obj, method = "Nelder-Mead")
# TODO: NEED TO USE SOME GLOBAL ENVIRONMENTS BECAUSE OPTIM ONLY ALLOWS SCALAR OUTPUT

# Run optimization routine -----------------------------------------------------

set.seed(1234)

