# About ------------------------------------------------------------------------

# Simulation helper functions 
# ECON 777 Problem Set 1
# Author:         Shirley Cai 
# Date created:   04/01/2024 
# Last edited:    05/07/2024

# Import functions -------------------------------------------------------------

source("analysis/support/elas_helpers.R")

# Functions --------------------------------------------------------------------

#' Predicts new consumer plan choice given new prices 
#' 
#' @param sim.df A df that includes all household-level data for simulation
#' @param logit A fixest object that describes the logit demand model 
#' @param new_price A vector of new consumer-facing choices
#' @returns A df of unique households 
predict_choice <- function(sim.df, logit, new_price){
  old_price <- sim.df$price_pp 
  sim.df$price_pp <- new_price
  
  # Predict utilities
  sim.df$sim_util <- predict(logit, newdata = sim.df)
  sim.df <- sim.df %>% 
    mutate(
      choice_prob = inv.logit(sim_util), 
      new_price_pp = new_price, 
      price_pp = old_price
    )
  sim.df$new_price <- new_price 
  sim.df$price_pp <- old_price
  
  # Predict chosen plan 
  sim_choice.df <- sim.df %>% 
    group_by(household_id) %>% 
    slice_max(choice_prob, n = 1, with_ties = TRUE) %>% 
    rename(sim_choice = plan_name)
  set.seed(987)
  sim_choice.df <- sim_choice.df %>% 
    group_by(household_id) %>% 
    slice_sample(n = 1)
  
  return(sim_choice.df)
}

#' Simulates premiums and consumer choices when penalty is changed
#' 
#' @param sim.df A df that includes all household-level data for simulation
#' @param mkt_all.df A df that includes all market data for simulation
#' @param logit A fixest object that describes the logit demand model 
#' @param new_penalty A vector of new penalties 
#' @returns A list including a df of simulated choices and a df of simulated premiums
sim_penalty <- function(sim.df, mkt_all.df, logit, new_penalty){
  alpha <- coef(logit)["fit_price_pp"]
  
  # Predict new consumer choice given new penalty
  sim.df <- sim.df %>% 
    mutate(
      new_price_pp = ((pmax(0, mon_premium - subsidy) - new_penalty) * 12) / household_size, 
      insurer = ifelse(is.na(insurer), "Uninsured", insurer)
    )
  sim_choice.df <- predict_choice(sim.df, logit, sim.df$new_price_pp)
  
  # Calculate new market shares 
  sim_mkt.df <- sim_choice.df %>% 
    group_by(rating_area, sim_choice) %>% 
    summarise(n_indv = sum(household_size))
  sim_mkt.df <- sim_mkt.df %>% 
    group_by(rating_area) %>% 
    mutate(sim_share = n_indv / sum(n_indv)) %>% 
    dplyr::select(!n_indv)
  
  # Calculate new ownership matrix 
  sim_mkt.df <- sim_mkt.df %>% 
    left_join(mkt_all.df, by = c('rating_area' = 'rating_area',
                                  'sim_choice' = 'plan')) %>% 
    filter(sim_choice != "Uninsured", !is.na(insurer))
  sim_Dfirm <- sapply(sim_mkt.df$insurer, FUN = is_equal, sim_mkt.df$insurer)
  sim_D <- sim_Dfirm * get_mnl_partials(alpha, sim_mkt.df$sim_share)
  sim_mkt.df$sim_premium <- (sim_mkt.df$mc + solve(sim_D) %*% sim_mkt.df$sim_share)[,1]
  
  # Return simulated choices and simulated premiums
  return(list(sim_choice.df = sim_choice.df, sim_mkt.df = sim_mkt.df))
}