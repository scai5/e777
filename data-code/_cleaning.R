# About ------------------------------------------------------------------------

# Data cleaning 
# ECON 777 Problem Set 1
# Author:         Shirley Cai 
# Date created:   02/28/2024 
# Last edited:    02/29/2024 
# Last run:       02/29/2024

# Preliminary ------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse)

# Import raw data --------------------------------------------------------------

household.df <- read_csv("data/input/households777.csv") %>% 
  select(!`...1`)
household_plan.df <- read_csv("data/input/household_plan_year777.csv") %>% 
  select(!`...1`)
plan.df <- read_csv("data/input/plans777.csv") %>% 
  select(!`...1`)

# Clean and merge household level data -----------------------------------------

household.df <- household.df %>% 
  left_join(plan.df, by = c('choice' = 'plan_name')) %>% 
  left_join(household_plan.df, by = c('household_id' = 'household_id',
                                      'year' = 'year', 
                                      'choice' = 'plan_name'))

household.df <- household.df %>% 
  mutate(
    sub_premium = pmax(0, premium - subsidy),          # Subsidized premium >= 0
    price = sub_premium - monthly_penalty, 
    sub_premium_pp = sub_premium / household_size, 
    price_pp = price / household_size
  ) 

# Aggregate to market-level data -----------------------------------------------

market.df <- household.df %>% 
  group_by(rating_area, year, choice) %>% 
  summarise(
    n_indv = sum(household_size), 
    n_household = n(),
    avg_sub_premium = mean(sub_premium), 
    avg_price = mean(price), 
    avg_sub_premium_pp = mean(sub_premium_pp), 
    avg_price_pp = mean(price_pp)
    # TODO: Add instrument
  ) 

market.df <- market.df %>% 
  group_by(rating_area, year) %>% 
  mutate(
    mkt_share_indv = n_indv / sum(n_indv),
    mkt_share_household = n_household / sum(n_household)
  )

# Export -----------------------------------------------------------------------

write_tsv(household.df, "data/output/indv_data.txt")
write_tsv(market.df, "data/output/mkt_data.txt")
rm(list = ls())
