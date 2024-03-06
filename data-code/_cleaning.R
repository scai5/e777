# About ------------------------------------------------------------------------

# Data cleaning 
# ECON 777 Problem Set 1
# Author:         Shirley Cai 
# Date created:   02/28/2024 
# Last edited:    03/06/2024 

# Import raw data --------------------------------------------------------------

household.df <- read_csv("data/input/households777.csv") %>% 
  select(!`...1`) %>% 
  rename_with(tolower)
household_plan.df <- read_csv("data/input/household_plan_year777.csv") %>% 
  select(!`...1`) %>% 
  rename_with(tolower)
plan.df <- read_csv("data/input/plans777.csv") %>% 
  select(!`...1`) %>% 
  rename_with(tolower)

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
  ) 

# Define market shares and merge in plan characteristics
market.df <- market.df %>% 
  group_by(rating_area, year) %>% 
  mutate(
    mkt_share_indv = n_indv / sum(n_indv),
    mkt_share_household = n_household / sum(n_household)
  ) %>% 
  rename(plan = choice) %>%
  left_join(plan.df, by = c('plan' = 'plan_name'))

# Create Hausman instruments
market.df <- market.df %>% 
  group_by(plan, year) %>% 
  mutate(
    plan_yr_avg_price = mean(avg_price),
    plan_yr_avg_price_pp = mean(avg_price_pp),
    n_plan_yr = n()
  ) %>% 
  ungroup() %>% 
  mutate(
    hausman = (n_plan_yr*plan_yr_avg_price - avg_price) / (n_plan_yr - 1), 
    hausman_pp = (n_plan_yr*plan_yr_avg_price_pp - avg_price) / (n_plan_yr - 1)
  ) %>% 
  select(!c(plan_yr_avg_price, plan_yr_avg_price_pp, n_plan_yr))

# Construct mean utility a la Berry 
market.df <- market.df %>% 
  group_by(rating_area, year) %>% 
  mutate(
    mean_util_indv = log(mkt_share_indv) - log(mkt_share_indv[plan == "Uninsured"]), 
    mean_util_household = log(mkt_share_household) - log(mkt_share_household[plan == "Uninsured"])
  )

# Create nest shares 
market.df <- market.df %>% 
  group_by(rating_area, year, metal_level) %>% 
  mutate(
    nest_share_indv = n_indv / sum(n_indv), 
    nest_share_household = n_household / sum(n_household)
  )

market.df <- market.df %>% 
  filter(plan != "Uninsured")

# Export -----------------------------------------------------------------------

write_tsv(household.df, "data/output/indv_data.txt")
write_tsv(market.df, "data/output/mkt_data.txt")
rm(list = ls())
