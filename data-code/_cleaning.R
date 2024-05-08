# About ------------------------------------------------------------------------

# Data cleaning 
# ECON 777 Problem Set 1
# Author:         Shirley Cai 
# Date created:   02/28/2024 
# Last edited:    05/07/2024 

# Import raw data --------------------------------------------------------------

household.df <- read_csv("data/input/households777.csv") %>% 
  dplyr::select(!`...1`) %>% 
  rename_with(tolower)
household_plan.df <- read_csv("data/input/household_plan_year777.csv") %>% 
  dplyr::select(!`...1`) %>% 
  rename_with(tolower)
plan.df <- read_csv("data/input/plans777.csv") %>% 
  dplyr::select(!`...1`) %>% 
  rename_with(tolower)

# Clean and merge household plan data for simulation ---------------------------

sim.df <- household_plan.df %>% 
  left_join(household.df, by = c('household_id' = 'household_id',
                                 'year' = 'year')) %>% 
  left_join(plan.df, by = c('plan_name' = 'plan_name'))

sim.df <- sim.df %>% 
  mutate(
    av = ifelse(is.na(av), 0, av),
    hmo = ifelse(is.na(hmo), 0, hmo), 
    mon_premium = premium, 
    mon_sub_premium = pmax(0, mon_premium - subsidy),  # Subsidized premium >= 0
    mon_price = mon_sub_premium - monthly_penalty, 
    premium = mon_premium * 12, 
    sub_premium = mon_sub_premium * 12, 
    price = mon_price * 12, 
    premium_pp = premium / household_size,
    sub_premium_pp = sub_premium / household_size, 
    price_pp = price / household_size
  ) 

# Clean and merge household level data -----------------------------------------

household.df <- household.df %>% 
  left_join(plan.df, by = c('choice' = 'plan_name')) %>% 
  left_join(household_plan.df, by = c('household_id' = 'household_id',
                                      'year' = 'year', 
                                      'choice' = 'plan_name'))

household.df <- household.df %>% 
  mutate(
    mon_premium = premium, 
    mon_sub_premium = pmax(0, mon_premium - subsidy),  # Subsidized premium >= 0
    mon_price = mon_sub_premium - monthly_penalty, 
    premium = mon_premium * 12, 
    sub_premium = mon_sub_premium * 12, 
    price = mon_price * 12, 
    premium_pp = premium / household_size,
    sub_premium_pp = sub_premium / household_size, 
    price_pp = price / household_size
  ) 

# Aggregate to market-level data -----------------------------------------------

market.df <- household.df %>% 
  group_by(rating_area, year, choice) %>% 
  summarise(
    n_indv = sum(household_size), 
    n_household = n(),
    premium = mean(premium), 
    sub_premium = mean(sub_premium), 
    price = mean(price), 
    premium_pp = mean(premium_pp),
    sub_premium_pp = mean(sub_premium_pp), 
    price_pp = mean(price_pp), 
    fpl = mean(fpl), 
    perc_eng = mean(language == "English", na.rm = TRUE), 
    perc_0to17 = mean(perc_0to17 * household_size) / sum(household_size), 
    perc_18to25 = mean(perc_18to25 * household_size) / sum(household_size), 
    perc_26to34 = mean(perc_26to34 * household_size) / sum(household_size), 
    perc_35to44 = mean(perc_35to44 * household_size) / sum(household_size), 
    perc_45to54 = mean(perc_45to54 * household_size) / sum(household_size), 
    perc_55to64 = mean(perc_55to64 * household_size) / sum(household_size), 
    perc_65plus = mean(perc_65plus * household_size) / sum(household_size), 
    perc_asian = mean(perc_asian * household_size) / sum(household_size),
    perc_black = mean(perc_black * household_size) / sum(household_size),
    perc_hispanic = mean(perc_hispanic * household_size) / sum(household_size),
    perc_white = mean(perc_white * household_size) / sum(household_size),
    perc_male = mean(perc_male * household_size) / sum(household_size)
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
    plan_yr_price = mean(price),
    plan_yr_price_pp = mean(price_pp),
    n_plan_yr = n()
  ) %>% 
  ungroup() %>% 
  mutate(
    hausman = (n_plan_yr*plan_yr_price - price) / (n_plan_yr - 1), 
    hausman_pp = (n_plan_yr*plan_yr_price_pp - price_pp) / (n_plan_yr - 1)
  ) %>% 
  dplyr::select(!c(plan_yr_price, plan_yr_price_pp, n_plan_yr))

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
  ) %>% 
  ungroup()

# Merge Hausman instruments into data for simulation ---------------------------

market.temp <- market.df %>% dplyr::select(hausman, hausman_pp, rating_area, year, plan)
sim.df <- sim.df %>% 
  left_join(market.temp, by = c('rating_area' = 'rating_area',
                                'choice' = 'plan', 'year' = 'year')) 

# Clean up market data and subset for testing ----------------------------------

# Remove small shares and uninsured for market data 
message(paste0("Total market-year observations: ", nrow(market.df)))
message(paste0("Uninsured observations: ", sum(market.df$plan == "Uninsured")))
message(paste0("Small market share observations ", sum(market.df$mkt_share_indv < 0.01)))

market.df <- market.df %>% 
  filter(plan != "Uninsured")

write_tsv(market.df, "data/output/mkt_data_all.txt")

market.df <- market.df %>% 
  filter(mkt_share_indv >= 0.01)

message("Removed uninsured and small market share observations.")

# Create a subset of market data for testing
subset.df <- market.df %>% 
  filter(rating_area <= 3)

# Export -----------------------------------------------------------------------

write_tsv(household.df, "data/output/household_data.txt")
write_tsv(sim.df, "data/output/sim_data.txt")
write_tsv(market.df, "data/output/mkt_data.txt")
write_tsv(subset.df , "data/temp/mkt_subset.txt")
rm(list = ls())
