# About ------------------------------------------------------------------------

# Simulation of the individual mandate 
# ECON 777 Problem Set 2
# Author:         Shirley Cai 
# Date created:   04/01/2024 
# Last edited:    05/08/2024

# Import helpers ---------------------------------------------------------------

source("analysis/support/sim_helpers.R")
mkt_all.df <- read_tsv("data/output/mkt_data_all.txt") %>% 
  filter(year == 2018)

# Import logit 
logit <- readRDS("data/output/logit.rds")
alpha <- coef(logit)["fit_price_pp"]

# Estimate marginal cost -------------------------------------------------------

Dfirm <- sapply(mkt_all.df$insurer, FUN = is_equal, mkt_all.df$insurer)
D <- Dfirm * get_mnl_partials(alpha, mkt_all.df$mkt_share_indv)
mkt_all.df$mc <- (mkt_all.df$premium_pp - solve(D) %*% mkt_all.df$mkt_share_indv)[,1]

# Simulate mandate repeal ------------------------------------------------------

sim.df <- read_tsv("data/output/sim_data.txt") %>% 
  filter(year == 2018) 

orig_price <- sim.df$price_pp
new_penalty <- rep(0, nrow(sim.df))

## Predict new shares and premiums ---------------------------------------------

out <- sim_penalty(sim.df, mkt_all.df, logit, new_penalty)
sim_choice.df <- out$sim_choice.df
sim_mkt.df<- out$sim_mkt.df
rm(out)

## Calculate effects of repeal ------------------------------------------------- 

sim_mkt.df <- sim_mkt.df %>% 
  mutate(perc_change_prem = (sim_premium - premium_pp) / abs(premium_pp) * 100)

sim_choice.df <- sim_choice.df %>% 
  mutate(exchange = ifelse(choice == "Uninsured", 0, 1), 
         sim_exchange = ifelse(sim_choice == "Uninsured", 0, 1))

## Format results --------------------------------------------------------------

# Change in premiums 
premium_perc <- sim_mkt.df %>% 
  ungroup() %>% 
  summarize(perc_change = mean(perc_change_prem)) %>% 
  add_column(effect = c("Premiums"), .before = 1)

# Total enrollment 
enroll_tot <- sim_choice.df %>% 
  ungroup() %>% 
  summarize(
    actual = sum(household_size * exchange), 
    simulated = sum(household_size * sim_exchange)
  ) %>% 
  mutate(perc_change = (simulated - actual) / abs(actual) * 100) %>% 
  dplyr::select(perc_change) %>% 
  add_column(effect = c("Enrollment"), .before = 1)

no_penalty <- rbind(premium_perc, enroll_tot)
rm(list = c("premium_perc", "enroll_tot"))

tab_no <- gt(no_penalty) %>% 
  cols_label(
    effect = "Effect", 
    perc_change = "Change (%)"
  ) %>% 
  fmt_number(decimals = 3) %>% 
  opt_horizontal_padding(scale = 3)

# Simulate a different penalty -------------------------------------------------

temp.df <- sim.df %>% 
  filter(plan_name != "Uninsured") %>% 
  dplyr::select(household_id, premium_pp) %>% 
  group_by(household_id) %>% 
  summarize(
    min_premium = min(premium_pp)
  ) %>% 
  ungroup()

sim.df <- sim.df %>% 
  left_join(temp.df, by = c("household_id" = "household_id"))

## Predict new shares and premiums ---------------------------------------------

out <- sim_penalty(sim.df, mkt_all.df, logit, sim.df$min_premium)
sim_choice.df <- out$sim_choice.df
sim_mkt.df<- out$sim_mkt.df
rm(out)

## Calculate effects of repeal ------------------------------------------------- 

sim_mkt.df <- sim_mkt.df %>% 
  mutate(perc_change_prem = (sim_premium - premium_pp) / abs(premium_pp) * 100)

sim_choice.df <- sim_choice.df %>% 
  mutate(exchange = ifelse(choice == "Uninsured", 0, 1), 
         sim_exchange = ifelse(sim_choice == "Uninsured", 0, 1))

## Format results --------------------------------------------------------------

# Change in premiums 
premium_perc <- sim_mkt.df %>% 
  ungroup() %>% 
  summarize(perc_change = mean(perc_change_prem)) %>% 
  add_column(effect = c("Premiums"), .before = 1)

# Total enrollment 
enroll_tot <- sim_choice.df %>% 
  ungroup() %>% 
  summarize(
    actual = sum(household_size * exchange), 
    simulated = sum(household_size * sim_exchange)
  ) %>% 
  mutate(perc_change = (simulated - actual) / abs(actual) * 100) %>% 
  dplyr::select(perc_change) %>% 
  add_column(effect = c("Enrollment"), .before = 1)

new_penalty <- rbind(premium_perc, enroll_tot)
rm(list = c("premium_perc", "enroll_tot"))

tab_new <- gt(new_penalty) %>% 
  cols_label(
    effect = "Effect", 
    perc_change = "Change (%)"
  ) %>% 
  fmt_number(decimals = 3) %>% 
  opt_horizontal_padding(scale = 3)

# Export results ---------------------------------------------------------------

gtsave(tab_no, "results/sim-no-penalty.html")
gtsave(tab_no, "results/sim-no-penalty.tex")
gtsave(tab_new, "results/sim-new-penalty.html")
gtsave(tab_new, "results/sim-new-penalty.tex")

rm(list = setdiff(ls(), "mkt.df"))