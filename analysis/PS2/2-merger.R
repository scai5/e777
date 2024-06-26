# About ------------------------------------------------------------------------

# Simulation of BCBS merger
# ECON 777 Problem Set 2
# Author:         Shirley Cai 
# Date created:   04/01/2024 
# Last edited:    05/07/2024

# Import helpers ---------------------------------------------------------------

source("analysis/support/sim_helpers.R")

# Import logit 
logit <- readRDS("data/output/logit.rds")
alpha <- coef(logit)["fit_price_pp"]

# Simulate merger --------------------------------------------------------------

mkt_2018.df <- mkt.df %>% filter(year == 2018)

Dfirm <- sapply(mkt_2018.df$insurer, FUN = is_equal, mkt_2018.df$insurer)
D <- Dfirm * get_mnl_partials(alpha, mkt_2018.df$mkt_share_indv)
mkt_2018.df$mc <- (mkt_2018.df$premium_pp - solve(D) %*% mkt_2018.df$mkt_share_indv)[,1]

# Construct new ownership matrix 
mkt_2018.df <- mkt_2018.df %>% 
  mutate(
    sim_insurer = case_when(
      insurer == "Anthem" ~ "BCBS", 
      insurer == "Blue_Shield" ~ "BCBS", 
      TRUE ~ insurer
    )
  ) %>% 
  filter(mkt_share_indv >= 0.01)
  
Dfirm <- sapply(mkt_2018.df$sim_insurer, FUN = is_equal, mkt_2018.df$sim_insurer)
D <- Dfirm * get_mnl_partials(alpha, mkt_2018.df$mkt_share_indv)

# Compute new premiums and mc needed to offset 
mkt_2018.df$sim_premium <- (mkt_2018.df$mc + solve(D) %*% mkt_2018.df$mkt_share_indv)[,1]
mkt_2018.df$sim_mc <- (mkt_2018.df$sim_premium - solve(D) %*% mkt_2018.df$mkt_share_indv)[,1]

# Compute percent change in premiums and mc needed to offset 
mkt_2018.df <- mkt_2018.df %>% 
  mutate(
    perc_change_prem = (sim_premium - premium_pp) / abs(premium_pp) * 100, 
    perc_change_mc = (sim_mc - mc) / abs(mc) * 100
  )

# Calculate results ---------------------------------------------------------------

avg_change <- mkt_2018.df %>% 
  ungroup() %>% 
  summarize(perc_change = mean(perc_change_prem)) %>% 
  add_column(group = "Overall", .before = 1)
avg_change_firm <- mkt_2018.df %>% 
  group_by(sim_insurer) %>% 
  summarize(perc_change = mean(perc_change_prem)) %>% 
  rename(group = sim_insurer)

avg_change <- rbind(avg_change, avg_change_firm)
rm(avg_change_firm)

avg_mc <- mkt_2018.df %>% 
  ungroup() %>% 
  filter(sim_insurer == "BCBS") %>%
  summarize(perc_change = mean(perc_change_mc)) %>% 
  add_column(group = "MC", .before = 1)

avg_change <- rbind(avg_change, avg_mc)

# Format results ---------------------------------------------------------------

tab_change <- gt(avg_change) %>% 
  tab_row_group(
    label = "Change in MC needed to offset", 
    rows = 6
  ) %>%
  tab_row_group(
    label = "By firm", 
    rows = 2:5
  ) %>%
  row_group_order(groups = c(NA, "By firm", "Change in MC needed to offset")) %>% 
  cols_label(
    group = "Group", 
    perc_change = "Change (%)"
  ) %>% 
  fmt_number(decimals = 3) %>% 
  opt_horizontal_padding(scale = 3)

# Export results ---------------------------------------------------------------

gtsave(tab_change, "results/sim-merger.html")
gtsave(tab_change, "results/sim-merger.tex")

rm(list = setdiff(ls(), "mkt.df"))