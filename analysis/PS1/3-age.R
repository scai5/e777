# About ------------------------------------------------------------------------

# Logit with age sensitivity
# ECON 777 Problem Set 1
# Author:         Shirley Cai 
# Date created:   03/04/2024 
# Last edited:    05/07/2024

# Import helpers ---------------------------------------------------------------

source("analysis/support/elas_helpers.R")
source("analysis/support/sim_helpers.R")

# Logit and price elasticities -------------------------------------------------

logit <- feols(mean_util_indv ~ av + hmo | rating_area + year 
               | price_pp + price_pp:perc_0to17 + price_pp:perc_18to25
               + price_pp:perc_26to34 + price_pp:perc_35to44 + price_pp:perc_45to54
               + price_pp:perc_55to64 + price_pp:perc_65plus
               ~ hausman_pp + hausman_pp:perc_0to17 + hausman_pp:perc_18to25
               + hausman_pp:perc_26to34 + hausman_pp:perc_35to44 + hausman_pp:perc_45to54
               + hausman_pp:perc_55to64 + hausman_pp:perc_65plus,
               data = mkt.df)

# Calculate effective price parameter per group
alpha <- coef(logit)["fit_price_pp"]
psi <- coef(logit)[2:8]
avg.perc <- mkt.df %>% dplyr::select(matches("^perc_[0-9]")) %>% summarize_all(mean)
avg.perc <- as.numeric(avg.perc[1,])
price.param <- alpha + psi*avg.perc

Dmarket <- sapply(mkt.df$rating_area, FUN = is_equal, mkt.df$rating_area)
Dyear <- sapply(mkt.df$year, FUN = is_equal, mkt.df$year)
Dmktyr <- Dmarket * Dyear
rm(list = c("Dmarket", "Dyear"))

Dnest <- sapply(mkt.df$metal_level, FUN = is_equal, mkt.df$metal_level)
diag(Dnest) <- 0

# Elasticities 
elas_mnl <- compute_mnl_elas(alpha, share = mkt.df$mkt_share_indv, price = mkt.df$price_pp, Dmarket = Dmktyr)
elas_mnl <- matrix(unlist(elas_mnl), ncol = 2, byrow = FALSE)
elas_age <- lapply(price.param, FUN = compute_mnl_elas, 
                   share = mkt.df$mkt_share_indv, price = mkt.df$price_pp, Dmarket = Dmktyr)
elas_age <- matrix(unlist(elas_age), ncol = 2, byrow = TRUE)

rm(list = c("Dmktyr", "Dnest"))

# Penalty removal simulation ---------------------------------------------------

sim.df <- read_tsv("data/output/sim_data.txt") %>% 
  filter(year == 2018) 

orig_price <- sim.df$price_pp
new_penalty <- rep(0, nrow(sim.df))

sim.df <- sim.df %>% 
  mutate(
    new_price_pp = ((pmax(0, mon_premium - subsidy) - new_penalty) * 12) / household_size, 
    insurer = ifelse(is.na(insurer), "Uninsured", insurer)
  )

# Simulate new choices 
sim_choice.df <- predict_choice(sim.df, logit, sim.df$new_price_pp)
sim_choice.df <- sim_choice.df %>% 
  mutate(exchange = ifelse(choice == "Uninsured", 0, 1), 
         sim_exchange = ifelse(sim_choice == "Uninsured", 0, 1))

# Format results tables --------------------------------------------------------

## Logit results ---------------------------------------------------------------

models <- list("IV" = logit)

gof_map <- tribble(
  ~raw,          ~clean,          ~fmt,     ~omit,
  "nobs",        "Observations",     0,     FALSE, 
  "r.squared",   "R2",               3,     FALSE
)

### Full table -----------------------------------------------------------------

coef_map <- c("fit_price_pp" = "Premium", 
              "fit_price_pp:perc_0to17" = "Premium * Percent 0-17", 
              "fit_price_pp:perc_18to25" = "Premium * Percent 18-25",
              "fit_price_pp:perc_26to34" = "Premium * Percent 26-34",
              "fit_price_pp:perc_35to44" = "Premium * Percent 35-44",
              "fit_price_pp:perc_45to54" = "Premium * Percent 45-54",
              "fit_price_pp:perc_55to64" = "Premium * Percent 55-64",
              "fit_price_pp:perc_65plus" = "Premium * Percent 65+",
              "av" = "AV", "hmo" = "HMO")

tab <- modelsummary(models, 
                    stars = c('*' = .1, '**' = .05, '***' = 0.01),
                    gof_map = gof_map, coef_map = coef_map,
                    output = "gt")

tab <- tab %>% 
  tab_source_note(
    source_note = "SEs are clustered at the market level. All models include year and market fixed effects. 
                   Market shares are calculated as share of individuals enrolled in a plan in a given market and year. 
                   Premium is calculated as subsidized premium and is adjusted for the outside option using the penalty."
  ) %>%
  cols_label(starts_with("IV") ~ "IV") %>%
  opt_horizontal_padding(scale = 3)

### Short table ----------------------------------------------------------------

coef_map <- c("fit_price_pp" = "Premium", 
              "fit_price_pp:perc_0to17" = "Premium * Percent 0-17", 
              "fit_price_pp:perc_18to25" = "Premium * Percent 18-25",
              "fit_price_pp:perc_26to34" = "Premium * Percent 26-34",
              "fit_price_pp:perc_35to44" = "Premium * Percent 35-44",
              "fit_price_pp:perc_45to54" = "Premium * Percent 45-54",
              "fit_price_pp:perc_55to64" = "Premium * Percent 55-64",
              "fit_price_pp:perc_65plus" = "Premium * Percent 65+")

tab_short <- modelsummary(models, 
                          stars = c('*' = .1, '**' = .05, '***' = 0.01),
                          gof_map = gof_map, coef_map = coef_map,
                          output = "gt")

tab_short <- tab_short %>% 
  tab_source_note(
    source_note = "SEs are clustered at the market level. All models include plan characteristics (AV, HMO) and year and market fixed effects. 
                   Market shares are calculated as share of individuals enrolled in a plan in a given market and year. 
                   Premium is calculated as subsidized premium and is adjusted for the outside option using the penalty."
  ) %>%
  cols_label(starts_with("IV") ~ "IV") %>%
  opt_horizontal_padding(scale = 3)

### Elasticity table -----------------------------------------------------------

elas <- as_tibble(as_tibble(rbind(elas_mnl, elas_age)), .name_repair = "unique") %>% 
  add_column(c("Overall", "0-17", "18-25", "26-34", "35-44", "45-54", "55-64", "65+"), 
             .before = 1)
colnames(elas) <- c("Group", "Own-price", "Cross-price")

tab_elas <- gt(elas) %>% 
  tab_row_group(
    label = "By age group", 
    rows = 2:8
  ) %>%
  row_group_order(groups = c(NA, "By age group")) %>% 
  fmt_number( 
    decimals = 3
  ) %>% 
  opt_horizontal_padding(scale = 3)

## Simulation results ----------------------------------------------------------

n_household <- nrow(sim_choice.df)
n_indv <- sum(sim_choice.df$household_size)

plan.df <- read_csv("data/input/plans777.csv") %>% 
  dplyr::select(!`...1`) %>% 
  rename_with(tolower) %>% 
  dplyr::select(plan_name, insurer, metal_level)

sim_choice.df <- sim_choice.df %>% 
  rename(sim_insurer = insurer, sim_metal = metal_level) %>% 
  left_join(plan.df, by = c("choice" = "plan_name"))

# Total enrollment 
enroll_tot <- sim_choice.df %>% 
  ungroup() %>% 
  summarize(
    actual = sum(household_size * exchange), 
    simulated = sum(household_size * sim_exchange)
  ) %>% 
  add_column(group = c("Total"), .before = 1)

# Enrollment over the 4 metal tiers 
metal_actual <- sim_choice.df %>% 
  filter(!is.na(metal_level)) %>% 
  group_by(metal_level) %>% 
  summarize(
    actual = sum(household_size)
  )
metal_sim <- sim_choice.df %>% 
  filter(!is.na(sim_metal)) %>% 
  group_by(sim_metal) %>% 
  summarize(
    simulated = sum(household_size)
  )
enroll_metal <- metal_actual %>% 
  left_join(metal_sim, by = c("metal_level" = "sim_metal")) %>% 
  rename(group = metal_level)
rm(list = c("metal_actual", "metal_sim"))

# Enrollment by age group 
enroll_0to17 <- sim_choice.df %>% 
  ungroup() %>% 
  summarize(
    actual = sum(household_size * perc_0to17* exchange), 
    simulated = sum(household_size * perc_0to17 * sim_exchange)
  ) %>% 
  add_column(group = c("0-17"), .before = 1)
enroll_18to25 <- sim_choice.df %>% 
  ungroup() %>% 
  summarize(
    actual = sum(household_size * perc_18to25* exchange), 
    simulated = sum(household_size * perc_18to25 * sim_exchange)
  ) %>% 
  add_column(group = c("18-25"), .before = 1)
enroll_26to34 <- sim_choice.df %>% 
  ungroup() %>% 
  summarize(
    actual = sum(household_size * perc_26to34* exchange), 
    simulated = sum(household_size * perc_26to34 * sim_exchange)
  ) %>% 
  add_column(group = c("26-34"), .before = 1)
enroll_35to44 <- sim_choice.df %>% 
  ungroup() %>% 
  summarize(
    actual = sum(household_size * perc_35to44* exchange), 
    simulated = sum(household_size * perc_35to44 * sim_exchange)
  ) %>% 
  add_column(group = c("35-44"), .before = 1)
enroll_45to54 <- sim_choice.df %>% 
  ungroup() %>% 
  summarize(
    actual = sum(household_size * perc_45to54* exchange), 
    simulated = sum(household_size * perc_45to54 * sim_exchange)
  ) %>% 
  add_column(group = c("45-54"), .before = 1)
enroll_55to64 <- sim_choice.df %>% 
  ungroup() %>% 
  summarize(
    actual = sum(household_size * perc_55to64* exchange), 
    simulated = sum(household_size * perc_55to64 * sim_exchange)
  ) %>% 
  add_column(group = c("55-64"), .before = 1)
enroll_65plus <- sim_choice.df %>% 
  ungroup() %>% 
  summarize(
    actual = sum(household_size * perc_65plus* exchange), 
    simulated = sum(household_size * perc_65plus * sim_exchange)
  ) %>% 
  add_column(group = c("65+"), .before = 1)
enroll_age <- rbind(enroll_0to17, enroll_18to25, enroll_26to34, enroll_35to44, enroll_45to54, enroll_55to64, enroll_65plus)
rm(list = c("enroll_0to17", "enroll_18to25", "enroll_26to34", "enroll_35to44", "enroll_45to54", "enroll_55to64", "enroll_65plus"))

# Enrollment by firm 
firm_actual <- sim_choice.df %>% 
  filter(!is.na(insurer)) %>%
  group_by(insurer) %>%
  summarize(
    actual = sum(household_size)
  )
firm_sim <- sim_choice.df %>% 
  filter(!is.na(sim_insurer)) %>% 
  group_by(sim_insurer) %>% 
  summarize(
    simulated = sum(household_size)
  )
enroll_firm <- firm_actual %>% 
  left_join(firm_sim, by = c("insurer" = "sim_insurer")) %>% 
  rename(group = insurer)
rm(list = c("firm_actual", "firm_sim"))

# Collapse data frames into one 
enroll_all <- rbind(enroll_tot, enroll_metal, enroll_age, enroll_firm)

tab_enroll <- gt(enroll_all) %>% 
  tab_row_group(
    label = "By firm", 
    rows = 14:18
  ) %>% 
  tab_row_group(
    label = "By age group", 
    rows = 7:13
  ) %>% 
  tab_row_group(
    label = "By metal tier", 
    rows = 2:6
  ) %>% 
  row_group_order(groups = c(NA, "By metal tier", "By age group", "By firm")) %>%
  text_case_match(
    "Minimum Coverage" ~ "Minimum coverage", 
    "Blue_Shield" ~ "Blue Shield", 
    "Health_Net" ~ "Health Net", 
    "Small_Insurer" ~ "Small insurer"
  ) %>% 
  cols_label(
    group = "Group", 
    actual = "Actual", 
    simulated = "Simulated"
  ) %>% 
  tab_spanner(label = "Enrollment", columns = 2:3) %>% 
  tab_source_note(
    source_note = "Enrollment calculated by individuals. There are a total of 101,275 individuals in the market."
  ) %>%
  fmt_number(decimals = 0) %>% 
  opt_horizontal_padding(scale = 3)

# Export results ---------------------------------------------------------------

# Regression results 
gtsave(tab, "results/age.html")
gtsave(tab, "results/age.tex")
gtsave(tab_short, "results/age-short.html")
gtsave(tab_short, "results/age-short.tex")

# Elasticity table 
gtsave(tab_elas, "results/age-elas.html")
gtsave(tab_elas, "results/age-elas.tex")

# Simulation results
gtsave(tab_enroll, "results/sim-enroll.html")
gtsave(tab_enroll, "results/sim-enroll.tex")

rm(list = setdiff(ls(), "mkt.df"))
