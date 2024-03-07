# About ------------------------------------------------------------------------

# 2a. Berry inversion / logit and nested logit
# ECON 777 Problem Set 1
# Author:         Shirley Cai 
# Date created:   03/04/2024 
# Last edited:    03/07/2024

# TODO: Add average market demographics

# Basic logit ------------------------------------------------------------------

logit.ols <- lm(mean_util_indv ~ av + hmo + avg_price_pp, 
                data = mkt.df)
logit.iv1 <- ivreg(mean_util_indv ~ av + hmo | avg_price_pp | hausman_pp,
                   data = mkt.df)
logit.iv2 <- ivreg(mean_util_indv ~ as.factor(insurer) + av + hmo | avg_price_pp | hausman_pp,
                  data = mkt.df)

# Nested logit -----------------------------------------------------------------

# Nests = metal tiers

nested.ols <- lm(mean_util_indv ~ av + hmo + avg_price_pp + log(nest_share_indv), 
                data = mkt.df)
nested.iv1 <- ivreg(mean_util_indv ~ av + hmo + log(nest_share_indv) | avg_price_pp | hausman_pp,
                   data = mkt.df)
nested.iv2 <- ivreg(mean_util_indv ~ as.factor(insurer) + av + hmo + log(nest_share_indv) | avg_price_pp | hausman_pp,
                  data = mkt.df)

# Format results table ---------------------------------------------------------

models <- list("OLS 1 " = logit.ols, "IV 1 " = logit.iv1, "IV 1 brand" = logit.iv2, 
               "OLS 2" = nested.ols, "IV 2" = nested.iv1, "IV 2 brand" = nested.iv2)

varnames <- c("AV", "HMO", "Premium",
              "Blue Shield", "Health Net", "Kaiser", "Small insurer", 
               "1-λ")

gof_map <- tribble(
  ~raw,      ~clean,          ~fmt,  ~omit,
  "nobs",      "Observations",     0,  FALSE
)

tab <- modelsummary(models,
                    stars = c('*' = .1, '**' = .05, '***' = 0.01),
                    coef_omit = "(Intercept)",
                    coef_rename = varnames,
                    statistic = NULL,
                    gof_map = gof_map,
                    output = "gt")

# TODO: Clean up table
tab <- tab %>% 
  tab_source_note(
    source_note = "Market shares are calculated as share of individuals enrolled in a plan in a given market and year. 
                   Premium is calculated as subsidized premium and is adjusted for the outside option using the penalty. 
                   Nests are defined as metal levels. The omitted insurer is Anthem."
  ) %>%
  tab_spanner(label = "Logit", columns = 2:4) %>% 
  tab_spanner(label = "Nested logit", columns = 5:7) %>% 
  cols_label(starts_with("OLS") ~ "OLS", starts_with("IV") ~ "IV") %>%
  opt_horizontal_padding(scale = 3)

# Export results ---------------------------------------------------------------

# TODO: Export as latex
gtsave(tab, "results/berry_inv.html")

rm(list = setdiff(ls(), "mkt.df"))
