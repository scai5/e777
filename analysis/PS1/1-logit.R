# About ------------------------------------------------------------------------

# Logit and nested logit
# ECON 777 Problem Set 1
# Author:         Shirley Cai 
# Date created:   03/04/2024 
# Last edited:    05/01/2024

# Import helpers ---------------------------------------------------------------

source("analysis/support/elas_helpers.R")

# Basic logit ------------------------------------------------------------------

logit.ols <- feols(mean_util_indv ~ av + hmo + price_pp | rating_area + year, 
                   data = mkt.df)
logit.iv1 <- feols(mean_util_indv ~ av + hmo | rating_area + year | price_pp ~ hausman_pp, 
                   data = mkt.df)
logit.iv2 <- feols(mean_util_indv ~ as.factor(insurer) + av + hmo | rating_area + year | price_pp ~ hausman_pp, 
                   data = mkt.df)

# Nested logit -----------------------------------------------------------------

# Nests = metal tiers

nested.ols <- feols(mean_util_indv ~ av + hmo + price_pp + log(nest_share_indv)| rating_area + year, 
                   data = mkt.df)
nested.iv1 <- feols(mean_util_indv ~ av + hmo + log(nest_share_indv)| rating_area + year | price_pp ~ hausman_pp, 
                   data = mkt.df)
nested.iv2 <- feols(mean_util_indv ~ as.factor(insurer) + av + hmo + log(nest_share_indv)| rating_area + year | price_pp ~ hausman_pp, 
                   data = mkt.df)

# Calculate price elasticities -------------------------------------------------

alpha <- c(coef(logit.ols)["price_pp"], coef(logit.iv1)["fit_price_pp"], coef(logit.iv2)["fit_price_pp"], 
           coef(nested.ols)["price_pp"], coef(nested.iv1)["fit_price_pp"], coef(nested.iv2)["fit_price_pp"])
sigma <- c(coef(nested.ols)["log(nest_share_indv)"], coef(nested.iv1)["log(nest_share_indv)"], coef(nested.iv2)["log(nest_share_indv)"])

Dmarket <- sapply(mkt.df$rating_area, FUN = is_equal, mkt.df$rating_area)
Dyear <- sapply(mkt.df$year, FUN = is_equal, mkt.df$year)
Dmktyr <- Dmarket * Dyear
rm(list = c("Dmarket", "Dyear"))
colnames(Dmktyr) <- NULL

Dnest <- sapply(mkt.df$metal_level, FUN = is_equal, mkt.df$metal_level)
diag(Dnest) <- 0
colnames(Dnest) <- NULL

# Elasticities 
elas_mnl <- lapply(alpha[1:3], FUN = compute_mnl_elas, 
                   share = mkt.df$mkt_share_indv, price = mkt.df$price_pp, Dmarket = Dmktyr)
elas_mnl <- matrix(unlist(elas_mnl), ncol = 3, byrow = FALSE)
elas_nested <- mapply(FUN = compute_nested_elas, alpha[4:6], sigma, 
                      MoreArgs = list(share = mkt.df$mkt_share_indv, nest_share = mkt.df$nest_share_indv, 
                                      price = mkt.df$price_pp, Dmarket = Dmktyr, Dnest = Dnest))

# Format results table ---------------------------------------------------------

models <- list("OLS_1 " = logit.ols, "IV_1" = logit.iv1, "IV_1_brand" = logit.iv2, 
               "OLS_2" = nested.ols, "IV_2" = nested.iv1, "IV_2_brand" = nested.iv2)

gof_map <- tribble(
  ~raw,          ~clean,          ~fmt,     ~omit,
  "nobs",        "Observations",     0,     FALSE, 
  "r.squared",   "R2",               3,     FALSE
)

# Used to add price elasticities to table
elas_mnl <- rbind(elas_mnl, matrix(NA, nrow = 2, ncol = 3))
elas_nested <- rbind(elas_nested, matrix(NA, nrow = 1, ncol = 3))[c(1,4,2,3), ]
elas <- cbind(elas_mnl, elas_nested)

rows <- as_tibble(as_tibble(elas), .name_repair = "unique") %>% 
  add_column(c("Price elasticity", "Cross-price elasticity", "Cross-price within nest", "Cross-price outside nest"), .before = 1)
colnames(rows) <- c("term", "OLS_1", "IV_1", "IV_1_brand", "OLS_2", "IV_2", "IV_2_brand")

## Full table ------------------------------------------------------------------

coef_map <- c("price_pp" = "Premium", "fit_price_pp" = "Premium", 
              "av" = "AV", "hmo" = "HMO", 
              "as.factor(insurer)Blue_Shield" = "Blue Shield", 
              "as.factor(insurer)Health_Net" = "Health Net", 
              "as.factor(insurer)Kaiser" = "Kaiser", 
              "as.factor(insurer)Small_Insurer" = "Small insurer", 
              "log(nest_share_indv)" = "1-λ")
attr(rows, 'position') <- 17:20

tab <- modelsummary(models, 
                    stars = c('*' = .1, '**' = .05, '***' = 0.01),
                    gof_map = gof_map, coef_map = coef_map,
                    add_rows = rows, 
                    output = "gt")

tab <- tab %>% 
  tab_source_note(
    source_note = "SEs are clustered at the market level. All models include year and market fixed effects. 
                   Market shares are calculated as share of individuals enrolled in a plan in a given market and year. 
                   Premium is calculated as subsidized premium and is adjusted for the outside option using the penalty. 
                   Nests are defined as metal levels. The omitted insurer is Anthem."
  ) %>%
  tab_spanner(label = "Logit", columns = 2:4) %>% 
  tab_spanner(label = "Nested logit", columns = 5:7) %>% 
  cols_label(starts_with("OLS") ~ "OLS", starts_with("IV") ~ "IV") %>%
  opt_horizontal_padding(scale = 3)

## Short table -----------------------------------------------------------------

rows_short <- rows %>% 
  mutate(across(OLS_1:IV_2_brand, function(x) as.character(round(x,3))))

row_short <- rows_short %>% 
  add_row(term = "Insurer dummies?", OLS_1 = "No", IV_1 = "No", IV_1_brand = "Yes", 
          OLS_2 = "No", IV_2 = "No", IV_2_brand = "Yes")

coef_map <- c("price_pp" = "Premium", "fit_price_pp" = "Premium", 
              "log(nest_share_indv)" = "1-λ")
attr(rows_short, 'position') <- 5:9

tab_short <- modelsummary(models, 
                    stars = c('*' = .1, '**' = .05, '***' = 0.01),
                    gof_map = gof_map, coef_map = coef_map,
                    add_rows = rows_short, 
                    output = "gt")

tab_short <- tab_short %>% 
  tab_source_note(
    source_note = "SEs are clustered at the market level. All models include plan characteristics (AV, HMO) and year and market fixed effects. 
                   Market shares are calculated as share of individuals enrolled in a plan in a given market and year. 
                   Premium is calculated as subsidized premium and is adjusted for the outside option using the penalty. 
                   Nests are defined as metal levels. The omitted insurer is Anthem."
  ) %>%
  tab_spanner(label = "Logit", columns = 2:4) %>% 
  tab_spanner(label = "Nested logit", columns = 5:7) %>% 
  cols_label(starts_with("OLS") ~ "OLS", starts_with("IV") ~ "IV") %>%
  sub_missing(missing_text = "") %>% 
  opt_horizontal_padding(scale = 3)

# Export results ---------------------------------------------------------------

# Export tables 
gtsave(tab, "results/berry-inv.html")
gtsave(tab, "results/berry-inv.tex")
gtsave(tab_short, "results/berry-inv-short.html")
gtsave(tab_short, "results/berry-inv-short.tex")

# Save logit for simulation use
saveRDS(logit.iv1, "data/output/logit.rds")

rm(list = setdiff(ls(), "mkt.df"))
