# About ------------------------------------------------------------------------

# BLP
# ECON 777 Problem Set 1
# Author:         Shirley Cai 
#                 Adapted from Evan Saltzman's BLP code 
# Date created:   03/04/2024 
# Last edited:    05/08/2024 

# Changing data for TESTING ONLY        # TODO: Remove -------------------------

rm(list=ls())
mkt.df <- read_tsv("data/temp/mkt_subset.txt")

# Import helpers ---------------------------------------------------------------

source("analysis/support/elas_helpers.R")
source("analysis/support/gmm_helpers.R")

# Setup ------------------------------------------------------------------------

logit <- lm(mean_util_indv ~ av + hmo + price_pp, data = mkt.df)
logit_coef <- coef(logit)

## Covariate matrices ----------------------------------------------------------

actual_share <- mkt.df$mkt_share_indv
covariates <- c("av", "hmo")

# Covariate and instrument matrices 
X <- as.matrix(cbind(1, mkt.df %>% dplyr::select(all_of(c(covariates, "price_pp")))))
X_rand <- as.matrix(cbind(1, mkt.df %>% dplyr::select(all_of(covariates))))
Z <- as.matrix(cbind(1, mkt.df %>% dplyr::select(all_of(c(covariates, "hausman_pp")))))

# Weight matrix 
W <- solve(crossprod(Z,Z))

# Accounting variables 
n_markets <- length(unique(mkt.df$rating_area))
n_products <- length(unique(mkt.df$plan))
n_total <- nrow(mkt.df)

## Initialize simulated consumers ----------------------------------------------

ns <- 100                                     # Number of simulated draws 
K <- length(covariates) + 1                   # Number of random coefficients

# Initialize simulated draws from iid N(0,1)
nu <- mvrnorm(ns, rep(0, K), diag(rep(1, K)))
nu <- t(nu)

# Matrix indicating which plans are in the same market year 
Dmarket <- sapply(mkt.df$rating_area, FUN = is_equal, mkt.df$rating_area)
Dyear <- sapply(mkt.df$year, FUN = is_equal, mkt.df$year)
Dmktyr <- Dmarket * Dyear
rm(list = c("Dmarket", "Dyear"))

## Initialize parameters to be estimated ---------------------------------------

# Initial parameter values theta = (beta, sigma) 
# By Nevo (2000), we only need to estimate sigma, since beta can be recovered 
# from the FOCs. 
sigma_init <- abs(logit_coef[c("(Intercept)", covariates)]) / 2

# Run optimization routine -----------------------------------------------------

set.seed(1234)
out <- BLP(sigma_init)

beta <- out$beta 
sigma <- out$sigma

# Format results ---------------------------------------------------------------

# TODO

# Export results ---------------------------------------------------------------

# TODO

rm(list = setdiff(ls(), "mkt.df"))

