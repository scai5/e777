# About ------------------------------------------------------------------------

# Master script
# ECON 777 Problem Set 1
# Author:         Shirley Cai 
# Date created:   02/29/2024 
# Last edited:    03/05/2024 
# Last run:       ---

# Preliminary ------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ivreg, gt, gtsummary, modelsummary)

# Clean data -------------------------------------------------------------------

source("data-code/_cleaning.R")
mkt.df <- read_tsv("data/output/mkt_data.txt")

# Demand estimation ------------------------------------------------------------ 

source("analysis/2a-berry-inversion.R")
# source("analysis/2b-BLP.R)
# TODO: Age sensitivity

# Results and counterfactuals --------------------------------------------------

# TODO: Calculate elasticities 
# TODO: Enrollment descriptives