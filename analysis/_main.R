# About ------------------------------------------------------------------------

# Master script
# ECON 777 Problem Set 1
# Author:         Shirley Cai 
# Date created:   02/29/2024 
# Last edited:    02/29/2024 
# Last run:       ---

# Preliminary ------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse)

# Clean data -------------------------------------------------------------------

source("data-code/_cleaning.R")
indv.df <- read_tsv("data/output/indv_data.txt")
mkt.df <- read_tsv("data/output/mkt_data.txt")

# Demand estimation ------------------------------------------------------------ 


# Results and counterfactuals --------------------------------------------------