# About ------------------------------------------------------------------------

# Master script
# ECON 777 Problem Sets
# Author:         Shirley Cai 
# Date created:   02/29/2024 
# Last edited:    05/08/2024 
# Last run:       05/08/2024

# Preliminary ------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, gt, gtsummary, modelsummary, MASS, boot, fixest, rootSolve)

# Clean data -------------------------------------------------------------------

source("data-code/_cleaning.R")
mkt.df <- read_tsv("data/output/mkt_data.txt")

# Problem set 1 ----------------------------------------------------------------

source("analysis/PS1/1-logit.R")
source("analysis/PS1/2-BLP.R")
source("analysis/PS1/3-age.R")

# Problem set 2 ----------------------------------------------------------------

source("analysis/PS2/1-mandate.R")
source("analysis/PS2/2-merger.R")
