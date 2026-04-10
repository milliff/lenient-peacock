# --------------------------------------------
#
# Author: Aidan Milliff
# Copyright (c) Aidan Milliff, 2024
# Email:  milliff.a@gmail.com
#
# Script Name: main.R
#
# Script Description: Master replication script. Sources all analysis scripts
#   in order, producing all 28 numbered figures in results/figs/.
#
# Usage:
#   Open an R session with this file's directory as the working directory, then:
#     source("main.R")
#   Or from the terminal:
#     Rscript main.R
#
# Output: results/figs/fig2-1.png through fig5-4.png (28 figures total)
#
# --------------------------------------------

library(here)

cat("==============================================\n")
cat("  Replication Archive: India Public Opinion   \n")
cat("  Cambridge Elements                          \n")
cat("==============================================\n\n")

# Part 2: Data descriptions and survey coverage maps
cat("Running 01_data-descriptions.R (fig2-1 through fig2-8)...\n")
source(here::here("code/01_data-descriptions.R"))

# Part 3: China approval analysis
cat("\nRunning 02_china-analysis.R (fig3-1 through fig3-9)...\n")
source(here::here("code/02_china-analysis.R"))

# Part 4: U.S. approval analysis
cat("\nRunning 03_us-analysis.R (fig4-1 through fig4-4)...\n")
source(here::here("code/03_us-analysis.R"))

# Part 5: Russia/USSR approval analysis
cat("\nRunning 04_russia-analysis.R (fig5-1 through fig5-4)...\n")
source(here::here("code/04_russia-analysis.R"))

cat("\n==============================================\n")
cat("  Done. 28 figures written to results/figs/   \n")
cat("==============================================\n\n")

sessionInfo()
