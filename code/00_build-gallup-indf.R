# --------------------------------------------
#
# Author: Aidan Milliff and Paul Staniland
# Copyright (c) Aidan Milliff, 2026
# Email:  milliff.a@gmail.com
#
# Script Name: 00_build-gallup-indf.R
#
# Script Description: Builds gallup_indf.RData from the Gallup World Poll
#   Stata release. Filters to India, selects and renames variables to the
#   INDF schema used by the analysis scripts, computes derived variables,
#   and saves to data/gallup_indf.RData.
#
# Raw data required:
#   Gallup World Poll microdata — March 2019 release (The_Gallup_032219.dta).
#   Data are available to academic subscribers through the Gallup Analytics
#   portal (analytics.gallup.com) or via a data-sharing agreement with Gallup.
#   Request: "Gallup World Poll microdata, India, waves 2006–2019."
#
# Usage:
#   1. Set raw_data_path below to the full path of The_Gallup_032219.dta.
#   2. Run this script from the project root (lenient-peacock/) or source
#      via main.R.
#
# Output: data/gallup_indf.RData  (object: indf, 50,434 obs x 15 vars)
#
# Variable mapping (GWP variable → INDF variable):
#   WP151     → leadershipOpinion_usa
#   WP156     → leadershipOpinion_china
#   WP155     → leadershipOpinion_russia
#   YEAR_WAVE → YEAR_WAVE, YEAR_CALENDAR
#   REGION_IND  → state
#   REGION2_IND → region
#   WP1219    → female
#   WP1220    → age
#   WP3117    → educ
#   WP14      → urban
#   EMP_2010  → employed_2010
#   INCOME_1  → hh_income_localcurrency
#   INCOME_3  → percap_income_localcurrency
#   wgt       → (used for DK_count, not retained in final object)
#   (derived) → DK_count
#
# --------------------------------------------


# SET PATH ------------------------------------------------------------------
# Change this to the location of your Gallup World Poll .dta file.

raw_data_path <- "[PATH TO The_Gallup_032219.dta]"   # <-- change this


# INSTALL PACKAGES & LOAD LIBRARIES -----------------------------------------
cat("INSTALLING PACKAGES & LOADING LIBRARIES... \n\n", sep = "")
packages <- c("tidyverse", "haven", "here")
n_packages <- length(packages)
new.pkg <- packages[!(packages %in% installed.packages())]
if (length(new.pkg)) { install.packages(new.pkg) }
for (n in 1:n_packages) {
  cat("Loading Library #", n, " of ", n_packages,
      "... Currently Loading: ", packages[n], "\n", sep = "")
  eval(parse(text = paste0("library(\"", packages[n], "\")")))
}


# LOAD RAW DATA (selected columns only) -------------------------------------
# The full GWP file is ~5.6 GB. col_select avoids loading unused columns.
cat("Loading Gallup World Poll data (selected columns)...\n")

df_raw <- haven::read_dta(
  raw_data_path,
  col_select = c(
    "countrynew",
    "YEAR_WAVE", "YEAR_CALENDAR",
    "WP151",      # US leadership approval
    "WP156",      # China leadership approval
    "WP155",      # Russia leadership approval
    "WP1219",     # Gender
    "WP1220",     # Age
    "WP3117",     # Education level
    "WP14",       # Urban/Rural
    "EMP_2010",   # Employment status
    "INCOME_1",   # Annual household income (local currency)
    "INCOME_3",   # Per capita annual income (local currency)
    "REGION_IND", # India state
    "REGION2_IND" # India region (North/South/East/West/Central)
  )
)

india <- df_raw |> filter(countrynew == "India")
cat("India rows:", nrow(india), "\n")
cat("Years covered:", paste(sort(unique(india$YEAR_WAVE)), collapse = ", "), "\n\n")


# BUILD INDF ----------------------------------------------------------------
cat("Building indf...\n")

indf <- india |>
  mutate(
    # ---- Year variables ----
    YEAR_WAVE     = as.integer(YEAR_WAVE),
    YEAR_CALENDAR = as.numeric(YEAR_CALENDAR),

    # ---- Leadership approval ----
    # Raw values: "Approve" | "Disapprove" | "(DK)" | "(Refused)"
    leadershipOpinion_usa = factor(
      as.character(haven::as_factor(WP151)),
      levels = c("(DK)", "(Refused)", "Approve", "Disapprove")
    ),
    leadershipOpinion_china = factor(
      as.character(haven::as_factor(WP156)),
      levels = c("(DK)", "(Refused)", "Approve", "Disapprove")
    ),
    leadershipOpinion_russia = factor(
      as.character(haven::as_factor(WP155)),
      levels = c("(DK)", "(Refused)", "Approve", "Disapprove")
    ),

    # ---- Demographics ----
    # WP1219: "Female" | "Male"
    female = factor(
      as.character(haven::as_factor(WP1219)),
      levels = c("Female", "Male")
    ),

    # WP1220: numeric age
    age = as.numeric(WP1220),

    # WP3117: education (3 substantive levels + DK/Refused)
    educ = factor(case_when(
      as.character(haven::as_factor(WP3117)) ==
        "Completed elementary education or less (up to 8 years of basic education)"
        ~ "elem",
      as.character(haven::as_factor(WP3117)) ==
        "Secondary - 3 year TertiarySecondary education and some education beyond secondary education (9-15 years of educatio"
        ~ "sec_comp",
      as.character(haven::as_factor(WP3117)) ==
        "Completed four years of education beyond high school and/or received a 4-year college degree."
        ~ "undergrad",
      as.character(haven::as_factor(WP3117)) == "(RF)" ~ "(Refused)",
      as.character(haven::as_factor(WP3117)) == "(DK)" ~ "(DK)",
      TRUE ~ NA_character_
    )),

    # WP14: urban/rural — binarize to Rural/Urban
    urban = factor(case_when(
      as.character(haven::as_factor(WP14)) %in%
        c("A large city", "A suburb of a large city", "A small town or village")
        ~ "Urban",
      as.character(haven::as_factor(WP14)) %in%
        c("A rural area or on a farm")
        ~ "Rural",
      TRUE ~ NA_character_
    ), levels = c("Rural", "Urban")),

    # EMP_2010: employment status (6 levels, used as-is)
    employed_2010 = factor(as.character(haven::as_factor(EMP_2010))),

    # ---- Income ----
    hh_income_localcurrency     = as.numeric(INCOME_1),
    percap_income_localcurrency = as.numeric(INCOME_3),

    # ---- State and region ----
    state  = factor(as.character(haven::as_factor(REGION_IND))),
    region = factor(as.character(haven::as_factor(REGION2_IND)))
  ) |>

  # ---- DK_count: per-respondent count of DK/Refused across opinion items ----
  # NOTE: The pre-built gallup_indf.RData shipped in this archive computed
  # DK_count across a wider set of GWP opinion variables (including Germany
  # leadership, Pakistan border terror, Iran attack morality, and others)
  # from an expanded internal analysis file. This rebuild counts only the
  # three leadership variables that are retained in the slim replication
  # object, giving a range of 0–3 rather than 0–8+. This difference affects
  # figs 2-7 and 2-8 (descriptive DK analysis) only; all Parts 3–5 figures
  # are unaffected. To exactly reproduce figs 2-7/2-8, add the additional
  # opinion variables (WP153 Germany, WP157 Japan, etc.) to col_select above
  # and include them in this rowSums call.
  mutate(
    DK_count = rowSums(
      across(
        c(leadershipOpinion_usa, leadershipOpinion_china, leadershipOpinion_russia),
        ~ . %in% c("(DK)", "(Refused)")
      ),
      na.rm = TRUE
    )
  ) |>

  # ---- Select final 15 columns ----
  select(
    leadershipOpinion_usa,
    leadershipOpinion_china,
    leadershipOpinion_russia,
    YEAR_WAVE,
    YEAR_CALENDAR,
    state,
    region,
    female,
    age,
    educ,
    percap_income_localcurrency,
    hh_income_localcurrency,
    urban,
    DK_count,
    employed_2010
  )

cat("indf dimensions:", nrow(indf), "x", ncol(indf), "\n")
cat("Column names:", paste(names(indf), collapse = ", "), "\n\n")


# SAVE ----------------------------------------------------------------------
save(indf, file = here::here("data/gallup_indf.RData"))
cat("Saved: data/gallup_indf.RData\n\n")


# VERIFICATION --------------------------------------------------------------
cat("=== Verification ===\n")
cat("Expected: 50,434 obs x 15 vars\n")
cat("Got:     ", nrow(indf), "obs x", ncol(indf), "vars\n\n")
cat("leadershipOpinion_china levels:", paste(levels(indf$leadershipOpinion_china), collapse=" | "), "\n")
cat("educ levels:", paste(levels(indf$educ), collapse=" | "), "\n")
cat("employed_2010 levels:", paste(levels(indf$employed_2010), collapse=" | "), "\n")
cat("region levels:", paste(levels(indf$region), collapse=" | "), "\n")
cat("Year range:", range(indf$YEAR_WAVE, na.rm=TRUE), "\n")
