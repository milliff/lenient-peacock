# ============================================================
#
# Author:       Aidan Milliff
# Email:        milliff.a@gmail.com
# Date:         2024-03-11 (updated 2026-05-04)
#
# Script:       Roper / IIOPO Estimation — China, Russia/USSR, USA
#
# Description:  Produces descriptive estimates of urban Indian public
#               approval of the USA, USSR/Russia, and China using
#               USIA/IIOPO survey microdata archived at the Roper Center
#               (waves 1975–2001) and pre-microdata topline reports
#               (waves 1959–1974). Generates Figures 2-4 and 2-5 in
#               Milliff & Staniland (2026).
#
# Data:         Roper Center microdata files (USIA/IIOPO series, i75009–i200152)
#               are proprietary and cannot be redistributed. Researchers may
#               obtain them directly from the Roper Center. The column
#               positions, widths, and variable names needed to parse each
#               wave's fixed-width punchcard files are documented in the
#               commented-out "Create Data" section below.
#
#               The pre-processed combined file (roper_allyears.RData) and
#               the pre-microdata topline spreadsheet (iiopo-59-88.xlsx) are
#               included in the replication archive and are freely available.
#
# ============================================================


# PACKAGES -------------------------------------------------------------------

packages <- c("tidyverse", "ggthemes", "lmtest", "sandwich",
              "readroper", "estimatr")

new.pkg <- packages[!(packages %in% installed.packages())]
if (length(new.pkg)) install.packages(new.pkg)

invisible(lapply(packages, library, character.only = TRUE))


# =============================================================================
# SECTION 1: CREATE DATA FROM ROPER PUNCHCARD FILES
# =============================================================================
#
# NOTE: This section cannot be run without access to the Roper Center
# proprietary .dat files. It is included so that researchers who obtain
# those files can reproduce our data construction from scratch.
#
# Each wave's .dat file is a fixed-width ASCII (punchcard) file. We use
# read_rpr() from the {readroper} package to parse them, specifying column
# start positions, column widths, and variable names derived from each
# wave's PDF codebook.
#
# Standard variable set across waves:
#   serial, city, sex, age, educ, occup, income, religion,
#   approve_usa, approve_ussr, approve_cn, approve_ba, approve_pk
#
# Approval scale (most waves): 1 = Very good ... 5 = Very bad; 9 = missing
# Some waves use a 6- or 9-value scale; see inline comments.
# =============================================================================

# Place the Roper Center files in a directory and set the path below:
roper_dir <- "path/to/roper-center-files/"

df75 <- read_rpr(filepath = file.path(roper_dir, "1975/i75009.dat"),
                 col_positions = c(1,5,6,7,8,9,10,11, 34,35,36,46,47),
                 widths       = c(4,1,1,1,1,1,1,1,  1,1,1,1,1),
                 col_names    = c("serial","city","sex","age","educ","occup","income","religion",
                                  "approve_usa","approve_ussr","approve_cn","approve_pk","approve_ba"))

df76 <- read_rpr(filepath = file.path(roper_dir, "1976/i76008.dat"),
                 col_positions = c(1,5,6,7,8,9,10,11, 32,33,34),
                 widths       = c(4,1,1,1,1,1,1,1,  1,1,1),
                 col_names    = c("serial","city","sex","age","educ","occup","income","religion",
                                  "approve_usa","approve_ussr","approve_cn"))
# Pakistan and Bangladesh not asked in 1976; padded below.

df77 <- read_rpr(filepath = file.path(roper_dir, "I77007.dat"),
                 col_positions = c(1,5,6,7,8,9,10,12, 15,16,17),
                 widths       = c(4,1,1,1,1,1,1,1,  1,1,1),
                 col_names    = c("serial","sex","age","educ","occup","city","income","religion",
                                  "approve_usa","approve_ussr","approve_cn"))

df78 <- read_rpr(filepath = file.path(roper_dir, "i78019.dat"),
                 col_positions = c(1,5,6,7,8,9,10,12, 13,14,19,20),
                 widths       = c(4,1,1,1,1,1,1,1,  1,1,1,1),
                 col_names    = c("serial","sex","age","educ","occup","city","income","religion",
                                  "approve_usa","approve_ussr","approve_cn","approve_ba"))

df80 <- read_rpr(filepath = file.path(roper_dir, "i80008.dat"),
                 col_positions = c(1,5,6,7,8,9,10,12, 13,14,19,20),
                 widths       = c(4,1,1,1,1,1,1,1,  1,1,1,1),
                 col_names    = c("serial","sex","age","educ","occup","city","income","religion",
                                  "approve_usa","approve_ussr","approve_cn","approve_ba"))
# 6 values; 1000 == 6 is top

df81 <- read_rpr(filepath = file.path(roper_dir, "i81044.dat"),
                 col_positions = c(1,5,6,7,8,9,10,12, 13,14,20,21,22),
                 widths       = c(4,1,1,1,1,1,1,1,  1,1,1,1,1),
                 col_names    = c("serial","sex","age","educ","occup","city","income","religion",
                                  "approve_usa","approve_ussr","approve_cn","approve_ba","approve_pk"))
# 7 values; 1500 == 7 is top

df82a <- read_rpr(filepath = file.path(roper_dir, "i82025.dat"),
                  col_positions = c(1,5,6,7,8,9,10,12, 13,14,20,21,22),
                  widths       = c(4,1,1,1,1,1,1,1,  1,1,1,1,1),
                  col_names    = c("serial","sex","age","educ","occup","city","income","religion",
                                   "approve_usa","approve_ussr","approve_cn","approve_ba","approve_pk"))
# 6 values; 1500 == 6 is top

df82b <- read_rpr(filepath = file.path(roper_dir, "i82047.dat"),
                  col_positions = c(1,5,6,7,8,9,10,12, 13,14,20,21,22),
                  widths       = c(4,1,1,1,1,1,1,1,  1,1,1,1,1),
                  col_names    = c("serial","sex","age","educ","occup","city","income","religion",
                                   "approve_usa","approve_ussr","approve_cn","approve_ba","approve_pk"))
# 6 values; 1500 == 6 is top

df83 <- read_rpr(filepath = file.path(roper_dir, "i83024.dat"),
                 col_positions = c(1,5,6,7,8,9,10,12, 13,14,20,21,22,80),
                 widths       = c(4,1,1,1,1,1,1,1,  1,1,1,1,1,1),
                 col_names    = c("serial","sex","age","educ","occup","city","income","religion",
                                  "approve_usa","approve_ussr","approve_cn","approve_ba","approve_pk","card"))
df83 <- df83 |> filter(card == 1) |> select(-card)
# 6 values; 1500 == 6 is top

df84a <- read_rpr(filepath = file.path(roper_dir, "i84021.dat"),
                  col_positions = c(1,6,7,8,9,10,11,12, 13,14,21,22,23),
                  widths       = c(4,1,1,1,1,1,1, 1,1,1,1,1,1),
                  col_names    = c("serial","sex","age","educ","occup","city","income","religion",
                                   "approve_usa","approve_ussr","approve_cn","approve_ba","approve_pk"))
# 6 values; 1500 == 6 is top

df84b <- read_rpr(filepath = file.path(roper_dir, "i84050.dat"),
                  col_positions = c(1,5,6,7,8,9,10,12, 13,14,21,22,23),
                  widths       = c(4,1,1,1,1,1,1,1,  1,1,1,1,1),
                  col_names    = c("serial","sex","age","educ","occup","city","income","religion",
                                   "approve_usa","approve_ussr","approve_cn","approve_ba","approve_pk"))
# 6 values; 1500 == 6 is top

df85a <- read_rpr(filepath = file.path(roper_dir, "i85039.dat"),
                  col_positions = c(1,5,6,7,8,9,10,12, 13,14,21,22,23),
                  widths       = c(4,1,1,1,1,1,1,1,  1,1,1,1,1),
                  col_names    = c("serial","sex","age","educ","occup","city","income","religion",
                                   "approve_usa","approve_ussr","approve_cn","approve_ba","approve_pk"))
# 6 values; 1500 == 6 is top

df85b <- read_rpr(filepath = file.path(roper_dir, "i85080.dat"),
                  col_positions = c(1,5,6,7,8,9,10,11,12, 13,14,21,22,23),
                  widths       = c(4,1,1,1,1,1,1,1,  1,1,1,1,1,1),
                  col_names    = c("serial","card","sex","age","educ","occup","city","income","religion",
                                   "approve_usa","approve_ussr","approve_cn","approve_ba","approve_pk"))
df85b <- df85b |> filter(card == 1) |> select(-card)
# 6 values; 1500 == 6 is top

df86a <- read_rpr(filepath = file.path(roper_dir, "i86030.dat"),
                  col_positions = c(1,5,6,7,8,9,10,11, 12,13,20,21,22),
                  widths       = c(4,1,1,1,1,1,1,1,  1,1,1,1,1),
                  col_names    = c("serial","sex","age","educ","occup","city","income","religion",
                                   "approve_usa","approve_ussr","approve_cn","approve_ba","approve_pk"))
# 9 values; 5000 == 9 is top

df86b <- read_rpr(filepath = file.path(roper_dir, "i86073.dat"),
                  col_positions = c(1,5,6,7,8,9,10,11, 12,13,20,21,22),
                  widths       = c(4,1,1,1,1,1,1,1,  1,1,1,1,1),
                  col_names    = c("serial","sex","age","educ","occup","city","income","religion",
                                   "approve_usa","approve_ussr","approve_cn","approve_ba","approve_pk"))
# 9 values; 5000 == 9 is top

df87a <- read_rpr(filepath = file.path(roper_dir, "i87042.dat"),
                  col_positions = c(2,73,76,77,78,1,79,80, 6,7,14,15,16),
                  widths       = c(4,1,1,1,1,1,1,1,  1,1,1,1,1),
                  col_names    = c("serial","sex","age","educ","occup","city","income","religion",
                                   "approve_usa","approve_ussr","approve_cn","approve_ba","approve_pk"))
# 9 values; 2500 == 9 is top

df87b <- read_rpr(filepath = file.path(roper_dir, "i87088.dat"),
                  col_positions = c(1,5,6,9,10,11,12,13,14, 15,16,23,24,25),
                  widths       = c(4,1,1,1,1,1,1,1,  1,1,1,1,1,1),
                  col_names    = c("serial","card","sex","age","educ","occup","city","income","religion",
                                   "approve_usa","approve_ussr","approve_cn","approve_ba","approve_pk"))
df87b <- df87b |> filter(card == 1) |> select(-card)
# 9 values; 2500 == 9 is top

df88a <- read_rpr(filepath = file.path(roper_dir, "i88044.dat"),
                  col_positions = c(2,6,9,10,11,12,13,14, 15,16,23,24,25),
                  widths       = c(4,1,1,1,1,1,1,1,  1,1,1,1,1),
                  col_names    = c("serial","sex","age","educ","occup","city","income","religion",
                                   "approve_usa","approve_ussr","approve_cn","approve_ba","approve_pk"))
# 9 values; 2500 == 9 is top

df88b <- read_rpr(filepath = file.path(roper_dir, "i88101.dat"),
                  col_positions = c(2,103,106,107,108,109,110,111, 7,8,15,16,17),
                  widths       = c(4,1,1,1,1,1,1,1,  1,1,1,1,1),
                  col_names    = c("serial","sex","age","educ","occup","city","income","religion",
                                   "approve_usa","approve_ussr","approve_cn","approve_ba","approve_pk"))
# 9 values; 2500 == 9 is top

df89 <- read_rpr(filepath = file.path(roper_dir, "i89048.dat"),
                 col_positions = c(2,90,93,94,95,96,97,98, 6,7,14,15,16),
                 widths       = c(4,1,1,1,1,1,1,1,  1,1,1,1,1),
                 col_names    = c("serial","sex","age","educ","occup","city","income","religion",
                                  "approve_usa","approve_ussr","approve_cn","approve_ba","approve_pk"))
# 9 values; 2500 == 9 is top

df90a <- read_rpr(filepath = file.path(roper_dir, "i90010.dat"),
                  col_positions = c(2,6,9,10,11,12,13,14, 20,21,28,29,30),
                  widths       = c(4,1,1,1,1,1,1,1,  1,1,1,1,1),
                  col_names    = c("serial","sex","age","educ","occup","city","income","religion",
                                   "approve_usa","approve_ussr","approve_cn","approve_ba","approve_pk"))
# 9 values; 2500 == 9 is top

df90b <- read_rpr(filepath = file.path(roper_dir, "i90056.dat"),
                  col_positions = c(5,84,87,88,89,90,91,92, 10,11,18,19,20),
                  widths       = c(4,1,1,1,1,1,1,1,  1,1,1,1,1),
                  col_names    = c("serial","sex","age","educ","occup","city","income","religion",
                                   "approve_usa","approve_ussr","approve_cn","approve_ba","approve_pk"))
# 9 values; 2500 == 9 is top. Codebook describes sampling points.

df91a <- read_rpr(filepath = file.path(roper_dir, "i91035.dat"),
                  col_positions = c(4,70,73,74,75,76,77,78, 9,10,17,18,19),
                  widths       = c(4,1,1,1,1,1,1,1,  1,1,1,1,1),
                  col_names    = c("serial","sex","age","educ","occup","city","income","religion",
                                   "approve_usa","approve_ussr","approve_cn","approve_ba","approve_pk"))
# 9 values; 2500 == 9 is top. Codebook describes sampling points.

df92a <- read_rpr(filepath = file.path(roper_dir, "i92034.dat"),
                  col_positions = c(4,69,72,73,74,75,76,77, 9,10,17,18,19),
                  widths       = c(4,1,1,1,1,1,1,1,  1,1,1,1,1),
                  col_names    = c("serial","sex","age","educ","occup","city","income","religion",
                                   "approve_usa","approve_ussr","approve_cn","approve_ba","approve_pk"))
# 9 values; 2500 == 9 is top

df92b <- read_rpr(filepath = file.path(roper_dir, "i92057.dat"),
                  col_positions = c(5,100,103,104,105,106,107,108, 10,11,18,19,20),
                  widths       = c(4,1,1,1,1,1,1,1,  1,1,1,1,1),
                  col_names    = c("serial","sex","age","educ","occup","city","income","religion",
                                   "approve_usa","approve_ussr","approve_cn","approve_ba","approve_pk"))
# 9 values; 2500 == 9 is top

df93a <- read_rpr(filepath = file.path(roper_dir, "i93032.dat"),
                  col_positions = c(6,92,95,96,97,98,99,100, 11,12,19,20,21),
                  widths       = c(4,1,1,1,1,1,1,1,  1,1,1,1,1),
                  col_names    = c("serial","sex","age","educ","occup","city","income","religion",
                                   "approve_usa","approve_ussr","approve_cn","approve_ba","approve_pk"))
# 9 values; 2500 == 9 is top

df93b <- read_rpr(filepath = file.path(roper_dir, "i93061.dat"),
                  col_positions = c(6,129,132,133,134,135,136,137, 11,12,19,20,21),
                  widths       = c(4,1,1,1,1,1,1,1,  1,1,1,1,1),
                  col_names    = c("serial","sex","age","educ","occup","city","income","religion",
                                   "approve_usa","approve_ussr","approve_cn","approve_ba","approve_pk"))
# 9 values; 5500 == 9 is top

df94a <- read_rpr(filepath = file.path(roper_dir, "i94020.dat"),
                  col_positions = c(6,85,88,89,90,91,92,93, 11,12,19,20,21),
                  widths       = c(4,1,1,1,1,1,1,1,  1,1,1,1,1),
                  col_names    = c("serial","sex","age","educ","occup","city","income","religion",
                                   "approve_usa","approve_ussr","approve_cn","approve_ba","approve_pk"))
# 9 values; 5500 == 9 is top

df94b <- read_rpr(filepath = file.path(roper_dir, "i94063.dat"),
                  col_positions = c(6,75,78,79,80,81,82,83, 11,12,19,20,21),
                  widths       = c(4,1,1,1,1,1,1,1,  1,1,1,1,1),
                  col_names    = c("serial","sex","age","educ","occup","city","income","religion",
                                   "approve_usa","approve_ussr","approve_cn","approve_ba","approve_pk"))
# 9 values; 5500 == 9 is top

df95b <- read_rpr(filepath = file.path(roper_dir, "i95063.dat"),
                  col_positions = c(6,110,113,114,115,116,117,118, 11,12,19,20,21),
                  widths       = c(4,1,1,1,1,1,1,1,  1,1,1,1,1),
                  col_names    = c("serial","sex","age","educ","occup","city","income","religion",
                                   "approve_usa","approve_ussr","approve_cn","approve_ba","approve_pk"))
# 9 values; 5500 == 9 is top

df96a <- read_rpr(filepath = file.path(roper_dir, "i96046.dat"),
                  col_positions = c(6,163,166,167,168,169,170,171, 11,12,19,20,21),
                  widths       = c(4,1,1,1,1,1,1,1,  1,1,1,1,1),
                  col_names    = c("serial","sex","age","educ","occup","city","income","religion",
                                   "approve_usa","approve_ussr","approve_cn","approve_ba","approve_pk"))
# 9 values; 5500 == 9 is top

df96b <- read_rpr(filepath = file.path(roper_dir, "i96084.dat"),
                  col_positions = c(6,107,110,111,112,113,114,115, 11,12,19,20,21),
                  widths       = c(4,1,1,1,1,1,1,1,  1,1,1,1,1),
                  col_names    = c("serial","sex","age","educ","occup","city","income","religion",
                                   "approve_usa","approve_ussr","approve_cn","approve_ba","approve_pk"))
# 9 values; 5500 == 9 is top

df97a <- read_rpr(filepath = file.path(roper_dir, "i97039.dat"),
                  col_positions = c(6,87,90,91,92,93,94,95, 11,12,19,20,21),
                  widths       = c(4,1,1,1,1,1,1,1,  1,1,1,1,1),
                  col_names    = c("serial","sex","age","educ","occup","city","income","religion",
                                   "approve_usa","approve_ussr","approve_cn","approve_ba","approve_pk"))
# 9 values; 5500 == 9 is top

df97b <- read_rpr(filepath = file.path(roper_dir, "i97092.dat"),
                  col_positions = c(6,115,118,119,120,121,122,123, 16,11,19,20,21),
                  widths       = c(4,1,1,1,1,1,1,1,  1,1,1,1,1),
                  col_names    = c("serial","sex","age","educ","occup","city","income","religion",
                                   "approve_usa","approve_ussr","approve_cn","approve_ba","approve_pk"))
# 9 values; 5500 == 9 is top

df98a <- read_rpr(filepath = file.path(roper_dir, "i98033.dat"),
                  col_positions = c(6,135,138,139,140,141,142,143, 14,12,11,20,21),
                  widths       = c(4,1,1,1,1,1,1,1,  1,1,1,1,1),
                  col_names    = c("serial","sex","age","educ","occup","city","income","religion",
                                   "approve_usa","approve_ussr","approve_cn","approve_ba","approve_pk"))
# 9 values; 5500 == 9 is top

df98b <- read_rpr(filepath = file.path(roper_dir, "i98054.dat"),
                  col_positions = c(6,125,128,129,130,131,132,133, 14,12,11,20,21),
                  widths       = c(4,1,1,1,1,1,1,1,  1,1,1,1,1),
                  col_names    = c("serial","sex","age","educ","occup","city","income","religion",
                                   "approve_usa","approve_ussr","approve_cn","approve_ba","approve_pk"))
# 9 values; 5500 == 9 is top

df99a <- read_rpr(filepath = file.path(roper_dir, "i99013.dat"),
                  col_positions = c(6,94,97,98,99,100,101,102, 28,26,25,34,35),
                  widths       = c(4,1,1,1,1,1,1,1,  1,1,1,1,1),
                  col_names    = c("serial","sex","age","educ","occup","city","income","religion",
                                   "approve_usa","approve_ussr","approve_cn","approve_ba","approve_pk"))
# 9 values; 5500 == 9 is top

df00a <- read_rpr(filepath = file.path(roper_dir, "i20047.dat"),
                  col_positions = c(6,99,102,103,104,105,106,107, 14,12,11,20,21),
                  widths       = c(4,1,1,1,1,1,1,1,  1,1,1,1,1),
                  col_names    = c("serial","sex","age","educ","occup","city","income","religion",
                                   "approve_usa","approve_ussr","approve_cn","approve_ba","approve_pk"))
# 9 values; 5500 == 9 is top

df00b <- read_rpr(filepath = file.path(roper_dir, "i20076.dat"),
                  col_positions = c(6,136,139,140,141,142,143,144, 46,44,43,52,53),
                  widths       = c(4,1,1,1,1,1,1,1,  1,1,1,1,1),
                  col_names    = c("serial","sex","age","educ","occup","city","income","religion",
                                   "approve_usa","approve_ussr","approve_cn","approve_ba","approve_pk"))

df01a <- read_rpr(filepath = file.path(roper_dir, "i200152.dat"),
                  col_positions = c(6,112,115,116,117,118,119,120, 17,16,15,28,20),
                  widths       = c(4,1,1,1,1,1,1,1,  1,1,1,1,1),
                  col_names    = c("serial","sex","age","educ","occup","city","income","religion",
                                   "approve_usa","approve_ussr","approve_cn","approve_ba","approve_pk"))
# 9 values; 30000 == 9 is top


# RECODE AND COMBINE WAVES ---------------------------------------------------
#
# Waves missing Bangladesh or Pakistan approval questions are padded with NA.
# Income is recoded to a within-wave binary (above/below wave mean) to achieve
# comparability across years where the income scale changes.

df76$approve_ba <- NA_character_; df76$approve_pk <- NA_character_
df77$approve_ba <- NA_character_; df77$approve_pk <- NA_character_
df78$approve_pk <- NA_character_
df80$approve_pk <- NA_character_

recoder <- function(x) {
  x |> mutate(
    sex    = recode_factor(sex,  `1` = "Male",   `2` = "Female"),
    age    = recode_factor(age,  `1` = "21-35",  `2` = "36-50", `3` = "50+", .ordered = TRUE),
    educ   = recode_factor(educ, `1` = "Primary or less", `2` = "Some secondary",
                           `3` = "Secondary completed", `4` = "Univ. or more", .ordered = TRUE),
    occup  = recode_factor(occup,
                           `1` = "Professional", `2` = "Trader",     `3` = "Gov/Exec",
                           `4` = "Clerk/Asst/Teacher", `5` = "Skilled worker",
                           `6` = "Unskilled worker", `7` = "Student",
                           `8` = "Housewife",  `9` = "Not working"),
    religion = recode_factor(religion,
                             `1` = "Hindu", `2` = "Muslim", `3` = "Christian",
                             `4` = "Sikh",  `5` = "Other"),
    city   = recode_factor(city,
                           `1` = "Mumbai", `2` = "Kolkata", `3` = "Delhi", `4` = "Chennai"),
    across(starts_with("approve_"),
           ~ recode_factor(.x,
                           `1` = "Very good", `2` = "Good", `3` = "Neither good nor bad",
                           `4` = "Bad",       `5` = "Very bad", .ordered = TRUE))
  )
}

dfs <- list(
  "1975" = df75, "1977" = df77, "1978" = df78, "1980" = df80, "1981" = df81,
  "1982a" = df82a, "1982b" = df82b, "1983" = df83,
  "1984a" = df84a, "1984b" = df84b,
  "1985a" = df85a, "1985b" = df85b,
  "1986a" = df86a, "1986b" = df86b,
  "1987a" = df87a, "1987b" = df87b,
  "1988a" = df88a, "1988b" = df88b,
  "1989"  = df89,
  "1990a" = df90a, "1990b" = df90b,
  "1991"  = df91a,
  "1992a" = df92a, "1992b" = df92b,
  "1993a" = df93a, "1993b" = df93b,
  "1994a" = df94a, "1994b" = df94b,
  "1995"  = df95b,
  "1996a" = df96a, "1996b" = df96b,
  "1997a" = df97a, "1997b" = df97b,
  "1998a" = df98a, "1998b" = df98b,
  "1999"  = df99a,
  "2000a" = df00a, "2000b" = df00b,
  "2001"  = df01a
)

mutated_waves    <- map(dfs, recoder)
add_wave_names   <- purrr::map(names(mutated_waves),
                               ~mutate(mutated_waves[[.]], src_col = .))
df               <- plyr::ldply(add_wave_names, data.frame) |>
                      rename(wave = src_col)

# Recode missing/invalid responses to NA
recode_missing <- function(col) forcats::fct_recode(col, NULL = "missing", NULL = "9")
df <- df |>
  mutate(across(starts_with("approve_"), recode_missing),
         religion = forcats::fct_recode(religion, NULL = "0", NULL = "-"),
         occup    = forcats::fct_recode(occup,    NULL = "0", NULL = "-"),
         income   = suppressWarnings(as.numeric(as.character(
                      forcats::fct_recode(income, NULL = "-", NULL = "N")))))

# Income binary: 1 = above within-wave mean
df <- df |>
  group_by(wave) |>
  mutate(avg_income = mean(income, na.rm = TRUE),
         income_bin = as.integer(income >= avg_income)) |>
  ungroup()

save(df, file = "../../data/roper_allyears.RData")