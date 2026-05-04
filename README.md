
# Replication Archive: *India Public Opinion Toward the Major Powers*
## Cambridge Elements in Indo-Pacific Security 

**Authors:** Aidan Milliff & Paul Staniland

**Contact:** milliff.a@gmail.com  

**Last updated:** 2026

---

## Overview

This archive contains all data and code necessary to reproduce the numbered figures in the *Element*. Running `main.R` from the project root will reproduce all 28 figures and write them to `results/figs/`.

## Extras

In addition to replication data/code, the repository contains two resources for other researchers interested in historical surveys from the _India Institute of Public Opinion._ 

1. `roper_ascii_read.R` contains code that can be adapted to read IIOPO data, which comes in ASCII/punchcard format from the Roper Center.
2. `IIOPO_FP_Categories.docx` and `IIOPO_FP_Questions.xlsx` contain an index of ALL foreign policy questions, by topic, asked on IIOPO surveys between 1956 and 1988. 

---

## Requirements

**Software:**
- R 4.5.3 (see `renv.lock` for exact package versions)

**Package management:** This archive uses [`renv`](https://rstudio.github.io/renv/) to lock all package versions. To restore the exact environment used to produce the figures, run `renv::restore()` before sourcing `main.R`. Packages will also auto-install on first run if missing.


---

## Data

Pre-processed data files are in `data/`. The underlying surveys require institutional access and cannot be redistributed; only the variables necessary to reproduce the figures are included. See source notes below.

| File | Description | Source |
|------|-------------|--------|
| `IND_ADM1.geojson` | Indian state boundaries shapefile (for survey location maps) | [datameet/maps](https://github.com/datameet/maps) |
| `gallup_indf.RData` | Gallup World Poll, India, 12 waves (2006–2019) — analysis variables only. **Cannot be redistributed.** To rebuild from raw data, see `code/00_build-gallup-indf.R` and *Obtaining Restricted Data* below. | Gallup Organization |
| `pew_clean.RData` | Pew Global Attitudes, India, 13 waves (2002–2019). **Cannot be redistributed.** To rebuild from raw data, see `code/00_clean-pew.R` and *Obtaining Restricted Data* below. | Pew Research Center |
| `roper_allyears.RData` | Indian Institute of Public Opinion (IIPO/IIOPO) microdata, 40 waves (1974–2001) | Roper Center for Public Opinion Research |
| `dk_rates.csv` | Pre-computed "Don't Know" response rates per survey question (summary only) | Derived from Gallup |
| `iiopo-59-88.xlsx` | IIOPO topline aggregates, 1959–1988 (pre-microdata) | Roper Center |
| `pew_over_time.xlsx` | Pew longitudinal favorability aggregates | Pew Research Center |
| `pew_1618_states.RData` | Pew 2016–2018 state identifiers (for maps) | Pew Research Center |
| `pew_0709_cities.RData` | Pew 2007–2009 city identifiers (for maps) | Pew Research Center |

---

## Obtaining Restricted Data

Two datasets require institutional access and cannot be included in this archive. Construction scripts are provided so that researchers who obtain the raw data can rebuild the `.RData` files used by the analysis scripts.

### Gallup World Poll (`gallup_indf.RData`)

**Source:** Gallup Organization  
**What to request:** Gallup World Poll microdata for India, waves 2006–2019 (March 2019 release: `The_Gallup_032219.dta`). Academic data-sharing arrangements are available through the [Gallup Analytics portal](https://www.gallup.com/analytics/318923/world-poll-public-datasets.aspx) or via a direct data-sharing agreement with Gallup.

**To rebuild:**
1. Obtain `The_Gallup_032219.dta` (or the South Asia subset `South Asia Subdata.dta`)
2. Open `code/00_build-gallup-indf.R` and set `raw_data_path` to your file's location
3. Un-comment and run the corresponding line in `main.R`

### Pew Global Attitudes (`pew_clean.RData`)

**Source:** Pew Research Center  
**What to request:** Pew Global Attitudes Survey SPSS (`.sav`) files for India-inclusive waves: 2002, 2004, 2005, 2007, 2008, 2009, 2010, 2011, 2012, 2014, 2015, 2016, 2017, 2018. Files are freely available from the [Pew Research Center data archive](https://www.pewresearch.org/global/datasets/) after registration.

**To rebuild:**
1. Download the relevant `.sav` files and place them in a single directory
2. Open `code/00_clean-pew.R` and set `raw_data_path` to that directory
3. Un-comment and run the corresponding line in `main.R`

---

## Reproducing the Figures

**Clone or Download the Repository**
```bash
git clone https://github.com/milliff/lenient-peacock/
```

**Recommended (with renv):**

```r
# From the project root
renv::restore()   # installs locked package versions
source("main.R")  # produces all 28 figures in results/figs/
```

**Without renv** (packages installed automatically if missing):

```r
source("main.R")
```

To reproduce a subset of figures, source individual scripts:

```r
library(here)
source(here::here("code/01_data-descriptions.R"))  # fig2-1 through fig2-8
source(here::here("code/02_china-analysis.R"))      # fig3-1 through fig3-9
source(here::here("code/03_us-analysis.R"))         # fig4-1 through fig4-4
source(here::here("code/04_russia-analysis.R"))     # fig5-1 through fig5-4
```

All scripts use `here::here()` for file paths and will work correctly as long as R's working directory is set to the project root (the folder containing `main.R` and `.here`).

---

## Figure Index

| Figure | Script | Description |
|--------|--------|-------------|
| fig2-1 | `01_data-descriptions.R` | Survey respondent counts by year and source |
| fig2-2 | `01_data-descriptions.R` | IIPO survey locations (1959–2001) |
| fig2-3 | `01_data-descriptions.R` | Pew and Gallup survey locations (2007–2009, 2016–2018) |
| fig2-4 | `01_data-descriptions.R` | IIOPO national average approval, 1959–2001 (with pre-microdata) |
| fig2-5 | `01_data-descriptions.R` | IIOPO city-level approval over time, three countries |
| fig2-6a | `01_data-descriptions.R` | Gallup leadership approval over time, by region |
| fig2-6b | `01_data-descriptions.R` | Pew favorability over time, by region |
| fig2-7 | `01_data-descriptions.R` | Distribution of "Don't Know" rates across foreign policy questions |
| fig2-8 | `01_data-descriptions.R` | Predicted DK count by region and SES |
| fig3-1 | `02_china-analysis.R` | China approval over time, three sources combined |
| fig3-2 | `02_china-analysis.R` | Sumdorong Chu crisis: predicted probability (IIOPO) |
| fig3-3a | `02_china-analysis.R` | Depsang standoff: predicted probability (Pew) |
| fig3-3b | `02_china-analysis.R` | Depsang standoff: predicted probability (Gallup) |
| fig3-4a | `02_china-analysis.R` | Doklam crisis: predicted probability (Pew) |
| fig3-4b | `02_china-analysis.R` | Doklam crisis: predicted probability (Gallup) |
| fig3-5 | `02_china-analysis.R` | Heterogeneous treatment effects (interflex), China |
| fig3-6 | `02_china-analysis.R` | Regional variation in China approval |
| fig3-7 | `02_china-analysis.R` | Party identification and China approval (Pew) |
| fig3-8 | `02_china-analysis.R` | Party-in-power effects on China approval |
| fig3-9 | `02_china-analysis.R` | Interaction effects by party (China) |
| fig4-1 | `03_us-analysis.R` | U.S. approval over time, three sources combined |
| fig4-2 | `03_us-analysis.R` | Pew and Gallup U.S. approval combined time series |
| fig4-3 | `03_us-analysis.R` | Regional variation in U.S. approval |
| fig4-4 | `03_us-analysis.R` | Interaction effects by region (U.S.) |
| fig5-1 | `04_russia-analysis.R` | Russia/USSR approval over time, three sources combined |
| fig5-2 | `04_russia-analysis.R` | Pew and Gallup Russia approval combined time series |
| fig5-3 | `04_russia-analysis.R` | Regional variation in Russia/USSR approval |
| fig5-4 | `04_russia-analysis.R` | Interaction effects by party (Russia) |

---

## Directory Structure

```
replication-material/
├── README.md                    # This file
├── main.R                       # Master run script
├── code/
│   ├── 00_build-gallup-indf.R   # Rebuilds gallup_indf.RData from raw GWP data
│   ├── 00_clean-pew.R           # Rebuilds pew_clean.RData from raw Pew data
│   ├── 01_data-descriptions.R   # Part 2: survey coverage and descriptive plots
│   ├── 02_china-analysis.R      # Part 3: China approval analysis
│   ├── 03_us-analysis.R         # Part 4: U.S. approval analysis
│   └── 04_russia-analysis.R     # Part 5: Russia/USSR approval analysis
├── data/                        # Pre-processed data files (see Data section)
├── renv/                        # renv package cache
├── renv.lock                    # Locked package versions
└── results/
    └── figs/                    # Output: numbered publication figures (populated by main.R)
```

---

## Why is it called this?

In order to keep project names unique and memorable, names for my github repositories come from a custom generator. You can use it too if you like. See: https://aidanmilliff.com/post/project-name-generator/
