# --------------------------------------------
#
# Author: Aidan Milliff and Paul Staniland
# Copyright (c) Aidan Milliff, 2026
# Email:  milliff.a@gmail.com
#
# Script Name: 01_data-descriptions.R
#
# Script Description: Data descriptions, survey coverage, and descriptive plots for
#   Part 2 of the Element. Merges content originally in part-1-2/ and part-6/.
#
# Produces: fig2-1.png through fig2-8.png
#
#
# --------------------------------------------

# INSTALL PACKAGES & LOAD LIBRARIES -----------------
cat("INSTALLING PACKAGES & LOADING LIBRARIES... \n\n", sep = "")
packages <- c("tidyverse", "ggthemes", "lmtest", "sandwich", "readroper", "estimatr",
              "gt", "kableExtra", "stringr", "readxl", "purrr", "sf", "tmap",
              "tidygeocoder", "gridExtra", "lubridate", "modelsummary", "here")
n_packages <- length(packages)
new.pkg <- packages[!(packages %in% installed.packages())]
if(length(new.pkg)){ install.packages(new.pkg) }
for(n in 1:n_packages){
  cat("Loading Library #", n, " of ", n_packages, "... Currently Loading: ", packages[n], "\n", sep = "")
  lib_load <- paste("library(\"",packages[n],"\")", sep = "")
  eval(parse(text = lib_load))
}

# LOAD DATA -----------------------------------------

load(here::here("data/gallup_indf.RData"))
load(here::here("data/pew_clean.RData"))
load(here::here("data/roper_allyears.RData"))
load(here::here("data/pew_1618_states.RData"))
load(here::here("data/pew_0709_cities.RData"))

# Create binary approval variables (IIOPO / Roper data)
df$usa_bin  <- ifelse(df$approve_usa  %in% c("Very good", "Good"), 1, 0)
df$usanum   <- as.numeric(forcats::fct_rev(df$approve_usa))
df$ussr_bin <- ifelse(df$approve_ussr %in% c("Very good", "Good"), 1, 0)
df$ussrnum  <- as.numeric(forcats::fct_rev(df$approve_ussr))
df$cn_bin   <- ifelse(df$approve_cn   %in% c("Very good", "Good"), 1, 0)
df$cnnum    <- as.numeric(forcats::fct_rev(df$approve_cn))
df$ba_bin   <- ifelse(df$approve_ba   %in% c("Very good", "Good"), 1, 0)
df$banum    <- as.numeric(forcats::fct_rev(df$approve_ba))
df$pk_bin   <- ifelse(df$approve_pk   %in% c("Very good", "Good"), 1, 0)
df$pknum    <- as.numeric(forcats::fct_rev(df$approve_pk))

# Create binary approval variables (Gallup data)
indf$us_leader_bin <- ifelse(indf$leadershipOpinion_usa    == "Approve", 1,
                      ifelse(indf$leadershipOpinion_usa    == "Disapprove", 0, NA))
indf$cn_leader_bin <- ifelse(indf$leadershipOpinion_china  == "Approve", 1,
                      ifelse(indf$leadershipOpinion_china  == "Disapprove", 0, NA))
indf$ru_leader_bin <- ifelse(indf$leadershipOpinion_russia == "Approve", 1,
                      ifelse(indf$leadershipOpinion_russia == "Disapprove", 0, NA))

# Fix region coding (Gallup)
indf <- indf |> mutate(region = case_when(
  state == "Chhattisgarh"  ~ "East",
  state == "Madhya Pradesh" ~ "North",
  is.na(state) & region == "Central" ~ "North",
  .default = as.character(region)))


# ── Figure 2-1: Survey Coverage Bar Chart ─────────────────────────────────────

# Surveys per Year

gallup <- indf |> select(YEAR_WAVE) |> rename(Year = YEAR_WAVE) |> mutate(source = "Gallup")
pew    <- dfall |> select(YEAR) |> rename(Year = YEAR) |> mutate(source = "Pew")
iipo   <- df |> select(wave) |> mutate(Year = as.numeric(gsub("[^0-9.-]", "", wave)),
                                       source = "IIPO") |> select(-wave)
iipo   <- iipo[,2:3]

surveys_years <- bind_rows(gallup, pew, iipo) |> group_by(Year, source) |> summarise(n = n())

survey_table <- surveys_years |> pivot_wider(names_from = Year, values_from = n) |> t() 
survey_table <-  cbind(rownames(survey_table), as.data.frame(survey_table))
colnames(survey_table) <- c("Year","IIPO", "Pew", "Gallup")
survey_table <- survey_table[2:nrow(survey_table),]
survey_table <- survey_table |> mutate(IIPO = as.numeric(IIPO),
                                       Pew = as.numeric(Pew),
                                       Gallup = as.numeric(Gallup))

fig2_1 <- ggplot(surveys_years) + geom_bar(aes(x = Year, fill = source, group = source, y = n), position = "dodge", stat = "identity") + scale_fill_ptol() + theme_bw(base_size = 14) +
  labs(title = "Survey Data Coverage by Year",
       subtitle = "Respondent Counts by Survey Data Source",
       x = "Year",
       y = "Respondents",
       fill = "Survey")
ggsave(filename = here::here("results/figs/fig2-1.png"), plot = fig2_1, width = 7, height = 4, dpi = "retina")


# ── Figure 2-2: IIPO Survey Location Map ──────────────────────────────────────
# ── Figure 2-3: Four-Panel Survey Location Maps (Pew and Gallup) ─────────────

# Approval Models ---------------------------------------------------------

# Over Time Plot ----------------------------------------------------------

# Fix regions?

# Approval Vars
# US = leadershipOpinion_usa
# China = leadershipOpinion_China
# Russia = = leadershipOpinion_Russia

indf$us_leader_bin <- ifelse(indf$leadershipOpinion_usa == "Approve", 1,
                                ifelse(indf$leadershipOpinion_usa == "Disapprove", 0, NA))
indf$cn_leader_bin <- ifelse(indf$leadershipOpinion_china == "Approve", 1,
                                ifelse(indf$leadershipOpinion_china == "Disapprove", 0, NA))
indf$ru_leader_bin <- ifelse(indf$leadershipOpinion_russia == "Approve", 1,
                             ifelse(indf$leadershipOpinion_russia == "Disapprove", 0, NA))

indf <- indf |> mutate(region = case_when(state == "Chhattisgarh" ~ "East",
                                          state == "Madhya Pradesh" ~ "North",
                                          is.na(state) & region == "Central" ~ "North",
                                          .default = as.character(region)))


time_df <- indf |> group_by(YEAR_WAVE, region) |> 
  summarise(USA = mean(us_leader_bin, na.rm = T),
            Russia = mean(ru_leader_bin,na.rm = T),
            China = mean(cn_leader_bin,na.rm = T)) |> 
  pivot_longer(!c(YEAR_WAVE, region),
               names_to = "country",
               values_to = "approval") |> 
  drop_na()

time_df$region <- relevel(factor(time_df$region, levels = c("North", "West", "East", "South")), ref = "North" ) # This is to get the colors/order to match IIOPO

# 4477aa to mumbai / west
# 117733 Kolkata/East
# DDCC77 Delhi/North
# CC6677 Chennai/South

timeplot <- ggplot(time_df) + geom_hline(yintercept = .5, color = "darkgrey") +
  geom_smooth(aes(x = YEAR_WAVE, y = approval, color= region), se = F, span = .45, lwd=1.1)  +
  geom_point(aes(x = YEAR_WAVE, y = approval, color= region), pch = 16, alpha = .7) +
  facet_wrap(~country+region, nrow = 3) +
  theme_bw(base_size = 14) +
  scale_color_manual(values = c(ptol_pal()(4), ptol_pal()(5)[2])) + # To get colors/order to match IIOPO
  ylim(c(0,1)) +
  labs(title= "Indian Public Attitudes Over Time",
       subtitle = "Gallup Survey Data across 12 waves",
       x = "Year",
       y = "% Approval (Leadership)",
       color = "Region")
ggsave(filename = here::here("results/figs/fig2-6a.png"), timeplot, width = 8, height = 5, dpi = "retina")



# Pew ---------------------------------------------------------------------


load(here::here("data/pew_clean.RData"))

time_df <- dfall |> group_by(YEAR, region) |> 
  summarise(USA = mean(approve_us_bin, na.rm = T),
            Russia = mean(approve_ru_bin,na.rm = T),
            China = mean(approve_cn_bin,na.rm = T)) |> 
  pivot_longer(!c(YEAR, region),
               names_to = "country",
               values_to = "approval") |> 
  drop_na()

time_df$region <- relevel(factor(time_df$region, levels = c("North", "West", "East", "South")), ref = "North") # This is to get the colors/order to match IIOPO

timeplot <- ggplot(time_df) + geom_hline(yintercept = .5, color = "darkgrey") +
  geom_smooth(aes(x = YEAR, y = approval, color= region), se = F, span = .45, lwd=1.1)  +
  geom_point(aes(x = YEAR, y = approval, color= region), pch = 16, alpha = .7) +
  facet_wrap(~country+region, nrow = 3) +
  theme_bw(base_size = 14) +
  scale_color_manual(values = c(ptol_pal()(4), ptol_pal()(5)[2])) + # To get colors/order to match IIOPO
  ylim(c(0,1)) +
  labs(title= "Indian Public Attitudes Over Time",
       subtitle = "Pew Survey Data across 13 waves",
       x = "Year",
       y = "% Approval",
       color = "Region")
ggsave(filename = here::here("results/figs/fig2-6b.png"), timeplot, width = 8, height = 5, dpi = "retina")



# Maps comparing Pew/Gallup Sampling --------------------------------------

# Pew only reports states or cities in certain years, so this is 2007-2010 and 2017-2016
sta <- st_read(here::here('data/IND_ADM1.geojson')) %>% st_make_valid()
load(here::here("data/pew_1618_states.RData"))
load(here::here("data/pew_0709_cities.RData"))

states <- gsub("National Capital Territory of Dheli", "Dheli", states)
states <- gsub("Dheli", "Dheli", states)
states <- gsub("Jammu and Kashmir", "Jammu & Kashmir", states)
states <- gsub("Orissa", "Odisha", states)

cities <- gsub("Delhi—North", "Delhi", cities)
cities <- gsub("Lucknow--North", "Uttar Pradesh", cities)
cities <- gsub("Chennai—South", "Tamil Nadu", cities)
cities <- gsub("Hyderabad—South", "Andhra Pradesh", cities)
cities <- gsub("Kolkata----East", "West Bengal", cities)
cities <- gsub("Patna--East", "Bihar", cities)
cities <- gsub("Mumbai—West", "Maharashtra", cities)
cities <- gsub("Ahmedabad—West", "Gujarat", cities)
cities <- gsub("Mumbai", "Maharashtra", cities)
cities <- gsub("Ahmedabad", "Gujarat", cities)
cities <- gsub("Kolkatta", "West Bengal", cities)
cities <- gsub("Patna", "Bihar", cities)
cities <- gsub("Lucknow", "Uttar Pradesh", cities)
cities <- gsub("Chennai", "Tamil Nadu", cities)
cities <- gsub("Hyderabad", "Andhra Pradesh", cities)


cities <- cities |> as.data.frame() |>  tidygeocoder::geocode(address = cities)
states <- states |> as.data.frame() |>  tidygeocoder::geocode(address = states)

cities_sf <- cities %>% 
  mutate_at(vars(long, lat), as.numeric) %>% 
  st_as_sf(
    coords = c("long", "lat"),
    agr = "constant",
    crs = 4326, # WGS84
    stringsAsFactors = F,
    remove = T
  )

states_sf <- states %>% 
  mutate_at(vars(long, lat), as.numeric) %>% 
  st_as_sf(
    coords = c("long", "lat"),
    agr = "constant",
    crs = 4326, # WGS84
    stringsAsFactors = F,
    remove = T
  )

# put schools within polygons
states_in_poly <- st_join(states_sf, sta, join = st_within)

cities_in_poly <- st_join(cities_sf, sta, join = st_within)

# Map 2016/2018
states_count <- count(as_tibble(states_in_poly), shapeID)
states_count$frac <- states_count$n / nrow(states)
states_count_df <- left_join(sta, states_count)

# Map 2007/2009
cities_count <- count(as_tibble(cities_in_poly), shapeID)
cities_count$frac <- cities_count$n / nrow(cities)
cities_count_df <- left_join(sta, cities_count)

# Shared breaks for aligned color scales
pew_breaks <- pretty(c(states_count_df$frac, cities_count_df$frac), na.rm = TRUE)

tmap_mode("plot")
states_map <- tm_shape(states_count_df) +
  tm_polygons(fill = "grey85", col = "white", lwd = 0.5) +
  tm_shape(states_count_df |> filter(!is.na(frac))) +
  tm_polygons(
    fill = "frac",
    col = "white",
    lwd = 0.5,
    fill.scale = tm_scale_intervals(
      values = "Reds",
      values.range = c(0.1, 1),
      breaks = pew_breaks
    ),
    fill.legend = tm_legend(
      title = "Fraction of Surveys",
      title.size = 1,
      na.show = FALSE
    ),
    fill_alpha = 0.8
  ) +
  tm_layout(
    main.title = "2016-2018 Pew Survey Locations",
    legend.position = tm_pos_in("right", "top")
  )

cities_map <- tm_shape(cities_count_df) +
  tm_polygons(fill = "grey85", col = "white", lwd = 0.5) +
  tm_shape(cities_count_df |> filter(!is.na(frac))) +
  tm_polygons(
    fill = "frac",
    col = "white",
    lwd = 0.5,
    fill.scale = tm_scale_intervals(
      values = "Reds",
      values.range = c(0.1, 1),
      breaks = pew_breaks
    ),
    fill.legend = tm_legend(
      title = "Fraction of Surveys",
      title.size = 1,
      na.show = FALSE
    ),
    fill_alpha = 0.8
  ) +
  tm_layout(
    main.title = "2007-2009 Pew Survey Locations",
    legend.position = tm_pos_in("right", "top")
  )

# Gallup Maps

g_0709 <- indf |> filter(YEAR_WAVE %in% c(2007:2009) & !is.na(state)) |> select(state)

geos <- unique(g_0709$state) |> as.character() |> as.data.frame() |> geocode(state = `as.character(unique(g_0709$state))`) |> rename(state = `as.character(unique(g_0709$state))`)

g_0709 <- g_0709 |> left_join(geos)


g_1618 <- indf |> filter(YEAR_WAVE %in% c(2016:2018) & !is.na(state)) |> select(state)

geos <- unique(g_1618$state) |> as.character() |> as.data.frame() |> geocode(state = `as.character(unique(g_1618$state))`) |> rename(state = `as.character(unique(g_1618$state))`)

g_1618 <- g_1618 |> left_join(geos)

g0709sf <- g_0709 %>% 
  mutate_at(vars(long, lat), as.numeric) %>% 
  st_as_sf(
    coords = c("long", "lat"),
    agr = "constant",
    crs = 4326, # WGS84
    stringsAsFactors = F,
    remove = T
  )

g1618sf <- g_1618 %>% 
  mutate_at(vars(long, lat), as.numeric) %>% 
  st_as_sf(
    coords = c("long", "lat"),
    agr = "constant",
    crs = 4326, # WGS84
    stringsAsFactors = F,
    remove = T
  )

# put schools within polygons
g0709_in_poly <- st_join(g0709sf, sta, join = st_within)
g1618_in_poly <- st_join(g1618sf, sta, join = st_within)

# Map 2016/2018
g1618_count <- count(as_tibble(g1618_in_poly), shapeID)
g1618_count$frac <- g1618_count$n / nrow(g_1618)
g1618_count_df <- left_join(sta, g1618_count)

# Map 2007/2009
g0709_count <- count(as_tibble(g0709_in_poly), shapeID)
g0709_count$frac <- g0709_count$n / nrow(g_0709)
g0709_count_df <- left_join(sta, g0709_count)

# Shared breaks for aligned color scales
gallup_breaks <- pretty(c(g1618_count_df$frac, g0709_count_df$frac), na.rm = TRUE)

# Little maps
tmap_mode("plot")
g1618_map <- tm_shape(g1618_count_df) +
  tm_polygons(fill = "grey85", col = "white", lwd = 0.5) +
  tm_shape(g1618_count_df |> filter(!is.na(frac))) +
  tm_fill(
    fill = "frac",
    fill.scale = tm_scale_intervals(
      values = "Blues",
      values.range = c(0.1, 1),
      breaks = gallup_breaks
    ),
    fill.legend = tm_legend(
      title = "Fraction of Surveys",
      title.size = 1
    ),
    fill_alpha = 0.8
  ) +
  tm_layout(
    main.title = "2016-2018 Gallup Survey Locations",
    legend.position = tm_pos_in("right", "top")
  )

g0709_map <- tm_shape(g0709_count_df) +
  tm_polygons(fill = "grey85", col = "white", lwd = 0.5) +
  tm_shape(g0709_count_df |> filter(!is.na(frac))) +
  tm_fill(
    fill = "frac",
    fill.scale = tm_scale_intervals(
      values = "Blues",
      values.range = c(0.1, 1),
      breaks = gallup_breaks
    ),
    fill.legend = tm_legend(
      title = "Fraction of Surveys",
      title.size = 1
    ),
    fill_alpha = 0.8
  ) +
  tm_layout(
    main.title = "2007-2009 Gallup Survey Locations",
    legend.position = tm_pos_in("right", "top")
  )

fourplot <- tmap_arrange(g1618_map, g0709_map, states_map, cities_map, ncol = 2)
tmap_save(fourplot, filename = here::here("results/figs/fig2-3.png"), height = 8, width = 10, units = "in", dpi = 320)

# IIPO

# Just to make it clear, this is just showing the plots of regions


ipo <- states_sf |> filter(states %in% c("Delhi", "Maharashtra", "West Bengal", "Tamil Nadu")) |> group_by(states) |> summarise(state = first(states), 
                                                                                                                             geometry = first(geometry))

ipo_in_poly <- st_join(ipo, sta, join = st_within)

ipo_count <- count(as_tibble(ipo_in_poly), shapeID)
ipo_count$frac <- .25
ipo_count_df <- left_join(sta, ipo_count)
# Little maps
tmap_mode("plot")
ipo_map <- tm_shape(ipo_count_df) +
  tm_polygons(fill = "grey85", col = "white", lwd = 0.5) +
  tm_shape(ipo_count_df |> filter(!is.na(frac))) +
  tm_fill(
    fill = "frac",
    fill.scale = tm_scale_categorical(
      values = "brewer.greens",
      values.range = c(0.9, 1)
    ),
    fill.legend = tm_legend(
      title = "Fraction of Surveys",
      title.size = 1
    ),
    fill_alpha = 0.8
  ) +
  tm_layout(
    main.title = "1959-2001 IIPO Survey Locations",
    legend.position = tm_pos_in("right", "top")
  )
tmap_save(file = here::here("results/figs/fig2-2.png"), ipo_map, width = 7, height = 4, dpi = 320)

# ── Figure 2-4: IIOPO National Time Series (All Three Data Sources Combined) ──
# ── Figure 2-5: IIOPO City-Level Time Series ─────────────────────────────────

######### Roper Analyses ############



# Analyses ----------------------------------------------------------------

load(here::here("data/roper_allyears.RData"))

df$usa_bin <- ifelse(df$approve_usa %in% c("Very good", "Good"), 1, 0)
df$usanum <- as.numeric(forcats::fct_rev(df$approve_usa))

df$ussr_bin <- ifelse(df$approve_ussr %in% c("Very good", "Good"), 1, 0)
df$ussrnum <- as.numeric(forcats::fct_rev(df$approve_ussr))

df$cn_bin <- ifelse(df$approve_cn %in% c("Very good", "Good"), 1, 0)
df$cnnum <- as.numeric(forcats::fct_rev(df$approve_cn))

df$ba_bin <- ifelse(df$approve_ba %in% c("Very good", "Good"), 1, 0)
df$banum <- as.numeric(forcats::fct_rev(df$approve_ba))

df$pk_bin <- ifelse(df$approve_pk %in% c("Very good", "Good"), 1, 0)
df$pknum <- as.numeric(forcats::fct_rev(df$approve_pk))


### USA Models

mod_usa_bin <- lm_robust(usa_bin ~ city + sex + age + educ + income_bin, data = df, fixed_effects = ~ wave + religion + occup)
mod_usa_num <- lm_robust(usanum ~ city + sex + age + educ + income_bin, data = df, fixed_effects = ~ wave + religion + occup)

### USSR Models

mod_ussr_bin <- lm_robust(ussr_bin ~ city + sex + age + educ + income_bin, data = df, fixed_effects = ~ wave + religion + occup)
mod_ussr_num <- lm_robust(ussrnum ~ city + sex + age + educ + income_bin, data = df, fixed_effects = ~ wave + religion + occup)

### China Models

mod_cn_bin <- lm_robust(cn_bin ~ city + sex + age + educ + income_bin, data = df, fixed_effects = ~ wave + religion + occup)
mod_cn_num <- lm_robust(cnnum ~ city + sex + age + educ + income_bin, data = df, fixed_effects = ~ wave + religion + occup)

### Bangladesh Models

mod_ba_bin <- lm_robust(ba_bin ~ city + sex + age + educ + income_bin, data = df, fixed_effects = ~ wave + religion + occup)
mod_ba_num <- lm_robust(banum ~ city + sex + age + educ + income_bin, data = df, fixed_effects = ~ wave + religion + occup)

### Pakistan Models

mod_pk_bin <- lm_robust(pk_bin ~ city + sex + age + educ + income_bin, data = df, fixed_effects = ~ wave + religion + occup)
mod_pk_num <- lm_robust(pknum ~ city + sex + age + educ + income_bin, data = df, fixed_effects = ~ wave + religion + occup)

time_df <- df |> group_by(wave, city) |> 
                summarise(USA = mean(usanum, na.rm = T),
                          `Russia/USSR` = mean(ussrnum,na.rm = T),
                          China = mean(cnnum,na.rm = T),
                          Pakistan = mean(pknum,na.rm = T),
                          Bangladesh = mean(banum,na.rm = T)) |> 
                pivot_longer(!c(wave, city),
                             names_to = "country",
                             values_to = "approval") |> 
                mutate(year = as.numeric(gsub("[[:alpha:]]", ".5", wave)),
                       approval = approval - 3) |> drop_na()


# Consider excluding Pakistan, Bangladesh

time_df_3country <- time_df |> filter(country %in% c("USA", "Russia/USSR", "China"))

natl_avg <- time_df_3country |> group_by(year, country) |> 
  summarise(approval = mean(approval))  |> mutate(year = lubridate::date_decimal(year))

time_df_3country$city <- relevel(factor(time_df_3country$city), ref = "Delhi" ) # This is to get the colors/order to match IIOPO


timeplot <- ggplot(time_df_3country) + geom_hline(yintercept = 0, color = "darkgrey") +
  geom_smooth(aes(x = year, y = approval, color= city), se = F, span = .2, lwd=1.1)  +
  geom_point(aes(x = year, y = approval, color= city), pch = 20, alpha = .7) +
  facet_wrap(~country+city, nrow = 3) +
  theme_bw(base_size = 14) + scale_color_ptol() + ylim(c(-2,3)) +
  scale_y_continuous(breaks = c(-2,-1,0,1,2,3),
                     labels =  c("Very Bad", "Bad", "Neither\nGood nor Bad", "Good", "Very Good", "X")) +
  scale_x_continuous(breaks = c(1980, 1990, 2000)) +
  labs(title= "Indian (Urban) Public Attitudes Over Time",
       subtitle = "IIOPO Survey Data across 40 waves",
       x = "Year",
       y = "Average Attitude",
       color = "City")
ggsave(filename = here::here("results/figs/fig2-5.png"), plot = timeplot, width = 8, height = 5, dpi = "retina")


# Pre-Microdata -----------------------------------------------------------

# Pull in data from the old topline reports (before Roper Microdata) and plot

old <- readxl::read_xlsx(here::here("data/iiopo-59-88.xlsx"), sheet = "All")

old$Country <- ifelse(old$Country == "USSR", "Russia/USSR", old$Country)

old_only <- old |> select(Country, Wave, Avg) |> filter(Wave < mean(Wave))  |> rename("approval"= "Avg",
                                                    "year" = "Wave",
                                                    "country" = "Country") 


time_df_natl <- bind_rows(old_only, natl_avg)


timeplot_natl <- ggplot(time_df_natl) + geom_hline(yintercept = 0, color = "darkgrey") +
  geom_smooth(aes(x = year, y = approval), se = F, span = .2, lwd=1.1)  +
  geom_point(aes(x = year, y = approval), pch = 20, alpha = .7) +
  geom_vline(xintercept = as.POSIXct("1975-01-01 UTC"), lty = 2, color = "grey") +
  geom_text(aes(x = as.POSIXct("1974-09-01 UTC"), y = -1.27, label = "Microdata Available"), color = "grey", angle = 90, size = 3) +
  facet_wrap(~country, nrow = 3) +
  theme_bw(base_size = 14) + scale_color_ptol() + ylim(c(-2,3)) +
  scale_y_continuous(breaks = c(-2,-1,0,1,2,3),
                     labels =  c("Very Bad", "Bad", "Neither\nGood nor Bad", "Good", "Very Good", "X")) +
  labs(title= "Indian (Urban) Public Attitudes Over Time",
       subtitle = "IIOPO Survey Data across 42 Years",
       x = "Year",
       y = "Average Attitude")
ggsave(filename = here::here("results/figs/fig2-4.png"), plot = timeplot_natl, width = 8, height =5, dpi = "retina")
 




# ── Figure 2-7: Distribution of DK Rates ─────────────────────────────────────
# ── Figure 2-8: Predicted DK Count by Region and SES ────────────────────────

# Using Gallup because it has the most IR questions
# Fix DK variables and region coding
indf$region <- relevel(factor(indf$region), ref = "North")
indf$emply <- ifelse(indf$employed_2010 %in% c("Employed full time for an employer",
                                               "Employed full time for self",
                                               "Employed part time do not want full time",
                                               "Employed part time want full time"), 1, 0)

############### DK/NA Per Question ##############
# Pre-computed DK rates (from full survey, summarized to avoid distributing
# unnecessary individual-level data from proprietary Gallup dataset)
dfna <- read.csv(here::here("data/dk_rates.csv")) |> arrange(desc(prop_na))
dfna$var <- as.character(dfna$var)

points <- cbind.data.frame(labels = c("Do you approve or disapprove of the job\n performance of the leadership of Germany?",
                                      "Do you approve or disapprove of the job\n performance of the leadership of Japan??",
                                      "Do you think Pakistan is doing enough\n to control cross-border terrorism?",
                                      "Do you think Indo-China relations have\n improved, declined, or stayed the same?"),
                           x      = c(0.5533370, 0.2893682833, 0.1058611254, 0.0160407661),
                           y      = rep(15, times = 4))

fig2_7 <- ggplot(dfna[which(dfna$prop_na > 0.0000),]) + geom_density(aes(prop_na), color = "darkgrey", fill = "darkgrey") +
  theme_minimal(base_size = 14) + geom_vline(xintercept = points$x, color = "navy", data = points) +
  geom_text(data=points, aes(x = x, y = y, label = labels), color = "navy", angle = 90, size = 4) +
  labs(title = "Distribution of DK Rates across FP Questions",
       subtitle = "Pooled Gallup World Poll Survey of Indian Foreign Policy Opinion",
       x = "Proportion of DK responses",
       y = "Density")
ggsave(filename = here::here("results/figs/fig2-7.png"), plot = fig2_7, width=7, height=7, dpi = "retina")

# Modeling DK/NA ---------------------------------------------------------
na_mod <- lm_robust(DK_count ~ urban +  hh_income_localcurrency +
               female + age + emply + region + educ + YEAR_CALENDAR,
             data = indf)




# Predictions

north_lowses_male <- predict(na_mod, newdata = indf %>% filter(region == "North", educ == "elem", urban == "Rural", hh_income_localcurrency < 42000), type = "response", se.fit=T)
north_highses_male <- predict(na_mod, newdata = indf %>% filter(region == "North", educ == "undergrad", urban == "Urban", hh_income_localcurrency > 120000), type = "response", se.fit=T)

south_lowses_male <- predict(na_mod, newdata = indf %>% filter(region == "South", educ == "elem", urban == "Rural", hh_income_localcurrency < 42000), type = "response", se.fit=T)
south_highses_male <- predict(na_mod, newdata = indf %>% filter(region == "South", educ == "undergrad", urban == "Urban", hh_income_localcurrency > 120000), type = "response", se.fit=T)

east_lowses_male <- predict(na_mod, newdata = indf %>% filter(region == "East", educ == "elem", urban == "Rural", hh_income_localcurrency < 42000), type = "response", se.fit=T)
east_highses_male <- predict(na_mod, newdata = indf %>% filter(region == "East", educ == "undergrad", urban == "Urban", hh_income_localcurrency > 120000), type = "response", se.fit=T)

west_lowses_male <- predict(na_mod, newdata = indf %>% filter(region == "West", educ == "elem", urban == "Rural", hh_income_localcurrency < 42000), type = "response", se.fit=T)
west_highses_male <- predict(na_mod, newdata = indf %>% filter(region == "West", educ == "undergrad", urban == "Urban", hh_income_localcurrency > 120000), type = "response", se.fit=T)

meanr <- function(x){
  mean(x$fit, na.rm = T)
}
ser <- function(x){
  mean(x$se.fit, na.rm = T)
}

dk_plots <- cbind.data.frame(region = factor(rep(c("North", "South", "East", "West"), each = 2), levels = c("North", "West", "East", "South")),
                               ses = factor(rep(c("Low", "High"), times = 4), levels = c("Low", "High")),
                               pp = sapply(list(north_lowses_male, north_highses_male,
                                                south_lowses_male, south_highses_male,
                                                east_lowses_male, east_highses_male,
                                                west_lowses_male, west_highses_male), FUN = meanr),
                               se = sapply(list(north_lowses_male, north_highses_male,
                                                south_lowses_male, south_highses_male,
                                                east_lowses_male, east_highses_male,
                                                west_lowses_male, west_highses_male), FUN = ser))
dk_plots$cilo <- dk_plots$pp - 1.96*dk_plots$se
dk_plots$cihi <- dk_plots$pp + 1.96*dk_plots$se

dk_byregion <- ggplot(dk_plots, aes(x = ses, y = pp, group = region, color = region)) + 
  geom_errorbar(aes(ymin=cilo, ymax=cihi), width = .1) +
  geom_point(aes(y = pp, x = ses, color = region), pch = 20) + 
  geom_line(lty = 2, alpha= .5) +
  facet_wrap(~region, ncol = 2) + theme_bw(base_size = 14) + scale_color_ptol() +
  labs(title = "Predicted Count of Don't Know Answers",
       subtitle = "Disaggregated by Region, SES",
       x = "SES",
       y = "DK Count",
       color = "Region",
       caption = "Low SES: Elementary education, 1st quartile HH income, Rural resident\nHigh SES: College education, 4th quartile HH income, Urban resident")
ggsave(filename = here::here("results/figs/fig2-8.png"), plot = dk_byregion, width = 8, height = 5, dpi = "retina")



