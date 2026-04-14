# --------------------------------------------
#
# Author: Aidan Milliff and Paul Staniland
# Copyright (c) Aidan Milliff, 2026
# Email:  milliff.a@gmail.com
#
# Script Name: 04_russia-analysis.R
#
# Script Description: Russia/USSR approval analysis using IIOPO, Gallup, and Pew data. Part 5 of the Element.
#
# Produces: fig5-1.png through fig5-4.png
#
# --------------------------------------------

library(here)


# SET OPTIONS ---------------------------------------
cat("SETTING OPTIONS... \n\n", sep = "")



# INSTALL PACKAGES & LOAD LIBRARIES -----------------
cat("INSTALLING PACKAGES & LOADING LIBRARIES... \n\n", sep = "")
packages <- c("tidyverse", "ggthemes", "lmtest", "sandwich", "readroper", "estimatr") # list of packages to load
n_packages <- length(packages) # count how many packages are required

new.pkg <- packages[!(packages %in% installed.packages())] # determine which packages aren't installed

# install missing packages
if(length(new.pkg)){
  install.packages(new.pkg)
}

# load all requried libraries
for(n in 1:n_packages){
  cat("Loading Library #", n, " of ", n_packages, "... Currently Loading: ", packages[n], "\n", sep = "")
  lib_load <- paste("library(\"",packages[n],"\")", sep = "") # create string of text for loading each library
  eval(parse(text = lib_load)) # evaluate the string to load the library
}

# LOAD FUNCTIONS ------------------------------------
# space reserved for your functions

mod_to_table_hc2 <- function(mod){
  bread <- vcov(mod)
  est   <- sandwich::estfun(mod)
  meat  <- t(est)%*%est
  sw    <- bread%*%meat%*%bread
  tab   <- coeftest(mod, sw)
  out   <- broom::tidy(tab) %>% mutate(ci_lo = estimate - 1.96*std.error,
                                       ci_hi = estimate + 1.96*std.error)
  return(out)
}
# 
##### Load Cleaned IIOPO, Pew, Gallup Data #####



# IIOPO Load --------------------------------------------------------------

load(here::here("data/roper_allyears.RData")) # IIOPO

# Create some additional variables 
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



# Gallup Load -------------------------------------------------------------

load(here::here("data/gallup_indf.RData"))


indf$us_leader_bin <- ifelse(indf$leadershipOpinion_usa == "Approve", 1,
                             ifelse(indf$leadershipOpinion_usa == "Disapprove", 0, NA))
indf$cn_leader_bin <- ifelse(indf$leadershipOpinion_china == "Approve", 1,
                             ifelse(indf$leadershipOpinion_china == "Disapprove", 0, NA))
indf$ru_leader_bin <- ifelse(indf$leadershipOpinion_russia == "Approve", 1,
                             ifelse(indf$leadershipOpinion_russia == "Disapprove", 0, NA))

# Pew Load ----------------------------------------------------------------

load(here::here("data/pew_clean.RData"))

# Over Time Plots ----------------------------------------------------------

time_df_natl <- df |> group_by(wave) |> 
  summarise(USA = mean(usanum, na.rm = T),
            `Russia/USSR` = mean(ussrnum,na.rm = T),
            China = mean(cnnum,na.rm = T),
            Pakistan = mean(pknum,na.rm = T),
            Bangladesh = mean(banum,na.rm = T)) |> 
  pivot_longer(!c(wave),
               names_to = "country",
               values_to = "approval") |> 
  mutate(year = as.numeric(gsub("[[:alpha:]]", ".5", wave)),
         approval = approval - 3) |> drop_na() |> mutate(year = lubridate::date_decimal(year))




# Pre-Microdata -----------------------------------------------------------

# Pull in data from the old topline reports (before Roper Microdata) and plot

old <- readxl::read_xlsx(here::here("data/iiopo-59-88.xlsx"), sheet = "All")

old$Country <- ifelse(old$Country == "USSR", "Russia/USSR", old$Country)

old_only <- old |> select(Country, Wave, Avg) |> filter(Wave < mean(Wave))  |> rename("approval"= "Avg",
                                                                                      "year" = "Wave",
                                                                                      "country" = "Country") 


time_df_natl <- bind_rows(old_only, time_df_natl)

time_df_long_ru <- time_df_natl |> filter(country == "Russia/USSR")

timeplot_natl <- ggplot(time_df_long_ru) + geom_hline(yintercept = 0, color = "darkgrey") +
  geom_smooth(aes(x = year, y = approval), se = F, span = .2, lwd=1.1, color = ptol_pal()(2)[2])  +
  geom_point(aes(x = year, y = approval), pch = 20, alpha = .7, color = ptol_pal()(2)[2]) +
  geom_vline(xintercept = as.POSIXct("1971-09-01 UTC"), lty = 2, color = "grey") +
  geom_text(aes(x = as.POSIXct("1971-02-18 UTC"), y = -1.27, label = "Indo-Soviet Treaty"), color = "grey", angle = 90, size = 4) +
  geom_vline(xintercept = as.POSIXct("1991-12-26 UTC"), lty = 2, color = "grey") +
  geom_text(aes(x = as.POSIXct("1991-06-26 UTC"), y = -1.27, label = "USSR Dissolves"), color = "grey", angle = 90, size = 4) +
  geom_vline(xintercept = as.POSIXct("1975-01-01 UTC"), lty = 2, color = "grey") +
  geom_text(aes(x = as.POSIXct("1974-06-01 UTC"), y = -1.27, label = "Microdata Available"), color = "grey", angle = 90, size = 4) +
  theme_bw(base_size = 14) + scale_color_ptol() + ylim(c(-2,3)) +
  scale_y_continuous(breaks = c(-2,-1,0,1,2,3),
                     labels =  c("Very Bad", "Bad", "Neither\nGood nor Bad", "Good", "Very Good", "X"), limits = c(-2,2)) +
  labs(title= "Indian (Urban) Public Attitudes Over Time",
       subtitle = "IIOPO Survey Data across 42 Years",
       x = "Year",
       y = "Average Attitude")
ggsave(filename = here::here("results/figs/fig5-1.png"),plot = timeplot_natl, width = 8, height = 5, units = "in", dpi = "retina")

# Pew, Gallup OT Plots ----------------------------------------------------
# Gallup
indf <- indf |> mutate(region = case_when(state == "Chhattisgarh" ~ "East",
                                          state == "Madhya Pradesh" ~ "North",
                                          is.na(state) & region == "Central" ~ "North",
                                          .default = as.character(region)))
indf$region <- relevel(factor(indf$region), ref = "North")
time_df_reg <- indf |> group_by(YEAR_WAVE, region) |> 
  summarise(USA = mean(us_leader_bin, na.rm = T),
            Russia = mean(ru_leader_bin,na.rm = T),
            China = mean(cn_leader_bin,na.rm = T)) |> 
  pivot_longer(!c(YEAR_WAVE, region),
               names_to = "country",
               values_to = "approval") |> 
  drop_na()

time_df_reg$region <- factor(time_df_reg$region, levels = c("West", "East", "North", "South")) # This is to get the colors/order to match IIOPO


# 4477aa to mumbai / west
# 117733 Kolkata/East
# DDCC77 Delhi/North
# CC6677 Chennai/South

time_df_natl <- indf |> group_by(YEAR_WAVE) |> 
  summarise(USA = mean(us_leader_bin, na.rm = T),
            Russia = mean(ru_leader_bin,na.rm = T),
            China = mean(cn_leader_bin,na.rm = T)) |> 
  pivot_longer(!c(YEAR_WAVE),
               names_to = "country",
               values_to = "approval") |> 
  drop_na()

time_df_natl_ru <- time_df_natl |> filter(country == "Russia")

# Pew Plots 

time_df_natl_pew <- dfall |> group_by(YEAR) |> 
  summarise(USA = mean(approve_us_bin, na.rm = T),
            Russia = mean(approve_ru_bin,na.rm = T),
            China = mean(approve_cn_bin,na.rm = T)) |> 
  pivot_longer(!c(YEAR),
               names_to = "country",
               values_to = "approval") |> 
  drop_na() |> filter(country == "Russia") |> select(-country)

time_df_natl_ru <- time_df_natl_ru |> select(-country) |> rename(YEAR = YEAR_WAVE)

time_df_natl_pew$source <- "Pew Global Attitudes\n(Country)"
time_df_natl_ru$source <- "Gallup World Poll\n(Leadership)"

pew_gallup_plot <- rbind.data.frame(time_df_natl_pew, time_df_natl_ru)

pew_gallup <- ggplot(pew_gallup_plot) + geom_hline(yintercept = .5, color = "darkgrey") +
  geom_smooth(aes(x = YEAR, y = approval, color= source), se = F, span = .8, lwd=1.1)  +
  geom_point(aes(x = YEAR, y = approval, color= source), pch = 16, alpha = .7) +
  theme_bw(base_size = 14) + scale_color_ptol() +
  ylim(c(0,1)) +
  labs(title= "Indian Public Attitudes Over Time",
       subtitle = "Pew and Gallup Data from the 21st Century",
       x = "Year",
       y = "% Approving/Favorable",
       color = "Source")
ggsave(filename = here::here("results/figs/fig5-2.png"),plot = pew_gallup, width = 8, height = 5, units = "in", dpi = "retina")



# Structure of Opinions Stuff ---------------------------------------------

# Pew
dfall$region <- relevel(factor(dfall$region), ref = "North")
pew_pooled <- lm_robust(approve_ru_bin ~ region + religion + edu + income + male + age, data= dfall, fixed_effects = ~YEAR)
pew_pooled

pew_party <- lm_robust(approve_ru_bin ~ region + religion + edu + income + male + age +partyid, data= dfall, fixed_effects = ~YEAR)
pew_party

# Gallup

gallup_pooled <- lm_robust(ru_leader_bin ~ female + age + educ  + percap_income_localcurrency + region, data= indf, fixed_effects = ~YEAR_WAVE)
gallup_pooled

# IIPO
df$city <- relevel(factor(df$city), ref = "Delhi")
iipo_pooled <- lm_robust(ussr_bin ~ city + sex + age + educ  + income + religion, data = df, fixed_effects = ~ wave)
iipo_pooled

# Regionplot

regionplot <- modelsummary::modelplot(list("Pew (2002-2018)" = pew_pooled,
                                           "Pew (w/ party preference, 2009-2018)" = pew_party,
                                           "Gallup (2006-2018)" = gallup_pooled,
                                           "IIPO (1975-2001)" = iipo_pooled),
                                      coef_map = c("regionEast"= "East",
                                                   "regionSouth" = "South",
                                                   "regionWest" = "West",
                                                   "cityMumbai" = "West",
                                                   "cityKolkata" = "East",
                                                   "cityChennai" = "South")) +
  geom_vline(xintercept = 0, lty = 2, color  = "darkgrey") +
  scale_color_ptol(0) +
  labs(title = "Regional Variation in Approval of Russia",
       subtitle = "Evidence from Modern and Historical Surveys",
       caption = "Coefficients are compared to approval in NORTH India\nModels include coefs. for income, age, education, gender, and sometimes religion, partyid.\nAll models include year FE, HC2 errors.")

ggsave(filename = here::here("results/figs/fig5-3.png"),plot = regionplot, width = 8, height = 5, units = "in", dpi = "retina")



# Party Stuff -------------------------------------------------------------

# Thought not to be very partisan in India, largely true
# Pew, look at party stuff across 2009, 2014

# Replicate the thigns in Pew Analysis

# Similar results, majorly by year

dfall$ndagov <- ifelse(dfall$YEAR >= 2014, 1, 0)
dfall$bjp <- ifelse(dfall$partyid == "BJP", 1, 0)
dfall <- dfall |> mutate(threeparty = case_when(partyid == "BJP" ~ "BJP",
                                                partyid == "INC" ~ "INC",
                                                partyid %in% c("NoParty", "Other") ~ "Other"))

party_in_power <- lm_robust(approve_ru_bin ~ bjp*ndagov + region + religion + male + age + edu + income, data = dfall)

threeparty_in_power <- lm_robust(approve_ru_bin ~ ndagov*threeparty + region + religion + male + age + edu + income, data = dfall)
# When the BJP is in power nationally, BJP supporters express more favorable views toward China compared to the Singh era

pp_pre <- predict(party_in_power, newdata = dfall |> filter(ndagov == 0 & bjp == 1), type = "response", se.fit=T)
pp_post <- predict(party_in_power, newdata = dfall |> filter(ndagov == 1 & bjp == 1), type = "response", se.fit=T)
pp_pre_other <- predict(party_in_power, newdata = dfall |> filter(ndagov == 0 & bjp == 0), type = "response", se.fit=T)
pp_post_other <- predict(party_in_power, newdata = dfall |> filter(ndagov == 1 & bjp == 0), type = "response", se.fit=T)

bjp_plotframe <- cbind.data.frame(Period = rep(c("Pre-2014", "Post-2014"), times = 2),
                                  Party = rep(c("BJP", "Other"), each = 2),
                                  pp = c(mean(pp_pre$fit, na.rm=T), mean(pp_post$fit, na.rm=T), mean(pp_pre_other$fit, na.rm=T), mean(pp_post_other$fit, na.rm=T)),
                                  se = c(mean(pp_pre$se.fit, na.rm = T), mean(pp_post$se.fit, na.rm = T), mean(pp_pre_other$se.fit, na.rm=T), mean(pp_post_other$se.fit, na.rm=T)))
bjp_plotframe$cilo <- bjp_plotframe$pp - 1.96*bjp_plotframe$se  
bjp_plotframe$cihi <- bjp_plotframe$pp + 1.96*bjp_plotframe$se
bjp_plotframe$Period <- relevel(factor(bjp_plotframe$Period), ref = "Pre-2014")

#Threeparty

pp_pre_bjp <- predict(threeparty_in_power, newdata = dfall |> filter(ndagov == 0 & threeparty == "BJP"), type = "response", se.fit=T)
pp_post_bjp <- predict(threeparty_in_power, newdata = dfall |> filter(ndagov == 1 & threeparty == "BJP"), type = "response", se.fit=T)
pp_pre_inc <- predict(threeparty_in_power, newdata = dfall |> filter(ndagov == 0 & threeparty == "INC"), type = "response", se.fit=T)
pp_post_inc <- predict(threeparty_in_power, newdata = dfall |> filter(ndagov == 1 & threeparty == "INC"), type = "response", se.fit=T)
pp_pre_oth <- predict(threeparty_in_power, newdata = dfall |> filter(ndagov == 0 & threeparty == "Other"), type = "response", se.fit=T)
pp_post_oth <- predict(threeparty_in_power, newdata = dfall |> filter(ndagov == 1 & threeparty == "Other"), type = "response", se.fit=T)

threeparty_plotframe <- cbind.data.frame(Period = rep(c("Pre-2014", "Post-2014"), times = 3),
                                         Party = rep(c("BJP", "INC", "Other"), each = 2),
                                         pp = c(mean(pp_pre_bjp$fit, na.rm=T), mean(pp_post_bjp$fit, na.rm=T), mean(pp_pre_inc$fit, na.rm=T), mean(pp_post_inc$fit, na.rm=T),  mean(pp_pre_other$fit, na.rm=T), mean(pp_post_other$fit, na.rm=T)),
                                         se = c(mean(pp_pre_bjp$se.fit, na.rm = T), mean(pp_post_bjp$se.fit, na.rm = T), mean(pp_pre_inc$se.fit, na.rm=T), mean(pp_post_inc$se.fit, na.rm=T), mean(pp_pre_other$se.fit, na.rm=T), mean(pp_post_other$se.fit, na.rm=T)))
threeparty_plotframe$cilo <- threeparty_plotframe$pp - 1.96*threeparty_plotframe$se  
threeparty_plotframe$cihi <- threeparty_plotframe$pp + 1.96*threeparty_plotframe$se
threeparty_plotframe$Period <- relevel(factor(threeparty_plotframe$Period), ref = "Pre-2014")

# Big Party Interaction models --------------------------------------------

pew_partymod <- lm_robust(approve_ru_bin ~ YEAR*bjp*region + religion + male + age + edu + income, data = dfall)

north_09_o <- predict(pew_partymod, newdata = dfall %>% filter(region == "North", YEAR == 2009, bjp == 0), type = "response", se.fit=T)
north_11_o <- predict(pew_partymod, newdata = dfall %>% filter(region == "North", YEAR == 2011, bjp == 0), type = "response", se.fit=T)
north_14_o <- predict(pew_partymod, newdata = dfall %>% filter(region == "North", YEAR == 2014, bjp == 0), type = "response", se.fit=T)
north_16_o <- predict(pew_partymod, newdata = dfall %>% filter(region == "North", YEAR == 2016, bjp == 0), type = "response", se.fit=T)
north_18_o <- predict(pew_partymod, newdata = dfall %>% filter(region == "North", YEAR == 2018, bjp == 0), type = "response", se.fit=T)

east_09_o <- predict(pew_partymod, newdata = dfall %>% filter(region == "East", YEAR == 2009, bjp == 0), type = "response", se.fit=T)
east_11_o <- predict(pew_partymod, newdata = dfall %>% filter(region == "East", YEAR == 2011, bjp == 0), type = "response", se.fit=T)
east_14_o <- predict(pew_partymod, newdata = dfall %>% filter(region == "East", YEAR == 2014, bjp == 0), type = "response", se.fit=T)
east_16_o <- predict(pew_partymod, newdata = dfall %>% filter(region == "East", YEAR == 2016, bjp == 0), type = "response", se.fit=T)
east_18_o <- predict(pew_partymod, newdata = dfall %>% filter(region == "East", YEAR == 2018, bjp == 0), type = "response", se.fit=T)

south_09_o <- predict(pew_partymod, newdata = dfall %>% filter(region == "South", YEAR == 2009, bjp == 0), type = "response", se.fit=T)
south_11_o <- predict(pew_partymod, newdata = dfall %>% filter(region == "South", YEAR == 2011, bjp == 0), type = "response", se.fit=T)
south_14_o <- predict(pew_partymod, newdata = dfall %>% filter(region == "South", YEAR == 2014, bjp == 0), type = "response", se.fit=T)
south_16_o <- predict(pew_partymod, newdata = dfall %>% filter(region == "South", YEAR == 2016, bjp == 0), type = "response", se.fit=T)
south_18_o <- predict(pew_partymod, newdata = dfall %>% filter(region == "South", YEAR == 2018, bjp == 0), type = "response", se.fit=T)

west_09_o <- predict(pew_partymod, newdata = dfall %>% filter(region == "West", YEAR == 2009, bjp == 0), type = "response", se.fit=T)
west_11_o <- predict(pew_partymod, newdata = dfall %>% filter(region == "West", YEAR == 2011, bjp == 0), type = "response", se.fit=T)
west_14_o <- predict(pew_partymod, newdata = dfall %>% filter(region == "West", YEAR == 2014, bjp == 0), type = "response", se.fit=T)
west_16_o <- predict(pew_partymod, newdata = dfall %>% filter(region == "West", YEAR == 2016, bjp == 0), type = "response", se.fit=T)
west_18_o <- predict(pew_partymod, newdata = dfall %>% filter(region == "West", YEAR == 2018, bjp == 0), type = "response", se.fit=T)

north_09_b <- predict(pew_partymod, newdata = dfall %>% filter(region == "North", YEAR == 2009, bjp == 1), type = "response", se.fit=T)
north_11_b <- predict(pew_partymod, newdata = dfall %>% filter(region == "North", YEAR == 2011, bjp == 1), type = "response", se.fit=T)
north_14_b <- predict(pew_partymod, newdata = dfall %>% filter(region == "North", YEAR == 2014, bjp == 1), type = "response", se.fit=T)
north_16_b <- predict(pew_partymod, newdata = dfall %>% filter(region == "North", YEAR == 2016, bjp == 1), type = "response", se.fit=T)
north_18_b <- predict(pew_partymod, newdata = dfall %>% filter(region == "North", YEAR == 2018, bjp == 1), type = "response", se.fit=T)

east_09_b <- predict(pew_partymod, newdata = dfall %>% filter(region == "East", YEAR == 2009, bjp == 1), type = "response", se.fit=T)
east_11_b <- predict(pew_partymod, newdata = dfall %>% filter(region == "East", YEAR == 2011, bjp == 1), type = "response", se.fit=T)
east_14_b <- predict(pew_partymod, newdata = dfall %>% filter(region == "East", YEAR == 2014, bjp == 1), type = "response", se.fit=T)
east_16_b <- predict(pew_partymod, newdata = dfall %>% filter(region == "East", YEAR == 2016, bjp == 1), type = "response", se.fit=T)
east_18_b <- predict(pew_partymod, newdata = dfall %>% filter(region == "East", YEAR == 2018, bjp == 1), type = "response", se.fit=T)

south_09_b <- predict(pew_partymod, newdata = dfall %>% filter(region == "South", YEAR == 2009, bjp == 1), type = "response", se.fit=T)
south_11_b <- predict(pew_partymod, newdata = dfall %>% filter(region == "South", YEAR == 2011, bjp == 1), type = "response", se.fit=T)
south_14_b <- predict(pew_partymod, newdata = dfall %>% filter(region == "South", YEAR == 2014, bjp == 1), type = "response", se.fit=T)
south_16_b <- predict(pew_partymod, newdata = dfall %>% filter(region == "South", YEAR == 2016, bjp == 1), type = "response", se.fit=T)
south_18_b <- predict(pew_partymod, newdata = dfall %>% filter(region == "South", YEAR == 2018, bjp == 1), type = "response", se.fit=T)

west_09_b <- predict(pew_partymod, newdata = dfall %>% filter(region == "West", YEAR == 2009, bjp == 1), type = "response", se.fit=T)
west_11_b <- predict(pew_partymod, newdata = dfall %>% filter(region == "West", YEAR == 2011, bjp == 1), type = "response", se.fit=T)
west_14_b <- predict(pew_partymod, newdata = dfall %>% filter(region == "West", YEAR == 2014, bjp == 1), type = "response", se.fit=T)
west_16_b <- predict(pew_partymod, newdata = dfall %>% filter(region == "West", YEAR == 2016, bjp == 1), type = "response", se.fit=T)
west_18_b <- predict(pew_partymod, newdata = dfall %>% filter(region == "West", YEAR == 2018, bjp == 1), type = "response", se.fit=T)


big_party_region_plot <- cbind.data.frame(region = factor(rep(c("North", "South", "East", "West"), each = 10), levels = c("North", "West", "East", "South")),
                                          year = rep(c("2009-06-01", "2011-04-07", "2014-01-01", "2016-05-01", "2018-06-23"), times = 8),
                                          party = factor(rep(c("non-BJP", "non-BJP", "non-BJP", "non-BJP", "non-BJP", "BJP", "BJP", "BJP","BJP","BJP"), times = 4)),
                                          pp = sapply(list(north_09_o, north_11_o, north_14_o, north_16_o, north_18_o,
                                                           north_09_b, north_11_b, north_14_b, north_16_b, north_18_b,
                                                           south_09_o, south_11_o, south_14_o, south_16_o, south_18_o,
                                                           south_09_b, south_11_b, south_14_b, south_16_b, south_18_b,
                                                           east_09_o, east_11_o, east_14_o, east_16_o, east_18_o,
                                                           east_09_b, east_11_b, east_14_b, east_16_b, east_18_b,
                                                           west_09_o, west_11_o, west_14_o, west_16_o, west_18_o,
                                                           west_09_b, west_11_b, west_14_b, west_16_b, west_18_b), FUN = meanr),
                                          se = sapply(list(north_09_o, north_11_o, north_14_o, north_16_o, north_18_o,
                                                           north_09_b, north_11_b, north_14_b, north_16_b, north_18_b,
                                                           south_09_o, south_11_o, south_14_o, south_16_o, south_18_o,
                                                           south_09_b, south_11_b, south_14_b, south_16_b, south_18_b,
                                                           east_09_o, east_11_o, east_14_o, east_16_o, east_18_o,
                                                           east_09_b, east_11_b, east_14_b, east_16_b, east_18_b,
                                                           west_09_o, west_11_o, west_14_o, west_16_o, west_18_o,
                                                           west_09_b, west_11_b, west_14_b, west_16_b, west_18_b), FUN = ser))
big_party_region_plot$cilo <- big_party_region_plot$pp - 1.96*big_party_region_plot$se
big_party_region_plot$cihi <- big_party_region_plot$pp + 1.96*big_party_region_plot$se
big_party_region_plot$year <- as.POSIXct(big_party_region_plot$year)

big_plot_byparty <- ggplot(big_party_region_plot, aes(x = year, y = pp, group = party, color = party)) + 
  geom_point(position = position_dodge(7.776e+6)) +
  geom_errorbar(aes(ymin = cilo, ymax= cihi), width = 5.592e+6,
                position = position_dodge(7.776e+6)) +
  geom_line(position = position_dodge(7.776e+6), lty = 2, alpha= .5) +
  geom_rect(aes(xmin = as.POSIXct("2014-04-07"), xmax = as.POSIXct("2014-05-16"), ymin = -Inf, ymax = Inf), fill = "darkgrey", lwd = 0, alpha = .05) +
  geom_text(aes(x = as.POSIXct("2014-01-23"), angle = 90, y = .76,
                label = "2014 Elections"), color = "darkgrey", size=3) +
  facet_wrap(~region, ncol = 2) + theme_bw(base_size = 14) + scale_color_manual(values = c("darkorange2", "chartreuse4")) +
  labs(title = "Predicted Favorability Toward Russia",
       subtitle = "Disaggregated by Region, Party",
       x = "Year",
       y = "p(Favorable)",
       color = "Party") + ylim(c(0,1))

ggsave(filename = here::here("results/figs/fig5-4.png"),plot = big_plot_byparty, width = 8, height = 5, units = "in", dpi ="retina")



#  -------------------------------------------------------------------------