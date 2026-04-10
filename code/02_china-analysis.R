# --------------------------------------------
#
# Author: Aidan Milliff and Paul Staniland
# Copyright (c) Aidan Milliff, 2026
# Email:  milliff.a@gmail.com
#
# Script Name: 02_china-analysis.R
#
# Script Description: China approval analysis using IIOPO, Gallup, and Pew data. Part 3 of the Element.
#
# Produces: fig3-1.png through fig3-9.png
#
# --------------------------------------------


# SET OPTIONS ---------------------------------------
cat("SETTING OPTIONS... \n\n", sep = "")



# INSTALL PACKAGES & LOAD LIBRARIES -----------------
cat("INSTALLING PACKAGES & LOADING LIBRARIES... \n\n", sep = "")
packages <- c("tidyverse", "ggthemes", "lmtest", "sandwich", "readroper", "estimatr", "here", "interflex") # list of packages to load
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


# Pew Load ----------------------------------------------------------------

load(here::here("data/pew_clean.RData"))

# Over Time Plots ----------------------------------------------------------

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


# IIOPO Over Time Analyses ----------------------------------------------------------------

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


# time_df_cn_natl <- time_df_natl |> filter(country =="China")
# 
# 
# timeplot <- ggplot(time_df_cn_natl) + geom_hline(yintercept = 0, color = "darkgrey") +
#   geom_smooth(aes(x = year, y = approval), se = F, span = .2, lwd=1.1, color = ptol_pal()(2)[2])  +
#   geom_point(aes(x = year, y = approval), pch = 20, alpha = .7, color = ptol_pal()(2)[2]) +
#   theme_bw() + scale_color_ptol() + 
#   scale_y_continuous(breaks = c(-2,-1,0,1,2,3),
#                      labels =  c("Very Bad", "Bad", "Neither\nGood nor Bad", "Good", "Very Good", "X"),
#                      limits = c(-2,2)) +
#   labs(title= "Indian (Urban) Public Attitudes Toward China",
#        subtitle = "IIOPO Survey Data across 40 waves",
#        x = "Year",
#        y = "Average Attitude",
#        caption = "Question Text:\nPlease give me your opinion of the countries listed on this card. First, take China.\nWould you rank China as...very good, good, neither good nor bad, or very bad?")
# timeplot





# Pre-Microdata -----------------------------------------------------------

# Pull in data from the old topline reports (before Roper Microdata) and plot

old <- readxl::read_xlsx(here::here("data/iiopo-59-88.xlsx"), sheet = "All")

old$Country <- ifelse(old$Country == "USSR", "Russia/USSR", old$Country)

old_only <- old |> select(Country, Wave, Avg) |> filter(Wave < mean(Wave))  |> rename("approval"= "Avg",
                                                                                      "year" = "Wave",
                                                                                      "country" = "Country") 


time_df_natl <- bind_rows(old_only, time_df_natl)

time_df_long_cn <- time_df_natl |> filter(country == "China")

# Pew, Gallup OT Plots ----------------------------------------------------
# Gallup

time_df_reg <- indf |> group_by(YEAR_WAVE, region) |> 
  summarise(USA = mean(us_leader_bin, na.rm = T),
            Russia = mean(ru_leader_bin,na.rm = T),
            China = mean(cn_leader_bin,na.rm = T)) |> 
  pivot_longer(!c(YEAR_WAVE, region),
               names_to = "country",
               values_to = "approval") |> 
  drop_na()

time_df_reg$region <- factor(time_df_reg$region, levels = c("West", "East", "North", "South", "Central")) # This is to get the colors/order to match IIOPO

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

time_df_natl_cn <- time_df_natl |> filter(country == "China")

# Pew Plots 

pew_natl <- readxl::read_xlsx(here::here("data/pew_over_time.xlsx"))

pew_natl <- pew_natl |> mutate(net_favorability = (`Very favorable` + `Somewhat favorable`) / (`Very favorable` + `Somewhat favorable` + `Somewhat unfavorable` + `Very unfavorable`),
                               favorability = (`Very favorable` + `Somewhat favorable`)/100) |> select(Year, net_favorability, favorability)

time_df_natl_cn <- time_df_natl_cn |> rename(Year = YEAR_WAVE, favorability = approval) |> select(-country)

pew_natl$source <- "Pew Global Attitudes\n(Country)"
time_df_natl_cn$source <- "Gallup World Poll\n(Leadership)"

pew_gallup_plot <- rbind.data.frame(pew_natl |> select(-c(net_favorability)), time_df_natl_cn)

# All three
iiopo <- time_df_long_cn |> 
   mutate(favorability = ((approval + 2)*25)/100,
          source = "IIOPO\n(Country)") |> select(year, favorability, source) |> rename(Year = year)

pew_gallup_plot$Year <- date_decimal(pew_gallup_plot$Year)

threesource <- rbind.data.frame(pew_gallup_plot, iiopo)


threesource_plot <- ggplot(threesource) + geom_hline(yintercept = .5, color = "darkgrey") +
  geom_smooth(aes(x = Year, y = favorability, color= source), se = F, method = "loess", lwd=1.1, span = .7)  +
  geom_point(aes(x = Year, y = favorability, color= source), pch = 16, alpha = .7) +
  geom_vline(xintercept = as.POSIXct("1962-10-21 UTC"), lty = 2, color = "grey") +
  geom_text(aes(x = as.POSIXct("1962-06-21 UTC"), y = .15, label = "1962 Sino-Indian War"), color = "grey", angle = 90, size = 2) +
  geom_vline(xintercept = as.POSIXct("1986-04-01 UTC"), lty = 2, color = "grey") +
  geom_text(aes(x = as.POSIXct("1985-12-01 UTC"), y = .15, label = "Sumdorong Chu Crisis"), color = "grey", angle = 90, size = 2) +
  geom_vline(xintercept = as.POSIXct("2013-04-01 UTC"), lty = 2, color = "grey") +
  geom_text(aes(x = as.POSIXct("2012-12-01 UTC"), y = .15, label = "Dalut Beg Oldi Standoff"), color = "grey", angle = 90, size = 2) +
  geom_vline(xintercept = as.POSIXct("2017-06-01 UTC"), lty = 2, color = "grey") +
  geom_text(aes(x = as.POSIXct("2017-02-1 UTC"), y = .15, label = "Doklam Crisis"), color = "grey", angle = 90, size = 2) +
  theme_bw() + scale_color_ptol() +
  ylim(c(0,1)) +
  labs(title= "Indian Public Attitudes Over Time",
       subtitle = "Combining Three Data Sources (And Sampling Methods)",
       x = "Year",
       y = "% Approving/Favorable",
       color = "Source",
       caption = "Gallup: Do you approve or disapprove of the job performance of the leadership of China?\nPew: Do you have a very/somewhat favorable, somewhat/very unfavorable opinion of China? \nIIOPO:Would you rank China as...very good, good, neither good nor bad, or very bad?")
ggsave(filename = here::here("results/figs/fig3-1.png"),plot = threesource_plot, width = 8, height = 5, units = "in", dpi = "retina")



########## Border Disputes ###########


# Evaluating Shocks (Gallup) -------------------------------------------------------

# India world poll dates
# 2012 ends jan 2013
# 2013 sep-oct
# 2014 sep oct

indf_1214 <-  indf %>% filter(YEAR_WAVE %in% c(2012, 2013, 2014))
indf_1214$YEAR_WAVE <- relevel(factor(indf_1214$YEAR_WAVE), ref="2012")
indf_1214$region    <- relevel(factor(indf_1214$region), ref="North")

indf_1214$us_leader_bin <- ifelse(indf_1214$leadershipOpinion_usa == "Approve", 1,
                                  ifelse(indf_1214$leadershipOpinion_usa == "Disapprove", 0, NA))
indf_1214$cn_leader_bin <- ifelse(indf_1214$leadershipOpinion_china == "Approve", 1,
                                  ifelse(indf_1214$leadershipOpinion_china == "Disapprove", 0, NA))

change_mod_cn_1214 <- glm(cn_leader_bin ~ YEAR_WAVE + female + age + educ  + percap_income_localcurrency  + region, data=indf_1214, family =binomial(link = "logit")) 

out_chmod_cn_1214 <- mod_to_table_hc2(change_mod_cn_1214)

change_mod_cn_1214_lm <- lm_robust(cn_leader_bin ~ YEAR_WAVE+ female + age + educ + percap_income_localcurrency + region, data = indf_1214)
change_mod_cn_1214_lm


pp12 <- predict(change_mod_cn_1214_lm, newdata = indf_1214 |> filter(YEAR_WAVE == 2012), type = "response", se.fit=T)
pp13 <- predict(change_mod_cn_1214_lm, newdata = indf_1214 |> filter(YEAR_WAVE == 2013), type = "response", se.fit=T)
pp14 <- predict(change_mod_cn_1214_lm, newdata = indf_1214 |> filter(YEAR_WAVE == 2014), type = "response", se.fit=T)


pp_plotframe <- cbind.data.frame(Year = c(as.POSIXct("2012-12-17"), as.POSIXct("2013-09-01"), as.POSIXct("2014-09-01")),
                                 pp = c(mean(pp12$fit, na.rm=T), mean(pp13$fit, na.rm=T), mean(pp14$fit, na.rm=T)),
                                 se = c(mean(pp12$se.fit, na.rm = T), mean(pp13$se.fit, na.rm = T), mean(pp14$se.fit, na.rm = T)))
pp_plotframe$cilo <- pp_plotframe$pp - 1.96*pp_plotframe$se  
pp_plotframe$cihi <- pp_plotframe$pp + 1.96*pp_plotframe$se  

depsang_pp_gallup <- ggplot(pp_plotframe) + geom_pointrange(aes(x = Year, y = pp, ymin = cilo, ymax = cihi),
                                       color = ptol_pal()(2)[2]) + geom_line(aes(x = Year, y = pp), lty = 3, color = ptol_pal()(2)[2]) +
  theme_bw() + geom_vline(xintercept = as.POSIXct("2013-04-13"), color = "darkgrey", lty = 4) +
  geom_vline(xintercept = as.POSIXct("2013-05-05"), color = "darkgrey", lty = 4) +
  geom_text(aes(x = as.POSIXct("2013-04-23"), angle = 90, y = .2, label = "Depsang Standoff"), color = "darkgrey", size=5) + ylim(c(0,1)) +
  labs(title = "Predicted Probability of Approving of China's Leadership",
       subtitle = "Gallup World Poll",
       y = "p(Approval)",
       x = "Survey Date")

ggsave(filename = here::here("results/figs/fig3-3b.png"),plot = depsang_pp_gallup, width = 8, height = 5, units = "in", dpi = "retina")

# DOKLAM

indf_1618 <-  indf %>% filter(YEAR_WAVE %in% c(2016, 2017, 2018))
indf_1618$YEAR_WAVE <- relevel(factor(indf_1618$YEAR_WAVE), ref="2016")
indf_1618$region    <- relevel(factor(indf_1618$region), ref="North")
indf_1618$us_leader_bin <- ifelse(indf_1618$leadershipOpinion_usa == "Approve", 1,
                                  ifelse(indf_1618$leadershipOpinion_usa == "Disapprove", 0, NA))
indf_1618$cn_leader_bin <- ifelse(indf_1618$leadershipOpinion_china == "Approve", 1,
                                  ifelse(indf_1618$leadershipOpinion_china == "Disapprove", 0, NA))

change_mod_cn_1618_g <- glm(cn_leader_bin ~ YEAR_WAVE + female + age + educ  + percap_income_localcurrency  + region, data=indf_1618, family =binomial(link = "logit")) 

change_mod_us_1618_lm <- lm_robust(us_leader_bin ~ YEAR_WAVE + female + age + educ + percap_income_localcurrency + region, data = indf_1618)
change_mod_cn_1618_lm <- lm_robust(cn_leader_bin ~ YEAR_WAVE + female + age + educ + percap_income_localcurrency + region, data = indf_1618)
change_mod_cn_1618_lm

pp16 <- predict(change_mod_cn_1618_lm, newdata = indf_1618 |> filter(YEAR_WAVE == 2016), type = "response", se.fit=T)
pp17 <- predict(change_mod_cn_1618_lm, newdata = indf_1618 |> filter(YEAR_WAVE == 2017), type = "response", se.fit=T)
pp18 <- predict(change_mod_cn_1618_lm, newdata = indf_1618 |> filter(YEAR_WAVE == 2018), type = "response", se.fit=T)


pp_plotframe <- cbind.data.frame(Year = c(as.POSIXct("2016-06-15"), as.POSIXct("2017-05-10"), as.POSIXct("2018-10-30")),
                                 pp = c(mean(pp16$fit, na.rm=T), mean(pp17$fit, na.rm=T), mean(pp18$fit, na.rm=T)),
                                 se = c(mean(pp16$se.fit, na.rm = T), mean(pp17$se.fit, na.rm = T), mean(pp18$se.fit, na.rm = T)))
pp_plotframe$cilo <- pp_plotframe$pp - 1.96*pp_plotframe$se  
pp_plotframe$cihi <- pp_plotframe$pp + 1.96*pp_plotframe$se  

doklam_pp_gallup <- ggplot(pp_plotframe) + geom_pointrange(aes(x = Year, y = pp, ymin = cilo, ymax = cihi),
                                                            color = ptol_pal()(2)[2]) + geom_line(aes(x = Year, y = pp), lty = 3, color = ptol_pal()(2)[2]) +
  theme_bw() + geom_vline(xintercept = as.POSIXct("2017-06-16"), color = "darkgrey", lty = 4) +
  geom_vline(xintercept = as.POSIXct("2017-08-28"), color = "darkgrey", lty = 4) +
  geom_text(aes(x = as.POSIXct("2017-07-23"), angle = 90, y = .3, label = "Doklam Crisis"), color = "darkgrey", size=5) + ylim(c(0,1)) +
  labs(title = "Predicted Probability of Approving of China's Leadership",
       subtitle = "Gallup World Poll",
       y = "p(Approval)",
       x = "Survey Date")



ggsave(filename = here::here("results/figs/fig3-4b.png"),plot = doklam_pp_gallup, width = 8, height = 5, units = "in", dpi = "retina")


# Evaluating Shocks (Pew) -------------------------------------------------------

# Pew shocks cover Doklam (2016, 7, 8) and DBO (2012-4)
# Dates
# 2014w = December 7, 2013 -- January 12, 2014
# 2014s = March 17, 2014 – June 5, 2014

# 2016 = April 7 - May 24, 2016
# 2017  = Feb. 21 - March 10, 2017
# 2018 = May 23 - July 23, 2018

## DOKLAM

pew_1618 <-  dfall %>% filter(YEAR %in% c(2016, 2017, 2018))
pew_1618$YEAR <- relevel(factor(pew_1618$YEAR), ref="2016")
pew_1618$region    <- relevel(factor(pew_1618$region), ref="North")
pew_1618$bjp    <- ifelse(pew_1618$partyid == "BJP", 1, 0)

change_mod_cn_1618 <- glm(approve_cn_bin ~ YEAR + male + age + edu + income + region, data=pew_1618, family =binomial(link = "logit")) 

out_chmod_cn_1618 <- mod_to_table_hc2(change_mod_cn_1618)

change_mod_cn_1618_lm <- lm_robust(approve_cn_bin ~ YEAR + male + age + edu + region + partyid + religion + income, data = pew_1618)
change_mod_cn_1618_lm

### Predicted Probability Plots

pp16 <- predict(change_mod_cn_1618_lm, newdata = pew_1618 |> filter(YEAR == 2016), type = "response", se.fit=T)
pp17 <- predict(change_mod_cn_1618_lm, newdata = pew_1618 |> filter(YEAR == 2017), type = "response", se.fit=T)
pp18 <- predict(change_mod_cn_1618_lm, newdata = pew_1618 |> filter(YEAR == 2018), type = "response", se.fit=T)


pp_plotframe <- cbind.data.frame(Year = c(as.POSIXct("2016-04-30"), as.POSIXct("2017-02-28"), as.POSIXct("2018-06-23")),
                                 pp = c(mean(pp16$fit, na.rm=T), mean(pp17$fit, na.rm=T), mean(pp18$fit, na.rm=T)),
                                 se = c(mean(pp16$se.fit, na.rm = T), mean(pp17$se.fit, na.rm = T), mean(pp18$se.fit, na.rm = T)))
pp_plotframe$cilo <- pp_plotframe$pp - 1.96*pp_plotframe$se  
pp_plotframe$cihi <- pp_plotframe$pp + 1.96*pp_plotframe$se  

doklam_pp_pew <- ggplot(pp_plotframe) + geom_pointrange(aes(x = Year, y = pp, ymin = cilo, ymax = cihi),
                                                           color = ptol_pal()(2)[2]) + geom_line(aes(x = Year, y = pp), lty = 3, color = ptol_pal()(2)[2]) +
  theme_bw() + geom_vline(xintercept = as.POSIXct("2017-06-16"), color = "darkgrey", lty = 4) +
  geom_vline(xintercept = as.POSIXct("2017-08-28"), color = "darkgrey", lty = 4) +
  geom_text(aes(x = as.POSIXct("2017-07-23"), angle = 90, y = .25, label = "Doklam Crisis"), color = "darkgrey", size=5) + ylim(c(0,1)) +
  labs(title = "Predicted Favorability Toward China",
       subtitle = "Pew Global Attitudes",
       y = "p(Approval)",
       x = "Survey Date")

ggsave(filename = here::here("results/figs/fig3-4a.png"),plot = doklam_pp_pew, width = 8, height = 5, units = "in", dpi = "retina")

### DBO

pew_1214 <-  dfall %>% filter(YEAR %in% c(2012, 2014))
pew_1214$YEAR <- relevel(factor(pew_1214$YEAR), ref="2012")
pew_1214$region    <- relevel(factor(pew_1214$region), ref="North")
pew_1214$bjp    <- ifelse(pew_1214$partyid == "BJP", 1, 0)

change_mod_cn_1214 <- glm(approve_cn_bin ~ YEAR + male + age + edu + income + region, data=pew_1214, family =binomial(link = "logit")) 

out_chmod_cn_1214 <- mod_to_table_hc2(change_mod_cn_1214)

change_mod_cn_1214_lm <- lm_robust(approve_cn_bin ~ YEAR + male + age + edu + region + partyid + religion + income, data = pew_1214)
change_mod_cn_1214_lm


pp12 <- predict(change_mod_cn_1214_lm, newdata = pew_1214 |> filter(YEAR == 2012), type = "response", se.fit=T)
pp14 <- predict(change_mod_cn_1214_lm, newdata = pew_1214 |> filter(YEAR == 2014), type = "response", se.fit=T)


pp_plotframe <- cbind.data.frame(Year = c(as.POSIXct("2012-04-01"), as.POSIXct("2014-01-01")),
                                 pp = c(mean(pp12$fit, na.rm=T), mean(pp14$fit, na.rm=T)),
                                 se = c(mean(pp12$se.fit, na.rm = T), mean(pp14$se.fit, na.rm = T)))
pp_plotframe$cilo <- pp_plotframe$pp - 1.96*pp_plotframe$se  
pp_plotframe$cihi <- pp_plotframe$pp + 1.96*pp_plotframe$se  

depsang_pp_pew <- ggplot(pp_plotframe) + geom_pointrange(aes(x = Year, y = pp, ymin = cilo, ymax = cihi),
                                                         color = ptol_pal()(2)[2]) + geom_line(aes(x = Year, y = pp), lty = 3, color = ptol_pal()(2)[2]) +
  theme_bw() + geom_vline(xintercept = as.POSIXct("2013-04-13"), color = "darkgrey", lty = 4) +
  geom_vline(xintercept = as.POSIXct("2013-05-05"), color = "darkgrey", lty = 4) +
  geom_text(aes(x = as.POSIXct("2013-04-23"), angle = 90, y = .25, label = "Depsang Standoff"), color = "darkgrey", size=5) + ylim(c(0,1)) +
  labs(title = "Predicted Favorability Toward China",
       subtitle = "Pew Global Attitudes ",
       y = "p(Approval)",
       x = "Survey Date")

ggsave(filename = here::here("results/figs/fig3-3a.png"),plot = depsang_pp_pew, width =8, height = 5, units = "in", dpi = "retina")



# Evaluating Shocks (IIOPO) -----------------------------------------------

iipo_8688 <- df |> filter(wave %in% c("1986a", "1986b", "1987a", "1987b", "1988a", "1988b"))
iipo_8688$wave <- relevel(factor(iipo_8688$wave), ref = "1986a")
iipo_8688$city    <- relevel(factor(iipo_8688$city), ref="Delhi")

change_mod_cn_8688 <- glm(cn_bin ~ wave + sex + age + educ + income + city + religion, data=iipo_8688, family =binomial(link = "logit")) 

out_chmod_cn_8688 <- mod_to_table_hc2(change_mod_cn_8688)

change_mod_cn_8688_lm <- lm_robust(cn_bin ~ wave + sex + age + educ + income + city + religion, data = iipo_8688)
change_mod_cn_8688_lm


pp86a <- predict(change_mod_cn_8688_lm, newdata = iipo_8688 |> filter(wave == "1986a"), type = "response", se.fit=T)
pp86b <- predict(change_mod_cn_8688_lm, newdata = iipo_8688 |> filter(wave == "1986b"), type = "response", se.fit=T)
pp87a <- predict(change_mod_cn_8688_lm, newdata = iipo_8688 |> filter(wave == "1987a"), type = "response", se.fit=T)
pp87b <- predict(change_mod_cn_8688_lm, newdata = iipo_8688 |> filter(wave == "1987b"), type = "response", se.fit=T)
pp88a <- predict(change_mod_cn_8688_lm, newdata = iipo_8688 |> filter(wave == "1988a"), type = "response", se.fit=T)
pp88b <- predict(change_mod_cn_8688_lm, newdata = iipo_8688 |> filter(wave == "1988b"), type = "response", se.fit=T)


pp_plotframe <- cbind.data.frame(Year = c("1986-03-30", "1986-09-30", "1987-03-30", "1987-10-15", "1988-04-30", "1988-10-01"),
                                 pp = c(mean(pp86a$fit, na.rm=T), mean(pp86b$fit, na.rm=T), mean(pp87a$fit, na.rm=T), mean(pp87b$fit, na.rm=T), mean(pp88a$fit, na.rm=T), mean(pp88b$fit, na.rm=T)),
                                 se =  c(mean(pp86a$se.fit, na.rm=T), mean(pp86b$se.fit, na.rm=T), mean(pp87a$se.fit, na.rm=T), mean(pp87b$se.fit, na.rm=T), mean(pp88a$se.fit, na.rm=T), mean(pp88b$se.fit, na.rm=T)))
pp_plotframe$Year <- as.POSIXct(pp_plotframe$Year)
pp_plotframe$cilo <- pp_plotframe$pp - 1.96*pp_plotframe$se  
pp_plotframe$cihi <- pp_plotframe$pp + 1.96*pp_plotframe$se  

sumdorong_pp_iipo <- ggplot(pp_plotframe) + geom_pointrange(aes(x = Year, y = pp, ymin = cilo, ymax = cihi),
                                                         color = ptol_pal()(2)[2]) + geom_line(aes(x = Year, y = pp), lty = 3, color = ptol_pal()(2)[2]) +
  theme_bw() + geom_vline(xintercept = as.POSIXct("1986-10-18"), color = "darkgrey", lty = 4) +
  geom_vline(xintercept = as.POSIXct("1987-05-01"), color = "darkgrey", lty = 4) +
  geom_text(aes(x = as.POSIXct("1987-01-23"), angle = 0, y = .1, label = "Sumdorong Chu\nStandoff"), color = "darkgrey", size=5) + ylim(c(0,1)) +
  labs(title = "Predicted Favorability Toward China",
       subtitle = "IIPO International Images - 1986-1988 ",
       y = "p(Approval)",
       x = "Survey Date")

ggsave(filename = here::here("results/figs/fig3-2.png"),plot = sumdorong_pp_iipo, width = 8, height = 5, units = "in", dpi = "retina")


# Structure of Opinions Stuff ---------------------------------------------

# Pew
dfall$region <- relevel(factor(dfall$region), ref = "North")
pew_pooled <- lm_robust(approve_cn_bin ~ region + religion + edu + income + male + age, data= dfall, fixed_effects = ~YEAR)
pew_pooled

dfall$bjp <- ifelse(dfall$partyid == "BJP", 1, 0)
dfall <- dfall |> mutate(threeparty = case_when(partyid == "BJP" ~ "BJP",
                                                partyid == "INC" ~ "INC",
                                                partyid %in% c("NoParty", "Other") ~ "Other"))

pew_party <- lm_robust(approve_cn_bin ~ region + religion + edu + income + male + age + threeparty, data= dfall, fixed_effects = ~YEAR)
pew_party


pew_party_plot <- modelsummary::modelplot(pew_party, coef_map = c("threepartyINC" = "INC Supporter",
                                                             "threepartyOther" = "Other Party Supporter",
                                                             "regionSouth" = "South Region (for reference)")) +
  geom_vline(xintercept = 0, lty = 2, color = "darkgrey") + scale_color_manual("#CC6677") +
  labs(title = "Marginal Associations with Partisanship")

ggsave(filename = here::here("results/figs/fig3-7.png"), plot = pew_party_plot, width = 8, height = 5, units = "in", dpi = "retina")


# Gallup
indf <- indf |> mutate(region = case_when(state == "Chhattisgarh" ~ "East",
                                          state == "Madhya Pradesh" ~ "North",
                                          is.na(state) & region == "Central" ~ "North",
                                          .default = as.character(region)))
indf$region <- relevel(factor(indf$region), ref = "North")

gallup_pooled <- lm_robust(cn_leader_bin ~ female + age + educ  + percap_income_localcurrency + region, data= indf, fixed_effects = ~YEAR_WAVE)
gallup_pooled
# IIPO
df$city <- relevel(factor(df$city), ref = "Delhi")
iipo_pooled <- lm_robust(cn_bin ~ city + sex + age + educ  + income + religion, data = df, fixed_effects = ~ wave)
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
  labs(title = "Regional Variation in Approval of China",
       subtitle = "Evidence from Modern and Historical Surveys",
       caption = "Coefficients are compared to approval in NORTH India\nModels include coefs. for income, age, education, gender, and sometimes religion, partyid.\nAll models include year FE, HC2 errors.")

ggsave(filename = here::here("results/figs/fig3-6.png"),plot = regionplot, width = 8, height = 5, units = "in", dpi = "retina")



# Party Stuff -------------------------------------------------------------

# Thought not to be very partisan in India, largely true
# Pew, look at party stuff across 2009, 2014

# Replicate the thigns in Pew Analysis

# Similar results, majorly by year

dfall$ndagov <- ifelse(dfall$YEAR >= 2014, 1, 0)


party_in_power <- lm_robust(approve_cn_bin ~ bjp*ndagov + region + religion + male + age + edu + income, data = dfall)
threeparty_in_power <- lm_robust(approve_cn_bin ~ ndagov*threeparty + region + religion + male + age + edu + income, data = dfall)

# When the BJP is in power nationally, BJP supporters express more favorable views toward China compared to the Singh era

pip_plot <- modelsummary::modelplot(party_in_power,
                        coef_map = c("bjp:ndagov" = "BJP Supporter x post-2014",
                                     "bjp" = "BJP Supporter x pre-2014")) +
  geom_vline(xintercept = 0, lty = 2, color = "darkgrey") + scale_color_manual(values = ptol_pal()(2)[2]) +
  labs(title = "Marginal Associations with Partisanship",
       subtitle = "Before and After 2014 election")

ggsave(filename = here::here("results/figs/fig3-8.png"), plot = pip_plot, width = 8, height = 5, units = "in", dpi = "retina")

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

pew_partymod <- lm_robust(approve_cn_bin ~ YEAR*bjp*region + religion + male + age + edu + income, data = dfall)

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
  geom_text(aes(x = as.POSIXct("2014-01-23"), angle = 90, y = .75,
                label = "2014 Elections"), color = "darkgrey", size=3) +
  facet_wrap(~region, ncol = 2) + theme_bw() + scale_color_manual(values = c("darkorange2", "chartreuse4")) +
  labs(title = "Predicted Favorability Toward China",
       subtitle = "Disaggregated by Region, Party",
       x = "Year",
       y = "p(Favorable)",
       color = "Party") + ylim(c(0,1))

ggsave(filename = here::here("results/figs/fig3-9.png"),plot = big_plot_byparty, width = 8, height = 5, units = "in", dpi = "retina")



# IIPO Regional Differences (fig3-5) -----------------------------------------------
df$Year <- as.numeric(gsub("[^0-9.-]", "", df$wave))

dfinfx <- df |> drop_na() |> mutate(Year = as.numeric(as.character(Year)),
                                    city_num = case_when(city == "Delhi" ~ 0, 
                                                         city == "Mumbai" ~ 1,
                                                         city == "Kolkata" ~ 2,
                                                         city == "Chennai" ~ 3)) |>  select(cn_bin, city_num, Year)

dfinfx <- as.matrix(dfinfx[,2:4])

interflex::interflex("binning", data = dfinfx, Y = "cn_bin", D = "city_num", X = "Year", na.rm = T, treat.type = "discrete",
                     main = "Over time variation in region effects",
                     Ylabel = "Approval of China",
                     Dlabel = "Region",
                     theme.bw = T, pool = T, color = c("#4477AA", "#117733", "#DDCC77", "#CC6677"), height = 5, width = 8, file = here::here("results/figs/fig3-5.png"))

