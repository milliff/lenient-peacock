# --------------------------------------------
#
# Author: Aidan Milliff and Paul Staniland
# Copyright (c) Aidan Milliff, 2026
# Email:  milliff.a@gmail.com
#
# Date: 2024-03-28
#
# Script Name: 00_clean-pew.R
#
# Script Description: Builds pew_clean.RData from Pew Global Attitudes SPSS
#   files. Cleans and harmonizes India-only responses across 13 survey waves
#   (2002–2018) into a single pooled data frame (dfall).
#
# Raw data required:
#   Pew Global Attitudes Survey .sav files for India-inclusive waves:
#   2002, 2004, 2005, 2007, 2008, 2009, 2010, 2011, 2012, 2014, 2015, 2016,
#   2017, 2018. Data are available from the Pew Research Center data archive:
#   https://www.pewresearch.org/global/datasets/
#
# Usage:
#   1. Set raw_data_path below to the directory containing your .sav files.
#   2. Run this script from the project root (lenient-peacock/) or source
#      via main.R.
#
# Output: data/pew_clean.RData  (object: dfall)
#
# --------------------------------------------


# SET PATH ------------------------------------------------------------------
# Change this to the directory containing your Pew Global Attitudes .sav files.

raw_data_path <- "[PATH TO YOUR PEW SAV FILES DIRECTORY]"   # <-- change this


# INSTALL PACKAGES & LOAD LIBRARIES -----------------------------------------
cat("INSTALLING PACKAGES & LOADING LIBRARIES... \n\n", sep = "")
packages <- c("tidyverse", "stringr", "readxl", "ggthemes", "estimatr", "purrr", "here") # list of packages to load
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
# 


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


# Pull in List object with all Pew ----------------------------------------

#### Read in all years that have India, Pakistan, Bangladesh ####
file_dir  <- raw_data_path
filenames <- Sys.glob(file.path(file_dir, "*.sav"))

dfs       <- lapply(filenames, function(x){data.frame(foreign::read.spss(x))})

# next add a "year" variable to each survey, then select only the India, Pak, Bangladesh items

years     <- c(2007, 2008, 2002, 2002, 2009,
               2004, 2005, 2010, 2011, 2016,
               2017, 2018,
               2012, 2013, 2014, 2014, 2015)
for (i in 1:length(dfs)){
  dfs[[i]]$YEAR <- rep(years[i], times = length(dfs[[i]][1]))
}


dfindia<- lapply(dfs[c(1,2,3,4,6,7,10)], function(x){dplyr::filter(x, country == "India")})
# Redo to catch years where it's capitalized :(
dfindia2 <- lapply(dfs[c(5,8,9,12,13,14,16,17)], function(x){dplyr::filter(x, COUNTRY == "India")})
dfindia3 <- dfs[[11]] |> filter(Country == "India")
# Make a list and save
dfindia <- list(dfindia[[1]], # 2007
                 dfindia[[2]],# 2008
                 dfindia[[4]], # 2002
                 dfindia[[6]], # 2005
                 dfindia[[7]], # 2016
                 dfs[[15]], # 2014
                 dfindia2[[1]], # 2009
                 dfindia2[[2]], # 2010
                 dfindia2[[3]], # 2011
                 dfindia2[[4]], # 2018
                 dfindia2[[5]], # 2012
                 dfindia2[[7]], # 2014
                 dfindia2[[8]], # 2015
                 dfindia3) # 2017

df_2007  <- dfindia[[1]]
df_2008  <- dfindia[[2]]
df_2002  <- dfindia[[3]]
df_2005  <- dfindia[[4]]
df_2016  <- dfindia[[5]]
df_2014w <- dfindia[[6]]
df_2009  <- dfindia[[7]]
df_2010  <- dfindia[[8]]
df_2011  <- dfindia[[9]]
df_2018  <- dfindia[[10]]
df_2012  <- dfindia[[11]]
df_2014s <- dfindia[[12]]
df_2015  <- dfindia[[13]]
df_2017  <- dfindia[[14]]



# Clean 2002 --------------------------------------------------------------
# 61B = United States (1= most favorable, 4 least)
# 84 = edu
# 86 = emply
# 88 income, 94ind = region 93 = ideology 79 = religion
# 73 gender, 74 age

df_2002 <- df_2002 |> mutate(approve_us = case_match(q61b, "Very favorable" ~ "Very favorable",
                                                     "Somewhat favorable" ~ "Somewhat favorable",
                                                     "Somewhat unfavorable" ~ "Somewhat unfavorable",
                                                     "Very unfavorable" ~ "Very unfavorable"),
                             approve_us_bin = ifelse(approve_us %in% c("Very favorable", "Somewhat favorable"), 1, 0),
                             edu = case_match(q84ida, "Illiterate" ~ 1,
                                                      "Literate but no formal schooling" ~ 2,
                                                      "School up to 4 years" ~ 3,
                                                      "School up to 5 to 9 years" ~ 4,
                                                      "SSC/HSC" ~ 5,
                                                      "Some college but not graduated" ~ 6,
                                                      "Graduate/Post grad-Gen BA MSc BCom etc" ~ 7,
                                                      "Graduate/Post grad-Prof BE MTech MBA MBBS etc" ~ 8),
                             empl = case_match(q86ida, c("Unskilled workers",
                                                         "Skilled workers",
                                                         "Petty traders",
                                                         "Shop owners",
                                                         "Businessmen/Industrialists \x96 with no employees",
                                                         "Businessmen/Industrialists \x96 with 1-9 employees",
                                                         "Businessmen/Industrialists \x96 with 10+ employees",
                                                         "Self employed professionals",
                                                         "Clerks/Salesmen",
                                                         "Supervisory Level",
                                                         "Officers/executives - junior",
                                                         "Officers/executives \x96 middle/senior") ~ "Employed",
                                                         c("Housewife",
                                                           "Student",
                                                           "Unemployed") ~ "Unemployed"),
                             income = case_match(q88ida,
                                                 "Up to Rs. 500" ~ 1,
                                                 "Rs. 501 \x96 Rs. 750" ~ 2,    
                                                 "Rs. 751 \x96 Rs. 1000" ~ 3,
                                                 "Rs. 1001 \x96 Rs. 1500" ~ 4,
                                                 "Rs. 1501 \x96 Rs. 2000" ~ 5,
                                                 "Rs. 2001 \x96 Rs. 2500" ~ 6, 
                                                 "Rs. 2501 \x96 Rs. 3000" ~ 7, 
                                                 "Rs. 3001 \x96 Rs. 4000" ~ 8,
                                                 "Rs. 4001 \x96 Rs. 5000" ~ 9,
                                                 "Rs. 5001 \x96 Rs. 6000" ~ 10,
                                                 "Rs. 6001 \x96 Rs. 10000" ~ 11,
                                                 "Rs. 10001 \x96 Rs. 15000" ~ 12,
                                                 "Rs. 15001 \x96 Rs. 20000" ~ 13,
                                                 "Rs. 20001 \x96 Rs. 30000" ~ 14,
                                                 "Rs. 30001 \x96 Rs. 40000" ~ 15,
                                                 "Rs. 40001 or more" ~ 16),
                             region = q94ida,
                             religion = case_match(q79,
                                                   "Buddhism" ~ "Buddhist",
                                                   "Christian" ~ "Christian",
                                                   "Hindu" ~ "Hindu",
                                                   "Jain" ~ "Jain",
                                                   "Muslim" ~ "Muslim",
                                                   "Sikh" ~ "Sikh",
                                                   "no religion" ~ "None"),
                             male = ifelse(q73 == "Male", 1, 0),
                             age = as.numeric(as.character(q74))
                             )

# Clean 2005 --------------------------------------------------------------
# No q about china or US


# Clean 2007 --------------------------------------------------------------
# Q16.C = China 1=most favorabe, 4 = l3ast 
# Q16.A = US
# Q16.F = Russia
# 130=Region, 125=Ethnicity, 123=Income, 120ind = employment, 118, edu, 108 age, 107 gender

df_2007 <- df_2007 |> mutate(approve_us = case_match(Q16A, "Very favorable" ~ "Very favorable",
                                                     "Somewhat favorable" ~ "Somewhat favorable",
                                                     "Somewhat unfavorable" ~ "Somewhat unfavorable",
                                                     "Very unfavorable " ~ "Very unfavorable"),
                             approve_cn = case_match(Q16C, "Very favorable" ~ "Very favorable",
                                                     "Somewhat favorable" ~ "Somewhat favorable",
                                                     "Somewhat unfavorable" ~ "Somewhat unfavorable",
                                                     "Very unfavorable " ~ "Very unfavorable"),
                             approve_ru = case_match(Q16F, "Very favorable" ~ "Very favorable",
                                                     "Somewhat favorable" ~ "Somewhat favorable",
                                                     "Somewhat unfavorable" ~ "Somewhat unfavorable",
                                                     "Very unfavorable " ~ "Very unfavorable"),
                             approve_us_bin = ifelse(approve_us %in% c("Very favorable", "Somewhat favorable"), 1, 0),
                             approve_cn_bin = ifelse(approve_cn %in% c("Very favorable", "Somewhat favorable"), 1, 0),
                             approve_ru_bin = ifelse(approve_ru %in% c("Very favorable", "Somewhat favorable"), 1, 0),
                             edu = case_match(Q118INDA, "Illiterate" ~ 1,
                                              "Literate but no formal schooling" ~ 2,
                                              "School up to 4 years" ~ 3,
                                              "School up to 5 to 9 years" ~ 4,
                                              "SSC/HSC" ~ 5,
                                              "Some college but not graduated" ~ 6,
                                              "Graduate/Post grad-Gen BA MSc BCom etc" ~ 7,
                                              "Graduate/Post grad-Prof BE MTech MBA MBBS etc" ~ 8),
                             empl = case_match(Q120INDA, c("Full-time employed",
                                                         "Part-time employed",
                                                         "Pensioner and employed",
                                                         "Self-employed") ~ "Employed",
                                                      c("Pensioner,not employed",
                                                        "Not employed (e.g housewife, houseman, student)")~ "Unemployed"),
                             income = case_match(Q123INDA,
                                                 "Up to Rs. 500 per month" ~ 1,
                                                 "Rs. 501 – Rs. 750" ~ 2,    
                                                 "Rs. 751 – Rs. 1000" ~ 3,
                                                 "Rs. 1001 – Rs. 1500" ~ 4,
                                                 "Rs. 1501 – Rs. 2000" ~ 5,
                                                 "Rs. 2001 – Rs. 2500" ~ 6, 
                                                 "Rs. 2501 – Rs. 3000" ~ 7, 
                                                 "Rs. 3001 – Rs. 4000" ~ 8,
                                                 "Rs. 4001 – Rs. 5000" ~ 9,
                                                 "Rs. 5001 – Rs. 6000" ~ 10,
                                                 "Rs. 6001 – Rs. 10000" ~ 11,
                                                 "Rs. 10001 – Rs. 15000" ~ 12,
                                                 "Rs. 15001 – Rs. 20000" ~ 13,
                                                 "Rs. 20001 – Rs. 30000" ~ 14,
                                                 "Rs. 30001 – Rs. 40000" ~ 15,
                                                 "Rs. 40001 & above" ~ 16),
                             region = case_match(Q130INDA,
                                                 c("Delhi—North",
                                                   "Lucknow--North") ~ "North",
                                                 c("Chennai—South",
                                                   "Hyderabad—South") ~ "South",
                                                 c("Kolkata----East",
                                                   "Patna--East") ~ "East",
                                                 c("Mumbai—West",
                                                   "Ahmedabad—West") ~ "West"),
                             religion = case_match(Q44INDA,
                                                   "Buddhist" ~ "Buddhist",
                                                   "Christian" ~ "Christian",
                                                   "Hindu" ~ "Hindu",
                                                   "Jain" ~ "Jain",
                                                   "Muslim" ~ "Muslim",
                                                   "Sikh" ~ "Sikh",
                                                   c("No religion/not a believer/atheist/agnostic (VOLUNTEERED)",
                                                     "Other religion (VOLUNTEERED)") ~ "None"),
                             male = ifelse(Q107 == "Male", 1, 0),
                             age = as.numeric(as.character(Q108))
)


# Clean 2008 --------------------------------------------------------------

# 10a: US
# 10c China
# 38 rel, 
# 75 gender, 76 age, 84 edu, 85 empl, 89 income, 90 ethnicity, 98 region


df_2008 <- df_2008 |> mutate(approve_us = case_match(Q10a, "Very favorable" ~ "Very favorable",
                                                     "Somewhat favorable" ~ "Somewhat favorable",
                                                     "Somewhat unfavorable " ~ "Somewhat unfavorable ",
                                                     "Very unfavorable " ~ "Very unfavorable"),
                             approve_cn = case_match(Q10c, "Very favorable" ~ "Very favorable",
                                                     "Somewhat favorable" ~ "Somewhat favorable",
                                                     "Somewhat unfavorable " ~ "Somewhat unfavorable",
                                                     "Very unfavorable " ~ "Very unfavorable"),
                             approve_us_bin = ifelse(approve_us %in% c("Very favorable", "Somewhat favorable"), 1, 0),
                             approve_cn_bin = ifelse(approve_cn %in% c("Very favorable", "Somewhat favorable"), 1, 0),
                             edu = case_match(Q84INDIA, "Illiterate " ~ 1,
                                              "Literate but no formal schooling  " ~ 2,
                                              "School up to 4 years" ~ 3,
                                              "School up to 5 to 9 years" ~ 4,
                                              "SSC/HSC" ~ 5,
                                              "Some college but not graduated" ~ 6,
                                              "Graduate/Post grad-Gen BA MSc BCom etc" ~ 7,
                                              "Graduate/Post grad-Prof BE MTech MBA MBBS etc" ~ 8),
                             empl = case_match(Q85, c("Full-time employed",
                                                           "Part-time employed",
                                                           "Pensioner and employed",
                                                           "Self-employed",
                                                           "Farmer") ~ "Employed",
                                               c("Unemployed, no state benefit",
                                                 "Unemployed, receiving state benefit",
                                                 "No job, other government assistance for such things as maternity or disability ",
                                                 "Pensioner, not employed",
                                                 "Not employed (e.g. housewife, houseman, student)") ~ "Unemployed"),
                             income = case_match(Q89INDIA,
                                                 "Up to Rs. 500 per month" ~ 1,
                                                 "Rs. 501 – Rs. 750" ~ 2,    
                                                 "Rs. 751 – Rs. 1000" ~ 3,
                                                 "Rs. 1001 – Rs. 1500" ~ 4,
                                                 "Rs. 1501 – Rs. 2000" ~ 5,
                                                 "Rs. 2001 – Rs. 2500" ~ 6, 
                                                 "Rs. 2501 – Rs. 3000" ~ 7, 
                                                 "Rs. 3001 – Rs. 4000" ~ 8,
                                                 "Rs. 4001 – Rs. 5000" ~ 9,
                                                 "Rs. 5001 – Rs. 6000" ~ 10,
                                                 "Rs. 6001 – Rs. 10000" ~ 11,
                                                 "Rs. 10001 – Rs. 15000" ~ 12,
                                                 "Rs. 15001 – Rs. 20000" ~ 13,
                                                 "Rs. 20001 – Rs. 30000" ~ 14,
                                                 "Rs. 30001 – Rs. 40000" ~ 15,
                                                 "Rs. 40001 & above" ~ 16),
                             region = case_match(Q98INDIA,
                                                 c("Delhi-North",
                                                   "Lucknow-North") ~ "North",
                                                 c("Chennai-South",
                                                   "Hyderabad-South") ~ "South",
                                                 c("Kolkata-East",
                                                   "Patna-East") ~ "East",
                                                 c("Mumbai-West",
                                                   "Ahmedabad-West") ~ "West"),
                             religion = case_match(Q38INDIA,
                                                   "Buddhist" ~ "Buddhist",
                                                   "Christian" ~ "Christian",
                                                   "Hindu" ~ "Hindu",
                                                   "Jain" ~ "Jain",
                                                   "Muslim" ~ "Muslim",
                                                   "Sikh" ~ "Sikh",
                                                   c("No religion / not a believer / atheist / agnostic",
                                                     "Other religion") ~ "None"),
                             male = ifelse(Q75 == "Male", 1, 0),
                             age = as.numeric(as.character(Q76))
)

# Clean 2009 --------------------------------------------------------------

# 11a US, 11c China, 11e Russia

# 80 gender, 81 age, 95 edu, 96, empl, 97 income, 98 ethnicity, 103 party ID, 107 region

df_2009 <- df_2009 |> mutate(approve_us = case_match(Q11A, "Very favorable" ~ "Very favorable",
                                                     "Somewhat favorable" ~ "Somewhat favorable",
                                                     "Somewhat unfavorable" ~ "Somewhat unfavorable ",
                                                     "Very unfavorable" ~ "Very unfavorable"),
                             approve_cn = case_match(Q11C, "Very favorable" ~ "Very favorable",
                                                     "Somewhat favorable" ~ "Somewhat favorable",
                                                     "Somewhat unfavorable" ~ "Somewhat unfavorable",
                                                     "Very unfavorable" ~ "Very unfavorable"),
                             approve_ru = case_match(Q11E, "Very favorable" ~ "Very favorable",
                                                     "Somewhat favorable" ~ "Somewhat favorable",
                                                     "Somewhat unfavorable" ~ "Somewhat unfavorable",
                                                     "Very unfavorable" ~ "Very unfavorable"),
                             approve_us_bin = ifelse(approve_us %in% c("Very favorable", "Somewhat favorable"), 1, 0),
                             approve_cn_bin = ifelse(approve_cn %in% c("Very favorable", "Somewhat favorable"), 1, 0),
                             approve_ru_bin = ifelse(approve_ru %in% c("Very favorable", "Somewhat favorable"), 1, 0),
                             edu = case_match(Q95INDIA, "Illiterate " ~ 1,
                                              "Literate but no formal schooling  " ~ 2,
                                              "School up to 4 years" ~ 3,
                                              "School up to 5 to 9 years" ~ 4,
                                              "SSC/HSC" ~ 5,
                                              "Some college but not graduated" ~ 6,
                                              "Graduate/Post grad-Gen BA MSc BCom etc" ~ 7,
                                              "Graduate/Post grad-Prof BE MTech MBA MBBS etc" ~ 8),
                             empl = case_match(Q96, c("Full-time employed",
                                                      "Part-time employed",
                                                      "Pensioner and employed",
                                                      "Self-employed") ~ "Employed",
                                               c("Unemployed, no state benefit",
                                                 "Unemployed, receiving state benefit",
                                                 "No job, other government assistance for such things as maternity or disability",
                                                 "Pensioner, not employed",
                                                 "Not employed (e.g. housewife, houseman, student)") ~ "Unemployed"),
                             income = case_match(Q97INDIA,
                                                 "Up to Rs. 500 per month" ~ 1,
                                                 "Rs. 501 – Rs. 750" ~ 2,    
                                                 "Rs. 751 – Rs. 1000" ~ 3,
                                                 "Rs. 1001 – Rs. 1500" ~ 4,
                                                 "Rs. 1501 – Rs. 2000" ~ 5,
                                                 "Rs. 2001 – Rs. 2500" ~ 6, 
                                                 "Rs. 2501 – Rs. 3000" ~ 7, 
                                                 "Rs. 3001 – Rs. 4000" ~ 8,
                                                 "Rs. 4001 – Rs. 5000" ~ 9,
                                                 "Rs. 5001 – Rs. 6000" ~ 10,
                                                 "Rs. 6001 – Rs. 10000" ~ 11,
                                                 "Rs. 10001 – Rs. 15000" ~ 12,
                                                 "Rs. 15001 – Rs. 20000" ~ 13,
                                                 "Rs. 20001 – Rs. 30000" ~ 14,
                                                 "Rs. 30001 – Rs. 40000" ~ 15,
                                                 "Rs. 40001 & above" ~ 16),
                             region = case_match(Q107INDIA,
                                                 "North" ~ "North",
                                                 "South" ~ "South",
                                                 "East" ~ "East",
                                                 "West" ~ "West"),
                             religion = case_match(Q41INDIA,
                                                   "Buddhist" ~ "Buddhist",
                                                   "Christian" ~ "Christian",
                                                   "Hindu" ~ "Hindu",
                                                   "Jain" ~ "Jain",
                                                   "Muslim" ~ "Muslim",
                                                   "Sikh" ~ "Sikh",
                                                   c("No religion / not a believer / atheist / agnostic  (Volunteered)",
                                                     "Other religion (Volunteered)") ~ "None"),
                             partyid = case_match(Q103INDIA,
                                                  "BJP: Bharatiya Janata Party" ~ "BJP",
                                                  "Congress Party" ~ "INC",
                                                  c("CPI (M): Communist Party of India (Marxist) ",
                                                    "Trinamul Congress",                                
                                                    "BSP: Bahujan Samaj Party  ",                       
                                                    "SP: Samajwadi Party  ",                            
                                                    "Shiv Sena",                                        
                                                    "Nationalist Congress Party",                       
                                                    "Maharashtra Nav Nirman Sena(MNS)",                 
                                                    "Rashtriya Janata Dal(RJD)",                        
                                                    "Janata Dal United(JDU)",                           
                                                    "Lok JanShakti Party (LJP)",                        
                                                    "Telegu Desham Party (TDP)",                        
                                                    "Telangana Rashtra  Samithi ",                      
                                                    "Dravida Munnetra Kazhagam ( DMK)",                 
                                                    "All India Anna Dravida Munnetra Kazhagam (AIADMK)",
                                                    "Marumalarchi Dravida  Munnetra Kazhagam(MDMK)",    
                                                    "Other (Volunteered)") ~ "Other",
                                                  "None/No party  (Volunteered)" ~ "NoParty"),
                             male = ifelse(Q80 == "Male", 1, 0),
                             age = as.numeric(as.character(Q81))
)

# Clean 2010 --------------------------------------------------------------

# 7a us, 7c china, 7e russia
# 101d china threat
# 120 gender, 121 age, 130 empl, 129 edu, 131 income, 131b caste, 133 ethn, 138 party id, 142 region


df_2010 <- df_2010 |> mutate(approve_us = case_match(Q7A, "Very favorable" ~ "Very favorable",
                                                     "Somewhat favorable" ~ "Somewhat favorable",
                                                     "Somewhat unfavorable" ~ "Somewhat unfavorable ",
                                                     "Very unfavorable" ~ "Very unfavorable"),
                             approve_cn = case_match(Q7C, "Very favorable" ~ "Very favorable",
                                                     "Somewhat favorable" ~ "Somewhat favorable",
                                                     "Somewhat unfavorable" ~ "Somewhat unfavorable",
                                                     "Very unfavorable" ~ "Very unfavorable"),
                             approve_ru = case_match(Q7E, "Very favorable" ~ "Very favorable",
                                                     "Somewhat favorable" ~ "Somewhat favorable",
                                                     "Somewhat unfavorable" ~ "Somewhat unfavorable",
                                                     "Very unfavorable" ~ "Very unfavorable"),
                             approve_us_bin = ifelse(approve_us %in% c("Very favorable", "Somewhat favorable"), 1, 0),
                             approve_cn_bin = ifelse(approve_cn %in% c("Very favorable", "Somewhat favorable"), 1, 0),
                             approve_ru_bin = ifelse(approve_ru %in% c("Very favorable", "Somewhat favorable"), 1, 0),
                             edu = case_match(Q129INDIA, "Illiterate" ~ 1,
                                              "Literate but no formal schooling" ~ 2,
                                              "School up to 4 years" ~ 3,
                                              "School up to 5 to 9 years" ~ 4,
                                              "SSC/HSC" ~ 5,
                                              "Some college but not graduated" ~ 6,
                                              "Graduate/Post grad-Gen BA MSc BCom etc" ~ 7,
                                              "Graduate/Post grad-Prof BE MTech MBA MBBS etc" ~ 8),
                             empl = case_match(Q130, c("Full-time employed",
                                                      "Part-time employed",
                                                      "Pensioner and employed",
                                                      "Self-employed") ~ "Employed",
                                               c("Unemployed, no state benefit",
                                                 "Unemployed, receiving state benefit",
                                                 "No job, other government assistance for such things as maternity or disability ",
                                                 "Pensioner, not employed",
                                                 "Not employed (e.g. housewife, houseman, student)") ~ "Unemployed"),
                             income = case_match(Q131INDIA,
                                                 "Up to Rs. 500 per month" ~ 1,
                                                 "Rs. 501 - Rs. 750" ~ 2,    
                                                 "Rs. 751 - Rs. 1000" ~ 3,
                                                 "Rs. 1001 - Rs. 1500" ~ 4,
                                                 "Rs. 1501 - Rs. 2000" ~ 5,
                                                 "Rs. 2001 - Rs. 2500" ~ 6, 
                                                 "Rs. 2501 - Rs. 3000" ~ 7, 
                                                 "Rs. 3001 - Rs. 4000" ~ 8,
                                                 "Rs. 4001 - Rs. 5000" ~ 9,
                                                 "Rs. 5001 - Rs. 6000" ~ 10,
                                                 "Rs. 6001 - Rs. 10000" ~ 11,
                                                 "Rs. 10001 - Rs. 15000" ~ 12,
                                                 "Rs. 15001 - Rs. 20000" ~ 13,
                                                 "Rs. 20001 - Rs. 30000" ~ 14,
                                                 "Rs. 30001 - Rs. 40000" ~ 15,
                                                 "Rs. 40001 & above" ~ 16),
                             region = case_match(Q142INDIA,
                                                 "North" ~ "North",
                                                 "South" ~ "South",
                                                 "East" ~ "East",
                                                 "West" ~ "West"),
                             religion = case_match(Q50INDIA,
                                                   "Buddhist" ~ "Buddhist",
                                                   "Christian" ~ "Christian",
                                                   "Hindu" ~ "Hindu",
                                                   "Jain" ~ "Jain",
                                                   "Muslim" ~ "Muslim",
                                                   "Sikh" ~ "Sikh",
                                                   c("No religion /not a believer/atheist/agnostic (DO NOT READ)",
                                                     "Other religion ") ~ "None"),
                             partyid = case_match(Q138INDIA,
                                                  "BJP: Bharatiya Janata Party" ~ "BJP",
                                                  "Congress Party" ~ "INC",
                                                  c("CPI (M): Communist Party of India (Marxist) ",
                                                    "Trinamul Congress",                                
                                                    "BSP: Bahujan Samaj Party  ",                       
                                                    "SP: Samajwadi Party  ",                            
                                                    "Shiv Sena",                                        
                                                    "Nationalist Congress Party",                       
                                                    "Maharashtra Nav Nirman Sena(MNS)",                 
                                                    "Rashtriya Janata Dal(RJD)",                        
                                                    "Janata Dal United(JDU)",                           
                                                    "Lok JanShakti Party (LJP)",                        
                                                    "Telegu Desham Party (TDP)",                        
                                                    "Telangana Rashtra  Samithi ",                      
                                                    "Dravida Munnetra Kazhagam ( DMK)",                 
                                                    "All India Anna Dravida Munnetra Kazhagam (AIADMK)",
                                                    "Marumalarchi Dravida  Munnetra Kazhagam(MDMK)",    
                                                    "Other",
                                                    "MURUASU", "DMDK", "LOKSATTA") ~ "Other",
                                                  "None/No party (DO NOT READ)" ~ "NoParty"),
                             male = ifelse(Q120 == "Male", 1, 0),
                             age = as.numeric(as.character(Q121))
)

# Clean 2011 --------------------------------------------------------------

# 3a us, 3c china, 3e russia
# 111 gender, 112 age, 120 edu, 121 empl, 123 income, 133 ethn, 129 partyid, 135 region

df_2011 <- df_2011 |> mutate(approve_us = case_match(Q3A, "Very favorable" ~ "Very favorable",
                                                     "Somewhat favorable" ~ "Somewhat favorable",
                                                     "Somewhat unfavorable" ~ "Somewhat unfavorable ",
                                                     "Very unfavorable" ~ "Very unfavorable"),
                             approve_cn = case_match(Q3C, "Very favorable" ~ "Very favorable",
                                                     "Somewhat favorable" ~ "Somewhat favorable",
                                                     "Somewhat unfavorable" ~ "Somewhat unfavorable",
                                                     "Very unfavorable" ~ "Very unfavorable"),
                             approve_ru = case_match(Q3E, "Very favorable" ~ "Very favorable",
                                                     "Somewhat favorable" ~ "Somewhat favorable",
                                                     "Somewhat unfavorable" ~ "Somewhat unfavorable",
                                                     "Very unfavorable" ~ "Very unfavorable"),
                             approve_us_bin = ifelse(approve_us %in% c("Very favorable", "Somewhat favorable"), 1, 0),
                             approve_cn_bin = ifelse(approve_cn %in% c("Very favorable", "Somewhat favorable"), 1, 0),
                             approve_ru_bin = ifelse(approve_ru %in% c("Very favorable", "Somewhat favorable"), 1, 0),
                             edu = case_match(Q120INDIA, "Illiterate" ~ 1,
                                              "Literate but no formal schooling" ~ 2,
                                              "School up to 4 years" ~ 3,
                                              "School up to 5 to 9 years" ~ 4,
                                              "SSC/HSC" ~ 5,
                                              "Some college but not graduated" ~ 6,
                                              "Graduate/Post grad-Gen BA MSc BCom etc" ~ 7,
                                              "Graduate/Post grad-Prof BE MTech MBA MBBS etc" ~ 8),
                             empl = case_match(Q121, c("Full-time employed",
                                                       "Part-time employed",
                                                       "Pensioner and employed",
                                                       "Self-employed") ~ "Employed",
                                               c("Unemployed, no state benefit",
                                                 "Unemployed, receiving state benefit",
                                                 "No job, other government assistance for such things as maternity or disability ",
                                                 "Pensioner, not employed",
                                                 "Not employed (e.g. housewife, houseman, student)") ~ "Unemployed"),
                             income = case_match(Q123INDIA,
                                                 "Up to Rs. 500 per month" ~ 1,
                                                 "Rs. 501 – Rs. 750" ~ 2,    
                                                 "Rs. 751 – Rs. 1000" ~ 3,
                                                 "Rs. 1001 – Rs. 1500" ~ 4,
                                                 "Rs. 1501 – Rs. 2000" ~ 5,
                                                 "Rs. 2001 – Rs. 2500" ~ 6, 
                                                 "Rs. 2501 – Rs. 3000" ~ 7, 
                                                 "Rs. 3001 – Rs. 4000" ~ 8,
                                                 "Rs. 4001 – Rs. 5000" ~ 9,
                                                 "Rs. 5001 – Rs. 6000" ~ 10,
                                                 "Rs. 6001 – Rs. 10,000" ~ 11,
                                                 "Rs. 10,001 – Rs. 15,000" ~ 12,
                                                 "Rs. 15,001 – Rs. 20,000" ~ 13,
                                                 "Rs. 20,001 – Rs. 30,000" ~ 14,
                                                 "Rs. 30,001 – Rs. 40,000" ~ 15,
                                                 "Rs. 40,001 – Rs. 50,000" ~ 16,
                                                 "Rs. 50,001 – Rs. 100,000" ~ 17,
                                                 "Rs. 100,001 and above " ~ 18),
                             region = case_match(Q135INDIA,
                                                 "North" ~ "North",
                                                 "South" ~ "South",
                                                 "East" ~ "East",
                                                 "West" ~ "West"),
                             religion = case_match(Q34INDIA,
                                                   "Buddhist" ~ "Buddhist",
                                                   "Christian" ~ "Christian",
                                                   "Hindu" ~ "Hindu",
                                                   "Jain" ~ "Jain",
                                                   "Muslim" ~ "Muslim",
                                                   "Sikh" ~ "Sikh",
                                                   c("No religion/not a believer/atheist/agnostic (DO NOT READ)",
                                                     "Other religion (DO NOT READ)") ~ "None"),
                             partyid = case_match(Q129INDIA,
                                                  "BJP: Bharatiya Janata Party" ~ "BJP",
                                                  "Congress Party" ~ "INC",
                                                  c("CPI (M): Communist Party of India (Marxist)",
                                                    "Trinamul Congress",                                
                                                    "BSP: Bahujan Samaj Party",                       
                                                    "SP: Samajwadi Party",                            
                                                    "Shiv Sena",                                        
                                                    "Nationalist Congress Party",                       
                                                    "Maharashtra Nav Nirman Sena (MNS)",                 
                                                    "Rashtriya Janata Dal (RJD)",                        
                                                    "Janata Dal United (JDU)",                           
                                                    "Lok JanShakti Party (LJP)",                        
                                                    "Telegu Desham Party (TDP)",                        
                                                    "Telangana Rashtra  Samithi",                      
                                                    "Dravida Munnetra Kazhagam (DMK)",                 
                                                    "All India Anna Dravida Munnetra Kazhagam (AIADMK)",
                                                    "Marumalarchi Dravida  Munnetra Kazhagam (MDMK)",    
                                                    "Other  (DO NOT READ)",
                                                    "Muruasu", "Dmdk", "Loksatta", "Akalai Dal", "YSR Party", "B.J.D. Sakha",
                                                    " MIM") ~ "Other",
                                                  "None/No party (DO NOT READ)" ~ "NoParty"),
                             male = ifelse(Q111 == "Male", 1, 0),
                             age = as.numeric(as.character(Q112))
)


# Clean 2012 --------------------------------------------------------------

# 8a us, 8c china, 8e russia 
# 107 india china relationship (1 coop, 2 hostility, 3 neither), 127 china threat
# 141 gender, 142 age, 154 edu, 155 empl, 156 income, 159 ethn, 164 partyid, 170 region


df_2012 <- df_2012 |> mutate(approve_us = case_match(Q8A, "Very favorable" ~ "Very favorable",
                                                     "Somewhat favorable" ~ "Somewhat favorable",
                                                     "Somewhat unfavorable" ~ "Somewhat unfavorable ",
                                                     "Very unfavorable" ~ "Very unfavorable"),
                             approve_cn = case_match(Q8C, "Very favorable" ~ "Very favorable",
                                                     "Somewhat favorable" ~ "Somewhat favorable",
                                                     "Somewhat unfavorable" ~ "Somewhat unfavorable",
                                                     "Very unfavorable" ~ "Very unfavorable"),
                             approve_ru = case_match(Q8E, "Very favorable" ~ "Very favorable",
                                                     "Somewhat favorable" ~ "Somewhat favorable",
                                                     "Somewhat unfavorable" ~ "Somewhat unfavorable",
                                                     "Very unfavorable" ~ "Very unfavorable"),
                             approve_us_bin = ifelse(approve_us %in% c("Very favorable", "Somewhat favorable"), 1, 0),
                             approve_cn_bin = ifelse(approve_cn %in% c("Very favorable", "Somewhat favorable"), 1, 0),
                             approve_ru_bin = ifelse(approve_ru %in% c("Very favorable", "Somewhat favorable"), 1, 0),
                             edu = case_match(Q154INDIA, "Illiterate" ~ 1,
                                              "Literate but no formal schooling" ~ 2,
                                              "School up to 4 years" ~ 3,
                                              "School up to 5 to 9 years" ~ 4,
                                              "SSC/HSC" ~ 5,
                                              "Some college but not graduated" ~ 6,
                                              "Graduate/Post grad-Gen BA MSc BCOM etc" ~ 7,
                                              "Graduate/Post grad-Prof BE MTech MBA MBBS etc" ~ 8),
                             empl = case_match(Q155, c("Full-time employed",
                                                       "Part-time employed",
                                                       "Pensioner and employed",
                                                       "Self-employed") ~ "Employed",
                                               c("Unemployed, no state benefit",
                                                 "Unemployed, receiving state benefit",
                                                 "No job, other government assistance for such things as maternity or disability",
                                                 "Pensioner, not employed",
                                                 "Not employed (e.g. housewife, houseman, student)") ~ "Unemployed"),
                             income = case_match(Q156INDIA,
                                                 "Up to Rs. 500 per month" ~ 1,
                                                 "Rs. 501 – Rs. 750" ~ 2,    
                                                 "Rs. 751 – Rs. 1000" ~ 3,
                                                 "Rs. 1001 – Rs. 1500" ~ 4,
                                                 "Rs. 1501 – Rs. 2000" ~ 5,
                                                 "Rs. 2001 – Rs. 2500" ~ 6, 
                                                 "Rs. 2501 – Rs. 3000" ~ 7, 
                                                 "Rs. 3001 – Rs. 4000" ~ 8,
                                                 "Rs. 4001 – Rs. 5000" ~ 9,
                                                 "Rs. 5001 – Rs. 6000" ~ 10,
                                                 "Rs. 6001 – Rs. 10,000" ~ 11,
                                                 "Rs. 10,001 – Rs. 15,000" ~ 12,
                                                 "Rs. 15,001 – Rs. 20,000" ~ 13,
                                                 "Rs. 20,001 – Rs. 30,000" ~ 14,
                                                 "Rs. 30,001 – Rs. 40,000" ~ 15,
                                                 "Rs. 40,001 – Rs. 50,000" ~ 16,
                                                 "Rs. 50,001 – Rs. 100,000" ~ 17,
                                                 "Rs. 100,001 and above" ~ 18),
                             region = case_match(Q170INDIA,
                                                 "North" ~ "North",
                                                 "South" ~ "South",
                                                 "East" ~ "East",
                                                 "West" ~ "West"),
                             religion = case_match(Q61INDIA,
                                                   "Buddhist" ~ "Buddhist",
                                                   "Christian" ~ "Christian",
                                                   "Hindu" ~ "Hindu",
                                                   "Jain" ~ "Jain",
                                                   "Muslim" ~ "Muslim",
                                                   "Sikh" ~ "Sikh",
                                                   c("No religion/not a believer/atheist/agnostic (Volunteered)",
                                                     "Other religion (Volunteered)") ~ "None"),
                             partyid = case_match(Q164INDIA,
                                                  "BJP: Bharatiya Janata Party" ~ "BJP",
                                                  "Congress Party" ~ "INC",
                                                  c("CPI (M): Communist Party of India (Marxist)",
                                                    "Trinamul Congress",                                
                                                    "BSP: Bahujan Samaj Party",                       
                                                    "SP: Samajwadi Party",                            
                                                    "Shiv Sena",                                        
                                                    "Nationalist Congress Party",                       
                                                    "Maharashtra Nav Nirman Sena (MNS)",                 
                                                    "Rashtriya Janata Dal (RJD)",                        
                                                    "Janata Dal United (JDU)",                           
                                                    "Lok JanShakti Party (LJP)",                        
                                                    "Telegu Desham Party (TDP)",                        
                                                    "Telangana Rashtra  Samithi",                      
                                                    "Dravida Munnetra Kazhagam (DMK)",                 
                                                    "All India Anna Dravida Munnetra Kazhagam (AIADMK)",
                                                    "Marumalarchi Dravida  Munnetra Kazhagam (MDMK)",    
                                                    "Other (Volunteered)",
                                                    "MURUASU", "DMDK", "LOKSATTA", "Akali Dal",
                                                    "Hand pump  (Volunteered)",                         
                                                    "BJD  (Volunteered)",   
                                                    "R.S.P.  (Volunteered)",                            
                                                    "J.D.S.  (Volunteered)",        
                                                    "MANSE  (Volunteered)",                             
                                                    "Maharatera navnirman sena  (Volunteered)", 
                                                    "YSR  congress  (Volunteered)",                     
                                                    "Ralod  (Volunteered)",
                                                    "RPI  (Volunteered)",                               
                                                    "Jharkhand  mukti morcha  (Volunteered)") ~ "Other",
                                                  "None/No party (Volunteered)" ~ "NoParty"),
                             male = ifelse(Q141 == "Male", 1, 0),
                             age = as.numeric(as.character(Q142))
)


# Clean 2014s -------------------------------------------------------------

# 15a us, b china, e russia
# 110 china territory disputes lead to conflict?
# 132 gender, 133 age, 138 edu, 140 empl, 149 income, 152 ethn, 158 partyid, 175 region, REL religion

df_2014s <- df_2014s |> mutate(approve_us = case_match(Q15A, "Very favorable" ~ "Very favorable",
                                                     "Somewhat favorable" ~ "Somewhat favorable",
                                                     "Somewhat unfavorable " ~ "Somewhat unfavorable ",
                                                     "Very unfavorable " ~ "Very unfavorable"),
                             approve_cn = case_match(Q15B, "Very favorable" ~ "Very favorable",
                                                     "Somewhat favorable" ~ "Somewhat favorable",
                                                     "Somewhat unfavorable " ~ "Somewhat unfavorable",
                                                     "Very unfavorable " ~ "Very unfavorable"),
                             approve_ru = case_match(Q15E, "Very favorable" ~ "Very favorable",
                                                     "Somewhat favorable" ~ "Somewhat favorable",
                                                     "Somewhat unfavorable " ~ "Somewhat unfavorable",
                                                     "Very unfavorable " ~ "Very unfavorable"),
                             approve_us_bin = ifelse(approve_us %in% c("Very favorable", "Somewhat favorable"), 1, 0),
                             approve_cn_bin = ifelse(approve_cn %in% c("Very favorable", "Somewhat favorable"), 1, 0),
                             approve_ru_bin = ifelse(approve_ru %in% c("Very favorable", "Somewhat favorable"), 1, 0),
                             edu = case_match(Q138INDIA, "Illiterate" ~ 1,
                                              "Literate but no formal schooling" ~ 2,
                                              "Primary Education (primary certificate)" ~ 3,
                                              "Upper Primary (Upper Primary Certificate)" ~ 4,
                                              c("Senior Secondary / intermediate (Senior Secondary School Leaving Certificate)",
                                                "High School; Industrial Training Institute (matriculation certificate or ITI certificate",
                                                "STILL IN EDUCATION (Volunteered)")~ 5,
                                              c("Technical Education Training; Junior teachers training (diploma)",
                                                "Tertiary, technical higher education (diploma / bachelor)  (Instruction: Vocational education such as nursing degree or")~ 6,
                                              "Bachelor (university 1st)" ~ 7,
                                              c("Master (university  2nd)", "Doctor (Tertiary 2nd)") ~ 8),
                             empl = case_match(Q140, c("In paid work",
                                                       "Apprentice or trainee") ~ "Employed",
                                               c("In education (not paid for by employer), in school, student even if on vacation",
                                                 "Unemployed and looking for a job",
                                                 "Retired",
                                                 "Doing housework, looking after the home, children or other persons (not paid)",
                                                 "Permanently sick or disabled") ~ "Unemployed"),
                             income = case_match(Q149INDIA,
                                                 "500" ~ 1,
                                                 "1000" ~ 3,
                                                 c("1200","1500") ~ 4,
                                                 c("1600","1800","1900","2000") ~ 5,
                                                 c("2100", "2400", "2500") ~ 6, 
                                                 "3000" ~ 7, 
                                                 c("3500","3600","3900","4000") ~ 8,
                                                 c("4200","4500","4600","5000") ~ 9,
                                                 c("5200","5400","5500","6000") ~ 10,
                                                 c("6500","7000","7500","8000","8500","8600","8700","9000","10000") ~ 11,
                                                 c("10500","11000","12000","12500","13000","13500","14000","14500","15000") ~ 12,
                                                 c("16000","17000","17500","17800","18000","19000","19700","20000") ~ 13,
                                                 c("21000","22000","23000","24000","25000","26000","27000","28000","29000","30000") ~ 14,
                                                 c("32000","34000","35000","36000","37000","40000") ~ 15,
                                                 c("42200","43000","44000","45000","48000","50000") ~ 16,
                                                 c("60000","62000","70000","72000","75000","78000","80000","90000","93000","1e+05") ~ 17,
                                                 c("120000","142000","150000","180000","2e+05","245000","250000","3e+05","5e+05","6e+05","610000") ~ 18),
                             region = case_match(Q175INDIA,
                                                 "North" ~ "North",
                                                 "South" ~ "South",
                                                 "East" ~ "East",
                                                 "West" ~ "West"),
                             religion = case_match(QRELIND,
                                                   "Buddhist" ~ "Buddhist",
                                                   c("Catholic",
                                                     "Just a Christian (Volunteered)",
                                                     "Protestant (including Anglican, Evangelical, Pentecostal, Seventh Day Adventist)") ~ "Christian",
                                                   "Hindu" ~ "Hindu",
                                                   "Jain" ~ "Jain",
                                                   "Muslim" ~ "Muslim",
                                                   "Sikh" ~ "Sikh",
                                                   c("Atheist", "Agnostic", "Nothing in particular, or") ~ "None"),
                             partyid = case_match(Q158INDIA,
                                                  "BJP: Bharatiya Janata Party" ~ "BJP",
                                                  "Congress Party" ~ "INC",
                                                  c("CPI (M): Communist Party of India (Marxist)","Trinamul Congress",                                         
                                                    "BSP: Bahujan Samaj Party","SP: Samajwadi Party",                                       
                                                    "Shiv Sena","Nationalist Congress Party",                                
                                                    "Maharashtra Nav Nirman Sena(MNS)","Rashtriya Janata Dal(RJD)",                                 
                                                    "Janata Dal United(JDU)","Lok JanShakti Party (LJP)",                                 
                                                    "Telegu Desham Party (TDP)","Telangana Rashtra  Samithi",                                
                                                    "Dravida Munnetra Kazhagam ( DMK)","All India Anna Dravida Munnetra Kazhagam (AIADMK)",         
                                                    "All India Majlis-e-Ittehadul Muslimeen (AIMIM / MIM)","Yuvajana Sramika Rythu Congress Party (YSR Congress Party)",
                                                    "Jharkhand Mukti Morcha (JMM)","DMDK",                                                      
                                                    "Janata Dal (Secular) (JD (S)","Akali Dal",                                                 
                                                    "Biju Janata Dal (BJD)","Communist Party of India (CPI)",                            
                                                    "Aam Aadmi Party (AAP)","Other (Volunteered)") ~ "Other",
                                                  "None/No party (Volunteered)" ~ "NoParty"),
                             male = ifelse(Q132 == "Male", 1, 0),
                             age = as.numeric(as.character(Q133))
)

# Clean 2014w -------------------------------------------------------------
# Q9 a us, c china, e russia
# 144d china threat
# 164 gender, 165 age, 180india edu, 181 empl, 183india income, 186india ethn , 190india partyid, 207india region

df_2014w <- df_2014w |> mutate(approve_us = case_match(Q9a, "Very favorable" ~ "Very favorable",
                                                       "Somewhat favorable" ~ "Somewhat favorable",
                                                       "Somewhat unfavorable" ~ "Somewhat unfavorable ",
                                                       "Very unfavorable" ~ "Very unfavorable"),
                               approve_cn = case_match(Q9c, "Very favorable" ~ "Very favorable",
                                                       "Somewhat favorable" ~ "Somewhat favorable",
                                                       "Somewhat unfavorable" ~ "Somewhat unfavorable",
                                                       "Very unfavorable" ~ "Very unfavorable"),
                               approve_ru = case_match(Q9e, "Very favorable" ~ "Very favorable",
                                                       "Somewhat favorable" ~ "Somewhat favorable",
                                                       "Somewhat unfavorable" ~ "Somewhat unfavorable",
                                                       "Very unfavorable" ~ "Very unfavorable"),
                               approve_us_bin = ifelse(approve_us %in% c("Very favorable", "Somewhat favorable"), 1, 0),
                               approve_cn_bin = ifelse(approve_cn %in% c("Very favorable", "Somewhat favorable"), 1, 0),
                               approve_ru_bin = ifelse(approve_ru %in% c("Very favorable", "Somewhat favorable"), 1, 0),
                               edu = case_match(Q180INDIA, "Illiterate" ~ 1,
                                                "Literate but no formal schooling" ~ 2,
                                                "School up to 4 years" ~ 3,
                                                "School up to 5 to 9 years" ~ 4,
                                                "SSC/HSC" ~ 5,
                                                "Some college but not graduated" ~ 6,
                                                "Graduate/Post grad-Gen BA MSc BCOM etc" ~ 7,
                                                "Graduate/Post grad-Prof BE MTech MBA MBBS etc" ~ 8),
                               empl = case_match(Q181, c("Yes, employed") ~ "Employed",
                                                 c("No, not employed") ~ "Unemployed"),
                               income = case_match(Q183INDIA,
                                                   "Up to Rs. 500 per month" ~ 1,
                                                   "Rs. 501 – Rs. 750" ~ 2,    
                                                   "Rs. 751 – Rs. 1000" ~ 3,
                                                   "Rs. 1001 – Rs. 1500" ~ 4,
                                                   "Rs. 1501 – Rs. 2000" ~ 5,
                                                   "Rs. 2001 – Rs. 2500" ~ 6, 
                                                   "Rs. 2501 – Rs. 3000" ~ 7, 
                                                   "Rs. 3001 – Rs. 4000" ~ 8,
                                                   "Rs. 4001 – Rs. 5000" ~ 9,
                                                   "Rs. 5001 – Rs. 6000" ~ 10,
                                                   "Rs. 6001 – Rs. 10,000" ~ 11,
                                                   "Rs. 10,001 – Rs. 15,000" ~ 12,
                                                   "Rs. 15,001 – Rs. 20,000" ~ 13,
                                                   "Rs. 20,001 – Rs. 30,000" ~ 14,
                                                   "Rs. 30,001 – Rs. 40,000" ~ 15,
                                                   "Rs. 40,001 – Rs. 50,000" ~ 16,
                                                   "Rs. 50,001 – Rs. 100,000" ~ 17,
                                                   "Rs. 100,001 and above" ~ 18),
                               region = case_match(Q207INDIA,
                                                   "North" ~ "North",
                                                   "South" ~ "South",
                                                   "East" ~ "East",
                                                   "West" ~ "West"),
                               religion = case_match(Q55INDIA,
                                                     "Buddhist" ~ "Buddhist",
                                                     "Christian" ~ "Christian",
                                                     "Hindu" ~ "Hindu",
                                                     "Jain" ~ "Jain",
                                                     "Muslim" ~ "Muslim",
                                                     "Sikh" ~ "Sikh",
                                                     c("No religion/not a believer/atheist/agnostic (Volunteered)",
                                                       "Other religion (Volunteered)")~ "None"),
                               partyid = case_match(Q190INDIA,
                                                    "BJP: Bharatiya Janata Party" ~ "BJP",
                                                    "Congress Party" ~ "INC",
                                                    c("CPI (M): Communist Party of India (Marxist)","Trinamul Congress",                                         
                                                      "BSP: Bahujan Samaj Party","SP: Samajwadi Party",                                       
                                                      "Shiv Sena","Nationalist Congress Party",                                
                                                      "Maharashtra Nav Nirman Sena (MNS)","Rashtriya Janata Dal(RJD)",                                 
                                                      "Janata Dal United (JDU)","Lok JanShakti Party (LJP)",                                 
                                                      "Telegu Desham Party (TDP)","Telangana Rashtra  Samithi",                                
                                                      "Dravida Munnetra Kazhagam (DMK)","All India Anna Dravida Munnetra Kazhagam (AIADMK)",         
                                                      "All India Majlis-e-Ittehadul Muslimeen (AIMIM / MIM)","Yuvajana Sramika Rythu Congress Party (YSR Congress Party)",
                                                      "Jharkhand Mukti Morcha (JMM)","DMDK",                                                      
                                                      "Janata Dal (Secular) (JD (S))","Akali Dal",                                                 
                                                      "Biju Janata Dal (BJD)","Communist Party of India (CPI)",                            
                                                      "Other (Volunteered)") ~ "Other",
                                                    "None/No party (Volunteered)" ~ "NoParty"),
                               male = ifelse(Q164 == "Male", 1, 0),
                               age = as.numeric(as.character(Q165))
)

# Clean 2015 ---------------------------------------------------------------
# 12a us, b china, d russia
# 145 gender, 146 age, 163edu, 164 empl, 167 caste, 182partyid, 213 region


df_2015 <- df_2015 |> mutate(approve_us = case_match(Q12A, "Very favorable" ~ "Very favorable",
                                                       "Somewhat favorable" ~ "Somewhat favorable",
                                                       "Somewhat unfavorable " ~ "Somewhat unfavorable ",
                                                       "Very unfavorable " ~ "Very unfavorable"),
                               approve_cn = case_match(Q12B, "Very favorable" ~ "Very favorable",
                                                       "Somewhat favorable" ~ "Somewhat favorable",
                                                       "Somewhat unfavorable " ~ "Somewhat unfavorable",
                                                       "Very unfavorable " ~ "Very unfavorable"),
                               approve_ru = case_match(Q12D, "Very favorable" ~ "Very favorable",
                                                       "Somewhat favorable" ~ "Somewhat favorable",
                                                       "Somewhat unfavorable " ~ "Somewhat unfavorable",
                                                       "Very unfavorable " ~ "Very unfavorable"),
                               approve_us_bin = ifelse(approve_us %in% c("Very favorable", "Somewhat favorable"), 1, 0),
                               approve_cn_bin = ifelse(approve_cn %in% c("Very favorable", "Somewhat favorable"), 1, 0),
                               approve_ru_bin = ifelse(approve_ru %in% c("Very favorable", "Somewhat favorable"), 1, 0),
                               edu = case_match(Q163INDIA, "Illiterate" ~ 1,
                                                "Literate but no formal schooling" ~ 2,
                                                "Primary Education (primary certificate)" ~ 3,
                                                "Upper Primary (Upper Primary Certificate)" ~ 4,
                                                c("Senior Secondary / intermediate (Senior Secondary School Leaving Certificate)",
                                                  "High School; Industrial Training Institute (matriculation certificate or ITI certificate)")~ 5,
                                                c("Technical Education Training; Junior teachers training (diploma)",
                                                  "Tertiary, technical higher education (diploma / bachelor)")~ 6,
                                                "Bachelor (university 1st)" ~ 7,
                                                c("Master (university 2nd)", "Doctor (Tertiary 2nd)") ~ 8),
                               empl = case_match(Q164, c("In paid work",
                                                         "Apprentice or trainee") ~ "Employed",
                                                 c("In education (not paid for by employer), in school, student even if on vacation",
                                                   "Unemployed and looking for a job",
                                                   "Retired",
                                                   "Doing housework, looking after the home, children or other persons (not paid)",
                                                   "Permanently sick or disabled") ~ "Unemployed"),
                               income = case_match(Q165INDIA,
                                                   "Up to Rs. 500 per month" ~ 1,
                                                   "Rs. 501 – Rs. 750" ~ 2,    
                                                   "Rs. 751 – Rs. 1000" ~ 3,
                                                   "Rs. 1001 – Rs. 1500" ~ 4,
                                                   "Rs. 1501 – Rs. 2000" ~ 5,
                                                   "Rs. 2001 – Rs. 2500" ~ 6, 
                                                   "Rs. 2501 – Rs. 3000" ~ 7, 
                                                   "Rs. 3001 – Rs. 4000" ~ 8,
                                                   "Rs. 4001 – Rs. 5000" ~ 9,
                                                   "Rs. 5001 – Rs. 6000" ~ 10,
                                                   c("Rs. 6001 – Rs. 8000","Rs. 8001 – Rs. 10,000") ~ 11,
                                                   "Rs. 10,001 – Rs. 15,000" ~ 12,
                                                   "Rs. 15,001 – Rs. 20,000" ~ 13,
                                                   "Rs. 20,001 – Rs. 30,000" ~ 14,
                                                   "Rs. 30,001 – Rs. 40,000" ~ 15,
                                                   "Rs. 40,001 – Rs. 50,000" ~ 16,
                                                   "Rs. 50,001 – Rs. 100,000" ~ 17,
                                                   "Rs. 100,001 and above" ~ 18),
                               region = case_match(Q213INDIA,
                                                   "North" ~ "North",
                                                   "South" ~ "South",
                                                   "East" ~ "East",
                                                   "West" ~ "West"),
                               religion = case_match(Q78IND,
                                                     "Buddhist" ~ "Buddhist",
                                                     c("Catholic",
                                                       "Just a Christian (Volunteered)",
                                                       "Protestant (including Anglican, Evangelical, Pentecostal, Seventh Day Adventist)") ~ "Christian",
                                                     "Hindu" ~ "Hindu",
                                                     "Jain" ~ "Jain",
                                                     "Muslim" ~ "Muslim",
                                                     "Sikh" ~ "Sikh",
                                                     c("Atheist", "Agnostic", "Nothing in particular, or") ~ "None"),
                               partyid = case_match(Q182INDIA,
                                                    "BJP: Bharatiya Janata Party" ~ "BJP",
                                                    "Congress Party" ~ "INC",
                                                    c("CPI (M): Communist Party of India (Marxist)","Trinamul Congress",                                         
                                                      "BSP: Bahujan Samaj Party","SP: Samajwadi Party",                                       
                                                      "Shiv Sena","Nationalist Congress Party",                                
                                                      "Maharashtra Nav Nirman Sena (MNS)","Rashtriya Janata Dal (RJD)",                                 
                                                      "Janata Dal United (JDU)","Lok JanShakti Party (LJP)",                                 
                                                      "Telegu Desham Party (TDP)","Telangana Rashtra  Samithi",                                
                                                      "Dravida Munnetra Kazhagam (DMK)","All India Anna Dravida Munnetra Kazhagam (AIADMK)",         
                                                      "All India Majlis-e-Ittehadul Muslimeen (AIMIM / MIM)","Yuvajana Sramika Rythu Congress Party (YSR Congress Party)",
                                                      "Jharkhand Mukti Morcha (JMM)","DMDK",                                                      
                                                      "Janata Dal (Secular) (JD (S))","Akali Dal",                                                 
                                                      "Biju Janata Dal (BJD)","Communist Party of India (CPI)",                            
                                                      "Aam Aadmi Party (AAP)","Other (Volunteered)") ~ "Other",
                                                    "None/No party (Volunteered)" ~ "NoParty"),
                               male = ifelse(Q145 == "Male", 1, 0),
                               age = as.numeric(as.character(Q146))
)

# Clean 2016 --------------------------------------------------------------
# 10a us, b china, NO RUSSIA
# 107 gender, 108 age, 113a edu,  116 income, 118 caste, 131 partyid, s5 or s8 region


df_2016 <- df_2016 |> mutate(approve_us = case_match(Q10A, "Very favorable" ~ "Very favorable",
                                                     "Somewhat favorable" ~ "Somewhat favorable",
                                                     "Somewhat unfavorable " ~ "Somewhat unfavorable ",
                                                     "Very unfavorable " ~ "Very unfavorable"),
                             approve_cn = case_match(Q10B, "Very favorable" ~ "Very favorable",
                                                     "Somewhat favorable" ~ "Somewhat favorable",
                                                     "Somewhat unfavorable " ~ "Somewhat unfavorable",
                                                     "Very unfavorable " ~ "Very unfavorable"),
                             # approve_ru = case_match(Q12D, "Very favorable" ~ "Very favorable",
                             #                         "Somewhat favorable" ~ "Somewhat favorable",
                             #                         "Somewhat unfavorable " ~ "Somewhat unfavorable",
                             #                         "Very unfavorable " ~ "Very unfavorable"),
                             approve_us_bin = ifelse(approve_us %in% c("Very favorable", "Somewhat favorable"), 1, 0),
                             approve_cn_bin = ifelse(approve_cn %in% c("Very favorable", "Somewhat favorable"), 1, 0),
                             # approve_ru_bin = ifelse(approve_ru %in% c("Very favorable", "Somewhat favorable"), 1, 0),
                             edu = case_match(Q113INDa, "Illiterate" ~ 1,
                                              "Literate but no formal schooling" ~ 2,
                                              "Primary Education (primary certificate)" ~ 3,
                                              "Upper Primary (Upper Primary Certificate)" ~ 4,
                                              c("Senior Secondary / intermediate (Senior Secondary School Leaving Certificate)",
                                                "High School; Industrial Training Institute (matriculation certificate or ITI certificate)")~ 5,
                                              c("Technical Education Training; Junior teachers training (diploma)",
                                                "Tertiary, technical higher education (diploma / bachelor)")~ 6,
                                              "Bachelor (university 1st)" ~ 7,
                                              c("Master (university 2nd)", "Doctor (Tertiary 2nd)") ~ 8),
                             # empl = case_match(Q164, c("In paid work",
                             #                           "Apprentice or trainee") ~ "Employed",
                             #                   c("In education (not paid for by employer), in school, student even if on vacation",
                             #                     "Unemployed and looking for a job",
                             #                     "Retired",
                             #                     "Doing housework, looking after the home, children or other persons (not paid)",
                             #                     "Permanently sick or disabled") ~ "Unemployed"),
                             income = case_match(Q116IND,
                                                 "Up to Rs. 1,000 per month" ~ 3,
                                                 "Rs. 1,001 – Rs. 1,500" ~ 4,
                                                 "Rs. 1,501 – Rs. 2,000" ~ 5,
                                                 "Rs. 2,001 – Rs. 2,500" ~ 6, 
                                                 "Rs. 2,501 – Rs. 3,000" ~ 7, 
                                                 "Rs. 3,001 – Rs. 4,000" ~ 8,
                                                 "Rs. 4,001 – Rs. 5,000" ~ 9,
                                                 "Rs. 5,001 – Rs. 6,000" ~ 10,
                                                 c("Rs. 6,001 – Rs. 8,000","Rs. 8,001 – Rs. 9,000", "Rs. 9,001 - Rs. 10,000") ~ 11,
                                                 "Rs. 10,001 – Rs. 15,000" ~ 12,
                                                 "Rs. 15,001 – Rs. 20,000" ~ 13,
                                                 "Rs. 20,001 – Rs. 30,000" ~ 14,
                                                 "Rs. 30,001 – Rs. 40,000" ~ 15,
                                                 "Rs. 40,001 – Rs. 50,000" ~ 16,
                                                 "Rs. 50,001 – Rs. 100,000" ~ 17,
                                                 "Rs. 100,001 and above" ~ 18),
                             region = case_match(QS5INDIA,
                                                 "North" ~ "North",
                                                 "South" ~ "South",
                                                 "East" ~ "East",
                                                 "West" ~ "West"),
                             religion = case_match(Q109IND,
                                                   "Buddhist" ~ "Buddhist",
                                                   c("Catholic",
                                                     "Just a Christian (DO NOT READ)",
                                                     "Protestant (including Anglican, Evangelical, Pentecostal, Seventh Day Adventist)") ~ "Christian",
                                                   "Hindu" ~ "Hindu",
                                                   "Jain" ~ "Jain",
                                                   "Muslim" ~ "Muslim",
                                                   "Sikh" ~ "Sikh",
                                                   c("Atheist", "Agnostic", "Nothing in particular, or") ~ "None"),
                             partyid = case_match(Q131IND,
                                                  "BJP: Bharatiya Janata Party" ~ "BJP",
                                                  "Congress Party" ~ "INC",
                                                  c("CPI (M): Communist Party of India (Marxist)","Trinamul Congress",                                         
                                                    "BSP: Bahujan Samaj Party","SP: Samajwadi Party",                                       
                                                    "Shiv Sena","Nationalist Congress Party",                                
                                                    "Maharashtra Nav Nirman Sena (MNS)","Rashtriya Janata Dal (RJD)",                                 
                                                    "Janata Dal United (JDU)","Lok JanShakti Party (LJP)",                                 
                                                    "Telegu Desham Party (TDP)","Telangana Rashtra  Samithi",                                
                                                    "Dravida Munnetra Kazhagam (DMK)","All India Anna Dravida Munnetra Kazhagam (AIADMK)",         
                                                    "All India Majlis-e-Ittehadul Muslimeen (AIMIM / MIM)","Yuvajana Sramika Rythu Congress Party (YSR Congress Party)",
                                                    "Jharkhand Mukti Morcha (JMM)","DMDK",                                                      
                                                    "Janata Dal (Secular) (JD (S))","Akali Dal",                                                 
                                                    "Biju Janata Dal (BJD)","Communist Party of India (CPI)",                            
                                                    "Aam Aadmi Party (AAP)","Other (DO NOT READ)", "26", "27") ~ "Other",
                                                  "None/No party (DO NOT READ)" ~ "NoParty"),
                             male = ifelse(q107 == "Male", 1, 0),
                             age = as.numeric(as.character(q108))
)

# Clean 2017 --------------------------------------------------------------
# 12a us, c china, e russia
# 17a china threat
# 144 gender, 145 age, 159 edu, 160 income, 165 partyid, 

df_2017 <- df_2017 |> mutate(approve_us = case_match(fav_US, "Very favorable" ~ "Very favorable",
                                                     "Somewhat favorable" ~ "Somewhat favorable",
                                                     "Somewhat unfavorable " ~ "Somewhat unfavorable ",
                                                     "Very unfavorable " ~ "Very unfavorable"),
                             approve_cn = case_match(fav_China, "Very favorable" ~ "Very favorable",
                                                     "Somewhat favorable" ~ "Somewhat favorable",
                                                     "Somewhat unfavorable " ~ "Somewhat unfavorable",
                                                     "Very unfavorable " ~ "Very unfavorable"),
                             approve_ru = case_match(fav_Russia, "Very favorable" ~ "Very favorable",
                                                     "Somewhat favorable" ~ "Somewhat favorable",
                                                     "Somewhat unfavorable " ~ "Somewhat unfavorable",
                                                     "Very unfavorable " ~ "Very unfavorable"),
                             approve_us_bin = ifelse(approve_us %in% c("Very favorable", "Somewhat favorable"), 1, 0),
                             approve_cn_bin = ifelse(approve_cn %in% c("Very favorable", "Somewhat favorable"), 1, 0),
                             approve_ru_bin = ifelse(approve_ru %in% c("Very favorable", "Somewhat favorable"), 1, 0),
                             edu = case_match(d_educ_india, "Illiterate" ~ 1,
                                              "Literate but no formal schooling" ~ 2,
                                              "Primary Education (primary certificate)" ~ 3,
                                              "Upper Primary (Upper Primary Certificate)" ~ 4,
                                              c("Senior Secondary / intermediate (Senior Secondary School Leaving Certificate)",
                                                "High School; Industrial Training Institute (matriculation certificate or ITI certificate)")~ 5,
                                              c("Technical Education Training",
                                                "Nursing, General nursing and Midwifery (GNM); Junior teachers training (diploma)")~ 6,
                                              "Bachelor (university 1st), or MBBS, LLB" ~ 7,
                                              c("Master (university 2nd), or Post graduate diploma", "Doctor (Tertiary 2nd)") ~ 8),
                             # empl = case_match(Q164, c("In paid work",
                             #                           "Apprentice or trainee") ~ "Employed",
                             #                   c("In education (not paid for by employer), in school, student even if on vacation",
                             #                     "Unemployed and looking for a job",
                             #                     "Retired",
                             #                     "Doing housework, looking after the home, children or other persons (not paid)",
                             #                     "Permanently sick or disabled") ~ "Unemployed"),
                             income = case_match(d_income_india,
                                                 "Up to Rs. 1,000 per month" ~ 3,
                                                 "Rs. 1,001 to Rs. 1,500" ~ 4,
                                                 "Rs. 1,501 to Rs. 2,000" ~ 5,
                                                 "Rs. 2,001 to Rs. 2,500" ~ 6, 
                                                 "Rs. 2,501 to Rs. 3,000" ~ 7, 
                                                 "Rs. 3,001 to Rs. 4,000" ~ 8,
                                                 "Rs. 4,001 to Rs. 5,000" ~ 9,
                                                 "Rs. 5,001 to Rs. 6,000" ~ 10,
                                                 c("Rs. 6,001 to Rs. 8,000","Rs. 8,001 to Rs. 9,000", "Rs. 9,001 to Rs. 10,000") ~ 11,
                                                 "Rs. 10,001 to Rs. 15,000" ~ 12,
                                                 "Rs. 15,001 to Rs. 20,000" ~ 13,
                                                 "Rs. 20,001 to Rs. 30,000" ~ 14,
                                                 "Rs. 30,001 to Rs. 40,000" ~ 15,
                                                 "Rs. 40,001 to Rs. 50,000" ~ 16,
                                                 "Rs. 50,001 to Rs. 100,000" ~ 17,
                                                 "Rs. 100,001 and above" ~ 18),
                             region = case_match(QS5IND,
                                                 "North" ~ "North",
                                                 "South" ~ "South",
                                                 "East" ~ "East",
                                                 "West" ~ "West"),
                             religion = coalesce(d_relig_india_A, d_relig_india_B_1),
                             religion = case_match(religion,
                                                   "Buddhist" ~ "Buddhist",
                                                   c("Catholic",
                                                     "Just a Christian (DO NOT READ)",
                                                     "Protestant (including Anglican, Evangelical, Pentecostal, Seventh Day Adventist)",
                                                     "Jehovah’s Witness (DO NOT READ)",
                                                     "Mormon (Church of Jesus Christ of Latter-day Saints/LDS) (DO NOT READ)") ~ "Christian",
                                                   "Hindu" ~ "Hindu",
                                                   "Jain" ~ "Jain",
                                                   "Muslim" ~ "Muslim",
                                                   "Sikh" ~ "Sikh",
                                                   c("Atheist (I do not believe in any gods or God)", "Agnostic (I don't really know whether there is a god, or whether there are any gods)", "Nothing in particular") ~ "None"),
                             partyid = case_match(d_ptyid_proximity_india,
                                                  "BJP: Bharatiya Janata Party" ~ "BJP",
                                                  "Congress Party" ~ "INC",
                                                  c("CPI (M): Communist Party of India (Marxist)","Trinamul Congress",                                         
                                                    "BSP: Bahujan Samaj Party","SP: Samajwadi Party",                                       
                                                    "Shiv Sena","Nationalist Congress Party",                                
                                                    "Maharashtra Nav Nirman Sena(MNS)","Rashtriya Janata Dal(RJD)",                                 
                                                    "Janata Dal United(JDU)","Lok JanShakti Party (LJP)",                                 
                                                    "Telegu Desham Party (TDP)","Telangana Rashtra  Samithi",                                
                                                    "Dravida Munnetra Kazhagam (DMK)","All India Anna Dravida Munnetra Kazhagam (AIADMK)",         
                                                    "All India MajlistoetoIttehadul Muslimeen AIMIM / MIM)","Yuvajana Sramika Rythu Congress Party (YSR Congress Party)",
                                                    "Jharkhand Mukti Morcha (JMM)","DMDK",                                                      
                                                    "Janata Dal (Secular) (JD (S))","Akali Dal",                                                 
                                                    "Biju Janata Dal (BJD)","Communist Party of India (CPI)",                            
                                                    "Aam Aadmi Party (AAP)","Other (SPECIFY)") ~ "Other",
                                                  "Do not feel close to any party" ~ "NoParty"),
                             male = ifelse(sex == "Male", 1, 0),
                             age = as.numeric(as.character(age))
)

# Clean 2018 --------------------------------------------------------------
# 17 a us, b china, c russia
# 83 gender 84 age, 96 edu, 97 inc, 103 partyid, 

df_2018 <- df_2018 |> mutate(approve_us = case_match(fav_US, "Very favorable" ~ "Very favorable",
                                                     "Somewhat favorable" ~ "Somewhat favorable",
                                                     "Somewhat unfavorable" ~ "Somewhat unfavorable ",
                                                     "Very unfavorable" ~ "Very unfavorable"),
                             approve_cn = case_match(fav_China, "Very favorable" ~ "Very favorable",
                                                     "Somewhat favorable" ~ "Somewhat favorable",
                                                     "Somewhat unfavorable" ~ "Somewhat unfavorable",
                                                     "Very unfavorable" ~ "Very unfavorable"),
                             approve_ru = case_match(fav_Russia, "Very favorable" ~ "Very favorable",
                                                     "Somewhat favorable" ~ "Somewhat favorable",
                                                     "Somewhat unfavorable" ~ "Somewhat unfavorable",
                                                     "Very unfavorable" ~ "Very unfavorable"),
                             approve_us_bin = ifelse(approve_us %in% c("Very favorable", "Somewhat favorable"), 1, 0),
                             approve_cn_bin = ifelse(approve_cn %in% c("Very favorable", "Somewhat favorable"), 1, 0),
                             approve_ru_bin = ifelse(approve_ru %in% c("Very favorable", "Somewhat favorable"), 1, 0),
                             edu = case_match(d_educ_india, "Illiterate" ~ 1,
                                              "Literate but no formal schooling" ~ 2,
                                              "Primary Education (primary certificate, up to 5th standard)" ~ 3,
                                              "Upper Primary (Upper Primary Certificate, up to 8th standard)" ~ 4,
                                              c("Senior Secondary / intermediate (Senior Secondary School Leaving Certificate)",
                                                "High School; Industrial Training Institute (matriculation certificate or ITI certificate, up to 10th standard)")~ 5,
                                              c("Technical Education Training",
                                                "Nursing, General nursing and Midwifery (GNM); Junior teachers training (diploma)")~ 6,
                                              "Bachelor (university 1st), or MBBS, LLB" ~ 7,
                                              c("Master (university 2nd), or Post graduate diploma", "Doctor (Tertiary 2nd)") ~ 8),
                             # empl = case_match(Q164, c("In paid work",
                             #                           "Apprentice or trainee") ~ "Employed",
                             #                   c("In education (not paid for by employer), in school, student even if on vacation",
                             #                     "Unemployed and looking for a job",
                             #                     "Retired",
                             #                     "Doing housework, looking after the home, children or other persons (not paid)",
                             #                     "Permanently sick or disabled") ~ "Unemployed"),
                             income = case_match(d_income_india,
                                                 "Up to Rs. 2,000 per month" ~ 5,
                                                 "Rs. 2,001  to Rs. 3,500" ~ 8, 
                                                 "Rs. 3,501  to Rs. 5,000" ~ 9,
                                                 "Rs. 5,001  to Rs. 6,500" ~ 10,
                                                 c("Rs. 6,501  to Rs. 8,000","Rs. 8,001  to Rs. 10,000") ~ 11,
                                                 "Rs. 10,001  to Rs. 15,000" ~ 12,
                                                 "Rs. 15,001  to Rs. 20,000" ~ 13,
                                                 "Rs. 20,001  to Rs. 40,000" ~ 15,
                                                 "Rs. 40,001  to Rs. 60,000" ~ 17,
                                                 "Rs. 60,001" ~ 18),
                             region = case_match(QS5IND,
                                                 "North" ~ "North",
                                                 "South" ~ "South",
                                                 c("East", "Northeast") ~ "East",
                                                 "West" ~ "West"),
                             religion = case_match(d_relig_india_A,
                                                   "Buddhist" ~ "Buddhist",
                                                   c("Catholic",
                                                     "Just a Christian (DO NOT READ)",
                                                     "Protestant (including Anglican, Evangelical, Pentecostal, Seventh Day Adventist)",
                                                     "Jehovah's Witness (DO NOT READ)") ~ "Christian",
                                                   "Hindu" ~ "Hindu",
                                                   "Jain" ~ "Jain",
                                                   "Muslim" ~ "Muslim",
                                                   "Sikh" ~ "Sikh",
                                                   c("Atheist (I do not believe in any gods or God)", "Agnostic (I don't really know whether there is a god, or whether there are any gods)", "Nothing in particular", "Something else (SPECIFY), or") ~ "None"),
                             partyid = case_match(d_ptyid_proximity_india,
                                                  "BJP: Bharatiya Janata Party" ~ "BJP",
                                                  "Congress Party" ~ "INC",
                                                  c("CPI (M): Communist Party of India (Marxist)","Trinamul Congress",                                         
                                                    "BSP: Bahujan Samaj Party","SP: Samajwadi Party",                                       
                                                    "Shiv Sena","Nationalist Congress Party",                                
                                                    "Maharashtra Nav Nirman Sena (MNS)","Rashtriya Janata Dal (RJD)",                                 
                                                    "Janata Dal United (JDU)","Lok JanShakti Party (LJP)",                                 
                                                    "Telegu Desham Party (TDP)","Telangana Rashtra Samithi",                                
                                                    "Dravida Munnetra Kazhagam (DMK)","All India Anna Dravida Munnetra Kazhagam (AIADMK)",         
                                                    "All India MajlistoetoIttehadul Muslimeen (AIMIM/MIM)","Yuvajana Sramika Rythu Congress Party (YSR Congress Party)",
                                                    "Jharkland Mukti Morcha (JMM)","DMDK",                                                      
                                                    "Janata Dal (Secular) (JD (S))","Akali Dal",                                                 
                                                    "Biju Janata Dal (BJD)","Communist Party of India (CPI)",                            
                                                    "Aam Aadmi Party (AAP)","Other (SPECIFY)") ~ "Other",
                                                  "Do not feel close to any party" ~ "NoParty"),
                             male = ifelse(sex == "Male", 1, 0),
                             age = as.numeric(as.character(age))
)


# Combine all years -------------------------------------------------------

df2002 <- df_2002 |> select(YEAR, approve_us, approve_us_bin,
                            edu, empl, income, religion, male, age, region)
df2007 <- df_2007 |> select(YEAR, approve_us, approve_cn, approve_ru, approve_us_bin, approve_cn_bin, approve_ru_bin,
                            edu, empl, income, religion, male, age, region)
df2008 <- df_2008 |> select(YEAR, approve_us, approve_cn, approve_us_bin, approve_cn_bin,
                            edu, empl, income, religion, male, age, region)
df2009 <- df_2009 |> select(YEAR, approve_us, approve_cn, approve_ru, approve_us_bin, approve_cn_bin, approve_ru_bin,
                            edu, empl, income, religion, male, age, region, partyid)
df2010 <- df_2010 |> select(YEAR, approve_us, approve_cn, approve_ru, approve_us_bin, approve_cn_bin, approve_ru_bin,
                            edu, empl, income, religion, male, age, region, partyid)
df2011 <- df_2011 |> select(YEAR, approve_us, approve_cn, approve_ru, approve_us_bin, approve_cn_bin, approve_ru_bin,
                            edu, empl, income, religion, male, age, region, partyid)
df2012 <- df_2012 |> select(YEAR, approve_us, approve_cn, approve_ru, approve_us_bin, approve_cn_bin, approve_ru_bin,
                            edu, empl, income, religion, male, age, region, partyid)
df2014w <- df_2014w |> select(YEAR, approve_us, approve_cn, approve_ru, approve_us_bin, approve_cn_bin, approve_ru_bin,
                            edu, empl, income, religion, male, age, region, partyid)
df2014s <- df_2014s |> select(YEAR, approve_us, approve_cn, approve_ru, approve_us_bin, approve_cn_bin, approve_ru_bin,
                            edu, empl, income, religion, male, age, region, partyid)
df2015 <- df_2015 |> select(YEAR, approve_us, approve_cn, approve_ru, approve_us_bin, approve_cn_bin, approve_ru_bin,
                            edu, empl, income, religion, male, age, region, partyid)
df2016 <- df_2016 |> select(YEAR, approve_us, approve_cn, approve_us_bin, approve_cn_bin,
                            edu, income, religion, male, age, region, partyid)
df2017 <- df_2017 |> select(YEAR, approve_us, approve_cn, approve_ru, approve_us_bin, approve_cn_bin, approve_ru_bin,
                            edu, income, religion, male, age, region, partyid)
df2018 <- df_2018 |> select(YEAR, approve_us, approve_cn, approve_ru, approve_us_bin, approve_cn_bin, approve_ru_bin,
                            edu, income, religion, male, age, region, partyid)

# Create NAs for missing columns

df2002$approve_cn <- NA
df2002$approve_cn_bin <- NA
df2002$approve_ru <- NA
df2002$approve_ru_bin <- NA
df2002$partyid <- NA

df2007$partyid <- NA

df2008$approve_ru <- NA
df2008$approve_ru_bin <- NA
df2008$partyid <- NA

df2016$approve_ru <- NA
df2016$approve_ru_bin <- NA
df2016$empl <- NA

df2017$empl <- NA

df2018$empl <- NA

# Bind rows

dfall <- bind_rows(df2002, df2007, df2008, df2009, df2010, df2011, df2012, df2014w, df2014s, df2015, df2016, df2017, df2018)

save(dfall, file = here::here("data/pew_clean.RData"))
cat("Saved: data/pew_clean.RData\n")
