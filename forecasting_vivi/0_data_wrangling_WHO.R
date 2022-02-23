#' Make a note of when the report was generated.
Sys.time()
knitr.table.format = "markdown"

#' As always, remove all objects fromt the workspace before starting.
rm(list = ls(all = TRUE))

setwd("C:/Users/hw3616/Desktop/Imperial/Project1_Forecasting/Project_Coding/iiag/forecasting_vivi")

#' Pull in packages needed
source("./functions/load_packages.R")

pkgs <- c("xgboost", "stringr", "rasterVis", "cdcfluview", "hrbrthemes",
          "dplyr", "ggplot2", "aweek")
load_package(pkgs)

source("./vivi_funcs.R")
source("./gbm_complex_funcs.R")

#' Load country list for WHO data.
countryISO <- read.csv("C:/Users/hw3616/Desktop/Imperial/Project1_Forecasting/Project_Coding/iiag/data_old/country_list_ISO.csv")

#' Load WHO FluID data set.
#' From 2010 week 1 to 2018 week 9 

fluWHO <- load.iiag.data.fluid(datadir = "C:/Users/hw3616/Desktop/Imperial/Project1_Forecasting/Project_Coding/iiag/data_old")

#' Extract weekly incidence for raw WHO FluID dataset
minprop <- 0.5
fluWHO.incidence <- extract.incidence.who(
  fluWHO,
  sel_iso3 = unique(fluWHO$ISO3),
  sel_ag = c("All"),
  sel_measure = c("ILI_CASES"),
  minYear=2010,
  maxYear = 2018)

sel_iso <- names(which(colSums(is.na(fluWHO.incidence))/dim(fluWHO.incidence)[1]<minprop))

#' countries whose data vailablity is over 50% are remained
fluWHO.incidence <- extract.incidence.who(
  fluWHO,
  sel_iso3 = sel_iso,
  sel_ag = c("All"),
  sel_measure = c("ILI_CASES"),
  minYear=2010,
  maxYear=2018
)

#' Check if the data of left countries is eligible for xgboost
#' check the data availablity in each year
#' exclude countries which do not have at least five consecutive weeks data in any year or do not have 
#' data records in 2010 or 2017 and 2018.
#' Current data is only up to 2018 week 9, so exclude 2018
country_year_fix <- duration(fluWHO.incidence, sel_iso, 4, 2010,2017,2015)
country_year_rolling <- duration(fluWHO.incidence, sel_iso, 1, 2010,2017,2015)

countryCode_exclude_fix <- as.character(country_year_fix$Country)[which(country_year_fix$end_year %in% c(2017, 2018) == FALSE | country_year$start_year!=2010)]
countryCode_no10_fix <- as.character(country_year_fix$Country)[which(country_year_fix$`2010`=="No")]

countryCode_exclude_rolling <- as.character(country_year_rolling$Country)[which(country_year_rolling$end_year %in% c(2017, 2018) == FALSE | country_year$start_year!=2010)]

#' countries will not be used in xgboost model because of lack of 2017 and 2018 data or 2010 data
#' They are Barbados,Belarus,Bhutan, Honduras, New Zealand, Nigeria,Oman, Pakistan,Singapore,Tajikistan,
#' Thailand, Macedonia, the former Yugoslav Republic of, Northern Mariana Islands. 
# country_no1718Or10 <- as.character(countryISO$Country)[which(sel_iso %in% countryCode_exclude_fix)] # 20 in total

#' Exlcude the countries do not have data of 2010 or 2017 
#' rolling and fix give the same countries to be excluded
#' 46 countries left
sel_iso_xgb <- sel_iso[-which(sel_iso %in% countryCode_exclude_fix)]

#' Check if left countries have less than 5 weeks data in a year, because I will do the 4-week ahead foreacast which
#' requires data of 5 consective weeks
#' Exclude countires whose datasets are uneligible to do the 4-week ahead forecast


# extract incidence of 46 eligible countries.
fluWHO.incidence <- extract.incidence.who(fluWHO,
                                          sel_iso_xgb,
                                          sel_ag = c("All"),
                                          sel_measure = c("ILI_CASES"),
                                          minYear = 2010,
                                          maxYear = 2017,
                                          yr53week = 2015)

#' Make sure the training and test sets for 1,2,3,4-week ahead are the same size for each country

length1 <- NULL
for (i in 1:length(sel_iso_xgb)){
  a <- adjust.data.size(fluWHO.incidence, sel_iso_xgb[i], 10, 1, 2015)
  tmp <- c(sel_iso_xgb[i],nrow(a))
  length1 <- rbind(length1,tmp)
}

length2 <- NULL
for (i in 1:length(sel_iso_xgb)){
  a <- adjust.data.size(fluWHO.incidence, sel_iso_xgb[i], 10,2, 2015)
  tmp <- c(sel_iso_xgb[i],nrow(a))
  length2 <- rbind(length2,tmp)
}

length3 <- NULL
for (i in 1:length(sel_iso_xgb)){
  a <- adjust.data.size(fluWHO.incidence, sel_iso_xgb[i], 10,3, 2015)
  ttmp <- c(sel_iso_xgb[i],nrow(a))
  length3 <- rbind(length3,tmp)
}

length4 <- NULL
for (i in 1:length(sel_iso_xgb)){
  a <- adjust.data.size(fluWHO.incidence, sel_iso_xgb[i], 10,4, 2015)
  tmp <- c(sel_iso_xgb[i],nrow(a))
  length4 <- rbind(length4,tmp)
}

#' Save data set as rds for later analysis
fluWHO.rds <- list(fluWHO.incidence,
                   sel_iso_xgb)
saveRDS(fluWHO.rds, "./saved_objects/fluWHO.rds")


