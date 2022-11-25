########################################
# This script is for calculating some values 
# that used in the text.
########################################

#' Make a note of when the report was generated.
Sys.time()
knitr.table.format = "markdown"

#' As always, remove all objects fromt the workspace before starting.
rm(list = ls(all = TRUE))

setwd("C:/Users/hw3616/Desktop/Imperial/Project1_Forecasting/Project_Coding/iiag/forecasting_vivi")
#setwd("C:/Users/haowe/Desktop/iiag/forecasting_vivi")

#' Pull in packages needed
source("./functions/load_packages.R")

pkgs <- c("xgboost", "stringr", "rasterVis", "cdcfluview", "hrbrthemes",
          "dplyr", "ggplot2", "aweek", "maps", "scales", "ggpubr", 
          "cowplot", "aweek", "hrbrthemes", "viridis","RColorBrewer")
load_package(pkgs)

#' Load self-written functions 
source("./vivi_funcs.R")
source("./gbm_complex_funcs.R")

#' Load country list for WHO data.
# countryISO <- read.csv("C:/Users/haowe/Desktop/iiag/data_old/country_list_ISO.csv")
countryISO <- read.csv("C:/Users/hw3616/Desktop/Imperial/Project1_Forecasting/Project_Coding/iiag/data_old/country_list_ISO.csv")

#' Load data
fluWHO <- readRDS("./saved_objects/fluWHO.rds")

fluWHO_incidence <- fluWHO[[1]]
sel_iso_xgb <- fluWHO[[2]]

gbm_complex_all <- readRDS("./saved_objects/df_gbm_complex_all.rds")

#' Quantify the number of weeks with level 1
table(gbm_complex_all$Y_week0[which(gbm_complex_all$Country == "MDA")], useNA = "always")
length(gbm_complex_all$Y_week0[which(gbm_complex_all$Country == "MDA")])

table(gbm_complex_all$Y_week0[which(gbm_complex_all$Country == "CHE")], useNA = "always")
length(gbm_complex_all$Y_week0[which(gbm_complex_all$Country == "CHE")])

table(gbm_complex_all$Y_week0[which(gbm_complex_all$Country == "EST")], useNA = "always")
length(gbm_complex_all$Y_week0[which(gbm_complex_all$Country == "EST")])


#' Countries with a lot of missing weeks
