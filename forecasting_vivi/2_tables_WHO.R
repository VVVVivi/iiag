#' Make a note of when the report was generated.
Sys.time()
knitr.table.format = "markdown"

#' As always, remove all objects fromt the workspace before starting.
rm(list = ls(all = TRUE))

# setwd("C:/Users/hw3616/Desktop/Imperial/Project1_Forecasting/Project_Coding/iiag/forecasting_vivi")
setwd("C:/Users/haowe/Desktop/iiag/forecasting_vivi")

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
countryISO <- read.csv("C:/Users/haowe/Desktop/iiag/data_old/country_list_ISO.csv")

#' Load data
fluWHO <- readRDS("./saved_objects/fluWHO.rds")

fluWHO_incidence <- fluWHO[[1]]
sel_iso_xgb <- fluWHO[[2]]

acc_WHO_roll_fix <- readRDS("./saved_objects/acc_WHO_roll_fix_new.rds")
baseline_acc <- readRDS("./saved_objects/acc_WHO_baseline.rds")
gbm_complex_all <- readRDS("./saved_objects/df_gbm_complex_all.rds")
indi_acc_baseline <- read.csv("./saved_objects/acc_hist_null_byYears.csv")
indi_acc_roll_fix <- read.csv("./saved_objects/indi_acc_roll_fix_by_countryWeek_new.csv")

countryISO$Country <- ifelse(countryISO$Country=="Moldova, Republic of", "Moldova", countryISO$Country)
countryISO$Country <- ifelse(countryISO$Country=="United States", "USA", countryISO$Country)

#### maccuracy metric - 2015 by model ######
accMetric_table_2015 <- rbind(indi_acc_roll_fix[which(indi_acc_roll_fix$Year==2015),],
                         indi_acc_baseline[which(indi_acc_baseline$Year==2015),]) %>%
  arrange(Country, Week_ahead)

accMetric_table_2015 <- accMetric_table_2015 %>%
  tidyr::pivot_wider(names_from = Model, values_from = Accuracy:macroMAE)

for (i in 1:nrow(accMetric_table_2015)){
  accMetric_table_2015$Country_fullName[i] <- countryISO$Country[which(accMetric_table_2015$Country[i] == countryISO$ISO3)]
}

write.csv(accMetric_table_2015, "./saved_objects/accMetric_table_2015.csv") 
  
  
#### accuracy metric - 2016 by model ######
accMetric_table_2016 <- rbind(indi_acc_roll_fix[which(indi_acc_roll_fix$Year==2016),],
                                indi_acc_baseline[which(indi_acc_baseline$Year==2016),]) %>%
  arrange(Country, Week_ahead)

accMetric_table_2016 <- accMetric_table_2016 %>%
  tidyr::pivot_wider(names_from = Model, values_from = Accuracy:macroMAE)

for (i in 1:nrow(accMetric_table_2016)){
  accMetric_table_2016$Country_fullName[i] <- countryISO$Country[which(accMetric_table_2016$Country[i] == countryISO$ISO3)]
}
 
write.csv(accMetric_table_2016, "./saved_objects/accMetric_table_2016.csv")  

#### accuracy metric - 2017 by model ######
accMetric_table_2017 <- rbind(indi_acc_roll_fix[which(indi_acc_roll_fix$Year==2017),],
                              indi_acc_baseline[which(indi_acc_baseline$Year==2017),]) %>%
  arrange(Country, Week_ahead)

accMetric_table_2017 <- accMetric_table_2017 %>%
  tidyr::pivot_wider(names_from = Model, values_from = Accuracy:macroMAE)

for (i in 1:nrow(accMetric_table_2017)){
  accMetric_table_2017$Country_fullName[i] <- countryISO$Country[which(accMetric_table_2017$Country[i] == countryISO$ISO3)]
}

write.csv(accMetric_table_2017, "./saved_objects/accMetric_table_2017.csv")  
