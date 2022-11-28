#' Forecasting analysis on WHO data

#' Make a note of when the report was generated.
Sys.time()
knitr.table.format = "markdown"

#' As always, remove all objects fromt the workspace before starting.
rm(list = ls(all = TRUE))

setwd("C:/Users/hw3616/Desktop/Imperial/Project1_Forecasting/Project_Coding/iiag/forecasting_vivi")
setwd("C:/Users/haowe/Desktop/iiag/forecasting_vivi")

#' Pull in packages needed
source("./functions/load_packages.R")

pkgs <- c("xgboost", "stringr", "rasterVis", "cdcfluview", "hrbrthemes",
          "dplyr", "ggplot2", "cdcfluview", "Metrics", "rminer", "tidyr")
load_package(pkgs)

#' Load self-written functions 
source("./vivi_funcs.R")
source("./gbm_complex_funcs.R")

#' Load data
fluWHO <- readRDS("./saved_objects/fluWHO.rds")

fluWHO_incidence <- fluWHO[[1]]
sel_iso_xgb <- fluWHO[[2]]

######### tuning parameter #######
# best_param = list()
# best_seednumber = 1234
# best_logloss = Inf
# best_logloss_index = 0

xgb_params <- readRDS("./saved_objects/hyperpara_list_final.rds")

########## 1-week ahead forecast #######
acc_1week_pred15_rol <- compare_accuracy_rolling(flu_data = fluWHO_incidence, 
                                                 country = sel_iso_xgb, 
                                                 num_category = 10, 
                                                 train_num_start = 0, 
                                                 train_num_end = 4,
                                                 nWeek_ahead = 1, 
                                                 yr53week = 2015,
                                                 nrounds = 300,
                                                 params_list = xgb_params)

acc_1week_pred15_fix <- compare_accuracy(flu_data = fluWHO_incidence, 
                                         country = sel_iso_xgb, 
                                         num_category = 10, 
                                         train_num_start = 0, 
                                         train_num_end = 4 ,
                                         nWeek_ahead = 1, 
                                         yr53week = 2015,
                                         nrounds = 300,
                                         params_list = xgb_params)

acc_1week_pred16_rol <- compare_accuracy_rolling(flu_data = fluWHO_incidence, 
                                                 country = sel_iso_xgb, 
                                                 num_category = 10, 
                                                 train_num_start = 1, 
                                                 train_num_end = 4,
                                                 nWeek_ahead = 1, 
                                                 yr53week = 2015,
                                                 nrounds = 300,
                                                 params_list = xgb_params)


acc_1week_pred16_fix <- compare_accuracy(flu_data = fluWHO_incidence, 
                                         country = sel_iso_xgb, 
                                         num_category = 10, 
                                         train_num_start = 1, 
                                         train_num_end = 4 ,
                                         nWeek_ahead = 1, 
                                         yr53week = 2015,
                                         nrounds = 300,
                                         params_list = xgb_params)


acc_1week_pred17_rol <- compare_accuracy_rolling(flu_data = fluWHO_incidence, 
                                                 country = sel_iso_xgb, 
                                                 num_category = 10, 
                                                 train_num_start = 2, 
                                                 train_num_end = 4,
                                                 nWeek_ahead = 1, 
                                                 yr53week = 2015,
                                                 nrounds = 300,
                                                 params_list = xgb_params)


acc_1week_pred17_fix <- compare_accuracy(flu_data = fluWHO_incidence, 
                                         country = sel_iso_xgb, 
                                         num_category = 10, 
                                         train_num_start = 2, 
                                         train_num_end = 4 ,
                                         nWeek_ahead = 1, 
                                         yr53week = 2015,
                                         nrounds = 300,
                                         params_list = xgb_params)


########## 2-week ahead forecast #######
acc_2week_pred15_rol <- compare_accuracy_rolling(flu_data = fluWHO_incidence, 
                                                 country = sel_iso_xgb, 
                                                 num_category = 10, 
                                                 train_num_start = 0, 
                                                 train_num_end = 4 ,
                                                 nWeek_ahead = 2, 
                                                 yr53week = 2015,
                                                 nrounds = 300,
                                                 params_list = xgb_params)


acc_2week_pred15_fix <- compare_accuracy(flu_data = fluWHO_incidence, 
                                         country = sel_iso_xgb, 
                                         num_category = 10, 
                                         train_num_start = 0, 
                                         train_num_end = 4 ,
                                         nWeek_ahead = 2, 
                                         yr53week = 2015,
                                         nrounds = 300,
                                         params_list = xgb_params)

acc_2week_pred16_rol <- compare_accuracy_rolling(flu_data = fluWHO_incidence, 
                                                 country = sel_iso_xgb, 
                                                 num_category = 10, 
                                                 train_num_start = 1, 
                                                 train_num_end = 4,
                                                 nWeek_ahead = 2, 
                                                 yr53week = 2015,
                                                 nrounds = 300,
                                                 params_list = xgb_params)


acc_2week_pred16_fix <- compare_accuracy(flu_data = fluWHO_incidence, 
                                         country = sel_iso_xgb, 
                                         num_category = 10, 
                                         train_num_start = 1, 
                                         train_num_end = 4 ,
                                         nWeek_ahead = 2, 
                                         yr53week = 2015,
                                         nrounds = 300,
                                         params_list = xgb_params)

acc_2week_pred17_rol <- compare_accuracy_rolling(flu_data = fluWHO_incidence, 
                                                 country = sel_iso_xgb, 
                                                 num_category = 10, 
                                                 train_num_start = 2, 
                                                 train_num_end = 4,
                                                 nWeek_ahead = 2, 
                                                 yr53week = 2015,
                                                 nrounds = 300,
                                                 params_list = xgb_params)


acc_2week_pred17_fix <- compare_accuracy(flu_data = fluWHO_incidence, 
                                         country = sel_iso_xgb, 
                                         num_category = 10, 
                                         train_num_start = 2, 
                                         train_num_end = 4 ,
                                         nWeek_ahead = 2, 
                                         yr53week = 2015,
                                         nrounds = 300,
                                         params_list = xgb_params)

########## 3-week ahead forecast #######
acc_3week_pred15_rol <- compare_accuracy_rolling(flu_data = fluWHO_incidence, 
                                                 country = sel_iso_xgb, 
                                                 num_category = 10, 
                                                 train_num_start = 0, 
                                                 train_num_end = 4 ,
                                                 nWeek_ahead = 3, 
                                                 yr53week = 2015,
                                                 nrounds = 300,
                                                 params_list = xgb_params)


acc_3week_pred15_fix <- compare_accuracy(flu_data = fluWHO_incidence, 
                                         country = sel_iso_xgb, 
                                         num_category = 10, 
                                         train_num_start = 0, 
                                         train_num_end = 4 ,
                                         nWeek_ahead = 3, 
                                         yr53week = 2015,
                                         nrounds = 300,
                                         params_list = xgb_params)

acc_3week_pred16_rol <- compare_accuracy_rolling(flu_data = fluWHO_incidence, 
                                                 country = sel_iso_xgb, 
                                                 num_category = 10, 
                                                 train_num_start = 1, 
                                                 train_num_end = 4,
                                                 nWeek_ahead = 3, 
                                                 yr53week = 2015,
                                                 nrounds = 300,
                                                 params_list = xgb_params)

acc_3week_pred16_fix <- compare_accuracy(flu_data = fluWHO_incidence, 
                                         country = sel_iso_xgb, 
                                         num_category = 10, 
                                         train_num_start = 1, 
                                         train_num_end = 4 ,
                                         nWeek_ahead = 3, 
                                         yr53week = 2015,
                                         nrounds = 300,
                                         params_list = xgb_params)

acc_3week_pred17_rol <- compare_accuracy_rolling(flu_data = fluWHO_incidence, 
                                                 country = sel_iso_xgb, 
                                                 num_category = 10, 
                                                 train_num_start = 2, 
                                                 train_num_end = 4,
                                                 nWeek_ahead = 3, 
                                                 yr53week = 2015,
                                                 nrounds = 300,
                                                 params_list = xgb_params)

acc_3week_pred17_fix <- compare_accuracy(flu_data = fluWHO_incidence, 
                                         country = sel_iso_xgb, 
                                         num_category = 10, 
                                         train_num_start = 2, 
                                         train_num_end = 4 ,
                                         nWeek_ahead = 3, 
                                         yr53week = 2015,
                                         nrounds = 300,
                                         params_list = xgb_params)

########## 4-week ahead forecast #######
acc_4week_pred15_rol <- compare_accuracy_rolling(flu_data = fluWHO_incidence, 
                                                 country = sel_iso_xgb, 
                                                 num_category = 10, 
                                                 train_num_start = 0, 
                                                 train_num_end = 4,
                                                 nWeek_ahead = 4, 
                                                 yr53week = 2015,
                                                 nrounds = 300,
                                                 params_list = xgb_params)


acc_4week_pred15_fix <- compare_accuracy(flu_data = fluWHO_incidence, 
                                         country = sel_iso_xgb, 
                                         num_category = 10, 
                                         train_num_start = 0, 
                                         train_num_end = 4,
                                         nWeek_ahead = 4, 
                                         yr53week = 2015,
                                         nrounds = 300,
                                         params_list = xgb_params)

acc_4week_pred16_rol <- compare_accuracy_rolling(flu_data = fluWHO_incidence, 
                                                 country = sel_iso_xgb, 
                                                 num_category = 10, 
                                                 train_num_start = 1, 
                                                 train_num_end = 4,
                                                 nWeek_ahead = 4, 
                                                 yr53week = 2015,
                                                 nrounds = 300,
                                                 params_list = xgb_params)

acc_4week_pred16_fix <- compare_accuracy(flu_data = fluWHO_incidence, 
                                         country = sel_iso_xgb, 
                                         num_category = 10, 
                                         train_num_start = 1, 
                                         train_num_end = 4 ,
                                         nWeek_ahead = 4, 
                                         yr53week = 2015,
                                         nrounds = 300,
                                         params_list = xgb_params)

acc_4week_pred17_rol <- compare_accuracy_rolling(flu_data = fluWHO_incidence, 
                                                 country = sel_iso_xgb, 
                                                 num_category = 10, 
                                                 train_num_start = 2, 
                                                 train_num_end = 4,
                                                 nWeek_ahead = 4, 
                                                 yr53week = 2015,
                                                 nrounds = 300,
                                                 params_list = xgb_params)

acc_4week_pred17_fix <- compare_accuracy(flu_data = fluWHO_incidence, 
                                         country = sel_iso_xgb, 
                                         num_category = 10, 
                                         train_num_start = 2, 
                                         train_num_end = 4 ,
                                         nWeek_ahead = 4, 
                                         yr53week = 2015,
                                         nrounds = 300,
                                         params_list = xgb_params)

acc_WHO_roll_fix <- list(acc_1week_pred15_rol = acc_1week_pred15_rol, acc_1week_pred16_rol = acc_1week_pred16_rol, 
                     acc_1week_pred17_rol = acc_1week_pred17_rol,
                     acc_2week_pred15_rol = acc_2week_pred15_rol, acc_2week_pred16_rol = acc_2week_pred16_rol,
                     acc_2week_pred17_rol = acc_2week_pred17_rol,
                     acc_3week_pred15_rol = acc_3week_pred15_rol, acc_3week_pred16_rol = acc_3week_pred16_rol,
                     acc_3week_pred17_rol = acc_3week_pred17_rol,
                     acc_4week_pred15_rol = acc_4week_pred15_rol, acc_4week_pred16_rol = acc_4week_pred16_rol,
                     acc_4week_pred17_rol = acc_4week_pred17_rol,
                     acc_1week_pred15_fix = acc_1week_pred15_fix, acc_1week_pred16_fix = acc_1week_pred16_fix, 
                     acc_1week_pred17_fix = acc_1week_pred17_fix,
                     acc_2week_pred15_fix = acc_2week_pred15_fix, acc_2week_pred16_fix = acc_2week_pred16_fix,
                     acc_2week_pred17_fix = acc_2week_pred17_fix,
                     acc_3week_pred15_fix = acc_3week_pred15_fix, acc_3week_pred16_fix = acc_3week_pred16_fix,
                     acc_3week_pred17_fix = acc_3week_pred17_fix,
                     acc_4week_pred15_fix = acc_4week_pred15_fix, acc_4week_pred16_fix = acc_4week_pred16_fix,
                     acc_4week_pred17_fix = acc_4week_pred17_fix)

saveRDS(acc_WHO_roll_fix, "./saved_objects/acc_WHO_roll_fix_new.rds")

########### cross validation ###########
#' 2010-2014 is used for cross validation. 2010-2011 train, 2012 test; 2011-2012 train, 2013 test; 
#' 2012-2013 train, 2014 test
#' only tune parameter on fix 1-week ahead forecast considering the speed of running

# smallestMZE <- 100
# smallestMAE <- 100
# df_cv <- data.frame()
# bts_list <- list()
# 
# for (depth in seq(1,6,1)){
#   for (rounds in seq(100, 500, 100)){
#     err_MZE <- c()
#     err_MAE <- c()
#     
#     indexCount <- 1
#     
#     xgb_params <- list(booster = "gbtree", objective = "multi:softprob", gamma=0, num_class = 10,
#                        subsample=1, colsample_bytree=1,eval_metric = "mlogloss", max_depth = depth)
#     
#     for (train_num_start in seq(0, 2, 1)){
#       bts <- compare_accuracy(flu_data = fluWHO_incidence, 
#                               country = sel_iso_xgb, 
#                               num_category = 10, 
#                               train_num_start = train_num_start, 
#                               train_num_end = 1,
#                               nWeek_ahead = 1, 
#                               yr53week = 2015,
#                               nrounds = rounds,
#                               params_list = xgb_params)
#       bts_list <- append(bts_list, bts)
#       gc()
#       
#       err_MZE <- c(err_MZE, bts$MZE)
#       err_MAE <- c(err_MAE, bts$macroMAE)
#       
#     }
#     df_cv <- rbind(df_cv, c(depth,rounds, mean(err_MZE), mean(err_MAE)))
#     
#   }
# }

#' according to results of CV, nrounds = 100 and depth = 6 give minimum macro MAE, at 1.417120, MZE = 0.2310000
#' minimum MZE is given by nroudns = 200 and depth = 1, MZE = 0.2310000 and MAE = 1.480200

########## individual countries #########
acc_WHO_roll_fix <- readRDS("./saved_objects/acc_WHO_roll_fix_new.rds")

indi_country_acc <- function(overall_pred, country, num_category){
  require(dplyr)
  indi_pred <-  overall_pred %>% 
    filter(Country == country)
  accuracy <- round(length(which(indi_pred$Accuracy == 1))/nrow(indi_pred),3)
  MZE <- 1- accuracy
  
  # macro_averaged MAE
  macroMAE_vec <- c()
  for (i in 1:num_category){
    if (i %in% indi_pred$Observation){
      mat <- indi_pred[which(indi_pred$Observation == i),]
      macroMAE_tmp <- mae(mat$Observation, mat$Prediction)
    }else{
      macroMAE_tmp <- 0 
    }
    macroMAE_vec <- c(macroMAE_vec, macroMAE_tmp)
  }
  macroMAE <- sum(macroMAE_vec)/num_category
  
  indi_acc <- c(country, accuracy, MZE, macroMAE)
  
  return(indi_acc)
}

###### rolling forecast accuracy #######
# 1-week ahead
indi_acc_1week_pred15_rol <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi <- indi_country_acc(acc_WHO_roll_fix$acc_1week_pred15_rol$pred, sel_iso_xgb[i], 10)
  indi_acc_1week_pred15_rol <- rbind(indi_acc_1week_pred15_rol, temp_indi)
  
}
indi_acc_1week_pred15_rol <- indi_acc_1week_pred15_rol %>% 
  as.data.frame() %>% 
  rename(Country = V1,
         Accuracy = V2,
         MZE = V3,
         macroMAE = V4) %>% 
  mutate(Week_ahead = 1,
         Year = 2015,
         Model = "rolling",
         Accuracy = as.numeric(Accuracy),
         MZE = as.numeric(MZE),
         macroMAE = as.numeric(macroMAE)) %>% 
  relocate(Year, .before = 2) %>% 
  relocate(Week_ahead, .before = 3) %>% 
  relocate(Model, .before = 4)

indi_acc_1week_pred16_rol <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi <- indi_country_acc(acc_WHO_roll_fix$acc_1week_pred16_rol$pred, sel_iso_xgb[i], 10)
  indi_acc_1week_pred16_rol <- rbind(indi_acc_1week_pred16_rol, temp_indi)
  
}
indi_acc_1week_pred16_rol <- indi_acc_1week_pred16_rol %>% 
  as.data.frame() %>% 
  rename(Country = V1,
         Accuracy = V2,
         MZE = V3,
         macroMAE = V4) %>% 
  mutate(Week_ahead = 1,
         Year = 2016,
         Model = "rolling",
         Accuracy = as.numeric(Accuracy),
         MZE = as.numeric(MZE),
         macroMAE = as.numeric(macroMAE)) %>% 
  relocate(Year, .before = 2) %>% 
  relocate(Week_ahead, .before = 3) %>% 
  relocate(Model, .before = 4)

indi_acc_1week_pred17_rol <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi <- indi_country_acc(acc_WHO_roll_fix$acc_1week_pred17_rol$pred, sel_iso_xgb[i], 10)
  indi_acc_1week_pred17_rol <- rbind(indi_acc_1week_pred17_rol, temp_indi)
  
}
indi_acc_1week_pred17_rol <- indi_acc_1week_pred17_rol %>% 
  as.data.frame() %>% 
  rename(Country = V1,
         Accuracy = V2,
         MZE = V3,
         macroMAE = V4) %>% 
  mutate(Week_ahead = 1,
         Year = 2017,
         Model = "rolling",
         Accuracy = as.numeric(Accuracy),
         MZE = as.numeric(MZE),
         macroMAE = as.numeric(macroMAE)) %>% 
  relocate(Year, .before = 2) %>% 
  relocate(Week_ahead, .before = 3) %>% 
  relocate(Model, .before = 4)

# 2-week ahead
indi_acc_2week_pred15_rol <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi <- indi_country_acc(acc_WHO_roll_fix$acc_2week_pred15_rol$pred, sel_iso_xgb[i], 10)
  indi_acc_2week_pred15_rol <- rbind(indi_acc_2week_pred15_rol, temp_indi)
  
}
indi_acc_2week_pred15_rol <- indi_acc_2week_pred15_rol %>% 
  as.data.frame() %>% 
  rename(Country = V1,
         Accuracy = V2,
         MZE = V3,
         macroMAE = V4) %>% 
  mutate(Week_ahead = 2,
         Year = 2015,
         Model = "rolling",
         Accuracy = as.numeric(Accuracy),
         MZE = as.numeric(MZE),
         macroMAE = as.numeric(macroMAE)) %>% 
  relocate(Year, .before = 2) %>% 
  relocate(Week_ahead, .before = 3) %>% 
  relocate(Model, .before = 4)

indi_acc_2week_pred16_rol <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi <- indi_country_acc(acc_WHO_roll_fix$acc_2week_pred16_rol$pred, sel_iso_xgb[i], 10)
  indi_acc_2week_pred16_rol <- rbind(indi_acc_2week_pred16_rol, temp_indi)
  
}
indi_acc_2week_pred16_rol <- indi_acc_2week_pred16_rol %>% 
  as.data.frame() %>% 
  rename(Country = V1,
         Accuracy = V2,
         MZE = V3,
         macroMAE = V4) %>% 
  mutate(Week_ahead = 2,
         Year = 2016,
         Model = "rolling",
         Accuracy = as.numeric(Accuracy),
         MZE = as.numeric(MZE),
         macroMAE = as.numeric(macroMAE)) %>% 
  relocate(Year, .before = 2) %>% 
  relocate(Week_ahead, .before = 3) %>% 
  relocate(Model, .before = 4)

indi_acc_2week_pred17_rol <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi <- indi_country_acc(acc_WHO_roll_fix$acc_2week_pred17_rol$pred, sel_iso_xgb[i], 10)
  indi_acc_2week_pred17_rol <- rbind(indi_acc_2week_pred17_rol, temp_indi)
  
}
indi_acc_2week_pred17_rol <- indi_acc_2week_pred17_rol %>% 
  as.data.frame() %>% 
  rename(Country = V1,
         Accuracy = V2,
         MZE = V3,
         macroMAE = V4) %>% 
  mutate(Week_ahead = 2,
         Year = 2017,
         Model = "rolling",
         Accuracy = as.numeric(Accuracy),
         MZE = as.numeric(MZE),
         macroMAE = as.numeric(macroMAE)) %>% 
  relocate(Year, .before = 2) %>% 
  relocate(Week_ahead, .before = 3) %>% 
  relocate(Model, .before = 4)

# 3-week ahead
indi_acc_3week_pred15_rol <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi <- indi_country_acc(acc_WHO_roll_fix$acc_3week_pred15_rol$pred, sel_iso_xgb[i], 10)
  indi_acc_3week_pred15_rol <- rbind(indi_acc_3week_pred15_rol, temp_indi)
  
}
indi_acc_3week_pred15_rol <- indi_acc_3week_pred15_rol %>% 
  as.data.frame() %>% 
  rename(Country = V1,
         Accuracy = V2,
         MZE = V3,
         macroMAE = V4) %>% 
  mutate(Week_ahead = 3,
         Year = 2015,
         Model = "rolling",
         Accuracy = as.numeric(Accuracy),
         MZE = as.numeric(MZE),
         macroMAE = as.numeric(macroMAE)) %>% 
  relocate(Year, .before = 2) %>% 
  relocate(Week_ahead, .before = 3) %>% 
  relocate(Model, .before = 4)

indi_acc_3week_pred16_rol <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi <- indi_country_acc(acc_WHO_roll_fix$acc_3week_pred16_rol$pred, sel_iso_xgb[i], 10)
  indi_acc_3week_pred16_rol <- rbind(indi_acc_3week_pred16_rol, temp_indi)
  
}
indi_acc_3week_pred16_rol <- indi_acc_3week_pred16_rol %>% 
  as.data.frame() %>% 
  rename(Country = V1,
         Accuracy = V2,
         MZE = V3,
         macroMAE = V4) %>% 
  mutate(Week_ahead = 3,
         Year = 2016,
         Model = "rolling",
         Accuracy = as.numeric(Accuracy),
         MZE = as.numeric(MZE),
         macroMAE = as.numeric(macroMAE)) %>% 
  relocate(Year, .before = 2) %>% 
  relocate(Week_ahead, .before = 3) %>% 
  relocate(Model, .before = 4)

indi_acc_3week_pred17_rol <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi <- indi_country_acc(acc_WHO_roll_fix$acc_3week_pred17_rol$pred, sel_iso_xgb[i], 10)
  indi_acc_3week_pred17_rol <- rbind(indi_acc_3week_pred17_rol, temp_indi)
  
}
indi_acc_3week_pred17_rol <- indi_acc_3week_pred17_rol %>% 
  as.data.frame() %>% 
  rename(Country = V1,
         Accuracy = V2,
         MZE = V3,
         macroMAE = V4) %>% 
  mutate(Week_ahead = 3,
         Year = 2017,
         Model = "rolling",
         Accuracy = as.numeric(Accuracy),
         MZE = as.numeric(MZE),
         macroMAE = as.numeric(macroMAE)) %>% 
  relocate(Year, .before = 2) %>% 
  relocate(Week_ahead, .before = 3) %>% 
  relocate(Model, .before = 4)

# 4-week ahead 
indi_acc_4week_pred15_rol <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi <- indi_country_acc(acc_WHO_roll_fix$acc_4week_pred15_rol$pred, sel_iso_xgb[i], 10)
  indi_acc_4week_pred15_rol <- rbind(indi_acc_4week_pred15_rol, temp_indi)
  
}
indi_acc_4week_pred15_rol <- indi_acc_4week_pred15_rol %>% 
  as.data.frame() %>% 
  rename(Country = V1,
         Accuracy = V2,
         MZE = V3,
         macroMAE = V4) %>% 
  mutate(Week_ahead = 4,
         Year = 2015,
         Model = "rolling",
         Accuracy = as.numeric(Accuracy),
         MZE = as.numeric(MZE),
         macroMAE = as.numeric(macroMAE)) %>% 
  relocate(Year, .before = 2) %>% 
  relocate(Week_ahead, .before = 3) %>% 
  relocate(Model, .before = 4)

indi_acc_4week_pred16_rol <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi <- indi_country_acc(acc_WHO_roll_fix$acc_4week_pred16_rol$pred, sel_iso_xgb[i], 10)
  indi_acc_4week_pred16_rol <- rbind(indi_acc_4week_pred16_rol, temp_indi)
  
}
indi_acc_4week_pred16_rol <- indi_acc_4week_pred16_rol %>% 
  as.data.frame() %>% 
  rename(Country = V1,
         Accuracy = V2,
         MZE = V3,
         macroMAE = V4) %>% 
  mutate(Week_ahead = 4,
         Year = 2016,
         Model = "rolling",
         Accuracy = as.numeric(Accuracy),
         MZE = as.numeric(MZE),
         macroMAE = as.numeric(macroMAE)) %>% 
  relocate(Year, .before = 2) %>% 
  relocate(Week_ahead, .before = 3) %>% 
  relocate(Model, .before = 4)

indi_acc_4week_pred17_rol <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi <- indi_country_acc(acc_WHO_roll_fix$acc_4week_pred17_rol$pred, sel_iso_xgb[i], 10)
  indi_acc_4week_pred17_rol <- rbind(indi_acc_4week_pred17_rol, temp_indi)
  
}
indi_acc_4week_pred17_rol <- indi_acc_4week_pred17_rol %>% 
  as.data.frame() %>% 
  rename(Country = V1,
         Accuracy = V2,
         MZE = V3,
         macroMAE = V4) %>% 
  mutate(Week_ahead = 4,
         Year = 2017,
         Model = "rolling",
         Accuracy = as.numeric(Accuracy),
         MZE = as.numeric(MZE),
         macroMAE = as.numeric(macroMAE)) %>% 
  relocate(Year, .before = 2) %>% 
  relocate(Week_ahead, .before = 3) %>% 
  relocate(Model, .before = 4)

########### fix forecast ##########
# 1-week ahead
indi_acc_1week_pred15_fix <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi <- indi_country_acc(acc_WHO_roll_fix$acc_1week_pred15_fix$pred, sel_iso_xgb[i], 10)
  indi_acc_1week_pred15_fix <- rbind(indi_acc_1week_pred15_fix, temp_indi)
}
indi_acc_1week_pred15_fix <- indi_acc_1week_pred15_fix %>% 
  as.data.frame() %>% 
  rename(Country = V1,
         Accuracy = V2,
         MZE = V3,
         macroMAE = V4) %>% 
  mutate(Week_ahead = 1,
         Year = 2015,
         Model = "fix",
         Accuracy = as.numeric(Accuracy),
         MZE = as.numeric(MZE),
         macroMAE = as.numeric(macroMAE)) %>% 
  relocate(Year, .before = 2) %>% 
  relocate(Week_ahead, .before = 3) %>% 
  relocate(Model, .before = 4)

indi_acc_1week_pred16_fix <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi <- indi_country_acc(acc_WHO_roll_fix$acc_1week_pred16_fix$pred, sel_iso_xgb[i], 10)
  indi_acc_1week_pred16_fix <- rbind(indi_acc_1week_pred16_fix, temp_indi)
}

indi_acc_1week_pred16_fix <- indi_acc_1week_pred16_fix %>% 
  as.data.frame() %>% 
  rename(Country = V1,
         Accuracy = V2,
         MZE = V3,
         macroMAE = V4) %>% 
  mutate(Week_ahead = 1,
         Year = 2016,
         Model = "fix",
         Accuracy = as.numeric(Accuracy),
         MZE = as.numeric(MZE),
         macroMAE = as.numeric(macroMAE)) %>% 
  relocate(Year, .before = 2) %>% 
  relocate(Week_ahead, .before = 3) %>% 
  relocate(Model, .before = 4)

indi_acc_1week_pred17_fix <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi <- indi_country_acc(acc_WHO_roll_fix$acc_1week_pred17_fix$pred, sel_iso_xgb[i], 10)
  indi_acc_1week_pred17_fix <- rbind(indi_acc_1week_pred17_fix, temp_indi)
  
}
indi_acc_1week_pred17_fix <- indi_acc_1week_pred17_fix %>% 
  as.data.frame() %>% 
  rename(Country = V1,
         Accuracy = V2,
         MZE = V3,
         macroMAE = V4) %>% 
  mutate(Week_ahead = 1,
         Year = 2017,
         Model = "fix",
         Accuracy = as.numeric(Accuracy),
         MZE = as.numeric(MZE),
         macroMAE = as.numeric(macroMAE)) %>% 
  relocate(Year, .before = 2) %>% 
  relocate(Week_ahead, .before = 3) %>% 
  relocate(Model, .before = 4)

# 2-week ahead
indi_acc_2week_pred15_fix <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi <- indi_country_acc(acc_WHO_roll_fix$acc_2week_pred15_fix$pred, sel_iso_xgb[i], 10)
  indi_acc_2week_pred15_fix <- rbind(indi_acc_2week_pred15_fix, temp_indi)
  
}
indi_acc_2week_pred15_fix <- indi_acc_2week_pred15_fix %>% 
  as.data.frame() %>% 
  rename(Country = V1,
         Accuracy = V2,
         MZE = V3,
         macroMAE = V4) %>% 
  mutate(Week_ahead = 2,
         Year = 2015,
         Model = "fix",
         Accuracy = as.numeric(Accuracy),
         MZE = as.numeric(MZE),
         macroMAE = as.numeric(macroMAE)) %>% 
  relocate(Year, .before = 2) %>% 
  relocate(Week_ahead, .before = 3) %>% 
  relocate(Model, .before = 4)

indi_acc_2week_pred16_fix <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi <- indi_country_acc(acc_WHO_roll_fix$acc_2week_pred16_fix$pred, sel_iso_xgb[i], 10)
  indi_acc_2week_pred16_fix <- rbind(indi_acc_2week_pred16_fix, temp_indi)
  
}
indi_acc_2week_pred16_fix <- indi_acc_2week_pred16_fix %>% 
  as.data.frame() %>% 
  rename(Country = V1,
         Accuracy = V2,
         MZE = V3,
         macroMAE = V4) %>% 
  mutate(Week_ahead = 2,
         Year = 2016,
         Model = "fix",
         Accuracy = as.numeric(Accuracy),
         MZE = as.numeric(MZE),
         macroMAE = as.numeric(macroMAE)) %>% 
  relocate(Year, .before = 2) %>% 
  relocate(Week_ahead, .before = 3) %>% 
  relocate(Model, .before = 4)

indi_acc_2week_pred17_fix <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi <- indi_country_acc(acc_WHO_roll_fix$acc_2week_pred17_fix$pred, sel_iso_xgb[i], 10)
  indi_acc_2week_pred17_fix <- rbind(indi_acc_2week_pred17_fix, temp_indi)
  
}
indi_acc_2week_pred17_fix <- indi_acc_2week_pred17_fix %>% 
  as.data.frame() %>% 
  rename(Country = V1,
         Accuracy = V2,
         MZE = V3,
         macroMAE = V4) %>% 
  mutate(Week_ahead = 2,
         Year = 2017,
         Model = "fix",
         Accuracy = as.numeric(Accuracy),
         MZE = as.numeric(MZE),
         macroMAE = as.numeric(macroMAE)) %>% 
  relocate(Year, .before = 2) %>% 
  relocate(Week_ahead, .before = 3) %>% 
  relocate(Model, .before = 4)

# 3-week ahead
indi_acc_3week_pred15_fix <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi <- indi_country_acc(acc_WHO_roll_fix$acc_3week_pred15_fix$pred, sel_iso_xgb[i], 10)
  indi_acc_3week_pred15_fix <- rbind(indi_acc_3week_pred15_fix, temp_indi)
  
}
indi_acc_3week_pred15_fix <- indi_acc_3week_pred15_fix %>% 
  as.data.frame() %>% 
  rename(Country = V1,
         Accuracy = V2,
         MZE = V3,
         macroMAE = V4) %>% 
  mutate(Week_ahead = 3,
         Year = 2015,
         Model = "fix",
         Accuracy = as.numeric(Accuracy),
         MZE = as.numeric(MZE),
         macroMAE = as.numeric(macroMAE)) %>% 
  relocate(Year, .before = 2) %>% 
  relocate(Week_ahead, .before = 3) %>% 
  relocate(Model, .before = 4)

indi_acc_3week_pred16_fix <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi <- indi_country_acc(acc_WHO_roll_fix$acc_3week_pred16_fix$pred, sel_iso_xgb[i], 10)
  indi_acc_3week_pred16_fix <- rbind(indi_acc_3week_pred16_fix, temp_indi)
  
}
indi_acc_3week_pred16_fix <- indi_acc_3week_pred16_fix %>% 
  as.data.frame() %>% 
  rename(Country = V1,
         Accuracy = V2,
         MZE = V3,
         macroMAE = V4) %>% 
  mutate(Week_ahead = 3,
         Year = 2016,
         Model = "fix",
         Accuracy = as.numeric(Accuracy),
         MZE = as.numeric(MZE),
         macroMAE = as.numeric(macroMAE)) %>% 
  relocate(Year, .before = 2) %>% 
  relocate(Week_ahead, .before = 3) %>% 
  relocate(Model, .before = 4)

indi_acc_3week_pred17_fix <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi <- indi_country_acc(acc_WHO_roll_fix$acc_3week_pred17_fix$pred, sel_iso_xgb[i], 10)
  indi_acc_3week_pred17_fix <- rbind(indi_acc_3week_pred17_fix, temp_indi)
  
}
indi_acc_3week_pred17_fix <- indi_acc_3week_pred17_fix %>% 
  as.data.frame() %>% 
  rename(Country = V1,
         Accuracy = V2,
         MZE = V3,
         macroMAE = V4) %>% 
  mutate(Week_ahead = 3,
         Year = 2017,
         Model = "fix",
         Accuracy = as.numeric(Accuracy),
         MZE = as.numeric(MZE),
         macroMAE = as.numeric(macroMAE)) %>% 
  relocate(Year, .before = 2) %>% 
  relocate(Week_ahead, .before = 3) %>% 
  relocate(Model, .before = 4)

# 4-week ahead
indi_acc_4week_pred15_fix <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi <- indi_country_acc(acc_WHO_roll_fix$acc_4week_pred15_fix$pred, sel_iso_xgb[i], 10)
  indi_acc_4week_pred15_fix <- rbind(indi_acc_4week_pred15_fix, temp_indi)
  
}
indi_acc_4week_pred15_fix <- indi_acc_4week_pred15_fix %>% 
  as.data.frame() %>% 
  rename(Country = V1,
         Accuracy = V2,
         MZE = V3,
         macroMAE = V4) %>% 
  mutate(Week_ahead = 4,
         Year = 2015,
         Model = "fix",
         Accuracy = as.numeric(Accuracy),
         MZE = as.numeric(MZE),
         macroMAE = as.numeric(macroMAE)) %>% 
  relocate(Year, .before = 2) %>% 
  relocate(Week_ahead, .before = 3) %>% 
  relocate(Model, .before = 4)

indi_acc_4week_pred16_fix <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi <- indi_country_acc(acc_WHO_roll_fix$acc_4week_pred16_fix$pred, sel_iso_xgb[i], 10)
  indi_acc_4week_pred16_fix <- rbind(indi_acc_4week_pred16_fix, temp_indi)
  
}
indi_acc_4week_pred16_fix <- indi_acc_4week_pred16_fix %>% 
  as.data.frame() %>% 
  rename(Country = V1,
         Accuracy = V2,
         MZE = V3,
         macroMAE = V4) %>% 
  mutate(Week_ahead = 4,
         Year = 2016,
         Model = "fix",
         Accuracy = as.numeric(Accuracy),
         MZE = as.numeric(MZE),
         macroMAE = as.numeric(macroMAE)) %>% 
  relocate(Year, .before = 2) %>% 
  relocate(Week_ahead, .before = 3) %>% 
  relocate(Model, .before = 4)

indi_acc_4week_pred17_fix <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi <- indi_country_acc(acc_WHO_roll_fix$acc_4week_pred17_fix$pred, sel_iso_xgb[i], 10)
  indi_acc_4week_pred17_fix <- rbind(indi_acc_4week_pred17_fix, temp_indi)
  
}
indi_acc_4week_pred17_fix <- indi_acc_4week_pred17_fix %>% 
  as.data.frame() %>% 
  rename(Country = V1,
         Accuracy = V2,
         MZE = V3,
         macroMAE = V4) %>% 
  mutate(Week_ahead = 4,
         Year = 2017,
         Model = "fix",
         Accuracy = as.numeric(Accuracy),
         MZE = as.numeric(MZE),
         macroMAE = as.numeric(macroMAE)) %>% 
  relocate(Year, .before = 2) %>% 
  relocate(Week_ahead, .before = 3) %>% 
  relocate(Model, .before = 4)

indi_acc_roll_fix <- rbind(indi_acc_1week_pred15_rol, indi_acc_1week_pred16_rol, indi_acc_1week_pred17_rol,
                           indi_acc_2week_pred15_rol, indi_acc_2week_pred16_rol, indi_acc_2week_pred17_rol,
                           indi_acc_3week_pred15_rol, indi_acc_3week_pred16_rol, indi_acc_3week_pred17_rol,
                           indi_acc_4week_pred15_rol, indi_acc_4week_pred16_rol, indi_acc_4week_pred17_rol,
                           indi_acc_1week_pred15_fix, indi_acc_1week_pred16_fix, indi_acc_1week_pred17_fix,
                           indi_acc_2week_pred15_fix, indi_acc_2week_pred16_fix, indi_acc_2week_pred17_fix,
                           indi_acc_3week_pred15_fix, indi_acc_3week_pred16_fix, indi_acc_3week_pred17_fix,
                           indi_acc_4week_pred15_fix, indi_acc_4week_pred16_fix, indi_acc_4week_pred17_fix)

write.csv(indi_acc_roll_fix, "./saved_objects/indi_acc_roll_fix_new.csv", row.names = FALSE)

indi_acc_roll_fix_by_countryWeek <- indi_acc_roll_fix %>% 
  arrange(Country, Week_ahead)

write.csv(indi_acc_roll_fix_by_countryWeek, "./saved_objects/indi_acc_roll_fix_by_countryWeek_new.csv", 
          row.names = FALSE)

############ baseline model ############
acc_1week_pred15_hist <- compare_accuracy_hist(flu_data = fluWHO_incidence,
                                           country = sel_iso_xgb,
                                           num_category = 10,
                                           train_num_start = 0, 
                                           train_num_end = 4,
                                           numWeek_ahead = 1, 
                                           yr53week = 2015)

acc_1week_pred16_hist <- compare_accuracy_hist(flu_data = fluWHO_incidence,
                                               country = sel_iso_xgb,
                                               num_category = 10,
                                               train_num_start = 1, 
                                               train_num_end = 4,
                                               numWeek_ahead = 1, 
                                               yr53week = 2015)

acc_1week_pred17_hist <- compare_accuracy_hist(flu_data = fluWHO_incidence,
                                               country = sel_iso_xgb,
                                               num_category = 10,
                                               train_num_start = 2, 
                                               train_num_end = 4,
                                               numWeek_ahead = 1, 
                                               yr53week = 2015)

acc_2week_pred15_hist <- compare_accuracy_hist(flu_data = fluWHO_incidence,
                                               country = sel_iso_xgb,
                                               num_category = 10,
                                               train_num_start = 0, 
                                               train_num_end = 4,
                                               numWeek_ahead = 2, 
                                               yr53week = 2015)

acc_2week_pred16_hist <- compare_accuracy_hist(flu_data = fluWHO_incidence,
                                               country = sel_iso_xgb,
                                               num_category = 10,
                                               train_num_start = 1, 
                                               train_num_end = 4,
                                               numWeek_ahead = 2, 
                                               yr53week = 2015)

acc_2week_pred17_hist <- compare_accuracy_hist(flu_data = fluWHO_incidence,
                                               country = sel_iso_xgb,
                                               num_category = 10,
                                               train_num_start = 2, 
                                               train_num_end = 4,
                                               numWeek_ahead = 2, 
                                               yr53week = 2015)

acc_3week_pred15_hist <- compare_accuracy_hist(flu_data = fluWHO_incidence,
                                               country = sel_iso_xgb,
                                               num_category = 10,
                                               train_num_start = 0, 
                                               train_num_end = 4,
                                               numWeek_ahead = 3, 
                                               yr53week = 2015)

acc_3week_pred16_hist <- compare_accuracy_hist(flu_data = fluWHO_incidence,
                                               country = sel_iso_xgb,
                                               num_category = 10,
                                               train_num_start = 1, 
                                               train_num_end = 4,
                                               numWeek_ahead = 3, 
                                               yr53week = 2015)

acc_3week_pred17_hist <- compare_accuracy_hist(flu_data = fluWHO_incidence,
                                               country = sel_iso_xgb,
                                               num_category = 10,
                                               train_num_start = 2, 
                                               train_num_end = 4,
                                               numWeek_ahead = 3, 
                                               yr53week = 2015)

acc_4week_pred15_hist <- compare_accuracy_hist(flu_data = fluWHO_incidence,
                                               country = sel_iso_xgb,
                                               num_category = 10,
                                               train_num_start = 0, 
                                               train_num_end = 4,
                                               numWeek_ahead = 4, 
                                               yr53week = 2015)

acc_4week_pred16_hist <- compare_accuracy_hist(flu_data = fluWHO_incidence,
                                               country = sel_iso_xgb,
                                               num_category = 10,
                                               train_num_start = 1, 
                                               train_num_end = 4,
                                               numWeek_ahead = 4, 
                                               yr53week = 2015)

acc_4week_pred17_hist <- compare_accuracy_hist(flu_data = fluWHO_incidence,
                                               country = sel_iso_xgb,
                                               num_category = 10,
                                               train_num_start = 2, 
                                               train_num_end = 4,
                                               numWeek_ahead = 4, 
                                               yr53week = 2015)

# Null (repeat) model
acc_1week_pred15_null <- compare_accuracy_repeat(flu_data = fluWHO_incidence,
                                             country = sel_iso_xgb,
                                             num_category = 10,
                                             numWeek_ahead = 1,
                                             test_year = 2015,
                                             yr53week = 2015)

acc_1week_pred16_null <- compare_accuracy_repeat(flu_data = fluWHO_incidence,
                                                 country = sel_iso_xgb,
                                                 num_category = 10,
                                                 numWeek_ahead = 1,
                                                 test_year = 2016,
                                                 yr53week = 2015)

acc_1week_pred17_null <- compare_accuracy_repeat(flu_data = fluWHO_incidence,
                                                 country = sel_iso_xgb,
                                                 num_category = 10,
                                                 numWeek_ahead = 1,
                                                 test_year = 2017,
                                                 yr53week = 2015)

acc_2week_pred15_null <- compare_accuracy_repeat(flu_data = fluWHO_incidence,
                                             country = sel_iso_xgb,
                                             num_category = 10,
                                             numWeek_ahead = 2, 
                                             test_year = 2015,
                                             yr53week = 2015)

acc_2week_pred16_null <- compare_accuracy_repeat(flu_data = fluWHO_incidence,
                                                 country = sel_iso_xgb,
                                                 num_category = 10,
                                                 numWeek_ahead = 2, 
                                                 test_year = 2016,
                                                 yr53week = 2015)

acc_2week_pred17_null <- compare_accuracy_repeat(flu_data = fluWHO_incidence,
                                                 country = sel_iso_xgb,
                                                 num_category = 10,
                                                 numWeek_ahead = 2, 
                                                 test_year = 2017,
                                                 yr53week = 2015)

acc_3week_pred15_null <- compare_accuracy_repeat(flu_data = fluWHO_incidence,
                                             country = sel_iso_xgb,
                                             num_category = 10,
                                             numWeek_ahead = 3, 
                                             test_year = 2015,
                                             yr53week = 2015)

acc_3week_pred16_null <- compare_accuracy_repeat(flu_data = fluWHO_incidence,
                                                 country = sel_iso_xgb,
                                                 num_category = 10,
                                                 numWeek_ahead = 3, 
                                                 test_year = 2016,
                                                 yr53week = 2015)

acc_3week_pred17_null <- compare_accuracy_repeat(flu_data = fluWHO_incidence,
                                                 country = sel_iso_xgb,
                                                 num_category = 10,
                                                 numWeek_ahead = 3, 
                                                 test_year = 2017,
                                                 yr53week = 2015)

acc_4week_pred15_null <- compare_accuracy_repeat(flu_data = fluWHO_incidence,
                                              country = sel_iso_xgb,
                                              num_category = 10,
                                              numWeek_ahead = 4,
                                              test_year = 2015,
                                              yr53week = 2015)

acc_4week_pred16_null <- compare_accuracy_repeat(flu_data = fluWHO_incidence,
                                                 country = sel_iso_xgb,
                                                 num_category = 10,
                                                 numWeek_ahead = 4,
                                                 test_year = 2016,
                                                 yr53week = 2015)

acc_4week_pred17_null <- compare_accuracy_repeat(flu_data = fluWHO_incidence,
                                                 country = sel_iso_xgb,
                                                 num_category = 10,
                                                 numWeek_ahead = 4,
                                                 test_year = 2017,
                                                 yr53week = 2015)

baseline_acc <- list(acc_1week_pred15_hist = acc_1week_pred15_hist, acc_1week_pred16_hist = acc_1week_pred16_hist,
                     acc_1week_pred17_hist = acc_1week_pred17_hist,
                     acc_2week_pred15_hist = acc_2week_pred15_hist, acc_2week_pred16_hist = acc_2week_pred16_hist,
                     acc_2week_pred17_hist = acc_2week_pred17_hist,
                     acc_3week_pred15_hist = acc_3week_pred15_hist, acc_3week_pred16_hist = acc_3week_pred16_hist,
                     acc_3week_pred17_hist = acc_3week_pred17_hist,
                     acc_4week_pred15_hist = acc_4week_pred15_hist, acc_4week_pred16_hist = acc_4week_pred16_hist,
                     acc_4week_pred17_hist = acc_4week_pred17_hist,
                     acc_1week_pred15_null = acc_1week_pred15_null, acc_1week_pred16_null = acc_1week_pred16_null, 
                     acc_1week_pred17_null = acc_1week_pred17_null,
                     acc_2week_pred15_null = acc_2week_pred15_null, acc_2week_pred16_null = acc_2week_pred16_null, 
                     acc_2week_pred17_null = acc_2week_pred17_null,
                     acc_3week_pred15_null = acc_3week_pred15_null, acc_3week_pred16_null = acc_3week_pred16_null,
                     acc_3week_pred17_null = acc_3week_pred17_null,
                     acc_4week_pred15_null = acc_4week_pred15_null, acc_4week_pred16_null = acc_4week_pred16_null,
                     acc_4week_pred17_null = acc_4week_pred17_null)

saveRDS(baseline_acc, "./saved_objects/acc_WHO_baseline.rds")

#' check accuracy of hist model and null model for year 2015-2017
baseline_acc <- readRDS("./saved_objects/acc_WHO_baseline.rds")

indi_country_acc_baseline <- function(pred, year, nweek_ahead, model, country, num_category){
  pred <- pred %>%
    separate(Week_time, c("Year","Week"), "-")
    
  if(nweek_ahead == 1){
      pred <- pred %>% 
        dplyr::select("Country", "Year", "Week", "OneWeek_ahead", "Observation", "Accurate")%>% 
        rename(Prediction = OneWeek_ahead) %>%
        mutate(Observation = as.numeric(Observation),
               Prediction = as.numeric(Prediction))
    }
    if(nweek_ahead == 2){
      pred <- pred %>% 
        dplyr::select("Country", "Year", "Week", "TwoWeek_ahead", "Observation", "Accurate")%>% 
        rename(Prediction = TwoWeek_ahead)%>%
        mutate(Observation = as.numeric(Observation),
               Prediction = as.numeric(Prediction))
    }
    
    if(nweek_ahead == 3){
      pred <- pred %>% 
        dplyr::select("Country", "Year", "Week", "ThreeWeek_ahead", "Observation", "Accurate")%>% 
        rename(Prediction = ThreeWeek_ahead)%>%
        mutate(Observation = as.numeric(Observation),
               Prediction = as.numeric(Prediction))
    }
    if(nweek_ahead == 4){
      pred <- pred %>% 
        dplyr::select("Country", "Year", "Week", "FourWeek_ahead", "Observation", "Accurate") %>% 
        rename(Prediction = FourWeek_ahead)%>%
        mutate(Observation = as.numeric(Observation),
               Prediction = as.numeric(Prediction))
    }
  
  indi_pred <-  pred %>% 
    filter(Country == country)
    
    accuracy <- round(length(which(indi_pred$Accurate == 1))/nrow(indi_pred),3)
    MZE <- 1- accuracy
    
    # macro_averaged MAE
    macroMAE_vec <- c()
    for (i in 1:num_category){
      if (i %in% indi_pred$Observation){
        mat <- indi_pred[which(indi_pred$Observation == i),]
        macroMAE_tmp <- mae(mat$Observation, mat$Prediction)
      }else{
        macroMAE_tmp <- 0 
      }
      macroMAE_vec <- c(macroMAE_vec, macroMAE_tmp)
    }
    macroMAE <- sum(macroMAE_vec)/num_category
    
    indi_acc <- c(country, year, nweek_ahead, model, accuracy, MZE, macroMAE)

  return(indi_acc)
}

#' Historical average 
indi_acc_1week_pred15_hist <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi_hist <- indi_country_acc_baseline(baseline_acc$acc_1week_pred15_hist$pred, year = 2015,
                                              nweek_ahead = 1, model = "hist", 
                                              country = sel_iso_xgb[i], num_category = 10)
  indi_acc_1week_pred15_hist <- rbind(indi_acc_1week_pred15_hist, temp_indi_hist)
  
}

indi_acc_1week_pred16_hist <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi_hist <- indi_country_acc_baseline(baseline_acc$acc_1week_pred16_hist$pred, year = 2016,
                                              nweek_ahead = 1, model = "hist", 
                                              country = sel_iso_xgb[i], num_category = 10)
  indi_acc_1week_pred16_hist <- rbind(indi_acc_1week_pred16_hist, temp_indi_hist)
  
}

indi_acc_1week_pred17_hist <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi_hist <- indi_country_acc_baseline(baseline_acc$acc_1week_pred17_hist$pred, year = 2017,
                                              nweek_ahead = 1, model = "hist", 
                                              country = sel_iso_xgb[i], num_category = 10)
  indi_acc_1week_pred17_hist <- rbind(indi_acc_1week_pred17_hist, temp_indi_hist)
  
}

indi_acc_2week_pred15_hist <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi_hist <- indi_country_acc_baseline(baseline_acc$acc_2week_pred15_hist$pred, year = 2015,
                                              nweek_ahead = 2, model = "hist", 
                                              country = sel_iso_xgb[i], num_category = 10)
  indi_acc_2week_pred15_hist <- rbind(indi_acc_2week_pred15_hist, temp_indi_hist)
  
}

indi_acc_2week_pred16_hist <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi_hist <- indi_country_acc_baseline(baseline_acc$acc_2week_pred16_hist$pred, year = 2016,
                                              nweek_ahead = 2, model = "hist", 
                                              country = sel_iso_xgb[i], num_category = 10)
  indi_acc_2week_pred16_hist <- rbind(indi_acc_2week_pred16_hist, temp_indi_hist)
  
}


indi_acc_2week_pred17_hist <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi_hist <- indi_country_acc_baseline(baseline_acc$acc_2week_pred17_hist$pred, year = 2017,
                                              nweek_ahead = 2, model = "hist", 
                                              country = sel_iso_xgb[i], num_category = 10)
  indi_acc_2week_pred17_hist <- rbind(indi_acc_2week_pred17_hist, temp_indi_hist)
  
}

indi_acc_3week_pred15_hist <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi_hist <- indi_country_acc_baseline(baseline_acc$acc_3week_pred15_hist$pred, year = 2015,
                                              nweek_ahead = 3, model = "hist", 
                                              country = sel_iso_xgb[i], num_category = 10)
  indi_acc_3week_pred15_hist <- rbind(indi_acc_3week_pred15_hist, temp_indi_hist)
  
}

indi_acc_3week_pred16_hist <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi_hist <- indi_country_acc_baseline(baseline_acc$acc_3week_pred16_hist$pred, year = 2016,
                                              nweek_ahead = 3, model = "hist", 
                                              country = sel_iso_xgb[i], num_category = 10)
  indi_acc_3week_pred16_hist <- rbind(indi_acc_3week_pred16_hist, temp_indi_hist)
  
}

indi_acc_3week_pred17_hist <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi_hist <- indi_country_acc_baseline(baseline_acc$acc_3week_pred17_hist$pred, year = 2017,
                                              nweek_ahead = 3, model = "hist", 
                                              country = sel_iso_xgb[i], num_category = 10)
  indi_acc_3week_pred17_hist <- rbind(indi_acc_3week_pred17_hist, temp_indi_hist)
  
}

indi_acc_4week_pred15_hist <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi_hist <- indi_country_acc_baseline(baseline_acc$acc_4week_pred15_hist$pred, year = 2015,
                                              nweek_ahead = 4, model = "hist", 
                                              country = sel_iso_xgb[i], num_category = 10)
  indi_acc_4week_pred15_hist <- rbind(indi_acc_4week_pred15_hist, temp_indi_hist)
  
}

indi_acc_4week_pred16_hist <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi_hist <- indi_country_acc_baseline(baseline_acc$acc_4week_pred16_hist$pred, year = 2016,
                                              nweek_ahead = 4, model = "hist", 
                                              country = sel_iso_xgb[i], num_category = 10)
  indi_acc_4week_pred16_hist <- rbind(indi_acc_4week_pred16_hist, temp_indi_hist)
  
}

indi_acc_4week_pred17_hist <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi_hist <- indi_country_acc_baseline(baseline_acc$acc_4week_pred17_hist$pred, year = 2017,
                                              nweek_ahead = 4, model = "hist", 
                                              country = sel_iso_xgb[i], num_category = 10)
  indi_acc_4week_pred17_hist <- rbind(indi_acc_4week_pred17_hist, temp_indi_hist)
  
}

#' Null model

indi_acc_1week_pred15_null <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi_null <- indi_country_acc_baseline(baseline_acc$acc_1week_pred15_null$pred, year = 2015,
                                              nweek_ahead = 1, model = "null", 
                                              country = sel_iso_xgb[i], num_category = 10)
  indi_acc_1week_pred15_null <- rbind(indi_acc_1week_pred15_null, temp_indi_null)
  
}

indi_acc_1week_pred16_null <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi_null <- indi_country_acc_baseline(baseline_acc$acc_1week_pred16_null$pred, year = 2016,
                                              nweek_ahead = 1, model = "null", 
                                              country = sel_iso_xgb[i], num_category = 10)
  indi_acc_1week_pred16_null <- rbind(indi_acc_1week_pred16_null, temp_indi_null)
  
}

indi_acc_1week_pred17_null <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi_null <- indi_country_acc_baseline(baseline_acc$acc_1week_pred17_null$pred, year = 2017,
                                              nweek_ahead = 1, model = "null", 
                                              country = sel_iso_xgb[i], num_category = 10)
  indi_acc_1week_pred17_null <- rbind(indi_acc_1week_pred17_null, temp_indi_null)
  
}

indi_acc_2week_pred15_null <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi_null <- indi_country_acc_baseline(baseline_acc$acc_2week_pred15_null$pred, year = 2015,
                                              nweek_ahead = 2, model = "null", 
                                              country = sel_iso_xgb[i], num_category = 10)
  indi_acc_2week_pred15_null <- rbind(indi_acc_2week_pred15_null, temp_indi_null)
  
}

indi_acc_2week_pred16_null <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi_null <- indi_country_acc_baseline(baseline_acc$acc_2week_pred16_null$pred, year = 2016,
                                              nweek_ahead = 2, model = "null", 
                                              country = sel_iso_xgb[i], num_category = 10)
  indi_acc_2week_pred16_null <- rbind(indi_acc_2week_pred16_null, temp_indi_null)
  
}

indi_acc_2week_pred17_null <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi_null <- indi_country_acc_baseline(baseline_acc$acc_2week_pred17_null$pred, year = 2017,
                                              nweek_ahead = 2, model = "null", 
                                              country = sel_iso_xgb[i], num_category = 10)
  indi_acc_2week_pred17_null <- rbind(indi_acc_2week_pred17_null, temp_indi_null)
  
}

indi_acc_3week_pred15_null <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi_null <- indi_country_acc_baseline(baseline_acc$acc_3week_pred15_null$pred, year = 2015,
                                              nweek_ahead = 3, model = "null", 
                                              country = sel_iso_xgb[i], num_category = 10)
  indi_acc_3week_pred15_null <- rbind(indi_acc_3week_pred15_null, temp_indi_null)
  
}

indi_acc_3week_pred16_null <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi_null <- indi_country_acc_baseline(baseline_acc$acc_3week_pred16_null$pred, year = 2016,
                                              nweek_ahead = 3, model = "null", 
                                              country = sel_iso_xgb[i], num_category = 10)
  indi_acc_3week_pred16_null <- rbind(indi_acc_3week_pred16_null, temp_indi_null)
  
}

indi_acc_3week_pred17_null <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi_null <- indi_country_acc_baseline(baseline_acc$acc_3week_pred17_null$pred, year = 2017,
                                              nweek_ahead = 3, model = "null", 
                                              country = sel_iso_xgb[i], num_category = 10)
  indi_acc_3week_pred17_null <- rbind(indi_acc_3week_pred17_null, temp_indi_null)
  
}

indi_acc_4week_pred15_null <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi_null <- indi_country_acc_baseline(baseline_acc$acc_4week_pred15_null$pred, year = 2015,
                                              nweek_ahead = 4, model = "null", 
                                              country = sel_iso_xgb[i], num_category = 10)
  indi_acc_4week_pred15_null <- rbind(indi_acc_4week_pred15_null, temp_indi_null)
  
}

indi_acc_4week_pred16_null <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi_null <- indi_country_acc_baseline(baseline_acc$acc_4week_pred16_null$pred, year = 2016,
                                              nweek_ahead = 4, model = "null", 
                                              country = sel_iso_xgb[i], num_category = 10)
  indi_acc_4week_pred16_null <- rbind(indi_acc_4week_pred16_null, temp_indi_null)
  
}

indi_acc_4week_pred17_null <- NULL
for (i in 1:length(sel_iso_xgb)){
  temp_indi_null <- indi_country_acc_baseline(baseline_acc$acc_4week_pred17_null$pred, year = 2017,
                                              nweek_ahead = 4, model = "null", 
                                              country = sel_iso_xgb[i], num_category = 10)
  indi_acc_4week_pred17_null <- rbind(indi_acc_4week_pred17_null, temp_indi_null)
  
}


acc_hist_null_byYears <- rbind(indi_acc_1week_pred15_hist, indi_acc_1week_pred16_hist, indi_acc_1week_pred17_hist,
                               indi_acc_2week_pred15_hist, indi_acc_2week_pred16_hist, indi_acc_2week_pred17_hist,
                               indi_acc_3week_pred15_hist, indi_acc_3week_pred16_hist, indi_acc_3week_pred17_hist,
                               indi_acc_4week_pred15_hist, indi_acc_4week_pred16_hist, indi_acc_4week_pred17_hist,
                               indi_acc_1week_pred15_null, indi_acc_1week_pred16_null, indi_acc_1week_pred17_null,
                               indi_acc_2week_pred15_null, indi_acc_2week_pred16_null, indi_acc_2week_pred17_null,
                               indi_acc_3week_pred15_null, indi_acc_3week_pred16_null, indi_acc_3week_pred17_null,
                               indi_acc_4week_pred15_null, indi_acc_4week_pred16_null, indi_acc_4week_pred17_null) %>% 
  as.data.frame() %>% 
  rename(Country = V1,
         Year = V2,
         Week_ahead = V3,
         Model = V4,
         Accuracy = V5,
         MZE = V6,
         macroMAE = V7) %>% 
  mutate(Accuracy = as.numeric(Accuracy),
         MZE = as.numeric(MZE),
         macroMAE = as.numeric(macroMAE))

write.csv(acc_hist_null_byYears, "./saved_objects/acc_hist_null_byYears.csv", row.names = FALSE)

  
########## calculate some descriptive statistics ###########
gbm_complex_all <- readRDS("./saved_objects/df_gbm_complex_all.rds")

table(gbm_complex_all$Y_week0, useNA = "always")
prop_category <- gbm_complex_all %>% 
  group_by(Y_week0) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

weeks <- c()
for (i in 1:ncol(fluWHO_incidence)){
  weeks <- append(weeks, length(which(!is.na(fluWHO_incidence[,i]))))
}

min(weeks)
max(weeks)
mean(weeks)
median(weeks)


#' MZE by countries
indi_acc_roll_fix_by_countryWeek <- read.csv("./saved_objects/indi_acc_roll_fix_by_countryWeek.csv")

indi_acc_roll <- indi_acc_roll_fix_by_countryWeek %>% 
  filter(Model == "rolling")

range(indi_acc_roll$MZE[which(indi_acc_roll$Week_ahead==1)]) # 0.000 0.643
median(indi_acc_roll$MZE[which(indi_acc_roll$Week_ahead==1)]) # 0.21

range(indi_acc_roll$MZE[which(indi_acc_roll$Week_ahead==2)]) # 0.000 0.765
median(indi_acc_roll$MZE[which(indi_acc_roll$Week_ahead==2)]) #  0.261

range(indi_acc_roll$MZE[which(indi_acc_roll$Week_ahead==3)]) # 0.000 0.765
median(indi_acc_roll$MZE[which(indi_acc_roll$Week_ahead==3)]) # 0.287

range(indi_acc_roll$MZE[which(indi_acc_roll$Week_ahead==4)]) # 0.000 0.824
median(indi_acc_roll$MZE[which(indi_acc_roll$Week_ahead==4)]) #  0.288

#' find countries with highest and lowest MZE 
ave_MZE_byCountry <- NULL
for (i in 1:length(unique(indi_acc_roll$Country))){
  temp_MZE <- mean(indi_acc_roll$MZE[which(indi_acc_roll$Country==sel_iso_xgb[i])])
  ave_MZE_byCountry <- rbind(ave_MZE_byCountry, c(sel_iso_xgb[i], temp_MZE))
}

ave_MZE_byCountry <- ave_MZE_byCountry %>%
  as.data.frame() %>%
  rename(Country = V1,
         MZE = V2) %>%
  mutate(MZE = as.numeric(MZE)) %>%
  arrange(desc(MZE))

#' find countries with highest and lowest MMAE 
ave_MMAE_byCountry <- NULL
for (i in 1:length(unique(indi_acc_roll$Country))){
  temp_MMAE <- mean(indi_acc_roll$macroMAE[which(indi_acc_roll$Country==sel_iso_xgb[i])])
  ave_MMAE_byCountry <- rbind(ave_MMAE_byCountry, c(sel_iso_xgb[i], temp_MMAE))
}

ave_MMAE_byCountry <- ave_MMAE_byCountry %>%
  as.data.frame() %>%
  rename(Country = V1,
         MMAE = V2) %>%
  mutate(MMAE = as.numeric(MMAE)) %>%
  arrange(desc(MMAE))
