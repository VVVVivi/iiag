#' Forecasting analysis on WHO data

#' Make a note of when the report was generated.
Sys.time()
knitr.table.format = "markdown"

#' As always, remove all objects fromt the workspace before starting.
rm(list = ls(all = TRUE))

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

# Create grid search for hyper parameters
param_df <- expand.grid(booster = "gbtree",
                        objective = "multi:softprob",
                        num_class = 10,
                        max_depth = seq(3,10, 1),
                        min_child_weight = seq(1,10, 1),
                        subsample = seq(0.7, 1, 0.1),
                        colsample_bytree = seq(0.6, 1, 0.1),
                        eta = seq(0.01, 0.3, 0.05))

# write a function for 4-fold cross-validation 
xgboost_para <- function(flu_data, country, num_category,
                               train_num_start, train_num_end, 
                               nWeek_ahead, yr53week, nrounds,
                               param_df){
  # set up dataset for xgboost
  flu_data_complex <- adjust.data.size(flu_data, country, num_category, nWeek_ahead, yr53week)
  
  year_start <- min(as.numeric(substr(rownames(flu_data_complex),0,4)))
  year_end <- max(as.numeric(substr(rownames(flu_data_complex),0,4)))
  start_year_tr <- year_start + train_num_start
  end_year_tr <-  start_year_tr + train_num_end
  start_year_ts <- end_year_tr + 1
  
  if ((start_year_ts == 2017 && year_end == 2018) == TRUE ){
    end_year_ts <- year_end
  }else{
    end_year_ts <- start_year_ts
  }
  
  xgb_tr <- xgboost_dat(flu_data_complex, start_year_tr, end_year_tr)
  xgb_ts <- xgboost_dat(flu_data_complex, start_year_ts, end_year_ts)

  
  watchlist <- list(train = xgb_tr, test = xgb_ts)
  xgb_model <- xgb.train(# params = params_list, 
                         booster = "gbtree",
                         objective = "multi:softprob",
                         num_class = num_category,
                         max_depth = param_df$max_depth,
                         eta = param_df$eta,
                         subsample = param_df$subsample,
                         colsample_bytree = param_df$colsample_bytree,
                         min_child_weight = param_df$min_child_weight,
                         data = xgb_tr, nrounds = nrounds, 
                         watchlist = watchlist,verbose = 2, print_every_n = 10,
                         early_stopping_round = 20)

  lowest_error <- min(xgb_model$evaluation_log$test_mlogloss)
  
  return(lowest_error)
}

# test <- xgboost_para(flu_data = fluWHO_incidence,
#                      country = sel_iso_xgb[1],
#                      num_category = 10,
#                      train_num_start = 0,
#                      train_num_end = 1,
#                      nWeek_ahead = 1,
#                      yr53week = 2015,
#                      nrounds = 200,
#                      param_df = param_df[1,])


xgboost_cv_para <- function(flu_data, country, num_category,
                         cv_fold, nWeek_ahead, yr53week, 
                         nrounds, param_df){
  cv_fold_error <- c()
  for (i in 1:length(cv_fold)){
    cv_fold_error_temp <- xgboost_para(flu_data, country, num_category,
                                       train_num_start = 0, train_num_end = cv_fold[i], 
                                       nWeek_ahead, yr53week, nrounds,
                                       param_df)
    
    cv_fold_error <- append(cv_fold_error, cv_fold_error_temp)
  }

  cv_error <- mean(cv_fold_error)
  cv_error
}

test_cv <- xgboost_cv_para(flu_data = fluWHO_incidence,
                     country = sel_iso_xgb[1],
                     num_category = 10,
                     cv_fold = c(1:3),
                     nWeek_ahead = 1,
                     yr53week = 2015,
                     nrounds = 300,
                     param_df = param_df[1,])

# Create empty lists

lowest_error_list <- list()
error_per_country <- c()

for (row in 1:nrow(param_df)){
  for (country in 1:length(sel_iso_xgb)){
    error_per_country_temp <- xgboost_cv_para(flu_data = fluWHO_incidence,
                                              country = sel_iso_xgb[country],
                                              num_category = 10,
                                              cv_fold = c(1:3),
                                              nWeek_ahead = 1,
                                              yr53week = 2015,
                                              nrounds = 300,
                                              param_df = param_df[row,])
    error_per_country <- append(error_per_country, error_per_country_temp)
  }
  lowest_error_ave <- as.data.frame(mean(error_per_country))
  lowest_error_list[[row]] <- lowest_error_ave
}

# Create object that contains all accuracy's
lowest_error_df <- do.call(rbind, lowest_error_list)

# Bind columns of accuracy values and random hyperparameter values
randomsearch <- cbind(lowest_error_df, parameters_df)

# Quickly display highest accuracy
max(randomsearch$`1 - min(mdcv$evaluation_log$val_error)`)

# Prepare table
randomsearch <- as.data.frame(randomsearch) %>%
  rename(val_acc = `1 - min(mdcv$evaluation_log$val_error)`) %>%
  arrange(-val_acc)
randomsearch


