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
  set.seed(123)
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
                         early_stopping_round = 20
                         )

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
#                      param_df = param_step1[1,])


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

# test_cv <- xgboost_cv_para(flu_data = fluWHO_incidence,
#                      country = sel_iso_xgb[1],
#                      num_category = 10,
#                      cv_fold = c(1:3),
#                      nWeek_ahead = 1,
#                      yr53week = 2015,
#                      nrounds = 300,
#                      param_df = param_step1[1,])

#' Create grid search for hyper parameters
#' 1. Search for the optimal learning_rate and gamma
param_step1 <- expand.grid(booster = "gbtree",
                           objective = "multi:softprob",
                           num_class = 10,
                           gamma = c(0.1, 0.2, 0.5, 1, 1.5, 2, 10),
                           eta = c(0.01, 0.02, 0.03, 0.06, 0.1, 0.2, 0.3),
                           max_depth = 6,
                           min_child_weight = 1,
                           subsample = 1, 
                           colsample_bytree = 1)


#' Create empty lists

lowest_error_list_step1 <- list()
error_per_country_step1 <- list()

for (row in 1:nrow(param_step1)){
  error_per_country <- c()
  for (country in 1:length(sel_iso_xgb)){
    set.seed(123)
    error_per_country_temp <- xgboost_cv_para(flu_data = fluWHO_incidence,
                                              country = sel_iso_xgb[country],
                                              num_category = 10,
                                              cv_fold = c(1:3),
                                              nWeek_ahead = 1,
                                              yr53week = 2015,
                                              nrounds = 300,
                                              param_df = param_step1[row,])
    error_per_country <- append(error_per_country, error_per_country_temp)
  }
  error_per_country_step1[[row]] <- error_per_country
  lowest_error_ave <- as.data.frame(mean(error_per_country))
  lowest_error_list_step1[[row]] <- lowest_error_ave
}

# Create object that contains all accuracy's
lowest_error_df_step1 <- do.call(rbind, lowest_error_list_step1)

# Bind columns of accuracy values and random hyperparameter values
randomsearch_step1 <- cbind(lowest_error_df_step1, param_step1)

# Quickly display highest accuracy
min(randomsearch_step1$`mean(error_per_country)`)

# Prepare table
randomsearch_step1 <- as.data.frame(randomsearch_step1) %>%
  rename(val_mlogloss = `mean(error_per_country)`) %>%
  arrange(val_mlogloss)
randomsearch_step1


#' Search for optimal max_depth and min_child_weight
param_step2 <- expand.grid(booster = "gbtree",
                           objective = "multi:softprob",
                           num_class = 10,
                           max_depth = seq(1,10, 1),
                           min_child_weight = seq(1,10, 1),
                           subsample = 1,
                           colsample_bytree = 1,
                           eta = randomsearch_step1$eta[1],
                           gamma = randomsearch_step1$gamma[1])

#' Create empty lists

lowest_error_list_step2 <- list()
error_per_country_step2 <- list()

for (row in 1:nrow(param_step2)){
  error_per_country <- c()
  for (country in 1:length(sel_iso_xgb)){
    set.seed(123)
    error_per_country_temp <- xgboost_cv_para(flu_data = fluWHO_incidence,
                                              country = sel_iso_xgb[country],
                                              num_category = 10,
                                              cv_fold = c(1:3),
                                              nWeek_ahead = 1,
                                              yr53week = 2015,
                                              nrounds = 300,
                                              param_df = param_step2[row,])
    error_per_country <- append(error_per_country, error_per_country_temp)
  }
  error_per_country_step2[[row]] <- error_per_country
  lowest_error_ave <- as.data.frame(mean(error_per_country))
  lowest_error_list_step2[[row]] <- lowest_error_ave
}

# Create object that contains all accuracy's
lowest_error_df_step2 <- do.call(rbind, lowest_error_list_step2)

# Bind columns of accuracy values and random hyperparameter values
randomsearch_step2 <- cbind(lowest_error_df_step2, param_step2)

# Quickly display highest accuracy
min(randomsearch_step2$`mean(error_per_country)`)

# Prepare table
randomsearch_step2 <- as.data.frame(randomsearch_step2) %>%
  rename(val_mlogloss = `mean(error_per_country)`) %>%
  arrange(val_mlogloss)
randomsearch_step2

#' Search for optimal subsample and colsample_bytree
param_step3 <- expand.grid(booster = "gbtree",
                           objective = "multi:softprob",
                           num_class = 10,
                           max_depth = randomsearch_step2$max_depth[1],
                           min_child_weight = randomsearch_step2$min_child_weight[1],
                           subsample = seq(0.1,1,0.1),
                           colsample_bytree = seq(0.1,1,0.1),
                           eta = randomsearch_step1$eta[1],
                           gamma = randomsearch_step1$gamma[1])

#' Create empty lists

lowest_error_list_step3 <- list()
error_per_country_step3 <- list()

for (row in 1:nrow(param_step3)){
  error_per_country <- c()
  for (country in 1:length(sel_iso_xgb)){
    set.seed(123)
    error_per_country_temp <- xgboost_cv_para(flu_data = fluWHO_incidence,
                                              country = sel_iso_xgb[country],
                                              num_category = 10,
                                              cv_fold = c(1:3),
                                              nWeek_ahead = 1,
                                              yr53week = 2015,
                                              nrounds = 300,
                                              param_df = param_step3[row,])
    error_per_country <- append(error_per_country, error_per_country_temp)
  }
  error_per_country_step3[[row]] <- error_per_country
  lowest_error_ave <- as.data.frame(mean(error_per_country))
  lowest_error_list_step3[[row]] <- lowest_error_ave
}

# Create object that contains all accuracy's
lowest_error_df_step3 <- do.call(rbind, lowest_error_list_step3)

# Bind columns of accuracy values and random hyperparameter values
randomsearch_step3 <- cbind(lowest_error_df_step3, param_step3)

# Quickly display highest accuracy
min(randomsearch_step3$`mean(error_per_country)`)

# Prepare table
randomsearch_step3 <- as.data.frame(randomsearch_step3) %>%
  rename(val_mlogloss = `mean(error_per_country)`) %>%
  arrange(val_mlogloss)
randomsearch_step3

para_df_final <- list(booster = "gbtree", 
                      objective = "multi:softprob",
                      num_class = 10,
                      max_depth = randomsearch_step2$max_depth[1],
                      min_child_weight = randomsearch_step2$min_child_weight[1],
                      eta = randomsearch_step1$eta[1],
                      gamma = randomsearch_step1$gamma[1],
                      subsample = randomsearch_step3$subsample[1],
                      colsample_bytree = randomsearch_step3$colsample_bytree[1])

saveRDS(para_df_final, "./saved_objects/hyperpara_list_final.rds")
