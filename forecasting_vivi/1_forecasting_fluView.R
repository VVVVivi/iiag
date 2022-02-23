#' Forecasting analysis on FluView data

#' Make a note of when the report was generated.
Sys.time()
knitr.table.format = "markdown"

#' As always, remove all objects fromt the workspace before starting.
rm(list = ls(all = TRUE))

setwd("C:/Users/hw3616/Desktop/Imperial/Project1_Forecasting/Project_Coding/iiag/forecasting_vivi")

#' Pull in packages needed
source("./functions/load_packages.R")

pkgs <- c("xgboost", "stringr", "rasterVis", "cdcfluview", "hrbrthemes",
          "dplyr", "ggplot2", "cdcfluview")
load_package(pkgs)

#' Load self-written functions 
source("./fluView_funcs_vivi.R")
source("./vivi_funcs.R")
source("./gbm_complex_funcs.R")
source("./fluView_funcs_vivi.R")

#' Load data
fluView <- readRDS("./saved_objects/fluView.rds")

fluView_incidence <- fluView[[1]]
us_states <- fluView[[2]]

#' XGB model
#' gbm_complex_fview: shape data structure and add features


######### individual state forecasting - fix window ########

#' 1-week ahead 
#' 2010-2017 traingin, 2018 testing 
indi18_one <- compare_accuracy_indi_fview(fluView_incidence,us_states,10,0,7,1,2014)

indi18_one_df <- accuracy_score_fview(indi18_one,us_states)

#' 2011-2018 traingin, 2019 testing 
indi19_one <- compare_accuracy_indi_fview(fluView_incidence,us_states,10,1,7,1,2014)

indi19_one_df <- accuracy_score_fview(indi19_one,us_states)

#' 2012-2019 traingin, 2020 testing 
indi20_one <- compare_accuracy_indi_fview(fluView_incidence,us_states,10,2,7,1,c(2014, 2020))

indi20_one_df <- accuracy_score_fview(indi20_one,us_states)

#' 2013-2020 traingin, 2021 testing 
indi21_one <- compare_accuracy_indi_fview(fluView_incidence,us_states,10,3,7,1,c(2014, 2020))

indi21_one_df <- accuracy_score_fview(indi21_one,us_states)

indi_one_df <- full_join(indi18_one_df, indi19_one_df, by = "States") %>% 
  full_join(., indi20_one_df, by = "States") %>% 
  full_join(., indi21_one_df, by = "States") %>% 
  rename(Accuracy_score_2018 = Accuracy_score.x,
         Accuracy_score_2019 = Accuracy_score.y,
         Accuracy_score_2020 = Accuracy_score.x.x,
         Accuracy_score_2021 = Accuracy_score.y.y)

#' 2-week ahead 
#' 2010-2017 traingin, 2018 testing 
indi18_two <- compare_accuracy_indi_fview(fluView_incidence,us_states,10,0,7,2,2014)

indi18_two_df <- accuracy_score_fview(indi18_two,us_states)

#' 2011-2018 traingin, 2019 testing 
indi19_two <- compare_accuracy_indi_fview(fluView_incidence,us_states,10,1,7,2,2014)

indi19_two_df <- accuracy_score_fview(indi19_two,us_states)

#' 2012-2019 traingin, 2020 testing 
indi20_two <- compare_accuracy_indi_fview(fluView_incidence,us_states,10,2,7,2,c(2014, 2020))

indi20_two_df <- accuracy_score_fview(indi20_two,us_states)

#' 2013-2020 traingin, 2021 testing 
indi21_two <- compare_accuracy_indi_fview(fluView_incidence,us_states,10,3,7,2,c(2014, 2020))

indi21_two_df <- accuracy_score_fview(indi21_two,us_states)

indi_two_df <- full_join(indi18_two_df, indi19_two_df, by = "States") %>% 
  full_join(., indi20_two_df, by = "States") %>% 
  full_join(., indi21_two_df, by = "States") %>% 
  rename(Accuracy_score_2018 = Accuracy_score.x,
         Accuracy_score_2019 = Accuracy_score.y,
         Accuracy_score_2020 = Accuracy_score.x.x,
         Accuracy_score_2021 = Accuracy_score.y.y)

#' 3-week ahead 
#' 2010-2017 traingin, 2018 testing 
indi18_three <- compare_accuracy_indi_fview(fluView_incidence,us_states,10,0,7,3,2014)

indi18_three_df <- accuracy_score_fview(indi18_three,us_states)

#' 2011-2018 traingin, 2019 testing 
indi19_three <- compare_accuracy_indi_fview(fluView_incidence,us_states,10,1,7,3,2014)

indi19_three_df <- accuracy_score_fview(indi19_three,us_states)

#' 2012-2019 traingin, 2020 testing 
indi20_three <- compare_accuracy_indi_fview(fluView_incidence,us_states,10,2,7,3,c(2014, 2020))

indi20_three_df <- accuracy_score_fview(indi20_three,us_states)

#' 2013-2020 traingin, 2021 testing 
indi21_three <- compare_accuracy_indi_fview(fluView_incidence,us_states,10,3,7,3,c(2014, 2020))

indi21_three_df <- accuracy_score_fview(indi21_three,us_states)

indi_three_df <- full_join(indi18_three_df, indi19_three_df, by = "States") %>% 
  full_join(., indi20_three_df, by = "States") %>% 
  full_join(., indi21_three_df, by = "States") %>% 
  rename(Accuracy_score_2018 = Accuracy_score.x,
         Accuracy_score_2019 = Accuracy_score.y,
         Accuracy_score_2020 = Accuracy_score.x.x,
         Accuracy_score_2021 = Accuracy_score.y.y)

#' 4-week ahead 
#' 2010-2017 traingin, 2018 testing 
indi18_four <- compare_accuracy_indi_fview(fluView_incidence,us_states,10,0,7,4,2014)

indi18_four_df <- accuracy_score_fview(indi18_four,us_states)

#' 2011-2018 traingin, 2019 testing 
indi19_four <- compare_accuracy_indi_fview(fluView_incidence,us_states,10,1,7,4,2014)

indi19_four_df <- accuracy_score_fview(indi19_four,us_states)

#' 2012-2019 traingin, 2020 testing 
indi20_four <- compare_accuracy_indi_fview(fluView_incidence,us_states,10,2,7,4,c(2014, 2020))

indi20_four_df <- accuracy_score_fview(indi20_four,us_states)

#' 2013-2020 traingin, 2021 testing 
indi21_four <- compare_accuracy_indi_fview(fluView_incidence,us_states,10,3,7,4,c(2014, 2020))

indi21_four_df <- accuracy_score_fview(indi21_four,us_states)

indi_four_df <- full_join(indi18_four_df, indi19_four_df, by = "States") %>% 
  full_join(., indi20_four_df, by = "States") %>% 
  full_join(., indi21_four_df, by = "States") %>% 
  rename(Accuracy_score_2018 = Accuracy_score.x,
         Accuracy_score_2019 = Accuracy_score.y,
         Accuracy_score_2020 = Accuracy_score.x.x,
         Accuracy_score_2021 = Accuracy_score.y.y)


indi_stats_df <- list(indi_one_df,
                      indi_two_df,
                      indi_three_df,
                      indi_four_df)
saveRDS(indi_stats_df, "./saved_objects/indi_state_forecasting.rds")

#' overall accuracy 
overall_accuracy_xgb <- data.frame(nWeek_ahead = c("1-week", "2-week", "3-week", "4-week"),
                               year2018 = c(sum(indi18_one_df$Accuracy_score)/dim(indi18_one_df)[1],
                                            sum(indi18_two_df$Accuracy_score)/dim(indi18_two_df)[1],
                                            sum(indi18_three_df$Accuracy_score)/dim(indi18_three_df)[1],
                                            sum(indi18_four_df$Accuracy_score)/dim(indi18_four_df)[1]),
                               year2019 = c(sum(indi19_one_df$Accuracy_score)/dim(indi19_one_df)[1],
                                            sum(indi19_two_df$Accuracy_score)/dim(indi19_two_df)[1],
                                            sum(indi19_three_df$Accuracy_score)/dim(indi19_three_df)[1],
                                            sum(indi19_four_df$Accuracy_score)/dim(indi19_four_df)[1]),
                               year2020 = c(sum(indi20_one_df$Accuracy_score)/dim(indi20_one_df)[1],
                                            sum(indi20_two_df$Accuracy_score)/dim(indi20_two_df)[1],
                                            sum(indi20_three_df$Accuracy_score)/dim(indi20_three_df)[1],
                                            sum(indi20_four_df$Accuracy_score)/dim(indi20_four_df)[1]),
                               year2021 = c(sum(indi21_one_df$Accuracy_score)/dim(indi21_one_df)[1],
                                            sum(indi21_two_df$Accuracy_score)/dim(indi21_two_df)[1],
                                            sum(indi21_three_df$Accuracy_score)/dim(indi21_three_df)[1],
                                            sum(indi21_four_df$Accuracy_score)/dim(indi21_four_df)[1]))
write.csv(overall_accuracy_xgb, "./saved_objects/overall_accuracy_xgb.csv", row.names = FALSE)

######## Baseline score ########
#' Historical average 
hist_one <- compare_accuracy_hist_fview(fluView_incidence,us_states,10,1,2014)
hist_two <- compare_accuracy_hist_fview(fluView_incidence,us_states,10,2,2014)
hist_three <- compare_accuracy_hist_fview(fluView_incidence,us_states,10,3,c(2014, 2020))
hist_four <- compare_accuracy_hist_fview(fluView_incidence,us_states,10,4,c(2014, 2020))


#' Null model
null_one <- compare_accuracy_null(fluView_incidence,us_states,10,1,2014)
null_two <- compare_accuracy_null(fluView_incidence,us_states,10,2,2014)
null_three <-compare_accuracy_null(fluView_incidence,us_states,10,3,c(2014, 2020))
null_four <-compare_accuracy_null(fluView_incidence,us_states,10,4,c(2014, 2020))

baseline_score <- data.frame(nWeek_ahead = c("1-week", "2-week", "3-week", "4-week"),
                             hist_score = c(hist_one$score, hist_two$score, hist_three$score, hist_four$score),
                             null_score = c(null_one$score, null_two$score, null_three$score, null_four$score),
                             Average = c(mean(c(hist_one$score, null_one$score)),
                                         mean(c(hist_two$score, null_two$score)),
                                         mean(c(hist_three$score, null_three$score)),
                                         mean(c(hist_four$score, null_four$score))))


write.csv(baseline_score, "baseline_score.csv", row.names = FALSE)

#' plot output
overall_accuracy_xgb_baseline <- cbind(overall_accuracy_xgb, baseline_score$Average)
colnames(overall_accuracy_xgb_baseline)[6] <- "Baseline_score"

accuracy_comparison <- list(overall_accuracy_xgb_baseline, 
                            overall_accuracy_xgb,
                            baseline_score)
saveRDS(accuracy_comparison, "./saved_objects/accuracy_comparison.rds")
