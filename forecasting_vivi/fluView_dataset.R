#' Make a note of when the report was generated.
Sys.time()

#' As always, remove all objects fromt the workspace before starting.
rm(list = ls(all = TRUE))

# CRAN
install.packages("cdcfluview")
# master branch
devtools::install_git("https://sr.ht/~hrbrmstr/cdcfluview")
devtools::install_git("https://gitlab.com/hrbrmstr/cdcfluview")
devtools::install_github("hrbrmstr/cdcfluview")

library("knitr")
library("xgboost")
library("stringr")

library("cdcfluview")
library("hrbrthemes")
library("tidyverse")
library("ggplot2")

# current verison
packageVersion("cdcfluview")

#' Obtaine survilliance ILI data by states 
#' 53 states in total
fview_ILINet <- ilinet(region = "state")


source("E:/Imperial/iiag/forecasting_vivi/fluView_funcs_vivi.R")
source("E:/Imperial/iiag/forecasting_vivi/vivi_funcs.R")
source("E:/Imperial/iiag/forecasting_vivi/gbm_complex_funcs.R")
source("C:/Users/hw3616/Desktop/Imperial/Project1_Forecasting/Project_Coding/iiag/forecasting_vivi/fluView_funcs_vivi.R")

#' U.S. Outpatient Influenza-like Illness Surveillance Network (ILINet) consist of 
#' information on outpatient visits to health care providers for influenza-like illness
#' and the number of outpatinent confirmed as ILI.

#' find out why extract.incidence.fluView stop at 2014.
#' Set up a dataframe that only contains two states and time period is between 2014-49 and 2019-52.
two_states <- fview_ILINet[which(fview_ILINet$region == "Alabama" | fview_ILINet$region == "Alaska"),]
two_states <- two_states[-c(1:434),]
two_states <- two_states[order(two_states$region),]

two_states_inci <- extract.incidence.fluView(two_states,unique(two_states$region),2010,2020,c(2014,2020))


#' Extract incidence data by states.
# region <- unique(fview_ILINet$REGION)

#' Shape 
fview_incidence <- extract.incidence.fluView(fview_ILINet,
                                             sel_states = unique(fview_ILINet$region),
                                             minYear = 2010,
                                             maxYear = 2019,
                                             c(2014,2020))
ncol(fview_incidence) # 53 states in total

minprop <- 0.5

#' None of satets contain entries between 2010-01 and 2010-39, so delete these rows before calculate the avaibility
#' of dataset
fview_incidence <- fview_incidence[-c(1:39),]

#' Florida and Commonwealth of the Northern Mariana Islands are excluded.
us_states <- names(which(colSums(is.na(fview_incidence))/dim(fview_incidence)[1]<minprop)) # 53 states left

fview_incidence <- extract.incidence.fluView(fview_ILINet,
                                             sel_states = us_states,
                                             minYear = 2010,
                                             maxYear = 2020,
                                             c(2014,2020))


#' Check if the data of remained states are eligible for xgboost
#' check the data availablity in each year
#' exclude states which do not have at least five consecutive weeks data in any year or do not have 
#' entries in certain years.
state_year <- NULL
for (i in 1:length(us_states)){
  tmp <- duration.fview(fview_incidence,us_states[i],1,2010,2020,c(2014,2020))
  state_year <- rbind(state_year, tmp)
}

state_year <- as.data.frame(state_year)
colnames(state_year) <- c("State","2010","2011","2012","2013","2014","2015",
                            "2016","2017","2018","2019","2020", "start_year","end_year")
state_year$end_year <- as.numeric(as.character(state_year$end_year))
state_year$start_year <- as.numeric(as.character(state_year$start_year))
rownames(state_year) <- c(1:nrow(state_year))

state_no20Or10 <- state_year$State[which(state_year$`2020`=="No" | state_year$`2010`=="No")]
                                            
state_no10 <- state_year$State[which(state_year$`2010`=="No")]

state_no20Or10 <- us_states[which(us_states %in% state_no20Or10)]
# Virgin Islands starts from 2011 and Puerto Rico starts from 2013.


#' Obtain the list of states will be maintained for xgboost analysis.
#' There are 51 states are maintained.
us_xgb <- us_states
for (i in 1:length(state_no20Or10)){
  index <- which(us_xgb == state_no20Or10[i])
  us_xgb <- us_xgb[-(index)]
}
us_xgb

#' Check if left states have less than 5 weeks data in a year because I will do the 4-week ahead foreacast which
#' requires data of 5 consective weeks
#' Exclude states whose datasets are uneligible to do the 4-week ahead forecast
#' So far I (Vivi) did this manually, will write a function to check automatically.



# extract incidence of eligible states.
fview_incidence2 <- extract.incidence.fluView(fview_ILINet,
                                             us_xgb,
                                             minYear = 2010,
                                             maxYear = 2020,
                                             c(2014,2020))
ncol(fview_incidence2)

length1 <- c()
for (i in 1:length(us_xgb)){
  a <- adjust.data.size.fview(fview_incidence2, us_xgb[i], 10,1,c(2014,2020))
  tmp <- nrow(a)
  length1 <- append(length1,tmp)
  
}

length2 <- c()
for (i in 1:length(us_xgb)){
  a <- adjust.data.size.fview(fview_incidence2, us_xgb[i], 10,2,c(2014,2020))
  tmp <- nrow(a)
  length2 <- append(length2,tmp)
}

length3 <- c()
for (i in 1:length(us_xgb)){
  a <- adjust.data.size.fview(fview_incidence2, us_xgb[i], 10,3,c(2014,2020))
  tmp <- nrow(a)
  length3 <- append(length3,tmp)
}

length4 <- c()
for (i in 1:length(us_xgb)){
  a <- adjust.data.size.fview(fview_incidence2, us_xgb[i], 10,4,c(2014,2020))
  tmp <- nrow(a)
  length4 <- append(length4,tmp)
}
 
#' XGB model
#' gbm_complex_fview: shape data structure and add features
#' First of all, look at one state to see if compare_accuracy_indi_fview() works

#' Take Alabama as an example. 
Alabama <- compare_accuracy_indi_fview(fview_incidence2,us_xgb[1],10,1,5,2,2014)

#' Individual states results, 2016 testing
indi16_one <- NULL
for (i in 1:length(us_xgb)){
  tmp <- compare_accuracy_indi_fview(fview_incidence2,us_xgb[i],10,0,5,1,2014)
  indi16_one <- append(indi16_one, tmp)
}

indi16_one_df <- accuracy_score_fview(indi16_one,us_xgb)

#' Individual states results, 2017 testing
indi17_one <- NULL
for (i in 1:length(us_xgb)){
  tmp <- compare_accuracy_indi_fview(fview_incidence2,us_xgb[i],10,1,5,1,2014)
  indi17_one <- append(indi17_one, tmp)
}

indi17_one_df <- accuracy_score_fview(indi17_one,us_xgb)

#' Individual states results, 2018 testing
indi18_one <- NULL
for (i in 1:length(us_xgb)){
  tmp <- compare_accuracy_indi_fview(fview_incidence2,us_xgb[i],10,2,5,1,2014)
  indi18_one <- append(indi18_one, tmp)
}

indi18_one_df <- accuracy_score_fview(indi18_one,us_xgb)

#' Individual states results, 2019 testing
indi19_one <- NULL
for (i in 1:length(us_xgb)){
  tmp <- compare_accuracy_indi_fview(fview_incidence2,us_xgb[i],10,3,5,1,2014)
  indi19_one <- append(indi19_one, tmp)
}

indi19_one_df <- accuracy_score_fview(indi19_one,us_xgb)


indi_one_df <- merge(merge(merge(indi16_one_df,indi17_one_df, by.x = "State", by.y = "State"),indi18_one_df, 
                     by.x="State", by.y="State"), indi19_one_df, by.x="State", by.y="State")
colnames(indi_one_df) <- c("State","2016","2017","2018","2019")
write.csv(indi_one_df, "indi_one_df.csv")


#' Apart from looking at forecast performance of each state, also check the total performance
#' over all states

#' One-week ahead forecast:
#' 2010-2015 training, 2016 testing
acc16_one <- compare_accuracy_fview(flu_data = fview_incidence2,
                                    state_list = us_xgb,
                                    num_category =  10,
                                    train_num_start = 0,
                                    train_num_end = 5,
                                    nWeek_ahead = 1,
                                    yr53week =2014)
acc16_two <- compare_accuracy_fview(flu_data = fview_incidence2,
                                    state_list = us_xgb,
                                    num_category =  10,
                                    train_num_start = 0,
                                    train_num_end = 5,
                                    nWeek_ahead = 2,
                                    yr53week =2014)

acc16_three <- compare_accuracy_fview(flu_data = fview_incidence2,
                                    state_list = us_xgb,
                                    num_category =  10,
                                    train_num_start = 0,
                                    train_num_end = 5,
                                    nWeek_ahead = 3,
                                    yr53week =2014)

acc16_four <- compare_accuracy_fview(flu_data = fview_incidence2,
                                      state_list = us_xgb,
                                      num_category =  10,
                                      train_num_start = 0,
                                      train_num_end = 5,
                                      nWeek_ahead = 4,
                                      yr53week =2014)


#' 2011-2016 training, 2017 testing
acc17_one <- compare_accuracy_fview(flu_data = fview_incidence2,
                                         state_list = us_xgb,
                                         num_category =  10,
                                         train_num_start = 1,
                                         train_num_end = 5,
                                         nWeek_ahead = 1,
                                         yr53week = 2014)

acc17_two <- compare_accuracy_fview(flu_data = fview_incidence2,
                                    state_list = us_xgb,
                                    num_category =  10,
                                    train_num_start = 1,
                                    train_num_end = 5,
                                    nWeek_ahead = 2,
                                    yr53week = 2014)

acc17_three <- compare_accuracy_fview(flu_data = fview_incidence2,
                                    state_list = us_xgb,
                                    num_category =  10,
                                    train_num_start = 1,
                                    train_num_end = 5,
                                    nWeek_ahead = 3,
                                    yr53week = 2014)

acc17_four <- compare_accuracy_fview(flu_data = fview_incidence2,
                                    state_list = us_xgb,
                                    num_category =  10,
                                    train_num_start = 1,
                                    train_num_end = 5,
                                    nWeek_ahead = 4,
                                    yr53week = 2014)


#' 2012-2017 training, 2018 testing
acc18_one <- compare_accuracy_fview(flu_data = fview_incidence2,
                                    state_list = us_xgb,
                                    num_category =  10,
                                    train_num_start = 2,
                                    train_num_end = 5,
                                    nWeek_ahead = 1,
                                    yr53week = 2014)

acc18_two <- compare_accuracy_fview(flu_data = fview_incidence2,
                                    state_list = us_xgb,
                                    num_category =  10,
                                    train_num_start = 2,
                                    train_num_end = 5,
                                    nWeek_ahead = 2,
                                    yr53week = 2014)

acc18_three <- compare_accuracy_fview(flu_data = fview_incidence2,
                                    state_list = us_xgb,
                                    num_category =  10,
                                    train_num_start = 2,
                                    train_num_end = 5,
                                    nWeek_ahead = 3,
                                    yr53week = 2014)

acc18_four <- compare_accuracy_fview(flu_data = fview_incidence2,
                                    state_list = us_xgb,
                                    num_category =  10,
                                    train_num_start = 2,
                                    train_num_end = 5,
                                    nWeek_ahead = 4,
                                    yr53week = 2014)

#' 2013-2018 training, 2019 testing
acc19_one <- compare_accuracy_fview(flu_data = fview_incidence2,
                                    state_list = us_xgb,
                                    num_category =  10,
                                    train_num_start = 3,
                                    train_num_end = 5,
                                    nWeek_ahead = 1,
                                    yr53week = 2014)

acc19_two <- compare_accuracy_fview(flu_data = fview_incidence2,
                                    state_list = us_xgb,
                                    num_category =  10,
                                    train_num_start = 3,
                                    train_num_end = 5,
                                    nWeek_ahead = 2,
                                    yr53week = 2014)

acc19_three <- compare_accuracy_fview(flu_data = fview_incidence2,
                                    state_list = us_xgb,
                                    num_category =  10,
                                    train_num_start = 3,
                                    train_num_end = 5,
                                    nWeek_ahead = 3,
                                    yr53week = 2014)

acc19_four <- compare_accuracy_fview(flu_data = fview_incidence2,
                                    state_list = us_xgb,
                                    num_category =  10,
                                    train_num_start = 3,
                                    train_num_end = 5,
                                    nWeek_ahead = 4,
                                    yr53week = 2014)

#' plot of comparing accuracy beteen different years.
accuracy_xgb <- cbind(c(1:4),
                      c(0.833,	0.801, 0.784,	0.765),
                      c(0.755,	0.725,	0.691,	0.676),
                      c(0.691,	0.653,	0.624,	0.61),
                      c(0.624,	0.578,	0.525,	0.495),
                      c(0.811,	0.756,	0.712,	0.677)) %>% as.data.frame()
colnames(accuracy_xgb) <- c("nWeek_ahead","year_2016","year_2017","year_2018","year_2019","Baseline_score")


ggplot(accuracy_xgb, aes(x = nWeek_ahead,group=1))+
  geom_point(aes(y = year_2016, color = "year_2016"),size=3)+
  geom_line(aes(y = year_2016, color = "year_2016"),size=1)+
  geom_point(aes(y = year_2017, color = "year_2017"),size=3)+
  geom_line(aes(y = year_2017, color = "year_2017"),size=1)+
  geom_point(aes(y = year_2018, color = "year_2018"),size=3)+
  geom_line(aes(y = year_2018, color = "year_2018"),size=1)+
  geom_point(aes(y = year_2019, color = "year_2019"),size=3)+
  geom_line(aes(y = year_2019, color = "year_2019"),size=1)+
  geom_point(aes(y = Baseline_score, color = "Baseline_score"),size=3)+
  geom_line(aes(y = Baseline_score, color = "Baseline_score"),size=1)+
  ylim(0.4,0.85)+
  labs(y = "Forecast accuracy",
       x = "n-week ahead",
       colour = "forecast year")+
  theme_bw()

#' Heat Plot

#' 2016 testing, one-week ahead
for (i in 1:length(us_xgb)){
  tem.pred <- xgboost.model.pred.fview(fview_incidence2,us_states[i],10,0,5,1,2014)
  tem.freq <- freq_table(tem.pred,10)
  heat_plot(tem.freq,us_xgb[i],indi16_one_df$Accuracy_score[i])
}

#' heat plots of predictions over all states in 2016, 2017, 2018 and 2019
acc16_one_freq <- freq_table_fview(acc16_one,10)
heat_plot_fview(acc16_one_freq,c(0,10,20,50,100,400,800,1200,1600,2000),
                "US",acc16_one[[2]])

acc17_one_freq <- freq_table_fview(acc17_one,10)
heat_plot_fview(acc17_one_freq,c(0,10,20,50,100,400,800,1200,1600,2000),
                "US",acc17_one[[2]])

acc18_one_freq <- freq_table_fview(acc18_one,10)
heat_plot_fview(acc18_one_freq,c(0,10,20,50,100,400,800,1200,1600,2000),
                "US",acc18_one[[2]])

acc19_one_freq <- freq_table_fview(acc19_one,10)
heat_plot_fview(acc19_one_freq,c(0,10,20,50,100,400,800,1200,1600,2000),
                "US",acc19_one[[2]])

#' marginal heat plots of predictions over all states in 2016, 2017, 2018 and 2019
heat_plot_fview_marginal(acc16_one_freq,"US",acc16_one[[2]])

heat_plot_fview_marginal(acc17_one_freq,"US",acc17_one[[2]])


heat_plot_fview_marginal(acc18_one_freq,"US",acc18_one[[2]])


heat_plot_fview_marginal(acc19_one_freq,"US",acc19_one[[2]])

#' Historical average and null model

# Historical model
hist_one <- compare_accuracy_hist_fview(fview_incidence2,us_xgb,10,1,2014)
hist_two <- compare_accuracy_hist_fview(fview_incidence2,us_xgb,10,2,2014)
hist_three <- compare_accuracy_hist_fview(fview_incidence2,us_xgb,10,3,2014)
hist_four <- compare_accuracy_hist_fview(fview_incidence2,us_xgb,10,4,2014)


# Null model
null_one <- compare_accuracy_null(fview_incidence2,us_xgb,10,1,2014)
null_two <- compare_accuracy_null(fview_incidence2,us_xgb,10,2,2014)
null_three <-compare_accuracy_null(fview_incidence2,us_xgb,10,3,2014)
null_four <-compare_accuracy_null(fview_incidence2,us_xgb,10,4,2014)
