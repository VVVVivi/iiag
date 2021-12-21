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
library("rasterVis") # heat plot
library("cdcfluview")
library("hrbrthemes")
library("tidyverse")
library("dplyr")
library("ggplot2")

# current verison
packageVersion("cdcfluview")

#' Obtaine survilliance ILI data by states 
#' 53 states in total
fview_ILINet <- ilinet(region = "state")


source("./GitHub_Vivi/iiag/forecasting_vivi/fluView_funcs_vivi.R")
source("./GitHub_Vivi/iiag/forecasting_vivi/vivi_funcs.R")
source("./GitHub_Vivi/iiag/forecasting_vivi/gbm_complex_funcs.R")
source("./GitHub_Vivi/iiag/forecasting_vivi/fluView_funcs_vivi.R")

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
fview_incidence <- extract.incidence.fview(fview_ILINet,
                                             sel_states = unique(fview_ILINet$region),
                                             minYear = 2010,
                                             maxYear = 2021,
                                             c(2014,2020))
ncol(fview_incidence) # 53 states in total

minprop <- 0.5

#' None of satets contain entries between 2010-01 and 2010-39, so delete these rows before calculate the avaibility
#' of dataset
fview_incidence <- fview_incidence[-c(1:39),]

#' Florida and Commonwealth of the Northern Mariana Islands are excluded.
us_states <- names(which(colSums(is.na(fview_incidence))/dim(fview_incidence)[1]<minprop)) # 53 states left

fview_incidence <- extract.incidence.fview(fview_ILINet,
                                             sel_states = us_states,
                                             minYear = 2010,
                                             maxYear = 2021,
                                             c(2014,2020))


#' Check if the data of remained states are eligible for xgboost
#' check the data availablity in each year
#' exclude states which do not have at least five consecutive weeks data in any year or do not have 
#' entries in certain years.
state_year <- NULL
for (i in 1:length(us_states)){
  tmp <- duration.fview(fview_incidence,us_states[i],1,2010,2021,c(2014,2020))
  state_year <- rbind(state_year, tmp)
}

state_year <- as.data.frame(state_year)
colnames(state_year) <- c("State","2010","2011","2012","2013","2014","2015",
                            "2016","2017","2018","2019","2020", "2021", "start_year","end_year")
state_year$end_year <- as.numeric(as.character(state_year$end_year))
state_year$start_year <- as.numeric(as.character(state_year$start_year))
rownames(state_year) <- c(1:nrow(state_year))

state_no20Or10 <- state_year$State[which(state_year$`2021`=="No" | state_year$`2010`=="No")]
                                            
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
fview_incidence2 <- extract.incidence.fview(fview_ILINet,
                                             us_xgb,
                                             minYear = 2010,
                                             maxYear = 2021,
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
Alabama <- compare_accuracy_indi_fview(fview_incidence2,us_xgb[1],10,1,6,2,2014)

#' Individual states results, 2018 testing
indi18_one <- NULL
for (i in 1:length(us_xgb)){
  tmp <- compare_accuracy_indi_fview(fview_incidence2,us_xgb[i],10,0,7,1,2014)
  indi18_one <- append(indi18_one, tmp)
}

indi18_one_df <- accuracy_score_fview(indi18_one,us_xgb)

#' Individual states results, 2019 testing
indi19_one <- NULL
for (i in 1:length(us_xgb)){
  tmp <- compare_accuracy_indi_fview(fview_incidence2,us_xgb[i],10,1,7,1,2014)
  indi19_one <- append(indi19_one, tmp)
}

indi19_one_df <- accuracy_score_fview(indi19_one,us_xgb)

#' Individual states results, 2020 testing
indi20_one <- NULL
for (i in 1:length(us_xgb)){
  tmp <- compare_accuracy_indi_fview(fview_incidence2,us_xgb[i],10,2,7,1,2014)
  indi20_one <- append(indi20_one, tmp)
}

indi20_one_df <- accuracy_score_fview(indi20_one,us_xgb)

#' Individual states results, 2021 testing
indi21_one <- NULL
for (i in 1:length(us_xgb)){
  tmp <- compare_accuracy_indi_fview(fview_incidence2,us_xgb[i],10,3,7,1,2014)
  indi21_one <- append(indi21_one, tmp)
}

indi21_one_df <- accuracy_score_fview(indi21_one,us_xgb)


indi_one_df <- full_join(indi18_one_df, indi19_one_df, by = "State") %>% 
  full_join(., indi20_one_df, by = "State") %>% 
  full_join(., indi21_one_df, by = "State") %>% 
  rename(Accuracy_score_2018 = Accuracy_score.x,
         Accuracy_score_2019 = Accuracy_score.y,
         Accuracy_score_2020 = Accuracy_score.x.x,
         Accuracy_score_2021 = Accuracy_score.y.y)

write.csv(indi_one_df, "indi_one_df.csv")


#' Apart from looking at forecast performance of each state, also check the total performance
#' over all states

#' One-week ahead forecast:
#' 2010-2017 training, 2018 testing
acc18_one <- compare_accuracy_fview(flu_data = fview_incidence2,
                                    state_list = us_xgb,
                                    num_category =  10,
                                    train_num_start = 0,
                                    train_num_end = 7,
                                    nWeek_ahead = 1,
                                    yr53week =2014)
acc18_two <- compare_accuracy_fview(flu_data = fview_incidence2,
                                    state_list = us_xgb,
                                    num_category =  10,
                                    train_num_start = 0,
                                    train_num_end = 7,
                                    nWeek_ahead = 2,
                                    yr53week =2014)

acc18_three <- compare_accuracy_fview(flu_data = fview_incidence2,
                                    state_list = us_xgb,
                                    num_category =  10,
                                    train_num_start = 0,
                                    train_num_end = 7,
                                    nWeek_ahead = 3,
                                    yr53week =2014)

acc18_four <- compare_accuracy_fview(flu_data = fview_incidence2,
                                      state_list = us_xgb,
                                      num_category =  10,
                                      train_num_start = 0,
                                      train_num_end = 7,
                                      nWeek_ahead = 4,
                                      yr53week =2014)


#' 2011-2018 training, 2019 testing
acc19_one <- compare_accuracy_fview(flu_data = fview_incidence2,
                                         state_list = us_xgb,
                                         num_category =  10,
                                         train_num_start = 1,
                                         train_num_end = 7,
                                         nWeek_ahead = 1,
                                         yr53week = 2014)

acc19_two <- compare_accuracy_fview(flu_data = fview_incidence2,
                                    state_list = us_xgb,
                                    num_category =  10,
                                    train_num_start = 1,
                                    train_num_end = 7,
                                    nWeek_ahead = 2,
                                    yr53week = 2014)

acc19_three <- compare_accuracy_fview(flu_data = fview_incidence2,
                                    state_list = us_xgb,
                                    num_category =  10,
                                    train_num_start = 1,
                                    train_num_end = 7,
                                    nWeek_ahead = 3,
                                    yr53week = 2014)

acc19_four <- compare_accuracy_fview(flu_data = fview_incidence2,
                                    state_list = us_xgb,
                                    num_category =  10,
                                    train_num_start = 1,
                                    train_num_end = 7,
                                    nWeek_ahead = 4,
                                    yr53week = 2014)


#' 2012-2019 training, 2020 testing
acc20_one <- compare_accuracy_fview(flu_data = fview_incidence2,
                                    state_list = us_xgb,
                                    num_category =  10,
                                    train_num_start = 2,
                                    train_num_end = 7,
                                    nWeek_ahead = 1,
                                    yr53week = 2014)

acc20_two <- compare_accuracy_fview(flu_data = fview_incidence2,
                                    state_list = us_xgb,
                                    num_category =  10,
                                    train_num_start = 2,
                                    train_num_end = 7,
                                    nWeek_ahead = 2,
                                    yr53week = 2014)

acc20_three <- compare_accuracy_fview(flu_data = fview_incidence2,
                                    state_list = us_xgb,
                                    num_category =  10,
                                    train_num_start = 2,
                                    train_num_end = 7,
                                    nWeek_ahead = 3,
                                    yr53week = 2014)

acc20_four <- compare_accuracy_fview(flu_data = fview_incidence2,
                                    state_list = us_xgb,
                                    num_category =  10,
                                    train_num_start = 2,
                                    train_num_end = 7,
                                    nWeek_ahead = 4,
                                    yr53week = 2014)

#' 2013-2020 training, 2021 testing
acc21_one <- compare_accuracy_fview(flu_data = fview_incidence2,
                                    state_list = us_xgb,
                                    num_category =  10,
                                    train_num_start = 3,
                                    train_num_end = 7,
                                    nWeek_ahead = 1,
                                    yr53week = 2014)

acc21_two <- compare_accuracy_fview(flu_data = fview_incidence2,
                                    state_list = us_xgb,
                                    num_category =  10,
                                    train_num_start = 3,
                                    train_num_end = 7,
                                    nWeek_ahead = 2,
                                    yr53week = 2014)

acc21_three <- compare_accuracy_fview(flu_data = fview_incidence2,
                                    state_list = us_xgb,
                                    num_category =  10,
                                    train_num_start = 3,
                                    train_num_end = 7,
                                    nWeek_ahead = 3,
                                    yr53week = 2014)

acc21_four <- compare_accuracy_fview(flu_data = fview_incidence2,
                                    state_list = us_xgb,
                                    num_category =  10,
                                    train_num_start = 3,
                                    train_num_end = 7,
                                    nWeek_ahead = 4,
                                    yr53week = 2014)

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

# baseline_score <- c(round((hist_one$score + null_one$score)/2, 3),
#                     round((hist_two$score + null_two$score)/2, 3),
#                     round((hist_three$score + null_three$score)/2, 3),
#                     round((hist_four$score + null_four$score)/2, 3))

baseline_score <- c(round(hist_one$score, 3),
                    round(hist_two$score, 3),
                    round(hist_three$score, 3),
                    round(hist_four$score, 3))

#' plot of comparing accuracy beteen different years.
accuracy_xgb <- cbind(c(1:4),
                      c(acc18_one$score, acc18_two$score, acc18_three$score, acc18_four$score),
                      c(acc19_one$score, acc19_two$score, acc19_three$score, acc19_four$score),
                      c(acc20_one$score, acc20_two$score, acc20_three$score, acc20_four$score),
                      c(acc21_one$score, acc21_two$score, acc21_three$score, acc21_four$score),
                      baseline_score) %>% 
  as.data.frame()
colnames(accuracy_xgb) <- c("nWeek_ahead","year2018","year2019","year2020","year2021","Baseline_score")
write.csv(accuracy_xgb, "./OneDrive - Imperial College London/Conference/accuracy_xgb.csv")

accCompare <- ggplot(accuracy_xgb, aes(x = nWeek_ahead,group=1))+
  geom_point(aes(y = year2018, color = "year2018"),size=3)+
  geom_line(aes(y = year2018, color = "year2018"),size=1)+
  geom_point(aes(y = year2019, color = "year2019"),size=3)+
  geom_line(aes(y = year2019, color = "year2019"),size=1)+
  geom_point(aes(y = year2020, color = "year2020"),size=3)+
  geom_line(aes(y = year2020, color = "year2020"),size=1)+
  geom_point(aes(y = year2021, color = "year2021"),size=3)+
  geom_line(aes(y = year2021, color = "year2021"),size=1)+
  geom_point(aes(y = Baseline_score, color = "Baseline_score"),size=3)+
  geom_line(aes(y = Baseline_score, color = "Baseline_score"),size=1)+
  ylim(0.4,0.85)+
  labs(y = "Forecast accuracy",
       x = "n-week ahead",
       colour = "Forecast year")+
  theme_bw(base_size = 18)

accCompare <- accCompare + theme(# legend.title = element_text(size = 18),
                   # legend.text = element_text(size = 18),
                   legend.position = c(0.85, 0.85))

pdf(file = "./OneDrive - Imperial College London/Conference/accCompare.pdf",
    width = 12, height = 8)
accCompare
dev.off()

ggsave(accCompare, filename = "./OneDrive - Imperial College London/Conference/accCompare.png",
       device = "png", width = 12, height = 10, scale = 1)


#' Heat Plot

#' 2018 testing, one-week ahead
for (i in 1:length(us_xgb)){
  tem.pred <- xgboost.model.pred.fview(fview_incidence2,us_states[i],10,0,7,1,2014)
  tem.freq <- freq_table(tem.pred,10)
  heat_plot(tem.freq,us_xgb[i],indi18_one_df$Accuracy_score[i])
}

#' heat plots of predictions over all states in 2016, 2017, 2018 and 2019
acc18_one_freq <- freq_table_fview(acc18_one,10)
heat_plot_fview(acc18_one_freq,c(0,10,20,50,100,400,800,1200,1600,2000),
                "US",acc18_one[[2]])

acc19_one_freq <- freq_table_fview(acc19_one,10)
heat_plot_fview(acc19_one_freq,c(0,10,20,50,100,400,800,1200,1600,2000),
                "US",acc19_one[[2]])

acc20_one_freq <- freq_table_fview(acc20_one,10)
heat_plot_fview(acc20_one_freq,c(0,10,20,50,100,400,800,1200,1600,2000),
                "US",acc20_one[[2]])

acc21_one_freq <- freq_table_fview(acc21_one,10)
heatPlot21_oneWeek <- heat_plot_fview(acc21_one_freq,c(0,10,20,50,100,400,800,1200,1600,2000),
                "US",acc21_one[[2]])

pdf(file = "./OneDrive - Imperial College London/Conference/heatPlot21_oneWeek.pdf",
    width = 12, height = 8)
heatPlot21_oneWeek
dev.off()



#' marginal heat plots of predictions over all states in 2016, 2017, 2018 and 2019
heat_plot_fview_marginal(acc18_one_freq,"US",acc18_one[[2]])

heat_plot_fview_marginal(acc19_one_freq,"US",acc19_one[[2]])


heat_plot_fview_marginal(acc20_one_freq,"US",acc20_one[[2]])


heat_plot_fview_marginal(acc21_one_freq,"US",acc21_one[[2]])

rawData_TS_fview_plot <- function(data, region){
  require(ggplot2)
  require(grid)
  require(aweek)
  
  count <- data[, which(colnames(data)==region)]
  week_time <- rownames(data)[-which(is.na(count))]
  count <- count[-(which(is.na(count)))]
  category <- cut_interval(count, 10)
  raw_data <- cbind(week_time, count, category)
  
  raw_data <- as.data.frame(raw_data)
  colnames(raw_data) <- c('week_time','count','category')
  
  # convert discrete week time to contious time series 
  raw_data$weekTS <- gsub("-", "-W", raw_data$week_time, fixed = TRUE)
  raw_data$weekTS <- week2date(raw_data$weekTS)
  raw_data$count <- as.numeric(as.character(raw_data$count))
  raw_data$category <- as.numeric(as.character(raw_data$category))
  
  # first plot the time series of numeric incidence
  grobTS <- grobTree(textGrob(region, x=0.85,  y=0.9, hjust=0,
                              gp=gpar(col="black", fontsize=14, fontface="bold")))
  p1 <- ggplot(raw_data, aes(x = weekTS))
  p1 <- p1 + geom_line(aes(y = count),colour = "salmon",size=1)
  p1 <- p1 + scale_y_continuous(expand = c(0, 0))
  # p1 <- p1 + scale_x_continuous(breaks = c(2010:2018))
  # p1 <- p1 + scale_colour_manual(values = "salmon")

  p1 <- p1 + labs(y = "Incidence*1,000",
                  x = "Year",
                  # colour = "Obsevation",
                  title = "Time series of numeric observations of incidence")
  p1 <- p1 + theme_bw() + 
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
    annotation_custom(grobTS)
  
  # secondly plot the time series of categorical incidence
  p2 <- ggplot(raw_data, aes(x = weekTS))
  p2 <- p2 + geom_line(aes(y = category),colour = "darkslategray2",size=1)
  # p2 <- p2 + scale_x_continuous(breaks = c(2010:2018))
  p2 <- p2 + scale_y_continuous(breaks = c(1:10),expand = c(0, 0))
  # p2 <- p2 + scale_colour_manual(values = "darkslategray2")
  p2 <- p2 + labs(y = "Incidence",
                  x = "Year",
                  # colour = "Obsevation",
                  title = "Time series of categorical observations of incidence")
  p2 <- p2 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  grid.newpage()
  grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))
  
}

ts_plot_NY <- rawData_TS_fview_plot(fview_incidence, "New York")
ggsave(rawData_TS_fview_plot(fview_incidence, "New York"), filename = "./OneDrive - Imperial College London/Conference/newYork_TS_plot.png",
       device = "png", width = 12, height = 4, scale = 1)

pdf(file = "./OneDrive - Imperial College London/Conference/newYork_TS_plot.pdf",
    width = 16, height = 8)
rawData_TS_fview_plot(fview_incidence, "New York")
dev.off()
#### measure the performance of ML model by CDC 