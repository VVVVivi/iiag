
load.iiag.data.fluView <- function(flu_data) {
  
  ## Helper function to fix some header names to be used below
  fix_headers <- function(x){
    x <- as.character(x)
    if (length(unlist(strsplit(x," "))) > 1){
      if (length(unlist(strsplit(x, "-"))) > 1){
        x <- unlist(strsplit(x,"-"))
      }
      if (length(unlist(strsplit(x, ".", fixed = TRUE))) > 1){
        x <- unlist(strsplit(x, ".", fixed = TRUE))
      }
      
      if (length(unlist(strsplit(x, "%", fixed = TRUE))) > 1){
        x <- str_replace(x, "%", "PERCENTAGE OF")
        x <- unlist(strsplit(x, " "))
      }
      if (length(unlist(strsplit(x, " "))) > 1){
        x <- unlist(strsplit(x, " "))
      }
    }else{
      x <- x
    }
    x <- paste0(x, collapse = "_")
    x
  }
  ## Define the strings for all the files needed and read 
 
  fview_ILINet <- flu_data
  
  ## Fix names for fluView ILINet
  curnames <- unlist(fview_ILINet[1,], use.names=FALSE)
  newnames <- c()
  for (i in 1:length(curnames)){
    tmp <- fix_headers(curnames[i])
    newnames <- append(newnames,tmp)
  }
    
  names(fview_ILINet) <- newnames
  fview_ILINet <- fview_ILINet[-1,]
  
  fview_ILINet
}

fix_headers <- function(x){
  x <- as.character(x)
  if (length(unlist(strsplit(x," "))) > 1){
    if (length(unlist(strsplit(x, "-"))) > 1){
      x <- unlist(strsplit(x,"-"))
    }
    if (length(unlist(strsplit(x, ".", fixed = TRUE))) > 1){
      x <- unlist(strsplit(x, ".", fixed = TRUE))
    }
    
    if (length(unlist(strsplit(x, "%", fixed = TRUE))) > 1){
      x <- str_replace(x, "%", "PERCENTAGE OF")
      x <- unlist(strsplit(x, " "))
    }
    if (length(unlist(strsplit(x, " "))) > 1){
      x <- unlist(strsplit(x, " "))
    }
  }else{
    x <- x
  }
  x <- paste0(x, collapse = "_")
  x
}


#' Extract incidence from raw data
#' The output of this function is a data matrix in which each column represent weekly incidence of a state
#' from 2010-01 to 2020-53
extract.incidence.fview <- function(fluView_data,
                                      sel_states,
                                      # sel_ag,
                                      # sel_measure,
                                      minYear,
                                      maxYear,
                                      yr53week){
  ## reorder data by country alphabetically
  # fluView_data <- fluView_data[order(fluView_data$region),]
  
  ## Setup the week scale in a format consistent with the week format
  ## in the data and cope with 53-week years. Needs the list of 53 week years
  ## extending in both directions.
  ## Perhaps should have a few lines to get rid of data NAs and avoid a warning
  ## at the next line?
  fluView_data$yrweek  <- paste(fluView_data$year,sprintf("%02d",as.numeric(as.character(fluView_data$week))),sep="-")
  
  min(as.numeric(as.character(fluView_data$year)))
  yrs53Weeks <- yr53week
  currentYear <- minYear
  vecWeekScale <- NULL
  while (currentYear <= maxYear) {
    if (currentYear %in% yrs53Weeks) {
      max_week <- 53
    } else {
      max_week <- 52
    }
    vecWeekScale <- c(vecWeekScale,
                      paste(currentYear,sprintf("%02d",1:max_week),sep="-"))
    currentYear <- currentYear +1
  }
  
  ## Define the return matrix for the function
  sel_weeks <- vecWeekScale
  rtnmat <- matrix(data=NA,nrow=length(sel_weeks),ncol=length(sel_states))
  colnames(rtnmat) <- sel_states
  rownames(rtnmat) <- sel_weeks
 
  ## Start outer loop over the country codes
  for (cur_iso3 in sel_states) {
    
    ## Define criteria and subset the data
    crit1 <- (fluView_data$region == cur_iso3)
    # if(!("AGEGROUP_CODE" %in% colnames(dfId))) {
      # crit2 <- TRUE
    # } else {
      # crit2 <- (dfId$AGEGROUP_CODE %in% sel_ag)
    # }
    
    # crit3 <- (dfId$MEASURE_CODE %in% sel_measure)
    tmpdf <- fluView_data[crit1,]
    tmpdf <- tmpdf[order(tmpdf$yrweek),]
    
    ## Setup the preconditions for the nested while loops
    max_ind_rtn <- dim(rtnmat)[1]
    max_ind_df <- dim(tmpdf)[1]
    cur_ind_rtn <- 1
    cur_ind_df <- 1
    
    ## 2-level while loop with index "pointers" into the rtn matrix
    ## and the dataframe. Scans through the data and the rtn matrix
    ## at the same time and adds any none-na value that meets the
    ## criteria for any given week. This works only because the
    ## date format is correctly ordered by sort even though its not
    ## a numeric and the subsetted dataframe _has_ been sorted.
    ## Could be done with a small number of table commands, but I
    ## (SR) wanted to be able to handle any line-by-line cleaning
    ## in future within this loop if needed.
    while (cur_ind_df <= max_ind_df) {
      while (
        sel_weeks[cur_ind_rtn] != tmpdf$yrweek[cur_ind_df] &&
        cur_ind_rtn <= max_ind_rtn
      ) {
        cur_ind_rtn <- cur_ind_rtn + 1
      }
      
      if (cur_ind_rtn <= max_ind_rtn) {
        val_rtn <- rtnmat[cur_ind_rtn,cur_iso3]
        val_df <- tmpdf$ilitotal[cur_ind_df]
        
        if (!is.na(val_df)) {
          if (is.na(val_rtn)) {
            rtnmat[cur_ind_rtn,cur_iso3] <- val_df
          } 
        }
      }
      cur_ind_df <- cur_ind_df + 1
    }
  
    ## Close the state-level loop
  }
  
  ## Return the populated incidence matrix as only result of function
  rtnmat 
}

#' check the data availablity in each year
duration.fview <- function(flu.incidence,country,numWeek_ahead,minYear, maxYear, yr53week){
  year_time <- c(minYear:maxYear)
  
  flu_data_complex <- gbm_complex_fview(flu.incidence, country, 10,numWeek_ahead,yr53week)

  year_start <- min(as.numeric(substr(rownames(flu_data_complex),0,4)))
  year_end <- max(as.numeric(substr(rownames(flu_data_complex),0,4)))
  all_year <- as.numeric(substr(rownames(flu_data_complex),0,4))
  
  country_year <- c()
  
  for (i in 1:length(year_time)){
    if (year_time[i] %in% all_year == TRUE){
      tmp <- "Yes"
    }
    if (year_time[i] %in% all_year == FALSE){
      tmp <- "No"
    }
    country_year <- append(country_year, tmp)
  }
  country_year <- c(country, country_year, year_start, year_end)
  
  country_year
}


#' A full year is defined as from week 27 to week 26 of next year.
extract.incidence.fview.centre <- extract.incidence <- function(flu_data,
                                                                country_code,
                                                                year,
                                                                yr53week) {
  flu_data <- as.data.frame(flu_data)
  year_names <- rownames(flu_data)
  # start from week 01 of current year
  row_name_start <- paste0(year, "-01") 
  # stop at week 52 or 53 of current year
  yr53weeks <- yr53week
  if (year %in% yr53weeks == TRUE){
    row_name_end <- paste0(year, "-53")
  }else{
    row_name_end <- paste0(year, "-52")
  }
  # find the corresponding weeks in the data
  row_index_start <- which(rownames(flu_data) == row_name_start)
  row_index_end <- which(rownames(flu_data) == row_name_end)
  # extrac the week number and incidence for those weeks
  incidence <- flu_data[seq(row_index_start, row_index_end), 
                        colnames(flu_data) == country_code]
  time_name_vec <- year_names[seq(row_index_start, row_index_end)]
  
  incidence_data <- data.frame(t = seq_along(time_name_vec), 
                               time_name = time_name_vec, 
                               incidence = incidence)
  return(incidence_data)
}

adjust.data.size.fview <- function(flu_data,state,category,numWeek_ahead,yr53week){
  complex1 <- gbm_complex_fview(flu_data,state,category,1,yr53week)
  complex2 <- gbm_complex_fview(flu_data,state,category,2,yr53week)
  complex3 <- gbm_complex_fview(flu_data,state,category,3,yr53week)
  complex4 <- gbm_complex_fview(flu_data,state,category,4,yr53week)
  week <- intersect(rownames(complex1),intersect(rownames(complex2),
                                                 intersect(rownames(complex3),rownames(complex4))))
  
  complex <- gbm_complex_fview(flu_data,state,category,numWeek_ahead,yr53week)
  index <- c()
  for (i in 1:nrow(complex)){
    if(rownames(complex)[i] %in% week == FALSE){
      tmp <- i
      index <- append(index, tmp)
    }
  }
  if (length(index) == 0){
    complex <- complex
  }else{
    complex <- complex[-(index),]
  }
  
  complex
}

xgboost.model.pred.fview <- function(flu_data, state, num_category,
                               train_num_start, train_num_end, nWeek_ahead,yr53week){
  # set up dataset for xgboost
  flu_data_complex <- adjust.data.size.fview(flu_data, state, num_category, nWeek_ahead,yr53week)
  
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
  
  # train the xgboost model
  params.train <- list(booster = "gbtree", objective = "multi:softprob", gamma=0, num_class = 10,
                       subsample=1, colsample_bytree=1,eval_metric = "mlogloss")
  watchlist <- list(train = xgb_tr, test = xgb_ts)
  xgb_model <- xgb.train(params = params.train, data = xgb_tr, nrounds = 100, 
                         watchlist = watchlist,verbose = 2, print_every_n = 10,
                         early_stopping_round = 20)
  xgb_pred <- predict(xgb_model, newdata = xgb_ts)
  start_year_ts_index <- grep(start_year_ts,rownames(flu_data_complex))[1]
  end_ts <- grep(end_year_ts,rownames(flu_data_complex))
  end_year_ts_index <- end_ts[length(end_ts)]
  xgb_val_out <- matrix(xgb_pred, nrow = 10, ncol = length(xgb_pred)/10) %>% 
    t() %>%
    data.frame() %>%
    mutate(max = max.col(., ties.method = "last"), 
           category = flu_data_complex$Y_week0[start_year_ts_index:end_year_ts_index])
  
  pred_timeseries <- rownames(flu_data_complex)[start_year_ts_index:end_year_ts_index] %>% 
    cbind(xgb_val_out[,(ncol(xgb_val_out)-1):ncol(xgb_val_out)]) %>%
    data.frame()
  colnames(pred_timeseries) <- c("week_time", "Prediction", "Observation")
  pred_timeseries$Observation <- as.numeric(pred_timeseries$Observation)
  pred_timeseries$Prediction <- as.numeric(pred_timeseries$Prediction)
  for (i in 1:nrow(pred_timeseries)){
    if (pred_timeseries[i,2]==pred_timeseries[i,3]){
      pred_timeseries$Accurate[i] <- 1
    }
    else{
      pred_timeseries$Accurate[i] <- 0
    }
  }
  pred_timeseries
}


#' Function that gives individual state forecast result and accuracy score

compare_accuracy_indi_fview <- function(flu_data,
                                        individual_state, 
                                        num_category,
                                        train_num_start, 
                                        train_num_end,
                                        nWeek_ahead,
                                        yr53week){
  state_list <- individual_state
  individual_pred <- xgboost.model.pred.fview(flu_data,state_list,num_category,
                                        train_num_start, train_num_end,nWeek_ahead,yr53week)
  individual_pred <- cbind(rep(individual_state, nrow(individual_pred)),individual_pred) %>%
    as.data.frame()
  individual_pred <- as.data.frame(individual_pred)
  colnames(individual_pred) <- c("State","week_time","Observation","Prediction","Accurate")
  
  score <- round(length(which(individual_pred$Accurate == 1))/nrow(individual_pred),3)
  
  result <- NULL
  result$individual_pred <- individual_pred
  result$score <- score
  
  return(result)
}

accuracy_score_fview <- function(prediction, 
                                 state){
  dataframe <- matrix(NA, nrow = length(state), ncol = 2)
  
  for (i in 1:length(state)){
    dataframe[i,1] <- state[i]
    dataframe[i,2] <- prediction[[i*2]]
  }
  
  dataframe <- as.data.frame(dataframe)
  colnames(dataframe) <- c("State","Accuracy_score")
  
  dataframe$Accuracy_score <- round(as.numeric(as.character(dataframe$Accuracy_score)),3)
  
  return(dataframe)
}

#' Function of caculating the accuracy metric of xgboost model
compare_accuracy_fview <- function(flu_data,
                                   state_list,
                                   num_category, 
                                   train_num_start, 
                                   train_num_end,
                                   nWeek_ahead,
                                   yr53week){
  pred <- NULL
  for (i in 1:length(state_list)){
    individual_pred <- xgboost.model.pred.fview(flu_data,
                                                state_list[i],
                                                num_category,
                                                train_num_start, 
                                                train_num_end,
                                                nWeek_ahead,
                                                yr53week)
    individual_pred <- cbind(rep(state_list[i], nrow(individual_pred)),individual_pred)
    pred <- rbind(pred,individual_pred)
    
  }
  pred <- as.data.frame(pred)
  colnames(pred) <- c("State","Week_time","Observation","Prediction","Accurate")
  
  score <- round(length(which(pred$Accurate == 1))/nrow(pred),3)
  
  result <- NULL
  result$pred <- pred
  result$score <- score
  
  return(result)
  
}

freq_table_fview <- function(prediction, row_col){
  accuracy <- prediction[[1]][,c("Observation","Prediction","Accurate")]
  
  per <- gtools::permutations(row_col,2,repeats.allowed = TRUE)
  freq <- cbind(per, rep(0, nrow(per)))
  freq <- as.data.frame(freq)
  colnames(freq) <- c('observation_category', 'forecast_category', 'frequency')
  forecast_freq <- ftable(accuracy[1:2], row.vars = 2:1)
  forecast_freq <- as.data.frame(forecast_freq)
  colnames(forecast_freq) <- c('observation_category', 'forecast_category', 'frequency')
  # calculate frequency 
  j <- 1
  while(j <= nrow(per)){
    for (i in 1:nrow(forecast_freq)){
      if (forecast_freq$observation_category[i] %in% freq$observation_category[j] && 
          forecast_freq$forecast_category[i] %in% freq$forecast_category[j] == TRUE){
        freq$frequency[j] <- forecast_freq$frequency[i]
      }
    }
    j <- j+1
  }
  freq
  
  squared_freq <- matrix(nrow = row_col, ncol = row_col)
  n <- 1
  while(n <= nrow(freq)){
    for (j in 1:ncol(squared_freq)){
      for (i in 1:nrow(squared_freq)){
        squared_freq[i,j] <- freq$frequency[n]
        n <- n+1
      }
    }
  }
  squared_freq <- as.data.frame(squared_freq)
  colnames(squared_freq) <- c('observed_1','observed_2','observed_3','observed_4','observed_5','observed_6',
                              'observed_7','observed_8','observed_9','observed_10')
  rownames(squared_freq) <- c('predicted_1','predicted_2','predicted_3','predicted_4','predicted_5',
                              'predicted_6','predicted_7','predicted_8','predicted_9','predicted_10')
  squared_freq
}

heat_plot_fview <- function(frequencyTable, myat,countryName, score){
  require("RColorBrewer")
  require("raster")
  require("rasterVis")
  
  frequencyMatrix <- as.matrix(frequencyTable)
  colnames(frequencyMatrix) <- c(1:10)
  rownames(frequencyMatrix) <- c(1:10)
  my.at <- myat
  my.brks <- seq(0, max(frequencyMatrix, na.rm = TRUE), length.out = length(my.at))
  blues <- brewer.pal(9, "Blues")
  # reds <-  brewer.pal(9, "Reds")
  # mapTheme <- rasterTheme(region= c(blues,reds))
  mapTheme <- rasterTheme(region= blues)
myColorkey <- list(at=my.brks, labels=list(at=my.brks, labels=my.at), space="right")
myPanel <- function(x, y, z,...) {
  panel.levelplot(x,y,z,...)
  for (i in 1:nrow(frequencyMatrix)){
    for (j in 1:ncol(frequencyMatrix)){
      panel.text(x=i, y=j, frequencyMatrix[cbind(j,i)])
    }
  }
}
title <- paste(countryName,"accuracy score:", score, collapse = " ")
country <- countryName 
levelplot(t(frequencyMatrix), xlab = 'Obeserved', ylab = 'Forecast', main = title,
          panel = myPanel, par.settings=mapTheme, at=my.at, colorkey=myColorkey, margin=F)
}

heat_plot_fview_marginal <- function(frequencyTable,countryName, score){
  require("RColorBrewer")
  require("raster")
  require("rasterVis")
  frequencyMatrix <- matrix(NA,nrow = nrow(frequencyTable), ncol = ncol(frequencyTable))
  for (j in 1:ncol(frequencyTable)){
    for (i in 1:nrow(frequencyTable)){
      frequencyMatrix[i,j] <- round(frequencyTable[i,j]/sum(frequencyTable[,j]),2)
    }
  }
  colnames(frequencyMatrix) <- c(1:10)
  rownames(frequencyMatrix) <- c(1:10)
  my.at <- seq(0,1,0.1)
  my.brks <- seq(0, max(frequencyMatrix, na.rm = TRUE), length.out = length(my.at))
  blues <- brewer.pal(9, "Blues")
  # reds <-  brewer.pal(9, "Reds")
  # mapTheme <- rasterTheme(region= c(blues,reds))
  mapTheme <- rasterTheme(region= blues)
  myColorkey <- list(at=my.brks, labels=list(at=my.brks, labels=my.at), space="right")
  myPanel <- function(x, y, z,...) {
    panel.levelplot(x,y,z,...)
    for (i in 1:nrow(frequencyMatrix)){
      for (j in 1:ncol(frequencyMatrix)){
        panel.text(x=i, y=j, frequencyMatrix[cbind(j,i)])
      }
    }
  }
  title <- paste(countryName,"accuracy score:", score, collapse = " ")
  country <- countryName 
  levelplot(t(frequencyMatrix), xlab = 'Obeserved', ylab = 'Forecast', main = title,
            panel = myPanel, par.settings=mapTheme, at=my.at, colorkey=myColorkey, margin=F)
}

#' Historical average model
hist_average_fview <- function(flu_data, state,num_category, numWeek_ahead,yr53week){
  
  flu_data_complex <- adjust.data.size.fview(flu_data,state,num_category,numWeek_ahead,yr53week)
  
  flu_data_complex <- cbind(substr(rownames(flu_data_complex), 0,4), 
                            substr(rownames(flu_data_complex),6,7),
                            flu_data_complex[,1:3]) %>% as.data.frame()
  colnames(flu_data_complex) <- c("Year","Week","Y_week0","week_1","week_2")
  flu_data_complex$Week <- as.numeric(flu_data_complex$Week)
  
  pred <- matrix(NA,nrow = nrow(flu_data_complex), ncol = numWeek_ahead)
  
  if (numWeek_ahead == 1){
    for (i in 1:nrow(flu_data_complex)){
      yr <- flu_data_complex$Year[i]
      week <- flu_data_complex$Week[i]-1
      obsTem <- flu_data_complex[which(flu_data_complex$Week==week),]
      obs <- obsTem$Y_week0[which(obsTem$Year != yr)]
      pred[i,] <- which.max(tabulate(obs))
    }
    
    pred <- cbind(rownames(flu_data_complex),pred,flu_data_complex$Y_week0) %>% 
      as.data.frame()
    colnames(pred) <- c("Week_time","OneWeek_ahead", "Observation")
    
    # accuracy
    for (i in 1:nrow(pred)){
      if (is.na(pred[i,2])==TRUE || is.na(pred[i,3])==TRUE){
        pred$accurate[i] <- NA
      }else{
        if (pred[i,2]==pred[i,3]){
          pred$Accurate[i] <- 1
        }else{
          pred$Accurate[i] <- 0
        }
      }
    }
  }
  
  if (numWeek_ahead == 2){
    for (i in 1:nrow(flu_data_complex)){
      yr <- flu_data_complex$Year[i]
      week <- flu_data_complex$Week[i]-2
      obsTem <- flu_data_complex[which(flu_data_complex$Week==week),]
      obs <- obsTem$Y_week0[which(obsTem$Year != yr)]
      pred[i,] <- which.max(tabulate(obs))
    }
    pred <- cbind(rownames(flu_data_complex),pred,flu_data_complex$Y_week0) %>% 
      as.data.frame()
    colnames(pred) <- c("Week_time","OneWeek_ahead", "TwoWeek_ahead","Observation")
    
    # accuracy
    for (i in 1:nrow(pred)){
      if (is.na(pred[i,3])==TRUE || is.na(pred[i,4])==TRUE){
        pred$accurate[i] <- NA
      }else{
        if (pred[i,3]==pred[i,4]){
          pred$Accurate[i] <- 1
        }else{
          pred$Accurate[i] <- 0
        }
      }
    }
  }
  
  if (numWeek_ahead == 3){
    for (i in 1:nrow(flu_data_complex)){
      yr <- flu_data_complex$Year[i]
      week <- flu_data_complex$Week[i]-3
      obsTem <- flu_data_complex[which(flu_data_complex$Week==week),]
      obs <- obsTem$Y_week0[which(obsTem$Year != yr)]
      pred[i,] <- which.max(tabulate(obs))
    }
    pred <- cbind(rownames(flu_data_complex),pred,flu_data_complex$Y_week0) %>% 
      as.data.frame()
    colnames(pred) <- c("Week_time","OneWeek_ahead", "TwoWeek_ahead","ThreeWeek_ahead","Observation")
    
    # accuracy
    for (i in 1:nrow(pred)){
      if (is.na(pred[i,4])==TRUE || is.na(pred[i,5])==TRUE){
        pred$accurate[i] <- NA
      }else{
        if (pred[i,4]==pred[i,5]){
          pred$Accurate[i] <- 1
        }else{
          pred$Accurate[i] <- 0
        }
      }
    }
  }
  
  if (numWeek_ahead == 4){
    for (i in 1:nrow(flu_data_complex)){
      yr <- flu_data_complex$Year[i]
      week <- flu_data_complex$Week[i]-4
      obsTem <- flu_data_complex[which(flu_data_complex$Week==week),]
      obs <- obsTem$Y_week0[which(obsTem$Year != yr)]
      pred[i,] <- which.max(tabulate(obs))
    }
    pred <- cbind(rownames(flu_data_complex),pred,flu_data_complex$Y_week0) %>% 
      as.data.frame()
    colnames(pred) <- c("Week_time","OneWeek_ahead", "TwoWeek_ahead","ThreeWeek_ahead","FourWeek_ahead","Observation")
    
    # accuracy
    for (i in 1:nrow(pred)){
      if (is.na(pred[i,5])==TRUE || is.na(pred[i,6])==TRUE){
        pred$accurate[i] <- NA
      }else{
        if (pred[i,5]==pred[i,6]){
          pred$Accurate[i] <- 1
        }else{
          pred$Accurate[i] <- 0
        }
      }
    }
  }
  
  score <- round(length(which(pred$Accurate == 1))/nrow(pred),3)
  
  result <- NULL
  result$pred <- pred
  result$score <- score
  
  return(result)
}

# compare 1,2,3,4-week ahead forecast accuracy
compare_accuracy_hist_fview <- function(flu_data,state,num_category,numWeek_ahead,yr53week){
  pred <- NULL
  
  for (i in 1:length(state)){
    individual_pred <- hist_average_fview(flu_data,state[i],num_category,numWeek_ahead,yr53week)
    individual_pred <- cbind(rep(state[i], nrow(individual_pred[[1]])),individual_pred[[1]])
    pred <- rbind(pred,individual_pred)
  }
  pred <- as.data.frame(pred)
  colnames(pred) <- c("State",colnames(pred)[2:ncol(pred)])
  
  score <- round(length(which(pred$Accurate == 1))/nrow(pred),3)
  
  result <- NULL
  result$pred <- pred
  result$score <- score
  
  return(result)
}

# null model
null_fview <- function(flu_data,state, num_category, numWeek_ahead, yr53week){
  require(dplyr)
  
  flu_data_complex <- adjust.data.size.fview(flu_data,state,num_category,numWeek_ahead,yr53week)                                
  # prediction of the week is the same as the last week
  if(numWeek_ahead == 1){
    pred <- c()
    for (i in 1:nrow(flu_data_complex)){
      tmp <- flu_data_complex$week_1[i]
      pred <- append(pred,tmp)
    }
    pred <- cbind(rownames(flu_data_complex),pred, flu_data_complex$Y_week0) %>% 
      as.data.frame()
    colnames(pred) <- c("Week_time","Prediction","Observation")
    
    # accuracy
    for (i in 1:nrow(pred)){
      if (is.na(pred[i,2])==TRUE || is.na(pred[i,3])==TRUE){
        pred$accurate[i] <- NA
      }else{
        if (pred[i,2]==pred[i,3]){
          pred$Accurate[i] <- 1
        }else{
          pred$Accurate[i] <- 0
        }
      }
    }
  }
  
  if (numWeek_ahead == 2){
    pred <- matrix(NA, nrow = nrow(flu_data_complex),ncol = 2)
    for (i in 1:nrow(pred)){
      pred[i,] <- flu_data_complex$week_2[i]
    }
    
    pred <- cbind(rownames(flu_data_complex),pred,flu_data_complex$Y_week0) %>% 
      as.data.frame()
    colnames(pred) <- c('Week_time','OneWeek_ahead','TwoWeek_ahead', "Observation")
    
    # accuracy
    for (i in 1:nrow(pred)){
      if (is.na(pred[i,3])==TRUE || is.na(pred[i,4])==TRUE){
        pred$accurate[i] <- NA
      }else{
        if (pred[i,3]==pred[i,4]){
          pred$Accurate[i] <- 1
        }else{
          pred$Accurate[i] <- 0
        }
      }
    }
  }
  
  if (numWeek_ahead == 3){
    pred <- matrix(NA, nrow = nrow(flu_data_complex),ncol = 3)
    for (i in 1:nrow(pred)){
      pred[i,] <- flu_data_complex$week_3[i]
    }
    
    pred <- cbind(rownames(flu_data_complex),pred,flu_data_complex$Y_week0) %>% 
      as.data.frame()
    colnames(pred) <- c("Week_time","OneWeek_ahead","TwoWeek_ahead","ThreeWeek_ahead", "Observation")
    
    # accuracy
    for (i in 1:nrow(pred)){
      if (is.na(pred[i,4])==TRUE || is.na(pred[i,5])==TRUE){
        pred$accurate[i] <- NA
      }else{
        if (pred[i,4]==pred[i,5]){
          pred$Accurate[i] <- 1
        }else{
          pred$Accurate[i] <- 0
        }
      }
    }
  }
  
  if (numWeek_ahead == 4){
    pred <- matrix(NA, nrow = nrow(flu_data_complex),ncol = 4)
    for (i in 1:nrow(pred)){
      pred[i,] <- flu_data_complex$week_4[i]
    }
    
    pred <- cbind(rownames(flu_data_complex),pred,flu_data_complex$Y_week0) %>% 
      as.data.frame()
    colnames(pred) <- c("Week_time","OneWeek_ahead","TwoWeek_ahead","ThreeWeek_ahead","FourWeek_ahead", "Observation")
    # accuracy
    for (i in 1:nrow(pred)){
      if (is.na(pred[i,5])==TRUE || is.na(pred[i,6])==TRUE){
        pred$accurate[i] <- NA
      }else{
        if (pred[i,5]==pred[i,6]){
          pred$Accurate[i] <- 1
        }else{
          pred$Accurate[i] <- 0
        }
      }
    }
  }
  
  score <- round(length(which(pred$Accurate == 1))/nrow(pred),3)
  
  result <- NULL
  result$pred <- pred
  result$score <- score
  
  return(result)
  
}

#' calculate the accuracy of repeat model
compare_accuracy_null <- function(flu_data,state,num_category,numWeek_ahead, yr53week){
  pred <- NULL
  
  for (i in 1:length(state)){
    individual_pred <- null_fview(flu_data,state[i],num_category, numWeek_ahead, yr53week)
    individual_pred <- cbind(rep(state[i], nrow(individual_pred[[1]])),individual_pred[[1]])
    pred <- rbind(pred,individual_pred)
  }
  pred <- as.data.frame(pred)
  colnames(pred) <- c("Country",colnames(pred)[2:ncol(pred)])
  
  score <- round(length(which(pred$Accurate == 1))/nrow(pred),3)
  
  result <- NULL
  result$pred <- pred
  result$score <- score
  
  return(result)
  
}
