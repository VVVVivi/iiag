library(knitr)

data("fluIliCountryData")
fluIliCountryData2 <- as.data.frame(fluIliCountryData)

#' summary of available data in non-temperate regions
country_idd$ISO3[which(country_idd$Region == "non-temperate")] # KIR MHL FSM PLW

nonTem_data <- cbind(fluIliCountryData2$KIR, fluIliCountryData2$MHL,
                     fluIliCountryData2$FSM,fluIliCountryData2$PLW)

nonTem_data <- as.data.frame(nonTem_data)
colnames(nonTem_data) <- c("KIR", "MHL", "FSM", "PLW")
length(which(is.na(nonTem_data$KIR)==FALSE)) # 298
length(which(is.na(nonTem_data$MHL)==FALSE)) # 303
length(which(is.na(nonTem_data$FSM)==FALSE)) # 284
length(which(is.na(nonTem_data$PLW)==FALSE)) # 301
mean(c(298,303,284,301)) # 296 weeks
median(c(298,303,284,301)) # 299.5

#' summary of available data in temperate regions
which(country_idd$Region == "temperate")
temData <- fluIliCountryData2[,which(country_idd$Region == "temperate")]
temObs <- c()
for (i in 1:ncol(temData)){
  tmp <- length(which(is.na(temData[,i])==FALSE))
  temObs <- append(temObs,tmp)
}
max(temObs) # 419
min(temObs) # 244
mean(temObs) #337
median(temObs) # 341

category <- NULL
for (i in 1:length(country_xgboost)){
  tmp <- gbm_complex(fluIliCountryData,country_xgboost[i],countryISO,10)
  tmp <- cbind(rep(country_xgboost[i],nrow(tmp)),tmp)
  category <- rbind(category, tmp)
}
colnames(category) <- c("CountryCode",colnames(category)[2:6])

obsCount <- c()
for (i in 1:nrow(category)){
  tmp <- length(which(as.character(category$CountryCode) == as.character(country_idd$ISO3[i])))
  obsCount <- append(obsCount,tmp)
}
max(obsCount) # 390
which.max(obsCount) # 14
country_idd$Country[14] # Ireland

categorySummaryChart <- cbind(c(1:10),c(length(which(category$Y_week0==1)),length(which(category$Y_week0==2)),
                              length(which(category$Y_week0==3)),length(which(category$Y_week0==4)),
                              length(which(category$Y_week0==5)),length(which(category$Y_week0==6)),
                              length(which(category$Y_week0==7)),length(which(category$Y_week0==8)),
                              length(which(category$Y_week0==9)),length(which(category$Y_week0==10))))%>%
  as.data.frame()
colnames(categorySummaryChart) <- c("Category","Count")
categorySummaryChart$Density <- round(categorySummaryChart$Count/sum(categorySummaryChart$Count),3)

#' accuracy of repeat model for every countries
oneWeek_ahead_accuracyByCountry_repeat <- NULL
for (i in 1:nrow(country_idd)){
  tmp <- length(which(oneWeek_ahead_totalAccuracy$Accurate[as.character(country_idd$ISO3[i]) == as.character(oneWeek_ahead_totalAccuracy$Country)]==1))
  tmp2 <- length(which(as.character(country_idd$ISO3[i]) == as.character(oneWeek_ahead_totalAccuracy$Country)))
  tmp3 <- round(tmp/tmp2,2)
  tmp <- cbind(as.character(country_idd$mapCountry[i]),tmp,tmp2,tmp3)
  oneWeek_ahead_accuracyByCountry_repeat <- rbind(oneWeek_ahead_accuracyByCountry_repeat,tmp)
}
oneWeek_ahead_accuracyByCountry_repeat <- as.data.frame(oneWeek_ahead_accuracyByCountry_repeat)
colnames(oneWeek_ahead_accuracyByCountry_repeat) <- c("Country","Accurate","Total","Percentage")

twoWeek_ahead_accuracyByCountry_repeat <- NULL
for (i in 1:nrow(country_idd)){
  tmp <- length(which(twoWeek_ahead_totalAccuracy$Accurate[as.character(country_idd$ISO3[i]) == as.character(twoWeek_ahead_totalAccuracy$Country)]==1))
  tmp2 <- length(which(as.character(country_idd$ISO3[i]) == as.character(twoWeek_ahead_totalAccuracy$Country)))
  tmp3 <- round(tmp/tmp2,2)
  tmp <- cbind(as.character(country_idd$mapCountry[i]),tmp,tmp2,tmp3)
  twoWeek_ahead_accuracyByCountry_repeat <- rbind(twoWeek_ahead_accuracyByCountry_repeat,tmp)
}
twoWeek_ahead_accuracyByCountry_repeat <- as.data.frame(twoWeek_ahead_accuracyByCountry_repeat)
colnames(twoWeek_ahead_accuracyByCountry_repeat) <- c("Country","Accurate","Total","Percentage")

threeWeek_ahead_accuracyByCountry_repeat <- NULL
for (i in 1:nrow(country_idd)){
  tmp <- length(which(threeWeek_ahead_totalAccuracy$Accurate[as.character(country_idd$ISO3[i]) == as.character(threeWeek_ahead_totalAccuracy$Country)]==1))
  tmp2 <- length(which(as.character(country_idd$ISO3[i]) == as.character(threeWeek_ahead_totalAccuracy$Country)))
  tmp3 <- round(tmp/tmp2,2)
  tmp <- cbind(as.character(country_idd$mapCountry[i]),tmp,tmp2,tmp3)
  threeWeek_ahead_accuracyByCountry_repeat <- rbind(threeWeek_ahead_accuracyByCountry_repeat,tmp)
}
threeWeek_ahead_accuracyByCountry_repeat <- as.data.frame(threeWeek_ahead_accuracyByCountry_repeat)
colnames(threeWeek_ahead_accuracyByCountry_repeat) <- c("Country","Accurate","Total","Percentage")

 
fourWeek_ahead_accuracyByCountry_repeat <- NULL
for (i in 1:nrow(country_idd)){
  tmp <- length(which(fourWeek_ahead_totalAccuracy$Accurate[as.character(country_idd$ISO3[i]) == as.character(fourWeek_ahead_totalAccuracy$Country)]==1))
  tmp2 <- length(which(as.character(country_idd$ISO3[i]) == as.character(fourWeek_ahead_totalAccuracy$Country)))
  tmp3 <- round(tmp/tmp2,2)
  tmp <- cbind(as.character(country_idd$mapCountry[i]),tmp,tmp2,tmp3)
  fourWeek_ahead_accuracyByCountry_repeat <- rbind(fourWeek_ahead_accuracyByCountry_repeat,tmp)
}
fourWeek_ahead_accuracyByCountry_repeat <- as.data.frame(fourWeek_ahead_accuracyByCountry_repeat)
colnames(fourWeek_ahead_accuracyByCountry_repeat) <- c("Country","Accurate","Total","Percentage")

#' accuracy of historical average model for every countries
oneWeek_ahead_accuracyByCountry_hist <- NULL
for (i in 1:nrow(country_idd)){
  tmp <- length(which(oneWeek_ahead_totalAccuracy_hist$Accurate[as.character(country_idd$ISO3[i]) == as.character(oneWeek_ahead_totalAccuracy_hist$Country)]==1))
  tmp2 <- length(which(as.character(country_idd$ISO3[i]) == as.character(oneWeek_ahead_totalAccuracy_hist$Country)))
  tmp3 <- round(tmp/tmp2,2)
  tmp <- cbind(as.character(country_idd$mapCountry[i]),tmp,tmp2,tmp3)
  oneWeek_ahead_accuracyByCountry_hist <- rbind(oneWeek_ahead_accuracyByCountry_hist,tmp)
}
oneWeek_ahead_accuracyByCountry_hist <- as.data.frame(oneWeek_ahead_accuracyByCountry_hist)
colnames(oneWeek_ahead_accuracyByCountry_hist) <- c("Country","Accurate","Total","Percentage")

twoWeek_ahead_accuracyByCountry_hist <- NULL
for (i in 1:nrow(country_idd)){
  tmp <- length(which(twoWeek_ahead_totalAccuracy_hist$Accurate[as.character(country_idd$ISO3[i]) == as.character(twoWeek_ahead_totalAccuracy_hist$Country)]==1))
  tmp2 <- length(which(as.character(country_idd$ISO3[i]) == as.character(twoWeek_ahead_totalAccuracy_hist$Country)))
  tmp3 <- round(tmp/tmp2,2)
  tmp <- cbind(as.character(country_idd$mapCountry[i]),tmp,tmp2,tmp3)
  twoWeek_ahead_accuracyByCountry_hist <- rbind(twoWeek_ahead_accuracyByCountry_hist,tmp)
}
twoWeek_ahead_accuracyByCountry_hist <- as.data.frame(twoWeek_ahead_accuracyByCountry_hist)
colnames(twoWeek_ahead_accuracyByCountry_hist) <- c("Country","Accurate","Total","Percentage")

threeWeek_ahead_accuracyByCountry_hist <- NULL
for (i in 1:nrow(country_idd)){
  tmp <- length(which(threeWeek_ahead_totalAccuracy_hist$Accurate[as.character(country_idd$ISO3[i]) == as.character(threeWeek_ahead_totalAccuracy_hist$Country)]==1))
  tmp2 <- length(which(as.character(country_idd$ISO3[i]) == as.character(threeWeek_ahead_totalAccuracy_hist$Country)))
  tmp3 <- round(tmp/tmp2,2)
  tmp <- cbind(as.character(country_idd$mapCountry[i]),tmp,tmp2,tmp3)
  threeWeek_ahead_accuracyByCountry_hist <- rbind(threeWeek_ahead_accuracyByCountry_hist,tmp)
}
threeWeek_ahead_accuracyByCountry_hist <- as.data.frame(threeWeek_ahead_accuracyByCountry_hist)
colnames(threeWeek_ahead_accuracyByCountry_hist) <- c("Country","Accurate","Total","Percentage")

fourWeek_ahead_accuracyByCountry_hist <- NULL
for (i in 1:nrow(country_idd)){
  tmp <- length(which(fourWeek_ahead_totalAccuracy_hist$Accurate[as.character(country_idd$ISO3[i]) == as.character(fourWeek_ahead_totalAccuracy_hist$Country)]==1))
  tmp2 <- length(which(as.character(country_idd$ISO3[i]) == as.character(fourWeek_ahead_totalAccuracy_hist$Country)))
  tmp3 <- round(tmp/tmp2,2)
  tmp <- cbind(as.character(country_idd$mapCountry[i]),tmp,tmp2,tmp3)
  fourWeek_ahead_accuracyByCountry_hist <- rbind(fourWeek_ahead_accuracyByCountry_hist,tmp)
}
fourWeek_ahead_accuracyByCountry_hist <- as.data.frame(fourWeek_ahead_accuracyByCountry_hist)
colnames(fourWeek_ahead_accuracyByCountry_hist) <- c("Country","Accurate","Total","Percentage") 

#' data set for plotting time series of all countries in one graph
rawData_TS_plot2 <- function(data, country,countryName){
  require(ggplot2)
  require(grid)
  
  final_dataset <- NULL
  for (i in 1:length(country)){
    count <- data[, which(colnames(data)==country[i])]
    week_time <- rownames(data)[-which(is.na(count))]
    count <- count[-(which(is.na(count)))]
    category <- cut_interval(count, 10)
    raw_data <- cbind(rep(as.character(countryName[i]),length(count)),week_time, count, category)
    
    raw_data <- as.data.frame(raw_data)
    colnames(raw_data) <- c('country','week_time','count','category')
    
    # convert discrete week time to contious time series 
    raw_data$weekTS <- gsub("-", "-W", raw_data$week_time, fixed = TRUE)
    raw_data$weekTS <- week2date(raw_data$weekTS)
    raw_data$count <- as.numeric(as.character(raw_data$count))
    raw_data$category <- as.numeric(as.character(raw_data$category))
    
    final_dataset <- rbind(final_dataset, raw_data)
  }
  final_dataset
}
allCountry_TS <- rawData_TS_plot2(fluIliCountryData,country_idd$ISO3,country_idd$mapCountry)

#'xgboost accuracy for each country
#' 2010-2016 train, 2017 test 
acc1016_pred17 <- NULL
for (i in 1:nrow(country_idd)){
  one <- compare1016_pred17[which(compare1016_pred17$Country==country_idd$ISO3[i]),]
  tmp <- round(length(which(one$Accurate==1))/nrow(one),2)
  two <- compare1016_pred17Two[which(compare1016_pred17Two$Country==country_idd$ISO3[i]),]
  tmp2 <- round(length(which(two$Accurate==1))/nrow(two),2)
  three <- compare1016_pred17Three[which(compare1016_pred17Three$Country==country_idd$ISO3[i]),]
  tmp3 <- round(length(which(three$Accurate==1))/nrow(three),2)
  four <- compare1016_pred17Four[which(compare1016_pred17Four$Country==country_idd$ISO3[i]),]
  tmp4 <- round(length(which(four$Accurate==1))/nrow(four),2)
  acc1016_pred17 <- rbind(acc1016_pred17,cbind(as.character(country_idd$Country[i]),tmp,tmp2,tmp3,tmp4)) 
}  
acc1016_pred17 <- as.data.frame(acc1016_pred17)
colnames(acc1016_pred17) <- c("Country", "oneWeek_ahead","twoWeek_ahead","threeWeek_ahead","fourWeek_ahead")

#' 2010-14 train, 15 test
acc1014_pred15 <- NULL
for (i in 1:nrow(country_idd)){
  one <- compare_pred15[which(compare_pred15$Country==country_idd$ISO3[i]),]
  tmp <- round(length(which(one$Accurate==1))/nrow(one),2)
  two <- compare_pred15Two[which(compare_pred15Two$Country==country_idd$ISO3[i]),]
  tmp2 <- round(length(which(two$Accurate==1))/nrow(two),2)
  three <- compare_pred15Three[which(compare_pred15Three$Country==country_idd$ISO3[i]),]
  tmp3 <- round(length(which(three$Accurate==1))/nrow(three),2)
  four <- compare_pred15Four[which(compare_pred15Four$Country==country_idd$ISO3[i]),]
  tmp4 <- round(length(which(four$Accurate==1))/nrow(four),2)
  acc1014_pred15 <- rbind(acc1014_pred15,cbind(as.character(country_idd$Country[i]),tmp,tmp2,tmp3,tmp4)) 
}  
acc1014_pred15 <- as.data.frame(acc1014_pred15)
colnames(acc1014_pred15) <- c("Country", "oneWeek_ahead","twoWeek_ahead","threeWeek_ahead","fourWeek_ahead")

#' 2011-15 train, 16 test
acc1115_pred16 <- NULL
for (i in 1:nrow(country_idd)){
  one <- compare_pred16[which(compare_pred16$Country==country_idd$ISO3[i]),]
  tmp <- round(length(which(one$Accurate==1))/nrow(one),2)
  two <- compare_pred16Two[which(compare_pred16Two$Country==country_idd$ISO3[i]),]
  tmp2 <- round(length(which(two$Accurate==1))/nrow(two),2)
  three <- compare_pred16Three[which(compare_pred16Three$Country==country_idd$ISO3[i]),]
  tmp3 <- round(length(which(three$Accurate==1))/nrow(three),2)
  four <- compare_pred16Four[which(compare_pred16Four$Country==country_idd$ISO3[i]),]
  tmp4 <- round(length(which(four$Accurate==1))/nrow(four),2)
  acc1115_pred16 <- rbind(acc1115_pred16,cbind(as.character(country_idd$Country[i]),tmp,tmp2,tmp3,tmp4)) 
}  
acc1115_pred16 <- as.data.frame(acc1115_pred16)
colnames(acc1115_pred16) <- c("Country", "oneWeek_ahead","twoWeek_ahead","threeWeek_ahead","fourWeek_ahead")

#' 2012-16 train, 17 train
acc1216_pred17 <- NULL
for (i in 1:nrow(country_idd)){
  one <- compare_pred17[which(compare_pred17$Country==country_idd$ISO3[i]),]
  tmp <- round(length(which(one$Accurate==1))/nrow(one),2)
  two <- compare_pred17Two[which(compare_pred17Two$Country==country_idd$ISO3[i]),]
  tmp2 <- round(length(which(two$Accurate==1))/nrow(two),2)
  three <- compare_pred17Three[which(compare_pred17Three$Country==country_idd$ISO3[i]),]
  tmp3 <- round(length(which(three$Accurate==1))/nrow(three),2)
  four <- compare_pred17Four[which(compare_pred17Four$Country==country_idd$ISO3[i]),]
  tmp4 <- round(length(which(four$Accurate==1))/nrow(four),2)
  acc1216_pred17 <- rbind(acc1216_pred17,cbind(as.character(country_idd$Country[i]),tmp,tmp2,tmp3,tmp4)) 
} 
acc1216_pred17 <- as.data.frame(acc1216_pred17)
colnames(acc1216_pred17) <- c("Country", "oneWeek_ahead","twoWeek_ahead","threeWeek_ahead","fourWeek_ahead")

#' category distribution in 2015, 2016 and 2017
#' in 2015
category15 <- cbind(c(1:10),c(length(which(compare_pred15$Observation==1)),
                              length(which(compare_pred15$Observation==2)),
                              length(which(compare_pred15$Observation==3)),
                              length(which(compare_pred15$Observation==4)),
                              length(which(compare_pred15$Observation==5)),
                              length(which(compare_pred15$Observation==6)),
                              length(which(compare_pred15$Observation==7)),
                              length(which(compare_pred15$Observation==8)),
                              length(which(compare_pred15$Observation==9)),
                              length(which(compare_pred15$Observation==10))))%>%
  as.data.frame()
colnames(category15 ) <- c("Category","Count")
category15$Density <- round(category15$Count/sum(category15$Count),3)

#' in 2016
category16 <- cbind(c(1:10),c(length(which(compare_pred16$Observation==1)),
                              length(which(compare_pred16$Observation==2)),
                              length(which(compare_pred16$Observation==3)),
                              length(which(compare_pred16$Observation==4)),
                              length(which(compare_pred16$Observation==5)),
                              length(which(compare_pred16$Observation==6)),
                              length(which(compare_pred16$Observation==7)),
                              length(which(compare_pred16$Observation==8)),
                              length(which(compare_pred16$Observation==9)),
                              length(which(compare_pred16$Observation==10))))%>%
  as.data.frame()
colnames(category16) <- c("Category","Count")
category16$Density <- round(category16$Count/sum(category16$Count),3)

#' in 2017
category17 <- cbind(c(1:10),c(length(which(compare_pred17$Observation==1)),
                              length(which(compare_pred17$Observation==2)),
                              length(which(compare_pred17$Observation==3)),
                              length(which(compare_pred17$Observation==4)),
                              length(which(compare_pred17$Observation==5)),
                              length(which(compare_pred17$Observation==6)),
                              length(which(compare_pred17$Observation==7)),
                              length(which(compare_pred17$Observation==8)),
                              length(which(compare_pred17$Observation==9)),
                              length(which(compare_pred17$Observation==10))))%>%
  as.data.frame()
colnames(category17) <- c("Category","Count")
category17$Density <- round(category17$Count/sum(category17$Count),3)
