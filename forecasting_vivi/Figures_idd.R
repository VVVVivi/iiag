library(ggplot2)
library(grid) # align two plots
library(ggmap)
library(maptools)
library(rgdal) # to assign colors for different regions
library(maps)
library(idd)
library(aweek) # convert discrete date into contiuous variable
library(knitr)
library(grid) # annotation in the plot
library(MASS) # require fitdistr(...) to generate density line
library(scales)
library(cowplot) # combine multiple plots


rm(list = ls(all = TRUE))


# countryISO <- read.csv("/Users/vvvvivi/GitHub_Vivi/iiag/data/country_list_ISO.csv")
countryISO <- read.csv("C:/Users/hw3616/Desktop/Imperial/Project1_Forecasting/Project_Coding/iiag/data_old/country_list_ISO.csv")

#' load forecast results date table for visualisation
worldPred_figures <- readRDS("world_predTable.rds")


#' Plot world distribution of countries used in this project 
# countryISO <- read.csv("/Users/vvvvivi/GitHub_Vivi/iiag/data_old/country_list_ISO.csv")
countryISO <- read.csv("C:/Users/hw3616/Desktop/Imperial/Project1_Forecasting/Project_Coding/iiag/data_old/country_list_ISO.csv")

data("fluIliCountryData")

#' Plot world distribution of countries used in this project 
#' Extract geographic information of countries included in this project
#' To fill different colors in temperate and non-temperate, I will give every country a level to distinguish 
country_code <- colnames(fluIliCountryData)[-c(8,31,35,45,50,56,58,64,65,66,68)]

country_idd <- c()
for (i in 1:length(country_code)){
  index <- grep(country_code[i],countryISO$ISO3)
  tmp <- countryISO[index,]
  country_idd <- rbind(country_idd, tmp)
}

for (i in 1:nrow(country_idd)){
  if (country_idd$Latitude[i] >= 23.25 | country_idd$Latitude[i] <= -23.25){
    country_idd$Region[i] <- "temperate"
  }
  if (country_idd$Latitude[i] < 23.25 && country_idd$Latitude[i] > -23.25){
    country_idd$Region[i] <- "non-temperate"
  }
}

for (i in 1:nrow(country_idd)){
  if (country_idd$Latitude[i] > 0){
    country_idd$Hemisphere[i] <- "Nothern hemisphere"
  }
  if (country_idd$Latitude[i] < 0){
    country_idd$Hemisphere[i] <- "Southern hemisphere"
  }
}
country_idd$Region <- as.factor(country_idd$Region)

length(which(country_idd$Region == "temperate"))
which(country_idd$Region == "non-temperate")
which(country_idd$Hemisphere == "Southern hemisphere")


#' change the country names in country_idd to the same as in world map
which(country_idd$Country %in% unique(world_map$region)==FALSE) # 5 countries are missing
# Micronesia, Federated States of, Moldova, Republic of,Russian Federation,
# Macedonia, the former Yugoslav Republic of,United States 
country_idd$Country[c(27,29,37)]
mapCountry <- country_idd$Country
country_idd <- cbind(country_idd$Country, mapCountry, country_idd[,(2:6)])
colnames(country_idd) <- c("Country","mapCountry",colnames(country_idd)[3:7])
country_idd$mapCountry <- as.character(country_idd$mapCountry)

country_idd$mapCountry[27] <- "Moldova"
country_idd$mapCountry[29] <- "Russia" 
country_idd$mapCountry[37] <- "USA"
country_idd$mapCountry <- as.factor(country_idd$mapCountry)
country_idd <- country_idd[which(country_idd$ISO3 %in% country_xgboost),]


world_map <- map_data ("world")
myCountries <- NULL
for (i in 1:nrow(country_idd)){
  if((country_idd$mapCountry[i] %in% unique(world_map$region)) == TRUE)
  tmp <- world_map[which(country_idd$mapCountry[i] == world_map$region),]
  myCountries <- rbind(myCountries, tmp)
}

for (i in 1:nrow(myCountries)){
  if (myCountries$region[i] %in% country_idd$mapCountry[which(country_idd$Region == "temperate")] == TRUE){
    myCountries$Region[i] <- "temperate"
  }
  if(myCountries$region[i] %in% country_idd$mapCountry[which(country_idd$Region == "temperate")] == FALSE){
    myCountries$Region[i] <- "non-temperate"
  }
}


world_map <- borders("world", colour="gray50", fill="gray50")
ggplot() +  world_map +
  geom_polygon(data = myCountries, 
               aes(x = long, y = lat, group = group, fill = Region), color = "white")+
  scale_fill_manual(values = c("red","blue"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank())
                                                     

#' sample of time series of observation versus prediction
pred_timeseries <- readRDS("/Users/vvvvivi/Downloads/pred_timeseries.rds")
library(aweek)
pred_timeseries$week_time <- gsub("-", "-W", pred_timeseries$week_time, fixed = TRUE) # change week into format used for aweek package
# the aweek package is used to convert from epidemiological weeks to dates
pred_timeseries$week_time <- week2date(pred_timeseries$week_time)
# convert numbers from factors to numerics
pred_timeseries$actual <- as.numeric(as.character(pred_timeseries$actual))
pred_timeseries$prediction <- as.numeric(as.character(pred_timeseries$prediction))
ggplot(pred_timeseries, aes(x = week_time, y = actual)) +  # Set up canvas with outcome variable on y-axis
  # geom_smooth(method = "auto", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = week_time, yend = prediction), alpha = .2)+
  geom_point(color = "black")+ # Plot the actual points
  geom_line(aes(y = actual))+
  geom_point(aes(y = prediction), shape = 1, color = "red")+
  geom_line(aes(y = prediction), color = "red")+
  theme_bw()  # Add theme for cleaner look

#' discrete data against raw data
rawData_TS_plot <- function(data,country){
  require(ggplot2)
  
  count <- data[, which(colnames(fluIliCountryData)==country)]
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

  p <- ggplot(raw_data, aes(x = weekTS))
  p <- p + geom_line(aes(y = count, colour = "Numeric Incidence"))
  
  # adding the relative humidity data, transformed to match roughly the range of the temperature
  p <- p + geom_line(aes(y = category *max(raw_data$count)/10, colour = "Categorical Incidence"))
  
  # now adding the secondary axis, following the example in the help file ?scale_y_continuous
  # and, very important, reverting the above transformation
  p <- p + scale_y_continuous(sec.axis = sec_axis(~. *10/max(raw_data$count), breaks = c(1:10), name = "Category"))
  
  # modifying colours and theme options
  p <- p + scale_colour_manual(values = c("darkslategray2", "salmon"))
  p <- p + labs(y = "Incidence",
                x = "Time",
                colour = "Obsevation")
  p <- p + theme(legend.position = c(0.25,0.85))
  p
}

#' Plot raw incidence data and categorical data in the same graph by dual axis is not clear for visualizing
#' So I will plot them into two seperate graphs but one is above another one
rawData_TS_plot <- function(data, country,countryName){
  require(ggplot2)
  require(grid)
  
  count <- data[, which(colnames(data)==country)]
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
  grobTS <- grobTree(textGrob(countryName, x=0.85,  y=0.9, hjust=0,
                              gp=gpar(col="black", fontsize=14, fontface="bold")))
  p1 <- ggplot(raw_data, aes(x = weekTS))
  p1 <- p1 + geom_line(aes(y = count),colour = "salmon",size=1)
  p1 <- p1 + scale_y_continuous(expand = c(0, 0))
  # p1 <- p1 + scale_x_continuous(breaks = c(2010:2018))
  # p1 <- p1 + scale_colour_manual(values = "salmon")
  p1 <- p1 + labs(y = "Incidence",
                x = "Time",
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
                  x = "Time",
                  # colour = "Obsevation",
                  title = "Time series of categorical observations of incidence")
  p2 <- p2 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  grid.newpage()
  grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))
  
}
rawData_TS_plot(fluIliCountryData,"ARM","Armenia")

#'store time series plot into work directory
for (i in 1:nrow(country_idd)){
  pdf(paste0(country_idd$Country[i],".pdf"))
  print(rawData_TS_plot(fluIliCountryData,country_idd$ISO3[i], country_idd$Country[i]))
  dev.off()
}
pdf("USA.pdf")
print(rawData_TS_plot(fluIliCountryData,"USA", "USA"))
dev.off()

#' time series plot for prediction
predTS_plot <- function(pred){
  # change week into format used for aweek package
  pred$weekTS <- gsub("-", "-W", pred$week_time, fixed = TRUE)
  
  # the aweek package is used to convert from epidemiological weeks to dates
  pred$weekTS <- week2date(pred$weekTS)
  
  # convert numbers from factors to numerics
  pred$Observation <- as.numeric(as.character(pred$Observation))
  pred$Prediction <- as.numeric(as.character(pred$Prediction))
  
  p <- ggplot(pred, aes(x = weekTS))
  
  # plot observational category 
  p <- p + geom_point(aes(y = Observation, color = "Observation"),size=3)
  p <- p + geom_line(aes(y = Observation, color = "Observation"),size=0.8)
  
  # plot predicted category
  p <- p + geom_point(aes(y = Prediction, color = "Prediction"),size=3)
  p <- p + geom_line(aes(y = Prediction, color = "Prediction"),size=0.8)
  
  # use segment to show the difference between prediction and obsercation more clearly
  # p <- p + geom_segment(aes(xend = weekTS, yend = Prediction), alpha = .2)
  
  # modifying colours and theme options
  p <- p + scale_colour_manual(values = c("darkslategray2", "salmon"))
  p <- p + scale_y_continuous(breaks = c(1:10))
  p <- p + labs(y = "Category",
                x = "Time",
                colour = "Category")
  p <- p + theme(legend.position = c(0.8,0.85))
  
  p
}

AUT_pred15 <- xgboost.model.pred(fluIliCountryData,"RUS",10,0,4,1)
predTS_plot(AUT_pred17)
RUS_data_1week <- gbm_complex(fluIliCountryData,"RUS",10,1)
RUS_pred17 <- xgboost.model.pred(fluIliCountryData,"RUS",10,0,6,1)
predTS_plot(RUS_pred17)

USA_pred15 <- xgboost.model.pred(fluIliCountryData,"USA",10,0,4,1)
USA_pred16 <- xgboost.model.pred(fluIliCountryData,"USA",10,1,4,1)
USA_pred17 <- xgboost.model.pred(fluIliCountryData,"USA",10,2,4,1)
predTS_plot(USA_pred15)
predTS_plot(USA_pred16)
predTS_plot(USA_pred17)

#' time series plot for 6-year training and 1-year test: observation versus prediction
for (i in 1:length(country_code)){
  pdf(paste0(country_idd$Country[i],".pdf"))
  forecast_result <- xgboost.model.pred(fluIliCountryData, country_code[i],countryISO,10,2,1)
  print(predTS_plot(forecast_result))
  dev.off()
}

#' time series plot for 4-year training and 3-year test: observation versus prediction
for (i in 1:length(country_code)){
  pdf(paste0(country_idd$Country[i],".pdf"))
  forecast_result <- xgboost.model.pred(fluIliCountryData, country_code[i],countryISO,10,4,3)
  print(predTS_plot(forecast_result))
  dev.off()
}

#' time series plot for 4-year training and 3-year test: last year observation versus prediction
for (i in 1:length(country_code)){
  pdf(paste0(country_idd$Country[i],".pdf"))
  forecast_result <-  pred1518_1718(fluIliCountryData,country_code[i])
  print(predTS_plot(forecast_result))
  dev.off()
}

#' compare last year of 6-year training and 4-year trainin
compare_lastYear_plot <- function(country){
  pred_compare <- compare_lastYear(country)
  # change week into format used for aweek package
  pred_compare$weekTS <- gsub("-", "-W", pred_compare$week_time, fixed = TRUE)
  
  # the aweek package is used to convert from epidemiological weeks to dates
  pred_compare$weekTS <- week2date(pred_compare$weekTS)
  
  # convert numbers from factors to numerics
  pred_compare$Observation<- as.numeric(as.character(pred_compare$Observation))
  
  pred_compare$Prediction1518 <- as.numeric(as.character(pred_compare$Prediction1518))
  pred_compare$Prediction1718 <- as.numeric(as.character(pred_compare$Prediction1718))
  
  p <- ggplot(pred_compare, aes(x = weekTS))
  
  # plot observational category 
  p <- p + geom_point(aes(y = Observation, color = "Observation"))
  p <- p + geom_line(aes(y = Observation, color = "Observation"))
  
  # plot predicted category
  
  p <- p + geom_point(aes(y = Prediction1718, color = "Prediction1718"))
  p <- p + geom_line(aes(y = Prediction1718, color = "Prediction1718"))
  
  p <- p + geom_point(aes(y = Prediction1518, color = "Prediction1518"))
  p <- p + geom_line(aes(y = Prediction1518, color = "Prediction1518"))
  
  # use segment to show the difference between prediction and obsercation more clearly
  # p <- p + geom_segment(aes(xend = weekTS, yend = Prediction), alpha = .2)
  
  # modifying colours and theme options
  p <- p + scale_colour_manual(values = c("darkslategray2", "salmon", "gold1"))
  p <- p + scale_y_continuous(breaks = c(1:10), limits = c(1,10))
  p <- p + labs(y = "Category",
                x = "Time",
                colour = "Category")
  p <- p + theme(legend.position = c(0.8,0.85))
  
  p
}

compare_lastYear_plot("BRB")

for (i in 1:(length(country_code)-1)){
  pdf(paste0(country_idd$Country[-4][i],".pdf"))
  print(compare_lastYear_plot(country_code[i]))
  dev.off()
}

#' plot the accuracy of historical model that the accuracy will keep roughlt stable as the number 
#' of week ahead forecast increases
grob_hist <- grobTree(textGrob("B", x=0.8,  y=0.95, hjust=0,
                               gp=gpar(col="black", fontsize=24, fontface="bold")))

ggplot(data = compareAccuracy_total_hist, aes(x = nWeek_ahead, y = percentage, group = 1)) +
  geom_line(size=1,colour="darkgreen")+
  geom_point(size=3,colour="darkgreen")+
  scale_y_continuous("Percentage of accurate forecst", breaks = c(0.00,0.25,0.50,0.75,1), limits = c(0,1),
                     expand = c(0, 0))+
  xlab("n-week ahead")+ 
  theme_bw()+
  annotation_custom(grob_hist)

#' plot the accuracy of repeat model that the accuracy drops off as the numebr of week ahead forecasted 
#' increases

# Create a text
grob_repeat <- grobTree(textGrob("A", x=0.8,  y=0.95, hjust=0,
                          gp=gpar(col="black", fontsize=24, fontface="bold")))

ggplot(data = compareAccuracy_total, aes(x = nWeek_ahead, y = percentage, group = 1)) +
  geom_line(size=1,colour="darkgreen")+
  geom_point(size=3,colour="darkgreen")+
  scale_y_continuous("Percentage of accurate forecst", breaks = c(0.00,0.25,0.50,0.75,1), limits = c(0,1),
                     expand = c(0, 0))+
  xlab("n-week ahead")+
  theme_bw()+
  annotation_custom(grob_repeat)

#' plot the distribution of categories
# Change the width of bars
ggplot(data=categorySummaryChart, aes(x=Category, y=Density)) +
  geom_bar(stat="identity", width=1, colour = "white", fill="steelblue")+
  scale_x_continuous("Category", breaks = c(1:10))+
  scale_y_continuous("Density",expand = c(0, 0),limits = c(0,1.00))+
  geom_text(aes(label=Density), vjust=-0.2, size=3.5)+
  # geom_text(aes(label=Density), vjust=1.6, color="white", size=3.5)+
  theme_bw()

ggplot(category, aes(x=Y_week0)) + 
  geom_histogram(aes(y=..density..), fill = "steelblue", binwidth = 1)+
  #scale_x_continuous("Category", breaks = c(1:10), limits = c(1,10))+
  # scale_y_continuous("Density",expand = c(0, 0))+
  xlab("Category")+
  ylab("Density")+
  theme_bw()
  # stat_function(fun = dgamma, args = list(shape1 = mean(category$Y_week0), shape2 = sd(category$Y_week0)))

#' time series of all countries categorical data

allCountry_TS$weekTS <- as.character(allCountry_TS$weekTS)
allCountry_TS$weekTS <- as.Date(allCountry_TS$weekTS)


# install.packages("cowplot")

all1 <- ggplot(allCountry_TS,aes(x=weekTS,y=count,colour=country,group=country)) + geom_line()+
  scale_y_continuous(expand = c(0,0))+
  xlab("Time")+
  ylab("Incidence")+
  scale_x_date(labels = date_format("%Y"))

all2 <- ggplot(allCountry_TS,aes(x=weekTS,y=category,colour=country,group=country)) + geom_line()+
  scale_y_continuous(breaks = c(1:10), limits = c(1,11),expand = c(0,0))+
  xlab("Time")+
  ylab("Categorical incidence")+
  scale_x_date(labels = date_format("%Y"))

leg <- get_legend(all1)
allTemp <- cowplot::plot_grid(all1+theme(legend.position="none"),
                   all2+theme(legend.position="none"),
                   nrow = 2)
cowplot::plot_grid(allTemp,leg,nrow=1)

                                                                                                                                                                                                                                       tmp1 <- allCountry_TS[,c("country","week_time","weekTS","count")]
tmp1$subset <- "count"
tmp2 <- data.frame(allCountry_TS[,c("country","week_time","weekTS","category")])
tmp2$subset <- "category"
colnames(tmp1) <- colnames(tmp2) <- c("country","week_time","weekTS","value","subset")
tmp3 <- rbind(tmp1, tmp2)

blank_data <- data.frame(subset = c("category", "category", "count", "count" ), x = 0, y = c(0, 
                                                                               11, 0, 120000))

all3 <- ggplot(tmp3,aes(x=weekTS,y=value,colour=country,group=country)) + geom_line()+
  geom_blank(data = blank_data, aes(x = x, y = y))+
  facet_wrap(~subset,nrow=2,scales="free_y",strip.position = "left",
             labeller = as_labeller(c(category = "Categorical incidence", count = "Numeric Incidence")))+
  expand_limits(y = 0)+
  scale_y_continuous(expand = c(0,0))+
  xlab("Time")+
  ylab(NULL)+
  scale_x_date(labels = date_format("%Y"))


#' Heat chart for XGboost
#' heat chart for 1-year ahead forecast
#' 2010-2014 training, 2015 test
for (i in 1:nrow(country_idd)){
  pdf(paste0(country_idd$Country[i],".pdf"))
  forecast_result <- xgboost.model.pred(fluIliCountryData, country_idd$ISO3[i], countryISO, 10, 0, 4, 4)
  frequency_table <- freq_table(forecast_result, 10)
  print(heat_plot(frequency_table, country_idd$ISO3[i]))
  dev.off()
}

#' 2011-2015 train, 2016 test
for (i in 1:nrow(country_idd)){
  pdf(paste0(country_idd$Country[i],".pdf"))
  forecast_result <- xgboost.model.pred(fluIliCountryData, country_idd$ISO3[i], countryISO, 10, 1, 4, 4)
  frequency_table <- freq_table(forecast_result, 10)
  print(heat_plot(frequency_table, country_idd$ISO3[i]))
  dev.off()
}

#' 2012-2016 train, 2017 test
for (i in 1:nrow(country_idd)){
  pdf(paste0(country_idd$Country[i],".pdf"))
  forecast_result <- xgboost.model.pred(fluIliCountryData, country_idd$ISO3[i], countryISO, 10, 2, 4, 4)
  frequency_table <- freq_table(forecast_result, 10)
  print(heat_plot(frequency_table, country_idd$ISO3[i]))
  dev.off()
}

#' 2010 -2016 train, 2017 test
for (i in 1:nrow(country_idd)){
  pdf(paste0(country_idd$Country[i],".pdf"))
  forecast_result <- xgboost.model.pred(fluIliCountryData, country_idd$ISO3[i], countryISO, 10, 0, 6, 4)
  frequency_table <- freq_table(forecast_result, 10)
  print(heat_plot(frequency_table, country_idd$ISO3[i]))
  dev.off()
}

#' 1-year ahead accuracy compare
oneYear_accuracy_xgb <- data.frame(c(2015,2016,2017,2017),c(length(which(compare_pred15$Accurate==1))/nrow(compare_pred15),
                                   length(which(compare_pred16$Accurate==1))/nrow(compare_pred16),
                                   length(which(compare_pred17$Accurate==1))/nrow(compare_pred17),
                                   length(which(compare1016_pred17$Accurate==1))/nrow(compare1016_pred17)))
colnames(oneYear_accuracy_xgb) <- c('year','accuracy')
oneYear_accuracy_xgb$accuracy <- round(oneYear_accuracy_xgb$accuracy,2)

ggplot(data = oneYear_accuracy_xgb, aes(x = year, y = accuracy, group = 1)) +
  geom_line(size=1,colour="darkgreen")+
  geom_point(size=3,colour="darkgreen")+
  scale_y_continuous("Forecast accuracy", breaks = c(0.00,0.25,0.50,0.75,1), limits = c(0,1),
                     expand = c(0, 0))+
  xlab("n-week ahead")+
  theme_bw()

#' 2010-16 train, 17-18 test, n=1,2,3,4
xgb_sixYear <- data.frame(c(1:4), c(length(which(compare1016_pred17$Accurate==1))/nrow(compare1016_pred17),
                                    length(which(compare1016_pred17Two$Accurate==1))/nrow(compare1016_pred17Two),
                                    length(which(compare1016_pred17Three$Accurate==1))/nrow(compare1016_pred17Three),
                                    length(which(compare1016_pred17Four$Accurate==1))/nrow(compare1016_pred17Four)))
colnames(xgb_sixYear) <- c("nWeek_ahead","accurate")
xgb_sixYear$accurate <- round(xgb_sixYear$accurate,2)

#' 2010-14 train, 15 test, n=1,2,3,4
xgb_oneYear15 <- data.frame(c(1:4),c(length(which(compare_pred15$Accurate==1))/nrow(compare_pred15),
                                     length(which(compare_pred15Two$Accurate==1))/nrow(compare_pred15Two),
                                     length(which(compare_pred15Three$Accurate==1))/nrow(compare_pred15Three),
                                     length(which(compare_pred15Four$Accurate==1))/nrow(compare_pred15Four)))
colnames(xgb_oneYear15) <- c("nWeek_ahead","accurate")
xgb_oneYear15$accurate <- round(xgb_oneYear15$accurate,2)

#' 2011-2015 train, 16 test, n=1,2,3,4
xgb_oneYear16 <- data.frame(c(1:4),c(length(which(compare_pred16$Accurate==1))/nrow(compare_pred16),
                                     length(which(compare_pred16Two$Accurate==1))/nrow(compare_pred16Two),
                                     length(which(compare_pred16Three$Accurate==1))/nrow(compare_pred16Three),
                                     length(which(compare_pred16Four$Accurate==1))/nrow(compare_pred16Four)))
colnames(xgb_oneYear16) <- c("nWeek_ahead","accurate")
xgb_oneYear16$accurate <- round(xgb_oneYear16$accurate,2)

#' 2012-2016 train, 17 test, n=1,2,3,4
xgb_oneYear17 <- data.frame(c(1:4),c(length(which(compare_pred17$Accurate==1))/nrow(compare_pred17),
                                     length(which(compare_pred17Two$Accurate==1))/nrow(compare_pred17Two),
                                     length(which(compare_pred17Three$Accurate==1))/nrow(compare_pred17Three),
                                     length(which(compare_pred17Four$Accurate==1))/nrow(compare_pred17Four)))
colnames(xgb_oneYear17) <- c("nWeek_ahead","accurate")
xgb_oneYear17$accurate <- round(xgb_oneYear17$accurate,2)

xgb_combineAcc <- cbind(c(1:4),xgb_sixYear$accurate,xgb_oneYear15$accurate,xgb_oneYear16$accurate,
                        xgb_oneYear17$accurate) %>% as.data.frame()
colnames(xgb_combineAcc) <- c("nWeek_ahead","sixYear_pred17","pred15","pred16","pred17")
# xgb_combineAcc$sixYear_pred17 <- as.factor(xgb_combineAcc$sixYear_pred17)
# xgb_combineAcc$pred15 <- as.factor(xgb_combineAcc$pred15)
# xgb_combineAcc$pred16 <- as.factor(xgb_combineAcc$pred16)
# xgb_combineAcc$pred17 <- as.factor(xgb_combineAcc$pred17)
# xgb_combineAcc$nWeek_ahead <- as.factor(xgb_combineAcc$nWeek_ahead)

# convert into numeric
xgb_combineAcc$pred15 <- as.numeric(as.character(xgb_combineAcc$pred15))
xgb_combineAcc$pred16 <- as.numeric(as.character(xgb_combineAcc$pred16))
xgb_combineAcc$pred17 <- as.numeric(as.character(xgb_combineAcc$pred17))

# convert numbers from factors to numerics
pred_compare$Observation<- as.numeric(as.character(pred_compare$Observation))

pred_compare$Prediction1518 <- as.numeric(as.character(pred_compare$Prediction1518))
pred_compare$Prediction1718 <- as.numeric(as.character(pred_compare$Prediction1718))

ggplot(xgb_combineAcc, aes(x = nWeek_ahead,group=1))+
  geom_point(aes(y = pred15, color = "pred15"),size=3)+
  geom_line(aes(y = pred15, color = "pred15"),size=1)+
  geom_point(aes(y = pred16, color = "pred16"),size=3)+
  geom_line(aes(y = pred16, color = "pred16"),size=1)+
  geom_point(aes(y = pred17, color = "pred17"),size=3)+
  geom_line(aes(y = pred17, color = "pred17"),size=1)+
  ylim(0.6,0.8)+
  labs(y = "Forecast accuracy",
       x = "n-week ahead",
       colour = "forecast year")+
  theme_bw()
  

#' hist of category distribution in 2015, 2016 and 2017
#' in 2015
ggplot(data=category15, aes(x=Category, y=Density)) +
  geom_bar(stat="identity", width=1, colour = "white", fill="steelblue")+
  scale_x_continuous("Category", breaks = c(1:10))+
  scale_y_continuous("Density",expand = c(0, 0),limits = c(0,1.00))+
  geom_text(aes(label=Density), vjust=-0.2, size=3.5)+
  # geom_text(aes(label=Density), vjust=1.6, color="white", size=3.5)+
  theme_bw()

#' in 2016
ggplot(data=category16, aes(x=Category, y=Density)) +
  geom_bar(stat="identity", width=1, colour = "white", fill="steelblue")+
  scale_x_continuous("Category", breaks = c(1:10))+
  scale_y_continuous("Density",expand = c(0, 0),limits = c(0,1.00))+
  geom_text(aes(label=Density), vjust=-0.2, size=3.5)+
  # geom_text(aes(label=Density), vjust=1.6, color="white", size=3.5)+
  theme_bw()

#' in 2017
ggplot(data=category17, aes(x=Category, y=Density)) +
  geom_bar(stat="identity", width=1, colour = "white", fill="steelblue")+
  scale_x_continuous("Category", breaks = c(1:10))+
  scale_y_continuous("Density",expand = c(0, 0),limits = c(0,1.00))+
  geom_text(aes(label=Density), vjust=-0.2, size=3.5)+
  # geom_text(aes(label=Density), vjust=1.6, color="white", size=3.5)+
  theme_bw()

#' heat plot for Austria and Russia
AUT_pred17 <- xgboost.model.pred(fluIliCountryData,"AUT",10,0,6,1)
AUT_freq <- freq_table(AUT_pred17,10)
heat_plot(AUT_freq)
